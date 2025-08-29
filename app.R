# Load necessary libraries
library(shiny)
library(bslib) # For modern, nice-looking dashboard theme
library(lares) # For theme_lares() and freqs()
library(dplyr) # For data manipulation
library(ggplot2) # For plotting
library(patchwork) # To join mutltiple plots
library(ChessPI) # To fetch Chess.com data
library(DT) # For interactive data tables
library(future) # For asynchronous operations
library(promises) # For handling promises in Shiny
library(scales) # For label_number() in plots
library(querychat) # For simplified chat UI and server
library(ellmer) # For interacting with OpenAI and other LLMs
library(purrr) # For purrr::partial, used in querychat_init
library(lareshiny) # For login
library(jsonlite) # For credentials
library(chess) # For the chess board visualization
library(magrittr) # For the pipe operator %>%
library(stringr) # str_extract_all
library(htmltools) # For htmlEscape

# Source the new plots.R file
source("global.R")
source("aux.R")
source("plots.R")

# Create credentials encrypted self-hosted file
if (FALSE) {
  creds <- data.frame(user = "xxx", pass = "xxx")
  my_enc <- lares::get_creds("encrypted")$lares_key
  Sys.setenv("LARES_ENC" = my_enc)
  write_encrypted(creds, "creds.enc", key = my_enc)
  creds <- fromJSON(read_encrypted("creds.enc", my_enc))
}

# Set up future for asynchronous processing
plan(multisession)

# Set cache directory
cache_dir <- getwd()

# Define UI for the application
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", font_scale = 0.9),
  tags$head(
    tags$style(HTML("
      #chess_board_output { /* Specific styling for the text board output */
        line-height: 1.2; /* Adjust line height for better spacing */
        white-space: pre; /* Preserve whitespace and line breaks */
        text-align: center; /* Center the text content */
        margin: 0 auto; /* Center the block element itself */
        background-color: #ffffff; /* White background for the board area */
        border: 3px solid #555; /* Thicker border for the board */
        box-shadow: 0 8px 16px rgba(0,0,0,0.2); /* Deeper shadow */
        width: 410px; /* Set a fixed width */
        height: 410px; /* Set height equal to width for a square */
        overflow: hidden; /* Hide any content that overflows the fixed dimensions */
        box-sizing: border-box; /* Ensure padding and border are included in the width/height */
      }"))
  ),
  titlePanel(
    title = NULL,
    windowTitle = "Chess.com Dashboard"
  ),
  sidebarLayout(
    sidebarPanel(
      h4("Player Information"),
      textInput(
        inputId = "chess_username",
        label = "Chess.com Username / Cache Key:", # Label updated
        value = "laresdj", # Default username
        placeholder = "e.g., laresdj or laresdj_all" # Placeholder updated
      ),
      radioButtons(
        inputId = "data_fetch_option",
        label = "Data Fetch Option:",
        choices = c("Fetch All Games/Load cache" = "all", "Fetch a Date Range" = "range"),
        selected = "all" # Default to fetching all games
      ),
      # Date range input, conditionally shown
      uiOutput("date_range_ui"),
      actionButton(
        inputId = "fetch_data",
        label = "Fetch Data / Load Cache",
        icon = icon("download"),
        class = "btn-primary",
        width = "100%"
      ),
      hr(),
      # Cache information and interactive removal
      h4("Cache Management"), # New header for clarity
      uiOutput("cache_list_ui"), # New UI for the editable text area
      br(),
      actionButton(
        inputId = "apply_cache_changes",
        label = "Apply Cache Changes",
        icon = icon("save"),
        class = "btn-primary",
        width = "100%"
      ),
      p(" "),
      actionButton(
        inputId = "reset_all_caches", # Renamed from reset_cache for clarity
        label = "Clear All Cache",
        icon = icon("broom"),
        class = "btn-danger",
        width = "100%"
      ),
      hr(),
      helpText(
        "Note: 'Fetch All Games' may take a while for users with many games. ",
        "If 'Select Date Range' is chosen, only games within that period will be fetched and displayed.",
        "Keep in mind API returns full months data."
      ),
      hr(), uiOutput("error_message")
    ),
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel(
          "Overview",
          icon = icon("chess-board"),
          br(),
          uiOutput("overview_content")
        ),
        tabPanel(
          "Games",
          icon = icon("table"),
          br(),
          uiOutput("recent_games_content")
        ),
        tabPanel(
          "Review",
          icon = icon("magnifying-glass"),
          br(),
          textAreaInput("pgn_input", "PGN of Loaded Game:",
            value = "", rows = 3, width = "100%",
            placeholder = "Load a game from the 'Games' tab to review it here."
          ),
          hr(),
          # Chess game viewer controls (now inside the Review tab)
          div(
            class = "button-group", align = "center",
            actionButton("reset_board", icon("angles-left"), class = "btn btn-primary"),
            actionButton("prev_move", icon("angle-left"), class = "btn btn-info"),
            actionButton("next_move", icon("angle-right"), class = "btn btn-info"),
            actionButton("restart_to_end", icon("angles-right"), class = "btn btn-primary"),
            actionButton("flip", icon("arrows-up-down"), class = "btn btn-info")
          ),
          br(),
          htmlOutput("chess_board_output"),
          textOutput("chess_board_note")
        ),
        tabPanel(
          "Chess AI",
          icon = icon("comments"),
          br(),
          uiOutput("chat_content")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # --- Login (from original app) ---
  creds <- fromJSON(read_encrypted("creds.enc", Sys.getenv("LARES_ENC")))
  login <- module_login(
    input, session,
    users = creds$user,
    pwds = creds$pass,
    style = list(botton_txt_colour = "#FFF", botton_bgd_colour = "#000"),
    logo = "icon.png",
    lang = "en",
    logged = TRUE
  )
  observe({
    if (login$authenticated) {
      message("User logged in successfully: ", login$user)
    }
  })

  # --- Reactive Values for Main App Data ---
  rv <- reactiveValues(
    processed_games = NULL,
    final_user = NULL,
    loading = FALSE,
    error = NULL,
    available_cache_keys = NULL # Renamed for clarity to hold just the keys
  )

  # --- Reactive Values for Chess Game Viewer ---
  game_state <- reactiveValues(
    game = NULL, # Stores the chess game object (root GameNode) parsed from PGN
    current_move_index = 0, # Tracks the current move being displayed (0 for initial board)
    flipped = FALSE # Boolean to toggle board orientation (FALSE for white's view, TRUE for black's)
  )

  # Function to get current cache keys
  get_current_cache_keys <- function() {
    cache_files <- list.files(cache_dir, pattern = "^lares_cache_.*\\.RDS$", full.names = FALSE)
    # Extract the part between "lares_cache_" and ".RDS"
    cache_keys <- gsub("^lares_cache_|\\.RDS$", "", cache_files)
    return(sort(unique(cache_keys)))
  }

  # Update the available cache keys and the text area
  update_cache_display <- function() {
    rv$available_cache_keys <- get_current_cache_keys()
    updateTextAreaInput(session, "cache_keys_input", value = paste(sort(rv$available_cache_keys), collapse = "\n"))
  }
  
  # Last move's name
  chess_move_name <- function(x) {
    if (is.null(x$parent)) {
      "<Start>" %>% paste0(strrep(" ", 15 - nchar(.)), .)
    } else {
      turn <- ifelse(!x$turn(), (x$ply() - 1)/2, (x$ply() - 2)/2) + 1
      turn <- ifelse(!x$turn(), paste0(turn, ". "), paste0(turn, "... "))
      paste0("<", turn, x$san(), ">") %>% paste0(strrep(" ", 15 - nchar(.)), .)
    }
  }

  # Initial call to populate the cache list on app start
  observeEvent(TRUE,
    { # Use TRUE to trigger on app start
      update_cache_display()
    },
    once = TRUE
  )

  # --- Data Fetching and Processing (from original app) ---
  output$date_range_ui <- renderUI({
    if (input$data_fetch_option == "range") {
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range:",
        start = Sys.Date() - 365,
        end = Sys.Date(),
        format = "yyyy-mm-dd",
        separator = "to"
      )
    }
  })

  # Render the interactive cache list UI
  output$cache_list_ui <- renderUI({
    tagList(
      textAreaInput(
        inputId = "cache_keys_input",
        label = "Available Caches (one per line):",
        value = paste(rv$available_cache_keys, collapse = "\n"),
        rows = 5,
        width = "100%"
      ),
      helpText("To remove caches, delete line(s) above and click button below:")
    )
  })

  # Observer for applying cache changes from the text area
  observeEvent(input$apply_cache_changes, {
    # Get the caches currently on disk
    caches_on_disk <- get_current_cache_keys()

    # Get the caches from the text input, split by new line and trim whitespace
    desired_caches <- str_trim(str_split(input$cache_keys_input, "\n")[[1]])
    # Remove any empty strings that might result from extra new lines
    desired_caches <- desired_caches[desired_caches != ""]

    # Caches to remove (present on disk but not in the input)
    caches_to_remove <- setdiff(caches_on_disk, desired_caches)

    if (length(caches_to_remove) > 0) {
      showNotification(
        paste("Removing", length(caches_to_remove), "cache(s)..."),
        type = "warning", duration = 3
      )
      for (cache_key in caches_to_remove) {
        file_path <- file.path(cache_dir, paste0("lares_cache_", cache_key, ".RDS"))
        if (file.exists(file_path)) {
          file.remove(file_path)
          message(paste("Removed cache file:", file_path))
        }
      }
      showNotification("Selected caches removed!", type = "message", duration = 3)
    } else {
      showNotification("No caches to remove or changes applied.", type = "info", duration = 3)
    }

    # Refresh the display of available caches
    update_cache_display()
  })


  observeEvent(input$reset_all_caches, { # Changed from input$reset_cache
    showNotification("Clearing all caches...", type = "warning", duration = 3)
    lares::cache_clear(cache_dir = cache_dir)
    rv$processed_games <- NULL
    rv$error <- NULL
    showNotification("All caches cleared!", type = "default", duration = 3)
    # Update the cache display after clearing all
    update_cache_display()
  })

  observeEvent(input$fetch_data, {
    req(input$chess_username)

    rv$loading <- TRUE
    rv$error <- NULL
    rv$processed_games <- NULL

    id <- showNotification(
      HTML('<i class="fas fa-spinner fa-spin"></i> Fetching data...'),
      duration = NULL,
      type = "warning",
      closeButton = FALSE
    )

    current_username_or_cache_key <- input$chess_username
    rv$final_user <- gsub("_.*", "", isolate(input$chess_username))
    fetch_option <- input$data_fetch_option
    selected_start_date <- if (fetch_option == "range") input$date_range[1] else as.Date("1970-01-01")
    # Only consider starting month completely given the API returns whole months
    selected_start_date <- format(selected_start_date, "%Y%m")
    selected_end_date <- if (fetch_option == "range") input$date_range[2] else Sys.Date()

    # --- Try to load exact cache key from input$chess_username ---
    fetched_data <- NULL
    loaded_from_exact_cache <- FALSE

    if (current_username_or_cache_key %in% rv$available_cache_keys) {
      exact_cache_file_path <- file.path(cache_dir, paste0("lares_cache_", current_username_or_cache_key, ".RDS"))
      if (file.exists(exact_cache_file_path)) {
        fetched_data <- lares::cache_read(base = current_username_or_cache_key, cache_dir = cache_dir)
        message(sprintf("Data for %s loaded from exact cache key: %s", current_username_or_cache_key, current_username_or_cache_key))
        loaded_from_exact_cache <- TRUE
        showNotification(sprintf(
          "Data for '%s' loaded from cache.", current_username_or_cache_key
        ), type = "message", duration = 5)
      } else {
        # This case should ideally not happen if rv$available_cache_keys is accurate but provides a safeguard.
        message(sprintf("Cache file for '%s' not found despite being in list. Proceeding with regular fetch.", current_username_or_cache_key))
      }
    }

    if (!loaded_from_exact_cache) {
      # Determine the cache key based on selected options (username + date/all)
      cache_key <- if (fetch_option == "all") {
        paste0(current_username_or_cache_key, "_all")
      } else {
        paste0(current_username_or_cache_key, "_", selected_start_date, "_", format(selected_end_date, "%Y%m%d"))
      }

      if (lares::cache_exists(base = cache_key, cache_dir = cache_dir)) {
        fetched_data <- lares::cache_read(base = cache_key, cache_dir = cache_dir)
        message(sprintf("Data for %s read from generated cache: %s", current_username_or_cache_key, cache_key))
        showNotification(sprintf(
          "Data for '%s' loaded from cache.", cache_key
        ), type = "message", duration = 5)
      } else {
        message(sprintf("Fetching data for %s (option: %s)...", current_username_or_cache_key, fetch_option))
        if (fetch_option == "all") {
          fetched_data <- tryCatch(
            {
              ChessPI::chessPI.pull(current_username_or_cache_key, range = "all")
            },
            error = function(e) {
              message(sprintf("Error fetching 'all' data for %s: %s", current_username_or_cache_key, e$message))
              return(NULL)
            }
          )
        } else {
          month_ranges_to_fetch <- generate_month_ranges(input$date_range[1], input$date_range[2])
          all_monthly_games <- list()
          for (month_range in month_ranges_to_fetch) {
            monthly_games <- tryCatch(
              {
                ChessPI::chessPI.pull(current_username_or_cache_key, range = month_range)
              },
              error = function(e) {
                message(sprintf("Error fetching data for %s/%s: %s", current_username_or_cache_key, month_range, e$message))
                return(NULL)
              }
            )
            if (!is.null(monthly_games)) {
              all_monthly_games[[month_range]] <- monthly_games
            }
          }
          fetched_data <- bind_rows(all_monthly_games)
        }

        if (!is.null(fetched_data) && nrow(fetched_data) > 0) {
          lares::cache_write(fetched_data, base = cache_key, cache_dir = cache_dir)
          message(sprintf("Data for %s cached successfully: %s", current_username_or_cache_key, cache_key))
          showNotification(sprintf(
            "New data for '%s' fetched and cached.", cache_key
          ), type = "message", duration = 5)
        }
      }
    }
    # --- END Existing Fetch Logic ---

    processed_data <- NULL
    if (!is.null(fetched_data) && nrow(fetched_data) > 0) {
      processed_data <- fetched_data %>%
        arrange(games.end_time) %>%
        mutate(
          game_number = row_number(),
          df.pgn = lapply(games.pgn, function(x) pgn2data(x, "df")[[1]]),
          games.end_time = as.POSIXct(games.end_time, origin = "1970-01-01", tz = "UTC"),
          games.eco = gsub("-|\\.|\\.\\.\\.", " ", gsub(".*openings/", "", games.eco)),
          turns = 0,
          user_color = ifelse(games.white$username == rv$final_user, "white", "black"), # Use input value for username
          user_result = ifelse(user_color == "white", games.white$result, games.black$result),
          user_rating = ifelse(user_color == "white", games.white$rating, games.black$rating),
          opponent_rating = ifelse(user_color == "white", games.black$rating, games.white$rating),
          rating_diff = user_rating - opponent_rating,
          user_accuracy = round(ifelse(user_color == "white", games.accuracies$white, games.accuracies$black), 2),
          opponent_accuracy = round(ifelse(user_color == "white", games.accuracies$black, games.accuracies$white), 2),
          accuracy_diff = round(user_accuracy - opponent_accuracy, 2),
          user_win = case_when(
            .data$user_result %in% c("win") ~ TRUE,
            .data$user_result %in% c("checkmated", "resigned", "timeout", "timevsinsufficient") ~ FALSE,
            TRUE ~ NA
          )
        ) %>%
        rowwise() %>%
        mutate(turns = nrow(df.pgn) / 2) %>%
        ungroup() %>%
        arrange(desc(games.end_time)) %>%
        select(-df.pgn)

      querychat_config_global <- querychat_init(
        df = select_if(processed_data, function(x) !is.list(x)),
        table_name = "chess_games_data",
        greeting = "Ask Chess Insights AI anything related to your games...",
        data_description = global_data_description,
        create_chat_func = purrr::partial(
          ellmer::chat_openai,
          model = "gpt-4.1",
          api_key = Sys.getenv("OPENAI_API")
        )
        # create_chat_func = purrr::partial(
        #   ellmer::chat_ollama,
        #   model = "llama3.2"
        # )
      )
      rv$querychat <- querychat_server("chess_chat", querychat_config_global)
    }
    rv$processed_games <- processed_data
    rv$loading <- FALSE
    removeNotification(id)

    if (is.null(rv$processed_games) || nrow(rv$processed_games) == 0) {
      if (!lares::haveInternet()) {
        rv$error <- "No internet connection."
      } else {
        rv$error <- "No games found for the selected username and date range."
      }
    }

    # Update cache display after fetching data (new cache might be created)
    update_cache_display()
  })

  output$error_message <- renderUI({
    if (!is.null(rv$error)) {
      div(
        class = "alert alert-danger",
        role = "alert",
        icon("exclamation-triangle"), rv$error
      )
    }
  })

  # --- UI elements that depend on fetched data ---
  output$overview_content <- renderUI({
    req(rv$processed_games)
    req(nrow(rv$processed_games) > 0)
    tagList(
      fluidRow(
        column(
          width = 6,
          wellPanel(
            uiOutput("latest_rating_box"),
            style = "background-color: #e0f2f7; border-color: #b3e5fc;"
          )
        ),
        column(
          width = 6,
          wellPanel(
            uiOutput("total_games_box"),
            style = "background-color: #e8f5e9; border-color: #c8e6c9;"
          )
        )
      ),
      textOutput("chat_title"),
      br(),
      plotOutput("rating_evolution_plot", height = "400px"),
      br(),
      plotOutput("freq_moves_plot", height = "400px"),
      br(),
      plotOutput("rating_diff_plot", height = "300px"),
      br(),
      plotOutput("accuracy_plot", height = "300px")
    )
  })

  output$recent_games_content <- renderUI({
    req(rv$processed_games)
    req(nrow(rv$processed_games) > 0)
    tagList(
      DTOutput("recent_games_table", width = "100%")
    )
  })

  output$chat_content <- renderUI({
    req(rv$processed_games)
    tagList(
      h3("Interact with your data and dashboards"),
      querychat_ui("chess_chat")
    )
  })

  output$latest_rating_box <- renderUI({
    latest_rating <- rv$querychat$df()$user_rating[1]
    latest_type <- paste(
      rv$querychat$df()$games.time_class[1],
      rv$querychat$df()$games.time_control[1]
    )
    div(
      class = "text-center",
      h2(latest_rating),
      p(sprintf("Latest rating [%s]*", latest_type))
    )
  })

  output$total_games_box <- renderUI({
    total_games <- nrow(rv$querychat$df())
    div(
      class = "text-center",
      h2(total_games),
      p("Total games played*")
    )
  })

  observe({
    req(rv$querychat)
    if (is.null(rv$querychat$title())) {
      if (nrow(rv$querychat$df()) >= 1) {
        date_range <- range(as.Date(rv$querychat$df()$games.end_time))
        output$chat_title <- renderText(paste(
          "*From available data, within date range:", paste(unique(date_range), collapse = " to ")
        ))
      } else {
        output$chat_title <- renderText("*Within selected date range")
      }
    } else {
      output$chat_title <- renderText(paste0("*", rv$querychat$title()))
    }
  })

  output$rating_evolution_plot <- renderPlot({
    plot_rating_evolution(rv$querychat$df(), isolate(rv$final_user))
  })

  output$rating_diff_plot <- renderPlot({
    plot_rating_diff(rv$querychat$df(), isolate(rv$final_user))
  })

  output$freq_moves_plot <- renderPlot({
    plot_frequent_moves(rv$querychat$df(), n_turns = 3, top_moves = 8)
  })

  output$accuracy_plot <- renderPlot({
    plot_accuracy(rv$querychat$df(), isolate(rv$final_user))
  })

  output$recent_games_table <- renderDT({
    df <- rv$querychat$df() %>%
      rowwise() %>%
      mutate(
        load_pgn = paste0(
          '<button id="load_button_', row_number(),
          '" type="button" class="btn btn-primary btn-sm load-game-button" data-row-pgn="',
          htmltools::htmlEscape(clean_pgn(`games.pgn`)),
          # Pass the user_color
          '" data-row-user-color="',
          user_color,
          '">Load</button>'
        ),
        games.end_time = format(games.end_time, "%Y-%m-%d %H:%M"),
        game_number = paste0("<a href='", games.url, "' target='_blank'>", game_number, "</a>")
      ) %>%
      select("load_pgn", any_of(variable_names)) %>%
      select(-any_of(c(
        "games.pgn", "games.uuid", "games.fen", "games.start_time",
        "games.initial_setup", "games.tcn", "games.url"
      )))
    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        dom = "Bfrtip"
      ),
      escape = FALSE, # to render links and buttons
      rownames = FALSE,
      filter = "top",
      selection = "none",
      callback = JS("
          // This callback executes after the table is drawn.
          // It attaches click event listeners to the 'Load' buttons.
          table.on('click', '.load-game-button', function() {
            var pgn = $(this).data('row-pgn'); // Get the PGN from the data attribute
            var userColor = $(this).data('row-user-color'); // <<< ADDED: Get user_color from data attribute
            // Send the PGN and user_color as an object to the server.
            Shiny.setInputValue('load_game_data', { pgn: pgn, userColor: userColor }, {priority: 'event'});
          });
        ")
    )
  })

  # --- Chess Game Viewer Logic ---

  # Helper function to get the total number of full moves in the game
  get_total_moves <- function(game_node) {
    if (is.null(game_node)) {
      return(0)
    }
    count <- 0
    temp_node <- game_node
    while (!is.null(chess::forward(temp_node, 1))) {
      temp_node <- chess::forward(temp_node, 1)
      count <- count + 1
    }
    return(count)
  }

  # Reactive expression to store the total number of moves in the game
  total_game_moves <- reactive({
    req(game_state$game)
    get_total_moves(game_state$game)
  })

  # Observer for the 'Load' button click from the DT table
  # This event now receives both PGN and user_color
  observeEvent(input$load_game_data,
    {
      req(input$load_game_data)

      loaded_pgn <- input$load_game_data$pgn
      loaded_user_color <- input$load_game_data$userColor

      # Update the textarea with the PGN (optional, but good for user feedback)
      updateTextAreaInput(session, "pgn_input", value = loaded_pgn)

      # Parse the PGN and update game_state
      tryCatch(
        {
          game <- read_game(file = textConnection(loaded_pgn))
          game_state$game <- game
          game_state$current_move_index <- total_game_moves() # Go to end of game by default

          # Set flipped based on loaded_user_color
          game_state$flipped <- (loaded_user_color == "black")

          showNotification(paste("PGN loaded successfully as", loaded_user_color), type = "message", duration = 5)
        },
        error = function(e) {
          showNotification(paste("Error parsing PGN:", e$message), type = "error", duration = NULL)
          game_state$game <- NULL
          game_state$flipped <- FALSE # Reset flip state on error
        }
      )
    },
    priority = 1
  ) # Give this higher priority to process first when loading from table

  # Observer for changes in the PGN input text area (manual or from 'Load' button)
  # This will primarily handle manual PGN entries now.
  observeEvent(input$pgn_input,
    {
      req(input$pgn_input)

      # Only process if this is a direct user input, not triggered by a load_game_data event.
      # A simple way to check is if game_state$game is NULL or if input$pgn_input has genuinely changed
      # from what was last loaded by the button (which is harder to track perfectly).
      # For manual entries, we default to White's view.

      # If the PGN input is empty or just changed (and not from load_game_data), reset.
      # This effectively ensures manual changes reset the flip state.
      if (!isTRUE(game_state$last_loaded_pgn == input$pgn_input)) { # Prevent re-trigger on programmatic update
        tryCatch(
          {
            game <- read_game(file = textConnection(input$pgn_input))
            game_state$game <- game
            game_state$current_move_index <- total_game_moves()
            game_state$flipped <- FALSE # Default to not flipped for manual entries
            game_state$last_loaded_pgn <- input$pgn_input # Store for comparison
            showNotification("Manual PGN loaded successfully! (Default view: White)", type = "message", duration = 5)
          },
          error = function(e) {
            showNotification(paste("Error parsing PGN manually:", e$message), type = "error", duration = NULL)
            game_state$game <- NULL
            game_state$flipped <- FALSE
            game_state$last_loaded_pgn <- "" # Clear on error
          }
        )
      }
    },
    ignoreNULL = FALSE,
    priority = -1 # Lower priority so input$load_game_data processes first
  )

  # A simple reactive value to store the last PGN loaded by the button
  # This helps prevent the input$pgn_input observer from re-processing
  # when it's programmatically updated by input$load_game_data.
  observeEvent(input$load_game_data,
    {
      game_state$last_loaded_pgn <- input$load_game_data$pgn
    },
    ignoreNULL = FALSE
  )


  observeEvent(input$reset_board, {
    game_state$current_move_index <- 0
    # When resetting, maintain the current flip state, or reset to default based on preference.
    # For simplicity, let's keep the user's initial preference if they loaded a game as Black.
    # If you want it to always reset to White's view, set game_state$flipped <- FALSE here.
  })

  observeEvent(input$restart_to_end, {
    req(game_state$game)
    game_state$current_move_index <- total_game_moves()
    # Same as reset_board, maintain current flip state or reset to default.
    # For consistency with initial load, we're not touching flipped here.
  })

  observeEvent(input$prev_move, {
    req(game_state$game)
    if (game_state$current_move_index > 0) {
      game_state$current_move_index <- game_state$current_move_index - 1
    }
  })

  observeEvent(input$next_move, {
    req(game_state$game)
    if (game_state$current_move_index < total_game_moves()) {
      game_state$current_move_index <- game_state$current_move_index + 1
    }
  })

  observeEvent(input$flip, {
    game_state$flipped <- !game_state$flipped
  })

  # Render the chess board
  output$chess_board_output <- renderUI({
    req(game_state$game)
    current_game_node <- game_state$game
    if (game_state$current_move_index > 0) {
      current_game_node <- chess::forward(current_game_node, game_state$current_move_index)
    }

    # Generate the board text, applying the flip directly in the chess::print call
    # The 'flipped' argument in chess::print directly controls the orientation.
    board_text_lines <- capture.output(print(
      current_game_node,
      unicode = TRUE
    ))

    # Remove the first line (PGN header or similar, often blank)
    board_text_lines <- board_text_lines[-1]

    # Conditionally apply your custom flip_board function based on game_state$flipped
    if (game_state$flipped) {
      board_text_lines <- flip_board(board_text_lines)
    }

    # Render the processed lines using <pre> for monospaced font and preserved whitespace
    pre(paste(board_text_lines, collapse = "\n"), style = "font-size:40px; align:center;")
  })
  
  # A reactive expression to generate the move string
  output$chess_board_note <- renderText({
    req(game_state$game)
    
    # Check if we are at the end of the game
    is_end_of_game <- game_state$current_move_index == total_game_moves()
    
    # Get the current move and its move number
    current_move_node <- chess::forward(game_state$game, game_state$current_move_index)
    
    # Get the game result from the PGN headers if it's the last move
    game_result <- game_state$game$headers$get("Result")
    if (is_end_of_game) {
      user_result <- switch(
        game_result,
        "1-0" = "Win",
        "0-1" = "Checkmate",
        "1/2-1/2" = "Draw",
        "*" = "", # Game not finished
        "Other"
      )
    }
    
    # Format the output string
    move_number <- round((game_state$current_move_index + 0.5) / 2)
    move_text <- chess_move_name(current_move_node)
    if (is_end_of_game) {
      paste0(move_text, " (", user_result, ")")
    } else {
      move_text
    }
  })
}

shinyApp(ui = ui, server = server)
