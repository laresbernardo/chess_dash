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
        label = "Chess.com Username:",
        value = "laresdj", # Default username
        placeholder = "e.g., laresdj"
      ),
      radioButtons(
        inputId = "data_fetch_option",
        label = "Data Fetch Option:",
        choices = c("Fetch All Games" = "all", "Select Date Range" = "range"),
        selected = "all" # Default to fetching all games
      ),
      # Date range input, conditionally shown
      uiOutput("date_range_ui"),
      actionButton(
        inputId = "fetch_data",
        label = "Fetch Data",
        icon = icon("download"),
        class = "btn-primary"
      ),
      hr(),
      # Cache information and reset button
      uiOutput("cache_info"), # Placeholder for cache info
      actionButton(
        inputId = "reset_cache",
        label = "Reset Cache",
        icon = icon("broom"),
        class = "btn-danger"
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
          htmlOutput("chess_board_output")
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
    loading = FALSE,
    error = NULL
  )

  # --- Reactive Values for Chess Game Viewer ---
  game_state <- reactiveValues(
    game = NULL, # Stores the chess game object (root GameNode) parsed from PGN
    current_move_index = 0, # Tracks the current move being displayed (0 for initial board)
    flipped = FALSE # Boolean to toggle board orientation (FALSE for white's view, TRUE for black's)
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

  cached_users <- reactive({
    cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = FALSE)
    unique_usernames <- unique(gsub("_(all|\\d{8}_\\d{8})\\.rds$", "", cache_files))
    if (length(unique_usernames) > 0) {
      return(sort(unique_usernames))
    } else {
      return(NULL)
    }
  })

  output$cache_info <- renderUI({
    caches <- lares::cache_exists(cache_dir = cache_dir)
    if (isTRUE(caches)) {
      users <- gsub("lares_cache_|\\.RDS", "", attr(caches, "base"))
      tagList(
        p(HTML(paste0("<b>Cache available for users/dates:</b> ", paste(users, collapse = ", "))))
      )
    } else {
      tagList(p("No cached data available."))
    }
  })

  observeEvent(input$reset_cache, {
    showNotification("Clearing cache...", type = "warning", duration = 3)
    lares::cache_clear(cache_dir = cache_dir)
    rv$processed_games <- NULL
    rv$error <- NULL
    showNotification("Cache cleared!", type = "default", duration = 3)
    output$cache_info <- renderUI({
      tagList(p("No cached data available."))
    })
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

    current_username <- input$chess_username
    fetch_option <- input$data_fetch_option
    selected_start_date <- if (fetch_option == "range") input$date_range[1] else as.Date("1970-01-01")
    # Only consider starting month completely given the API returns whole months
    selected_start_date <- format(selected_start_date, "%Y%m")
    selected_end_date <- if (fetch_option == "range") input$date_range[2] else Sys.Date()

    cache_key <- if (fetch_option == "all") {
      paste0(current_username, "_all")
    } else {
      paste0(current_username, "_", selected_start_date, "_", format(selected_end_date, "%Y%m%d"))
    }

    # Fetch data
    fetched_data <- NULL
    if (cache_exists(base = cache_key, cache_dir = cache_dir)) {
      fetched_data <- cache_read(base = cache_key, cache_dir = cache_dir)
      message(sprintf("Data for %s read from cache: %s", current_username, cache_key))
    } else {
      message(sprintf("Fetching data for %s (option: %s)...", current_username, fetch_option))
      if (fetch_option == "all") {
        fetched_data <- tryCatch(
          {
            chessPI.pull(current_username, range = "all")
          },
          error = function(e) {
            message(sprintf("Error fetching 'all' data for %s: %s", current_username, e$message))
            return(NULL)
          }
        )
      } else {
        month_ranges_to_fetch <- generate_month_ranges(input$date_range[1], input$date_range[2])
        all_monthly_games <- list()
        for (month_range in month_ranges_to_fetch) {
          monthly_games <- tryCatch(
            {
              chessPI.pull(current_username, range = month_range)
            },
            error = function(e) {
              message(sprintf("Error fetching data for %s/%s: %s", current_username, month_range, e$message))
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
        cache_write(fetched_data, base = cache_key, cache_dir = cache_dir)
        message(sprintf("Data for %s cached successfully: %s", current_username, cache_key))
      }
    }

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
          user_color = ifelse(games.white$username == current_username, "white", "black"),
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

    caches <- lares::cache_exists(cache_dir = cache_dir)
    output$cache_info <- renderUI({
      users <- gsub("lares_cache_|\\.RDS", "", attr(caches, "base"))
      if (!is.null(users)) {
        tagList(
          p(HTML(paste0("<b>Cache available for users/dates:</b> ", paste(users, collapse = ", ")))),
        )
      } else {
        tagList(p("No cached data available."))
      }
    })

    if (cache_exists(base = cache_key, cache_dir = cache_dir)) {
      showNotification(sprintf(
        "Data for %s read from cache", cache_key
      ), type = "default", duration = 3)
    }
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
    plot_rating_evolution(rv$querychat$df(), isolate(input$chess_username))
  })
  
  output$rating_diff_plot <- renderPlot({
    plot_rating_diff(rv$querychat$df(), isolate(input$chess_username))
  })

  output$freq_moves_plot <- renderPlot({
    plot_frequent_moves(rv$querychat$df(), n_turns = 3, top_moves = 8)
  })

  output$accuracy_plot <- renderPlot({
    plot_accuracy(rv$querychat$df(), isolate(input$chess_username))
  })

  output$recent_games_table <- renderDT({
    df <- rv$querychat$df() %>%
      rowwise() %>%
      mutate(
        load_pgn = paste0(
          '<button id="load_button_', row_number(),
          '" type="button" class="btn btn-primary btn-sm load-game-button" data-row-pgn="',
          htmltools::htmlEscape(clean_pgn(`games.pgn`)), '">Load</button>'
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
            // Send the PGN to the server.
            // The input$load_pgn_data event will be triggered with the PGN value.
            Shiny.setInputValue('load_pgn_data', pgn, {priority: 'event'});
          });
        ")
    )
  })

  # --- Chess Game Viewer Logic (from second app) ---

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
  observeEvent(input$load_pgn_data, {
    # Clean the PGN before updating the textarea and parsing
    cleaned_pgn <- input$load_pgn_data
    updateTextAreaInput(session, "pgn_input", value = cleaned_pgn)
  })

  # Observer for changes in the PGN input text area (manual or from 'Load' button)
  observeEvent(input$pgn_input,
    {
      req(input$pgn_input)

      # Use the cleaned PGN directly from the input for parsing
      tryCatch(
        {
          game <- read_game(file = textConnection(input$pgn_input))
          game_state$game <- game
          game_state$current_move_index <- total_game_moves() # Go to end of game by default
          game_state$flipped <- FALSE
          showNotification("PGN loaded successfully!", type = "message", duration = 5)
        },
        error = function(e) {
          showNotification(paste("Error parsing PGN:", e$message), type = "error", duration = NULL)
          game_state$game <- NULL
        }
      )
    },
    ignoreNULL = FALSE
  )

  observeEvent(input$reset_board, {
    game_state$current_move_index <- 0
    game_state$flipped <- FALSE
  })

  observeEvent(input$restart_to_end, {
    req(game_state$game)
    game_state$current_move_index <- total_game_moves()
    game_state$flipped <- FALSE
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

    # 1. Get the board lines without any color inversion from chess::print
    # We still use unicode = TRUE for nice piece representation
    board_text_lines <- capture.output(print(
      current_game_node,
      unicode = TRUE,
      invert_color = FALSE # Always get the board with default (white at bottom) colors
    ))

    # Remove the first line (PGN header or similar)
    board_text_lines <- board_text_lines[-1]

    # 2. Conditionally apply your custom flip_board function based on game_state$flipped
    if (game_state$flipped) {
      board_text_lines <- flip_board(board_text_lines)
    }

    # 3. Render the processed lines
    pre(paste(board_text_lines, collapse = "\n"), style = "font-size:40px; align:center;")
  })
}

shinyApp(ui = ui, server = server)
