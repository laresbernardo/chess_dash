# Load necessary libraries
library(shiny)
library(bslib) # For modern, nice-looking dashboard theme
library(lares) # For theme_lares() and freqs()
library(dplyr) # For data manipulation
library(ggplot2) # For plotting
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

# Helper function to generate YYYY/MM ranges from a date range
# This is now only used if the user explicitly selects a date range for fetching
# instead of 'all'. It's still good to have for granular fetching.
generate_month_ranges <- function(start_date, end_date) {
  start_year <- as.numeric(format(start_date, "%Y"))
  start_month <- as.numeric(format(start_date, "%m"))
  end_year <- as.numeric(format(end_date, "%Y"))
  end_month <- as.numeric(format(end_date, "%m"))

  ranges <- c()
  current_year <- start_year
  current_month <- start_month

  while (current_year < end_year || (current_year == end_year && current_month <= end_month)) {
    ranges <- c(ranges, sprintf("%d/%02d", current_year, current_month))
    current_month <- current_month + 1
    if (current_month > 12) {
      current_month <- 1
      current_year <- current_year + 1
    }
  }
  return(ranges)
}

global_data_description <- paste0(
  "Your name is 'Chess Insights AI'. You are an AI assistant specialized in analyzing Chess.com game data. ",
  "The data contains a player's chess game history with other real players. ",
  "Here's a description of the columns provided:\n",
  "- `games.uuid`: A unique identifier for each game provided by Chess.com.\n",
  "- `game_number`: A unique, sequential identifier for each game, where 1 is the first available game.\n",
  "- `games.end_time`: The date and time when the game ended.\n",
  "- `games.eco`: The ECO (Encyclopedia of Chess Openings) code and name for the opening played in the game.\n",
  "- `turns`: Number of turns per player, as in times each player moved pieces.\n",
  "- `user_color`: The color (white or black) the player used in that specific game.\n",
  "- `user_result`: The outcome of the game from the player's perspective (e.g., 'win', 'checkmated', 'resigned', 'draw').\n",
  "- `user_rating`: The player's rating at the end of this game.\n",
  "- `opponent_rating`: The opponent's rating at the end of this game.\n",
  "- `rating_diff`: The difference between the player's rating and the opponent's rating (`user_rating - opponent_rating`). Positive means the player had a higher rating than the opponent.\n",
  "- `user_accuracy`: The accuracy of the user's moves in the game.\n",
  "- `opponent_accuracy`: The accuracy of the opponent user's moves in the game.\n",
  "- `accuracy_diff`: user_accuracy - opponent_accuracy.\n",
  "- `user_win`: A logical value indicating if the player won the game (`TRUE` for win, `FALSE` for loss, `NA` for draws or other non-win/loss results).\n",
  "- `games.rated`: A logical value indicating if the game was rated (`TRUE`) or unrated (`FALSE`).\n",
  "- `games.url`: The URL to the game on Chess.com.\n",
  "- `games.pgn`: The game record in Portable Game Notation (PGN) format, including moves and game metadata.\n",
  "- `games.time_control`: The time control in seconds for the game (e.g., 600 for 10-minute games).\n",
  "- `games.tcn`: A unique identifier related to the game's time control and possibly other technical details (specific meaning from Chess.com API).\n",
  "- `games.initial_setup`: The FEN (Forsyth-Edwards Notation) string representing the board setup at the start of the game. For standard games, this will be the initial chess position.\n",
  "- `games.fen`: The FEN (Forsyth-Edwards Notation) string representing the board state at the end of the game or at the point where the game data was captured.\n",
  "- `games.time_class`: The time control classification of the game (e.g., 'rapid', 'blitz', 'bullet').\n",
  "- `games.rules`: The ruleset of the game (e.g., 'chess').\n",
  "- `games.accuracies.white`: The accuracy percentage of the white player's moves, as calculated by Chess.com's game analysis.\n",
  "- `games.accuracies.black`: The accuracy percentage of the black player's moves, as calculated by Chess.com's game analysis.\n",
  "- `games.white.rating`: The white player's rating at the end of the game.\n",
  "- `games.white.result`: The outcome of the game for the white player.\n",
  "- `games.white.@id`: The API URL for the white player's profile on Chess.com.\n",
  "- `games.white.username`: The username of the white player.\n",
  "- `games.white.uuid`: A unique identifier for the white player.\n",
  "- `games.black.rating`: The black player's rating at the end of the game.\n",
  "- `games.black.result`: The outcome of the game for the black player.\n",
  "- `games.black.@id`: The API URL for the black player's profile on Chess.com.\n",
  "- `games.black.username`: The username of the black player.\n",
  "- `games.black.uuid`: A unique identifier for the black player.\n",
  "- `games.start_time`: The date and time when the game started.\n\n",
  "When answering questions, focus on providing insights and summaries based on this data. ",
  "You can calculate win rates, average ratings, analyze performance against different rating ranges, ",
  "identify common openings, etc. If the user asks for data not present in this table, state that you cannot provide it. ",
  "In the queries, always use double quotes when calling column names given some of them have dots. ",
  "Avoid providing very long lists of raw data. Summarize when possible. ",
  "When referring to numerical values, use appropriate rounding or abbreviations if they are very large. ",
  "For counters, use integers without decimals. "
)

pgn2data <- function(games.pgn, format = "json") {
  lapply(games.pgn, function(x) {
    game <- data.frame(stringr::str_split(
      gsub("%clk ", "", stringr::str_split(x, "\n\n1. | \\d+... | \\d+. ")[[1]][-1]),
      " ",
      simplify = TRUE
    )[, -3])
    colnames(game) <- c("move", "timer")
    game$timer <- gsub("\\{\\[|\\]\\}", "", game$timer)
    game$turn <- rep(seq(1, 1 + nrow(game) / 2, by = 1), each = 2)[1:nrow(game)]
    game$color <- rep(c("white", "black"), length = nrow(game))
    if (format == "json") {
      jsonlite::toJSON(game)
    } else {
      game
    }
  })
}


# Define UI for the application
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly", font_scale = 0.9),
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
      # error_message is still useful for persistent errors
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
          "Chess Insights AI",
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
  creds <- fromJSON(read_encrypted("creds.enc", Sys.getenv("LARES_ENC")))
  login <- module_login(
    input, session,
    users = creds$user,
    pwds = creds$pass,
    style = list(botton_txt_colour = "#FFF", botton_bgd_colour = "#000"),
    logo = "icon.png",
    lang = "en",
    personal = "Bernardos-MacBook-Pro.local"
  )
  observe({
    if (login$authenticated) {
      message("User logged in successfully: ", login$user)
    }
  })

  rv <- reactiveValues(
    processed_games = NULL,
    loading = FALSE,
    error = NULL
  )

  # Conditionally render dateRangeInput based on radio button selection
  output$date_range_ui <- renderUI({
    if (input$data_fetch_option == "range") {
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range:",
        start = Sys.Date() - 365, # Default to 1 year ago
        end = Sys.Date(), # Default to today
        format = "yyyy-mm-dd",
        separator = "to"
      )
    }
  })

  # Reactive expression to get cached usernames
  cached_users <- reactive({
    cache_files <- list.files(cache_dir, pattern = "\\.rds$", full.names = FALSE)
    # Extract usernames from cache file names (e.g., "username_all.rds" or "username_20230101_20240101.rds")
    # This regex is simplified and assumes format "username_all" or "username_YYYYMMDD_YYYYMMDD"
    unique_usernames <- unique(gsub("_(all|\\d{8}_\\d{8})\\.rds$", "", cache_files))
    if (length(unique_usernames) > 0) {
      return(sort(unique_usernames))
    } else {
      return(NULL)
    }
  })

  # Render cache information
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

  # Observe event for resetting cache
  observeEvent(input$reset_cache, {
    showNotification("Clearing cache...", type = "warning", duration = 3)
    lares::cache_clear(cache_dir = cache_dir)
    rv$processed_games <- NULL # Clear current displayed data
    rv$error <- NULL # Clear any error messages
    showNotification("Cache cleared!", type = "default", duration = 3)
    # Re-render cache info after clearing
    output$cache_info <- renderUI({
      tagList(p("No cached data available."))
    })
  })

  observeEvent(input$fetch_data, {
    req(input$chess_username)

    rv$loading <- TRUE
    rv$error <- NULL
    rv$processed_games <- NULL # Clear previous data

    # Show loading notification
    id <- showNotification(
      HTML('<i class="fas fa-spinner fa-spin"></i> Fetching data...'),
      duration = NULL,
      type = "warning",
      closeButton = FALSE
    )

    current_username <- input$chess_username
    fetch_option <- input$data_fetch_option
    selected_start_date <- if (fetch_option == "range") input$date_range[1] else as.Date("1970-01-01") # If 'all', use a very old date
    selected_end_date <- if (fetch_option == "range") input$date_range[2] else Sys.Date() # If 'all', use today's date

    # Construct a cache key based on username and the effective date range for the data
    # For "all" games, we'll cache the entire dataset and then filter it.
    # The cache key needs to reflect the content of the cached file.
    # If fetch_option is "all", the key is "username_all".
    # If fetch_option is "range", the key is "username_startdate_enddate".
    cache_key <- if (fetch_option == "all") {
      paste0(current_username, "_all")
    } else {
      paste0(current_username, "_", format(selected_start_date, "%Y%m%d"), "_", format(selected_end_date, "%Y%m%d"))
    }

    future_promise({
      fetched_data <- NULL
      # Check cache first
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
        } else { # fetch_option == "range"
          month_ranges_to_fetch <- generate_month_ranges(selected_start_date, selected_end_date)
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

        # If data was fetched, cache it before processing
        if (!is.null(fetched_data) && nrow(fetched_data) > 0) {
          cache_write(fetched_data, base = cache_key, cache_dir = cache_dir)
          message(sprintf("Data for %s cached successfully: %s", current_username, cache_key))
        }
      }

      processed_data <- NULL
      if (!is.null(fetched_data) && nrow(fetched_data) > 0) {
        processed_data <- fetched_data %>%
          mutate(
            game_number = rev(row_number()),
            games.end_time = as.POSIXct(games.end_time, origin = "1970-01-01", tz = "UTC"),
            games.eco = gsub("-|\\.|\\.\\.\\.", " ", gsub(".*openings/", "", games.eco)),
            turns = unlist(lapply(pgn2data(games.pgn, "df"), function(x) nrow(x) / 2)),
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
          arrange(desc(games.end_time))
      }
      list(data = processed_data, error = NULL)
    }) %...>%
      {
        rv$processed_games <- .$data
        rv$error <- .$error
        rv$loading <- FALSE
        removeNotification(id) # Remove loading notification
        querychat_config_global <- querychat_init(
          df = rv$processed_games,
          table_name = "chess_games_data",
          greeting = "Ask Chess Insights AI anything related to your games...",
          data_description = global_data_description,
          # create_chat_func = purrr::partial(
          #   ellmer::chat_openai,
          #   model = "gpt-4.1",
          #   api_key = Sys.getenv("OPENAI_API") # Ensure API key is set as env var
          # )
          create_chat_func = purrr::partial(
            ellmer::chat_ollama,
            model = "llama3.2"
          )
        )
        rv$querychat <- querychat_server("chess_chat", querychat_config_global)
        if (is.null(rv$processed_games) || nrow(rv$processed_games) == 0) {
          rv$error <- "No games found for the selected username and date range."
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
      } %...!% {
        rv$error <- "An unexpected error occurred during data fetching. Please check the username or try again later."
        rv$loading <- FALSE
        removeNotification(id) # Remove loading notification on error
      }
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
    div(
      class = "text-center",
      h2(latest_rating),
      p("Latest rating*")
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
    plot_data <- rv$querychat$df() %>%
      filter(!is.na(user_win)) %>%
      arrange(games.end_time) %>%
      mutate(game_index = row_number())
    time_control <- freqs(plot_data$games.time_control)[1:3, ]
    plot_data <- filter(plot_data, .data$games.time_control %in% time_control$values) %>%
      mutate(games.time_control = factor(.data$games.time_control, levels = time_control$values))
    ggplot(plot_data, aes(x = game_index, y = user_rating)) +
      geom_point(alpha = 0.6, color = "darkblue") +
      facet_grid(games.time_control ~ ., scales = "free") +
      labs(
        title = "Rating Evolution in per Time Control",
        subtitle = sprintf(
          "Player: %s | Period: %s to %s",
          isolate(input$chess_username),
          format(min(plot_data$games.end_time), "%Y-%m-%d"), # Use actual min/max of filtered data
          format(max(plot_data$games.end_time), "%Y-%m-%d")
        ),
        y = "Rating",
        x = "Game Number (Chronological)"
      ) +
      geom_smooth(method = "loess", se = TRUE, color = "red", linetype = "dashed") +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
      theme_lares(legend = "top") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  })

  output$rating_diff_plot <- renderPlot({
    rv$querychat$df() %>%
      mutate(games.rated = factor(ifelse(games.rated, "Rated", "Not-Rated"), levels = c("Rated", "Not-Rated"))) %>%
      ggplot(aes(x = rating_diff)) +
      geom_histogram(aes(fill = user_win), bins = 50, color = "white", alpha = 0.8) +
      facet_grid(games.rated ~ ., scales = "free_y") +
      geom_vline(xintercept = 0, alpha = 0.6, linetype = "dotted", color = "darkgrey") +
      labs(
        title = "Difference in Ratings Between Players",
        subtitle = sprintf("Player: %s | Negative means you played higher rated opponents", isolate(input$chess_username)),
        x = "User Rating - Opponent's Rating",
        y = "Number of games",
        fill = "User won the game"
      ) +
      scale_y_abbr() +
      theme_lares(legend = "top") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  })

  output$accuracy_plot <- renderPlot({
    rv$querychat$df() %>%
      filter(!is.na(user_accuracy)) %>%
      mutate(games.rated = factor(ifelse(games.rated, "Rated", "Not-Rated"), levels = c("Rated", "Not-Rated"))) %>%
      ggplot(aes(x = user_accuracy)) +
      geom_histogram(aes(fill = user_win), binwidth = 5, color = "white", alpha = 0.8) +
      facet_grid(games.rated ~ ., scales = "free_y") +
      labs(
        title = "Win Rate per Accuracy Range",
        subtitle = sprintf("Player: %s | Showing only accuracies reported", isolate(input$chess_username)),
        x = "User Accuracy",
        y = "Number of games",
        fill = "User won the game"
      ) +
      scale_y_abbr() +
      theme_lares(legend = "top") +
      theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
  })

  output$recent_games_table <- renderDT({
    df <- rv$querychat$df()
    colnames(df) <- stringr::str_to_title(
      gsub("_", " ", cleanNames(gsub("games.", "", colnames(df))))
    )
    df %>%
      mutate(
        `End Time` = format(`End Time`, "%Y-%m-%d %H:%M"),
        `Url` = paste0("<a href='", `Url`, "' target='_blank'>Link</a>")
      ) %>%
      select(-any_of(c(
        "Pgn", "Tcn", "Initial Setup", "Fen", "White",
        "Black", "Uuid", "Game Number", "Start Time", "Accuracies"
      ))) %>%
      datatable(
        .,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          autoWidth = TRUE,
          dom = "Bfrtip"
        ),
        escape = FALSE,
        rownames = FALSE,
        filter = "top",
        selection = "none"
      )
  })
}

shinyApp(ui = ui, server = server)
