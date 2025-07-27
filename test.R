# Load necessary libraries
library(shiny)
library(chess) # Make sure you have this package installed: install.packages("chess")
library(magrittr) # For the pipe operator %>%

# Define the User Interface (UI) for the Shiny application
ui <- fluidPage(

  # Application title
  titlePanel(div(class = "titlePanel", "Interactive Chess Game Viewer")),

  # Sidebar layout with input and output sections
  sidebarLayout(
    sidebarPanel(
      # Text area for PGN input
      textAreaInput("pgn_input", "Enter PGN String:",
        value = "1. e4 e5 2. Nf3 Nc6 3. Bb5 a6 4. Ba4 Nf6 5. O-O Be7 6. Re1 b5 7. Bb3 O-O 8. c3 d5 9. exd5 Nxd5 10. Nxe5 Nxe5 11. Rxe5 c6 12. d4 Bd6 13. Re1 Qh4 14. g3 Qh3 15. Bxd5 cxd5 16. Qf3 Be6 17. Bf4 Bxf4 18. Qxf4 Rae8 19. Nd2 f6 20. Nf3 Bf7 21. Rxe8 Rxe8 22. Re1 Rxe1+ 23. Nxe1 Qe6 24. Nd3 g5 25. Qb8+ Kg7 26. Qxb5 Qe4 27. Nb4 Qb1+ 28. Kg2 Bg6 29. Nxd5 Be4+ 30. Kh3 Qf1+ 31. Kg4 f5+ 32. Kxg5 Qxb5 33. Nf6 h6+ 34. Kh4 Kxf6 35. Kh5 Kg7 36. Kh4 Qxb2 37. g4 Qxf2+ 38. Kh5 Qxh2#",
        rows = 12, # Increased rows for more PGN visibility
        placeholder = "Paste your PGN string here..."
      ),
      hr(), # Horizontal rule for visual separation

      # Button group container for centering
      div(
        class = "button-group", align = "center",
        # Action buttons with icons
        actionButton("reset_board", icon("angles-left"), class = "btn btn-primary"),
        actionButton("prev_move", icon("angle-left"), class = "btn btn-info"),
        actionButton("next_move", icon("angle-right"), class = "btn btn-info"),
        actionButton("restart_to_end", icon("angles-right"), class = "btn btn-primary"),
        actionButton("flip_board", icon("arrows-rotate"), class = "btn btn-warning")
      )
    ),

    # Main panel for displaying the chess board
    mainPanel(
      htmlOutput("chess_board_output")
    )
  )
)

# Define server logic required to draw the chess board and handle interactions
server <- function(input, output, session) {
  # Reactive values are used to store and manage the application's state
  # These values automatically trigger updates in the UI when they change
  game_state <- reactiveValues(
    game = NULL, # Stores the chess game object (root GameNode) parsed from PGN
    current_move_index = 0, # Tracks the current move being displayed (0 for initial board)
    flipped = FALSE # Boolean to toggle board orientation (FALSE for white's view, TRUE for black's)
  )

  # Helper function to get the total number of full moves in the game
  # This function traverses the game tree to count moves, avoiding direct attribute access issues.
  get_total_moves <- function(game_node) {
    if (is.null(game_node)) {
      return(0)
    }
    count <- 0
    temp_node <- game_node
    # Loop until advancing one move forward returns NULL (end of mainline)
    # The 'forward' function advances by one move from the current node
    while (!is.null(chess::forward(temp_node, 1))) {
      temp_node <- chess::forward(temp_node, 1)
      count <- count + 1
    }
    return(count)
  }

  # Reactive expression to store the total number of moves in the game
  # This will re-calculate only when game_state$game changes (i.e., new PGN loaded)
  total_game_moves <- reactive({
    req(game_state$game) # Ensure game_state$game is not NULL
    get_total_moves(game_state$game)
  })

  # Observer for changes in the PGN input text area
  # This block runs whenever the user types or pastes a new PGN string
  observeEvent(input$pgn_input,
    {
      req(input$pgn_input) # Ensure that the PGN input is not empty before proceeding

      # Use tryCatch for robust error handling during PGN parsing
      tryCatch(
        {
          # Attempt to parse the PGN string into a 'game' object (GameNode)
          game <- read_game(file = textConnection(input$pgn_input))
          game_state$game <- game # Store the parsed game object (root GameNode)
          # Set current_move_index to the last move when a new PGN is loaded
          game_state$current_move_index <- total_game_moves()
          game_state$flipped <- FALSE # Reset view to white's perspective by default
          showNotification("PGN loaded successfully!", type = "message", duration = 3) # User feedback
        },
        error = function(e) {
          # If an error occurs during parsing, display a notification and clear the game state
          # This can happen if the PGN is malformed or incomplete
          showNotification(paste("Error parsing PGN:", e$message), type = "error", duration = 5)
          game_state$game <- NULL # Clear the game object to prevent further errors
        }
      )
    },
    ignoreNULL = FALSE
  ) # Process the initial default value of pgn_input when the app starts

  # Observer for the "Reset Board to Start" button
  observeEvent(input$reset_board, {
    game_state$current_move_index <- 0 # Set index back to 0 for the starting position
    game_state$flipped <- FALSE # Ensure view is white's perspective
  })

  # Observer for the "Restart Board to End" button
  observeEvent(input$restart_to_end, {
    req(game_state$game) # Ensure game object is loaded
    game_state$current_move_index <- total_game_moves() # Set index to the last move
    game_state$flipped <- FALSE # Reset view to white's perspective
  })

  # Observer for the "Previous Move" button
  observeEvent(input$prev_move, {
    req(game_state$game) # Ensure game object is loaded
    # Check if there are previous moves to go back to
    if (game_state$current_move_index > 0) {
      game_state$current_move_index <- game_state$current_move_index - 1 # Decrement move index
    }
  })

  # Observer for the "Next Move" button
  observeEvent(input$next_move, {
    req(game_state$game) # Ensure game object is loaded
    # Check if there are more moves to advance to using the reactive total_game_moves
    if (game_state$current_move_index < total_game_moves()) {
      game_state$current_move_index <- game_state$current_move_index + 1 # Increment move index
    }
  })

  # Render the chess board dynamically as text
  output$chess_board_output <- renderUI({
    req(game_state$game) # Require a game object (GameNode) to be loaded

    # Start with the root GameNode
    current_game_node <- game_state$game

    # Navigate forward to the desired move index
    # 'forward' function advances by 'n' full moves from the given node
    if (game_state$current_move_index > 0) {
      current_game_node <- chess::forward(current_game_node, game_state$current_move_index)
    }

    # Capture the print output of the board, using 'flipped' argument directly
    # This should correctly handle the visual flipping of ranks and pieces.
    board_text_lines <- capture.output(print(current_game_node,
      unicode = TRUE,
      invert_color = game_state$flipped
    ))

    # Remove the first line which typically contains game node info like "<Start>" or "<1. e4>"
    # This line is not part of the visual board representation.
    board_text_lines <- board_text_lines[-1]

    # Join the lines with newline characters and wrap in <pre> tags for HTML rendering
    pre(paste(board_text_lines, collapse = "\n"), style = "font-size:40px")
  })
}

# Run the Shiny application
shinyApp(ui, server)
