# Extract PGN data in JSON or dataframe format
pgn2data <- function(games.pgn, format = "json") {
  lapply(games.pgn, function(x) {
    game <- data.frame(stringr::str_split(
      gsub("%clk ", "", stringr::str_split(x, "\\]\n\n1. | \\d+... | \\d+. ")[[1]][-1]),
      " ",
      simplify = TRUE
    )[, 1:2])
    colnames(game) <- c("move", "timer")
    game <- game[!grepl("Timezone", game$move), ]
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

clean_pgn <- function(pgn_string) {
  pgn_string <- stringr::str_split(pgn_string, "\n\n", n = 2)[[1]][2]
  pgn_string <- gsub("\\{\\[%clk [0-9:.]+\\]\\}", "", pgn_string, perl = TRUE)
  pgn_string <- gsub("\\{\\[%[a-zA-Z]+ [^\\]]+\\]\\}", "", pgn_string, perl = TRUE)
  pgn_string <- gsub("\\{[^\\}]*\\}", "", pgn_string, perl = TRUE)
  pgn_string <- trimws(pgn_string) # Trim leading/trailing whitespace
  return(pgn_string)
}

flip_board <- function(s) {
  s <- rev(s)
  unlist(lapply(s, function(x) {
    s_split <- strsplit(x, "")[[1]]
    s_reversed <- rev(s_split)
    paste(s_reversed, collapse = "")
  }))
}

# Generate YYYY/MM ranges from a date range
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