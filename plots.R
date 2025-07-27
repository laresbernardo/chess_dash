# plots.R
# This file contains all plotting functions

#' Plot Rating Evolution over time
#'
#' @param df Dataframe containing game data.
#' @param username Chess.com username for the plot subtitle.
#' @return A ggplot object.
plot_rating_evolution <- function(df, username) {
  plot_data <- df %>%
    filter(!is.na(user_win)) %>%
    arrange(games.end_time) %>%
    mutate(game_index = row_number())
  filters <- head(freqs(plot_data, games.time_class, games.time_control), 3) %>%
    mutate(mix = paste(games.time_class, games.time_control))
  plot_data <- plot_data %>%
    mutate(
      mix = paste(games.time_class, games.time_control),
      mix = factor(mix, levels = filters$mix)
    )
  ggplot(plot_data, aes(x = game_index, y = user_rating)) +
    geom_point(alpha = 0.6, color = "darkblue") +
    facet_grid(mix ~ ., scales = "free") +
    labs(
      title = "Rating Evolution in per Time Control",
      subtitle = sprintf(
        "Player: %s | Period: %s to %s | Most frequent class + time control",
        username,
        format(min(plot_data$games.end_time), "%Y-%m-%d"),
        format(max(plot_data$games.end_time), "%Y-%m-%d")
      ),
      y = "Rating",
      x = "Game Number (Chronological)"
    ) +
    geom_smooth(method = "loess", se = TRUE, color = "red", linetype = "dashed") +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    theme_lares(legend = "top") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
}

#' Plot Difference in Ratings Between Players
#'
#' @param df Dataframe containing game data.
#' @param username Chess.com username for the plot subtitle.
#' @return A ggplot object.
plot_rating_diff <- function(df, username) {
  df %>%
    mutate(games.rated = factor(ifelse(games.rated, "Rated", "Not-Rated"), levels = c("Rated", "Not-Rated"))) %>%
    ggplot(aes(x = rating_diff)) +
    geom_histogram(aes(fill = user_win), bins = 50, color = "white", alpha = 0.8) +
    facet_grid(games.rated ~ ., scales = "free_y") +
    geom_vline(xintercept = 0, alpha = 0.6, linetype = "dotted", color = "darkgrey") +
    labs(
      title = "Difference in Ratings Between Players",
      subtitle = sprintf("Player: %s | Negative means you played higher rated opponents", username),
      x = "User Rating - Opponent's Rating",
      y = "Number of games",
      fill = "User won the game"
    ) +
    scale_y_abbr() +
    theme_lares(legend = "top") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
}

#' Plot Frequent Moves
#'
#' @param df Dataframe containing game data.
#' @param n_turns Number of turns to analyze.
#' @param top_moves Number of top moves to display.
#' @return A patchwork object of ggplot objects.
plot_frequent_moves <- function(df, n_turns = 3, top_moves = 8) {
  df$df.pgn <- lapply(df$games.pgn, function(x) pgn2data(x, "df")[[1]])
  firstn <- lapply(df$df.pgn, function(x) x[x$turn <= n_turns, ])
  firstn <- lapply(seq_along(firstn), function(i) filter(firstn[[i]], color == df$user_color[i]))
  first_df <- bind_rows(firstn)
  combs <- freqs(first_df, color, turn) %>% arrange(desc(color), turn)
  plots <- lapply(1:nrow(combs), function(i) {
    first_df[first_df$turn == combs$turn[i] & first_df$color == combs$color[i], ] %>%
      mutate(turn = paste0("#", turn)) %>%
      freqs(turn, color, move) %>%
      head(top_moves) %>%
      ggplot() +
      geom_col(aes(y = reorder(move, n), x = n, fill = color)) +
      facet_grid(. ~ turn, scales = "free") +
      scale_fill_manual(values = c("black" = "black", "white" = "grey90")) +
      labs(x = NULL, y = NULL, fill = NULL) +
      lares::scale_x_abbr() +
      lares::theme_lares(legend = "none")
  })
  patchwork::wrap_plots(plots, ncol = n_turns) &
    patchwork::plot_annotation(
      theme = lares::theme_lares(),
      title = sprintf(
        "%s most frequent moves on first %s turns by color",
        top_moves, n_turns
      )
    )
}

#' Plot Win Rate per Accuracy Range
#'
#' @param df Dataframe containing game data.
#' @param username Chess.com username for the plot subtitle.
#' @return A ggplot object.
plot_accuracy <- function(df, username) {
  df %>%
    filter(!is.na(user_accuracy)) %>%
    mutate(games.rated = factor(ifelse(games.rated, "Rated", "Not-Rated"), levels = c("Rated", "Not-Rated"))) %>%
    ggplot(aes(x = user_accuracy)) +
    geom_histogram(aes(fill = user_win), binwidth = 5, color = "white", alpha = 0.8) +
    facet_grid(games.rated ~ ., scales = "free_y") +
    labs(
      title = "Win Rate per Accuracy Range",
      subtitle = sprintf("Player: %s | Showing only accuracies reported", username),
      x = "User Accuracy",
      y = "Number of games",
      fill = "User won the game"
    ) +
    scale_y_abbr() +
    theme_lares(legend = "top") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))
}
