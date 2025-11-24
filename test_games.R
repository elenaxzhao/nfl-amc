# Analyze multiple games in test set: calculate win probabilities and visualize

library(tidyverse)
library(ggplot2)
library(gridExtra)
source('build_absorbing_markov_chain.R')

# Load Markov chain
cat("Loading Markov chain...\n")
markov_chain = readRDS("job_output/absorbing_markov_chain.rds")

# Load test data
cat("Loading test data...\n")
df_test = read_csv("job_output/test.csv", show_col_types = FALSE)

# Get first 4 games
game_ids = unique(df_test$game_id)[1:4]
cat("Analyzing games:", paste(game_ids, collapse = ", "), "\n\n")

# Analyze each game
for (game_idx in 1:length(game_ids)) {
  game_id = game_ids[game_idx]
  cat("=== Game", game_idx, ":", game_id, "===\n")
  
  df_game = df_test %>%
    filter(game_id == !!game_id) %>%
    arrange(n)
  
  cat("  Plays:", nrow(df_game), "\n")
  
  # Calculate win probability for each state
  df_game = df_game %>%
    rowwise() %>%
    mutate(
      wp = get_win_probability(markov_chain, n, x, s, possession)
    ) %>%
    ungroup()
  
  # Prepare data for plotting
  df_plot = df_game %>%
    mutate(
      play_num = n,
      state_label = paste0("(x=", x, ", s=", s, ", p=", possession, ")"),
      x_factor = factor(x, levels = 1:3, labels = c("x=1 (own endzone)", "x=2 (midfield)", "x=3 (opp endzone)")),
      p_label = ifelse(possession == 1, "Team 1 has ball", "Team 2 has ball")
    )
  
  # Plot 1: Win Probability over plays
  p1 = ggplot(df_plot, aes(x = play_num, y = wp)) +
    geom_line(color = "steelblue", linewidth = 1.2) +
    geom_point(aes(color = factor(x)), size = 3, alpha = 0.7) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray50") +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = seq(1, 20, 2)) +
    scale_color_manual(
      name = "Position",
      values = c("1" = "red", "2" = "orange", "3" = "green"),
      labels = c("1" = "x=1", "2" = "x=2", "3" = "x=3")
    ) +
    labs(
      title = paste("Win Probability Over Time -", game_id),
      subtitle = "Team 1's win probability (s is from team 1's perspective)",
      x = "Play Number (n)",
      y = "Win Probability",
      color = "Position (x)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11),
      legend.position = "bottom"
    )
  
  # Plot 2a: Position
  p2a = ggplot(df_plot, aes(x = play_num, y = x)) +
    geom_line(color = "red", linewidth = 1.2) +
    geom_point(color = "red", size = 3) +
    scale_y_continuous(breaks = 1:3, labels = c("x=1\n(own endzone)", "x=2\n(midfield)", "x=3\n(opp endzone)")) +
    scale_x_continuous(breaks = seq(1, 20, 2)) +
    labs(title = "Position (x) Over Time", x = "Play Number (n)", y = "Position") +
    theme_minimal()
  
  # Plot 2b: Score
  p2b = ggplot(df_plot, aes(x = play_num, y = s)) +
    geom_line(color = "blue", linewidth = 1.2) +
    geom_point(color = "blue", size = 3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    scale_y_continuous(breaks = -3:3, limits = c(-3.5, 3.5)) +
    scale_x_continuous(breaks = seq(1, 20, 2)) +
    labs(title = "Score Differential (s) Over Time", 
         subtitle = "s > 0: Team 1 ahead, s < 0: Team 2 ahead",
         x = "Play Number (n)", y = "Score Differential") +
    theme_minimal()
  
  # Plot 2c: Possession
  p2c = ggplot(df_plot, aes(x = play_num, y = possession)) +
    geom_line(color = "green", linewidth = 1.2) +
    geom_point(aes(color = factor(possession)), size = 3) +
    scale_y_continuous(breaks = c(-1, 1), labels = c("Team 2\nhas ball", "Team 1\nhas ball")) +
    scale_x_continuous(breaks = seq(1, 20, 2)) +
    scale_color_manual(values = c("-1" = "darkgreen", "1" = "lightgreen"), guide = "none") +
    labs(title = "Possession (p) Over Time", x = "Play Number (n)", y = "Possession") +
    theme_minimal()
  
  # Combined plot
  combined = grid.arrange(
    p1,
    arrangeGrob(p2a, p2b, p2c, ncol = 3),
    nrow = 2,
    heights = c(1, 0.8),
    top = paste("Game Analysis:", game_id),
    bottom = "Top: Win Probability | Bottom: State Variables (Position, Score, Possession)"
  )
  
  # Save plot
  output_file = paste0("job_output/test_game_", game_idx, "_analysis.png")
  ggsave(output_file, combined, width = 14, height = 10, dpi = 300)
  cat("  Saved plot to:", output_file, "\n")
  
  # Save detailed data
  output_csv = paste0("job_output/test_game_", game_idx, "_detailed.csv")
  write_csv(df_game, output_csv)
  cat("  Saved data to:", output_csv, "\n")
  
  # Summary
  cat("\n  Summary:\n")
  cat("    Starting WP:", round(df_game$wp[1], 4), "\n")
  cat("    Ending WP:", round(df_game$wp[nrow(df_game)], 4), "\n")
  cat("    WP Range:", round(min(df_game$wp), 4), "to", round(max(df_game$wp), 4), "\n")
  cat("    Final Score (s):", df_game$s[nrow(df_game)], "\n")
  cat("    Outcome:", ifelse(df_game$s[nrow(df_game)] > 0, "Team 1 wins", 
                            ifelse(df_game$s[nrow(df_game)] < 0, "Team 2 wins", "Tie")), "\n\n")
}

cat("=== All Games Analyzed ===\n")
cat("Plots saved to:\n")
for (i in 1:length(game_ids)) {
  cat("  Game", i, ":", paste0("job_output/test_game_", i, "_analysis.png"), "\n")
}

