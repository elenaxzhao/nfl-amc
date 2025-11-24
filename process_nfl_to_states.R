################################################################################
# Process NFL Play-by-Play Data to State Transitions (n, x, s)
# 
# Transforms play-by-play data into a sequence of state transitions where:
#   n = play number (aggregated - only counts plays that change position)
#   x = field position (0, 1, 2, 3, 4 where 0 and 4 are endzones)
#   s = score differential (positive if possession team ahead, negative if behind)
################################################################################

library(tidyverse)
library(nflfastR)

##############################################################
### Helper Functions
##############################################################

yardline_to_x <- function(yardline_100) {
  # Maps yardline (0-100) to field position bin
  # 0 = own endzone, 100 = opponent endzone
  # Returns: 0, 1, 2, 3, or 4
  #   0 = own endzone (yardline 0-5)
  #   1 = close to own endzone (yardline 6-25)
  #   2 = midfield (yardline 26-75)
  #   3 = close to opponent endzone (yardline 76-94)
  #   4 = opponent endzone (yardline 95-100)
  case_when(
    yardline_100 <= 5 ~ 0L,           # Own endzone
    yardline_100 <= 25 ~ 1L,          # Close to own endzone
    yardline_100 <= 75 ~ 2L,          # Midfield
    yardline_100 <= 94 ~ 3L,          # Close to opponent endzone
    yardline_100 <= 100 ~ 4L,         # Opponent endzone
    TRUE ~ NA_integer_
  )
}

##############################################################
### Process NFL Data to State Transitions
##############################################################

process_nfl_to_states <- function(seasons = 2010:2023, 
                                  cache_file = NULL,
                                  min_yardline = 0,
                                  max_yardline = 100) {
  # Processes NFL play-by-play data into state transitions (n, x, s)
  #
  # Args:
  #   seasons: Vector of seasons to load (default: 2010-2023)
  #   cache_file: Optional path to cache file for loaded data
  #   min_yardline, max_yardline: Filter yardline range
  #
  # Returns:
  #   Data frame with columns: game_id, play_id, n, x, s, x_next, s_next
  
  cat("=== Processing NFL Data to State Transitions ===\n\n")
  
  # Load or use cached data
  if (!is.null(cache_file) && file.exists(cache_file)) {
    cat("Loading cached NFL data from", cache_file, "...\n")
    pbp_data = readRDS(cache_file)
  } else {
    cat("Loading NFL play-by-play data for seasons", 
        paste(range(seasons), collapse="-"), "...\n")
    pbp_data = load_pbp(seasons = seasons)
    
    if (!is.null(cache_file)) {
      cat("Saving cached data to", cache_file, "...\n")
      saveRDS(pbp_data, cache_file)
    }
  }
  
  cat("Loaded", nrow(pbp_data), "plays\n\n")
  
  # Filter to valid plays
  cat("Filtering to valid plays...\n")
  df = pbp_data %>%
    filter(
      !is.na(posteam),              # Must have possession team
      !is.na(yardline_100),          # Must have yardline
      play_type %in% c("run", "pass"),  # Only offensive plays
      !is.na(epa),                   # Must have EPA (valid play)
      down %in% c(1, 2, 3, 4),       # Valid down
      yardline_100 >= min_yardline,   # Yardline filter
      yardline_100 <= max_yardline
    ) %>%
    arrange(game_id, play_id) %>%
    select(game_id, play_id, posteam, defteam, yardline_100, 
           score_differential_post, yards_gained, touchdown, 
           play_type, down, season, week)
  
  cat("  Valid plays:", nrow(df), "\n\n")
  
  # Process each game separately
  cat("Processing games...\n")
  
  all_states = data.frame()
  
  for (game in unique(df$game_id)) {
    game_df = df %>% filter(game_id == game) %>% arrange(play_id)
    
    if (nrow(game_df) == 0) next
    
    # Get initial score differential (from possession team's perspective)
    # score_differential_post is from possession team's perspective
    # Positive = possession team ahead, Negative = possession team behind
    game_df = game_df %>%
      mutate(
        # Field position bin
        x = yardline_to_x(yardline_100),
        
        # Score differential from possession team's perspective
        # score_differential_post is already from posteam's perspective
        s = score_differential_post,
        
        # Get next state
        x_next = lead(x),
        s_next = lead(score_differential_post)
      )
    
    # Filter out plays with missing x
    game_df = game_df %>% filter(!is.na(x))
    
    if (nrow(game_df) == 0) next
    
    # Aggregate plays that don't change position (x stays the same)
    # Only count plays where x actually changes
    game_states = game_df %>%
      mutate(
        # Identify when position changes
        x_changed = (x != lag(x, default = first(x))) | (row_number() == 1),
        # Cumulative count of position changes (this becomes n)
        n = cumsum(x_changed)
      ) %>%
      # Only keep plays where position changed (or first play)
      filter(x_changed) %>%
      # Get next state for transitions
      mutate(
        x_next = lead(x),
        s_next = lead(s),
        # Remove plays where we can't determine next state
        has_next = !is.na(x_next)
      ) %>%
      filter(has_next) %>%
      select(game_id, play_id, n, x, s, x_next, s_next, 
             posteam, defteam, season, week, yards_gained, touchdown)
    
    if (nrow(game_states) > 0) {
      all_states = bind_rows(all_states, game_states)
    }
    
    if (length(unique(all_states$game_id)) %% 100 == 0) {
      cat("  Processed", length(unique(all_states$game_id)), "games...\n")
    }
  }
  
  cat("\nDone processing!\n")
  cat("  Total games:", length(unique(all_states$game_id)), "\n")
  cat("  Total state transitions:", nrow(all_states), "\n")
  cat("  Average transitions per game:", round(nrow(all_states) / length(unique(all_states$game_id)), 2), "\n\n")
  
  # Summary statistics
  cat("=== Summary Statistics ===\n")
  cat("Field position (x) distribution:\n")
  print(table(all_states$x, useNA = "ifany"))
  cat("\nScore differential (s) range:\n")
  cat("  Min:", min(all_states$s, na.rm = TRUE), "\n")
  cat("  Max:", max(all_states$s, na.rm = TRUE), "\n")
  cat("  Mean:", round(mean(all_states$s, na.rm = TRUE), 2), "\n")
  cat("\nPlay number (n) distribution:\n")
  cat("  Max n per game:", max(all_states$n, na.rm = TRUE), "\n")
  cat("  Mean n per game:", round(mean(all_states %>% group_by(game_id) %>% summarize(max_n = max(n)) %>% pull(max_n)), 2), "\n\n")
  
  return(all_states)
}

##############################################################
### Main Execution
##############################################################

if (!interactive()) {
  # If running as script, process data
  cat("Running as script...\n\n")
  
  # Process data from 2010 onwards
  df_states = process_nfl_to_states(
    seasons = 2010:2023,
    cache_file = "job_output/nfl_states_2010_2023.rds"
  )
  
  # Save results
  output_file = "job_output/nfl_state_transitions_2010_2023.csv"
  write_csv(df_states, output_file)
  cat("Saved state transitions to", output_file, "\n")
  
  # Also save as RDS for faster loading
  rds_file = "job_output/nfl_state_transitions_2010_2023.rds"
  saveRDS(df_states, rds_file)
  cat("Saved state transitions to", rds_file, "\n")
}

