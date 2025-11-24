# Estimate Transition Probability Matrix for Absorbing Markov Chain
# State space: (n, x, s, p)
#   n: play number (1-20)
#   x: position (1, 2, 3)
#   s: score differential from possession team's perspective (-3 to +3)
#   p: possession (1 or -1)

library(tidyverse)
library(Matrix)

# ============================================================================
# SYMMETRY FUNCTION
# ============================================================================
# For every transition (n, x, s, p) -> (n+1, x_next, s_next, p_next),
# we also count the symmetric transition from the opposite team's perspective:
# (n, 4-x, -s, -p) -> (n+1, 4-x_next, -s_next, -p_next)
# This doubles our training data and enforces symmetry in the model.

get_symmetric_state <- function(n, x, s, p) {
  list(
    n = n,
    x = 4 - x,  # Flip position: 1 <-> 3, 2 stays 2
    s = -s,     # Flip score differential
    p = -p      # Flip possession
  )
}

# ============================================================================
# CONSTRAINT CHECKING
# ============================================================================
# Check if a transition is valid given the constraints

is_valid_transition <- function(n, x, s, p, n_next, x_next, s_next, p_next) {
  # Play number must increment by 1
  if (n_next != n + 1) return(FALSE)
  
  # Play number cannot exceed 20
  if (n_next > 20) return(FALSE)
  
  # Position must be 1, 2, or 3
  if (!x_next %in% 1:3) return(FALSE)
  
  # Score differential must be in valid range
  if (!s_next %in% -3:3) return(FALSE)
  
  # Possession must be 1 or -1
  if (!p_next %in% c(-1, 1)) return(FALSE)
  
  # Score direction constraint: if p=1, s_next >= s; if p=-1, s_next <= s
  # (from possession team's perspective, score can only stay same or improve)
  if (p == 1 && s_next < s) return(FALSE)
  if (p == -1 && s_next > s) return(FALSE)
  
  # When possession flips, the perspective changes
  # If p flips to -p, then s_next should be -s + delta (where delta is the actual score change)
  # This is more complex, so we'll be more lenient here and let the data speak
  
  return(TRUE)
}

# ============================================================================
# EXTRACT TRANSITIONS FROM DATA
# ============================================================================

extract_transitions <- function(df) {
  cat("Extracting transitions from data...\n")
  
  # Get transitions within each game
  df_trans = df %>%
    group_by(game_id) %>%
    arrange(n) %>%
    mutate(
      n_next = lead(n),
      x_next = lead(x),
      s_next = lead(s),
      p_next = lead(possession)
    ) %>%
    filter(!is.na(n_next)) %>%
    select(game_id, n, x, s, p = possession, n_next, x_next, s_next, p_next) %>%
    ungroup()
  
  cat("  Original transitions:", nrow(df_trans), "\n")
  
  # Filter to valid transitions
  df_trans_valid = df_trans %>%
    rowwise() %>%
    filter(is_valid_transition(n, x, s, p, n_next, x_next, s_next, p_next)) %>%
    ungroup()
  
  cat("  Valid transitions:", nrow(df_trans_valid), "\n")
  
  # Add symmetric transitions
  df_trans_symmetric = df_trans_valid %>%
    bind_rows(
      df_trans_valid %>%
        mutate(
          # Original state -> symmetric state
          n_sym = n,
          x_sym = 4 - x,
          s_sym = -s,
          p_sym = -p,
          # Next state -> symmetric next state
          n_next_sym = n_next,
          x_next_sym = 4 - x_next,
          s_next_sym = -s_next,
          p_next_sym = -p_next
        ) %>%
        select(game_id, 
               n = n_sym, x = x_sym, s = s_sym, p = p_sym,
               n_next = n_next_sym, x_next = x_next_sym, 
               s_next = s_next_sym, p_next = p_next_sym)
    )
  
  cat("  Total transitions (with symmetry):", nrow(df_trans_symmetric), "\n")
  cat("  (Effectively doubled the data)\n\n")
  
  return(df_trans_symmetric)
}

# ============================================================================
# ESTIMATE TRANSITION PROBABILITIES
# ============================================================================

estimate_transition_probs <- function(df_trans, smoothing = TRUE, alpha = 1.0) {
  cat("Estimating transition probabilities...\n")
  
  # Count transitions for each (current_state, next_state) pair
  transition_counts = df_trans %>%
    count(n, x, s, p, n_next, x_next, s_next, p_next, name = 'count')
  
  cat("  Unique transition types:", nrow(transition_counts), "\n")
  
  # Generate all possible (current_state, next_state) pairs
  all_states = expand_grid(
    n = 1:20,
    x = 1:3,
    s = -3:3,
    p = c(-1, 1)
  )
  
  all_next_states = expand_grid(
    n_next = 2:20,  # n+1, capped at 20
    x_next = 1:3,
    s_next = -3:3,
    p_next = c(-1, 1)
  )
  
  # Create all possible state-next_state combinations
  all_combinations = all_states %>%
    cross_join(all_next_states) %>%
    filter(n_next == n + 1) %>%
    rowwise() %>%
    filter(is_valid_transition(n, x, s, p, n_next, x_next, s_next, p_next)) %>%
    ungroup()
  
  cat("  Valid state-next_state combinations:", nrow(all_combinations), "\n")
  
  # Merge with observed counts
  prob_df = all_combinations %>%
    left_join(transition_counts, by = c('n', 'x', 's', 'p', 'n_next', 'x_next', 's_next', 'p_next')) %>%
    mutate(
      count = ifelse(is.na(count), 0, count),
      # Apply smoothing: add alpha to all possible transitions
      count_smoothed = if(smoothing) count + alpha else count
    ) %>%
    # Calculate probabilities for each current state
    group_by(n, x, s, p) %>%
    mutate(
      total_count = sum(count_smoothed),
      prob = count_smoothed / total_count
    ) %>%
    ungroup() %>%
    select(n, x, s, p, n_next, x_next, s_next, p_next, 
           count, count_smoothed, prob)
  
  cat("  Total state-next_state pairs:", nrow(prob_df), "\n")
  cat("  States with non-zero transitions:", 
      sum(prob_df %>% group_by(n, x, s, p) %>% summarize(total = sum(count)) %>% pull(total) > 0), "\n\n")
  
  return(prob_df)
}

# ============================================================================
# MAIN FUNCTION
# ============================================================================

build_transition_matrix <- function(train_file = "job_output/train.csv",
                                    output_rds = "job_output/transition_matrix.rds",
                                    output_csv = "job_output/transition_matrix.csv",
                                    smoothing = TRUE,
                                    alpha = 1.0) {
  cat("=== Building Transition Probability Matrix ===\n\n")
  cat("WARNING: TRAINING DATA: ONLY using", train_file, "\n")
  cat("   (NOT using test.csv - this is for training only)\n\n")
  
  # Load training data
  cat("Loading training data...\n")
  df_train = read_csv(train_file, show_col_types = FALSE)
  cat("  Training file:", train_file, "\n")
  cat("  Games:", length(unique(df_train$game_id)), "\n")
  cat("  Total plays:", nrow(df_train), "\n\n")
  
  # Extract transitions (with symmetry)
  df_trans = extract_transitions(df_train)
  
  # Estimate probabilities
  prob_df = estimate_transition_probs(df_trans, smoothing = smoothing, alpha = alpha)
  
  # Save results
  cat("Saving transition matrix...\n")
  saveRDS(prob_df, output_rds)
  cat("  Saved RDS to:", output_rds, "\n")
  
  write_csv(prob_df, output_csv)
  cat("  Saved CSV to:", output_csv, "\n\n")
  
  # Summary statistics
  cat("=== Summary Statistics ===\n")
  cat("Total state-next_state pairs:", nrow(prob_df), "\n")
  cat("Unique current states:", 
      nrow(prob_df %>% distinct(n, x, s, p)), "\n")
  cat("Average transitions per state:", 
      round(mean(prob_df %>% group_by(n, x, s, p) %>% summarize(n_trans = n()) %>% pull(n_trans)), 2), "\n")
  cat("States with at least one observed transition:", 
      sum(prob_df %>% group_by(n, x, s, p) %>% summarize(total_count = sum(count)) %>% pull(total_count) > 0), "\n\n")
  
  return(prob_df)
}

# ============================================================================
# HELPER: Get transition probability for a specific state
# ============================================================================

get_transition_prob <- function(prob_df, n, x, s, p) {
  prob_df %>%
    filter(n == !!n, x == !!x, s == !!s, p == !!p) %>%
    select(n_next, x_next, s_next, p_next, prob, count)
}

# ============================================================================
# RUN IF EXECUTED DIRECTLY
# ============================================================================

if (!interactive()) {
  transition_matrix = build_transition_matrix()
}

