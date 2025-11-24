# Build Absorbing Markov Chain for Win Probability Prediction
# State space: (n, x, s, p)
#   n: play number (1-20)
#   x: position (1, 2, 3)
#   s: score differential from possession team's perspective (-3 to +3)
#   p: possession (1 or -1)

library(tidyverse)
library(Matrix)

# ============================================================================
# STATE INDEXING
# ============================================================================
# Map (n, x, s, p) to a single index for matrix operations

state_to_index <- function(n, x, s, p) {
  # Transient states: n = 1 to 19
  # n: 1-19 (19 values)
  # x: 1-3 (3 values)
  # s: -3 to 3 (7 values)
  # p: -1, 1 (2 values)
  
  if (n < 1 || n > 19) {
    stop("n must be between 1 and 19 for transient states")
  }
  
  # Convert to 0-based indexing for calculation
  n_idx = n - 1  # 0-18
  x_idx = x - 1  # 0-2
  s_idx = s + 3  # 0-6 (maps -3 to 0, -2 to 1, ..., 3 to 6)
  p_idx = ifelse(p == -1, 0, 1)  # -1 -> 0, 1 -> 1
  
  # Total transient states: 19 × 3 × 7 × 2 = 798
  index = n_idx * (3 * 7 * 2) + 
          x_idx * (7 * 2) + 
          s_idx * 2 + 
          p_idx + 1  # +1 for 1-based indexing
  
  return(index)
}

index_to_state <- function(index) {
  # Convert 1-based index back to (n, x, s, p)
  index = index - 1  # Convert to 0-based
  
  p_idx = index %% 2
  index = index %/% 2
  
  s_idx = index %% 7
  index = index %/% 7
  
  x_idx = index %% 3
  index = index %/% 3
  
  n_idx = index
  
  n = n_idx + 1
  x = x_idx + 1
  s = s_idx - 3
  p = ifelse(p_idx == 0, -1, 1)
  
  return(list(n = n, x = x, s = s, p = p))
}

# ============================================================================
# BUILD TRANSITION MATRICES
# ============================================================================

build_transition_matrices <- function(transition_matrix_file = "job_output/transition_matrix.csv") {
  cat("=== Building Absorbing Markov Chain ===\n\n")
  
  # Load transition matrix
  cat("Loading transition matrix...\n")
  prob_df = read_csv(transition_matrix_file, show_col_types = FALSE)
  cat("  Loaded", nrow(prob_df), "transitions\n\n")
  
  # Number of transient states (n = 1 to 19)
  num_transient = 19 * 3 * 7 * 2  # 798 states
  num_absorbing = 3  # WIN, LOSS, TIE
  
  cat("State space:\n")
  cat("  Transient states:", num_transient, "\n")
  cat("  Absorbing states:", num_absorbing, "(WIN, LOSS, TIE)\n\n")
  
  # Initialize Q (transient -> transient) and R (transient -> absorbing) matrices
  cat("Building Q and R matrices...\n")
  Q = Matrix(0, nrow = num_transient, ncol = num_transient, sparse = TRUE)
  R = Matrix(0, nrow = num_transient, ncol = num_absorbing, sparse = TRUE)
  
  colnames(R) = c("WIN", "LOSS", "TIE")
  
  # Fill Q matrix: transitions between transient states (n < 20, n_next < 20)
  transient_transitions = prob_df %>%
    filter(n < 20, n_next < 20)
  
  cat("  Transient -> transient transitions:", nrow(transient_transitions), "\n")
  
  for (i in 1:nrow(transient_transitions)) {
    row = transient_transitions[i, ]
    
    from_idx = state_to_index(row$n, row$x, row$s, row$p)
    to_idx = state_to_index(row$n_next, row$x_next, row$s_next, row$p_next)
    
    Q[from_idx, to_idx] = row$prob
  }
  
  # Fill R matrix: transitions from n=19 to n=20 (game ends)
  # For states at n=19, transitions to n=20 determine absorption
  absorbing_transitions = prob_df %>%
    filter(n == 19, n_next == 20)
  
  cat("  Transient -> absorbing transitions:", nrow(absorbing_transitions), "\n")
  
  for (i in 1:nrow(absorbing_transitions)) {
    row = absorbing_transitions[i, ]
    
    from_idx = state_to_index(row$n, row$x, row$s, row$p)
    
    # Determine absorbing state based on final score
    if (row$s_next > 0) {
      R[from_idx, "WIN"] = R[from_idx, "WIN"] + row$prob
    } else if (row$s_next < 0) {
      R[from_idx, "LOSS"] = R[from_idx, "LOSS"] + row$prob
    } else {
      R[from_idx, "TIE"] = R[from_idx, "TIE"] + row$prob
    }
  }
  
  # Check: rows of Q + R should sum to 1 (or close to 1 due to rounding)
  row_sums = rowSums(Q) + rowSums(R)
  cat("\n  Row sum check:\n")
  cat("    Min:", min(row_sums), "\n")
  cat("    Max:", max(row_sums), "\n")
  cat("    Mean:", round(mean(row_sums), 6), "\n")
  
  if (abs(mean(row_sums) - 1.0) > 0.01) {
    warning("Some rows don't sum to 1! This may indicate missing transitions.")
  }
  
  return(list(Q = Q, R = R, num_transient = num_transient, num_absorbing = num_absorbing))
}

# ============================================================================
# SOLVE ABSORBING MARKOV CHAIN
# ============================================================================

solve_absorbing_markov_chain <- function(Q, R) {
  cat("\n=== Solving Absorbing Markov Chain ===\n")
  
  num_transient = nrow(Q)
  
  # Compute fundamental matrix: N = (I - Q)^(-1)
  cat("Computing fundamental matrix N = (I - Q)^(-1)...\n")
  I = Diagonal(num_transient)
  N = solve(I - Q)
  
  cat("  N matrix computed\n")
  
  # Compute absorption probabilities: B = N × R
  cat("Computing absorption probabilities B = N × R...\n")
  B = N %*% R
  
  cat("  B matrix computed\n")
  cat("  Shape:", nrow(B), "×", ncol(B), "\n\n")
  
  return(B)
}

# ============================================================================
# BUILD COMPLETE MARKOV CHAIN
# ============================================================================

build_absorbing_markov_chain <- function(transition_matrix_file = "job_output/transition_matrix.csv",
                                         output_file = "job_output/absorbing_markov_chain.rds") {
  cat("=== Building Complete Absorbing Markov Chain ===\n\n")
  
  # Build Q and R matrices
  matrices = build_transition_matrices(transition_matrix_file)
  Q = matrices$Q
  R = matrices$R
  
  # Solve for absorption probabilities
  B = solve_absorbing_markov_chain(Q, R)
  
  # Create result object
  result = list(
    Q = Q,
    R = R,
    B = B,
    num_transient = matrices$num_transient,
    num_absorbing = matrices$num_absorbing
  )
  
  # Save
  cat("Saving Markov chain...\n")
  saveRDS(result, output_file)
  cat("  Saved to:", output_file, "\n\n")
  
  # Summary statistics
  cat("=== Summary ===\n")
  cat("Win probabilities range:\n")
  cat("  Min:", min(B[, "WIN"]), "\n")
  cat("  Max:", max(B[, "WIN"]), "\n")
  cat("  Mean:", round(mean(B[, "WIN"]), 4), "\n")
  cat("  Median:", round(median(B[, "WIN"]), 4), "\n\n")
  
  return(result)
}

# ============================================================================
# GET WIN PROBABILITY FUNCTION
# ============================================================================

get_win_probability <- function(markov_chain, n, x, s, p) {
  # markov_chain: result from build_absorbing_markov_chain()
  # n, x, s, p: state parameters
  
  # Handle end-of-game states (n = 20)
  if (n == 20) {
    if (s > 0) return(1.0)
    if (s < 0) return(0.0)
    if (s == 0) return(0.5)
  }
  
  # Handle invalid transient states
  if (n < 1 || n > 19) {
    stop("n must be between 1 and 19 for transient states, or 20 for end-of-game")
  }
  
  if (!x %in% 1:3) {
    stop("x must be 1, 2, or 3")
  }
  
  if (!s %in% -3:3) {
    stop("s must be between -3 and 3")
  }
  
  if (!p %in% c(-1, 1)) {
    stop("p must be -1 or 1")
  }
  
  # Get state index
  state_idx = state_to_index(n, x, s, p)
  
  # Get win probability from B matrix
  wp = markov_chain$B[state_idx, "WIN"]
  
  # Add half of tie probability
  tie_prob = markov_chain$B[state_idx, "TIE"]
  wp = wp + 0.5 * tie_prob
  
  return(as.numeric(wp))
}

# ============================================================================
# HELPER: Get all probabilities for a state
# ============================================================================

get_state_probabilities <- function(markov_chain, n, x, s, p) {
  # Returns win, loss, and tie probabilities for a state
  
  if (n == 20) {
    if (s > 0) return(list(win = 1.0, loss = 0.0, tie = 0.0))
    if (s < 0) return(list(win = 0.0, loss = 1.0, tie = 0.0))
    if (s == 0) return(list(win = 0.5, loss = 0.5, tie = 0.0))
  }
  
  state_idx = state_to_index(n, x, s, p)
  
  return(list(
    win = as.numeric(markov_chain$B[state_idx, "WIN"]),
    loss = as.numeric(markov_chain$B[state_idx, "LOSS"]),
    tie = as.numeric(markov_chain$B[state_idx, "TIE"])
  ))
}

# ============================================================================
# RUN IF EXECUTED DIRECTLY
# ============================================================================

if (!interactive()) {
  markov_chain = build_absorbing_markov_chain()
  
  # Test the function
  cat("=== Testing Win Probability Function ===\n\n")
  
  test_states = list(
    list(n = 10, x = 2, s = 0, p = 1, desc = "Play 10, midfield, tied, team has ball"),
    list(n = 15, x = 3, s = -2, p = -1, desc = "Play 15, near endzone, down by 2, opponent has ball"),
    list(n = 19, x = 2, s = 1, p = 1, desc = "Play 19, midfield, up by 1, team has ball"),
    list(n = 20, x = 2, s = 3, p = 1, desc = "End of game, up by 3")
  )
  
  for (state in test_states) {
    wp = get_win_probability(markov_chain, state$n, state$x, state$s, state$p)
    cat(sprintf("State (%d, %d, %d, %d): %s\n", 
                state$n, state$x, state$s, state$p, state$desc))
    cat(sprintf("  Win Probability: %.4f\n\n", wp))
  }
}

