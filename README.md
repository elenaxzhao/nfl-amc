# Absorbing Markov Chain Win Probability Model

A probabilistic model for predicting NFL game win probabilities using an absorbing Markov chain trained on historical play-by-play data. [Full Presentation Here](https://docs.google.com/presentation/d/1D66B3HM5KTGr8bA1JufWVsNjO5PdzZo4nGav3RqdYN4/edit?usp=sharing)

## Table of Contents

1. [Overview](#overview)
2. [Model Architecture](#model-architecture)
3. [Data Processing](#data-processing)
4. [Features](#features)
5. [How to Run](#how-to-run)
6. [Usage Examples](#usage-examples)
7. [File Structure](#file-structure)

---

## Overview

This implementation builds an **absorbing Markov chain** model that predicts win probability for any game state during an NFL game. The model:

- **Learns from data**: Estimates transition probabilities from 2010-2020 NFL play-by-play data
- **Uses symmetry**: Doubles training data by exploiting game symmetry
- **Handles sparsity**: Uses smoothing to handle unobserved state transitions
- **Provides exact solutions**: Computes win probabilities analytically (no simulation needed)

### What It Predicts

For any game state `(n, x, s, p)`, the model predicts:
- **Win probability for Team 1** (the team whose perspective the score differential is from)
- Probability ranges from 0.0 (certain loss) to 1.0 (certain win)

### State Space

Each game state is represented as `(n, x, s, p)`:
- **n**: Play number (1-20)
- **x**: Field position (1, 2, or 3)
  - `x = 1`: Close to own endzone
  - `x = 2`: Midfield
  - `x = 3`: Close to opponent endzone
- **s**: Score differential from Team 1's perspective (-3 to +3, scaled)
  - `s > 0`: Team 1 is ahead
  - `s < 0`: Team 2 is ahead
  - `s = 0`: Tied
- **p**: Possession indicator (1 or -1)
  - `p = 1`: Team 1 has the ball
  - `p = -1`: Team 2 has the ball

---

## Model Architecture

### Absorbing Markov Chain

The model uses an absorbing Markov chain with:

- **Transient states**: All states where `n < 20` (game in progress)
- **Absorbing states**: WIN, LOSS, TIE (game ended)
  - **WIN**: Team 1 wins (final `s > 0`)
  - **LOSS**: Team 2 wins (final `s < 0`)
  - **TIE**: Game tied (final `s = 0`)

### Transition Matrix Structure

```
P = [Q  R]
    [0  I]
```

Where:
- **Q**: Transitions between transient states (798 × 798 sparse matrix)
- **R**: Transitions from transient to absorbing states (798 × 3 sparse matrix)
- **I**: Identity matrix (absorbing states stay absorbed)

### Solution

The model solves for absorption probabilities:

```
N = (I - Q)^(-1)  # Fundamental matrix
B = N × R         # Absorption probabilities
```

`B[i, j]` gives the probability of ending in absorbing state `j` starting from transient state `i`.

### Win Probability Calculation

For state `(n, x, s, p)`:
```
WP = B[state_index, WIN] + 0.5 × B[state_index, TIE]
```

---

## Data Processing

### Input Data

Raw NFL play-by-play data from `nflfastR` (2010-2023).

### Processing Steps

1. **Field Position Mapping**: Convert yardline (0-100) to bins (1, 2, 3)
2. **Score Scaling**: 
   - `|s| = 0` → `s = 0`
   - `|s| = 1-3` → `s = ±1`
   - `|s| = 4-7` → `s = ±2`
   - `|s| >= 8` → `s = ±3`
3. **Continuous Score Perspective**: Multiply by possession so `s` is always from Team 1's perspective
4. **Play Aggregation**: Only count plays where field position `x` changes
5. **Filtering**: Keep only games with exactly 20 plays

### Train/Test Split

- **Train**: 2010-2020 (2,833 games, 56,660 plays)
- **Test**: 2021-2023 (771 games, 15,420 plays)

### Output Files

- `job_output/train.csv`: Training data
- `job_output/test.csv`: Test data

---

## Features

### 1. Symmetry Enforcement

**Doubles training data** by adding symmetric transitions:

For every transition:
```
(n, x, s, p) → (n+1, x_next, s_next, p_next)
```

We also count:
```
(n, 4-x, -s, -p) → (n+1, 4-x_next, -s_next, -p_next)
```

**Benefits**:
- More robust probability estimates
- Enforces symmetry in the model
- Better coverage of state space

### 2. Constraint Validation

Only valid transitions are considered:
- Play number increments by 1: `n_next = n + 1`
- Valid positions: `x_next ∈ {1, 2, 3}`
- Valid scores: `s_next ∈ {-3, -2, -1, 0, 1, 2, 3}`
- Score direction: If `p=1`, then `s_next ≥ s`; if `p=-1`, then `s_next ≤ s`

### 3. Add-One Smoothing

Prevents zero probabilities for unobserved but valid transitions:
```
P = (count + 1) / (total_count + num_possible_transitions)
```

**Benefits**:
- Handles sparse data gracefully
- Gives small probability to rare transitions
- Improves generalization

### 4. Renormalization

After filtering invalid transitions, probabilities are renormalized to sum to 1:
```
P_normalized = P_raw / sum(all_valid_P_raw)
```

### 5. Efficient Sparse Matrix Storage

Uses sparse matrix representation for:
- Q matrix (transient → transient)
- R matrix (transient → absorbing)
- B matrix (absorption probabilities)

**Benefits**: Memory efficient, fast computation

---

## How to Run

### Prerequisites

```r
install.packages(c("tidyverse", "nflfastR", "Matrix", "gridExtra"))
```

### Step 1: Process NFL Data (if not already done)

```r
source("process_nfl_to_states.R")
# This creates job_output/train.csv and job_output/test.csv
```

### Step 2: Build Transition Matrix

```r
source("estimate_transition_matrix.R")
# This creates job_output/transition_matrix.csv
```

**Output**: `job_output/transition_matrix.csv` with all state-to-state transition probabilities.

### Step 3: Build Absorbing Markov Chain

```r
source("build_absorbing_markov_chain.R")
# This creates job_output/absorbing_markov_chain.rds
```

**Output**: `job_output/absorbing_markov_chain.rds` containing Q, R, and B matrices.

### Step 4: Query Win Probabilities

```r
# Step 1: Source the file (defines the function)
source("build_absorbing_markov_chain.R")

# Step 2: Load the pre-built Markov chain
markov_chain = readRDS("job_output/absorbing_markov_chain.rds")

# Step 3: Use the function to get win probability
wp = get_win_probability(markov_chain, n=10, x=2, s=0, p=1)
print(wp)  # Returns: 0.5254 (52.54% win probability for Team 1)
```

### Complete Pipeline (One Command)

```r
# Run all steps in sequence
source("estimate_transition_matrix.R")      # Step 1: Build transition matrix
source("build_absorbing_markov_chain.R")   # Step 2: Build Markov chain
```

---

## Usage Examples

### Example 1: Get Win Probability for a Specific State

```r
source("build_absorbing_markov_chain.R")
markov_chain = readRDS("job_output/absorbing_markov_chain.rds")

# State: Play 10, midfield, tied, Team 1 has ball
wp = get_win_probability(markov_chain, n=10, x=2, s=0, p=1)
print(wp)  # 0.5254
```

### Example 2: Analyze Test Games

```r
source("test_games.R")
# Creates visualizations for first 4 games in test set
```

### Example 3: Get All Probabilities for a State

```r
source("build_absorbing_markov_chain.R")
markov_chain = readRDS("job_output/absorbing_markov_chain.rds")

# Get win, loss, and tie probabilities
probs = get_state_probabilities(markov_chain, n=15, x=3, s=-2, p=-1)
print(probs)
# $win: 0.2136
# $loss: 0.7864
# $tie: 0.0000
```

### Example 4: Evaluate on Test Set

```r
library(tidyverse)
source("build_absorbing_markov_chain.R")

# Load Markov chain and test data
markov_chain = readRDS("job_output/absorbing_markov_chain.rds")
df_test = read_csv("job_output/test.csv", show_col_types = FALSE)

# Calculate win probabilities for all test states
df_test = df_test %>%
  rowwise() %>%
  mutate(
    wp_pred = get_win_probability(markov_chain, n, x, s, possession)
  ) %>%
  ungroup()

# View results
head(df_test)
```

---

## File Structure

### Core Scripts

- **`process_nfl_to_states.R`**: Transforms raw NFL data into state format `(n, x, s, p)`
- **`estimate_transition_matrix.R`**: Estimates transition probabilities with symmetry
- **`build_absorbing_markov_chain.R`**: Builds and solves the absorbing Markov chain

### Analysis Scripts

- **`test_games.R`**: Analyzes multiple games in test set

### Data Files

- **`job_output/train.csv`**: Training data (2010-2020)
- **`job_output/test.csv`**: Test data (2021-2023)
- **`job_output/transition_matrix.csv`**: Estimated transition probabilities
- **`job_output/absorbing_markov_chain.rds`**: Solved Markov chain (Q, R, B matrices)

### Documentation

- **`README.md`**: This file (main documentation)

---

## Key Functions

### `get_win_probability(markov_chain, n, x, s, p)`

Returns win probability for Team 1 from state `(n, x, s, p)`.

**Parameters**:
- `markov_chain`: Result from `build_absorbing_markov_chain()`
- `n`: Play number (1-20)
- `x`: Position (1, 2, or 3)
- `s`: Score differential from Team 1's perspective (-3 to +3)
- `p`: Possession (1 or -1)

**Returns**: Win probability (0.0 to 1.0)

**Special cases**:
- If `n = 20`: Returns 1.0 if `s > 0`, 0.0 if `s < 0`, 0.5 if `s = 0`

### `get_state_probabilities(markov_chain, n, x, s, p)`

Returns win, loss, and tie probabilities for a state.

**Returns**: List with `win`, `loss`, `tie` probabilities

### `build_transition_matrix(train_file, ...)`

Estimates transition probabilities from training data.

**Parameters**:
- `train_file`: Path to training CSV (default: `"job_output/train.csv"`)
- `smoothing`: Apply add-one smoothing (default: `TRUE`)
- `alpha`: Smoothing parameter (default: `1.0`)

**Returns**: Data frame with transition probabilities

### `build_absorbing_markov_chain(transition_matrix_file, ...)`

Builds and solves the absorbing Markov chain.

**Parameters**:
- `transition_matrix_file`: Path to transition matrix CSV (default: `"job_output/transition_matrix.csv"`)

**Returns**: List containing Q, R, and B matrices

---

## Model Statistics

### Training Data
- **Games**: 2,833 (2010-2020)
- **Plays**: 56,660
- **Transitions observed**: 52,667
- **Transitions with symmetry**: 105,334 (doubled)

### Transition Matrix
- **Total state-next_state pairs**: 19,152
- **Unique transition types**: 7,228
- **States with observed transitions**: 776 out of 798 possible

### Markov Chain
- **Transient states**: 798 (19 plays × 3 positions × 7 scores × 2 possessions)
- **Absorbing states**: 3 (WIN, LOSS, TIE)
- **Win probability range**: 0.0 to 1.0
- **Mean win probability**: 0.46 (slightly favors Team 2, likely due to data distribution)

---

## Design Decisions

### Why Absorbing Markov Chain?

- **Exact solution**: No simulation needed, computes probabilities analytically
- **Efficient**: O(1) lookup once built
- **Interpretable**: Probabilities come directly from observed transitions
- **Complete**: Works for all valid state combinations

### Why Symmetry?

- **Data efficiency**: Effectively doubles training data
- **Consistency**: Enforces logical symmetry in game dynamics
- **Robustness**: Better probability estimates for sparse states

### Why Add-One Smoothing?

- **Handles sparsity**: Many state combinations are rarely observed
- **Prevents overfitting**: Gives small probability to unobserved but valid transitions
- **Standard technique**: Well-established in probabilistic modeling

### Why Continuous Score Perspective?

- **Consistency**: Score differential always from same perspective
- **Simplicity**: Easier to interpret and reason about
- **Transition clarity**: Makes state transitions more intuitive

---

## Troubleshooting

### "Function not found" Error

Make sure to source the script first:
```r
source("build_absorbing_markov_chain.R")
```

### Win Probabilities All 0 or 1

Check if the R matrix was built correctly. The model should have a range of probabilities.

### Invalid State Error

Ensure state parameters are in valid ranges:
- `n`: 1-20
- `x`: 1, 2, or 3
- `s`: -3 to +3
- `p`: -1 or 1

### Memory Issues

The model uses sparse matrices, so memory usage should be reasonable. If issues occur, check:
- Sparse matrix package is installed
- Sufficient RAM for matrix operations

---

## Further Reading

For more details on the implementation, see the source code:
- **`estimate_transition_matrix.R`**: Transition probability estimation with symmetry
- **`build_absorbing_markov_chain.R`**: Markov chain construction and solution

---

## Summary

This absorbing Markov chain model provides a data-driven approach to win probability prediction:

1. **Learns from historical data**: Uses 2010-2020 NFL games to estimate transition probabilities
2. **Leverages symmetry**: Doubles training data through symmetric transitions
3. **Handles sparsity**: Uses smoothing for unobserved states
4. **Provides exact solutions**: Computes win probabilities analytically
5. **Easy to use**: Simple function call to get win probability for any state

The model is ready to evaluate on the 2021-2023 test set to assess predictive performance.

