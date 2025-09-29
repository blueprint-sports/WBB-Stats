library(tidyverse)
library(Matrix)
library(glmnet)
library(knitr)

# Load base data first
source("wbb_player_stats.R")

# Check if required data exists
if(!exists("player_box")) {
  stop("player_box dataset not found. Please ensure wbb_player_stats.R ran successfully.")
}
if(!exists("wbb_pbp")) {
  stop("wbb_pbp dataset not found. Please ensure wbb_player_stats.R ran successfully.")
}

# =============================================================================
# RAPM ANALYSIS: TRACKING PLAYERS ON COURT FOR EACH PLAY
# =============================================================================

# Load data.table for efficient processing
library(data.table)

# Get starting lineups for each game
starting_lineups <- player_box |>
  filter(starter == TRUE) |>
  select(game_id, team_id, athlete_id, athlete_display_name) |>
  group_by(game_id, team_id) |>
  summarise(
    starters = list(athlete_id),
    starter_names = list(athlete_display_name),
    num_starters = n(),
    .groups = 'drop'
  )

# Create optimized stint tracking function using data.table
track_stints_dt <- function() {
  # Convert to data.table for speed
  pbp_dt <- as.data.table(wbb_pbp)
  starters_dt <- as.data.table(starting_lineups)
  
  # Get games with complete data
  valid_games <- intersect(pbp_dt$game_id, starters_dt$game_id)
  cat("Processing", length(valid_games), "games with data.table...\n")
  
  # Filter to valid games
  pbp_dt <- pbp_dt[game_id %in% valid_games]
  starters_dt <- starters_dt[game_id %in% valid_games]
  
  # Set keys for fast joining
  setkey(pbp_dt, game_id, game_play_number)
  setkey(starters_dt, game_id)
  
  # Process each game using data.table operations
  stint_results <- pbp_dt[, {
    # Get this game's data
    game_pbp <- .SD
    game_starters <- starters_dt[game_id == .BY$game_id]
    
    if(nrow(game_starters) != 2) {
      data.table()
    } else {
      # Initialize lineups
      home_team_id <- game_pbp$home_team_id[1]
      away_team_id <- game_pbp$away_team_id[1]
      
      home_starters <- game_starters[team_id == home_team_id]$starters[[1]]
      away_starters <- game_starters[team_id == away_team_id]$starters[[1]]
      
      # Current lineups
      home_lineup <- home_starters
      away_lineup <- away_starters
      
      # Process substitutions in order
      game_subs <- game_pbp[type_text == "Substitution"]
      
      # Create lineup tracking vectors
      lineup_data <- data.table(
        game_play_number = game_pbp$game_play_number,
        home_player_1 = home_lineup[1],
        home_player_2 = home_lineup[2],
        home_player_3 = home_lineup[3],
        home_player_4 = home_lineup[4],
        home_player_5 = home_lineup[5],
        away_player_1 = away_lineup[1],
        away_player_2 = away_lineup[2],
        away_player_3 = away_lineup[3],
        away_player_4 = away_lineup[4],
        away_player_5 = away_lineup[5]
      )
      
      # Update for each substitution
      if(nrow(game_subs) > 0) {
        for(i in 1:nrow(game_subs)) {
          sub <- game_subs[i]
          sub_play_num <- sub$game_play_number
          
          # Parse substitution
          if(grepl("subbing out", sub$text, ignore.case = TRUE)) {
            player_out <- sub$athlete_id_1
            if(sub$team_id == home_team_id) {
              home_lineup <- home_lineup[home_lineup != player_out]
            } else {
              away_lineup <- away_lineup[away_lineup != player_out]
            }
          } else if(grepl("subbing in", sub$text, ignore.case = TRUE)) {
            player_in <- sub$athlete_id_1
            if(sub$team_id == home_team_id) {
              home_lineup <- c(home_lineup, player_in)
            } else {
              away_lineup <- c(away_lineup, player_in)
            }
          }
          
          # Update all subsequent plays
          lineup_data[game_play_number >= sub_play_num, `:=`(
            home_player_1 = home_lineup[1],
            home_player_2 = home_lineup[2],
            home_player_3 = home_lineup[3],
            home_player_4 = home_lineup[4],
            home_player_5 = home_lineup[5],
            away_player_1 = away_lineup[1],
            away_player_2 = away_lineup[2],
            away_player_3 = away_lineup[3],
            away_player_4 = away_lineup[4],
            away_player_5 = away_lineup[5]
          )]
        }
      }
      
      lineup_data[, game_id := .BY$game_id]
      lineup_data
    }
  }, by = game_id]
  
  return(stint_results)
}

# Process all games
cat("Starting lineup tracking for all games...\n")
start_full_time <- Sys.time()

all_stint_data <- track_stints_dt()

end_full_time <- Sys.time()
full_processing_time <- as.numeric(difftime(end_full_time, start_full_time, units = "secs"))

cat("Processing time:", round(full_processing_time / 60, 2), "minutes\n")
cat("Total rows processed:", nrow(all_stint_data), "\n")
cat("Total games processed:", length(unique(all_stint_data$game_id)), "\n")

# Clean up duplicate columns
if(nrow(all_stint_data) > 0) {
  unique_cols_final <- !duplicated(names(all_stint_data))
  all_stint_clean <- all_stint_data[, ..unique_cols_final]
} else {
  stop("No stint data processed")
}

# =============================================================================
# PREPARE CLEAN DATASET FOR RAPM ANALYSIS
# =============================================================================

if(exists("all_stint_clean") && nrow(all_stint_clean) > 0) {
  cat("\n\n=== PREPARING RAPM DATASET ===\n")
  
  # Create clean RAPM dataset with only 10-player plays
  cat("Creating clean RAPM dataset...\n")
  
  # Convert to tibble and add player counts (vectorized approach)
  rapm_data_tibble <- as_tibble(all_stint_clean)
  
  # Vectorized player count calculation
  player_cols <- c("home_player_1", "home_player_2", "home_player_3", "home_player_4", "home_player_5",
                   "away_player_1", "away_player_2", "away_player_3", "away_player_4", "away_player_5")
  
  # Count non-NA values across player columns (much faster than rowwise)
  total_players <- rowSums(!is.na(rapm_data_tibble[player_cols]))
  
  # Filter to only plays with exactly 10 players
  rapm_data <- rapm_data_tibble[total_players == 10, ]
  
  cat("Original plays:", nrow(all_stint_clean), "\n")
  cat("Plays with exactly 10 players:", nrow(rapm_data), "\n")
  cat("Percentage retained:", round(100 * nrow(rapm_data) / nrow(all_stint_clean), 2), "%\n")
  
  # Join with pbp data to add play context
  cat("Adding play context from pbp data...\n")
  
  rapm_with_context <- rapm_data |>
    left_join(
      wbb_pbp |> select(game_id, game_play_number, type_text, period_number,
                        clock_display_value, home_score, away_score, scoring_play),
      by = c("game_id", "game_play_number")
    )
  
  cat("Plays with context added:", nrow(rapm_with_context), "\n")
  
  # Filter out substitutions for RAPM (not meaningful possessions)
  rapm_final <- rapm_with_context |>
    filter(type_text != "Substitution" | is.na(type_text))
  
  cat("Non-substitution plays for RAPM:", nrow(rapm_final), "\n")
  cat("Games in RAPM dataset:", length(unique(rapm_final$game_id)), "\n")
  
  cat("\n✅ RAPM dataset ready!\n")
  cat("- Variable name: rapm_final\n")
  cat("- Rows:", nrow(rapm_final), "\n")
  cat("- Columns:", ncol(rapm_final), "\n")
  cat("- Each row = 1 play with 10 players tracked\n")
  
} else {
  cat("❌ No stint data available for RAPM preparation\n")
}

cat("Starting RAPM calculation for women's basketball...\n")
cat("Dataset dimensions:", nrow(rapm_final), "plays x", ncol(rapm_final), "variables\n")
cat("Games in dataset:", length(unique(rapm_final$game_id)), "\n")

# Process lineup data for RAPM
process_lineup_data <- function() {
  # Convert rapm_final to lineup format similar to men's data
  lineup_data <- rapm_final %>%
    # Filter out plays without complete lineup data
    filter(!is.na(home_player_1) & !is.na(home_player_2) & !is.na(home_player_3) &
           !is.na(home_player_4) & !is.na(home_player_5) &
           !is.na(away_player_1) & !is.na(away_player_2) & !is.na(away_player_3) &
           !is.na(away_player_4) & !is.na(away_player_5)) %>%
    # Add possession and scoring information
    group_by(game_id, period_number,
             home_player_1, home_player_2, home_player_3, home_player_4, home_player_5,
             away_player_1, away_player_2, away_player_3, away_player_4, away_player_5) %>%
    summarise(
      total_possessions = n(),
      home_points = sum(ifelse(scoring_play == TRUE & !is.na(home_score),
                              c(0, diff(home_score)), 0), na.rm = TRUE),
      away_points = sum(ifelse(scoring_play == TRUE & !is.na(away_score),
                              c(0, diff(away_score)), 0), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    # Calculate points per 100 possessions
    mutate(
      home_ppp100 = 100 * home_points / total_possessions,
      away_ppp100 = 100 * away_points / total_possessions
    ) %>%
    # Filter out very short possessions to reduce noise
    filter(total_possessions >= 1) %>%
    select(
      home_player_1, home_player_2, home_player_3, home_player_4, home_player_5,
      away_player_1, away_player_2, away_player_3, away_player_4, away_player_5,
      possessions = total_possessions,
      home_ppp100,
      away_ppp100
    )

  return(lineup_data)
}

lineup_data <- process_lineup_data()

print(paste0("Processed ", nrow(lineup_data), " lineup combinations"))
print("Sample of processed data:")
print(head(lineup_data))

# Get all unique player IDs
get_players <- function(lineup_data) {
  players <- unique(c(
    unique(lineup_data$home_player_1),
    unique(lineup_data$home_player_2),
    unique(lineup_data$home_player_3),
    unique(lineup_data$home_player_4),
    unique(lineup_data$home_player_5),
    unique(lineup_data$away_player_1),
    unique(lineup_data$away_player_2),
    unique(lineup_data$away_player_3),
    unique(lineup_data$away_player_4),
    unique(lineup_data$away_player_5)
  ))

  # Remove NAs
  players <- players[!is.na(players)]

  return(sort(players))
}

players <- get_players(lineup_data)
print(paste0("There are ", length(players), " unique players in the dataset."))

# Include ALL players - no minimum possession filter
print("Including ALL players - no filtering applied")

print(paste0("Final dataset: ", length(players), " players, ", nrow(lineup_data), " possessions"))

# Memory-efficient sparse matrix creation for RAPM

# FAST VECTORIZED SPARSE MATRIX CREATION - MUCH FASTER!
print("Creating sparse matrix (VECTORIZED - MUCH FASTER)...")

n_possessions <- nrow(lineup_data)
n_players <- length(players)
n_cols <- n_players * 2  # offense and defense

# Create player index lookup for fast conversion
player_index <- setNames(1:n_players, players)

# Extract player matrices as numeric matrices
home_players_matrix <- as.matrix(lineup_data[, c("home_player_1", "home_player_2",
                                                 "home_player_3", "home_player_4", "home_player_5")])
away_players_matrix <- as.matrix(lineup_data[, c("away_player_1", "away_player_2",
                                                 "away_player_3", "away_player_4", "away_player_5")])

# Convert to indices - VECTORIZED
home_indices <- matrix(player_index[as.character(home_players_matrix)], nrow = n_possessions, ncol = 5)
away_indices <- matrix(player_index[as.character(away_players_matrix)], nrow = n_possessions, ncol = 5)

# Create row indices (repeat each possession 5 times for 5 players)
row_indices <- rep(1:n_possessions, 5)

# Create column indices for home team matrix
col_indices_home_off <- as.vector(home_indices)                    # Home offense
col_indices_away_def <- as.vector(away_indices) + n_players       # Away defense

# Create column indices for away team matrix
col_indices_away_off <- as.vector(away_indices)                   # Away offense
col_indices_home_def <- as.vector(home_indices) + n_players       # Home defense

print("Building combined sparse matrix...")

# Create COMBINED sparse matrix directly (much faster than rbind)
i_all <- c(row_indices, row_indices,                              # Home team rows
           row_indices + n_possessions, row_indices + n_possessions) # Away team rows
j_all <- c(col_indices_home_off, col_indices_away_def,           # Home offense + Away defense
           col_indices_away_off, col_indices_home_def)            # Away offense + Home defense
x_all <- c(rep(1, length(row_indices)), rep(-1, length(row_indices)),    # Home team
           rep(1, length(row_indices)), rep(-1, length(row_indices)))     # Away team

# Create final sparse matrix
player_matrix <- sparseMatrix(i = i_all, j = j_all, x = x_all, dims = c(2 * n_possessions, n_cols))
target <- c(lineup_data$home_ppp100, lineup_data$away_ppp100)

print(paste0("Combined matrix dimensions: ", nrow(player_matrix), " x ", ncol(player_matrix)))
print(paste0("Target vector length: ", length(target)))

# Fit RAPM model using ridge regression
print("Fitting RAPM model...")

# Cross-validated model to pick optimal lambda
cv_model <- cv.glmnet(
  x = player_matrix,
  y = target,
  alpha = 0,  # Ridge regression
  standardize = FALSE
)

optimal_lambda <- cv_model$lambda.min
print(paste0("Optimal lambda: ", round(optimal_lambda, 6)))

# Refit model with optimal lambda
rapm_model <- glmnet(
  x = player_matrix,
  y = target,
  alpha = 0,
  standardize = FALSE,
  lambda = optimal_lambda
)

# Extract coefficients
player_coefficients <- rapm_model$beta

# Split into offensive and defensive components
o_rapm <- player_coefficients[1:length(players)]
d_rapm <- player_coefficients[(length(players) + 1):(2 * length(players))]

# Get player names from player_box data
player_info <- player_box %>%
  select(athlete_id, athlete_display_name, team_display_name) %>%
  distinct()

# Create results data frame
rapm_results <- data.frame(
  player_id = players,
  o_rapm = as.numeric(o_rapm),
  d_rapm = as.numeric(d_rapm)
) %>%
  mutate(rapm = o_rapm + d_rapm) %>%
  left_join(player_info, by = c("player_id" = "athlete_id")) %>%
  filter(!is.na(athlete_display_name)) %>%  # Remove players not in our info table
  arrange(desc(rapm)) %>%
  select(
    Player = athlete_display_name,
    Team = team_display_name,
    RAPM = rapm,
    `O-RAPM` = o_rapm,
    `D-RAPM` = d_rapm
  )

# Display top results
print("Top 20 RAPM Players:")
kable(rapm_results[1:20, ] %>%
        mutate(across(where(is.numeric), function(x) round(x, 2))),
      align = "c")

# Save results
write_csv(rapm_results, "womens_basketball_RAPM_2025.csv")
print("Results saved to womens_basketball_RAPM_2025.csv")

# Quick summary stats
print(paste0("Players analyzed: ", nrow(rapm_results)))
print(paste0("RAPM range: ", round(min(rapm_results$RAPM, na.rm = TRUE), 2),
             " to ", round(max(rapm_results$RAPM, na.rm = TRUE), 2)))
print(paste0("Mean RAPM: ", round(mean(rapm_results$RAPM, na.rm = TRUE), 2)))
print(paste0("Standard deviation: ", round(sd(rapm_results$RAPM, na.rm = TRUE), 2)))