library(tidyverse)
library(Matrix)
library(glmnet)
library(knitr)

# Source the main script to get rapm_final dataset
source("wbb_player_stats.R")

# Check if rapm_final exists
if(!exists("ramp_final")) {
  stop("rapm_final dataset not found. Please run wbb_player_stats.R first.")
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

# Create sparse matrix for RAPM model
make_matrix_rows <- function(lineup, players_in) {
  home_player1 <- lineup[1]
  home_player2 <- lineup[2]
  home_player3 <- lineup[3]
  home_player4 <- lineup[4]
  home_player5 <- lineup[5]
  away_player1 <- lineup[6]
  away_player2 <- lineup[7]
  away_player3 <- lineup[8]
  away_player4 <- lineup[9]
  away_player5 <- lineup[10]

  zeroRow <- rep(0, length(players_in) * 2)

  # HOME TEAM (OFFENSE) - positive values
  zeroRow[which(players_in == home_player1)] <- 1
  zeroRow[which(players_in == home_player2)] <- 1
  zeroRow[which(players_in == home_player3)] <- 1
  zeroRow[which(players_in == home_player4)] <- 1
  zeroRow[which(players_in == home_player5)] <- 1

  # AWAY TEAM (DEFENSE) - negative values
  # Use second half of the matrix for defensive ratings
  zeroRow[which(players_in == away_player1) + length(players_in)] <- -1
  zeroRow[which(players_in == away_player2) + length(players_in)] <- -1
  zeroRow[which(players_in == away_player3) + length(players_in)] <- -1
  zeroRow[which(players_in == away_player4) + length(players_in)] <- -1
  zeroRow[which(players_in == away_player5) + length(players_in)] <- -1

  return(zeroRow)
}

# Create the player matrix from home team perspective
print("Creating player matrix (this may take a moment)...")
player_matrix_home <- t(apply(
  lineup_data[, c("home_player_1", "home_player_2", "home_player_3", "home_player_4", "home_player_5",
                   "away_player_1", "away_player_2", "away_player_3", "away_player_4", "away_player_5")],
  1,
  function(x) make_matrix_rows(lineup = x, players_in = players)
))

# Convert to sparse matrix for efficiency
player_matrix_home <- as(player_matrix_home, "dgCMatrix")
target_home <- lineup_data$home_ppp100

print(paste0("Home team matrix dimensions: ", nrow(player_matrix_home), " x ", ncol(player_matrix_home)))

# Create matrix from away team perspective (flip the sign)
make_matrix_rows_away <- function(lineup, players_in) {
  home_player1 <- lineup[1]
  home_player2 <- lineup[2]
  home_player3 <- lineup[3]
  home_player4 <- lineup[4]
  home_player5 <- lineup[5]
  away_player1 <- lineup[6]
  away_player2 <- lineup[7]
  away_player3 <- lineup[8]
  away_player4 <- lineup[9]
  away_player5 <- lineup[10]

  zeroRow <- rep(0, length(players_in) * 2)

  # AWAY TEAM (OFFENSE) - positive values
  zeroRow[which(players_in == away_player1)] <- 1
  zeroRow[which(players_in == away_player2)] <- 1
  zeroRow[which(players_in == away_player3)] <- 1
  zeroRow[which(players_in == away_player4)] <- 1
  zeroRow[which(players_in == away_player5)] <- 1

  # HOME TEAM (DEFENSE) - negative values
  zeroRow[which(players_in == home_player1) + length(players_in)] <- -1
  zeroRow[which(players_in == home_player2) + length(players_in)] <- -1
  zeroRow[which(players_in == home_player3) + length(players_in)] <- -1
  zeroRow[which(players_in == home_player4) + length(players_in)] <- -1
  zeroRow[which(players_in == home_player5) + length(players_in)] <- -1

  return(zeroRow)
}

# Create away team matrix
player_matrix_away <- t(apply(
  lineup_data[, c("home_player_1", "home_player_2", "home_player_3", "home_player_4", "home_player_5",
                   "away_player_1", "away_player_2", "away_player_3", "away_player_4", "away_player_5")],
  1,
  function(x) make_matrix_rows_away(lineup = x, players_in = players)
))

player_matrix_away <- as(player_matrix_away, "dgCMatrix")
target_away <- lineup_data$away_ppp100

# Combine both perspectives for more data
player_matrix <- rbind(player_matrix_home, player_matrix_away)
target <- c(target_home, target_away)

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
write_csv(rapm_results, "womens_basketball_RAPM_2024.csv")
print("Results saved to womens_basketball_RAPM_2024.csv")

# Quick summary stats
print(paste0("Players analyzed: ", nrow(rapm_results)))
print(paste0("RAPM range: ", round(min(rapm_results$RAPM, na.rm = TRUE), 2),
             " to ", round(max(rapm_results$RAPM, na.rm = TRUE), 2)))
print(paste0("Mean RAPM: ", round(mean(rapm_results$RAPM, na.rm = TRUE), 2)))
print(paste0("Standard deviation: ", round(sd(rapm_results$RAPM, na.rm = TRUE), 2)))