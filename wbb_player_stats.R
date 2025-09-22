tictoc::tic()
progressr::with_progress({
  wbb_pbp <- wehoop::load_wbb_pbp()
})
tictoc::toc()

player_box <- wehoop::load_wbb_player_box()
team_box <- wehoop::load_wbb_team_box()

# wehoop functions loaded above

# Examine the structure of existing box score data
cat("Player box score data structure:\n")
cat("Dimensions:", nrow(player_box), "rows x", ncol(player_box), "columns\n")
cat("Columns:", paste(names(player_box), collapse = ", "), "\n\n")

cat("Team box score data structure:\n")
cat("Dimensions:", nrow(team_box), "rows x", ncol(team_box), "columns\n")
cat("Columns:", paste(names(team_box), collapse = ", "), "\n\n")

# Look at a sample of the data
cat("Sample player box score data:\n")
head(player_box)

cat("\nSample team box score data:\n")
head(team_box)

# Load dplyr for data manipulation
library(dplyr)

# Create player season stats by aggregating box scores
cat("\n\nCreating player season statistics...\n")
player_season_stats <- player_box |>
  # Filter out players who didn't play (did_not_play == TRUE)
  filter(is.na(did_not_play) | did_not_play == FALSE) |>
  group_by(athlete_id, athlete_display_name, team_id, team_name, team_display_name) |>
  summarise(
    games_played = n(),
    total_minutes = sum(minutes, na.rm = TRUE),
    avg_minutes = mean(minutes, na.rm = TRUE),
    total_points = sum(points, na.rm = TRUE),
    avg_points = mean(points, na.rm = TRUE),
    total_rebounds = sum(rebounds, na.rm = TRUE),
    avg_rebounds = mean(rebounds, na.rm = TRUE),
    total_assists = sum(assists, na.rm = TRUE),
    avg_assists = mean(assists, na.rm = TRUE),
    total_steals = sum(steals, na.rm = TRUE),
    avg_steals = mean(steals, na.rm = TRUE),
    total_blocks = sum(blocks, na.rm = TRUE),
    avg_blocks = mean(blocks, na.rm = TRUE),
    total_turnovers = sum(turnovers, na.rm = TRUE),
    avg_turnovers = mean(turnovers, na.rm = TRUE),
    total_fgm = sum(field_goals_made, na.rm = TRUE),
    total_fga = sum(field_goals_attempted, na.rm = TRUE),
    fg_pct = total_fgm / total_fga,
    total_3pm = sum(three_point_field_goals_made, na.rm = TRUE),
    total_3pa = sum(three_point_field_goals_attempted, na.rm = TRUE),
    three_pt_pct = total_3pm / total_3pa,
    total_ftm = sum(free_throws_made, na.rm = TRUE),
    total_fta = sum(free_throws_attempted, na.rm = TRUE),
    ft_pct = total_ftm / total_fta,
    .groups = 'drop'
  ) |>
  arrange(desc(total_points))

# Create team season stats by aggregating box scores
cat("Creating team season statistics...\n")
team_season_stats <- team_box |>
  group_by(team_id, team_short_display_name, team_display_name) |>
  summarise(
    games_played = n(),
    wins = sum(team_winner == TRUE, na.rm = TRUE),
    losses = sum(team_winner == FALSE, na.rm = TRUE),
    win_pct = wins / games_played,
    total_points = sum(team_score, na.rm = TRUE),
    avg_points = mean(team_score, na.rm = TRUE),
    total_points_allowed = sum(opponent_team_score, na.rm = TRUE),
    avg_points_allowed = mean(opponent_team_score, na.rm = TRUE),
    point_differential = avg_points - avg_points_allowed,
    total_assists = sum(assists, na.rm = TRUE),
    avg_assists = mean(assists, na.rm = TRUE),
    total_rebounds = sum(total_rebounds, na.rm = TRUE),
    avg_rebounds = mean(total_rebounds, na.rm = TRUE),
    total_steals = sum(steals, na.rm = TRUE),
    avg_steals = mean(steals, na.rm = TRUE),
    total_blocks = sum(blocks, na.rm = TRUE),
    avg_blocks = mean(blocks, na.rm = TRUE),
    total_turnovers = sum(total_turnovers, na.rm = TRUE),
    avg_turnovers = mean(total_turnovers, na.rm = TRUE),
    total_fgm = sum(field_goals_made, na.rm = TRUE),
    total_fga = sum(field_goals_attempted, na.rm = TRUE),
    fg_pct = mean(field_goal_pct, na.rm = TRUE),
    total_3pm = sum(three_point_field_goals_made, na.rm = TRUE),
    total_3pa = sum(three_point_field_goals_attempted, na.rm = TRUE),
    three_pt_pct = mean(three_point_field_goal_pct, na.rm = TRUE),
    total_ftm = sum(free_throws_made, na.rm = TRUE),
    total_fta = sum(free_throws_attempted, na.rm = TRUE),
    ft_pct = mean(free_throw_pct, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  arrange(desc(win_pct))

# Display results
cat("\nPlayer Season Stats Summary:\n")
cat("Total players:", nrow(player_season_stats), "\n")
cat("Top 10 scorers:\n")
print(player_season_stats |> select(athlete_display_name, team_name, games_played, avg_points, total_points) |> head(10))

cat("\nTeam Season Stats Summary:\n")
cat("Total teams:", nrow(team_season_stats), "\n")
cat("Top 10 teams by win percentage:\n")
print(team_season_stats |> select(team_display_name, games_played, wins, losses, win_pct, avg_points) |> head(10))


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


