library(tidyverse)

# Source the main script to get rapm_final dataset
source("wbb_player_stats.R")

# Check if rapm_final exists
if(!exists("rapm_final")) {
  stop("rapm_final dataset not found. Please run wbb_player_stats.R first.")
}

cat("Calculating plus/minus for women's basketball players...\n")
cat("Dataset dimensions:", nrow(rapm_final), "plays x", ncol(rapm_final), "variables\n")

# Calculate plus/minus for each play
calculate_plus_minus <- function() {
  # Filter to plays with complete lineup data and scoring information
  pm_data <- rapm_final %>%
    filter(!is.na(home_player_1) & !is.na(home_player_2) & !is.na(home_player_3) &
           !is.na(home_player_4) & !is.na(home_player_5) &
           !is.na(away_player_1) & !is.na(away_player_2) & !is.na(away_player_3) &
           !is.na(away_player_4) & !is.na(away_player_5) &
           !is.na(home_score) & !is.na(away_score)) %>%
    arrange(game_id, period_number, desc(clock_display_value)) %>%
    group_by(game_id) %>%
    # Calculate score changes for each play
    mutate(
      home_score_change = c(0, diff(home_score)),
      away_score_change = c(0, diff(away_score)),
      # Net score change from home team perspective
      net_score_change = home_score_change - away_score_change
    ) %>%
    ungroup()

  return(pm_data)
}

pm_data <- calculate_plus_minus()

cat("Plays with plus/minus data:", nrow(pm_data), "\n")

# Create player plus/minus summary
calculate_player_plus_minus <- function(pm_data) {
  # Home team players
  home_pm <- pm_data %>%
    select(game_id, game_play_number, home_player_1:home_player_5, net_score_change) %>%
    pivot_longer(cols = home_player_1:home_player_5,
                 names_to = "position",
                 values_to = "player_id") %>%
    mutate(plus_minus = net_score_change) %>%
    select(game_id, game_play_number, player_id, plus_minus)

  # Away team players (flip the sign since net_score_change is from home perspective)
  away_pm <- pm_data %>%
    select(game_id, game_play_number, away_player_1:away_player_5, net_score_change) %>%
    pivot_longer(cols = away_player_1:away_player_5,
                 names_to = "position",
                 values_to = "player_id") %>%
    mutate(plus_minus = -net_score_change) %>%  # Flip sign for away team
    select(game_id, game_play_number, player_id, plus_minus)

  # Combine home and away
  all_pm <- bind_rows(home_pm, away_pm) %>%
    filter(!is.na(player_id))

  return(all_pm)
}

all_player_pm <- calculate_player_plus_minus(pm_data)

cat("Player-play observations:", nrow(all_player_pm), "\n")

# Get player minutes from box scores
player_minutes <- player_box %>%
  filter(is.na(did_not_play) | did_not_play == FALSE) %>%
  group_by(athlete_id, athlete_display_name, team_display_name) %>%
  summarise(
    games_played = n(),
    total_minutes = sum(minutes, na.rm = TRUE),
    avg_minutes = mean(minutes, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate season plus/minus by player
player_plus_minus <- all_player_pm %>%
  group_by(player_id) %>%
  summarise(
    total_plays = n(),
    total_plus_minus = sum(plus_minus, na.rm = TRUE),
    avg_plus_minus_per_play = mean(plus_minus, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Join with player info and minutes
  left_join(player_minutes, by = c("player_id" = "athlete_id")) %>%
  filter(!is.na(athlete_display_name) & total_minutes > 0) %>%
  # Calculate plus/minus per minute
  mutate(
    plus_minus_per_minute = total_plus_minus / total_minutes,
    plus_minus_per_36_min = plus_minus_per_minute * 36  # Per 36 minutes
  ) %>%
  # Filter to players with meaningful playing time (at least 5 games and 50 minutes)
  filter(games_played >= 5 & total_minutes >= 50) %>%
  arrange(desc(total_plus_minus))

# Display results
cat("\n=== SEASON PLUS/MINUS LEADERS ===\n")
cat("Top 20 players by total plus/minus:\n")
top_pm <- player_plus_minus %>%
  select(Player = athlete_display_name,
         Team = team_display_name,
         Games = games_played,
         Minutes = total_minutes,
         `Total +/-` = total_plus_minus,
         `+/- per Min` = plus_minus_per_minute,
         `+/- per 36` = plus_minus_per_36_min) %>%
  head(20) %>%
  mutate(across(where(is.numeric), function(x) round(x, 2)))

print(top_pm)

cat("\n=== PLUS/MINUS PER MINUTE LEADERS ===\n")
cat("Top 20 players by plus/minus per minute:\n")
top_pm_per_min <- player_plus_minus %>%
  arrange(desc(plus_minus_per_minute)) %>%
  select(Player = athlete_display_name,
         Team = team_display_name,
         Games = games_played,
         Minutes = total_minutes,
         `Total +/-` = total_plus_minus,
         `+/- per Min` = plus_minus_per_minute,
         `+/- per 36` = plus_minus_per_36_min) %>%
  head(20) %>%
  mutate(across(where(is.numeric), function(x) round(x, 2)))

print(top_pm_per_min)

# Save results
write_csv(player_plus_minus %>%
            select(Player = athlete_display_name,
                   Team = team_display_name,
                   Games = games_played,
                   Minutes = total_minutes,
                   `Total Plus Minus` = total_plus_minus,
                   `Plus Minus per Minute` = plus_minus_per_minute,
                   `Plus Minus per 36 Min` = plus_minus_per_36_min) %>%
            mutate(across(where(is.numeric), function(x) round(x, 3))),
          "womens_basketball_plus_minus_2024.csv")

# Summary statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Players analyzed:", nrow(player_plus_minus), "\n")
cat("Total plus/minus range:", round(min(player_plus_minus$total_plus_minus), 1),
    "to", round(max(player_plus_minus$total_plus_minus), 1), "\n")
cat("Plus/minus per minute range:", round(min(player_plus_minus$plus_minus_per_minute), 3),
    "to", round(max(player_plus_minus$plus_minus_per_minute), 3), "\n")
cat("Mean plus/minus per minute:", round(mean(player_plus_minus$plus_minus_per_minute), 3), "\n")

cat("\nResults saved to womens_basketball_plus_minus_2024.csv\n")

# Create plus_minus_stats object for compatibility with other scripts
plus_minus_stats <- player_plus_minus %>%
  select(
    athlete_id = player_id,
    athlete_display_name,
    team_display_name,
    games_played,
    total_minutes,
    total_plus_minus,
    plus_minus_per_minute
  ) %>%
  # Add team_points_for calculation (needed for shooting_analysis)
  left_join(
    all_player_pm %>%
      group_by(player_id) %>%
      # Calculate total points scored by player's team while on court
      summarise(
        team_points_for = sum(pmax(plus_minus, 0), na.rm = TRUE), # Points gained while on court
        .groups = 'drop'
      ),
    by = c("athlete_id" = "player_id")
  )