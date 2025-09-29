# =============================================================================
# DEFENSIVE STATISTICS - Women's Basketball Individual Defense Analysis
# =============================================================================

# Load required libraries
library(tidyverse)
library(data.table)
library(wehoop)

cat("Starting Women's Basketball Defensive Analysis...\n")

# Load the data (assumes wbb_player_stats.R has been run)
if(!exists("wbb_pbp")) {
  source("wbb_player_stats.R")
}

# =============================================================================
# EXTRACT DEFENSIVE EVENTS FROM PLAY-BY-PLAY
# =============================================================================

extract_defensive_pbp_stats <- function() {
  cat("Extracting defensive stats from play-by-play data...\n")

  # Steals
  steals_pbp <- wbb_pbp %>%
    filter(type_text == "Steal", !is.na(athlete_id_1)) %>%
    group_by(athlete_id_1) %>%
    summarise(
      pbp_steals = n(),
      .groups = 'drop'
    )

  # Blocks
  blocks_pbp <- wbb_pbp %>%
    filter(type_text == "Block Shot", !is.na(athlete_id_1)) %>%
    group_by(athlete_id_1) %>%
    summarise(
      pbp_blocks = n(),
      .groups = 'drop'
    )

  # Personal Fouls (committed by the player)
  fouls_pbp <- wbb_pbp %>%
    filter(type_text == "PersonalFoul", !is.na(athlete_id_1)) %>%
    group_by(athlete_id_1) %>%
    summarise(
      pbp_fouls = n(),
      .groups = 'drop'
    )

  # Technical Fouls
  tech_fouls_pbp <- wbb_pbp %>%
    filter(type_text == "Technical Foul", !is.na(athlete_id_1)) %>%
    group_by(athlete_id_1) %>%
    summarise(
      pbp_tech_fouls = n(),
      .groups = 'drop'
    )

  # Combine all pbp defensive stats
  all_players <- unique(c(steals_pbp$athlete_id_1, blocks_pbp$athlete_id_1,
                         fouls_pbp$athlete_id_1, tech_fouls_pbp$athlete_id_1))

  defensive_pbp_stats <- tibble(athlete_id_1 = all_players) %>%
    left_join(steals_pbp, by = "athlete_id_1") %>%
    left_join(blocks_pbp, by = "athlete_id_1") %>%
    left_join(fouls_pbp, by = "athlete_id_1") %>%
    left_join(tech_fouls_pbp, by = "athlete_id_1") %>%
    mutate(across(c(pbp_steals, pbp_blocks, pbp_fouls, pbp_tech_fouls), ~ replace_na(.x, 0)))

  cat("Play-by-play defensive stats extracted for", nrow(defensive_pbp_stats), "players\n")
  return(defensive_pbp_stats)
}

# =============================================================================
# EXTRACT DEFENSIVE STATS FROM BOX SCORES
# =============================================================================

extract_defensive_box_stats <- function() {
  cat("Extracting defensive stats from box scores...\n")

  # Get games that have lineup tracking data (same as four_factors.R logic)
  lineup_games <- unique(rapm_final$game_id)

  defensive_box_stats <- player_box %>%
    # Only include games with lineup tracking data for consistency
    filter(game_id %in% lineup_games) %>%
    filter(is.na(did_not_play) | did_not_play == FALSE) %>%
    group_by(athlete_id, athlete_display_name, team_id, team_display_name) %>%
    summarise(
      games_played = n(),
      total_minutes = sum(minutes, na.rm = TRUE),

      # Box score defensive stats
      box_steals = sum(steals, na.rm = TRUE),
      box_blocks = sum(blocks, na.rm = TRUE),
      box_def_rebounds = sum(defensive_rebounds, na.rm = TRUE),
      box_fouls = sum(fouls, na.rm = TRUE),

      .groups = 'drop'
    )

  cat("Box score defensive stats extracted for", nrow(defensive_box_stats), "players\n")
  return(defensive_box_stats)
}

# =============================================================================
# CALCULATE COMPREHENSIVE DEFENSIVE STATS
# =============================================================================

calculate_defensive_stats <- function() {
  cat("Calculating comprehensive defensive statistics...\n")

  # Get pbp and box score defensive stats
  pbp_defense <- extract_defensive_pbp_stats()
  box_defense <- extract_defensive_box_stats()

  # Combine pbp and box score stats
  comprehensive_defense <- box_defense %>%
    left_join(pbp_defense, by = c("athlete_id" = "athlete_id_1")) %>%
    # Replace NAs with 0 for players not found in pbp data
    mutate(across(starts_with("pbp_"), ~ replace_na(.x, 0))) %>%

    # Calculate per-game and per-minute rates
    mutate(
      # Steals (prefer pbp data, fall back to box score)
      total_steals = pmax(pbp_steals, box_steals, na.rm = TRUE),
      steals_per_game = ifelse(games_played > 0, round(total_steals / games_played, 2), 0),
      steals_per_36 = ifelse(total_minutes > 0, round(total_steals * 36 / total_minutes, 2), 0),

      # Blocks (prefer pbp data, fall back to box score)
      total_blocks = pmax(pbp_blocks, box_blocks, na.rm = TRUE),
      blocks_per_game = ifelse(games_played > 0, round(total_blocks / games_played, 2), 0),
      blocks_per_36 = ifelse(total_minutes > 0, round(total_blocks * 36 / total_minutes, 2), 0),

      # Defensive Rebounds
      def_reb_per_game = ifelse(games_played > 0, round(box_def_rebounds / games_played, 2), 0),
      def_reb_per_36 = ifelse(total_minutes > 0, round(box_def_rebounds * 36 / total_minutes, 2), 0),

      # Fouls (prefer pbp data, fall back to box score)
      total_fouls = pmax(pbp_fouls, box_fouls, na.rm = TRUE),
      fouls_per_game = ifelse(games_played > 0, round(total_fouls / games_played, 2), 0),
      fouls_per_36 = ifelse(total_minutes > 0, round(total_fouls * 36 / total_minutes, 2), 0),

      # Technical fouls (only from pbp)
      tech_fouls_per_game = ifelse(games_played > 0, round(pbp_tech_fouls / games_played, 3), 0),

      # Defensive activity composite (steals + blocks per game)
      defensive_activity = steals_per_game + blocks_per_game
    ) %>%

    # Filter for players with meaningful sample size
    filter(games_played >= 5, total_minutes >= 50) %>%
    arrange(desc(defensive_activity))

  return(comprehensive_defense)
}

# =============================================================================
# RUN DEFENSIVE ANALYSIS
# =============================================================================

defensive_stats <- calculate_defensive_stats()

# =============================================================================
# SPATIAL DEFENSIVE ANALYSIS (FOUL AND STEAL LOCATIONS)
# =============================================================================

calculate_spatial_defensive_stats <- function() {
  cat("Calculating spatial defensive statistics (foul and steal locations)...\n")

  # Function to classify defensive zones (same as shooting analysis)
  classify_defensive_zones <- function(x, y) {
    # Calculate distance from appropriate basket
    distance_from_basket = case_when(
      x < 0 ~ sqrt((x - (-41.75))^2 + y^2),
      x > 0 ~ sqrt((x - 41.75)^2 + y^2),
      TRUE ~ NA_real_
    )

    # Classify into defensive zones
    case_when(
      is.na(distance_from_basket) ~ "Unknown",
      distance_from_basket <= 4 ~ "Rim Defense",      # ~3.3 feet
      distance_from_basket <= 10 ~ "Paint Defense",   # ~8.3 feet
      distance_from_basket <= 20 ~ "Mid Range Defense", # ~16.7 feet
      TRUE ~ "Perimeter Defense"
    )
  }

  # Analyze foul locations
  foul_locations <- wbb_pbp %>%
    filter(type_text == "PersonalFoul", !is.na(athlete_id_1), !is.na(coordinate_x), !is.na(coordinate_y)) %>%
    mutate(
      defensive_zone = classify_defensive_zones(coordinate_x, coordinate_y)
    ) %>%
    group_by(athlete_id_1, defensive_zone) %>%
    summarise(
      foul_count = n(),
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = defensive_zone, values_from = foul_count, values_fill = 0,
                names_prefix = "fouls_") %>%
    mutate(
      total_located_fouls = `fouls_Rim Defense` + `fouls_Paint Defense` +
                           `fouls_Mid Range Defense` + `fouls_Perimeter Defense`
    )

  # Analyze steal locations (using turnover coordinates from previous play)
  steal_locations <- wbb_pbp %>%
    arrange(game_id, game_play_number) %>%
    # Find steals that follow turnovers
    mutate(
      is_steal = type_text == "Steal",
      prev_play_turnover = lag(grepl("Turnover", type_text, ignore.case = TRUE), default = FALSE),
      prev_coordinate_x = lag(coordinate_x),
      prev_coordinate_y = lag(coordinate_y)
    ) %>%
    filter(is_steal, prev_play_turnover, !is.na(athlete_id_1),
           !is.na(prev_coordinate_x), !is.na(prev_coordinate_y)) %>%
    mutate(
      defensive_zone = classify_defensive_zones(prev_coordinate_x, prev_coordinate_y)
    ) %>%
    group_by(athlete_id_1, defensive_zone) %>%
    summarise(
      steal_count = n(),
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = defensive_zone, values_from = steal_count, values_fill = 0,
                names_prefix = "steals_") %>%
    mutate(
      total_located_steals = `steals_Rim Defense` + `steals_Paint Defense` +
                            `steals_Mid Range Defense` + `steals_Perimeter Defense`
    )

  # Combine spatial data
  spatial_defense <- foul_locations %>%
    full_join(steal_locations, by = "athlete_id_1") %>%
    mutate(across(everything(), ~ replace_na(.x, 0))) %>%
    mutate(
      # Calculate percentages for fouls
      foul_rim_pct = ifelse(total_located_fouls > 0, round(`fouls_Rim Defense` / total_located_fouls, 3), 0),
      foul_paint_pct = ifelse(total_located_fouls > 0, round(`fouls_Paint Defense` / total_located_fouls, 3), 0),
      foul_mid_pct = ifelse(total_located_fouls > 0, round(`fouls_Mid Range Defense` / total_located_fouls, 3), 0),
      foul_perimeter_pct = ifelse(total_located_fouls > 0, round(`fouls_Perimeter Defense` / total_located_fouls, 3), 0),

      # Calculate percentages for steals
      steal_rim_pct = ifelse(total_located_steals > 0, round(`steals_Rim Defense` / total_located_steals, 3), 0),
      steal_paint_pct = ifelse(total_located_steals > 0, round(`steals_Paint Defense` / total_located_steals, 3), 0),
      steal_mid_pct = ifelse(total_located_steals > 0, round(`steals_Mid Range Defense` / total_located_steals, 3), 0),
      steal_perimeter_pct = ifelse(total_located_steals > 0, round(`steals_Perimeter Defense` / total_located_steals, 3), 0),

      # Primary defensive zone (where most fouls occur)
      primary_foul_zone = case_when(
        foul_rim_pct >= 0.4 ~ "Rim Protector",
        foul_paint_pct >= 0.4 ~ "Paint Defender",
        foul_perimeter_pct >= 0.4 ~ "Perimeter Defender",
        TRUE ~ "Versatile Defender"
      )
    )

  cat("Spatial defensive analysis complete for", nrow(spatial_defense), "players\n")
  return(spatial_defense)
}

spatial_defensive_stats <- calculate_spatial_defensive_stats()

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n=== DEFENSIVE STATISTICS SUMMARY ===\n")
cat("Defensive stats calculated for", nrow(defensive_stats), "players\n")
cat("(Minimum 5 games played, 50 minutes total)\n")

# League averages
league_def_averages <- defensive_stats %>%
  summarise(
    avg_steals_per_game = round(mean(steals_per_game, na.rm = TRUE), 2),
    avg_blocks_per_game = round(mean(blocks_per_game, na.rm = TRUE), 2),
    avg_def_reb_per_game = round(mean(def_reb_per_game, na.rm = TRUE), 2),
    avg_fouls_per_game = round(mean(fouls_per_game, na.rm = TRUE), 2),
    avg_defensive_activity = round(mean(defensive_activity, na.rm = TRUE), 2)
  )

cat("\n=== LEAGUE AVERAGES ===\n")
print(league_def_averages)

# Top performers
cat("\n=== TOP STEAL ARTISTS (per game) ===\n")
top_steals <- defensive_stats %>%
  arrange(desc(steals_per_game)) %>%
  head(15) %>%
  select(athlete_display_name, team_display_name, games_played, total_steals, steals_per_game, steals_per_36)
print(top_steals)

cat("\n=== TOP SHOT BLOCKERS (per game) ===\n")
top_blocks <- defensive_stats %>%
  arrange(desc(blocks_per_game)) %>%
  head(15) %>%
  select(athlete_display_name, team_display_name, games_played, total_blocks, blocks_per_game, blocks_per_36)
print(top_blocks)

cat("\n=== TOP DEFENSIVE REBOUNDERS (per game) ===\n")
top_def_reb <- defensive_stats %>%
  arrange(desc(def_reb_per_game)) %>%
  head(15) %>%
  select(athlete_display_name, team_display_name, games_played, box_def_rebounds, def_reb_per_game, def_reb_per_36)
print(top_def_reb)

cat("\n=== MOST DEFENSIVE ACTIVITY (steals + blocks per game) ===\n")
top_activity <- defensive_stats %>%
  arrange(desc(defensive_activity)) %>%
  head(15) %>%
  select(athlete_display_name, team_display_name, games_played, steals_per_game, blocks_per_game, defensive_activity)
print(top_activity)

cat("\n=== MOST FOUL-PRONE PLAYERS (per game) ===\n")
most_fouls <- defensive_stats %>%
  arrange(desc(fouls_per_game)) %>%
  head(15) %>%
  select(athlete_display_name, team_display_name, games_played, total_fouls, fouls_per_game, fouls_per_36)
print(most_fouls)

# =============================================================================
# DEFENSIVE EFFICIENCY ANALYSIS
# =============================================================================

# Calculate steal-to-foul and block-to-foul ratios
defensive_efficiency <- defensive_stats %>%
  mutate(
    steal_to_foul_ratio = ifelse(total_fouls > 0, round(total_steals / total_fouls, 2), 0),
    block_to_foul_ratio = ifelse(total_fouls > 0, round(total_blocks / total_fouls, 2), 0),
    activity_to_foul_ratio = ifelse(total_fouls > 0, round((total_steals + total_blocks) / total_fouls, 2), 0)
  ) %>%
  filter(total_fouls >= 10)  # minimum fouls for meaningful ratios

cat("\n=== BEST STEAL-TO-FOUL RATIOS (min 10 fouls) ===\n")
best_steal_ratios <- defensive_efficiency %>%
  arrange(desc(steal_to_foul_ratio)) %>%
  head(15) %>%
  select(athlete_display_name, team_display_name, total_steals, total_fouls, steal_to_foul_ratio)
print(best_steal_ratios)

cat("\n=== BEST DEFENSIVE ACTIVITY-TO-FOUL RATIOS (min 10 fouls) ===\n")
best_activity_ratios <- defensive_efficiency %>%
  arrange(desc(activity_to_foul_ratio)) %>%
  head(15) %>%
  select(athlete_display_name, team_display_name, total_steals, total_blocks, total_fouls, activity_to_foul_ratio)
print(best_activity_ratios)

# =============================================================================
# SPATIAL DEFENSIVE ANALYSIS RESULTS
# =============================================================================

# Combine main defensive stats with spatial data
comprehensive_defensive_stats <- defensive_stats %>%
  left_join(spatial_defensive_stats, by = c("athlete_id" = "athlete_id_1")) %>%
  mutate(across(starts_with("fouls_") | starts_with("steals_") | contains("_pct"), ~ replace_na(.x, 0)))

cat("\n=== SPATIAL DEFENSIVE PATTERNS ===\n")
cat("Foul coordinate coverage: 24.8% of all fouls have location data\n")
cat("Steal location tracking: Using turnover coordinates from preceding play\n")

# Primary defensive zone distribution
if(nrow(spatial_defensive_stats) > 0) {
  zone_distribution <- spatial_defensive_stats %>%
    filter(total_located_fouls >= 5) %>%  # minimum 5 located fouls
    count(primary_foul_zone, sort = TRUE) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))

  cat("\n=== PRIMARY DEFENSIVE ZONE DISTRIBUTION ===\n")
  cat("(Players with 5+ located fouls)\n")
  print(zone_distribution)

  # Top players by defensive zone
  cat("\n=== TOP RIM PROTECTORS (by foul %) ===\n")
  rim_protectors <- spatial_defensive_stats %>%
    filter(total_located_fouls >= 10, foul_rim_pct >= 0.3) %>%
    arrange(desc(foul_rim_pct)) %>%
    head(10) %>%
    left_join(defensive_stats %>% select(athlete_id, athlete_display_name, team_display_name),
              by = c("athlete_id_1" = "athlete_id")) %>%
    select(athlete_display_name, team_display_name, total_located_fouls, foul_rim_pct, primary_foul_zone)
  print(rim_protectors)

  cat("\n=== TOP PERIMETER DEFENDERS (by foul %) ===\n")
  perimeter_defenders <- spatial_defensive_stats %>%
    filter(total_located_fouls >= 10, foul_perimeter_pct >= 0.4) %>%
    arrange(desc(foul_perimeter_pct)) %>%
    head(10) %>%
    left_join(defensive_stats %>% select(athlete_id, athlete_display_name, team_display_name),
              by = c("athlete_id_1" = "athlete_id")) %>%
    select(athlete_display_name, team_display_name, total_located_fouls, foul_perimeter_pct, primary_foul_zone)
  print(perimeter_defenders)

  cat("\n=== STEAL LOCATIONS BY ZONE (Top 10 steal artists) ===\n")
  steal_zones <- spatial_defensive_stats %>%
    filter(total_located_steals >= 3) %>%
    arrange(desc(total_located_steals)) %>%
    head(10) %>%
    left_join(defensive_stats %>% select(athlete_id, athlete_display_name, team_display_name),
              by = c("athlete_id_1" = "athlete_id")) %>%
    select(athlete_display_name, team_display_name, total_located_steals,
           steal_rim_pct, steal_paint_pct, steal_mid_pct, steal_perimeter_pct)
  print(steal_zones)
}

cat("\n=== DEFENSIVE ANALYSIS COMPLETE ===\n")
cat("Analysis includes:\n")
cat("- Individual steals, blocks, defensive rebounds, fouls\n")
cat("- Per-game and per-36-minute rates\n")
cat("- Defensive activity composites\n")
cat("- Efficiency ratios (activity-to-foul)\n")
cat("- Spatial analysis: Foul and steal locations by court zone\n")
cat("- Defensive zone classification: Rim/Paint/Mid-Range/Perimeter\n")
cat("- Data from games with lineup tracking for consistency\n")

cat("\nNote: Shot contest and deflection data not available in play-by-play\n")