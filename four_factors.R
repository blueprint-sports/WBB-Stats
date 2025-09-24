library(tidyverse)
library(knitr)

# Four Factors Analysis for Women's Basketball
# Calculates eFG%, TO Rate, OREB Rate, FT Rate for players and teams
# Includes both individual stats and on-court team performance

# Source the main script to get data
source("wbb_player_stats.R")

# Check if required datasets exist
if(!exists("player_box") || !exists("team_box") || !exists("rapm_final")) {
  stop("Required datasets not found. Please run wbb_player_stats.R first.")
}

cat("Calculating Four Factors for Women's Basketball...\n")
cat("Player box score rows:", nrow(player_box), "\n")
cat("Team box score rows:", nrow(team_box), "\n")
cat("Play-by-play rows with lineups:", nrow(rapm_final), "\n")

# =============================================================================
# INDIVIDUAL PLAYER FOUR FACTORS (from box scores)
# =============================================================================

cat("\n=== CALCULATING INDIVIDUAL PLAYER FOUR FACTORS ===\n")

# Calculate individual player stats combining pbp shooting and box score other stats
calculate_player_individual_stats <- function() {

  # Get games that have lineup tracking data
  lineup_games <- unique(rapm_final$game_id)

  # Get non-shooting stats from box scores (ONLY from games with lineup data)
  player_other_stats <- player_box %>%
    # Only include games with lineup tracking data
    filter(game_id %in% lineup_games) %>%
    filter(is.na(did_not_play) | did_not_play == FALSE) %>%
    group_by(athlete_id, athlete_display_name, team_id, team_display_name) %>%
    summarise(
      games_played = n(),
      total_minutes = sum(minutes, na.rm = TRUE),
      ind_oreb = sum(offensive_rebounds, na.rm = TRUE),
      ind_dreb = sum(defensive_rebounds, na.rm = TRUE),
      ind_turnovers = sum(turnovers, na.rm = TRUE),
      .groups = 'drop'
    )

  # Combine with shooting stats from pbp
  player_individual <- player_other_stats %>%
    left_join(individual_shooting_stats, by = c("athlete_id" = "athlete_id_1")) %>%
    # Replace NAs with 0 for players with no shooting data
    mutate(across(c(ind_2pm, ind_2pa, ind_3pm, ind_3pa, ind_fgm, ind_fga, ind_ftm, ind_fta),
                  ~ replace_na(.x, 0))) %>%
    # Calculate individual four factors
    mutate(
      # Individual eFG%
      ind_efg_pct = ifelse(ind_fga > 0, (ind_fgm + 0.5 * ind_3pm) / ind_fga, 0),

      # Individual FT Rate
      ind_ft_rate = ifelse(ind_fga > 0, ind_fta / ind_fga, 0),

      # Individual TO Rate
      ind_to_rate = ifelse((ind_fga + 0.5 * ind_fta + ind_turnovers) > 0,
                           ind_turnovers / (ind_fga + 0.5 * ind_fta + ind_turnovers), 0)

      # Note: Individual OREB/DREB rates need team context, calculated below
    )

  return(player_individual)
}

# Extract individual player shooting stats from enhanced pbp data
extract_individual_shooting <- function() {
  cat("Extracting individual shooting stats from play-by-play...\n")

  individual_shooting <- wbb_pbp %>%
    filter((is_fga | is_fta) & !is.na(athlete_id_1)) %>%
    group_by(athlete_id_1) %>%
    summarise(
      ind_2pm = sum(is_2pm, na.rm = TRUE),
      ind_2pa = sum(is_2pa, na.rm = TRUE),
      ind_3pm = sum(is_3pm, na.rm = TRUE),
      ind_3pa = sum(is_3pa, na.rm = TRUE),
      ind_fgm = sum(is_fgm, na.rm = TRUE),
      ind_fga = sum(is_fga, na.rm = TRUE),
      ind_ftm = sum(is_ftm, na.rm = TRUE),
      ind_fta = sum(is_fta, na.rm = TRUE),
      .groups = 'drop'
    )

  return(individual_shooting)
}

# Note: individual_shooting_stats will be created after enhancing wbb_pbp

# =============================================================================
# ON-COURT TEAM STATISTICS FOR EACH PLAYER
# =============================================================================

cat("\n=== CALCULATING ON-COURT TEAM STATISTICS ===\n")

# Extract play-by-play events we need for four factors
prepare_pbp_events <- function() {

  # Get shooting events (now using the enhanced variables)
  shooting_events <- wbb_pbp %>%
    filter(is_fga | is_fta) %>%
    select(game_id, game_play_number, type_text, team_id, home_team_id, away_team_id,
           athlete_id_1, text, is_2pa, is_3pa, is_2pm, is_3pm, is_fga, is_fgm, is_fta, is_ftm)

  # Get rebound events
  rebound_events <- wbb_pbp %>%
    filter(grepl("Rebound", type_text, ignore.case = TRUE)) %>%
    select(game_id, game_play_number, type_text, team_id, home_team_id, away_team_id, text) %>%
    mutate(
      is_offensive = grepl("Offensive", type_text, ignore.case = TRUE),
      is_defensive = grepl("Defensive", type_text, ignore.case = TRUE)
    )

  # Get turnover events
  turnover_events <- wbb_pbp %>%
    filter(grepl("Turnover", type_text, ignore.case = TRUE)) %>%
    select(game_id, game_play_number, type_text, team_id, home_team_id, away_team_id, text)

  return(list(
    shooting = shooting_events,
    rebounds = rebound_events,
    turnovers = turnover_events
  ))
}

# First, enhance wbb_pbp with shot type variables
cat("Adding shot type variables to play-by-play data...\n")
wbb_pbp <- wbb_pbp %>%
  mutate(
    # Parse shooting attempts from text
    is_three_point_shot = grepl("Three Point", text, ignore.case = TRUE),
    is_shot_made = grepl("made", text, ignore.case = TRUE),
    is_shot_missed = grepl("missed", text, ignore.case = TRUE),
    is_shot_attempt = type_text %in% c("JumpShot", "LayUpShot", "TipShot", "DunkShot"),
    is_free_throw = type_text == "MadeFreeThrow" | grepl("Free Throw", text, ignore.case = TRUE),

    # Shot type classifications
    is_2pa = is_shot_attempt & !is_three_point_shot,
    is_3pa = is_shot_attempt & is_three_point_shot,
    is_2pm = is_2pa & is_shot_made,
    is_3pm = is_3pa & is_shot_made,
    is_fga = is_shot_attempt,
    is_fgm = is_shot_attempt & is_shot_made,
    is_fta = is_free_throw,
    is_ftm = is_free_throw & grepl("made", text, ignore.case = TRUE)
  )

cat("Shot type variables added to wbb_pbp\n")

# Now extract individual shooting stats using the enhanced data
individual_shooting_stats <- extract_individual_shooting()
player_individual_stats <- calculate_player_individual_stats()
cat("Individual player stats calculated for", nrow(player_individual_stats), "players\n")

cat("Preparing play-by-play events...\n")
pbp_events <- prepare_pbp_events()

cat("Events extracted:\n")
cat("- Shooting events:", nrow(pbp_events$shooting), "\n")
cat("- Rebound events:", nrow(pbp_events$rebounds), "\n")
cat("- Turnover events:", nrow(pbp_events$turnovers), "\n")

# Calculate on-court statistics for each player
calculate_on_court_stats <- function() {

  cat("Calculating on-court statistics for each player...\n")

  # Create long format of players on court
  players_on_court <- rapm_final %>%
    select(game_id, game_play_number, home_player_1:home_player_5, away_player_1:away_player_5) %>%
    pivot_longer(cols = home_player_1:away_player_5,
                 names_to = "position",
                 values_to = "player_id") %>%
    filter(!is.na(player_id)) %>%
    mutate(
      team_side = ifelse(grepl("home", position), "home", "away")
    ) %>%
    select(game_id, game_play_number, player_id, team_side)

  cat("Player-play combinations:", nrow(players_on_court), "\n")

  # Join with shooting events
  cat("Processing shooting events...\n")
  shooting_on_court <- players_on_court %>%
    inner_join(pbp_events$shooting, by = c("game_id", "game_play_number")) %>%
    mutate(
      # Determine if this is the player's team shooting or opponent
      is_team_shooting = (team_side == "home" & team_id == home_team_id) |
                        (team_side == "away" & team_id == away_team_id),
      is_opp_shooting = !is_team_shooting
    )

  # Join with rebound events
  cat("Processing rebound events...\n")
  rebounds_on_court <- players_on_court %>%
    inner_join(pbp_events$rebounds, by = c("game_id", "game_play_number")) %>%
    mutate(
      is_team_rebound = (team_side == "home" & team_id == home_team_id) |
                       (team_side == "away" & team_id == away_team_id),
      is_opp_rebound = !is_team_rebound
    )

  # Join with turnover events
  cat("Processing turnover events...\n")
  turnovers_on_court <- players_on_court %>%
    inner_join(pbp_events$turnovers, by = c("game_id", "game_play_number")) %>%
    mutate(
      is_team_turnover = (team_side == "home" & team_id == home_team_id) |
                        (team_side == "away" & team_id == away_team_id),
      is_opp_turnover = !is_team_turnover
    )

  # Aggregate by player
  cat("Aggregating on-court statistics...\n")

  # Team shooting while player on court
  team_shooting_stats <- shooting_on_court %>%
    filter(is_team_shooting) %>%
    group_by(player_id) %>%
    summarise(
      team_2pm = sum(is_2pm, na.rm = TRUE),
      team_2pa = sum(is_2pa, na.rm = TRUE),
      team_3pm = sum(is_3pm, na.rm = TRUE),
      team_3pa = sum(is_3pa, na.rm = TRUE),
      team_fgm = sum(is_fgm, na.rm = TRUE),
      team_fga = sum(is_fga, na.rm = TRUE),
      team_ftm = sum(is_ftm, na.rm = TRUE),
      team_fta = sum(is_fta, na.rm = TRUE),
      .groups = 'drop'
    )

  # Opponent shooting while player on court
  opp_shooting_stats <- shooting_on_court %>%
    filter(is_opp_shooting) %>%
    group_by(player_id) %>%
    summarise(
      opp_2pm = sum(is_2pm, na.rm = TRUE),
      opp_2pa = sum(is_2pa, na.rm = TRUE),
      opp_3pm = sum(is_3pm, na.rm = TRUE),
      opp_3pa = sum(is_3pa, na.rm = TRUE),
      opp_fgm = sum(is_fgm, na.rm = TRUE),
      opp_fga = sum(is_fga, na.rm = TRUE),
      opp_ftm = sum(is_ftm, na.rm = TRUE),
      opp_fta = sum(is_fta, na.rm = TRUE),
      .groups = 'drop'
    )

  # Team rebounds while player on court
  team_rebound_stats <- rebounds_on_court %>%
    filter(is_team_rebound) %>%
    group_by(player_id) %>%
    summarise(
      team_oreb = sum(is_offensive, na.rm = TRUE),
      team_dreb = sum(is_defensive, na.rm = TRUE),
      .groups = 'drop'
    )

  # Opponent rebounds while player on court
  opp_rebound_stats <- rebounds_on_court %>%
    filter(is_opp_rebound) %>%
    group_by(player_id) %>%
    summarise(
      opp_oreb = sum(is_offensive, na.rm = TRUE),
      opp_dreb = sum(is_defensive, na.rm = TRUE),
      .groups = 'drop'
    )

  # Team turnovers while player on court
  team_turnover_stats <- turnovers_on_court %>%
    filter(is_team_turnover) %>%
    group_by(player_id) %>%
    summarise(
      team_turnovers = n(),
      .groups = 'drop'
    )

  # Opponent turnovers while player on court
  opp_turnover_stats <- turnovers_on_court %>%
    filter(is_opp_turnover) %>%
    group_by(player_id) %>%
    summarise(
      opp_turnovers = n(),
      .groups = 'drop'
    )

  # Combine all on-court stats
  cat("Combining all on-court statistics...\n")

  all_players <- unique(players_on_court$player_id)

  on_court_stats <- tibble(player_id = all_players) %>%
    left_join(team_shooting_stats, by = "player_id") %>%
    left_join(opp_shooting_stats, by = "player_id") %>%
    left_join(team_rebound_stats, by = "player_id") %>%
    left_join(opp_rebound_stats, by = "player_id") %>%
    left_join(team_turnover_stats, by = "player_id") %>%
    left_join(opp_turnover_stats, by = "player_id") %>%
    # Replace NAs with 0
    mutate(across(where(is.numeric), ~ replace_na(.x, 0)))

  return(on_court_stats)
}

on_court_stats <- calculate_on_court_stats()
cat("On-court statistics calculated for", nrow(on_court_stats), "players\n")

# =============================================================================
# COMBINE INDIVIDUAL AND ON-COURT STATS
# =============================================================================

cat("\n=== COMBINING INDIVIDUAL AND ON-COURT STATISTICS ===\n")

# Combine individual and on-court stats
player_complete_stats <- player_individual_stats %>%
  left_join(on_court_stats, by = c("athlete_id" = "player_id")) %>%
  # Replace NAs with 0 for players not found in on-court data
  mutate(across(starts_with("team_") | starts_with("opp_"), ~ replace_na(.x, 0))) %>%
  # Calculate all four factors
  mutate(
    # Individual Four Factors (already calculated above)
    # Add proper individual rebound rates using team context
    # Only calculate rates if sufficient team context (at least 20 total rebounds in each category)
    ind_oreb_rate = ifelse((team_oreb + opp_dreb) >= 20, ind_oreb / (team_oreb + opp_dreb), NA),
    ind_dreb_rate = ifelse((team_dreb + opp_oreb) >= 20, ind_dreb / (team_dreb + opp_oreb), NA),

    # Team Four Factors (while player on court)
    team_efg_pct = ifelse(team_fga > 0, (team_fgm + 0.5 * team_3pm) / team_fga, 0),
    team_ft_rate = ifelse(team_fga > 0, team_fta / team_fga, 0),
    team_to_rate = ifelse((team_fga + 0.5 * team_fta + team_turnovers) > 0,
                         team_turnovers / (team_fga + 0.5 * team_fta + team_turnovers), 0),
    team_oreb_rate = ifelse((team_oreb + opp_dreb) > 0, team_oreb / (team_oreb + opp_dreb), 0),

    # Opponent Four Factors (against - while player on court)
    opp_efg_pct = ifelse(opp_fga > 0, (opp_fgm + 0.5 * opp_3pm) / opp_fga, 0),
    opp_ft_rate = ifelse(opp_fga > 0, opp_fta / opp_fga, 0),
    opp_to_rate = ifelse((opp_fga + 0.5 * opp_fta + opp_turnovers) > 0,
                        opp_turnovers / (opp_fga + 0.5 * opp_fta + opp_turnovers), 0),
    opp_oreb_rate = ifelse((opp_oreb + team_dreb) > 0, opp_oreb / (opp_oreb + team_dreb), 0),

    # Calculate possessions for team performance while player on court (Dean Oliver formula)
    team_possessions = team_fga - team_oreb + team_turnovers + 0.4 * team_fta,
    opp_possessions = opp_fga - opp_oreb + opp_turnovers + 0.4 * opp_fta,
    avg_possessions = (team_possessions + opp_possessions) / 2,

    # Offensive, Defensive, and Net Ratings for team performance while player on court (points per 100 possessions)
    team_off_rating = ifelse(team_possessions > 0, 100 * (team_fgm * 2 + team_3pm + team_ftm) / team_possessions, 0),
    team_def_rating = ifelse(opp_possessions > 0, 100 * (opp_fgm * 2 + opp_3pm + opp_ftm) / opp_possessions, 0),
    team_net_rating = team_off_rating - team_def_rating
  ) %>%
  # Reorder columns to put four factors stats right after team names
  select(
    athlete_id, athlete_display_name, team_id, team_display_name,
    # Individual Four Factors
    ind_efg_pct, ind_ft_rate, ind_to_rate, ind_oreb_rate, ind_dreb_rate,
    # Team Four Factors (while on court)
    team_efg_pct, team_ft_rate, team_to_rate, team_oreb_rate,
    # Opponent Four Factors (while on court)
    opp_efg_pct, opp_ft_rate, opp_to_rate, opp_oreb_rate,
    # Ratings
    team_off_rating, team_def_rating, team_net_rating,
    # Everything else
    everything()
  )

cat("Complete player statistics calculated for", nrow(player_complete_stats), "players\n")

# =============================================================================
# TEAM FOUR FACTORS (from team box scores)
# =============================================================================

cat("\n=== CALCULATING TEAM FOUR FACTORS ===\n")

# Calculate team four factors by joining games to get opponent stats
calculate_team_four_factors <- function() {

  # Get offensive stats (team's own performance)
  team_offensive <- team_box %>%
    group_by(team_id, team_short_display_name, team_display_name) %>%
    summarise(
      games_played = n(),
      off_fgm = sum(field_goals_made, na.rm = TRUE),
      off_fga = sum(field_goals_attempted, na.rm = TRUE),
      off_3pm = sum(three_point_field_goals_made, na.rm = TRUE),
      off_3pa = sum(three_point_field_goals_attempted, na.rm = TRUE),
      off_ftm = sum(free_throws_made, na.rm = TRUE),
      off_fta = sum(free_throws_attempted, na.rm = TRUE),
      off_oreb = sum(offensive_rebounds, na.rm = TRUE),
      off_dreb = sum(defensive_rebounds, na.rm = TRUE),
      off_turnovers = sum(total_turnovers, na.rm = TRUE),
      .groups = 'drop'
    )

  # Get defensive stats by flipping the perspective - what this team allowed
  # Join team_box with itself to get opponent performance
  team_defensive <- team_box %>%
    # Join to get the opponent's stats for the same games
    inner_join(
      team_box %>% select(game_id, opp_team_id = team_id,
                         opp_fgm = field_goals_made,
                         opp_fga = field_goals_attempted,
                         opp_3pm = three_point_field_goals_made,
                         opp_3pa = three_point_field_goals_attempted,
                         opp_ftm = free_throws_made,
                         opp_fta = free_throws_attempted,
                         opp_oreb = offensive_rebounds,
                         opp_dreb = defensive_rebounds,
                         opp_turnovers = total_turnovers),
      by = c("game_id", "opponent_team_id" = "opp_team_id")
    ) %>%
    group_by(team_id, team_short_display_name, team_display_name) %>%
    summarise(
      def_fgm = sum(opp_fgm, na.rm = TRUE),
      def_fga = sum(opp_fga, na.rm = TRUE),
      def_3pm = sum(opp_3pm, na.rm = TRUE),
      def_3pa = sum(opp_3pa, na.rm = TRUE),
      def_ftm = sum(opp_ftm, na.rm = TRUE),
      def_fta = sum(opp_fta, na.rm = TRUE),
      def_oreb = sum(opp_oreb, na.rm = TRUE),
      def_dreb = sum(opp_dreb, na.rm = TRUE),
      def_turnovers = sum(opp_turnovers, na.rm = TRUE),
      .groups = 'drop'
    )

  # Combine offensive and defensive stats
  team_four_factors <- team_offensive %>%
    left_join(team_defensive, by = c("team_id", "team_short_display_name", "team_display_name")) %>%
    # Replace NAs with 0 (for teams that don't have opponent data)
    mutate(across(starts_with("def_"), ~ replace_na(.x, 0))) %>%
    # Calculate four factors
    mutate(
      # Offensive Four Factors
      off_efg_pct = ifelse(off_fga > 0, (off_fgm + 0.5 * off_3pm) / off_fga, 0),
      off_ft_rate = ifelse(off_fga > 0, off_fta / off_fga, 0),
      off_to_rate = ifelse((off_fga + 0.5 * off_fta + off_turnovers) > 0,
                           off_turnovers / (off_fga + 0.5 * off_fta + off_turnovers), 0),
      off_oreb_rate = ifelse((off_oreb + def_dreb) > 0, off_oreb / (off_oreb + def_dreb), 0),

      # Defensive Four Factors (opponent performance)
      def_efg_pct = ifelse(def_fga > 0, (def_fgm + 0.5 * def_3pm) / def_fga, 0),
      def_ft_rate = ifelse(def_fga > 0, def_fta / def_fga, 0),
      def_to_rate = ifelse((def_fga + 0.5 * def_fta + def_turnovers) > 0,
                           def_turnovers / (def_fga + 0.5 * def_fta + def_turnovers), 0),
      def_oreb_rate = ifelse((def_oreb + off_dreb) > 0, def_oreb / (def_oreb + off_dreb), 0),

      # Calculate possessions (Dean Oliver formula)
      off_possessions = off_fga - off_oreb + off_turnovers + 0.4 * off_fta,
      def_possessions = def_fga - def_oreb + def_turnovers + 0.4 * def_fta,
      avg_possessions = (off_possessions + def_possessions) / 2,

      # Offensive, Defensive, and Net Ratings (points per 100 possessions)
      off_rating = ifelse(off_possessions > 0, 100 * (off_fgm * 2 + off_3pm + off_ftm) / off_possessions, 0),
      def_rating = ifelse(def_possessions > 0, 100 * (def_fgm * 2 + def_3pm + def_ftm) / def_possessions, 0),
      net_rating = off_rating - def_rating,

      # Net four factors
      net_four_factors = (off_efg_pct + off_ft_rate - off_to_rate + off_oreb_rate) -
                        (def_efg_pct + def_ft_rate - def_to_rate + def_oreb_rate)
    ) %>%
    # Reorder columns to put four factors stats right after team names
    select(
      team_id, team_short_display_name, team_display_name, games_played,
      # Offensive Four Factors
      off_efg_pct, off_ft_rate, off_to_rate, off_oreb_rate,
      # Defensive Four Factors
      def_efg_pct, def_ft_rate, def_to_rate, def_oreb_rate,
      # Ratings
      off_rating, def_rating, net_rating, net_four_factors,
      # Everything else
      everything()
    ) %>%
    arrange(desc(net_rating))

  return(team_four_factors)
}

team_four_factors <- calculate_team_four_factors()
cat("Team four factors calculated for", nrow(team_four_factors), "teams\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== SAVING RESULTS ===\n")

# Save comprehensive player four factors
write_csv(player_complete_stats %>%
            select(Player = athlete_display_name,
                   Team = team_display_name,
                   Games = games_played,
                   Minutes = total_minutes,

                   # Individual stats
                   `Ind FGM` = ind_fgm,
                   `Ind FGA` = ind_fga,
                   `Ind 3PM` = ind_3pm,
                   `Ind 3PA` = ind_3pa,
                   `Ind FTM` = ind_ftm,
                   `Ind FTA` = ind_fta,
                   `Ind OREB` = ind_oreb,
                   `Ind DREB` = ind_dreb,
                   `Ind TOV` = ind_turnovers,

                   # Individual four factors
                   `Ind eFG%` = ind_efg_pct,
                   `Ind FT Rate` = ind_ft_rate,
                   `Ind TO Rate` = ind_to_rate,
                   `Ind OREB Rate` = ind_oreb_rate,
                   `Ind DREB Rate` = ind_dreb_rate,

                   # Team performance while player on court
                   `Team eFG%` = team_efg_pct,
                   `Team FT Rate` = team_ft_rate,
                   `Team TO Rate` = team_to_rate,
                   `Team OREB Rate` = team_oreb_rate,

                   # Opponent performance while player on court (against stats)
                   `Opp eFG%` = opp_efg_pct,
                   `Opp FT Rate` = opp_ft_rate,
                   `Opp TO Rate` = opp_to_rate,
                   `Opp OREB Rate` = opp_oreb_rate) %>%
            mutate(across(contains("%") | contains("Rate"), ~ round(.x, 4))),
          "womens_basketball_player_four_factors_complete.csv")

# Save team four factors
write_csv(team_four_factors %>%
            select(Team = team_display_name,
                   Games = games_played,
                   `Off eFG%` = off_efg_pct,
                   `Off FT Rate` = off_ft_rate,
                   `Off TO Rate` = off_to_rate,
                   `Off OREB Rate` = off_oreb_rate,
                   `Def eFG%` = def_efg_pct,
                   `Def FT Rate` = def_ft_rate,
                   `Def TO Rate` = def_to_rate,
                   `Def OREB Rate` = def_oreb_rate,
                   `Net Four Factors` = net_four_factors) %>%
            mutate(across(where(is.numeric) & !contains("Games"), ~ round(.x, 4))),
          "womens_basketball_team_four_factors.csv")

cat("\n=== SUMMARY ===\n")
cat("Complete player four factors saved to: womens_basketball_player_four_factors_complete.csv\n")
cat("Team four factors saved to: womens_basketball_team_four_factors.csv\n")
cat("\nFour Factors Analysis Complete!\n")
cat("- Player analysis:", nrow(player_complete_stats), "players\n")
cat("- Team analysis:", nrow(team_four_factors), "teams\n")
cat("\nEach player now has:\n")
cat("- Individual four factors (their personal performance)\n")
cat("- Team four factors (team performance while they're on court)\n")
cat("- Opponent four factors (what opponents do while they're on court)\n")