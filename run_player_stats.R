# =============================================================================
# COMPREHENSIVE WOMEN'S BASKETBALL PLAYER STATISTICS
# Combines all analyses into a single dataset: full_player_info_womens
# =============================================================================

library(tidyverse)
library(data.table)

cat("=== COMPREHENSIVE WOMEN'S BASKETBALL PLAYER STATISTICS ===\n")
cat("Loading and combining all player analyses...\n\n")

# =============================================================================
# LOAD ALL REQUIRED ANALYSES
# =============================================================================

# 1. Load RAPM analysis (includes base data loading)
cat("1. Loading RAPM analysis (includes base data)...\n")
source("RAPM_calculation.R")

# 2. Load Four Factors and basic stats
cat("2. Loading Four Factors analysis...\n")
if(!exists("player_complete_stats")) {
  source("four_factors.R")
}

# 3. Load shooting analysis
cat("3. Loading shooting analysis...\n")
if(!exists("comprehensive_shooting_profile")) {
  source("shooting_analysis.R")
}

# 4. Load defensive stats
cat("4. Loading defensive statistics...\n")
if(!exists("defensive_stats")) {
  source("defensive_stats.R")
}

# 5. Load plus/minus stats
cat("5. Loading plus/minus analysis...\n")
if(!exists("plus_minus_stats")) {
  source("plus_minus.R")
}

# 6. Load offensive archetypes
cat("6. Loading offensive archetypes...\n")
if(!exists("offensive_archetypes")) {
  source("offensive_archetypes.R")
}

# 7. Load defensive archetypes
cat("7. Loading defensive archetypes...\n")
if(!exists("defensive_archetypes")) {
  source("defensive_archetypes.R")
}

cat("\nAll analyses loaded successfully!\n\n")

# =============================================================================
# PREPARE CORE PLAYER INFORMATION
# =============================================================================

cat("=== PREPARING CORE PLAYER INFORMATION ===\n")

# RAPM data is now available from RAPM_calculation.R
cat("RAPM analysis completed successfully!\n")
cat("Converting RAPM results to usable format...\n")

# Convert rapm_results to the format needed for joining
rapm_player_stats <- rapm_results %>%
  # Convert player names back to IDs for joining
  left_join(
    player_box %>% select(athlete_id, athlete_display_name) %>% distinct(),
    by = c("Player" = "athlete_display_name")
  ) %>%
  select(
    athlete_id,
    rapm = RAPM,
    o_rapm = `O-RAPM`,
    d_rapm = `D-RAPM`
  ) %>%
  filter(!is.na(athlete_id))

cat("RAPM stats prepared for", nrow(rapm_player_stats), "players\n")

# =============================================================================
# PREPARE ARCHETYPE INFORMATION
# =============================================================================

cat("\n=== PREPARING ARCHETYPE INFORMATION ===\n")

# Offensive archetypes - extract key columns from player_archetype_table
offensive_archetype_info <- player_archetype_table %>%
  select(
    athlete_id,  # Match naming convention
    offensive_archetype = primary_archetype,
    contains("_fit")  # All fit percentages
  ) %>%
  # Rename fit columns to be more descriptive
  rename_with(~ paste0("off_", .), contains("_fit"))

cat("Offensive archetype info prepared for", nrow(offensive_archetype_info), "players\n")

# Defensive archetypes - extract key columns
defensive_archetype_info <- defensive_archetypes %>%
  select(
    athlete_id,
    defensive_archetype = cluster,  # Will need to map to names
    contains("archetype_") & contains("_fit")  # All fit percentages
  ) %>%
  # Convert cluster numbers to descriptive names based on analysis
  mutate(
    defensive_archetype = case_when(
      defensive_archetype == 1 ~ "Rim Protector",
      defensive_archetype == 2 ~ "Low Activity Guard",
      defensive_archetype == 3 ~ "Perimeter Specialist",
      defensive_archetype == 4 ~ "Steal Artist",
      defensive_archetype == 5 ~ "Low Activity Defender",
      defensive_archetype == 6 ~ "Interior Rebounder",
      TRUE ~ paste0("Cluster_", defensive_archetype)
    )
  ) %>%
  # Rename fit columns to be more descriptive
  rename_with(~ paste0("def_", .), contains("archetype_") & contains("_fit"))

cat("Defensive archetype info prepared for", nrow(defensive_archetype_info), "players\n")

# =============================================================================
# PREPARE FOUR FACTORS AND TEAM PERFORMANCE
# =============================================================================

cat("\n=== PREPARING FOUR FACTORS AND TEAM PERFORMANCE ===\n")

# Individual four factors from player_complete_stats
individual_four_factors <- player_complete_stats %>%
  select(
    athlete_id, athlete_display_name, team_id, team_display_name,
    # Individual Four Factors
    ind_efg_pct, ind_ft_rate, ind_to_rate, ind_oreb_rate, ind_dreb_rate,
    # Team performance while player on court (Four Factors)
    team_efg_pct, team_ft_rate, team_to_rate, team_oreb_rate,
    # Opponent performance while player on court
    opp_efg_pct, opp_ft_rate, opp_to_rate, opp_oreb_rate,
    # Team ratings while player on court
    team_off_rating, team_def_rating, team_net_rating,
    # Basic counting stats that might not be elsewhere
    total_minutes, games_played
  )

cat("Four factors prepared for", nrow(individual_four_factors), "players\n")

# =============================================================================
# PREPARE SHOOTING STATISTICS
# =============================================================================

cat("\n=== PREPARING SHOOTING STATISTICS ===\n")

# Extract key shooting metrics from comprehensive_shooting_profile
shooting_stats <- comprehensive_shooting_profile %>%
  select(
    athlete_id = athlete_id_1,  # Use the correct column name from shooting analysis
    # Overall shooting stats
    overall_fga, overall_fgm, overall_fg_pct,
    overall_3pa, overall_3pm, overall_3pt_pct,
    overall_fta, overall_ftm, overall_ft_pct,
    overall_pct_assisted,

    # Shot zone frequency and performance - get ALL the shooting location data
    contains("freq_pct"),  # rim_freq_pct, mid_range_freq_pct, three_point_freq_pct
    contains("_fga"), contains("_fgm"), contains("_fg_pct"),  # Zone-specific shooting
    contains("_assist_rate"),  # Zone-specific assist rates

    # Points generated metrics
    any_of(c("total_points_generated", "pct_team_points_generated"))
  )

cat("Shooting statistics prepared for", nrow(shooting_stats), "players\n")

# =============================================================================
# PREPARE DEFENSIVE STATISTICS
# =============================================================================

cat("\n=== PREPARING DEFENSIVE STATISTICS ===\n")

# Extract key defensive metrics using any_of for flexible column matching
defensive_player_stats <- defensive_stats %>%
  select(
    athlete_id,
    # Basic defensive stats
    any_of(c("steals_per_game", "spg")),
    any_of(c("blocks_per_game", "bpg")),
    any_of(c("def_reb_per_game", "drpg")),
    any_of(c("fouls_per_game", "fpg")),
    # Efficiency ratios
    any_of(c("steal_to_foul_ratio", "steal_foul_ratio")),
    any_of(c("activity_to_foul_ratio", "activity_foul_ratio")),
    # Spatial defensive data (if available)
    any_of(c("total_located_fouls", "located_fouls")),
    any_of(c("total_located_steals", "located_steals")),
    any_of(c("foul_rim_pct", "foul_at_rim_pct")),
    any_of(c("foul_paint_pct", "foul_in_paint_pct")),
    any_of(c("foul_mid_pct", "foul_mid_range_pct")),
    any_of(c("foul_perimeter_pct", "foul_three_pct")),
    any_of(c("steal_rim_pct", "steal_at_rim_pct")),
    any_of(c("steal_paint_pct", "steal_in_paint_pct")),
    any_of(c("steal_mid_pct", "steal_mid_range_pct")),
    any_of(c("steal_perimeter_pct", "steal_three_pct")),
    # Primary defensive zone classification
    any_of(c("primary_foul_zone", "main_foul_zone"))
  )

cat("Defensive statistics prepared for", nrow(defensive_player_stats), "players\n")

# =============================================================================
# PREPARE PLUS/MINUS DATA
# =============================================================================

cat("\n=== PREPARING PLUS/MINUS DATA ===\n")

# Extract plus/minus metrics using any_of for flexible column matching
plus_minus_player_stats <- plus_minus_stats %>%
  select(
    athlete_id,
    # Plus/minus metrics
    any_of(c("total_plus_minus", "Total +/-")),
    any_of(c("plus_minus_per_game", "+/- per Game")),
    any_of(c("plus_minus_per_minute", "+/- per Min")),
    # Pace and possession metrics (if available)
    any_of(c("possessions_involved", "pace_impact", "usage_rate"))
  )

cat("Plus/minus statistics prepared for", nrow(plus_minus_player_stats), "players\n")

# =============================================================================
# COMBINE ALL DATA INTO COMPREHENSIVE DATASET
# =============================================================================

cat("\n=== COMBINING ALL DATA ===\n")

# Start with the core player info from four_factors (most complete player list)
full_player_info_womens <- individual_four_factors %>%
  # Add RAPM data
  left_join(rapm_player_stats, by = "athlete_id") %>%
  # Add offensive archetypes
  left_join(offensive_archetype_info, by = "athlete_id") %>%
  # Add defensive archetypes
  left_join(defensive_archetype_info, by = "athlete_id") %>%
  # Add shooting statistics
  left_join(shooting_stats, by = "athlete_id") %>%
  # Add defensive statistics
  left_join(defensive_player_stats, by = "athlete_id") %>%
  # Add plus/minus statistics
  left_join(plus_minus_player_stats, by = "athlete_id") %>%
  # Add season identifier
  mutate(season = 2025) %>%
  # Add basic box score stats from player_box
  left_join(
    player_box %>%
      group_by(athlete_id) %>%
      summarise(
        games_played = n_distinct(game_id),
        total_minutes = sum(minutes, na.rm = TRUE),
        points = sum(points, na.rm = TRUE),
        field_goals_made = sum(field_goals_made, na.rm = TRUE),
        field_goals_attempted = sum(field_goals_attempted, na.rm = TRUE),
        three_point_field_goals_made = sum(three_point_field_goals_made, na.rm = TRUE),
        three_point_field_goals_attempted = sum(three_point_field_goals_attempted, na.rm = TRUE),
        free_throws_made = sum(free_throws_made, na.rm = TRUE),
        free_throws_attempted = sum(free_throws_attempted, na.rm = TRUE),
        rebounds = sum(rebounds, na.rm = TRUE),
        assists = sum(assists, na.rm = TRUE),
        steals = sum(steals, na.rm = TRUE),
        blocks = sum(blocks, na.rm = TRUE),
        turnovers = sum(turnovers, na.rm = TRUE),
        fouls = sum(fouls, na.rm = TRUE),
        .groups = 'drop'
      ),
    by = "athlete_id"
  ) %>%
  # Sort by plus/minus for now (will sort by RAPM when added later)
  arrange(desc(if("total_plus_minus" %in% names(.)) total_plus_minus else if("Total +/-" %in% names(.)) `Total +/-` else 0)) %>%
  # Reorder columns according to user specification
  select(
    # Core identifiers
    athlete_id, athlete_display_name, team_id, team_display_name, season,

    # RAPM metrics (will be NA for now, can be populated later)
    any_of(c("rapm", "o_rapm", "d_rapm")),

    # Archetypes (no fit percentages)
    offensive_archetype, defensive_archetype,

    # Plus/minus - use any_of for flexible column matching
    any_of(c("total_plus_minus", "Total +/-")),
    any_of(c("plus_minus_per_game", "+/- per Game")),
    any_of(c("plus_minus_per_minute", "+/- per Min")),

    # Individual Four Factors
    ind_efg_pct, ind_ft_rate, ind_to_rate, ind_oreb_rate, ind_dreb_rate,

    # Basic box score stats (use any_of for flexible column matching)
    any_of(c("games_played", "box_games_played")),
    any_of(c("total_minutes", "box_total_minutes")),
    any_of(c("points")),
    any_of(c("field_goals_made", "field_goals_attempted")),
    any_of(c("three_point_field_goals_made", "three_point_field_goals_attempted")),
    any_of(c("free_throws_made", "free_throws_attempted")),
    any_of(c("rebounds", "assists", "steals", "blocks", "turnovers", "fouls")),

    # Comprehensive shooting stats from shooting analysis
    any_of(c("overall_fga", "overall_fgm", "overall_fg_pct")),
    any_of(c("overall_3pa", "overall_3pm", "overall_3pt_pct")),
    any_of(c("overall_fta", "overall_ftm", "overall_ft_pct")),
    any_of(c("overall_pct_assisted")),

    # Shot zone frequency and performance data
    contains("freq_pct"),  # rim_freq_pct, mid_range_freq_pct, three_point_freq_pct
    contains("_fga"), contains("_fgm"), contains("_fg_pct"),  # Zone-specific shooting
    contains("_assist_rate"),  # Zone-specific assist rates

    # Points generated metrics
    any_of(c("total_points_generated", "pct_team_points_generated")),

    # Defensive stats (no per-36 or defensive_activity)
    any_of(c("steals_per_game", "spg")),
    any_of(c("blocks_per_game", "bpg")),
    any_of(c("def_reb_per_game", "drpg")),
    any_of(c("fouls_per_game", "fpg")),
    any_of(c("steal_to_foul_ratio", "steal_foul_ratio")),
    any_of(c("activity_to_foul_ratio", "activity_foul_ratio")),
    any_of(c("total_located_fouls", "located_fouls")),
    any_of(c("total_located_steals", "located_steals")),
    any_of(c("foul_rim_pct", "foul_at_rim_pct")),
    any_of(c("foul_paint_pct", "foul_in_paint_pct")),
    any_of(c("foul_mid_pct", "foul_mid_range_pct")),
    any_of(c("foul_perimeter_pct", "foul_three_pct")),
    any_of(c("steal_rim_pct", "steal_at_rim_pct")),
    any_of(c("steal_paint_pct", "steal_in_paint_pct")),
    any_of(c("steal_mid_pct", "steal_mid_range_pct")),
    any_of(c("steal_perimeter_pct", "steal_three_pct")),
    any_of(c("primary_foul_zone", "main_foul_zone")),

    # Team Four Factors (when player on court)
    team_efg_pct, team_ft_rate, team_to_rate, team_oreb_rate,
    opp_efg_pct, opp_ft_rate, opp_to_rate, opp_oreb_rate,
    team_off_rating, team_def_rating, team_net_rating,

    # Any remaining variables
    everything()
  )

# =============================================================================
# DATA QUALITY AND SUMMARY
# =============================================================================

cat("\n=== DATA QUALITY SUMMARY ===\n")
cat("Total players in comprehensive dataset:", nrow(full_player_info_womens), "\n")
cat("Total variables:", ncol(full_player_info_womens), "\n")

# Check data completeness
completeness_summary <- full_player_info_womens %>%
  summarise(
    players_with_rapm = sum(!is.na(rapm)),
    players_with_offensive_archetype = sum(!is.na(offensive_archetype)),
    players_with_defensive_archetype = sum(!is.na(defensive_archetype)),
    players_with_shooting_stats = ifelse("overall_fga" %in% names(.), sum(!is.na(overall_fga)),
                                        ifelse("total_attempts" %in% names(.), sum(!is.na(total_attempts)),
                                        ifelse("attempts" %in% names(.), sum(!is.na(attempts)), 0))),
    players_with_defensive_stats = ifelse("steals_per_game" %in% names(.), sum(!is.na(steals_per_game)),
                                         ifelse("spg" %in% names(.), sum(!is.na(spg)), 0)),
    players_with_plus_minus = ifelse("total_plus_minus" %in% names(.), sum(!is.na(total_plus_minus)),
                                    ifelse("Total +/-" %in% names(.), sum(!is.na(`Total +/-`)), 0))
  )

cat("\n=== DATA COMPLETENESS ===\n")
print(completeness_summary)

# Show sample of the comprehensive dataset
cat("\n=== SAMPLE DATA ===\n")
sample_players <- full_player_info_womens %>%
  filter(!is.na(rapm) & !is.na(offensive_archetype) & !is.na(defensive_archetype)) %>%
  arrange(desc(if("points_per_game" %in% names(.)) points_per_game else if("ppg" %in% names(.)) ppg else 0)) %>%
  head(10) %>%
  select(athlete_display_name, team_display_name, rapm, offensive_archetype, defensive_archetype,
         any_of(c("points_per_game", "ppg")), any_of(c("steals_per_game", "spg")), any_of(c("blocks_per_game", "bpg")))

print(sample_players)
# =============================================================================
# EXPORT COMPREHENSIVE DATASET
# =============================================================================

full_player_info_womens_clean <- full_player_info_womens|>
  select(-field_goals_made, 
         -field_goals_attempted,
         -three_point_field_goals_made,
         -three_point_field_goals_attempted, 
         -three_point_freq_pct,
         -free_throws_made,
         -free_throws_attempted,
         -rim_freq_pct,
         -mid_range_freq_pct,
         -Unknown_fg_pct,
         -`Corner 3_fg_pct`,
         -`Above Break 3_fg_pct`,
         -`Long 2_fg_pct`,
         -total_minutes.y,
         -games_played.y,
         -def_archetype_1_fit,
         -def_archetype_2_fit,
         -def_archetype_3_fit,
         -def_archetype_4_fit,
         -def_archetype_5_fit,
         -def_archetype_6_fit)|>
  rename(total_minutes = total_minutes.x)|>
  rename(games_played = games_played.x)

cat("\n=== EXPORTING COMPREHENSIVE DATASET ===\n")

# Save as CSV for external use
write.csv(full_player_info_womens_clean, "full_player_info_womens.csv", row.names = FALSE)

# Save as RDS for R use
saveRDS(full_player_info_womens_clean, "full_player_info_womens.rds")

cat("Comprehensive dataset exported to:\n")
cat("- full_player_info_womens.csv\n")
cat("- full_player_info_womens.rds\n")

cat("\n=== COMPREHENSIVE ANALYSIS COMPLETE ===\n")
cat("Dataset 'full_player_info_womens' ready for analysis!\n")
cat("Contains all player statistics, archetypes, and performance metrics\n")