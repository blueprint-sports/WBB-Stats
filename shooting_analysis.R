# =============================================================================
# SHOOTING ANALYSIS - Women's Basketball Advanced Shot Zone Classification
# =============================================================================

# Load required libraries
library(tidyverse)
library(data.table)
library(wehoop)

cat("Starting Women's Basketball Shooting Analysis...\n")

# Load the data (assumes wbb_player_stats.R has been run)
if(!exists("wbb_pbp")) {
  source("wbb_player_stats.R")
}

# =============================================================================
# SHOT ZONE CLASSIFICATION FUNCTION
# =============================================================================

classify_shot_zones <- function(data) {
  cat("Classifying shot zones using coordinate data...\n")

  data %>%
    # Add enhanced shot variables (same as four_factors.R)
    mutate(
      is_three_point_shot = grepl("Three Point", text, ignore.case = TRUE),
      is_shot_made = grepl("made", text, ignore.case = TRUE),
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
    ) %>%
    # Calculate distances from appropriate basket (baskets at ±41.75, 0)
    mutate(
      distance_from_basket = case_when(
        coordinate_x < 0 ~ sqrt((coordinate_x - (-41.75))^2 + coordinate_y^2),
        coordinate_x > 0 ~ sqrt((coordinate_x - 41.75)^2 + coordinate_y^2),
        TRUE ~ NA_real_
      ),
      x_distance_from_basket = case_when(
        coordinate_x < 0 ~ abs(coordinate_x - (-41.75)),
        coordinate_x > 0 ~ abs(coordinate_x - 41.75),
        TRUE ~ NA_real_
      )
    ) %>%
    # Shot zone classification
    mutate(
      shot_zone = case_when(
        is_free_throw ~ "Free Throw",

        # Three-point classifications (validated with championship game)
        is_3pa & x_distance_from_basket <= 5 ~ "Corner 3",
        is_3pa ~ "Above Break 3",

        # Two-point zones (using ~10 inches per coordinate unit)
        distance_from_basket <= 4 ~ "Rim",          # ~3.3 feet
        distance_from_basket <= 10 ~ "Paint",       # ~8.3 feet
        distance_from_basket <= 20 ~ "Mid Range",   # ~16.7 feet

        !is.na(distance_from_basket) ~ "Long 2",
        TRUE ~ "Unknown"
      ),

      # Additional shot context
      shot_side = case_when(
        coordinate_y > 5 ~ "Right",
        coordinate_y < -5 ~ "Left",
        TRUE ~ "Center"
      )
    )
}

# =============================================================================
# APPLY SHOT ZONE CLASSIFICATION
# =============================================================================

cat("Applying shot zone classification to play-by-play data...\n")
wbb_pbp_enhanced <- classify_shot_zones(wbb_pbp)

cat("Shot zone classification complete!\n")

# =============================================================================
# SHOOTING ANALYSIS BY ZONE
# =============================================================================

calculate_shooting_stats_by_zone <- function() {
  cat("Calculating shooting statistics by zone...\n")

  # Player shooting by zone
  player_shooting_by_zone <- wbb_pbp_enhanced %>%
    filter(is_fga | is_fta, !is.na(athlete_id_1)) %>%
    mutate(
      # Check if shot was assisted (has athlete_id_2 and shot was made)
      is_assisted = is_shot_made & !is.na(athlete_id_2)
    ) %>%
    group_by(athlete_id_1, shot_zone) %>%
    summarise(
      attempts = n(),
      makes = sum(is_shot_made, na.rm = TRUE),
      assisted_makes = sum(is_assisted, na.rm = TRUE),
      fg_pct = ifelse(attempts > 0, makes / attempts, 0),
      assist_rate = ifelse(makes > 0, assisted_makes / makes, 0),
      .groups = 'drop'
    ) %>%
    # Add player info
    left_join(
      player_box %>%
        select(athlete_id, athlete_display_name, team_display_name) %>%
        distinct(),
      by = c("athlete_id_1" = "athlete_id")
    ) %>%
    filter(!is.na(athlete_display_name))

  # Team shooting by zone
  team_shooting_by_zone <- wbb_pbp_enhanced %>%
    filter(is_fga | is_fta) %>%
    mutate(
      # Check if shot was assisted (has athlete_id_2 and shot was made)
      is_assisted = is_shot_made & !is.na(athlete_id_2)
    ) %>%
    group_by(team_id, shot_zone) %>%
    summarise(
      attempts = n(),
      makes = sum(is_shot_made, na.rm = TRUE),
      assisted_makes = sum(is_assisted, na.rm = TRUE),
      fg_pct = ifelse(attempts > 0, makes / attempts, 0),
      assist_rate = ifelse(makes > 0, assisted_makes / makes, 0),
      .groups = 'drop'
    ) %>%
    # Add team info
    left_join(
      team_box %>%
        select(team_id, team_display_name) %>%
        distinct(),
      by = "team_id"
    ) %>%
    filter(!is.na(team_display_name))

  return(list(
    player_by_zone = player_shooting_by_zone,
    team_by_zone = team_shooting_by_zone
  ))
}

shooting_by_zone <- calculate_shooting_stats_by_zone()

# =============================================================================
# POINTS GENERATED ANALYSIS (SCORING + PLAYMAKING)
# =============================================================================

calculate_points_generated <- function() {
  cat("Calculating points generated (scoring + assists)...\n")

  # Points scored by each player
  points_scored <- wbb_pbp_enhanced %>%
    filter(is_shot_made, !is.na(athlete_id_1)) %>%
    mutate(
      points_on_shot = case_when(
        is_3pm ~ 3,
        is_2pm ~ 2,
        is_ftm ~ 1,
        TRUE ~ 0
      )
    ) %>%
    group_by(athlete_id_1) %>%
    summarise(
      points_scored = sum(points_on_shot, na.rm = TRUE),
      .groups = 'drop'
    )

  # Assists given by each player (points generated for teammates)
  assists_given <- wbb_pbp_enhanced %>%
    filter(is_shot_made, !is.na(athlete_id_2)) %>%  # assisted shots only
    mutate(
      points_assisted = case_when(
        is_3pm ~ 3,
        is_2pm ~ 2,
        TRUE ~ 0  # no assists on free throws
      )
    ) %>%
    group_by(athlete_id_2) %>%  # group by assisting player
    summarise(
      total_assists = n(),
      assists_2pt = sum(is_2pm, na.rm = TRUE),
      assists_3pt = sum(is_3pm, na.rm = TRUE),
      points_from_assists = sum(points_assisted, na.rm = TRUE),
      .groups = 'drop'
    )

  # Combine scoring and playmaking
  points_generated <- points_scored %>%
    full_join(assists_given, by = c("athlete_id_1" = "athlete_id_2")) %>%
    mutate(
      across(c(points_scored, total_assists, assists_2pt, assists_3pt, points_from_assists),
             ~ replace_na(.x, 0)),
      total_points_generated = points_scored + points_from_assists
    ) %>%
    # Add player info
    left_join(
      player_box %>%
        select(athlete_id, athlete_display_name, team_display_name) %>%
        distinct(),
      by = c("athlete_id_1" = "athlete_id")
    ) %>%
    filter(!is.na(athlete_display_name)) %>%
    arrange(desc(total_points_generated))

  return(points_generated)
}

points_generated_stats <- calculate_points_generated()

# =============================================================================
# POINTS GENERATED PERCENTAGE (vs Team Points While On Court)
# =============================================================================

calculate_points_generated_percentage <- function() {
  cat("Calculating points generated percentage using plus_minus data...\n")

  # Load plus_minus data if it doesn't exist
  if(!exists("plus_minus_stats")) {
    if(file.exists("plus_minus.R")) {
      source("plus_minus.R")
    } else {
      cat("Warning: plus_minus.R not found. Cannot calculate percentage of team points.\n")
      return(points_generated_stats)
    }
  }

  # Join points generated with team points while on court
  points_with_percentage <- points_generated_stats %>%
    left_join(
      plus_minus_stats %>%
        select(athlete_id, team_points_for), # team points while player on court
      by = c("athlete_id_1" = "athlete_id")
    ) %>%
    mutate(
      # Calculate percentage of team points generated
      pct_team_points_generated = ifelse(
        !is.na(team_points_for) & team_points_for > 0,
        round(total_points_generated / team_points_for * 100, 1),
        NA_real_
      )
    ) %>%
    # Filter for players with meaningful data
    filter(!is.na(pct_team_points_generated), team_points_for >= 50) %>%
    arrange(desc(pct_team_points_generated))

  return(points_with_percentage)
}

points_with_percentage <- calculate_points_generated_percentage()

# =============================================================================
# COMPREHENSIVE SHOOTING PROFILE
# =============================================================================

create_comprehensive_shooting_profile <- function() {
  cat("Creating comprehensive shooting profile...\n")

  # Overall shooting stats by player
  overall_shooting <- wbb_pbp_enhanced %>%
    filter(!is.na(athlete_id_1)) %>%
    mutate(
      is_assisted = is_shot_made & !is.na(athlete_id_2)
    ) %>%
    group_by(athlete_id_1) %>%
    summarise(
      # Overall shooting
      overall_fga = sum(is_fga, na.rm = TRUE),
      overall_fgm = sum(is_fgm, na.rm = TRUE),
      overall_fg_pct = ifelse(overall_fga > 0, round(overall_fgm / overall_fga, 3), 0),

      # Three-point shooting
      overall_3pa = sum(is_3pa, na.rm = TRUE),
      overall_3pm = sum(is_3pm, na.rm = TRUE),
      overall_3pt_pct = ifelse(overall_3pa > 0, round(overall_3pm / overall_3pa, 3), 0),

      # Free throw shooting
      overall_fta = sum(is_fta, na.rm = TRUE),
      overall_ftm = sum(is_ftm, na.rm = TRUE),
      overall_ft_pct = ifelse(overall_fta > 0, round(overall_ftm / overall_fta, 3), 0),

      # Assist rate on makes
      assisted_makes = sum(is_assisted, na.rm = TRUE),
      overall_pct_assisted = ifelse(overall_fgm > 0, round(assisted_makes / overall_fgm, 3), 0),

      .groups = 'drop'
    )

  # Shooting by zone - frequency (attempts)
  zone_frequency <- wbb_pbp_enhanced %>%
    filter(is_fga, !is.na(athlete_id_1), !is.na(shot_zone)) %>%
    group_by(athlete_id_1) %>%
    mutate(total_fga = n()) %>%
    group_by(athlete_id_1, shot_zone, total_fga) %>%
    summarise(zone_attempts = n(), .groups = 'drop') %>%
    mutate(
      zone_freq_pct = round(zone_attempts / total_fga, 3),
      freq_metric = paste0(shot_zone, "_freq_pct")
    ) %>%
    select(athlete_id_1, freq_metric, zone_freq_pct) %>%
    pivot_wider(names_from = freq_metric, values_from = zone_freq_pct, values_fill = 0)

  # Shooting by zone - efficiency (FG%)
  zone_efficiency <- wbb_pbp_enhanced %>%
    filter(is_fga, !is.na(athlete_id_1), !is.na(shot_zone)) %>%
    group_by(athlete_id_1, shot_zone) %>%
    summarise(
      zone_attempts = n(),
      zone_makes = sum(is_shot_made, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(zone_attempts >= 3) %>%  # minimum 3 attempts for meaningful %
    mutate(
      zone_fg_pct = round(zone_makes / zone_attempts, 3),
      eff_metric = paste0(shot_zone, "_fg_pct")
    ) %>%
    select(athlete_id_1, eff_metric, zone_fg_pct) %>%
    pivot_wider(names_from = eff_metric, values_from = zone_fg_pct, values_fill = NA_real_)

  # Create broader shooting categories
  broad_categories <- zone_frequency %>%
    # Remove free throw frequency (not field goal attempts)
    select(-contains("Free Throw_freq_pct")) %>%
    mutate(
      # Broad categories (funnel approach)
      rim_freq_pct = ifelse("Rim_freq_pct" %in% names(.), `Rim_freq_pct`, 0),
      mid_range_freq_pct = ifelse("Mid Range_freq_pct" %in% names(.), `Mid Range_freq_pct`, 0) +
                           ifelse("Paint_freq_pct" %in% names(.), `Paint_freq_pct`, 0) +
                           ifelse("Long 2_freq_pct" %in% names(.), `Long 2_freq_pct`, 0),
      three_point_freq_pct = ifelse("Corner 3_freq_pct" %in% names(.), `Corner 3_freq_pct`, 0) +
                             ifelse("Above Break 3_freq_pct" %in% names(.), `Above Break 3_freq_pct`, 0)
    )

  # Combine all metrics
  comprehensive_profile <- overall_shooting %>%
    # Add player info
    left_join(
      player_box %>%
        select(athlete_id, athlete_display_name, team_id, team_display_name) %>%
        distinct(),
      by = c("athlete_id_1" = "athlete_id")
    ) %>%
    # Add points generated data
    left_join(
      if(exists("points_with_percentage") && nrow(points_with_percentage) > 0) {
        points_with_percentage %>%
          select(athlete_id_1, total_points_generated, team_points_for, pct_team_points_generated)
      } else {
        points_generated_stats %>%
          select(athlete_id_1, total_points_generated) %>%
          mutate(team_points_for = NA_real_, pct_team_points_generated = NA_real_)
      },
      by = "athlete_id_1"
    ) %>%
    # Add broad categories
    left_join(broad_categories, by = "athlete_id_1") %>%
    # Add zone frequency
    left_join(zone_frequency %>% select(-contains("Free Throw_freq_pct")), by = "athlete_id_1") %>%
    # Add zone efficiency
    left_join(zone_efficiency, by = "athlete_id_1") %>%
    # Filter and arrange
    filter(!is.na(athlete_display_name), overall_fga >= 10) %>%  # minimum 10 FGA
    select(
      # Player identifiers
      athlete_id_1, athlete_display_name, team_id, team_display_name,

      # Overall shooting metrics
      overall_fga, overall_fgm, overall_fg_pct,
      overall_3pa, overall_3pm, overall_3pt_pct,
      overall_fta, overall_ftm, overall_ft_pct,
      overall_pct_assisted,

      # Points generated
      total_points_generated, pct_team_points_generated,

      # Broad shooting categories (FUNNEL LEVEL 1)
      rim_freq_pct, mid_range_freq_pct, three_point_freq_pct,

      # Detailed zone frequency (FUNNEL LEVEL 2)
      ends_with("_freq_pct"),

      # Zone efficiency (FUNNEL LEVEL 3)
      ends_with("_fg_pct")
    ) %>%
    arrange(desc(total_points_generated))

  return(comprehensive_profile)
}

comprehensive_shooting_profile <- create_comprehensive_shooting_profile()

cat("\n=== COMPREHENSIVE SHOOTING PROFILE CREATED ===\n")
cat("Profiles created for", nrow(comprehensive_shooting_profile), "players\n")
cat("Columns:", ncol(comprehensive_shooting_profile), "metrics per player\n")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

# Overall shot distribution
shot_zone_summary <- wbb_pbp_enhanced %>%
  filter(is_fga, !is.na(shot_zone)) %>%
  count(shot_zone, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

cat("\n=== SHOT ZONE DISTRIBUTION ===\n")
print(shot_zone_summary)

# League averages by zone
league_averages <- wbb_pbp_enhanced %>%
  filter(is_fga, !is.na(shot_zone)) %>%
  mutate(
    is_assisted = is_shot_made & !is.na(athlete_id_2)
  ) %>%
  group_by(shot_zone) %>%
  summarise(
    total_attempts = n(),
    total_makes = sum(is_shot_made, na.rm = TRUE),
    total_assisted = sum(is_assisted, na.rm = TRUE),
    league_fg_pct = round(total_makes / total_attempts, 3),
    league_assist_rate = round(ifelse(total_makes > 0, total_assisted / total_makes, 0), 3),
    .groups = 'drop'
  ) %>%
  arrange(desc(total_attempts))

cat("\n=== LEAGUE AVERAGES BY ZONE ===\n")
print(league_averages)

# Top performers by zone (minimum 20 attempts)
get_top_performers_by_zone <- function(zone_name, min_attempts = 20) {
  shooting_by_zone$player_by_zone %>%
    filter(shot_zone == zone_name, attempts >= min_attempts) %>%
    arrange(desc(fg_pct)) %>%
    head(10) %>%
    select(athlete_display_name, team_display_name, attempts, makes, fg_pct, assist_rate)
}

cat("\n=== TOP CORNER 3-POINT SHOOTERS ===\n")
print(get_top_performers_by_zone("Corner 3"))

cat("\n=== TOP ABOVE BREAK 3-POINT SHOOTERS ===\n")
print(get_top_performers_by_zone("Above Break 3", 50))

cat("\n=== TOP RIM FINISHERS ===\n")
print(get_top_performers_by_zone("Rim", 30))

# Points Generated Leaders
cat("\n=== TOP POINTS GENERATORS (SCORING + ASSISTS) ===\n")
if(exists("points_with_percentage") && nrow(points_with_percentage) > 0) {
  top_points_generators <- points_with_percentage %>%
    head(15) %>%
    select(athlete_display_name, team_display_name, points_scored, total_assists,
           points_from_assists, total_points_generated, team_points_for, pct_team_points_generated)
  print(top_points_generators)
} else {
  top_points_generators <- points_generated_stats %>%
    head(15) %>%
    select(athlete_display_name, team_display_name, points_scored, total_assists,
           assists_2pt, assists_3pt, points_from_assists, total_points_generated)
  print(top_points_generators)
}

cat("\n=== HIGHEST % OF TEAM POINTS GENERATED ===\n")
if(exists("points_with_percentage") && nrow(points_with_percentage) > 0) {
  highest_percentage <- points_with_percentage %>%
    filter(total_points_generated >= 100) %>%  # minimum threshold for meaningful sample
    head(15) %>%
    select(athlete_display_name, team_display_name, total_points_generated,
           team_points_for, pct_team_points_generated)
  print(highest_percentage)
}

cat("\n=== COMPREHENSIVE SHOOTING PROFILE - TOP 15 PLAYERS ===\n")
profile_display <- comprehensive_shooting_profile %>%
  head(15) %>%
  select(athlete_display_name, team_display_name, overall_fga, overall_fg_pct,
         overall_3pt_pct, overall_ft_pct, overall_pct_assisted,
         total_points_generated, pct_team_points_generated)
print(profile_display)

cat("\n=== FUNNEL LEVEL 1: BROAD SHOT SELECTION (Top 10 Players) ===\n")
broad_breakdown <- comprehensive_shooting_profile %>%
  head(10) %>%
  select(athlete_display_name, rim_freq_pct, mid_range_freq_pct, three_point_freq_pct)
print(broad_breakdown)

cat("\n=== FUNNEL LEVEL 2: DETAILED SHOT ZONES (Top 5 Players) ===\n")
detailed_zones <- comprehensive_shooting_profile %>%
  head(5) %>%
  select(athlete_display_name, contains("_freq_pct")) %>%
  select(-rim_freq_pct, -mid_range_freq_pct, -three_point_freq_pct)  # exclude broad categories
print(detailed_zones)

cat("\n=== FUNNEL LEVEL 3: SHOOTING EFFICIENCY BY ZONE (Top 5 Players) ===\n")
zone_efficiency_display <- comprehensive_shooting_profile %>%
  head(5) %>%
  select(athlete_display_name, ends_with("_fg_pct"))
print(zone_efficiency_display)


cat("\n=== SHOOTING ANALYSIS COMPLETE ===\n")
cat("Shot zone classification applied to", nrow(wbb_pbp_enhanced), "plays\n")
cat("Player analysis:", nrow(shooting_by_zone$player_by_zone), "player-zone combinations\n")
cat("Team analysis:", nrow(shooting_by_zone$team_by_zone), "team-zone combinations\n")

cat("\nZone breakdown:\n")
cat("- Corner 3s: Shots within 5 coordinate units of basket X-position\n")
cat("- Above Break 3s: All other three-point attempts\n")
cat("- Rim: Shots within 4 units of basket (~3.3 feet)\n")
cat("- Paint: Shots 4-10 units from basket (~3.3-8.3 feet)\n")
cat("- Mid Range: Shots 10-20 units from basket (~8.3-16.7 feet)\n")
cat("- Long 2: Two-point shots beyond 20 units (~16.7+ feet)\n")