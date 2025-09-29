# =============================================================================
# OFFENSIVE ARCHETYPES - Women's Basketball Player Clustering
# Creates offensive archetypes using volume-based features (no defensive stats)
# Adapted from men's basketball clustering methodology
# =============================================================================

library(tidyverse)
library(cluster)
library(factoextra)

cat("=== WOMEN'S BASKETBALL OFFENSIVE ARCHETYPE CLUSTERING ===\n")

# Load required data
if(!exists("comprehensive_shooting_profile")) {
  # Load plus_minus data first to avoid dependency issues
  if(!exists("plus_minus_stats") && file.exists("plus_minus.R")) {
    source("plus_minus.R")
  }
  source("shooting_analysis.R")
}

cat("Starting archetype analysis...\n")

# =============================================================================
# FEATURE ENGINEERING FOR OFFENSIVE CLUSTERING
# =============================================================================

create_offensive_clustering_features <- function() {
  cat("Creating offensive clustering features...\n")

  # Get assists data from points_generated_stats if available, otherwise from player_box
  assists_data <- if(exists("points_generated_stats")) {
    points_generated_stats %>%
      select(athlete_id_1, total_assists, assists_2pt, assists_3pt) %>%
      mutate(total_assists = replace_na(total_assists, 0))
  } else {
    # Fallback to box score data
    player_box %>%
      group_by(athlete_id) %>%
      summarise(
        total_assists = sum(assists, na.rm = TRUE),
        assists_2pt = NA_real_,
        assists_3pt = NA_real_,
        .groups = 'drop'
      ) %>%
      rename(athlete_id_1 = athlete_id)
  }

  # Calculate simplified shot frequencies using box score data
  # This is faster than PBP and sufficient to fix the main issue
  cat("Calculating individual shot frequencies from box score data...\n")

  # Start with comprehensive shooting profile and add additional metrics
  offensive_features <- comprehensive_shooting_profile %>%
    # Add assists data
    left_join(assists_data, by = "athlete_id_1") %>%
    mutate(
      # Handle missing assists data
      total_assists = replace_na(total_assists, 0)
    ) %>%
    # Filter to qualified players (minimum thresholds for meaningful analysis)
    filter(
      overall_fga >= 50,  # Minimum 50 field goal attempts
      total_points_generated >= 50  # Minimum 50 points generated
    ) %>%

    # Calculate per-minute rates for volume metrics
    left_join(
      # Get minutes played from plus_minus data or estimate
      if(exists("plus_minus_stats") && "total_minutes" %in% names(plus_minus_stats)) {
        plus_minus_stats %>% select(athlete_id, total_minutes)
      } else {
        # Fallback: estimate minutes from FGA (rough estimate: 2 minutes per FGA)
        tibble(athlete_id = comprehensive_shooting_profile$athlete_id_1,
               total_minutes = pmax(comprehensive_shooting_profile$overall_fga * 2, 100))  # Minimum 100 min
      },
      by = c("athlete_id_1" = "athlete_id")
    ) %>%

    # Create volume-based features (per minute rates)
    mutate(
      # Handle missing minutes (use reasonable default)
      total_minutes = ifelse(is.na(total_minutes) | total_minutes == 0,
                            overall_fga * 2, total_minutes),  # Rough estimate: 2 min per FGA

      # === CORE VOLUME METRICS (per minute) ===
      shots_per_minute = overall_fga / total_minutes,
      points_per_minute = total_points_generated / total_minutes,

      # Three-point volume (ATTEMPTS ONLY)
      threes_attempted_per_minute = overall_3pa / total_minutes,

      # Free throw volume (ATTEMPTS ONLY)
      ft_attempted_per_minute = overall_fta / total_minutes,

      # === SHOT LOCATION STYLE (individual frequencies from box score) ===
      # Calculate actual individual frequencies - this is the key fix!
      three_point_frequency = ifelse(overall_fga > 0, overall_3pa / overall_fga, 0),
      two_point_frequency = ifelse(overall_fga > 0, (overall_fga - overall_3pa) / overall_fga, 0),

      # Estimate detailed breakdowns (players with different 3PT rates will now cluster differently)
      rim_frequency = two_point_frequency * 0.35,  # 35% of 2PT shots are rim attempts
      paint_frequency = two_point_frequency * 0.25,  # 25% of 2PT shots are paint
      mid_frequency = two_point_frequency * 0.30,  # 30% of 2PT shots are mid-range
      long2_frequency = two_point_frequency * 0.10,  # 10% of 2PT shots are long 2s

      # 3-point breakdown
      corner3_frequency = three_point_frequency * 0.20,  # 20% of 3PA are corner 3s
      abovebreak3_frequency = three_point_frequency * 0.80,  # 80% of 3PA are above break

      # Combined frequencies for backward compatibility
      mid_range_frequency = mid_frequency + long2_frequency,

      # === SHOT LOCATION VOLUME (per minute attempts) ===
      # These capture HOW MUCH a player shoots from each zone (volume)
      rim_shots_per_minute = (overall_fga * rim_frequency) / total_minutes,
      paint_shots_per_minute = (overall_fga * paint_frequency) / total_minutes,
      mid_shots_per_minute = (overall_fga * mid_frequency) / total_minutes,
      corner3_shots_per_minute = (overall_fga * corner3_frequency) / total_minutes,
      abovebreak3_shots_per_minute = (overall_fga * abovebreak3_frequency) / total_minutes,
      long2_shots_per_minute = (overall_fga * long2_frequency) / total_minutes,

      # === SHOT CREATION INDICATORS ===
      # Use assist rate as proxy for shot creation (lower assist rate = more self-created)
      assisted_shots_rate = overall_pct_assisted,
      unassisted_shots_rate = 1 - overall_pct_assisted,
      # Volume measures of shot creation (based on FGA, not makes to avoid efficiency)
      unassisted_shots_per_minute = (overall_fga * unassisted_shots_rate) / total_minutes,
      assisted_shots_per_minute = (overall_fga * assisted_shots_rate) / total_minutes,

      # === ROLE AND USAGE INDICATORS ===
      # Fix usage rate calculation - cap at 100% and handle missing data
      team_usage_rate = ifelse(
        is.na(pct_team_points_generated) | pct_team_points_generated > 100 | pct_team_points_generated <= 0,
        pmin(shots_per_minute / 0.4, 1.0),  # Fallback: estimate from shots (cap at 100%)
        pct_team_points_generated / 100
      ),

      # === BROAD SHOOTING CATEGORIES ===
      interior_shots_per_minute = rim_shots_per_minute + paint_shots_per_minute,
      perimeter_shots_per_minute = corner3_shots_per_minute + abovebreak3_shots_per_minute,

      # === OFFENSIVE STYLE INDICATORS ===
      # Three-point specialization
      three_point_rate = ifelse(overall_fga > 0, overall_3pa / overall_fga, 0),
      corner3_specialization = corner3_shots_per_minute,
      abovebreak3_specialization = abovebreak3_shots_per_minute,

      # Interior vs perimeter balance
      interior_vs_perimeter_ratio = ifelse(perimeter_shots_per_minute > 0,
                                          interior_shots_per_minute / perimeter_shots_per_minute,
                                          interior_shots_per_minute * 2),

      # === PLAYMAKER/FACILITATOR INDICATORS ===
      # Weighted facilitator metrics - emphasize actual assist volume over ratio
      assists_per_minute = total_assists / total_minutes,  # Primary facilitator indicator
      playmaker_score = ifelse(overall_fga > 0, total_assists / overall_fga, 0),  # assists per FGA (secondary)

      # True facilitator composite score - high assists + reasonable usage
      facilitator_score = assists_per_minute * 2 + (team_usage_rate * assists_per_minute),  # Volume + context

      # Assist context for shot types (spot-up shooter identification)
      assist_rate_on_threes = ifelse(overall_3pm > 0,
                                     overall_pct_assisted * (overall_3pm / overall_fgm),
                                     0),  # Approximation of 3PT assist rate

      # === OFFENSIVE ROLE SCORES ===
      # High usage indicator
      high_usage_score = shots_per_minute + team_usage_rate,

      # Shot creator vs spot shooter - emphasize STYLE over volume
      shot_creator_score = unassisted_shots_rate + (unassisted_shots_per_minute * 0.5),
      spot_shooter_score = assisted_shots_rate + (three_point_rate * assisted_shots_rate),

      # Pure catch-and-shoot indicator (for spot shooter identification)
      catch_and_shoot_score = ifelse(three_point_rate > 0.3 & assisted_shots_rate > 0.65,
                                   assisted_shots_rate * three_point_rate * 2,
                                   assisted_shots_rate),

      # Interior vs perimeter player
      interior_player_score = rim_shots_per_minute + paint_shots_per_minute,
      perimeter_player_score = corner3_shots_per_minute + abovebreak3_shots_per_minute,

      # Free throw generation (indicator of driving/contact)
      ft_generation_score = ft_attempted_per_minute + rim_shots_per_minute * 0.5,

      # === POSITIONAL INDICATORS (offensive activity based) ===
      guard_activity = perimeter_shots_per_minute + (unassisted_shots_per_minute * 0.5),
      forward_activity = (interior_shots_per_minute + perimeter_shots_per_minute) * 0.5 + mid_shots_per_minute,
      center_activity = interior_shots_per_minute + ft_generation_score * 0.3
    ) %>%

    # Handle missing values and infinites
    mutate(across(where(is.numeric), ~ ifelse(is.na(.x) | is.infinite(.x), 0, .x))) %>%

    # Select key features for clustering
    select(
      # Identifiers
      athlete_id_1, athlete_display_name, team_display_name,

      # Core volume metrics (ATTEMPTS ONLY, no makes/efficiency)
      shots_per_minute, points_per_minute,
      threes_attempted_per_minute, ft_attempted_per_minute,

      # Shot location style (frequencies - where they prefer to shoot)
      rim_frequency, paint_frequency, mid_frequency,
      corner3_frequency, abovebreak3_frequency, long2_frequency,
      mid_range_frequency, three_point_frequency,

      # Shot location volume (per minute attempts - how much they shoot from each zone)
      rim_shots_per_minute, paint_shots_per_minute, mid_shots_per_minute,
      corner3_shots_per_minute, abovebreak3_shots_per_minute, long2_shots_per_minute,
      interior_shots_per_minute, perimeter_shots_per_minute,

      # Shot creation
      assisted_shots_per_minute, unassisted_shots_per_minute,
      assisted_shots_rate, unassisted_shots_rate,

      # Role indicators
      team_usage_rate, high_usage_score,
      shot_creator_score, spot_shooter_score, catch_and_shoot_score,

      # Style indicators
      three_point_rate, interior_vs_perimeter_ratio,
      corner3_specialization, abovebreak3_specialization,
      ft_generation_score,

      # Position indicators
      guard_activity, forward_activity, center_activity,

      # Playmaker/facilitator metrics (volume-based, not efficiency)
      assists_per_minute, playmaker_score, facilitator_score, assist_rate_on_threes
    )

  cat("Offensive clustering features created for", nrow(offensive_features), "qualified players\n")
  cat("Using", ncol(offensive_features) - 3, "offensive features\n")

  return(offensive_features)
}

# =============================================================================
# RUN CLUSTERING ANALYSIS
# =============================================================================

run_offensive_clustering <- function(features_data) {
  cat("\\n=== RUNNING CLUSTERING ANALYSIS ===\\n")

  # Prepare clustering matrix with better data cleaning
  clustering_matrix <- features_data %>%
    select(-athlete_id_1, -athlete_display_name, -team_display_name) %>%
    # Handle missing, infinite, and NaN values
    mutate(across(everything(), ~ ifelse(is.na(.x) | is.infinite(.x) | is.nan(.x), 0, .x))) %>%
    # Remove any remaining problematic values
    mutate(across(everything(), ~ ifelse(abs(.x) > 1e10, 0, .x))) %>%
    scale() %>%
    as.matrix()

  # Check for any remaining problematic values
  if(any(is.na(clustering_matrix)) || any(is.infinite(clustering_matrix)) || any(is.nan(clustering_matrix))) {
    cat("Warning: Found problematic values in clustering matrix. Replacing with 0.\n")
    clustering_matrix[is.na(clustering_matrix) | is.infinite(clustering_matrix) | is.nan(clustering_matrix)] <- 0
  }

  # Add player IDs as row names
  rownames(clustering_matrix) <- features_data$athlete_id_1

  cat("Clustering matrix prepared:", nrow(clustering_matrix), "players x", ncol(clustering_matrix), "features\\n")

  # Find optimal number of clusters
  cat("\\n=== FINDING OPTIMAL NUMBER OF CLUSTERS ===\\n")

  k_values <- 4:12
  wss_values <- numeric(length(k_values))
  sil_values <- numeric(length(k_values))

  set.seed(42)
  for(i in seq_along(k_values)) {
    k <- k_values[i]

    kmeans_result <- kmeans(clustering_matrix, centers = k, nstart = 50, iter.max = 500)
    wss_values[i] <- kmeans_result$tot.withinss

    if(k > 1) {  # Silhouette requires k > 1
      sil_score <- mean(silhouette(kmeans_result$cluster, dist(clustering_matrix))[, 3])
      sil_values[i] <- sil_score
    } else {
      sil_values[i] <- 0
    }

    cat("k =", k, ": WSS =", round(wss_values[i], 1), ", Silhouette =", round(sil_values[i], 3), "\\n")
  }

  # Find optimal k
  optimal_k_statistical <- k_values[which.max(sil_values)]
  cat("\\nOptimal k based on silhouette score:", optimal_k_statistical, "\\n")

  # Use optimal k or override based on basketball analysis needs
  # Back to k=8 with improved facilitator weighting for better granular separation
  optimal_k <- 8
  cat("Statistical optimal k =", optimal_k_statistical, ", but using k =", optimal_k, "with improved facilitator weighting\\n")

  # Final clustering
  cat("\\n=== FINAL CLUSTERING (k =", optimal_k, ") ===\\n")

  set.seed(42)
  final_kmeans <- kmeans(clustering_matrix, centers = optimal_k, nstart = 100, iter.max = 1000)

  final_sil_score <- mean(silhouette(final_kmeans$cluster, dist(clustering_matrix))[, 3])
  cat("Final silhouette score:", round(final_sil_score, 3), "\\n")

  return(list(
    kmeans_result = final_kmeans,
    clustering_matrix = clustering_matrix,
    optimal_k = optimal_k,
    silhouette_score = final_sil_score
  ))
}

# =============================================================================
# CREATE ARCHETYPE ASSIGNMENTS AND FIT PERCENTAGES
# =============================================================================

create_archetype_assignments <- function(features_data, clustering_result) {
  cat("\\n=== CREATING ARCHETYPE ASSIGNMENTS ===\\n")

  optimal_k <- clustering_result$optimal_k
  final_kmeans <- clustering_result$kmeans_result
  clustering_matrix <- clustering_result$clustering_matrix

  # Create initial results with cluster assignments
  archetype_results <- features_data %>%
    mutate(cluster = as.factor(final_kmeans$cluster))

  # Analyze cluster characteristics for naming
  cluster_summary <- archetype_results %>%
    group_by(cluster) %>%
    summarise(
      n_players = n(),
      avg_shots_per_min = round(mean(shots_per_minute), 3),
      avg_points_per_min = round(mean(points_per_minute), 3),
      avg_interior_shots = round(mean(interior_shots_per_minute), 3),
      avg_perimeter_shots = round(mean(perimeter_shots_per_minute), 3),
      avg_three_rate = round(mean(three_point_rate), 3),
      avg_usage = round(mean(team_usage_rate), 3),
      avg_assisted_rate = round(mean(assisted_shots_rate), 3),
      .groups = "drop"
    ) %>%
    arrange(cluster)

  cat("\\n=== CLUSTER CHARACTERISTICS ===\\n")
  print(cluster_summary)

  # Calculate fit percentages using soft clustering
  cat("\\n=== CALCULATING ARCHETYPE FIT PERCENTAGES ===\\n")

  distances_to_centers <- matrix(0, nrow = nrow(clustering_matrix), ncol = optimal_k)
  colnames(distances_to_centers) <- paste0("distance_to_cluster_", 1:optimal_k)

  for(i in 1:optimal_k) {
    center <- final_kmeans$centers[i, ]
    for(j in 1:nrow(clustering_matrix)) {
      distances_to_centers[j, i] <- sqrt(sum((clustering_matrix[j, ] - center)^2))
    }
  }

  # Convert distances to fit percentages
  scaling_factor <- 0.8  # Tuned for realistic hybrid distributions
  exp_similarities <- exp(-distances_to_centers * scaling_factor)
  fit_percentages <- exp_similarities / rowSums(exp_similarities)

  # Add fit percentages to results
  fit_df <- as.data.frame(round(fit_percentages, 3))
  names(fit_df) <- paste0("archetype_", 1:optimal_k, "_fit")

  final_results <- archetype_results %>%
    bind_cols(fit_df)

  cat("Fit percentages calculated for", nrow(final_results), "players\\n")

  return(list(
    results = final_results,
    cluster_summary = cluster_summary,
    kmeans_result = final_kmeans
  ))
}

# =============================================================================
# RUN COMPLETE ANALYSIS
# =============================================================================

cat("Creating offensive clustering features...\\n")
offensive_features <- create_offensive_clustering_features()

cat("\\nRunning clustering analysis...\\n")
clustering_result <- run_offensive_clustering(offensive_features)

cat("\\nCreating archetype assignments...\\n")
archetype_analysis <- create_archetype_assignments(offensive_features, clustering_result)

# Extract final results
offensive_archetypes <- archetype_analysis$results
cluster_summary <- archetype_analysis$cluster_summary

# Show sample players from each cluster
cat("\\n=== SAMPLE PLAYERS BY CLUSTER ===\\n")
for(i in 1:clustering_result$optimal_k) {
  cluster_players <- offensive_archetypes %>%
    filter(cluster == i) %>%
    arrange(desc(points_per_minute)) %>%
    head(5)

  cat("\\nCluster", i, "(", nrow(offensive_archetypes %>% filter(cluster == i)), "players):\\n")
  if(nrow(cluster_players) > 0) {
    cat(paste(cluster_players$athlete_display_name, collapse = ", "), "\\n")
  }
}

# Show cluster centers analysis
cat("\\n=== CLUSTER CENTERS ANALYSIS ===\\n")
cluster_centers <- as.data.frame(clustering_result$kmeans_result$centers)
cluster_centers$cluster <- 1:clustering_result$optimal_k

for(i in 1:clustering_result$optimal_k) {
  center_values <- cluster_centers[i, -ncol(cluster_centers)]
  center_vector <- as.numeric(center_values)
  names(center_vector) <- names(center_values)
  top_features <- names(sort(abs(center_vector), decreasing = TRUE))[1:5]

  cat("\\nCluster", i, "distinctive features:\\n")
  for(feature in top_features) {
    value <- round(center_vector[[feature]], 2)
    cat("-", feature, ":", value, "\\n")
  }
}

cat("\\n=== OFFENSIVE ARCHETYPE ANALYSIS COMPLETE ===\\n")
cat("Total players analyzed:", nrow(offensive_archetypes), "\\n")
cat("Number of archetypes found:", clustering_result$optimal_k, "\\n")
cat("Silhouette score:", round(clustering_result$silhouette_score, 3), "\\n")
cat("Each player has fit percentages for all archetypes\\n")

# =============================================================================
# CREATE COMPREHENSIVE PLAYER ARCHETYPE TABLE
# =============================================================================

cat("\\n=== CREATING PLAYER ARCHETYPE TABLE ===\\n")

# Create archetype name mapping - matching men's framework for continuity
archetype_names <- c(
  '1' = 'Interior Finisher',         # Audi Crooks - interior focus, high volume
  '2' = 'Spot Up Shooter',           # High 3PT, high assisted, low volume
  '3' = 'Perimeter Shot Creator',    # High 3PT volume, shot creators
  '4' = 'Low Usage Role Player',     # High 3PT freq, high assisted, very low volume
  '5' = 'Facilitator',               # Emily Ryan - assists, playmaking
  '6' = 'Balanced Role Scorer',      # Moderate everything
  '7' = 'Primary Option',            # Paige Bueckers, Ta'Niya Latson - high usage scorers
  '8' = 'Interior Role Player'       # Interior focus, role players
)

# Create comprehensive table
player_archetype_table <- offensive_archetypes %>%
  # Add primary archetype name
  mutate(primary_archetype = archetype_names[as.character(cluster)]) %>%
  # Select key columns
  select(
    # Player info
    athlete_id = athlete_id_1,
    player_name = athlete_display_name,
    team = team_display_name,
    primary_archetype,

    # Fit percentages for all 8 archetypes (corrected mapping)
    interior_finisher_fit = archetype_1_fit,           # Cluster 1 = Interior Finisher
    spot_up_shooter_fit = archetype_2_fit,              # Cluster 2 = Spot Up Shooter
    perimeter_shot_creator_fit = archetype_3_fit,       # Cluster 3 = Perimeter Shot Creator
    low_usage_role_player_fit = archetype_4_fit,        # Cluster 4 = Low Usage Role Player
    facilitator_fit = archetype_5_fit,                  # Cluster 5 = Facilitator
    balanced_role_scorer_fit = archetype_6_fit,         # Cluster 6 = Balanced Role Scorer
    primary_option_fit = archetype_7_fit,               # Cluster 7 = Primary Option
    interior_role_player_fit = archetype_8_fit,         # Cluster 8 = Interior Role Player

    # Key clustering features
    shots_per_minute,
    threes_attempted_per_minute,
    ft_attempted_per_minute,
    assists_per_minute,
    playmaker_score,
    facilitator_score,
    assisted_shots_rate,
    unassisted_shots_rate,
    three_point_frequency,
    team_usage_rate,
    shot_creator_score,
    spot_shooter_score
  ) %>%
  # Round percentages and scores
  mutate(
    across(ends_with('_fit'), ~ round(.x, 3)),
    across(c(shots_per_minute:spot_shooter_score), ~ round(.x, 4))
  ) %>%
  # Order by primary archetype then by fit percentage for that archetype
  arrange(primary_archetype, desc(case_when(
    primary_archetype == "Facilitator" ~ facilitator_fit,
    primary_archetype == "Interior Finisher" ~ interior_finisher_fit,
    primary_archetype == "Low Usage Role Player" ~ low_usage_role_player_fit,
    primary_archetype == "Interior Role Player" ~ interior_role_player_fit,
    primary_archetype == "Spot Up Shooter" ~ spot_up_shooter_fit,
    primary_archetype == "Balanced Role Scorer" ~ balanced_role_scorer_fit,
    primary_archetype == "Perimeter Shot Creator" ~ perimeter_shot_creator_fit,
    primary_archetype == "Primary Option" ~ primary_option_fit,
    TRUE ~ 0
  )))

# Show summary
cat("Player Archetype Table Created\\n")
cat("Total players:", nrow(player_archetype_table), "\\n")
cat("Columns:", ncol(player_archetype_table), "\\n\\n")

# Show archetype distribution
cat("Archetype Distribution:\\n")
archetype_counts <- table(player_archetype_table$primary_archetype)
for(archetype in names(archetype_counts)) {
  cat("-", archetype, ":", archetype_counts[archetype], "players\\n")
}

# Show top players from each archetype
cat("\\n=== TOP PLAYERS BY ARCHETYPE ===\\n")
for(archetype in names(archetype_names)) {
  archetype_name <- archetype_names[archetype]
  top_players <- player_archetype_table %>%
    filter(primary_archetype == archetype_name) %>%
    head(3)

  if(nrow(top_players) > 0) {
    cat("\\n", archetype_name, ":\\n")
    for(i in 1:nrow(top_players)) {
      player <- top_players[i, ]
      fit_col <- paste0(tolower(gsub(" ", "_", archetype_name)), "_fit")
      fit_value <- player[[fit_col]]
      cat("-", player$player_name, "(", player$team, ") - Fit:", fit_value, "\\n")
    }
  }
}

# Save to CSV
write.csv(player_archetype_table, "womens_basketball_offensive_archetypes.csv", row.names = FALSE)
cat("\\nTable saved as: womens_basketball_offensive_archetypes.csv\\n")

cat("\\nUse 'player_archetype_table' for the complete results\\n")
cat("Use 'offensive_archetypes' for detailed clustering output\\n")
cat("Use 'cluster_summary' for cluster characteristics\\n")