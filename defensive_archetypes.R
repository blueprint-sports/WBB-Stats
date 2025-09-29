# =============================================================================
# WOMEN'S BASKETBALL DEFENSIVE ARCHETYPE CLUSTERING
# Adapts men's defensive archetype framework to women's basketball data
# Uses steals, blocks, fouls, rebounds + spatial locations as proxies
# =============================================================================

library(tidyverse)
library(cluster)
library(factoextra)
library(data.table)
library(wehoop)

cat("=== WOMEN'S BASKETBALL DEFENSIVE ARCHETYPE CLUSTERING ===\n")

# Load defensive stats data
if(!exists("defensive_stats")) {
  cat("Loading defensive stats data...\n")
  source("defensive_stats.R")
} else {
  cat("Using existing defensive stats data...\n")
}

create_defensive_clustering_features <- function() {
  cat("Creating defensive clustering features...\n")

  # Start with defensive stats data and add spatial data
  # All the volume data is already computed in defensive_stats!
  defensive_features <- defensive_stats %>%
    # Add spatial defensive stats (note: spatial uses athlete_id_1, defensive uses athlete_id)
    left_join(spatial_defensive_stats, by = c("athlete_id" = "athlete_id_1")) %>%

    mutate(
      # === CORE DEFENSIVE VOLUME METRICS (per minute) ===
      # Use defensive_stats columns that are already computed
      steals_per_minute = steals_per_36 / 36,  # Convert from per-36 to per-minute
      blocks_per_minute = blocks_per_36 / 36,  # Convert from per-36 to per-minute
      def_rebounds_per_minute = def_reb_per_36 / 36,  # Convert from per-36 to per-minute
      fouls_per_minute = fouls_per_36 / 36,  # Convert from per-36 to per-minute

      # === SPATIAL DEFENSIVE ACTIVITY ===
      # Using steal and foul locations as proxies for defensive positioning
      # Handle missing spatial data
      total_located_fouls = ifelse(is.na(total_located_fouls), 0, total_located_fouls),
      total_located_steals = ifelse(is.na(total_located_steals), 0, total_located_steals),

      # Foul location activity (defensive positioning indicators)
      perimeter_fouls_per_min = ifelse(is.na(`fouls_Perimeter Defense`), 0, `fouls_Perimeter Defense`) / (total_minutes / games_played),
      paint_fouls_per_min = ifelse(is.na(`fouls_Paint Defense`), 0, `fouls_Paint Defense`) / (total_minutes / games_played),
      rim_fouls_per_min = ifelse(is.na(`fouls_Rim Defense`), 0, `fouls_Rim Defense`) / (total_minutes / games_played),
      mid_fouls_per_min = ifelse(is.na(`fouls_Mid Range Defense`), 0, `fouls_Mid Range Defense`) / (total_minutes / games_played),

      # Interior fouls = rim + paint
      interior_fouls_per_min = rim_fouls_per_min + paint_fouls_per_min,

      # Steal location activity (using spatial percentages with steals_per_game)
      perimeter_steals_per_min = ifelse(is.na(steal_perimeter_pct), 0, steal_perimeter_pct * steals_per_game) / (total_minutes / games_played),
      paint_steals_per_min = ifelse(is.na(steal_paint_pct), 0, steal_paint_pct * steals_per_game) / (total_minutes / games_played),
      rim_steals_per_min = ifelse(is.na(steal_rim_pct), 0, steal_rim_pct * steals_per_game) / (total_minutes / games_played),
      mid_steals_per_min = ifelse(is.na(steal_mid_pct), 0, steal_mid_pct * steals_per_game) / (total_minutes / games_played),

      # Interior steals = rim + paint
      interior_steals_per_min = rim_steals_per_min + paint_steals_per_min,

      # Wing/mid-court defensive activity
      wing_steals_per_min = mid_steals_per_min,

      # === TOTAL DEFENSIVE ACTIVITY ===
      # Use the defensive_activity already computed in defensive_stats
      defensive_actions_per_minute = defensive_activity / games_played,  # Already computed composite

      # === DEFENSIVE ROLE INDICATORS (adapted from men's framework) ===

      # 1. RIM PROTECTOR ACTIVITY (blocks + interior fouls + def rebounds)
      rim_protector_activity = blocks_per_minute + interior_fouls_per_min +
                              def_rebounds_per_minute + paint_steals_per_min,

      # 2. PERIMETER PEST ACTIVITY (perimeter steals + steal volume)
      perimeter_pest_activity = perimeter_steals_per_min + steals_per_minute,

      # 3. VERSATILE DEFENDER ACTIVITY (balanced across zones)
      versatile_defender_activity = (perimeter_steals_per_min + interior_steals_per_min +
                                    wing_steals_per_min) / 3 +
                                   (blocks_per_minute + steals_per_minute) * 0.5,

      # 4. WING STOPPER ACTIVITY (wing positioning + balanced steal/foul activity)
      wing_stopper_activity = wing_steals_per_min +
                             (perimeter_steals_per_min + interior_steals_per_min) * 0.3 +
                             (steals_per_minute + blocks_per_minute) * 0.4,

      # 5. INTERIOR HELPER ACTIVITY (interior positioning + team rebounds)
      interior_helper_activity = interior_steals_per_min + interior_fouls_per_min +
                                def_rebounds_per_minute + blocks_per_minute * 0.5,

      # 6. LOW ACTIVITY DEFENDER (overall low defensive volume)
      low_activity_score = 1 / (defensive_actions_per_minute + 0.01), # Inverse activity

      # === DEFENSIVE SPECIALIZATION VS VERSATILITY ===

      # Perimeter vs Interior specialization
      perimeter_focus = perimeter_steals_per_min + perimeter_fouls_per_min,
      interior_focus = interior_steals_per_min + paint_steals_per_min +
                      interior_fouls_per_min + blocks_per_minute,

      # Specialization score (difference between perimeter and interior)
      defensive_specialization = abs(perimeter_focus - interior_focus),

      # Balance score (ability to defend multiple areas)
      defensive_balance = pmin(perimeter_focus, interior_focus) + wing_steals_per_min,

      # === POSITIONAL DEFENSIVE INDICATORS ===

      # Guard-like defensive activity (perimeter pressure, steals)
      guard_defensive_activity = perimeter_steals_per_min + steals_per_minute,

      # Big-like defensive activity (interior, blocks, rebounds)
      big_defensive_activity = blocks_per_minute + def_rebounds_per_minute +
                              interior_fouls_per_min + paint_steals_per_min,

      # Wing-like defensive activity (balanced positioning)
      wing_defensive_activity = wing_steals_per_min +
                               (perimeter_focus + interior_focus) * 0.3,

      # === DISRUPTIVE VS POSITIONAL DEFENSE ===

      # Disruptive activity (steals, blocks - active plays)
      disruptive_activity = steals_per_minute + blocks_per_minute,

      # Positional activity (fouls, rebounds - positioning/fundamentals)
      positional_activity = fouls_per_minute + def_rebounds_per_minute,

      # Aggressive vs conservative defender
      aggression_ratio = ifelse(positional_activity > 0,
                               disruptive_activity / positional_activity,
                               disruptive_activity)
    ) %>%

    # Handle missing values and infinites
    mutate(across(where(is.numeric), ~ ifelse(is.na(.x) | is.infinite(.x), 0, .x))) %>%

    # Filter to qualified players (minimum activity for meaningful clustering)
    filter(
      total_minutes >= 200,  # Minimum playing time
      !is.na(steals_per_game), !is.na(blocks_per_game),
      games_played >= 10      # Minimum games played (use games_played from defensive_stats)
    ) %>%

    # Select key features for clustering
    select(
      # Identifiers
      athlete_id, athlete_display_name, team_display_name,

      # Core volume metrics
      steals_per_minute, blocks_per_minute, def_rebounds_per_minute,
      fouls_per_minute, defensive_actions_per_minute,

      # Spatial defensive activity
      perimeter_steals_per_min, interior_steals_per_min, wing_steals_per_min,
      paint_steals_per_min,
      perimeter_fouls_per_min, interior_fouls_per_min, paint_fouls_per_min,

      # Defensive role indicators
      rim_protector_activity, perimeter_pest_activity, versatile_defender_activity,
      wing_stopper_activity, interior_helper_activity, low_activity_score,

      # Specialization vs versatility
      perimeter_focus, interior_focus, defensive_specialization, defensive_balance,

      # Positional indicators
      guard_defensive_activity, big_defensive_activity, wing_defensive_activity,

      # Disruptive vs positional
      disruptive_activity, positional_activity, aggression_ratio,

      # Basic totals for reference
      total_minutes, games_played, steals_per_game, blocks_per_game, def_reb_per_game
    )

  cat("Defensive clustering features created for", nrow(defensive_features), "qualified players\n")
  cat("Using", ncol(defensive_features) - 8, "defensive features\n") # 8 = identifiers + totals

  return(defensive_features)
}

# =============================================================================
# RUN CLUSTERING ANALYSIS
# =============================================================================

# Create features
defensive_features <- create_defensive_clustering_features()

# Prepare clustering matrix
clustering_features <- defensive_features %>%
  select(-athlete_id, -athlete_display_name, -team_display_name,
         -total_minutes, -games_played, -steals_per_game, -blocks_per_game, -def_reb_per_game) %>%
  scale() %>%
  as.matrix()

rownames(clustering_features) <- defensive_features$athlete_id

cat("Defensive clustering matrix prepared:", nrow(clustering_features), "players x", ncol(clustering_features), "features\n")

# Find optimal number of clusters
cat("\n=== FINDING OPTIMAL NUMBER OF CLUSTERS ===\n")

k_values <- 5:8
wss_values <- numeric(length(k_values))
sil_values <- numeric(length(k_values))

set.seed(42)
for(i in seq_along(k_values)) {
  k <- k_values[i]

  kmeans_result <- kmeans(clustering_features, centers = k, nstart = 50, iter.max = 500)
  wss_values[i] <- kmeans_result$tot.withinss

  sil_score <- mean(silhouette(kmeans_result$cluster, dist(clustering_features))[, 3])
  sil_values[i] <- sil_score

  cat("k =", k, ": WSS =", round(wss_values[i], 1), ", Silhouette =", round(sil_score, 3), "\n")
}

# Use k=6 for defensive archetypes (matches men's framework)
optimal_k <- 6
cat("\nUsing k =", optimal_k, "for defensive archetypes\n")

# Final clustering
cat("\n=== FINAL CLUSTERING (k =", optimal_k, ") ===\n")

set.seed(42)
final_kmeans <- kmeans(clustering_features, centers = optimal_k, nstart = 100, iter.max = 1000)

final_sil_score <- mean(silhouette(final_kmeans$cluster, dist(clustering_features))[, 3])
cat("Final silhouette score:", round(final_sil_score, 3), "\n")

# Create results with cluster assignments
defensive_archetypes <- defensive_features %>%
  mutate(cluster = final_kmeans$cluster)

# Analyze cluster characteristics
cat("\n=== CLUSTER ANALYSIS ===\n")

cluster_summary <- defensive_archetypes %>%
  group_by(cluster) %>%
  summarise(
    n_players = n(),
    avg_steals_per_min = round(mean(steals_per_minute), 4),
    avg_blocks_per_min = round(mean(blocks_per_minute), 4),
    avg_def_reb_per_min = round(mean(def_rebounds_per_minute), 4),
    avg_rim_protector = round(mean(rim_protector_activity), 3),
    avg_perimeter_pest = round(mean(perimeter_pest_activity), 3),
    avg_versatile = round(mean(versatile_defender_activity), 3),
    avg_defensive_actions = round(mean(defensive_actions_per_minute), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(n_players))

print(cluster_summary)

# Show sample players from each cluster
cat("\n=== SAMPLE PLAYERS BY CLUSTER ===\n")
for(i in 1:optimal_k) {
  cluster_players <- defensive_archetypes %>%
    filter(cluster == i) %>%
    arrange(desc(defensive_actions_per_minute)) %>%
    head(5)

  cat("\nCluster", i, "(", nrow(defensive_archetypes %>% filter(cluster == i)), "players):\n")
  cat(paste(cluster_players$athlete_display_name, collapse = ", "), "\n")
}

# Calculate cluster centers for interpretation
cluster_centers <- as.data.frame(final_kmeans$centers)
cluster_centers$cluster <- 1:optimal_k

cat("\n=== CLUSTER CENTERS ANALYSIS ===\n")
for(i in 1:optimal_k) {
  center_values <- cluster_centers[i, -ncol(cluster_centers)]
  center_vector <- as.numeric(center_values)
  names(center_vector) <- names(center_values)
  top_features <- names(sort(abs(center_vector), decreasing = TRUE))[1:5]

  cat("\nCluster", i, "distinctive features:\n")
  for(feature in top_features) {
    value <- round(center_vector[[feature]], 2)
    cat("-", feature, ":", value, "\n")
  }
}

# Calculate archetype fit percentages
cat("\n=== CALCULATING ARCHETYPE FIT PERCENTAGES ===\n")

distances_to_centers <- matrix(0, nrow = nrow(clustering_features), ncol = optimal_k)
colnames(distances_to_centers) <- paste0("archetype_", 1:optimal_k, "_fit")

for(i in 1:optimal_k) {
  center <- final_kmeans$centers[i, ]
  for(j in 1:nrow(clustering_features)) {
    distances_to_centers[j, i] <- sqrt(sum((clustering_features[j, ] - center)^2))
  }
}

# Convert distances to fit percentages
scaling_factor <- 0.8
exp_similarities <- exp(-distances_to_centers * scaling_factor)
fit_percentages <- exp_similarities / rowSums(exp_similarities)

# Add fit percentages to results
defensive_archetypes <- defensive_archetypes %>%
  bind_cols(as.data.frame(round(fit_percentages, 3)))

cat("Fit percentages calculated for", nrow(defensive_archetypes), "players\n")

cat("\n=== DEFENSIVE ARCHETYPE ANALYSIS COMPLETE ===\n")
cat("Total players analyzed:", nrow(defensive_archetypes), "\n")
cat("Number of archetypes found:", optimal_k, "\n")
cat("Silhouette score:", round(final_sil_score, 3), "\n")
cat("Each player has fit percentages for all archetypes\n")

# Will determine archetype names after analyzing cluster characteristics
cat("\n=== ARCHETYPE NAMING PENDING ===\n")
cat("Run clustering first to analyze characteristics, then assign names\n")
cat("Based on cluster centers and sample players\n")

# Save intermediate results
write.csv(defensive_archetypes, "womens_basketball_defensive_archetypes_temp.csv", row.names = FALSE)
cat("Temporary results saved for analysis\n")