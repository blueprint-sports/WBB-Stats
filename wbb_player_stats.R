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





