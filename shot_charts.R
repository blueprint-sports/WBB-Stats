library(tidyverse)
library(ggplot2)

# Source the main script to get wbb_pbp dataset
source("wbb_player_stats.R")

# Check if wbb_pbp exists
if(!exists("wbb_pbp")) {
  stop("wbb_pbp dataset not found. Please run wbb_player_stats.R first.")
}

cat("Examining shot data for shot charts...\n")

# Get the first game from the dataset
first_game_id <- unique(wbb_pbp$game_id)[1]
cat("Using game ID:", first_game_id, "\n")

# Get game info
game_info <- wbb_pbp %>%
  filter(game_id == first_game_id) %>%
  select(home_team_name, away_team_name) %>%
  slice(1)

cat("Game:", game_info$away_team_name, "vs", game_info$home_team_name, "\n")

# Filter to shot attempts in the first game
shot_data <- wbb_pbp %>%
  filter(game_id == first_game_id) %>%
  filter(grepl("Shot", type_text, ignore.case = TRUE)) %>%
  select(game_id, type_text, coordinate_x, coordinate_y, team_id, home_team_id, away_team_id,
         home_team_name, away_team_name, text, scoring_play) %>%
  # Add team indicator
  mutate(
    team = ifelse(team_id == home_team_id, home_team_name, away_team_name),
    made = ifelse(scoring_play == TRUE, "Made", "Missed")
  ) %>%
  # Remove shots without coordinates
  filter(!is.na(coordinate_x) & !is.na(coordinate_y))

cat("Shot attempts with coordinates:", nrow(shot_data), "\n")
cat("Shot types found:\n")
print(table(shot_data$type_text))

# Create basic shot chart
if(nrow(shot_data) > 0) {

  cat("\nCoordinate ranges:\n")
  cat("X coordinates:", min(shot_data$coordinate_x, na.rm = TRUE), "to", max(shot_data$coordinate_x, na.rm = TRUE), "\n")
  cat("Y coordinates:", min(shot_data$coordinate_y, na.rm = TRUE), "to", max(shot_data$coordinate_y, na.rm = TRUE), "\n")

  # UConn shot chart
  uconn_shots <- shot_data %>% filter(team == "UConn")

  if(nrow(uconn_shots) > 0) {
    cat("\nUConn shots:", nrow(uconn_shots), "\n")

    uconn_chart <- ggplot(uconn_shots, aes(x = coordinate_x, y = coordinate_y)) +
      geom_point(aes(color = made), size = 3, alpha = 0.7) +
      scale_color_manual(values = c("Made" = "green", "Missed" = "red")) +
      labs(
        title = "UConn Shot Chart",
        subtitle = paste("vs", game_info$away_team_name, "- Game ID:", first_game_id),
        x = "X Coordinate",
        y = "Y Coordinate",
        color = "Shot Result"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )

    print(uconn_chart)
  }

  # South Carolina shot chart
  sc_shots <- shot_data %>% filter(team == "South Carolina")

  if(nrow(sc_shots) > 0) {
    cat("\nSouth Carolina shots:", nrow(sc_shots), "\n")

    sc_chart <- ggplot(sc_shots, aes(x = coordinate_x, y = coordinate_y)) +
      geom_point(aes(color = made), size = 3, alpha = 0.7) +
      scale_color_manual(values = c("Made" = "green", "Missed" = "red")) +
      labs(
        title = "South Carolina Shot Chart",
        subtitle = paste("vs", game_info$home_team_name, "- Game ID:", first_game_id),
        x = "X Coordinate",
        y = "Y Coordinate",
        color = "Shot Result"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)
      )

    print(sc_chart)
  }

  # Show sample of shot data
  cat("\nSample shot data:\n")
  print(shot_data %>%
        select(type_text, coordinate_x, coordinate_y, team, made, text) %>%
        head(10))

} else {
  cat("No shots with coordinates found in this game.\n")
}
