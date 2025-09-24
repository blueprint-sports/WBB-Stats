library(tidyverse)
library(Matrix)
library(glmnet)
library(knitr)
library(data.table)
library(scales)

# Multi-Season RAPM Analysis for Women's Basketball
# Processes seasons 2021-2025 individually

cat("Starting Multi-Season RAPM Analysis for Women's Basketball...\n")

# Define seasons to process (post-COVID)
seasons <- c(2021, 2022, 2023, 2024, 2025)

# Function to process a single season
process_season_rapm <- function(season_year) {
  cat("\n", rep("=", 80), "\n")
  cat("PROCESSING SEASON:", season_year, "\n")
  cat(rep("=", 80), "\n")

  # Load data for this season
  cat("Loading data for", season_year, "season...\n")

  tictoc::tic()
  progressr::with_progress({
    wbb_pbp_season <- wehoop::load_wbb_pbp(seasons = season_year)
  })
  tictoc::toc()

  player_box_season <- wehoop::load_wbb_player_box(seasons = season_year)

  cat("Season", season_year, "data loaded:\n")
  cat("- Play-by-play rows:", nrow(wbb_pbp_season), "\n")
  cat("- Player box score rows:", nrow(player_box_season), "\n")
  cat("- Games:", length(unique(wbb_pbp_season$game_id)), "\n")

  # Get starting lineups for this season
  starting_lineups_season <- player_box_season |>
    filter(starter == TRUE) |>
    select(game_id, team_id, athlete_id, athlete_display_name) |>
    group_by(game_id, team_id) |>
    summarise(
      starters = list(athlete_id),
      starter_names = list(athlete_display_name),
      num_starters = n(),
      .groups = 'drop'
    )

  cat("Starting lineup data:", nrow(starting_lineups_season), "team-game combinations\n")

  # Track player stints for this season using data.table
  track_stints_season <- function() {
    pbp_dt <- as.data.table(wbb_pbp_season)
    starters_dt <- as.data.table(starting_lineups_season)

    valid_games <- intersect(pbp_dt$game_id, starters_dt$game_id)
    cat("Processing", length(valid_games), "games for season", season_year, "...\n")

    pbp_dt <- pbp_dt[game_id %in% valid_games]
    starters_dt <- starters_dt[game_id %in% valid_games]

    setkey(pbp_dt, game_id, game_play_number)
    setkey(starters_dt, game_id)

    stint_results <- pbp_dt[, {
      game_pbp <- .SD
      game_starters <- starters_dt[game_id == .BY$game_id]

      if(nrow(game_starters) != 2) {
        data.table()
      } else {
        home_team_id <- game_pbp$home_team_id[1]
        away_team_id <- game_pbp$away_team_id[1]

        home_starters <- game_starters[team_id == home_team_id]$starters[[1]]
        away_starters <- game_starters[team_id == away_team_id]$starters[[1]]

        home_lineup <- home_starters
        away_lineup <- away_starters

        game_subs <- game_pbp[type_text == "Substitution"]

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

        if(nrow(game_subs) > 0) {
          for(i in 1:nrow(game_subs)) {
            sub <- game_subs[i]
            sub_play_num <- sub$game_play_number

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

  # Process stint tracking
  cat("Starting lineup tracking...\n")
  start_time <- Sys.time()

  all_stint_data_season <- track_stints_season()

  end_time <- Sys.time()
  processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  cat("Lineup tracking completed in", round(processing_time / 60, 2), "minutes\n")
  cat("Total plays processed:", nrow(all_stint_data_season), "\n")

  # Clean up duplicate columns
  if(nrow(all_stint_data_season) > 0) {
    unique_cols <- !duplicated(names(all_stint_data_season))
    all_stint_clean_season <- all_stint_data_season[, ..unique_cols]
  } else {
    cat("WARNING: No stint data processed for season", season_year, "\n")
    return(NULL)
  }

  # Prepare RAPM dataset
  cat("Preparing RAPM dataset...\n")

  rapm_data_tibble_season <- as_tibble(all_stint_clean_season)

  player_cols <- c("home_player_1", "home_player_2", "home_player_3", "home_player_4", "home_player_5",
                   "away_player_1", "away_player_2", "away_player_3", "away_player_4", "away_player_5")

  total_players <- rowSums(!is.na(rapm_data_tibble_season[player_cols]))
  rapm_data_season <- rapm_data_tibble_season[total_players == 10, ]

  cat("Plays with exactly 10 players:", nrow(rapm_data_season), "\n")

  # Join with pbp data to add context
  rapm_with_context_season <- rapm_data_season |>
    left_join(
      wbb_pbp_season |> select(game_id, game_play_number, type_text, period_number,
                               clock_display_value, home_score, away_score, scoring_play),
      by = c("game_id", "game_play_number")
    )

  # Filter out substitutions for RAPM
  rapm_final_season <- rapm_with_context_season |>
    filter(type_text != "Substitution" | is.na(type_text))

  cat("Final RAPM dataset:", nrow(rapm_final_season), "plays\n")
  cat("Games in RAPM dataset:", length(unique(rapm_final_season$game_id)), "\n")

  # Now calculate RAPM for this season
  cat("Calculating RAPM...\n")

  # Process lineup data for RAPM
  lineup_data_season <- rapm_final_season %>%
    filter(!is.na(home_player_1) & !is.na(home_player_2) & !is.na(home_player_3) &
           !is.na(home_player_4) & !is.na(home_player_5) &
           !is.na(away_player_1) & !is.na(away_player_2) & !is.na(away_player_3) &
           !is.na(away_player_4) & !is.na(away_player_5)) %>%
    group_by(game_id, period_number,
             home_player_1, home_player_2, home_player_3, home_player_4, home_player_5,
             away_player_1, away_player_2, away_player_3, away_player_4, away_player_5) %>%
    summarise(
      total_possessions = n(),
      home_points = sum(ifelse(scoring_play == TRUE & !is.na(home_score),
                              c(0, diff(home_score)), 0), na.rm = TRUE),
      away_points = sum(ifelse(scoring_play == TRUE & !is.na(away_score),
                              c(0, diff(away_score)), 0), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      home_ppp100 = 100 * home_points / total_possessions,
      away_ppp100 = 100 * away_points / total_possessions
    ) %>%
    filter(total_possessions >= 1) %>%
    select(
      home_player_1, home_player_2, home_player_3, home_player_4, home_player_5,
      away_player_1, away_player_2, away_player_3, away_player_4, away_player_5,
      possessions = total_possessions,
      home_ppp100,
      away_ppp100
    )

  cat("Lineup combinations:", nrow(lineup_data_season), "\n")

  # Get unique players
  players_season <- unique(c(
    unique(lineup_data_season$home_player_1),
    unique(lineup_data_season$home_player_2),
    unique(lineup_data_season$home_player_3),
    unique(lineup_data_season$home_player_4),
    unique(lineup_data_season$home_player_5),
    unique(lineup_data_season$away_player_1),
    unique(lineup_data_season$away_player_2),
    unique(lineup_data_season$away_player_3),
    unique(lineup_data_season$away_player_4),
    unique(lineup_data_season$away_player_5)
  ))

  players_season <- players_season[!is.na(players_season)]
  players_season <- sort(players_season)

  cat("Unique players (before filtering):", length(players_season), "\n")
  cat("Lineup combinations (before filtering):", nrow(lineup_data_season), "\n")

  # Apply filtering for memory management if dataset is large
  if(length(players_season) > 4500 || nrow(lineup_data_season) > 30000) {
    cat("Large dataset detected - applying filters to reduce memory usage...\n")

    # Filter to players with minimum possessions to reduce matrix size
    player_possessions <- lineup_data_season %>%
      pivot_longer(cols = c(home_player_1:home_player_5, away_player_1:away_player_5),
                   values_to = "player_id") %>%
      group_by(player_id) %>%
      summarise(total_possessions = sum(possessions), .groups = 'drop') %>%
      filter(total_possessions >= 50)  # Minimum 50 possessions

    qualified_players <- player_possessions$player_id
    cat("Players with 50+ possessions:", length(qualified_players), "\n")

    # Filter lineup data to only include lineups with qualified players
    lineup_data_season <- lineup_data_season %>%
      filter(home_player_1 %in% qualified_players &
             home_player_2 %in% qualified_players &
             home_player_3 %in% qualified_players &
             home_player_4 %in% qualified_players &
             home_player_5 %in% qualified_players &
             away_player_1 %in% qualified_players &
             away_player_2 %in% qualified_players &
             away_player_3 %in% qualified_players &
             away_player_4 %in% qualified_players &
             away_player_5 %in% qualified_players)

    # Additionally filter to lineup combinations with minimum possessions
    lineup_data_season <- lineup_data_season %>%
      filter(possessions >= 3)  # Minimum 3 possessions per lineup combination

    # Update player list
    players_season <- unique(c(
      unique(lineup_data_season$home_player_1),
      unique(lineup_data_season$home_player_2),
      unique(lineup_data_season$home_player_3),
      unique(lineup_data_season$home_player_4),
      unique(lineup_data_season$home_player_5),
      unique(lineup_data_season$away_player_1),
      unique(lineup_data_season$away_player_2),
      unique(lineup_data_season$away_player_3),
      unique(lineup_data_season$away_player_4),
      unique(lineup_data_season$away_player_5)
    ))
    players_season <- players_season[!is.na(players_season)]
    players_season <- sort(players_season)

    cat("After filtering:\n")
    cat("- Unique players:", length(players_season), "\n")
    cat("- Lineup combinations:", nrow(lineup_data_season), "\n")

    # Check if still too large
    estimated_matrix_size <- nrow(lineup_data_season) * 2 * length(players_season) * 2
    cat("Estimated matrix elements:", scales::comma(estimated_matrix_size), "\n")

    if(estimated_matrix_size > 500000000) {  # ~500M elements
      cat("Still too large - applying stricter filters...\n")

      # Even stricter filtering
      player_possessions_strict <- lineup_data_season %>%
        pivot_longer(cols = c(home_player_1:home_player_5, away_player_1:away_player_5),
                     values_to = "player_id") %>%
        group_by(player_id) %>%
        summarise(total_possessions = sum(possessions), .groups = 'drop') %>%
        filter(total_possessions >= 100)  # Minimum 100 possessions

      qualified_players_strict <- player_possessions_strict$player_id
      cat("Players with 100+ possessions:", length(qualified_players_strict), "\n")

      lineup_data_season <- lineup_data_season %>%
        filter(home_player_1 %in% qualified_players_strict &
               home_player_2 %in% qualified_players_strict &
               home_player_3 %in% qualified_players_strict &
               home_player_4 %in% qualified_players_strict &
               home_player_5 %in% qualified_players_strict &
               away_player_1 %in% qualified_players_strict &
               away_player_2 %in% qualified_players_strict &
               away_player_3 %in% qualified_players_strict &
               away_player_4 %in% qualified_players_strict &
               away_player_5 %in% qualified_players_strict) %>%
        filter(possessions >= 5)  # Minimum 5 possessions per lineup

      # Update player list again
      players_season <- unique(c(
        unique(lineup_data_season$home_player_1),
        unique(lineup_data_season$home_player_2),
        unique(lineup_data_season$home_player_3),
        unique(lineup_data_season$home_player_4),
        unique(lineup_data_season$home_player_5),
        unique(lineup_data_season$away_player_1),
        unique(lineup_data_season$away_player_2),
        unique(lineup_data_season$away_player_3),
        unique(lineup_data_season$away_player_4),
        unique(lineup_data_season$away_player_5)
      ))
      players_season <- players_season[!is.na(players_season)]
      players_season <- sort(players_season)

      cat("After strict filtering:\n")
      cat("- Unique players:", length(players_season), "\n")
      cat("- Lineup combinations:", nrow(lineup_data_season), "\n")
    }
  }

  cat("Final dataset for RAPM:\n")
  cat("- Unique players:", length(players_season), "\n")
  cat("- Lineup combinations:", nrow(lineup_data_season), "\n")

  # Create matrix functions (same as single season)
  make_matrix_rows <- function(lineup, players_in) {
    home_player1 <- lineup[1]; home_player2 <- lineup[2]; home_player3 <- lineup[3]
    home_player4 <- lineup[4]; home_player5 <- lineup[5]
    away_player1 <- lineup[6]; away_player2 <- lineup[7]; away_player3 <- lineup[8]
    away_player4 <- lineup[9]; away_player5 <- lineup[10]

    zeroRow <- rep(0, length(players_in) * 2)

    zeroRow[which(players_in == home_player1)] <- 1
    zeroRow[which(players_in == home_player2)] <- 1
    zeroRow[which(players_in == home_player3)] <- 1
    zeroRow[which(players_in == home_player4)] <- 1
    zeroRow[which(players_in == home_player5)] <- 1

    zeroRow[which(players_in == away_player1) + length(players_in)] <- -1
    zeroRow[which(players_in == away_player2) + length(players_in)] <- -1
    zeroRow[which(players_in == away_player3) + length(players_in)] <- -1
    zeroRow[which(players_in == away_player4) + length(players_in)] <- -1
    zeroRow[which(players_in == away_player5) + length(players_in)] <- -1

    return(zeroRow)
  }

  make_matrix_rows_away <- function(lineup, players_in) {
    home_player1 <- lineup[1]; home_player2 <- lineup[2]; home_player3 <- lineup[3]
    home_player4 <- lineup[4]; home_player5 <- lineup[5]
    away_player1 <- lineup[6]; away_player2 <- lineup[7]; away_player3 <- lineup[8]
    away_player4 <- lineup[9]; away_player5 <- lineup[10]

    zeroRow <- rep(0, length(players_in) * 2)

    zeroRow[which(players_in == away_player1)] <- 1
    zeroRow[which(players_in == away_player2)] <- 1
    zeroRow[which(players_in == away_player3)] <- 1
    zeroRow[which(players_in == away_player4)] <- 1
    zeroRow[which(players_in == away_player5)] <- 1

    zeroRow[which(players_in == home_player1) + length(players_in)] <- -1
    zeroRow[which(players_in == home_player2) + length(players_in)] <- -1
    zeroRow[which(players_in == home_player3) + length(players_in)] <- -1
    zeroRow[which(players_in == home_player4) + length(players_in)] <- -1
    zeroRow[which(players_in == home_player5) + length(players_in)] <- -1

    return(zeroRow)
  }

  # Create matrices
  cat("Creating player matrices...\n")

  player_matrix_home_season <- t(apply(
    lineup_data_season[, c("home_player_1", "home_player_2", "home_player_3", "home_player_4", "home_player_5",
                           "away_player_1", "away_player_2", "away_player_3", "away_player_4", "away_player_5")],
    1,
    function(x) make_matrix_rows(lineup = x, players_in = players_season)
  ))

  player_matrix_away_season <- t(apply(
    lineup_data_season[, c("home_player_1", "home_player_2", "home_player_3", "home_player_4", "home_player_5",
                           "away_player_1", "away_player_2", "away_player_3", "away_player_4", "away_player_5")],
    1,
    function(x) make_matrix_rows_away(lineup = x, players_in = players_season)
  ))

  player_matrix_home_season <- as(player_matrix_home_season, "dgCMatrix")
  player_matrix_away_season <- as(player_matrix_away_season, "dgCMatrix")

  player_matrix_season <- rbind(player_matrix_home_season, player_matrix_away_season)
  target_season <- c(lineup_data_season$home_ppp100, lineup_data_season$away_ppp100)

  cat("Matrix dimensions:", nrow(player_matrix_season), "x", ncol(player_matrix_season), "\n")

  # Fit RAPM model
  cat("Fitting RAPM model...\n")

  cv_model_season <- cv.glmnet(
    x = player_matrix_season,
    y = target_season,
    alpha = 0,
    standardize = FALSE
  )

  optimal_lambda_season <- cv_model_season$lambda.min
  cat("Optimal lambda:", round(optimal_lambda_season, 6), "\n")

  rapm_model_season <- glmnet(
    x = player_matrix_season,
    y = target_season,
    alpha = 0,
    standardize = FALSE,
    lambda = optimal_lambda_season
  )

  # Extract results
  player_coefficients_season <- rapm_model_season$beta
  o_rapm_season <- player_coefficients_season[1:length(players_season)]
  d_rapm_season <- player_coefficients_season[(length(players_season) + 1):(2 * length(players_season))]

  # Get player info for this season
  player_info_season <- player_box_season %>%
    select(athlete_id, athlete_display_name, team_display_name) %>%
    distinct()

  # Create results
  rapm_results_season <- data.frame(
    player_id = players_season,
    o_rapm = as.numeric(o_rapm_season),
    d_rapm = as.numeric(d_rapm_season)
  ) %>%
    mutate(rapm = o_rapm + d_rapm) %>%
    left_join(player_info_season, by = c("player_id" = "athlete_id")) %>%
    filter(!is.na(athlete_display_name)) %>%
    arrange(desc(rapm)) %>%
    mutate(season = season_year) %>%
    select(
      Season = season,
      Player = athlete_display_name,
      Team = team_display_name,
      RAPM = rapm,
      `O-RAPM` = o_rapm,
      `D-RAPM` = d_rapm
    )

  # Display and save results
  cat("\nTop 15 RAPM Players for", season_year, "season:\n")
  kable(rapm_results_season[1:15, ] %>%
          mutate(across(where(is.numeric), function(x) round(x, 2))),
        align = "c") %>% print()

  # Save season-specific results
  output_file <- paste0("womens_basketball_RAPM_", season_year, ".csv")
  write_csv(rapm_results_season, output_file)
  cat("\nResults saved to", output_file, "\n")

  # Summary stats
  cat("\nSeason", season_year, "Summary:\n")
  cat("Players analyzed:", nrow(rapm_results_season), "\n")
  cat("RAPM range:", round(min(rapm_results_season$RAPM, na.rm = TRUE), 2),
      "to", round(max(rapm_results_season$RAPM, na.rm = TRUE), 2), "\n")
  cat("Mean RAPM:", round(mean(rapm_results_season$RAPM, na.rm = TRUE), 2), "\n")

  return(rapm_results_season)
}

# Process all seasons
all_season_results <- list()

for(season in seasons) {
  tryCatch({
    season_result <- process_season_rapm(season)
    if(!is.null(season_result)) {
      all_season_results[[as.character(season)]] <- season_result
    }
  }, error = function(e) {
    cat("ERROR processing season", season, ":", e$message, "\n")
  })
}

# Combine all results for comparison
if(length(all_season_results) > 0) {
  cat("\n", "=".rep(80), "\n")
  cat("MULTI-SEASON SUMMARY\n")
  cat(rep("=", 80), "\n")

  combined_results <- bind_rows(all_season_results)

  # Save combined results
  write_csv(combined_results, "womens_basketball_RAPM_2021_2025.csv")
  cat("Combined results saved to womens_basketball_RAPM_2021_2025.csv\n")

  # Season summary
  season_summary <- combined_results %>%
    group_by(Season) %>%
    summarise(
      Players = n(),
      Mean_RAPM = round(mean(RAPM, na.rm = TRUE), 2),
      Top_RAPM = round(max(RAPM, na.rm = TRUE), 2),
      Bottom_RAPM = round(min(RAPM, na.rm = TRUE), 2),
      .groups = 'drop'
    )

  cat("\nSeason-by-Season Summary:\n")
  print(season_summary)

  cat("\nMulti-season RAPM analysis complete!\n")
  cat("Processed", length(all_season_results), "seasons successfully\n")
} else {
  cat("No seasons processed successfully\n")
}