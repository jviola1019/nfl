# =============================================================================
# FILE: sports/nfl/props/props_pipeline.R
# PURPOSE: NFL player props integration pipeline
#
# VERSION: 2.8.0
# LAST UPDATED: 2026-02-03
#
# DESCRIPTION:
#   Main entry point for running all player prop simulations for a game.
#   Integrates passing_yards, rushing_yards, receiving_yards, and touchdowns
#   modules into a unified pipeline with HTML report generation support.
#
# USAGE:
#   source("sports/nfl/props/props_pipeline.R")
#   results <- run_player_props(game_id = "2024_22_KC_SF")
# =============================================================================

# Source all prop modules
local({
  props_dir <- dirname(sys.frame(1)$ofile %||% ".")
  source(file.path(props_dir, "props_config.R"))
  api_path <- file.path(props_dir, "..", "..", "..", "R", "prop_odds_api.R")
  if (file.exists(api_path)) source(api_path)
  source(file.path(props_dir, "passing_yards.R"))
  source(file.path(props_dir, "rushing_yards.R"))
  source(file.path(props_dir, "receiving_yards.R"))
  source(file.path(props_dir, "touchdowns.R"))
  source(file.path(props_dir, "data_sources.R"))
})

#' Run All Player Props for a Game
#'
#' Main pipeline function that analyzes all player props for a given game.
#'
#' @param game_id Game ID in format "YYYY_WW_AWAY_HOME" (e.g., "2024_22_KC_SF")
#' @param players Optional vector of player IDs to analyze. If NULL, uses top players.
#' @param prop_types Vector of prop types to analyze
#' @param include_market_lines If TRUE, attempts to load market lines for comparison
#' @param n_trials Number of Monte Carlo trials per prop
#'
#' @return Tibble with all prop recommendations
#'
#' @examples
#' \dontrun{
#'   # Analyze all props for Super Bowl
#'   results <- run_player_props("2024_22_KC_SF")
#'
#'   # Analyze specific prop types
#'   results <- run_player_props("2024_22_KC_SF", prop_types = c("passing", "rushing"))
#'
#'   # Filter positive EV props
#'   positive_ev <- results[results$recommendation != "PASS", ]
#' }
#'
#' @export
run_player_props <- function(
  game_id,
  players = NULL,
  prop_types = c("passing", "rushing", "receiving", "td"),
  include_market_lines = FALSE,
  n_trials = PROP_TRIALS
) {

  message(sprintf("Running player props pipeline for game: %s", game_id))

  # Parse game_id
  parts <- strsplit(game_id, "_")[[1]]
  if (length(parts) != 4) {
    stop("Invalid game_id format. Expected 'YYYY_WW_AWAY_HOME'")
  }
  season <- as.integer(parts[1])
  week <- as.integer(parts[2])
  away_team <- parts[3]
  home_team <- parts[4]

  message(sprintf("  Season: %d, Week: %d, %s @ %s", season, week, away_team, home_team))

  # Optional: load prop odds cache (API or CSV)
  prop_odds_cache <- NULL
  if (isTRUE(include_market_lines)) {
    prop_odds_cache <- tryCatch({
      if (exists("resolve_prop_odds_cache", mode = "function")) {
        resolve_prop_odds_cache()
      } else if (exists("load_prop_odds", mode = "function")) {
        load_prop_odds()
      } else {
        NULL
      }
    }, error = function(e) {
      warning(sprintf("Prop odds source unavailable: %s", e$message))
      NULL
    })
  }

  # Load player data
  player_data <- tryCatch({
    load_player_projections(season, week)
  }, error = function(e) {
    warning("Could not load player projections: ", e$message)
    NULL
  })

  # Load defense rankings
  defense_data <- tryCatch({
    load_defense_rankings(season)
  }, error = function(e) {
    warning("Could not load defense rankings: ", e$message)
    NULL
  })

  # If no player data, return empty result

if (is.null(player_data) || nrow(player_data) == 0) {
    warning("No player data available. Returning empty results.")
    return(tibble::tibble(
      player = character(),
      prop_type = character(),
      line = numeric(),
      projection = numeric(),
      p_over = numeric(),
      p_under = numeric(),
      ev_over = numeric(),
      ev_under = numeric(),
      recommendation = character()
    ))
  }

  # Filter to players in this game
  game_players <- player_data %>%
    dplyr::filter(recent_team %in% c(away_team, home_team))

  if (nrow(game_players) == 0) {
    warning("No players found for teams in this game.")
    return(tibble::tibble())
  }

  # Initialize results list
  results_list <- list()

  # Get defense ranks for both teams
  home_pass_def <- if (!is.null(defense_data)) {
    defense_data$pass_def_rank[defense_data$team == home_team]
  } else 16
  away_pass_def <- if (!is.null(defense_data)) {
    defense_data$pass_def_rank[defense_data$team == away_team]
  } else 16
  home_rush_def <- if (!is.null(defense_data)) {
    defense_data$rush_def_rank[defense_data$team == home_team]
  } else 16
  away_rush_def <- if (!is.null(defense_data)) {
    defense_data$rush_def_rank[defense_data$team == away_team]
  } else 16

  # Use first value if vector
  home_pass_def <- if (length(home_pass_def) > 0) home_pass_def[1] else 16
  away_pass_def <- if (length(away_pass_def) > 0) away_pass_def[1] else 16
  home_rush_def <- if (length(home_rush_def) > 0) home_rush_def[1] else 16
  away_rush_def <- if (length(away_rush_def) > 0) away_rush_def[1] else 16

  # Process each prop type
  for (prop_type in prop_types) {
    message(sprintf("  Processing %s props...", prop_type))

    if (prop_type == "passing") {
      # QBs only
      qbs <- game_players %>%
        dplyr::filter(position == "QB", !is.na(avg_passing_yards), avg_passing_yards > 50)

      for (i in seq_len(nrow(qbs))) {
        qb <- qbs[i, ]
        is_home <- qb$recent_team == home_team
        opp_def <- if (is_home) away_pass_def else home_pass_def

        market_line <- NULL
        if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
          market_line <- get_market_prop_line(
            qb$player_name,
            "passing_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team
          )
        }

        result <- analyze_passing_yards_prop(
          qb_name = qb$player_name,
          qb_avg_yards = qb$avg_passing_yards,
          line = if (!is.null(market_line)) market_line$line else NA_real_,
          opponent = if (is_home) away_team else home_team,
          opp_pass_def_rank = opp_def,
          is_home = is_home,
          is_dome = FALSE,  # TODO: Add stadium data
          over_odds = if (!is.null(market_line)) market_line$over_odds else NA_real_,
          under_odds = if (!is.null(market_line)) market_line$under_odds else NA_real_
        )

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = result$player,
          position = "QB",
          prop_type = "passing_yards",
          line = result$line,
          projection = result$projection,
          p_over = result$p_over,
          p_under = result$p_under,
          ev_over = result$ev_over,
          ev_under = result$ev_under,
          recommendation = result$recommendation
        )
      }
    }

    if (prop_type == "rushing") {
      # RBs and mobile QBs
      rushers <- game_players %>%
        dplyr::filter(position %in% c("RB", "QB"), !is.na(avg_rushing_yards), avg_rushing_yards > 10)

      for (i in seq_len(nrow(rushers))) {
        player <- rushers[i, ]
        is_home <- player$recent_team == home_team
        opp_def <- if (is_home) away_rush_def else home_rush_def

        market_line <- NULL
        if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
          market_line <- get_market_prop_line(
            player$player_name,
            "rushing_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team
          )
        }

        result <- analyze_rushing_yards_prop(
          player_name = player$player_name,
          player_avg_yards = player$avg_rushing_yards,
          position = player$position,
          line = if (!is.null(market_line)) market_line$line else NA_real_,
          opponent = if (is_home) away_team else home_team,
          opp_rush_def_rank = opp_def,
          is_home = is_home,
          over_odds = if (!is.null(market_line)) market_line$over_odds else NA_real_,
          under_odds = if (!is.null(market_line)) market_line$under_odds else NA_real_
        )

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = result$player,
          position = player$position,
          prop_type = "rushing_yards",
          line = result$line,
          projection = result$projection,
          p_over = result$p_over,
          p_under = result$p_under,
          ev_over = result$ev_over,
          ev_under = result$ev_under,
          recommendation = result$recommendation
        )
      }
    }

    if (prop_type == "receiving") {
      # WRs, TEs, RBs
      receivers <- game_players %>%
        dplyr::filter(position %in% c("WR", "TE", "RB"), !is.na(avg_receiving_yards), avg_receiving_yards > 10)

      for (i in seq_len(nrow(receivers))) {
        player <- receivers[i, ]
        is_home <- player$recent_team == home_team
        opp_def <- if (is_home) away_pass_def else home_pass_def

        market_line <- NULL
        if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
          market_line <- get_market_prop_line(
            player$player_name,
            "receiving_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team
          )
        }

        result <- analyze_receiving_yards_prop(
          player_name = player$player_name,
          player_avg_yards = player$avg_receiving_yards,
          position = player$position,
          line = if (!is.null(market_line)) market_line$line else NA_real_,
          opponent = if (is_home) away_team else home_team,
          opp_pass_def_rank = opp_def,
          is_home = is_home,
          over_odds = if (!is.null(market_line)) market_line$over_odds else NA_real_,
          under_odds = if (!is.null(market_line)) market_line$under_odds else NA_real_
        )

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = result$player,
          position = player$position,
          prop_type = "receiving_yards",
          line = result$line,
          projection = result$projection,
          p_over = result$p_over,
          p_under = result$p_under,
          ev_over = result$ev_over,
          ev_under = result$ev_under,
          recommendation = result$recommendation
        )
      }
    }

    if (prop_type == "td") {
      # All skill players
      skill_players <- game_players %>%
        dplyr::filter(position %in% c("QB", "RB", "WR", "TE"), !is.na(avg_touchdowns))

      for (i in seq_len(nrow(skill_players))) {
        player <- skill_players[i, ]
        is_home <- player$recent_team == home_team

        market_td <- NULL
        if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
          market_td <- get_market_prop_line(
            player$player_name,
            "anytime_td",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team
          )
        }

        result <- analyze_touchdown_prop(
          player_name = player$player_name,
          player_td_avg = player$avg_touchdowns,
          position = player$position,
          opponent = if (is_home) away_team else home_team,
          opp_scoring_def_rank = 16,  # Default - TODO: Add scoring defense data
          is_home = is_home,
          anytime_odds = if (!is.null(market_td)) market_td$over_odds else NA_real_
        )

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = result$player,
          position = player$position,
          prop_type = "anytime_td",
          line = NA_real_,
          projection = result$projection,
          p_over = result$p_anytime_td,
          p_under = 1 - result$p_anytime_td,
          ev_over = result$anytime_td$ev,
          ev_under = NA_real_,
          recommendation = result$anytime_td$recommendation
        )
      }
    }
  }

  # Combine results
  if (length(results_list) > 0) {
    results <- dplyr::bind_rows(results_list)
    message(sprintf("  Generated %d prop recommendations", nrow(results)))
    results
  } else {
    message("  No props generated")
    tibble::tibble()
  }
}

#' Generate Player Props HTML Report
#'
#' Creates an HTML report for player props similar to the main game predictions.
#'
#' @param props_results Output from run_player_props()
#' @param game_id Game identifier
#' @param output_file Output HTML file path
#'
#' @return Invisible NULL, writes HTML file
#'
#' @export
generate_props_html_report <- function(props_results, game_id, output_file = "player_props_report.html") {

  if (nrow(props_results) == 0) {
    warning("No props results to report")
    return(invisible(NULL))
  }

  # Format results for display
  display_tbl <- props_results %>%
    dplyr::mutate(
      `Player` = player,
      `Position` = position,
      `Prop Type` = prop_type,
      `Line` = ifelse(is.na(line), "-", as.character(line)),
      `Projection` = round(projection, 1),
      `P(Over)` = scales::percent(p_over, accuracy = 0.1),
      `P(Under)` = scales::percent(p_under, accuracy = 0.1),
      `EV Over` = scales::percent(ev_over, accuracy = 0.1),
      `Recommendation` = recommendation
    ) %>%
    dplyr::select(`Player`, `Position`, `Prop Type`, `Line`, `Projection`,
                  `P(Over)`, `P(Under)`, `EV Over`, `Recommendation`)

  # Create gt table
  gt_tbl <- gt::gt(display_tbl) %>%
    gt::tab_header(
      title = sprintf("Player Props Analysis: %s", game_id),
      subtitle = sprintf("Generated: %s | Monte Carlo Trials: %d", Sys.time(), PROP_TRIALS)
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#22c55e20"),
      locations = gt::cells_body(
        columns = `Recommendation`,
        rows = `Recommendation` %in% c("OVER", "UNDER", "BET")
      )
    ) %>%
    gt::tab_source_note(
      source_note = "Model: Monte Carlo simulation | Normal dist (yards) | Negative Binomial (TDs)"
    )

  # Save HTML
  gt::gtsave(gt_tbl, output_file)
  message(sprintf("Props report saved to: %s", output_file))

  invisible(NULL)
}

#' Quick Props Summary
#'
#' Returns a quick summary of positive EV props.
#'
#' @param props_results Output from run_player_props()
#' @param min_ev Minimum EV threshold (default 0.02 = 2%)
#'
#' @return Filtered tibble with positive EV props
#'
#' @export
get_positive_ev_props <- function(props_results, min_ev = 0.02) {
  props_results %>%
    dplyr::filter(
      (ev_over > min_ev & recommendation %in% c("OVER", "BET")) |
      (ev_under > min_ev & recommendation == "UNDER")
    ) %>%
    dplyr::arrange(dplyr::desc(pmax(ev_over, ev_under, na.rm = TRUE)))
}
