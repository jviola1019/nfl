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

# Provide local fallback for %||% when sourced directly
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 ||
                                 (length(x) == 1 && is.na(x))) y else x
}

# Source all prop modules
local({
  props_dir <- dirname(sys.frame(1)$ofile %||% ".")
  source(file.path(props_dir, "props_config.R"))
  api_path <- file.path(props_dir, "..", "..", "..", "R", "prop_odds_api.R")
  if (file.exists(api_path)) source(api_path)
  source(file.path(props_dir, "passing_yards.R"))
  source(file.path(props_dir, "rushing_yards.R"))
  source(file.path(props_dir, "receiving_yards.R"))
  source(file.path(props_dir, "receptions.R"))
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
  prop_types = c("passing", "rushing", "receiving", "receptions", "td"),
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

  book_priority <- if (exists("PROP_ODDS_BOOK_PRIORITY")) PROP_ODDS_BOOK_PRIORITY else c("draftkings", "fanduel")
  allow_model_odds <- isTRUE(if (exists("PROP_ALLOW_MODEL_ODDS")) PROP_ALLOW_MODEL_ODDS else FALSE)

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
        market_line_dk <- NULL
        market_line_fd <- NULL
        if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
          market_line <- get_market_prop_line(
            qb$player_name,
            "passing_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = book_priority
          )
          market_line_dk <- get_market_prop_line(
            qb$player_name,
            "passing_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "draftkings"
          )
          market_line_fd <- get_market_prop_line(
            qb$player_name,
            "passing_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "fanduel"
          )
        }

        result <- analyze_passing_yards_prop(
          qb_name = qb$player_name,
          qb_avg_yards = qb$avg_passing_yards,
          line = if (!is.null(market_line)) market_line$line else NA_real_,
          line_over = if (!is.null(market_line)) market_line$line_over else NA_real_,
          line_under = if (!is.null(market_line)) market_line$line_under else NA_real_,
          opponent = if (is_home) away_team else home_team,
          opp_pass_def_rank = opp_def,
          is_home = is_home,
          is_dome = FALSE,  # TODO: Add stadium data
          over_odds = if (!is.null(market_line)) market_line$over_odds else NA_real_,
          under_odds = if (!is.null(market_line)) market_line$under_odds else NA_real_
        )

        has_market_line <- !is.null(market_line) &&
          (is.finite(market_line$line) || is.finite(market_line$line_over) || is.finite(market_line$line_under))
        has_market_odds <- !is.null(market_line) &&
          is.finite(market_line$over_odds) && is.finite(market_line$under_odds)

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = result$player,
          position = "QB",
          team = qb$recent_team,
          prop_type = "passing_yards",
          line = result$line,
          line_over = result$line_over,
          line_under = result$line_under,
          projection = result$projection,
          p_over = result$p_over,
          p_under = result$p_under,
          p_push = result$p_push,
          over_odds = result$over_odds,
          under_odds = result$under_odds,
          book = if (!is.null(market_line)) market_line$book else NA_character_,
          dk_line = if (!is.null(market_line_dk)) market_line_dk$line else NA_real_,
          dk_line_over = if (!is.null(market_line_dk)) market_line_dk$line_over else NA_real_,
          dk_line_under = if (!is.null(market_line_dk)) market_line_dk$line_under else NA_real_,
          dk_over_odds = if (!is.null(market_line_dk)) market_line_dk$over_odds else NA_real_,
          dk_under_odds = if (!is.null(market_line_dk)) market_line_dk$under_odds else NA_real_,
          fd_line = if (!is.null(market_line_fd)) market_line_fd$line else NA_real_,
          fd_line_over = if (!is.null(market_line_fd)) market_line_fd$line_over else NA_real_,
          fd_line_under = if (!is.null(market_line_fd)) market_line_fd$line_under else NA_real_,
          fd_over_odds = if (!is.null(market_line_fd)) market_line_fd$over_odds else NA_real_,
          fd_under_odds = if (!is.null(market_line_fd)) market_line_fd$under_odds else NA_real_,
          line_source = ifelse(has_market_line, "market", "model"),
          odds_source = ifelse(has_market_odds, "market", ifelse(allow_model_odds, "model", "missing")),
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
        market_line_dk <- NULL
        market_line_fd <- NULL
        if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
          market_line <- get_market_prop_line(
            player$player_name,
            "rushing_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = book_priority
          )
          market_line_dk <- get_market_prop_line(
            player$player_name,
            "rushing_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "draftkings"
          )
          market_line_fd <- get_market_prop_line(
            player$player_name,
            "rushing_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "fanduel"
          )
        }

        result <- analyze_rushing_yards_prop(
          player_name = player$player_name,
          player_avg_yards = player$avg_rushing_yards,
          position = player$position,
          line = if (!is.null(market_line)) market_line$line else NA_real_,
          line_over = if (!is.null(market_line)) market_line$line_over else NA_real_,
          line_under = if (!is.null(market_line)) market_line$line_under else NA_real_,
          opponent = if (is_home) away_team else home_team,
          opp_rush_def_rank = opp_def,
          is_home = is_home,
          over_odds = if (!is.null(market_line)) market_line$over_odds else NA_real_,
          under_odds = if (!is.null(market_line)) market_line$under_odds else NA_real_
        )

        has_market_line <- !is.null(market_line) &&
          (is.finite(market_line$line) || is.finite(market_line$line_over) || is.finite(market_line$line_under))
        has_market_odds <- !is.null(market_line) &&
          is.finite(market_line$over_odds) && is.finite(market_line$under_odds)

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = result$player,
          position = player$position,
          team = player$recent_team,
          prop_type = "rushing_yards",
          line = result$line,
          line_over = result$line_over,
          line_under = result$line_under,
          projection = result$projection,
          p_over = result$p_over,
          p_under = result$p_under,
          p_push = result$p_push,
          over_odds = result$over_odds,
          under_odds = result$under_odds,
          book = if (!is.null(market_line)) market_line$book else NA_character_,
          dk_line = if (!is.null(market_line_dk)) market_line_dk$line else NA_real_,
          dk_line_over = if (!is.null(market_line_dk)) market_line_dk$line_over else NA_real_,
          dk_line_under = if (!is.null(market_line_dk)) market_line_dk$line_under else NA_real_,
          dk_over_odds = if (!is.null(market_line_dk)) market_line_dk$over_odds else NA_real_,
          dk_under_odds = if (!is.null(market_line_dk)) market_line_dk$under_odds else NA_real_,
          fd_line = if (!is.null(market_line_fd)) market_line_fd$line else NA_real_,
          fd_line_over = if (!is.null(market_line_fd)) market_line_fd$line_over else NA_real_,
          fd_line_under = if (!is.null(market_line_fd)) market_line_fd$line_under else NA_real_,
          fd_over_odds = if (!is.null(market_line_fd)) market_line_fd$over_odds else NA_real_,
          fd_under_odds = if (!is.null(market_line_fd)) market_line_fd$under_odds else NA_real_,
          line_source = ifelse(has_market_line, "market", "model"),
          odds_source = ifelse(has_market_odds, "market", ifelse(allow_model_odds, "model", "missing")),
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
        market_line_dk <- NULL
        market_line_fd <- NULL
        if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
          market_line <- get_market_prop_line(
            player$player_name,
            "receiving_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = book_priority
          )
          market_line_dk <- get_market_prop_line(
            player$player_name,
            "receiving_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "draftkings"
          )
          market_line_fd <- get_market_prop_line(
            player$player_name,
            "receiving_yards",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "fanduel"
          )
        }

        result <- analyze_receiving_yards_prop(
          player_name = player$player_name,
          player_avg_yards = player$avg_receiving_yards,
          position = player$position,
          line = if (!is.null(market_line)) market_line$line else NA_real_,
          line_over = if (!is.null(market_line)) market_line$line_over else NA_real_,
          line_under = if (!is.null(market_line)) market_line$line_under else NA_real_,
          opponent = if (is_home) away_team else home_team,
          opp_pass_def_rank = opp_def,
          is_home = is_home,
          over_odds = if (!is.null(market_line)) market_line$over_odds else NA_real_,
          under_odds = if (!is.null(market_line)) market_line$under_odds else NA_real_
        )

        has_market_line <- !is.null(market_line) &&
          (is.finite(market_line$line) || is.finite(market_line$line_over) || is.finite(market_line$line_under))
        has_market_odds <- !is.null(market_line) &&
          is.finite(market_line$over_odds) && is.finite(market_line$under_odds)

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = result$player,
          position = player$position,
          team = player$recent_team,
          prop_type = "receiving_yards",
          line = result$line,
          line_over = result$line_over,
          line_under = result$line_under,
          projection = result$projection,
          p_over = result$p_over,
          p_under = result$p_under,
          p_push = result$p_push,
          over_odds = result$over_odds,
          under_odds = result$under_odds,
          book = if (!is.null(market_line)) market_line$book else NA_character_,
          dk_line = if (!is.null(market_line_dk)) market_line_dk$line else NA_real_,
          dk_line_over = if (!is.null(market_line_dk)) market_line_dk$line_over else NA_real_,
          dk_line_under = if (!is.null(market_line_dk)) market_line_dk$line_under else NA_real_,
          dk_over_odds = if (!is.null(market_line_dk)) market_line_dk$over_odds else NA_real_,
          dk_under_odds = if (!is.null(market_line_dk)) market_line_dk$under_odds else NA_real_,
          fd_line = if (!is.null(market_line_fd)) market_line_fd$line else NA_real_,
          fd_line_over = if (!is.null(market_line_fd)) market_line_fd$line_over else NA_real_,
          fd_line_under = if (!is.null(market_line_fd)) market_line_fd$line_under else NA_real_,
          fd_over_odds = if (!is.null(market_line_fd)) market_line_fd$over_odds else NA_real_,
          fd_under_odds = if (!is.null(market_line_fd)) market_line_fd$under_odds else NA_real_,
          line_source = ifelse(has_market_line, "market", "model"),
          odds_source = ifelse(has_market_odds, "market", ifelse(allow_model_odds, "model", "missing")),
          ev_over = result$ev_over,
          ev_under = result$ev_under,
          recommendation = result$recommendation
        )
      }
    }

    if (prop_type == "receptions") {
      receivers <- game_players %>%
        dplyr::filter(position %in% c("WR", "TE", "RB"), !is.na(avg_receptions), avg_receptions > 1)

      for (i in seq_len(nrow(receivers))) {
        player <- receivers[i, ]
        is_home <- player$recent_team == home_team
        opp_def <- if (is_home) away_pass_def else home_pass_def

        market_line <- NULL
        market_line_dk <- NULL
        market_line_fd <- NULL
        if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
          market_line <- get_market_prop_line(
            player$player_name,
            "receptions",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = book_priority
          )
          market_line_dk <- get_market_prop_line(
            player$player_name,
            "receptions",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "draftkings"
          )
          market_line_fd <- get_market_prop_line(
            player$player_name,
            "receptions",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "fanduel"
          )
        }

        result <- analyze_receptions_prop(
          player_name = player$player_name,
          player_avg_receptions = player$avg_receptions,
          position = player$position,
          line = if (!is.null(market_line)) market_line$line else NA_real_,
          line_over = if (!is.null(market_line)) market_line$line_over else NA_real_,
          line_under = if (!is.null(market_line)) market_line$line_under else NA_real_,
          opponent = if (is_home) away_team else home_team,
          opp_pass_def_rank = opp_def,
          is_home = is_home,
          over_odds = if (!is.null(market_line)) market_line$over_odds else NA_real_,
          under_odds = if (!is.null(market_line)) market_line$under_odds else NA_real_
        )

        has_market_line <- !is.null(market_line) &&
          (is.finite(market_line$line) || is.finite(market_line$line_over) || is.finite(market_line$line_under))
        has_market_odds <- !is.null(market_line) &&
          is.finite(market_line$over_odds) && is.finite(market_line$under_odds)

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = result$player,
          position = player$position,
          team = player$recent_team,
          prop_type = "receptions",
          line = result$line,
          line_over = result$line_over,
          line_under = result$line_under,
          projection = result$projection,
          p_over = result$p_over,
          p_under = result$p_under,
          p_push = result$p_push,
          over_odds = result$over_odds,
          under_odds = result$under_odds,
          book = if (!is.null(market_line)) market_line$book else NA_character_,
          dk_line = if (!is.null(market_line_dk)) market_line_dk$line else NA_real_,
          dk_line_over = if (!is.null(market_line_dk)) market_line_dk$line_over else NA_real_,
          dk_line_under = if (!is.null(market_line_dk)) market_line_dk$line_under else NA_real_,
          dk_over_odds = if (!is.null(market_line_dk)) market_line_dk$over_odds else NA_real_,
          dk_under_odds = if (!is.null(market_line_dk)) market_line_dk$under_odds else NA_real_,
          fd_line = if (!is.null(market_line_fd)) market_line_fd$line else NA_real_,
          fd_line_over = if (!is.null(market_line_fd)) market_line_fd$line_over else NA_real_,
          fd_line_under = if (!is.null(market_line_fd)) market_line_fd$line_under else NA_real_,
          fd_over_odds = if (!is.null(market_line_fd)) market_line_fd$over_odds else NA_real_,
          fd_under_odds = if (!is.null(market_line_fd)) market_line_fd$under_odds else NA_real_,
          line_source = ifelse(has_market_line, "market", "model"),
          odds_source = ifelse(has_market_odds, "market", ifelse(allow_model_odds, "model", "missing")),
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
        market_td_dk <- NULL
        market_td_fd <- NULL
        if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
          market_td <- get_market_prop_line(
            player$player_name,
            "anytime_td",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = book_priority
          )
          market_td_dk <- get_market_prop_line(
            player$player_name,
            "anytime_td",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "draftkings"
          )
          market_td_fd <- get_market_prop_line(
            player$player_name,
            "anytime_td",
            prop_odds_cache,
            home_team = home_team,
            away_team = away_team,
            preferred_books = "fanduel"
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

        has_market_line <- !is.null(market_td) &&
          (is.finite(market_td$line) || is.finite(market_td$line_over) || is.finite(market_td$line_under))
        has_market_odds <- !is.null(market_td) && is.finite(market_td$over_odds)

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = result$player,
          position = player$position,
          team = player$recent_team,
          prop_type = "anytime_td",
          line = if (!is.null(market_td)) market_td$line else 0.5,
          line_over = if (!is.null(market_td)) market_td$line_over else 0.5,
          line_under = if (!is.null(market_td)) market_td$line_under else 0.5,
          projection = result$projection,
          p_over = result$p_anytime_td,
          p_under = 1 - result$p_anytime_td,
          over_odds = result$anytime_td$odds,
          under_odds = NA_real_,
          ev_over = result$anytime_td$ev,
          ev_under = NA_real_,
          book = if (!is.null(market_td)) market_td$book else NA_character_,
          dk_line = if (!is.null(market_td_dk)) market_td_dk$line else NA_real_,
          dk_line_over = if (!is.null(market_td_dk)) market_td_dk$line_over else NA_real_,
          dk_line_under = if (!is.null(market_td_dk)) market_td_dk$line_under else NA_real_,
          dk_over_odds = if (!is.null(market_td_dk)) market_td_dk$over_odds else NA_real_,
          dk_under_odds = if (!is.null(market_td_dk)) market_td_dk$under_odds else NA_real_,
          fd_line = if (!is.null(market_td_fd)) market_td_fd$line else NA_real_,
          fd_line_over = if (!is.null(market_td_fd)) market_td_fd$line_over else NA_real_,
          fd_line_under = if (!is.null(market_td_fd)) market_td_fd$line_under else NA_real_,
          fd_over_odds = if (!is.null(market_td_fd)) market_td_fd$over_odds else NA_real_,
          fd_under_odds = if (!is.null(market_td_fd)) market_td_fd$under_odds else NA_real_,
          line_source = ifelse(has_market_line, "market", "model"),
          odds_source = ifelse(has_market_odds, "market", ifelse(allow_model_odds, "model", "missing")),
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

  # Ensure optional columns exist for display formatting
  if (!"team" %in% names(props_results)) props_results$team <- NA_character_
  if (!"book" %in% names(props_results)) props_results$book <- NA_character_
  if (!"line" %in% names(props_results)) props_results$line <- NA_real_
  if (!"line_over" %in% names(props_results)) props_results$line_over <- props_results$line
  if (!"line_under" %in% names(props_results)) props_results$line_under <- props_results$line
  if (!"over_odds" %in% names(props_results)) props_results$over_odds <- NA_real_
  if (!"under_odds" %in% names(props_results)) props_results$under_odds <- NA_real_
  if (!"line_source" %in% names(props_results)) props_results$line_source <- NA_character_
  if (!"odds_source" %in% names(props_results)) props_results$odds_source <- NA_character_

  format_line <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    if (!is.finite(num)) return("-")
    if (abs(num - round(num)) < 0.001) return(sprintf("%.0f", num))
    sprintf("%.1f", num)
  }

  format_odds <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    if (!is.finite(num)) return("-")
    if (num >= 0) sprintf("+%d", round(num)) else sprintf("%d", round(num))
  }

  format_market_cell <- function(line_over, line_under, over_odds, under_odds, prop_type = NULL) {
    if (length(line_over) > 1 || length(line_under) > 1 ||
        length(over_odds) > 1 || length(under_odds) > 1 ||
        length(prop_type) > 1) {
      return(mapply(
        function(lo, lu, oo, uu, pt) format_market_cell(lo, lu, oo, uu, pt),
        line_over, line_under, over_odds, under_odds, prop_type,
        SIMPLIFY = TRUE, USE.NAMES = FALSE
      ))
    }

    if (is.list(prop_type)) {
      prop_type <- if (length(prop_type) && !is.null(prop_type[[1]])) {
        as.character(prop_type[[1]])
      } else {
        NA_character_
      }
    }

    coerce_numeric <- function(x) {
      if (is.list(x)) {
        return(vapply(x, function(v) {
          if (is.null(v) || length(v) == 0) return(NA_real_)
          suppressWarnings(as.numeric(v[[1]]))
        }, numeric(1)))
      }
      suppressWarnings(as.numeric(x))
    }

    line_over <- coerce_numeric(line_over)
    line_under <- coerce_numeric(line_under)
    over_odds <- coerce_numeric(over_odds)
    under_odds <- coerce_numeric(under_odds)

    if (!is.null(prop_type) && prop_type == "anytime_td") {
      if (is.finite(over_odds)) {
        implied <- if (over_odds >= 0) {
          100 / (over_odds + 100)
        } else {
          abs(over_odds) / (abs(over_odds) + 100)
        }
        return(sprintf("Yes %s (%.1f%%)", format_odds(over_odds), implied * 100))
      }
      return("-")
    }

    parts <- character(0)
    if (is.finite(line_over) || is.finite(over_odds)) {
      parts <- c(parts, sprintf("O%s (%s)", format_line(line_over), format_odds(over_odds)))
    }
    if (is.finite(line_under) || is.finite(under_odds)) {
      parts <- c(parts, sprintf("U%s (%s)", format_line(line_under), format_odds(under_odds)))
    }
    if (!length(parts)) return("-")
    paste(parts, collapse = " / ")
  }

  format_book <- function(x) {
    if (length(x) > 1) {
      return(vapply(x, format_book, character(1)))
    }
    if (is.list(x)) {
      x <- if (length(x) && !is.null(x[[1]])) as.character(x[[1]]) else NA_character_
    }
    if (is.null(x) || is.na(x) || !nzchar(x)) return("-")
    x
  }

  display_tbl <- props_results %>%
    dplyr::mutate(
      Player = player,
      Position = position,
      Team = dplyr::if_else(!is.na(team), team, "-"),
      `Prop Type` = dplyr::case_when(
        prop_type == "passing_yards" ~ "Passing Yards",
        prop_type == "rushing_yards" ~ "Rushing Yards",
        prop_type == "receiving_yards" ~ "Receiving Yards",
        prop_type == "receptions" ~ "Receptions",
        prop_type == "anytime_td" ~ "Anytime TD",
        TRUE ~ tools::toTitleCase(gsub("_", " ", prop_type))
      ),
      `Market Used` = format_market_cell(line_over, line_under, over_odds, under_odds, prop_type),
      `Book Used` = format_book(book),
      Model = dplyr::case_when(
        prop_type == "anytime_td" ~ sprintf("%.1f%%", projection * 100),
        TRUE ~ sprintf("%.1f", projection)
      ),
      `P(Over/Yes)` = dplyr::if_else(is.na(p_over), "-", sprintf("%.1f%%", p_over * 100)),
      `P(Under/No)` = dplyr::if_else(is.na(p_under), "-", sprintf("%.1f%%", p_under * 100)),
      `EV Over` = dplyr::if_else(is.na(ev_over), "-", sprintf("%+.1f%%", ev_over * 100)),
      `EV Under` = dplyr::if_else(is.na(ev_under), "-", sprintf("%+.1f%%", ev_under * 100)),
      Recommendation = recommendation,
      Source = dplyr::case_when(
        is.na(line_source) & is.na(odds_source) ~ "-",
        TRUE ~ paste0(dplyr::coalesce(line_source, "-"),
                      "/",
                      dplyr::coalesce(odds_source, "-"))
      )
    ) %>%
    dplyr::select(
      Player, Position, Team, `Prop Type`, `Market Used`, `Book Used`,
      Model, `P(Over/Yes)`, `P(Under/No)`, `EV Over`, `EV Under`,
      Recommendation, Source
    )

  # Create gt table
  gt_tbl <- gt::gt(display_tbl) %>%
    gt::tab_header(
      title = sprintf("Player Props Analysis: %s", game_id),
      subtitle = sprintf("Generated: %s | Monte Carlo Trials: %d", Sys.time(), PROP_TRIALS)
    ) %>%
    gt::tab_spanner(
      label = "Market",
      columns = c("Market Used", "Book Used")
    ) %>%
    gt::tab_spanner(
      label = "Model",
      columns = c("Model", "P(Over/Yes)", "P(Under/No)")
    ) %>%
    gt::tab_spanner(
      label = "Value",
      columns = c("EV Over", "EV Under", "Recommendation")
    ) %>%
    gt::tab_spanner(
      label = "Meta",
      columns = c("Source")
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "#22c55e20"),
      locations = gt::cells_body(
        columns = `Recommendation`,
        rows = `Recommendation` %in% c("OVER", "UNDER", "BET")
      )
    ) %>%
    gt::tab_source_note(
      source_note = "Model: Monte Carlo simulation | Normal dist (yards) | Poisson (receptions) | Negative Binomial (TDs)"
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
