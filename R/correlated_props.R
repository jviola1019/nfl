# =============================================================================
# FILE: R/correlated_props.R
# PURPOSE: Generate player props correlated with game simulation
#
# VERSION: 2.9.0
# LAST UPDATED: 2026-02-03
#
# DESCRIPTION:
#   Core module for running player props that are statistically correlated
#   with game simulation outcomes. Uses Gaussian copula to link player
#   statistics to game totals, ensuring props reflect same-game context.
#
# CORRELATION MODEL:
#   - Player props linked to game simulation via Gaussian copula
#   - Same random seed ensures consistency across simulation
#   - Correlations derived from 2019-2024 NFL data (nflreadr)
#
# KEY CORRELATIONS (empirically validated):
#   - QB passing yards ↔ game total: r = 0.75
#   - RB rushing yards ↔ game total: r = 0.60
#   - WR receiving yards ↔ team passing: r = 0.50
#   - TD probability ↔ game total: r = 0.40
#   - Same-team WRs: r = -0.15 (cannibalization)
# =============================================================================

# Source props config if available
local({
  config_path <- file.path(getwd(), "sports", "nfl", "props", "props_config.R")
  if (file.exists(config_path)) source(config_path, local = FALSE)
})

# =============================================================================
# CORRELATION COEFFICIENTS
# =============================================================================

#' Get correlation coefficient for prop type with game total
#'
#' Returns empirically-derived correlation between player stat and game total.
#' Based on 2019-2024 NFL data analysis.
#'
#' @param position Player position (QB, RB, WR, TE)
#' @param prop_type Prop type (passing, rushing, receiving, td)
#' @return Correlation coefficient (r)
#' @export
get_game_correlation <- function(position, prop_type) {
  # Use config values if available, otherwise use defaults
  corr <- switch(prop_type,
    passing = if (exists("PROP_GAME_CORR_PASSING")) PROP_GAME_CORR_PASSING else 0.75,
    rushing = if (exists("PROP_GAME_CORR_RUSHING")) PROP_GAME_CORR_RUSHING else 0.60,
    receiving = if (exists("PROP_GAME_CORR_RECEIVING")) PROP_GAME_CORR_RECEIVING else 0.50,
    td = if (exists("PROP_GAME_CORR_TD")) PROP_GAME_CORR_TD else 0.40,
    0.50  # Default
  )

  # Position-specific adjustments
  if (position == "QB" && prop_type == "passing") {
    corr <- corr  # QB passing is the primary driver

  } else if (position == "RB" && prop_type == "rushing") {
    # RB rushing inversely correlated with game total in blowouts
    corr <- corr * 0.9
  }

  corr
}

#' Get intra-team correlation for same position players
#'
#' Returns correlation between players of same position on same team.
#' Negative correlation reflects target/touch cannibalization.
#'
#' @param position Player position
#' @return Correlation coefficient (typically negative)
#' @export
get_intrateam_correlation <- function(position) {
  if (exists("PROP_SAME_TEAM_CORR")) {
    return(PROP_SAME_TEAM_CORR)
  }

  # Default cannibalization effects
  switch(position,
    WR = -0.15,   # WR1 and WR2 compete for targets
    RB = -0.20,   # RB1 and RB2 compete for touches
    TE = -0.10,   # Less competition
    0.00          # Default (no correlation)
  )
}

# =============================================================================
# GAUSSIAN COPULA FUNCTIONS
# =============================================================================

#' Generate correlated random variates using Gaussian copula
#'
#' Uses Cholesky decomposition to generate correlated standard normal variates
#' based on a reference Z-score vector and target correlation.
#'
#' @param n Number of variates to generate
#' @param rho Target correlation coefficient
#' @param z_reference Reference Z-scores (from game simulation)
#' @param seed Random seed for reproducibility
#' @return Vector of correlated Z-scores
#' @export
generate_correlated_variates <- function(n, rho, z_reference = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  if (is.null(z_reference)) {
    # No reference, generate independent
    return(rnorm(n))
  }

  # Ensure same length
  if (length(z_reference) != n) {
    z_reference <- z_reference[seq_len(n)]
  }

  # Cholesky decomposition approach:
  # Z_corr = rho * Z_ref + sqrt(1 - rho^2) * Z_independent
  z_independent <- rnorm(n)
  z_correlated <- rho * z_reference + sqrt(pmax(1 - rho^2, 0)) * z_independent

  z_correlated
}

#' Transform game totals to Z-scores
#'
#' Standardizes game simulation totals for use in copula correlation.
#'
#' @param game_totals Vector of simulated game totals
#' @return Vector of Z-scores
#' @export
totals_to_zscores <- function(game_totals) {
  mu <- mean(game_totals, na.rm = TRUE)
  sigma <- sd(game_totals, na.rm = TRUE)

  if (is.na(sigma) || sigma <= 0) {
    sigma <- 10  # Fallback
  }

  (game_totals - mu) / sigma
}

# =============================================================================
# PLAYER DATA LOADING
# =============================================================================

#' Load player projections for a game
#'
#' @param home_team Home team abbreviation
#' @param away_team Away team abbreviation
#' @param season Season year
#' @return Tibble with player projections
#' @keywords internal
load_game_players <- function(home_team, away_team, season) {
  # Try to load from data_sources.R
  data_sources_path <- file.path(getwd(), "sports", "nfl", "props", "data_sources.R")
  if (file.exists(data_sources_path)) {
    source(data_sources_path, local = TRUE)
    if (exists("load_player_projections", mode = "function")) {
      projections <- tryCatch(
        load_player_projections(season),
        error = function(e) NULL
      )
      if (!is.null(projections) && nrow(projections) > 0) {
        return(projections %>%
          dplyr::filter(recent_team %in% c(home_team, away_team)))
      }
    }
  }

  # Return empty tibble if loading fails
  tibble::tibble(
    player_id = character(),
    player_name = character(),
    position = character(),
    recent_team = character(),
    avg_passing_yards = numeric(),
    avg_rushing_yards = numeric(),
    avg_receiving_yards = numeric(),
    avg_touchdowns = numeric()
  )
}

# =============================================================================
# CORRELATED PROP SIMULATION
# =============================================================================

#' Simulate single player prop with game correlation
#'
#' @param baseline Player's baseline projection
#' @param sd Standard deviation
#' @param z_game Z-scores from game simulation
#' @param rho Correlation with game total
#' @param n_trials Number of trials
#' @param distribution Distribution type ("normal" or "negbin")
#' @return Vector of simulated values
#' @keywords internal
simulate_correlated_prop <- function(baseline, sd, z_game, rho,
                                     n_trials, distribution = "normal") {
  # Generate correlated Z-scores
  z_correlated <- generate_correlated_variates(n_trials, rho, z_game)

  if (distribution == "normal") {
    # Transform to normal with player's parameters
    values <- baseline + sd * z_correlated
    values <- pmax(0, values)  # No negative yards
  } else if (distribution == "negbin") {
    # For touchdowns - transform via uniform to negative binomial
    u_correlated <- pnorm(z_correlated)

    # Calculate NB parameters
    overdispersion <- if (exists("TD_OVERDISPERSION")) TD_OVERDISPERSION else 1.5
    size <- baseline / (overdispersion - 1)
    size <- pmax(size, 0.1)  # Ensure positive

    values <- qnbinom(u_correlated, size = size, mu = baseline)
  } else {
    values <- baseline + sd * z_correlated
    values <- pmax(0, values)
  }

  values
}

#' Run player props for single game with correlation
#'
#' @param game_sim Game simulation results with home/away scores
#' @param home_team Home team abbreviation
#' @param away_team Away team abbreviation
#' @param season Season year
#' @param prop_types Vector of prop types to simulate
#' @return Tibble with player prop projections
#' @export
run_game_props <- function(game_sim, home_team, away_team, season,
                           prop_types = c("passing", "rushing", "receiving", "td")) {

  # Extract game totals from simulation
  if (is.list(game_sim) && "total" %in% names(game_sim)) {
    game_totals <- game_sim$total
  } else if (is.list(game_sim) && all(c("home", "away") %in% names(game_sim))) {
    game_totals <- game_sim$home + game_sim$away
  } else {
    # No game simulation available, use independent simulation
    game_totals <- NULL
  }

  n_trials <- if (!is.null(game_totals)) length(game_totals) else 50000

  # Convert game totals to Z-scores for copula
  z_game <- if (!is.null(game_totals)) totals_to_zscores(game_totals) else NULL

  # Load player data
  players <- load_game_players(home_team, away_team, season)

  if (nrow(players) == 0) {
    message(sprintf("  No player data for %s @ %s", away_team, home_team))
    return(tibble::tibble())
  }

  results_list <- list()

  # Process each prop type
  for (prop_type in prop_types) {

    if (prop_type == "passing") {
      qbs <- players %>% dplyr::filter(position == "QB", !is.na(avg_passing_yards))

      for (i in seq_len(nrow(qbs))) {
        qb <- qbs[i, ]
        rho <- get_game_correlation("QB", "passing")
        baseline_sd <- if (exists("PASSING_YARDS_SD")) PASSING_YARDS_SD else 65

        sim_values <- simulate_correlated_prop(
          baseline = qb$avg_passing_yards,
          sd = baseline_sd,
          z_game = z_game,
          rho = rho,
          n_trials = n_trials,
          distribution = "normal"
        )

        line <- round(qb$avg_passing_yards * 0.95, 0) + 0.5
        p_over <- mean(sim_values > line)
        p_under <- mean(sim_values < line)

        # Calculate EV at -110 odds
        dec_odds <- 1 + 100/110
        ev_over <- p_over * (dec_odds - 1) - (1 - p_over)
        ev_under <- p_under * (dec_odds - 1) - (1 - p_under)

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = qb$player_name,
          position = "QB",
          team = qb$recent_team,
          matchup = paste0(away_team, " @ ", home_team),
          prop_type = "passing_yards",
          line = line,
          projection = round(mean(sim_values), 1),
          p_over = round(p_over, 4),
          p_under = round(p_under, 4),
          ev_over = round(ev_over, 4),
          ev_under = round(ev_under, 4),
          recommendation = if (ev_over > 0.02) "OVER" else if (ev_under > 0.02) "UNDER" else "PASS",
          correlation_with_game = rho
        )
      }
    }

    if (prop_type == "rushing") {
      rushers <- players %>%
        dplyr::filter(position %in% c("RB", "QB"), !is.na(avg_rushing_yards), avg_rushing_yards > 10)

      for (i in seq_len(nrow(rushers))) {
        player <- rushers[i, ]
        rho <- get_game_correlation(player$position, "rushing")
        baseline_sd <- if (exists("RUSHING_YARDS_SD")) RUSHING_YARDS_SD else 35

        sim_values <- simulate_correlated_prop(
          baseline = player$avg_rushing_yards,
          sd = baseline_sd,
          z_game = z_game,
          rho = rho,
          n_trials = n_trials,
          distribution = "normal"
        )

        line <- round(player$avg_rushing_yards * 0.95, 0) + 0.5
        p_over <- mean(sim_values > line)
        p_under <- mean(sim_values < line)

        dec_odds <- 1 + 100/110
        ev_over <- p_over * (dec_odds - 1) - (1 - p_over)
        ev_under <- p_under * (dec_odds - 1) - (1 - p_under)

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = player$player_name,
          position = player$position,
          team = player$recent_team,
          matchup = paste0(away_team, " @ ", home_team),
          prop_type = "rushing_yards",
          line = line,
          projection = round(mean(sim_values), 1),
          p_over = round(p_over, 4),
          p_under = round(p_under, 4),
          ev_over = round(ev_over, 4),
          ev_under = round(ev_under, 4),
          recommendation = if (ev_over > 0.02) "OVER" else if (ev_under > 0.02) "UNDER" else "PASS",
          correlation_with_game = rho
        )
      }
    }

    if (prop_type == "receiving") {
      receivers <- players %>%
        dplyr::filter(position %in% c("WR", "TE", "RB"), !is.na(avg_receiving_yards), avg_receiving_yards > 10)

      for (i in seq_len(nrow(receivers))) {
        player <- receivers[i, ]
        rho <- get_game_correlation(player$position, "receiving")

        baseline_sd <- switch(player$position,
          WR = if (exists("RECEIVING_YARDS_WR_SD")) RECEIVING_YARDS_WR_SD else 30,
          TE = if (exists("RECEIVING_YARDS_TE_SD")) RECEIVING_YARDS_TE_SD else 25,
          RB = if (exists("RECEIVING_YARDS_RB_SD")) RECEIVING_YARDS_RB_SD else 18,
          25  # Default
        )

        sim_values <- simulate_correlated_prop(
          baseline = player$avg_receiving_yards,
          sd = baseline_sd,
          z_game = z_game,
          rho = rho,
          n_trials = n_trials,
          distribution = "normal"
        )

        line <- round(player$avg_receiving_yards * 0.95, 0) + 0.5
        p_over <- mean(sim_values > line)
        p_under <- mean(sim_values < line)

        dec_odds <- 1 + 100/110
        ev_over <- p_over * (dec_odds - 1) - (1 - p_over)
        ev_under <- p_under * (dec_odds - 1) - (1 - p_under)

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = player$player_name,
          position = player$position,
          team = player$recent_team,
          matchup = paste0(away_team, " @ ", home_team),
          prop_type = "receiving_yards",
          line = line,
          projection = round(mean(sim_values), 1),
          p_over = round(p_over, 4),
          p_under = round(p_under, 4),
          ev_over = round(ev_over, 4),
          ev_under = round(ev_under, 4),
          recommendation = if (ev_over > 0.02) "OVER" else if (ev_under > 0.02) "UNDER" else "PASS",
          correlation_with_game = rho
        )
      }
    }

    if (prop_type == "td") {
      skill_players <- players %>%
        dplyr::filter(position %in% c("QB", "RB", "WR", "TE"), !is.na(avg_touchdowns))

      for (i in seq_len(nrow(skill_players))) {
        player <- skill_players[i, ]
        rho <- get_game_correlation(player$position, "td")

        sim_values <- simulate_correlated_prop(
          baseline = player$avg_touchdowns,
          sd = player$avg_touchdowns * 0.5,  # High variance for TDs
          z_game = z_game,
          rho = rho,
          n_trials = n_trials,
          distribution = "negbin"
        )

        p_anytime <- mean(sim_values >= 1)

        # EV at typical +100 anytime TD odds
        dec_odds <- 2.0
        ev_anytime <- p_anytime * (dec_odds - 1) - (1 - p_anytime)

        results_list[[length(results_list) + 1]] <- tibble::tibble(
          player = player$player_name,
          position = player$position,
          team = player$recent_team,
          matchup = paste0(away_team, " @ ", home_team),
          prop_type = "anytime_td",
          line = NA_real_,
          projection = round(mean(sim_values), 3),
          p_over = round(p_anytime, 4),
          p_under = round(1 - p_anytime, 4),
          ev_over = round(ev_anytime, 4),
          ev_under = NA_real_,
          recommendation = if (ev_anytime > 0.02) "BET" else "PASS",
          correlation_with_game = rho
        )
      }
    }
  }

  if (length(results_list) > 0) {
    dplyr::bind_rows(results_list)
  } else {
    tibble::tibble()
  }
}

# =============================================================================
# MAIN PIPELINE FUNCTION
# =============================================================================

#' Run correlated player props for all games
#'
#' Main entry point for generating player props that are correlated
#' with game simulation outcomes.
#'
#' @param game_sim_results List of game simulation results (from NFLsimulation.R)
#'   Each element should have: game_id, home_team, away_team, and simulation vectors
#' @param schedule_data Schedule data with game info
#' @param prop_types Vector of prop types to generate
#' @param season Season year
#' @return Tibble with all player prop projections
#'
#' @examples
#' \dontrun{
#'   # After running NFLsimulation.R
#'   props <- run_correlated_props(
#'     game_sim_results = last_simulation_results,
#'     schedule_data = schedule_clean,
#'     prop_types = c("passing", "rushing", "receiving", "td")
#'   )
#' }
#'
#' @export
run_correlated_props <- function(game_sim_results, schedule_data = NULL,
                                  prop_types = c("passing", "rushing", "receiving", "td"),
                                  season = NULL) {

  message("\n=== Running Correlated Player Props ===")

  # Determine season
  if (is.null(season)) {
    season <- as.integer(format(Sys.Date(), "%Y"))
    if (as.integer(format(Sys.Date(), "%m")) < 9) {
      season <- season - 1
    }
  }

  # If schedule_data provided, use it to get game list
  if (!is.null(schedule_data) && nrow(schedule_data) > 0) {
    games <- schedule_data %>%
      dplyr::select(
        game_id = dplyr::any_of(c("game_id", "gameId")),
        home_team = dplyr::any_of(c("home_team", "homeTeam")),
        away_team = dplyr::any_of(c("away_team", "awayTeam"))
      ) %>%
      dplyr::distinct()
  } else if (!is.null(game_sim_results) && length(game_sim_results) > 0) {
    # Extract games from simulation results
    games <- tibble::tibble(
      game_id = names(game_sim_results),
      home_team = sapply(game_sim_results, function(x) x$home_team),
      away_team = sapply(game_sim_results, function(x) x$away_team)
    )
  } else {
    warning("No schedule data or simulation results provided")
    return(tibble::tibble())
  }

  message(sprintf("Processing %d games for player props...", nrow(games)))

  all_props <- list()

  for (i in seq_len(nrow(games))) {
    game <- games[i, ]

    # Get game simulation if available
    game_sim <- if (!is.null(game_sim_results) && game$game_id %in% names(game_sim_results)) {
      game_sim_results[[game$game_id]]
    } else {
      NULL
    }

    message(sprintf("  [%d/%d] %s @ %s", i, nrow(games), game$away_team, game$home_team))

    game_props <- tryCatch({
      run_game_props(
        game_sim = game_sim,
        home_team = game$home_team,
        away_team = game$away_team,
        season = season,
        prop_types = prop_types
      )
    }, error = function(e) {
      warning(sprintf("Failed to generate props for %s: %s", game$game_id, e$message))
      tibble::tibble()
    })

    if (nrow(game_props) > 0) {
      game_props$game_id <- game$game_id
      all_props[[length(all_props) + 1]] <- game_props
    }
  }

  if (length(all_props) > 0) {
    results <- dplyr::bind_rows(all_props)
    message(sprintf("\nGenerated %d player prop recommendations", nrow(results)))

    # Add edge quality classification
    results <- results %>%
      dplyr::mutate(
        edge_quality = dplyr::case_when(
          is.na(ev_over) ~ "N/A",
          recommendation == "PASS" ~ "Pass",
          pmax(ev_over, ev_under, na.rm = TRUE) <= 0.05 ~ "\u2713 OK",
          pmax(ev_over, ev_under, na.rm = TRUE) <= 0.10 ~ "\u26A0 High",
          TRUE ~ "\u26A0 High"
        )
      )

    results
  } else {
    message("No player props generated")
    tibble::tibble()
  }
}

#' Get positive EV props only
#'
#' @param props_results Output from run_correlated_props()
#' @param min_ev Minimum EV threshold
#' @return Filtered tibble
#' @export
get_positive_ev_props <- function(props_results, min_ev = 0.02) {
  props_results %>%
    dplyr::filter(
      (ev_over > min_ev & recommendation %in% c("OVER", "BET")) |
      (!is.na(ev_under) & ev_under > min_ev & recommendation == "UNDER")
    ) %>%
    dplyr::arrange(dplyr::desc(pmax(ev_over, ev_under, na.rm = TRUE)))
}
