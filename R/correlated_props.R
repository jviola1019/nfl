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

# Source prop odds API if available
local({
  api_path <- file.path(getwd(), "R", "prop_odds_api.R")
  if (file.exists(api_path)) source(api_path, local = FALSE)
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
#' @param week Week number (optional)
#' @return Tibble with player projections
#' @keywords internal
load_game_players <- function(home_team, away_team, season, week = NULL) {
  # Source data_sources.R to get fallback function
  data_sources_path <- file.path(getwd(), "sports", "nfl", "props", "data_sources.R")
  if (file.exists(data_sources_path)) {
    source(data_sources_path, local = TRUE)
  }

  # Use fallback-enabled projection loading (handles future games)
  if (exists("get_player_projections_with_fallback", mode = "function")) {
    projections <- tryCatch(
      get_player_projections_with_fallback(
        season = season,
        week = if (!is.null(week)) week else 18,
        home_team = home_team,
        away_team = away_team,
        min_games = 3
      ),
      error = function(e) {
        warning(sprintf("Fallback projection loading failed: %s", e$message))
        NULL
      }
    )

    if (!is.null(projections) && nrow(projections) > 0) {
      message(sprintf("  Loaded %d players for %s @ %s%s",
                      nrow(projections), away_team, home_team,
                      if (any(projections$is_projection)) " (using fallback)" else ""))
      return(projections)
    }
  }

  # Legacy fallback: Try basic load_player_projections
  if (exists("load_player_projections", mode = "function")) {
    projections <- tryCatch(
      load_player_projections(season),
      error = function(e) NULL
    )
    if (!is.null(projections) && nrow(projections) > 0) {
      return(projections %>%
        dplyr::filter(recent_team %in% c(home_team, away_team)) %>%
        dplyr::mutate(is_projection = FALSE, is_baseline = FALSE))
    }
  }

  # Return empty tibble if all loading fails
  warning(sprintf("No player data available for %s @ %s", away_team, home_team))
  tibble::tibble(
    player_id = character(),
    player_name = character(),
    position = character(),
    recent_team = character(),
    avg_passing_yards = numeric(),
    avg_rushing_yards = numeric(),
    avg_receiving_yards = numeric(),
    avg_touchdowns = numeric(),
    is_projection = logical(),
    is_baseline = logical()
  )
}

# =============================================================================
# DEFENSE AND GAME CONTEXT ADJUSTMENTS
# =============================================================================

#' Load defense rankings for a season
#'
#' @param season Season year
#' @return Tibble with team defense multipliers
#' @keywords internal
load_defense_rankings <- function(season) {
  tryCatch({
    # Load play-by-play data for defense rankings (PBP-based approach)
    pbp <- nflreadr::load_pbp(seasons = season)

    if (is.null(pbp) || nrow(pbp) == 0) return(NULL)

    # Aggregate defensive yards allowed by team
    defense_stats <- pbp %>%
      dplyr::filter(!is.na(.data$defteam), !is.na(.data$play_type)) %>%
      dplyr::group_by(.data$defteam) %>%
      dplyr::summarize(
        pass_yards_allowed = sum(.data$passing_yards, na.rm = TRUE),
        rush_yards_allowed = sum(.data$rushing_yards, na.rm = TRUE),
        .groups = "drop"
      )

    # Convert to multipliers (league average = 1.0)
    avg_pass <- mean(defense_stats$pass_yards_allowed, na.rm = TRUE)
    avg_rush <- mean(defense_stats$rush_yards_allowed, na.rm = TRUE)

    defense_stats %>%
      dplyr::transmute(
        team = .data$defteam,
        # Higher yards allowed = easier defense = higher multiplier for opponent
        pass_def_multiplier = dplyr::if_else(
          avg_pass > 0,
          pmin(pmax(.data$pass_yards_allowed / avg_pass, 0.80), 1.20),
          1.0
        ),
        rush_def_multiplier = dplyr::if_else(
          avg_rush > 0,
          pmin(pmax(.data$rush_yards_allowed / avg_rush, 0.75), 1.25),
          1.0
        ),
        recv_def_multiplier = .data$pass_def_multiplier  # Same as pass for receiving
      )
  }, error = function(e) {
    message(sprintf("  Defense rankings unavailable: %s", e$message))
    NULL
  })
}

#' Apply defense adjustments to player projections
#'
#' Multiplies baseline projections by opponent defense efficiency.
#' A higher multiplier means the defense allows more yards.
#'
#' @param players Tibble of player projections
#' @param home_team Home team abbreviation
#' @param away_team Away team abbreviation
#' @param season Season year
#' @return Adjusted player projections
#' @keywords internal
apply_defense_adjustments <- function(players, home_team, away_team, season) {
  # Load defense rankings (with fallback to neutral)
  defense_ranks <- tryCatch({
    load_defense_rankings(season)
  }, error = function(e) NULL)

  if (is.null(defense_ranks) || nrow(defense_ranks) == 0) {
    # Use neutral defense rankings
    defense_ranks <- tibble::tibble(
      team = unique(c(home_team, away_team)),
      pass_def_multiplier = 1.0,
      rush_def_multiplier = 1.0,
      recv_def_multiplier = 1.0
    )
  }

  # Home team faces away defense, away team faces home defense
  players %>%
    dplyr::mutate(
      opponent = dplyr::if_else(recent_team == home_team, away_team, home_team),
      pass_def_mult = defense_ranks$pass_def_multiplier[match(opponent, defense_ranks$team)],
      rush_def_mult = defense_ranks$rush_def_multiplier[match(opponent, defense_ranks$team)],
      recv_def_mult = defense_ranks$recv_def_multiplier[match(opponent, defense_ranks$team)],
      # Apply multipliers (default to 1.0 if missing)
      pass_def_mult = dplyr::coalesce(pass_def_mult, 1.0),
      rush_def_mult = dplyr::coalesce(rush_def_mult, 1.0),
      recv_def_mult = dplyr::coalesce(recv_def_mult, 1.0),
      # Adjust projections
      avg_passing_yards = dplyr::if_else(
        !is.na(avg_passing_yards),
        avg_passing_yards * pass_def_mult,
        avg_passing_yards
      ),
      avg_rushing_yards = dplyr::if_else(
        !is.na(avg_rushing_yards),
        avg_rushing_yards * rush_def_mult,
        avg_rushing_yards
      ),
      avg_receiving_yards = dplyr::if_else(
        !is.na(avg_receiving_yards),
        avg_receiving_yards * recv_def_mult,
        avg_receiving_yards
      )
    ) %>%
    dplyr::select(-opponent, -pass_def_mult, -rush_def_mult, -recv_def_mult)
}

#' Apply game context adjustments (dome, weather, home field)
#'
#' @param players Tibble of player projections
#' @param game Game data with home_team and optional roof column
#' @return Adjusted player projections
#' @keywords internal
apply_game_context <- function(players, game) {
  is_dome <- if (!is.null(game$roof)) {
    tolower(game$roof) %in% c("dome", "closed", "retractable")
  } else {
    FALSE
  }

  home_team <- if (!is.null(game$home_team)) game$home_team else NA_character_

  # Get adjustment constants from config or use defaults
  pass_home_adj <- if (exists("PASSING_YARDS_HOME_ADJ")) PASSING_YARDS_HOME_ADJ else 5
  rush_home_adj <- if (exists("RUSHING_YARDS_HOME_ADJ")) RUSHING_YARDS_HOME_ADJ else 3
  recv_home_adj <- if (exists("RECEIVING_YARDS_HOME_ADJ")) RECEIVING_YARDS_HOME_ADJ else 4
  pass_dome_adj <- if (exists("PASSING_YARDS_DOME_ADJ")) PASSING_YARDS_DOME_ADJ else 12
  recv_dome_adj <- if (exists("RECEIVING_YARDS_DOME_ADJ")) RECEIVING_YARDS_DOME_ADJ else 8

  players %>%
    dplyr::mutate(
      is_home = !is.na(home_team) & recent_team == home_team,
      # Home field advantage
      avg_passing_yards = dplyr::if_else(
        !is.na(avg_passing_yards) & is_home,
        avg_passing_yards + pass_home_adj,
        avg_passing_yards
      ),
      avg_rushing_yards = dplyr::if_else(
        !is.na(avg_rushing_yards) & is_home,
        avg_rushing_yards + rush_home_adj,
        avg_rushing_yards
      ),
      avg_receiving_yards = dplyr::if_else(
        !is.na(avg_receiving_yards) & is_home,
        avg_receiving_yards + recv_home_adj,
        avg_receiving_yards
      ),
      # Dome bonus (for indoor games)
      avg_passing_yards = dplyr::if_else(
        !is.na(avg_passing_yards) & is_dome,
        avg_passing_yards + pass_dome_adj,
        avg_passing_yards
      ),
      avg_receiving_yards = dplyr::if_else(
        !is.na(avg_receiving_yards) & is_dome,
        avg_receiving_yards + recv_dome_adj,
        avg_receiving_yards
      )
    ) %>%
    dplyr::select(-is_home)
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
                           prop_types = c("passing", "rushing", "receiving", "td"),
                           prop_odds_cache = NULL) {

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

  # Load prop odds from API if not provided and enabled
  if (is.null(prop_odds_cache) && exists("USE_REAL_PROP_ODDS") && USE_REAL_PROP_ODDS) {
    prop_odds_cache <- tryCatch({
      if (exists("load_prop_odds", mode = "function")) {
        load_prop_odds()
      } else {
        NULL
      }
    }, error = function(e) {
      message(sprintf("  Prop odds API unavailable: %s", e$message))
      NULL
    })
  }

  # Load player data
  players <- load_game_players(home_team, away_team, season)

  if (nrow(players) == 0) {
    message(sprintf("  No player data for %s @ %s", away_team, home_team))
    return(tibble::tibble())
  }

  # Apply defense adjustments based on opponent strength (skip if fails)
  players <- tryCatch({
    adjusted <- apply_defense_adjustments(players, home_team, away_team, season)
    if (!is.null(adjusted) && nrow(adjusted) > 0) adjusted else players
  }, error = function(e) {
    message(sprintf("  Defense adjustments skipped: %s", e$message))
    players
  })

  # Apply game context adjustments (home field, dome)
  game_context <- list(home_team = home_team, away_team = away_team, roof = NULL)
  players <- tryCatch({
    adjusted <- apply_game_context(players, game_context)
    if (!is.null(adjusted) && nrow(adjusted) > 0) adjusted else players
  }, error = function(e) {
    message(sprintf("  Context adjustments skipped: %s", e$message))
    players
  })

  # Ensure required columns exist with defaults
  if (!"avg_passing_yards" %in% names(players)) players$avg_passing_yards <- NA_real_
  if (!"avg_rushing_yards" %in% names(players)) players$avg_rushing_yards <- NA_real_
  if (!"avg_receiving_yards" %in% names(players)) players$avg_receiving_yards <- NA_real_

  results_list <- list()

  # Process each prop type
  for (prop_type in prop_types) {

    if (prop_type == "passing") {
      qbs <- tryCatch({
        players %>% dplyr::filter(position == "QB", !is.na(avg_passing_yards))
      }, error = function(e) tibble::tibble())

      if (nrow(qbs) > 0) {
        for (i in seq_len(nrow(qbs))) {
          tryCatch({
            qb <- qbs[i, ]
            qb_name <- as.character(qb$player_name[1])
            qb_team <- as.character(qb$recent_team[1])
            qb_passing <- as.numeric(qb$avg_passing_yards[1])

            rho <- get_game_correlation("QB", "passing")
            # Scale SD proportionally to baseline (CV ~0.29 from NFL data)
            baseline_sd <- max(qb_passing * 0.29, 15)

            sim_values <- simulate_correlated_prop(
              baseline = qb_passing,
              sd = baseline_sd,
              z_game = z_game,
              rho = rho,
              n_trials = n_trials,
              distribution = "normal"
            )

            # Model projection is the simulation median
            projection <- round(median(sim_values), 1)

            # Get market line from API or use default (baseline * 0.95)
            market_line <- NULL
            if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
              market_line <- get_market_prop_line(qb_name, "passing_yards", prop_odds_cache)
            }

            if (!is.null(market_line)) {
              line <- market_line$line
              over_odds <- market_line$over_odds
              under_odds <- market_line$under_odds
            } else {
              # Fallback: use baseline * 0.95 (typical market placement)
              line <- round(qb_passing * 0.95, 0) + 0.5
              over_odds <- if (exists("DEFAULT_YARD_PROP_ODDS")) DEFAULT_YARD_PROP_ODDS else -110
              under_odds <- if (exists("DEFAULT_YARD_PROP_ODDS")) DEFAULT_YARD_PROP_ODDS else -110
            }

            # P(Over/Under) based on simulation vs MARKET line
            p_over <- mean(sim_values > line)
            p_under <- mean(sim_values < line)

            # Calculate EV using actual market odds
            dec_over <- if (over_odds >= 0) 1 + over_odds/100 else 1 + 100/abs(over_odds)
            dec_under <- if (under_odds >= 0) 1 + under_odds/100 else 1 + 100/abs(under_odds)
            ev_over <- p_over * (dec_over - 1) - (1 - p_over)
            ev_under <- p_under * (dec_under - 1) - (1 - p_under)

            # Recommendation with model error detection
            model_error <- is_prop_model_error(p_over, p_under, over_odds, under_odds, is_two_sided = TRUE)
            rec <- get_prop_recommendation(ev_over, ev_under, model_error = model_error)

            results_list[[length(results_list) + 1]] <- tibble::tibble(
              player = qb_name,
              position = "QB",
              team = qb_team,
              matchup = paste0(away_team, " @ ", home_team),
              prop_type = "passing_yards",
              line = line,
              projection = projection,
              p_over = round(p_over, 4),
              p_under = round(p_under, 4),
              over_odds = over_odds,
              under_odds = under_odds,
              ev_over = round(ev_over, 4),
              ev_under = round(ev_under, 4),
              recommendation = rec,
              correlation_with_game = rho
            )
          }, error = function(e) {
            message(sprintf("    Skipping QB %d: %s", i, e$message))
          })
        }
      }
    }

    if (prop_type == "rushing") {
      rushers <- tryCatch({
        players %>%
          dplyr::filter(position %in% c("RB", "QB"), !is.na(avg_rushing_yards), avg_rushing_yards > 10)
      }, error = function(e) tibble::tibble())

      if (nrow(rushers) > 0) {
        for (i in seq_len(nrow(rushers))) {
          tryCatch({
            player <- rushers[i, ]
            pl_name <- as.character(player$player_name[1])
            pl_pos <- as.character(player$position[1])
            pl_team <- as.character(player$recent_team[1])
            pl_rushing <- as.numeric(player$avg_rushing_yards[1])

            rho <- get_game_correlation(pl_pos, "rushing")
            # Scale SD proportionally to baseline (CV ~0.40 for rushing)
            baseline_sd <- max(pl_rushing * 0.40, 10)

            sim_values <- simulate_correlated_prop(
              baseline = pl_rushing,
              sd = baseline_sd,
              z_game = z_game,
              rho = rho,
              n_trials = n_trials,
              distribution = "normal"
            )

            # Model projection
            projection <- round(median(sim_values), 1)

            # Get market line from API or use default
            market_line <- NULL
            if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
              market_line <- get_market_prop_line(pl_name, "rushing_yards", prop_odds_cache)
            }

            if (!is.null(market_line)) {
              line <- market_line$line
              over_odds <- market_line$over_odds
              under_odds <- market_line$under_odds
            } else {
              line <- round(pl_rushing * 0.95, 0) + 0.5
              over_odds <- if (exists("DEFAULT_YARD_PROP_ODDS")) DEFAULT_YARD_PROP_ODDS else -110
              under_odds <- if (exists("DEFAULT_YARD_PROP_ODDS")) DEFAULT_YARD_PROP_ODDS else -110
            }

            p_over <- mean(sim_values > line)
            p_under <- mean(sim_values < line)

            dec_over <- if (over_odds >= 0) 1 + over_odds/100 else 1 + 100/abs(over_odds)
            dec_under <- if (under_odds >= 0) 1 + under_odds/100 else 1 + 100/abs(under_odds)
            ev_over <- p_over * (dec_over - 1) - (1 - p_over)
            ev_under <- p_under * (dec_under - 1) - (1 - p_under)

            model_error <- is_prop_model_error(p_over, p_under, over_odds, under_odds, is_two_sided = TRUE)
            rec <- get_prop_recommendation(ev_over, ev_under, model_error = model_error)

            results_list[[length(results_list) + 1]] <- tibble::tibble(
              player = pl_name,
              position = pl_pos,
              team = pl_team,
              matchup = paste0(away_team, " @ ", home_team),
              prop_type = "rushing_yards",
              line = line,
              projection = projection,
              p_over = round(p_over, 4),
              p_under = round(p_under, 4),
              over_odds = over_odds,
              under_odds = under_odds,
              ev_over = round(ev_over, 4),
              ev_under = round(ev_under, 4),
              recommendation = rec,
              correlation_with_game = rho
            )
          }, error = function(e) {
            message(sprintf("    Skipping rusher %d: %s", i, e$message))
          })
        }
      }
    }

    if (prop_type == "receiving") {
      receivers <- tryCatch({
        players %>%
          dplyr::filter(position %in% c("WR", "TE", "RB"), !is.na(avg_receiving_yards), avg_receiving_yards > 10)
      }, error = function(e) tibble::tibble())

      if (nrow(receivers) > 0) {
        for (i in seq_len(nrow(receivers))) {
          tryCatch({
            player <- receivers[i, ]
            pl_name <- as.character(player$player_name[1])
            pl_pos <- as.character(player$position[1])
            pl_team <- as.character(player$recent_team[1])
            pl_receiving <- as.numeric(player$avg_receiving_yards[1])

            rho <- get_game_correlation(pl_pos, "receiving")

            # Scale SD proportionally to baseline (CV ~0.35 for receiving)
            baseline_sd <- max(pl_receiving * 0.35, 8)

            sim_values <- simulate_correlated_prop(
              baseline = pl_receiving,
              sd = baseline_sd,
              z_game = z_game,
              rho = rho,
              n_trials = n_trials,
              distribution = "normal"
            )

            # Model projection
            projection <- round(median(sim_values), 1)

            # Get market line from API or use default
            market_line <- NULL
            if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
              market_line <- get_market_prop_line(pl_name, "receiving_yards", prop_odds_cache)
            }

            if (!is.null(market_line)) {
              line <- market_line$line
              over_odds <- market_line$over_odds
              under_odds <- market_line$under_odds
            } else {
              line <- round(pl_receiving * 0.95, 0) + 0.5
              over_odds <- if (exists("DEFAULT_YARD_PROP_ODDS")) DEFAULT_YARD_PROP_ODDS else -110
              under_odds <- if (exists("DEFAULT_YARD_PROP_ODDS")) DEFAULT_YARD_PROP_ODDS else -110
            }

            p_over <- mean(sim_values > line)
            p_under <- mean(sim_values < line)

            dec_over <- if (over_odds >= 0) 1 + over_odds/100 else 1 + 100/abs(over_odds)
            dec_under <- if (under_odds >= 0) 1 + under_odds/100 else 1 + 100/abs(under_odds)
            ev_over <- p_over * (dec_over - 1) - (1 - p_over)
            ev_under <- p_under * (dec_under - 1) - (1 - p_under)

            model_error <- is_prop_model_error(p_over, p_under, over_odds, under_odds, is_two_sided = TRUE)
            rec <- get_prop_recommendation(ev_over, ev_under, model_error = model_error)

            results_list[[length(results_list) + 1]] <- tibble::tibble(
              player = pl_name,
              position = pl_pos,
              team = pl_team,
              matchup = paste0(away_team, " @ ", home_team),
              prop_type = "receiving_yards",
              line = line,
              projection = projection,
              p_over = round(p_over, 4),
              p_under = round(p_under, 4),
              over_odds = over_odds,
              under_odds = under_odds,
              ev_over = round(ev_over, 4),
              ev_under = round(ev_under, 4),
              recommendation = rec,
              correlation_with_game = rho
            )
          }, error = function(e) {
            message(sprintf("    Skipping receiver %d: %s", i, e$message))
          })
        }
      }
    }

    if (prop_type == "td") {
      skill_players <- tryCatch({
        players %>%
          dplyr::filter(position %in% c("QB", "RB", "WR", "TE"), !is.na(avg_touchdowns))
      }, error = function(e) tibble::tibble())

      if (nrow(skill_players) > 0) {
        for (i in seq_len(nrow(skill_players))) {
          tryCatch({
            player <- skill_players[i, ]
            pl_name <- as.character(player$player_name[1])
            pl_pos <- as.character(player$position[1])
            pl_team <- as.character(player$recent_team[1])
            # Use scoring TDs (rush+recv) for anytime TD, fall back to avg_touchdowns
            pl_td <- if ("avg_scoring_tds" %in% names(player) && !is.na(player$avg_scoring_tds[1])) {
              as.numeric(player$avg_scoring_tds[1])
            } else {
              as.numeric(player$avg_touchdowns[1])
            }

            # Position-specific defaults for scoring TDs
            if (is.na(pl_td) || pl_td <= 0) {
              pl_td <- switch(pl_pos,
                QB = 0.15,   # ~15% of games QB rushes for TD
                RB = 0.40,   # ~40% of games RB scores
                WR = 0.30,   # ~30% of games WR scores
                TE = 0.20,   # ~20% of games TE scores
                0.25
              )
            }

            rho <- get_game_correlation(pl_pos, "td")

            sim_values <- simulate_correlated_prop(
              baseline = pl_td,
              sd = pl_td * 0.5,  # High variance for TDs
              z_game = z_game,
              rho = rho,
              n_trials = n_trials,
              distribution = "negbin"
            )

            p_anytime <- mean(sim_values >= 1)

            # Get market odds for anytime TD from API or use position-based defaults
            market_td <- NULL
            if (!is.null(prop_odds_cache) && exists("get_market_prop_line", mode = "function")) {
              market_td <- get_market_prop_line(pl_name, "anytime_td", prop_odds_cache)
            }

            if (!is.null(market_td)) {
              anytime_odds <- market_td$over_odds  # Yes odds for TD
            } else if (exists("DEFAULT_TD_ODDS") && pl_pos %in% names(DEFAULT_TD_ODDS)) {
              anytime_odds <- DEFAULT_TD_ODDS[[pl_pos]]
            } else {
              # Position-based fallback
              anytime_odds <- switch(pl_pos,
                QB = 350,    # QBs score ~15% of games
                RB = -110,   # RB1s score ~45% of games
                WR = 140,    # WR1s score ~35% of games
                TE = 200,    # TE1s score ~25% of games
                200
              )
            }

            # Convert to decimal
            dec_odds <- if (anytime_odds >= 0) 1 + anytime_odds/100 else 1 + 100/abs(anytime_odds)
            ev_anytime <- p_anytime * (dec_odds - 1) - (1 - p_anytime)

            model_error <- is_prop_model_error(p_anytime, 1 - p_anytime, anytime_odds, NA_real_, is_two_sided = FALSE)
            rec <- if (isTRUE(model_error)) "MODEL ERROR" else if (abs(ev_anytime) > PROP_EDGE_BIN_HIGH_MAX) "PASS" else if (ev_anytime > PROP_MIN_BET_EDGE) "BET" else "PASS"

            results_list[[length(results_list) + 1]] <- tibble::tibble(
              player = pl_name,
              position = pl_pos,
              team = pl_team,
              matchup = paste0(away_team, " @ ", home_team),
              prop_type = "anytime_td",
              line = NA_real_,
              projection = round(p_anytime, 3),  # Probability of 1+ TD (not expected count)
              p_over = round(p_anytime, 4),
              p_under = round(1 - p_anytime, 4),
              over_odds = anytime_odds,
              under_odds = NA_real_,  # TD props don't have "No" odds typically
              ev_over = round(ev_anytime, 4),
              ev_under = NA_real_,
              recommendation = rec,
              correlation_with_game = rho
            )
          }, error = function(e) {
            message(sprintf("    Skipping TD player %d: %s", i, e$message))
          })
        }
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
    # Note: TD props naturally have higher EV variance due to long odds, so use different thresholds
    # For TD props, ev_over contains the ev_anytime value (set at line 836)
    results <- results %>%
      dplyr::mutate(
        governance_ev = dplyr::case_when(
          recommendation == "OVER" ~ ev_over,
          recommendation == "UNDER" ~ ev_under,
          recommendation == "BET" ~ ev_over,
          TRUE ~ dplyr::coalesce(ev_over, ev_under)
        ),
        governance_prob = dplyr::case_when(
          recommendation == "OVER" ~ p_over,
          recommendation == "UNDER" ~ p_under,
          recommendation == "BET" ~ p_over,
          TRUE ~ p_over
        ),
        governance_odds = dplyr::case_when(
          recommendation == "OVER" ~ over_odds,
          recommendation == "UNDER" ~ under_odds,
          recommendation == "BET" ~ over_odds,
          TRUE ~ over_odds
        ),
        governance = purrr::pmap(
          list(
            ev = governance_ev,
            prob = governance_prob,
            odds = governance_odds,
            is_placeholder_odds = is.na(governance_odds) | governance_odds == 0
          ),
          ~ apply_bet_governance(
            ev = ..1,
            prob = ..2,
            odds = ..3,
            min_stake = 0.01,
            kelly_fraction = 0.125,
            max_stake = 0.02,
            is_placeholder_odds = ..4
          )
        ),
        `Raw Kelly (%)` = purrr::map_dbl(governance, ~ .x$raw_kelly_pct[[1]]),
        `Capped Stake (%)` = purrr::map_dbl(governance, ~ .x$capped_stake_pct[[1]]),
        `Final Stake (%)` = purrr::map_dbl(governance, ~ .x$final_stake_pct[[1]]),
        `Pass Reason` = purrr::map_chr(governance, ~ .x$pass_reason[[1]]),
        # Get the relevant EV for classification (ev_over for both yard props and TDs)
        .ev_for_quality = dplyr::coalesce(pmax(ev_over, ev_under, na.rm = TRUE), ev_over),
        edge_quality = dplyr::case_when(
          recommendation == "PASS" ~ "Pass",
          TRUE ~ classify_prop_edge_quality(ev_over, ev_under, recommendation)
        ),
        # Document why positive EV can still show PASS (2% minimum edge threshold)
        edge_note = dplyr::case_when(
          recommendation == "PASS" & !is.na(.ev_for_quality) & .ev_for_quality > 0 & .ev_for_quality <= PROP_MIN_BET_EDGE ~ "Edge < 2%",
          recommendation == "PASS" & !is.na(.ev_for_quality) & .ev_for_quality > PROP_EDGE_BIN_HIGH_MAX ~ "Blocked (>10%)",
          recommendation == "PASS" & !is.na(.ev_for_quality) & .ev_for_quality <= 0 ~ "Negative EV",
          TRUE ~ ""
        )
      ) %>%
      dplyr::select(-.ev_for_quality, -governance_ev, -governance_prob, -governance_odds, -governance)  # Remove helper columns

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
