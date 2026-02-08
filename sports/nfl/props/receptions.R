# =============================================================================
# FILE: sports/nfl/props/receptions.R
# PURPOSE: NFL player receptions prop simulation
#
# VERSION: 1.0.0
# LAST UPDATED: 2026-02-07
#
# DESCRIPTION:
#   Simulates receptions distributions and calculates over/under
#   probabilities for player prop betting (WR, TE, RB).
# =============================================================================

# Provide local fallback for %||% when sourced directly
if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 ||
                                 (length(x) == 1 && is.na(x))) y else x
}

# Source configuration
local({
  config_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "props_config.R")
  if (file.exists(config_path)) source(config_path)
})

#' Simulate player receptions
#'
#' @param player_receptions Player's baseline receptions per game
#' @param position Player position ("WR", "TE", or "RB")
#' @param opponent_def_rank Opponent pass defense rank (1-32, 1 = best)
#' @param is_home Boolean: is player on home team?
#' @param is_dome Boolean: is game indoors?
#' @param game_script Expected point differential (negative = trailing)
#' @param n_trials Number of simulation trials
#'
#' @return List with projection, sd, and simulated receptions
#'
#' @export
simulate_receptions <- function(
  player_receptions = NULL,
  position = "WR",
  opponent_def_rank = 16,
  is_home = FALSE,
  is_dome = FALSE,
  game_script = 0,
  n_trials = PROP_TRIALS
) {

  baseline <- if (is.null(player_receptions)) {
    switch(position,
      WR = RECEPTIONS_WR_BASELINE,
      TE = RECEPTIONS_TE_BASELINE,
      RB = RECEPTIONS_RB_BASELINE,
      RECEPTIONS_WR_BASELINE
    )
  } else {
    player_receptions
  }

  base_sd <- switch(position,
    WR = RECEPTIONS_WR_SD,
    TE = RECEPTIONS_TE_SD,
    RB = RECEPTIONS_RB_SD,
    RECEPTIONS_WR_SD
  )

  projection <- baseline

  if (is_home) projection <- projection + RECEPTIONS_HOME_ADJ
  if (is_dome) projection <- projection + RECEPTIONS_DOME_ADJ

  # Opponent defense adjustment (passing defense)
  def_mult_range <- PASSING_DEF_MULTIPLIER_RANGE
  def_multiplier <- def_mult_range[1] + (opponent_def_rank - 1) / 31 * diff(def_mult_range)
  projection <- projection * def_multiplier

  # Game script adjustment (trailing -> more targets)
  if (game_script < 0) {
    projection <- projection * (1 + abs(game_script) * RECEPTIONS_GAMESCRIPT_COEF / 10)
  }

  projection <- max(0.5, projection)

  # Distribution for counts
  if (exists("PROP_DISTRIBUTION_COUNT") && PROP_DISTRIBUTION_COUNT == "negbin") {
    overdispersion <- if (exists("RECEPTIONS_OVERDISPERSION")) RECEPTIONS_OVERDISPERSION else 1.15
    if (overdispersion <= 1) {
      simulated <- rpois(n_trials, lambda = projection)
    } else {
      size <- projection / (overdispersion - 1)
      simulated <- rnbinom(n_trials, size = size, mu = projection)
    }
  } else {
    simulated <- rpois(n_trials, lambda = projection)
  }

  list(
    projection = round(projection, 2),
    sd = round(base_sd, 2),
    simulated_receptions = simulated
  )
}

#' Calculate over/under probabilities for receptions
#'
#' @param simulation Output from simulate_receptions()
#' @param line Market line (e.g., 5.5)
#' @param over_odds American odds for over (NA -> derived from simulation)
#' @param under_odds American odds for under (NA -> derived from simulation)
#' @param line_over Optional line for over side
#' @param line_under Optional line for under side
#'
#' @return List with probabilities and expected values
#'
#' @export
receptions_over_under <- function(simulation,
                                  line = NA_real_,
                                  over_odds = NA_real_,
                                  under_odds = NA_real_,
                                  line_over = NA_real_,
                                  line_under = NA_real_) {
  recs <- simulation$simulated_receptions
  allow_model_lines <- isTRUE(if (exists("PROP_ALLOW_MODEL_LINES")) PROP_ALLOW_MODEL_LINES else TRUE)
  allow_model_odds <- isTRUE(if (exists("PROP_ALLOW_MODEL_ODDS")) PROP_ALLOW_MODEL_ODDS else FALSE)

  # Derive fallback line from simulation when missing
  if (!is.finite(line) && allow_model_lines) {
    if (exists("derive_prop_market_from_sim", mode = "function")) {
      derived <- derive_prop_market_from_sim(
        recs,
        line_quantile = if (exists("PROP_FALLBACK_LINE_QUANTILE")) PROP_FALLBACK_LINE_QUANTILE else 0.50,
        vig = if (exists("PROP_MARKET_VIG")) PROP_MARKET_VIG else 0.045
      )
      if (!is.finite(line)) line <- derived$line
    }
  }

  if (!is.finite(line_over)) line_over <- line
  if (!is.finite(line_under)) line_under <- line

  # Calculate probabilities using side-specific lines
  p_over <- mean(recs > line_over)
  p_under <- mean(recs < line_under)
  p_push_over <- mean(recs == line_over)
  p_push_under <- mean(recs == line_under)
  p_push <- if (is.finite(line_over) && is.finite(line_under) &&
                abs(line_over - line_under) <= 0.001) {
    p_push_over
  } else {
    NA_real_
  }

  # Derive fallback odds from probabilities only when explicitly allowed
  if (allow_model_odds && (!is.finite(over_odds) || !is.finite(under_odds))) {
    if (exists("derive_two_way_odds_from_probs", mode = "function")) {
      derived_odds <- derive_two_way_odds_from_probs(
        p_over,
        p_under,
        vig = if (exists("PROP_MARKET_VIG")) PROP_MARKET_VIG else 0.045
      )
      if (!is.finite(over_odds)) over_odds <- derived_odds$over_odds
      if (!is.finite(under_odds)) under_odds <- derived_odds$under_odds
    }
  }

  # Convert odds to decimal
  over_dec <- if (is.finite(over_odds)) {
    if (over_odds >= 0) 1 + over_odds/100 else 1 + 100/abs(over_odds)
  } else {
    NA_real_
  }
  under_dec <- if (is.finite(under_odds)) {
    if (under_odds >= 0) 1 + under_odds/100 else 1 + 100/abs(under_odds)
  } else {
    NA_real_
  }

  # Calculate EV (push-aware, line-specific)
  ev_over <- if (is.finite(over_dec)) {
    p_over * (over_dec - 1) - (1 - p_over - p_push_over)
  } else {
    NA_real_
  }
  ev_under <- if (is.finite(under_dec)) {
    p_under * (under_dec - 1) - (1 - p_under - p_push_under)
  } else {
    NA_real_
  }

  list(
    line = line,
    line_over = line_over,
    line_under = line_under,
    projection = simulation$projection,
    p_over = round(p_over, 4),
    p_under = round(p_under, 4),
    p_push = round(p_push, 4),
    ev_over = round(ev_over, 4),
    ev_under = round(ev_under, 4),
    recommendation = if (is.finite(ev_over) && ev_over > 0.02) {
      "OVER"
    } else if (is.finite(ev_under) && ev_under > 0.02) {
      "UNDER"
    } else {
      "PASS"
    }
  )
}

#' Full receptions prop analysis
#'
#' @param player_name Player name (for display)
#' @param player_avg_receptions Player's season average receptions
#' @param position Player position ("WR", "TE", "RB")
#' @param opponent Opponent team abbreviation
#' @param opp_pass_def_rank Opponent pass defense rank (1-32)
#' @param is_home Is player's team at home?
#' @param is_dome Indoor game?
#' @param game_script Expected point differential
#' @param line Market line
#' @param over_odds American odds for over (NA -> derived from simulation)
#' @param under_odds American odds for under (NA -> derived from simulation)
#' @param line_over Optional line for over side
#' @param line_under Optional line for under side
#'
#' @return List with complete analysis
#'
#' @export
analyze_receptions_prop <- function(
  player_name,
  player_avg_receptions,
  position = "WR",
  line = NA_real_,
  opponent,
  opp_pass_def_rank,
  is_home = FALSE,
  is_dome = FALSE,
  game_script = 0,
  over_odds = NA_real_,
  under_odds = NA_real_,
  line_over = NA_real_,
  line_under = NA_real_
) {

  sim <- simulate_receptions(
    player_receptions = player_avg_receptions,
    position = position,
    opponent_def_rank = opp_pass_def_rank,
    is_home = is_home,
    is_dome = is_dome,
    game_script = game_script
  )

  ou <- receptions_over_under(sim, line, over_odds, under_odds, line_over, line_under)

  list(
    player = player_name,
    projection = ou$projection,
    line = ou$line,
    line_over = ou$line_over,
    line_under = ou$line_under,
    p_over = ou$p_over,
    p_under = ou$p_under,
    p_push = ou$p_push,
    ev_over = ou$ev_over,
    ev_under = ou$ev_under,
    recommendation = ou$recommendation
  )
}
