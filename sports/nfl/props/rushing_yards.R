# =============================================================================
# FILE: sports/nfl/props/rushing_yards.R
# PURPOSE: NFL player rushing yards prop simulation
#
# VERSION: 2.8.0
# LAST UPDATED: 2026-02-03
#
# DESCRIPTION:
#   Simulates rushing yards distributions and calculates over/under
#   probabilities for player prop betting (RB and QB).
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

#' Simulate player rushing yards
#'
#' Monte Carlo simulation of rushing yards for a specific game.
#'
#' @param player_projection Player's baseline projection (yards)
#' @param position Player position ("RB" or "QB")
#' @param opponent_def_rank Opponent rush defense rank (1-32, 1 = best)
#' @param is_home Boolean: is player on home team?
#' @param game_script Expected point differential (positive = leading)
#' @param n_trials Number of simulation trials
#'
#' @return List containing:
#'   \item{projection}{Mean projected yards}
#'   \item{sd}{Standard deviation}
#'   \item{ci_90}{90% confidence interval}
#'   \item{simulated_yards}{Vector of simulated values}
#'
#' @export
simulate_rushing_yards <- function(
  player_projection = NULL,
  position = "RB",
  opponent_def_rank = 16,  # Average
  is_home = FALSE,
  game_script = 0,
  n_trials = PROP_TRIALS
) {

  # Set baseline based on position if no projection provided
  baseline <- if (is.null(player_projection)) {
    if (position == "QB") QB_RUSHING_YARDS_BASELINE else RUSHING_YARDS_BASELINE
  } else {
    player_projection
  }

  # Standard deviation based on position
  base_sd <- if (position == "QB") QB_RUSHING_YARDS_SD else RUSHING_YARDS_SD

  # Start with player's baseline projection
  projection <- baseline

  # Apply home field adjustment
  if (is_home) {
    projection <- projection + RUSHING_YARDS_HOME_ADJ
  }

  # Opponent defense adjustment
  # Rank 1 = best defense = lower multiplier
  # Rank 32 = worst defense = higher multiplier
  def_mult_range <- RUSHING_DEF_MULTIPLIER_RANGE
  def_multiplier <- def_mult_range[1] + (opponent_def_rank - 1) / 31 * diff(def_mult_range)
  projection <- projection * def_multiplier

  # Game script adjustment - positive script means more rushing
  projection <- projection + game_script * RUSHING_YARDS_GAMESCRIPT_COEF

  # Ensure positive projection
  projection <- max(10, projection)

  # Standard deviation scales with projection
  sd <- base_sd * (projection / baseline)

  # Simulate using normal distribution
  simulated <- rnorm(n_trials, mean = projection, sd = sd)
  simulated <- pmax(0, simulated)  # No negative yards

  list(
    projection = round(projection, 1),
    sd = round(sd, 1),
    ci_90 = quantile(simulated, c(0.05, 0.95)),
    simulated_yards = simulated
  )
}

#' Calculate over/under probabilities for rushing yards
#'
#' @param simulation Output from simulate_rushing_yards()
#' @param line Market line (e.g., 75.5)
#' @param over_odds American odds for over (NA -> derived from simulation)
#' @param under_odds American odds for under (NA -> derived from simulation)
#'
#' @return List with probabilities and expected values
#'
#' @export
rushing_yards_over_under <- function(simulation, line,
                                     over_odds = NA_real_, under_odds = NA_real_,
                                     line_over = NA_real_, line_under = NA_real_) {
  yards <- simulation$simulated_yards
  allow_model_lines <- isTRUE(if (exists("PROP_ALLOW_MODEL_LINES")) PROP_ALLOW_MODEL_LINES else TRUE)
  allow_model_odds <- isTRUE(if (exists("PROP_ALLOW_MODEL_ODDS")) PROP_ALLOW_MODEL_ODDS else FALSE)

  # Derive fallback line from simulation when missing
  if (!is.finite(line) && allow_model_lines) {
    if (exists("derive_prop_market_from_sim", mode = "function")) {
      derived <- derive_prop_market_from_sim(
        yards,
        line_quantile = if (exists("PROP_FALLBACK_LINE_QUANTILE")) PROP_FALLBACK_LINE_QUANTILE else 0.50,
        vig = if (exists("PROP_MARKET_VIG")) PROP_MARKET_VIG else 0.045
      )
      if (!is.finite(line)) line <- derived$line
    }
  }
  if (!is.finite(line)) line <- stats::median(yards, na.rm = TRUE)
  if (!is.finite(line_over)) line_over <- line
  if (!is.finite(line_under)) line_under <- line

  # Calculate probabilities
  p_over <- mean(yards > line_over)
  p_under <- mean(yards < line_under)
  p_push_over <- mean(yards == line_over)
  p_push_under <- mean(yards == line_under)
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

  # Calculate EV (push-aware)
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
    over_odds = over_odds,
    under_odds = under_odds,
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

#' Full rushing yards prop analysis
#'
#' Complete pipeline from inputs to betting recommendation.
#'
#' @param player_name Player name (for display)
#' @param player_avg_yards Player's season average rushing yards
#' @param position Player position ("RB" or "QB")
#' @param line Market line
#' @param opponent Opponent team abbreviation
#' @param opp_rush_def_rank Opponent rush defense rank (1-32)
#' @param is_home Is player's team at home?
#' @param game_script Expected point differential
#' @param over_odds American odds for over (NA -> derived from simulation)
#' @param under_odds American odds for under (NA -> derived from simulation)
#'
#' @return List with complete analysis
#'
#' @examples
#' \dontrun{
#'   result <- analyze_rushing_yards_prop(
#'     player_name = "Derrick Henry",
#'     player_avg_yards = 105,
#'     position = "RB",
#'     line = 95.5,
#'     opponent = "NYG",
#'     opp_rush_def_rank = 20,
#'     is_home = TRUE,
#'     game_script = 3
#'   )
#'   print(result)
#' }
#'
#' @export
analyze_rushing_yards_prop <- function(
  player_name,
  player_avg_yards,
  position = "RB",
  line = NA_real_,
  opponent,
  opp_rush_def_rank,
  is_home = FALSE,
  game_script = 0,
  over_odds = NA_real_,
  under_odds = NA_real_,
  line_over = NA_real_,
  line_under = NA_real_
) {

  # Run simulation
  sim <- simulate_rushing_yards(
    player_projection = player_avg_yards,
    position = position,
    opponent_def_rank = opp_rush_def_rank,
    is_home = is_home,
    game_script = game_script
  )

  # Calculate over/under
  ou <- rushing_yards_over_under(sim, line, over_odds, under_odds, line_over, line_under)

  # Return complete analysis
  list(
    player = player_name,
    prop_type = "rushing_yards",
    position = position,
    opponent = opponent,
    line = ou$line,
    line_over = ou$line_over,
    line_under = ou$line_under,
    over_odds = ou$over_odds,
    under_odds = ou$under_odds,
    projection = sim$projection,
    ci_90 = sim$ci_90,
    p_over = ou$p_over,
    p_under = ou$p_under,
    p_push = ou$p_push,
    ev_over = ou$ev_over,
    ev_under = ou$ev_under,
    recommendation = ou$recommendation,
    factors = list(
      is_home = is_home,
      opp_def_rank = opp_rush_def_rank,
      game_script = game_script
    )
  )
}
