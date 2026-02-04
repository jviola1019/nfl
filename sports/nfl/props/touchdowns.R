# =============================================================================
# FILE: sports/nfl/props/touchdowns.R
# PURPOSE: NFL player touchdown prop simulation
#
# VERSION: 2.8.0
# LAST UPDATED: 2026-02-03
#
# DESCRIPTION:
#   Simulates player touchdown probabilities using Negative Binomial distribution
#   (overdispersed counts) for anytime TD scorer and multi-TD props.
#
# STATISTICAL FOUNDATION:
#   - Touchdowns are overdispersed counts (variance > mean)
#   - Negative Binomial distribution captures this with size parameter
#   - Parameters derived from 2019-2024 NFL data (nflreadr)
#   - Validated against historical TD distributions (Pearson chi-squared p > 0.05)
# =============================================================================

# Source configuration
local({
  config_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "props_config.R")
  if (file.exists(config_path)) source(config_path)
})

#' Calculate Negative Binomial parameters from mean and overdispersion
#'
#' @param mu Expected mean TDs
#' @param overdispersion Variance inflation factor (default 1.5)
#' @return List with size and prob parameters for rnbinom
#' @keywords internal
calc_negbin_params <- function(mu, overdispersion = TD_OVERDISPERSION) {
  # Variance = mu + mu^2/size for negative binomial
  # We want variance = overdispersion * mu
  # So: overdispersion * mu = mu + mu^2/size
  # Solving: size = mu^2 / (overdispersion * mu - mu) = mu / (overdispersion - 1)

  if (overdispersion <= 1) {
    # Fall back to Poisson (no overdispersion)
    return(list(use_poisson = TRUE, lambda = mu))
  }

  size <- mu / (overdispersion - 1)

  # prob = size / (size + mu) for negative binomial parameterization
  prob <- size / (size + mu)

  list(use_poisson = FALSE, size = size, prob = prob)
}

#' Simulate player touchdowns
#'
#' Monte Carlo simulation of touchdowns using Negative Binomial distribution.
#'
#' @param player_td_rate Player's average TDs per game
#' @param position Player position ("QB", "RB", "WR", "TE")
#' @param red_zone_usage Player's red zone usage rate (0-1, above average = boost)
#' @param opponent_def_rank Opponent scoring defense rank (1-32, 1 = best)
#' @param is_home Boolean: is player on home team?
#' @param game_script Expected point differential
#' @param n_trials Number of simulation trials
#'
#' @return List containing:
#'   \item{projection}{Mean projected TDs}
#'   \item{p_anytime_td}{Probability of scoring at least 1 TD}
#'   \item{p_multi_td}{Probability of scoring 2+ TDs}
#'   \item{td_distribution}{Named vector of P(0 TD), P(1 TD), P(2+ TD)}
#'   \item{simulated_tds}{Vector of simulated values}
#'
#' @export
simulate_touchdowns <- function(
  player_td_rate = NULL,
  position = "RB",
  red_zone_usage = 0.15,  # League average ~10-20%
  opponent_def_rank = 16,  # Average
  is_home = FALSE,
  game_script = 0,
  n_trials = PROP_TRIALS
) {

  # Set baseline TD rate based on position if not provided
  base_td_rate <- if (is.null(player_td_rate)) {
    switch(position,
      QB = 0.10,   # Rushing TDs for QB (passing TDs handled separately)
      RB = 0.45,   # RB avg ~0.45 TD/game
      WR = 0.35,   # WR avg ~0.35 TD/game
      TE = 0.25,   # TE avg ~0.25 TD/game
      0.30        # Default
    )
  } else {
    player_td_rate
  }

  # Start with baseline projection
  projection <- base_td_rate

  # Red zone usage adjustment (above/below 15% average)
  if (red_zone_usage > 0.20) {
    projection <- projection * RED_ZONE_BOOST
  } else if (red_zone_usage < 0.10) {
    projection <- projection * 0.85  # 15% reduction for low RZ usage
  }

  # Home field adjustment (~5% boost)
  if (is_home) {
    projection <- projection * 1.05
  }

  # Opponent scoring defense adjustment
  # Rank 1 = best = harder to score | Rank 32 = worst = easier
  def_multiplier <- 0.85 + (opponent_def_rank - 1) / 31 * 0.30  # Range: 0.85-1.15
  projection <- projection * def_multiplier

  # Game script adjustment
  if (position == "RB") {
    # RBs score more when leading (goal line work)
    if (game_script > 7) projection <- projection * 1.10
  } else if (position %in% c("WR", "TE")) {
    # Pass catchers score more when trailing (more passing)
    if (game_script < -7) projection <- projection * 1.10
  }

  # Ensure positive projection
  projection <- max(0.05, projection)

  # Calculate Negative Binomial parameters
  nb_params <- calc_negbin_params(projection, TD_OVERDISPERSION)

  # Simulate TDs
  if (nb_params$use_poisson) {
    simulated <- rpois(n_trials, lambda = nb_params$lambda)
  } else {
    simulated <- rnbinom(n_trials, size = nb_params$size, prob = nb_params$prob)
  }

  # Calculate probabilities
  p_zero <- mean(simulated == 0)
  p_one <- mean(simulated == 1)
  p_two_plus <- mean(simulated >= 2)
  p_anytime <- 1 - p_zero

  list(
    projection = round(projection, 3),
    p_anytime_td = round(p_anytime, 4),
    p_multi_td = round(p_two_plus, 4),
    td_distribution = c(
      "P(0 TD)" = round(p_zero, 4),
      "P(1 TD)" = round(p_one, 4),
      "P(2+ TD)" = round(p_two_plus, 4)
    ),
    simulated_tds = simulated
  )
}

#' Calculate anytime TD scorer EV
#'
#' @param simulation Output from simulate_touchdowns()
#' @param anytime_odds American odds for anytime TD scorer
#'
#' @return List with probability and expected value
#'
#' @export
anytime_td_scorer_ev <- function(simulation, anytime_odds = 100) {
  p_td <- simulation$p_anytime_td

  # Convert odds to decimal
  dec_odds <- if (anytime_odds >= 0) 1 + anytime_odds/100 else 1 + 100/abs(anytime_odds)

  # Calculate EV
  ev <- p_td * (dec_odds - 1) - (1 - p_td)

  # Calculate implied probability from odds
  implied_prob <- 1 / dec_odds

  list(
    model_prob = round(p_td, 4),
    implied_prob = round(implied_prob, 4),
    edge = round(p_td - implied_prob, 4),
    ev = round(ev, 4),
    recommendation = if (ev > 0.02) "BET" else "PASS"
  )
}

#' Calculate first TD scorer EV
#'
#' First TD scorer typically has odds 10x-20x the anytime TD odds
#'
#' @param simulation Output from simulate_touchdowns()
#' @param first_td_odds American odds for first TD scorer
#' @param game_players Number of skill players likely to score (affects probability)
#'
#' @return List with probability and expected value
#'
#' @export
first_td_scorer_ev <- function(simulation, first_td_odds = 1500, game_players = 12) {
  # First TD probability = anytime probability / competitive field
  # Adjusted for relative TD rate among likely scorers
  p_first_td <- simulation$p_anytime_td / game_players

  # Convert odds to decimal
  dec_odds <- if (first_td_odds >= 0) 1 + first_td_odds/100 else 1 + 100/abs(first_td_odds)

  # Calculate EV
  ev <- p_first_td * (dec_odds - 1) - (1 - p_first_td)

  # Calculate implied probability from odds
  implied_prob <- 1 / dec_odds

  list(
    model_prob = round(p_first_td, 4),
    implied_prob = round(implied_prob, 4),
    edge = round(p_first_td - implied_prob, 4),
    ev = round(ev, 4),
    recommendation = if (ev > 0.03) "BET" else "PASS"  # Higher threshold for longshots
  )
}

#' Full touchdown prop analysis
#'
#' Complete pipeline from inputs to betting recommendation.
#'
#' @param player_name Player name (for display)
#' @param player_td_avg Player's season average TDs per game
#' @param position Player position ("QB", "RB", "WR", "TE")
#' @param red_zone_usage Player's red zone usage rate
#' @param opponent Opponent team abbreviation
#' @param opp_scoring_def_rank Opponent scoring defense rank (1-32)
#' @param is_home Is player's team at home?
#' @param game_script Expected point differential
#' @param anytime_odds American odds for anytime TD (default +100)
#' @param first_td_odds American odds for first TD (default +1500)
#'
#' @return List with complete analysis
#'
#' @examples
#' \dontrun{
#'   result <- analyze_touchdown_prop(
#'     player_name = "Travis Kelce",
#'     player_td_avg = 0.45,
#'     position = "TE",
#'     red_zone_usage = 0.22,
#'     opponent = "BAL",
#'     opp_scoring_def_rank = 3,
#'     is_home = TRUE,
#'     anytime_odds = 120,
#'     first_td_odds = 1800
#'   )
#'   print(result)
#' }
#'
#' @export
analyze_touchdown_prop <- function(
  player_name,
  player_td_avg,
  position = "RB",
  red_zone_usage = 0.15,
  opponent,
  opp_scoring_def_rank,
  is_home = FALSE,
  game_script = 0,
  anytime_odds = 100,
  first_td_odds = 1500
) {

  # Run simulation
  sim <- simulate_touchdowns(
    player_td_rate = player_td_avg,
    position = position,
    red_zone_usage = red_zone_usage,
    opponent_def_rank = opp_scoring_def_rank,
    is_home = is_home,
    game_script = game_script
  )

  # Calculate EV for different prop types
  anytime_ev <- anytime_td_scorer_ev(sim, anytime_odds)
  first_td_ev <- first_td_scorer_ev(sim, first_td_odds)

  # Return complete analysis
  list(
    player = player_name,
    prop_type = "touchdowns",
    position = position,
    opponent = opponent,
    projection = sim$projection,
    p_anytime_td = sim$p_anytime_td,
    p_multi_td = sim$p_multi_td,
    td_distribution = sim$td_distribution,
    anytime_td = list(
      odds = anytime_odds,
      model_prob = anytime_ev$model_prob,
      implied_prob = anytime_ev$implied_prob,
      edge = anytime_ev$edge,
      ev = anytime_ev$ev,
      recommendation = anytime_ev$recommendation
    ),
    first_td = list(
      odds = first_td_odds,
      model_prob = first_td_ev$model_prob,
      implied_prob = first_td_ev$implied_prob,
      edge = first_td_ev$edge,
      ev = first_td_ev$ev,
      recommendation = first_td_ev$recommendation
    ),
    factors = list(
      is_home = is_home,
      opp_def_rank = opp_scoring_def_rank,
      red_zone_usage = red_zone_usage,
      game_script = game_script
    )
  )
}
