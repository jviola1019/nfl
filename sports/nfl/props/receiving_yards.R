# =============================================================================
# FILE: sports/nfl/props/receiving_yards.R
# PURPOSE: NFL player receiving yards prop simulation
#
# VERSION: 2.8.0
# LAST UPDATED: 2026-02-03
#
# DESCRIPTION:
#   Simulates receiving yards distributions and calculates over/under
#   probabilities for player prop betting (WR, TE, RB).
# =============================================================================

# Source configuration
local({
  config_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "props_config.R")
  if (file.exists(config_path)) source(config_path)
})

#' Simulate player receiving yards
#'
#' Monte Carlo simulation of receiving yards for a specific game.
#'
#' @param player_projection Player's baseline projection (yards)
#' @param position Player position ("WR", "TE", or "RB")
#' @param target_share Player's target share (0-1, e.g., 0.25 = 25%)
#' @param opponent_def_rank Opponent pass defense rank (1-32, 1 = best)
#' @param is_home Boolean: is player on home team?
#' @param is_dome Boolean: is game indoors?
#' @param game_script Expected point differential (negative = trailing = more passing)
#' @param n_trials Number of simulation trials
#'
#' @return List containing:
#'   \item{projection}{Mean projected yards}
#'   \item{sd}{Standard deviation}
#'   \item{ci_90}{90% confidence interval}
#'   \item{simulated_yards}{Vector of simulated values}
#'
#' @export
simulate_receiving_yards <- function(
  player_projection = NULL,
  position = "WR",
  target_share = 0.20,  # League average ~15-25%
  opponent_def_rank = 16,  # Average
  is_home = FALSE,
  is_dome = FALSE,
  game_script = 0,
  n_trials = PROP_TRIALS
) {

  # Set baseline and SD based on position if no projection provided
  baseline <- if (is.null(player_projection)) {
    switch(position,
      WR = RECEIVING_YARDS_WR_BASELINE,
      TE = RECEIVING_YARDS_TE_BASELINE,
      RB = RECEIVING_YARDS_RB_BASELINE,
      RECEIVING_YARDS_WR_BASELINE
    )
  } else {
    player_projection
  }

  base_sd <- switch(position,
    WR = RECEIVING_YARDS_WR_SD,
    TE = RECEIVING_YARDS_TE_SD,
    RB = RECEIVING_YARDS_RB_SD,
    RECEIVING_YARDS_WR_SD
  )

  # Start with player's baseline projection
  projection <- baseline

  # Target share adjustment (above/below 20% average)
  target_adj <- (target_share - 0.20) * 10 * TARGET_SHARE_MULTIPLIER * baseline
  projection <- projection + target_adj

  # Apply home field adjustment
  if (is_home) {
    projection <- projection + RECEIVING_YARDS_HOME_ADJ
  }

  # Dome adjustment
  if (is_dome) {
    projection <- projection + RECEIVING_YARDS_DOME_ADJ
  }

  # Opponent defense adjustment
  # Rank 1 = best defense = lower multiplier
  # Rank 32 = worst defense = higher multiplier
  def_mult_range <- PASSING_DEF_MULTIPLIER_RANGE  # Use passing defense for receivers
  def_multiplier <- def_mult_range[1] + (opponent_def_rank - 1) / 31 * diff(def_mult_range)
  projection <- projection * def_multiplier

  # Game script adjustment - negative script (trailing) means more passing
  if (game_script < -7) {
    projection <- projection * 1.15  # 15% boost when trailing
  } else if (game_script > 7) {
    projection <- projection * 0.90  # 10% reduction when leading
  }

  # Ensure positive projection
  projection <- max(5, projection)

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

#' Calculate over/under probabilities for receiving yards
#'
#' @param simulation Output from simulate_receiving_yards()
#' @param line Market line (e.g., 65.5)
#' @param over_odds American odds for over
#' @param under_odds American odds for under
#'
#' @return List with probabilities and expected values
#'
#' @export
receiving_yards_over_under <- function(simulation, line, over_odds = -110, under_odds = -110) {
  yards <- simulation$simulated_yards

  # Calculate probabilities
  p_over <- mean(yards > line)
  p_under <- mean(yards < line)
  p_push <- mean(yards == line)

  # Convert odds to decimal
  over_dec <- if (over_odds >= 0) 1 + over_odds/100 else 1 + 100/abs(over_odds)
  under_dec <- if (under_odds >= 0) 1 + under_odds/100 else 1 + 100/abs(under_odds)

  # Calculate EV
  ev_over <- p_over * (over_dec - 1) - (1 - p_over)
  ev_under <- p_under * (under_dec - 1) - (1 - p_under)

  list(
    line = line,
    projection = simulation$projection,
    p_over = round(p_over, 4),
    p_under = round(p_under, 4),
    p_push = round(p_push, 4),
    ev_over = round(ev_over, 4),
    ev_under = round(ev_under, 4),
    recommendation = if (ev_over > 0.02) "OVER" else if (ev_under > 0.02) "UNDER" else "PASS"
  )
}

#' Full receiving yards prop analysis
#'
#' Complete pipeline from inputs to betting recommendation.
#'
#' @param player_name Player name (for display)
#' @param player_avg_yards Player's season average receiving yards
#' @param position Player position ("WR", "TE", "RB")
#' @param target_share Player's target share (0-1)
#' @param line Market line
#' @param opponent Opponent team abbreviation
#' @param opp_pass_def_rank Opponent pass defense rank (1-32)
#' @param is_home Is player's team at home?
#' @param is_dome Indoor game?
#' @param game_script Expected point differential
#' @param over_odds American odds for over (default -110)
#' @param under_odds American odds for under (default -110)
#'
#' @return List with complete analysis
#'
#' @examples
#' \dontrun{
#'   result <- analyze_receiving_yards_prop(
#'     player_name = "CeeDee Lamb",
#'     player_avg_yards = 95,
#'     position = "WR",
#'     target_share = 0.28,
#'     line = 85.5,
#'     opponent = "PHI",
#'     opp_pass_def_rank = 8,
#'     is_home = FALSE,
#'     is_dome = FALSE,
#'     game_script = 0
#'   )
#'   print(result)
#' }
#'
#' @export
analyze_receiving_yards_prop <- function(
  player_name,
  player_avg_yards,
  position = "WR",
  target_share = 0.20,
  line,
  opponent,
  opp_pass_def_rank,
  is_home = FALSE,
  is_dome = FALSE,
  game_script = 0,
  over_odds = -110,
  under_odds = -110
) {

  # Run simulation
  sim <- simulate_receiving_yards(
    player_projection = player_avg_yards,
    position = position,
    target_share = target_share,
    opponent_def_rank = opp_pass_def_rank,
    is_home = is_home,
    is_dome = is_dome,
    game_script = game_script
  )

  # Calculate over/under
  ou <- receiving_yards_over_under(sim, line, over_odds, under_odds)

  # Return complete analysis
  list(
    player = player_name,
    prop_type = "receiving_yards",
    position = position,
    opponent = opponent,
    line = line,
    projection = sim$projection,
    ci_90 = sim$ci_90,
    p_over = ou$p_over,
    p_under = ou$p_under,
    ev_over = ou$ev_over,
    ev_under = ou$ev_under,
    recommendation = ou$recommendation,
    factors = list(
      is_home = is_home,
      is_dome = is_dome,
      opp_def_rank = opp_pass_def_rank,
      target_share = target_share,
      game_script = game_script
    )
  )
}
