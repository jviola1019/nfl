# =============================================================================
# FILE: sports/nfl/props/passing_yards.R
# PURPOSE: NFL quarterback passing yards prop simulation
#
# VERSION: 2.7.0
# LAST UPDATED: 2026-02-02
#
# DESCRIPTION:
#   Simulates QB passing yards distributions and calculates over/under
#   probabilities for player prop betting.
# =============================================================================

# Source configuration
local({
  config_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "props_config.R")
  if (file.exists(config_path)) source(config_path)
})

#' Simulate QB passing yards
#'
#' Monte Carlo simulation of quarterback passing yards for a specific game.
#'
#' @param player_projection Player's baseline projection (yards)
#' @param opponent_def_rank Opponent pass defense rank (1-32, 1 = best)
#' @param is_home Boolean: is player on home team?
#' @param is_dome Boolean: is game indoors?
#' @param wind_mph Wind speed (only matters for outdoor games)
#' @param is_rain Boolean: rain or snow?
#' @param temp_f Temperature in Fahrenheit
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
simulate_passing_yards <- function(
  player_projection = PASSING_YARDS_BASELINE,
  opponent_def_rank = 16,  # Average
  is_home = FALSE,
  is_dome = FALSE,
  wind_mph = 0,
  is_rain = FALSE,
  temp_f = 70,
  game_script = 0,
  n_trials = PROP_TRIALS
) {

  # Start with player's baseline projection
  projection <- player_projection

  # Apply home field adjustment
  if (is_home) {
    projection <- projection + PASSING_YARDS_HOME_ADJ
  }

  # Weather adjustments (only for outdoor games)
  if (!is_dome) {
    # Wind penalty
    if (wind_mph > 15) {
      wind_penalty <- PASSING_YARDS_WIND_COEF * ((wind_mph - 15) / 5)
      projection <- projection + wind_penalty
    }

    # Precipitation penalty
    if (is_rain) {
      projection <- projection + PASSING_YARDS_RAIN_ADJ
    }

    # Cold weather penalty
    if (temp_f < 32) {
      projection <- projection + PASSING_YARDS_COLD_ADJ
    }
  } else {
    # Dome bonus
    projection <- projection + PASSING_YARDS_DOME_ADJ
  }

  # Opponent defense adjustment
  # Rank 1 = best defense = lower multiplier
  # Rank 32 = worst defense = higher multiplier
  def_mult_range <- PASSING_DEF_MULTIPLIER_RANGE
  def_multiplier <- def_mult_range[1] + (opponent_def_rank - 1) / 31 * diff(def_mult_range)
  projection <- projection * def_multiplier

  # Game script adjustment
  if (game_script < -7) {
    # Likely trailing, more passing
    projection <- projection + PASSING_YARDS_TRAILING_ADJ
  } else if (game_script > 7) {
    # Likely leading, less passing
    projection <- projection + PASSING_YARDS_LEADING_ADJ
  }

  # Ensure positive projection
  projection <- max(50, projection)

  # Standard deviation scales with projection
  sd <- PASSING_YARDS_SD * (projection / PASSING_YARDS_BASELINE)

  # Simulate using normal distribution (passing yards are roughly normal)
  simulated <- rnorm(n_trials, mean = projection, sd = sd)
  simulated <- pmax(0, simulated)  # No negative yards

  list(
    projection = round(projection, 1),
    sd = round(sd, 1),
    ci_90 = quantile(simulated, c(0.05, 0.95)),
    simulated_yards = simulated
  )
}

#' Calculate over/under probabilities for passing yards
#'
#' @param simulation Output from simulate_passing_yards()
#' @param line Market line (e.g., 275.5)
#' @param over_odds American odds for over (NA -> derived from simulation)
#' @param under_odds American odds for under (NA -> derived from simulation)
#'
#' @return List with probabilities and expected values
#'
#' @export
passing_yards_over_under <- function(simulation, line, over_odds = NA_real_, under_odds = NA_real_) {
  yards <- simulation$simulated_yards

  # Derive fallback line/odds from simulation when missing
  if (!is.finite(line) || !is.finite(over_odds) || !is.finite(under_odds)) {
    if (exists("derive_prop_market_from_sim", mode = "function")) {
      derived <- derive_prop_market_from_sim(
        yards,
        line_quantile = if (exists("PROP_FALLBACK_LINE_QUANTILE")) PROP_FALLBACK_LINE_QUANTILE else 0.50,
        vig = if (exists("PROP_MARKET_VIG")) PROP_MARKET_VIG else 0.045
      )
      if (!is.finite(line)) line <- derived$line
      if (!is.finite(over_odds)) over_odds <- derived$over_odds
      if (!is.finite(under_odds)) under_odds <- derived$under_odds
    }
  }
  if (!is.finite(line)) line <- stats::median(yards, na.rm = TRUE)
  if (!is.finite(over_odds)) over_odds <- if (exists("DEFAULT_YARD_PROP_ODDS")) DEFAULT_YARD_PROP_ODDS else -110
  if (!is.finite(under_odds)) under_odds <- if (exists("DEFAULT_YARD_PROP_ODDS")) DEFAULT_YARD_PROP_ODDS else -110

  # Calculate probabilities
  p_over <- mean(yards > line)
  p_under <- mean(yards < line)
  p_push <- mean(yards == line)  # Usually ~0 for non-integer lines

  # Convert odds to decimal
  over_dec <- if (over_odds >= 0) 1 + over_odds/100 else 1 + 100/abs(over_odds)
  under_dec <- if (under_odds >= 0) 1 + under_odds/100 else 1 + 100/abs(under_odds)

  # Calculate EV
  ev_over <- p_over * (over_dec - 1) - p_under
  ev_under <- p_under * (under_dec - 1) - p_over

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

#' Full passing yards prop analysis
#'
#' Complete pipeline from inputs to betting recommendation.
#'
#' @param qb_name Quarterback name (for display)
#' @param qb_avg_yards QB's season average passing yards
#' @param line Market line
#' @param opponent Opponent team abbreviation
#' @param opp_pass_def_rank Opponent pass defense rank (1-32)
#' @param is_home Is QB's team at home?
#' @param is_dome Indoor game?
#' @param weather List with wind_mph, is_rain, temp_f (optional)
#' @param over_odds American odds for over (NA -> derived from simulation)
#' @param under_odds American odds for under (NA -> derived from simulation)
#'
#' @return List with complete analysis
#'
#' @examples
#' \dontrun{
#'   result <- analyze_passing_yards_prop(
#'     qb_name = "Patrick Mahomes",
#'     qb_avg_yards = 285,
#'     line = 275.5,
#'     opponent = "BUF",
#'     opp_pass_def_rank = 5,
#'     is_home = TRUE,
#'     is_dome = FALSE,
#'     weather = list(wind_mph = 10, is_rain = FALSE, temp_f = 45)
#'   )
#'   print(result)
#' }
#'
#' @export
analyze_passing_yards_prop <- function(
  qb_name,
  qb_avg_yards,
  line = NA_real_,
  opponent,
  opp_pass_def_rank,
  is_home = FALSE,
  is_dome = FALSE,
  weather = NULL,
  over_odds = NA_real_,
  under_odds = NA_real_
) {

  # Extract weather params
  wind_mph <- if (!is.null(weather)) weather$wind_mph else 0
  is_rain <- if (!is.null(weather)) weather$is_rain else FALSE
  temp_f <- if (!is.null(weather)) weather$temp_f else 70

  # Run simulation
  sim <- simulate_passing_yards(
    player_projection = qb_avg_yards,
    opponent_def_rank = opp_pass_def_rank,
    is_home = is_home,
    is_dome = is_dome,
    wind_mph = wind_mph,
    is_rain = is_rain,
    temp_f = temp_f,
    game_script = 0  # Neutral assumption
  )

  # Calculate over/under
  ou <- passing_yards_over_under(sim, line, over_odds, under_odds)

  # Return complete analysis
  list(
    player = qb_name,
    prop_type = "passing_yards",
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
      weather = weather
    )
  )
}
