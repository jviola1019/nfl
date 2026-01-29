# =============================================================================
# R/simulation_helpers.R
# Simulation Utility Functions
# =============================================================================
# Utility functions used by NFLsimulation.R for score modeling and adjustments.
# This file provides re-exportable versions of internal helpers.
#
# Usage:
#   source("R/simulation_helpers.R")
#
# =============================================================================

#' Dynamic Regression Games
#'
#' Returns adjusted regression games based on season progress.
#' More regression early season (noisy stats), less late season (stable estimates).
#'
#' @param week Current week number (1-18)
#' @param base_games Base regression games value (default 6)
#' @return Adjusted regression games value (minimum 3)
#' @export
get_regression_games_ext <- function(week, base_games = 6) {
  # Week 1-4: Use full base_games (more shrinkage to prior)
  # Week 5-10: Gradually reduce regression
  # Week 11+: Minimum regression (trust current season data)
  pmax(3, base_games * (1 - 0.3 * pmin(week - 1, 10) / 10))
}


#' Pace Variance Adjustment
#'
#' Teams with high play volume have more scoring variance.
#' Fast-paced offenses have more opportunities and thus more variance.
#'
#' @param plays_pg Plays per game
#' @param league_avg League average plays/game (default 65)
#' @return Variance multiplier (>1 = more variance)
#' @export
pace_variance_adj_ext <- function(plays_pg, league_avg = 65) {
  1 + 0.05 * (plays_pg - league_avg) / league_avg
}


#' QB Rushing Threat Adjustment
#'
#' Mobile QBs add offensive value beyond passing EPA.
#' Dual-threat QBs like Lamar Jackson or Josh Allen provide extra scoring upside.
#'
#' @param qb_rush_ypg QB rush yards per game
#' @param league_avg League average QB rush yards (default 25)
#' @return Point adjustment (-0.3 to +0.3)
#' @export
qb_rush_adj_ext <- function(qb_rush_ypg, league_avg = 25) {
  # Each 20 yards above/below average = 0.3 point adjustment
  0.3 * pmin(pmax((qb_rush_ypg - league_avg) / 20, -1), 1)
}


#' Margin to Win Probability
#'
#' Converts a point margin to win probability using normal CDF.
#' Accounts for tie probability in NFL games.
#'
#' @param margin Expected point margin (positive = home favored)
#' @param sd Margin standard deviation
#' @param tie_width Half-width for tie zone (default 0.5)
#' @return Named vector with home_win_prob, away_win_prob, tie_prob
#' @export
margin_to_win_prob <- function(margin, sd, tie_width = 0.5) {
  if (!is.finite(margin) || !is.finite(sd) || sd <= 0) {
    return(c(home_win_prob = 0.5, away_win_prob = 0.5, tie_prob = 0))
  }

  # Probability home wins (margin > tie_width)
  p_home <- stats::pnorm(margin / sd) - stats::pnorm(tie_width / sd)
  p_home <- pmax(0, p_home)

  # Probability away wins (margin < -tie_width)
  p_away <- stats::pnorm(-tie_width / sd) - stats::pnorm(-margin / sd)
  p_away <- pmax(0, p_away)

  # Tie probability (margin within tie_width)
  p_tie <- stats::pnorm(tie_width / sd, mean = margin / sd) -
           stats::pnorm(-tie_width / sd, mean = margin / sd)
  p_tie <- pmax(0, pmin(1, p_tie))

  # Normalize to sum to 1
  total <- p_home + p_away + p_tie
  if (total > 0) {
    p_home <- p_home / total
    p_away <- p_away / total
    p_tie <- p_tie / total
  } else {
    p_home <- 0.5
    p_away <- 0.5
    p_tie <- 0
  }

  c(home_win_prob = p_home, away_win_prob = p_away, tie_prob = p_tie)
}


#' Normalize Probability Vector
#'
#' Ensures probabilities sum to 1 and are within [epsilon, 1-epsilon].
#'
#' @param probs Named or unnamed probability vector
#' @param epsilon Minimum probability (default 0.001)
#' @return Normalized probability vector
#' @export
normalize_probs <- function(probs, epsilon = 0.001) {
  # Clamp to valid range
  probs <- pmax(epsilon, pmin(1 - epsilon, probs))

  # Normalize to sum to 1
  probs / sum(probs)
}
