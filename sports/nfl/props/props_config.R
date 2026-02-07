# =============================================================================
# FILE: sports/nfl/props/props_config.R
# PURPOSE: NFL player props configuration and shared utilities
#
# VERSION: 2.9.0
# LAST UPDATED: 2026-02-03
#
# DESCRIPTION:
#   Configuration parameters for NFL player prop predictions.
#   Includes baseline projections, variance parameters, and adjustment factors.
#
# PROP TYPES SUPPORTED:
#   - Passing yards (QB)
#   - Rushing yards (RB, QB)
#   - Receiving yards (WR, TE, RB)
#   - Touchdowns (any player)
#   - Receptions (WR, TE, RB)
#   - Completions (QB)
# =============================================================================

# =============================================================================
# PASSING YARDS PARAMETERS
# =============================================================================

# League averages (2023-2024 seasons)
PASSING_YARDS_BASELINE <- 225      # League average per game
PASSING_YARDS_SD <- 65             # Standard deviation

# Situational adjustments
PASSING_YARDS_HOME_ADJ <- 5        # Home field boost
PASSING_YARDS_DOME_ADJ <- 12       # Indoor game bonus
PASSING_YARDS_WIND_COEF <- -0.8    # Per 5 mph wind over 15 mph
PASSING_YARDS_RAIN_ADJ <- -15      # Rain/snow penalty
PASSING_YARDS_COLD_ADJ <- -8       # Cold weather (<32F)

# Game script adjustments
PASSING_YARDS_TRAILING_ADJ <- 25   # When likely to trail (more pass attempts)
PASSING_YARDS_LEADING_ADJ <- -15   # When likely to lead (more run game)

# Opponent strength multipliers (vs league average)
PASSING_DEF_MULTIPLIER_RANGE <- c(0.80, 1.20)  # Elite D to weak D

# =============================================================================
# RUSHING YARDS PARAMETERS
# =============================================================================

# League averages (RB specific)
RUSHING_YARDS_BASELINE <- 65       # RB league average per game
RUSHING_YARDS_SD <- 35             # Standard deviation

# Situational adjustments
RUSHING_YARDS_HOME_ADJ <- 3        # Home field boost
RUSHING_YARDS_GAMESCRIPT_COEF <- 2.0  # Per expected point lead

# QB rushing (different parameters)
QB_RUSHING_YARDS_BASELINE <- 25    # Mobile QB average
QB_RUSHING_YARDS_SD <- 20          # Higher variance

# Opponent strength multipliers
RUSHING_DEF_MULTIPLIER_RANGE <- c(0.75, 1.25)

# =============================================================================
# RECEIVING YARDS PARAMETERS
# =============================================================================

# League averages by position
RECEIVING_YARDS_WR_BASELINE <- 55  # WR league average
RECEIVING_YARDS_TE_BASELINE <- 35  # TE league average
RECEIVING_YARDS_RB_BASELINE <- 20  # RB league average

# Standard deviations
RECEIVING_YARDS_WR_SD <- 30
RECEIVING_YARDS_TE_SD <- 25
RECEIVING_YARDS_RB_SD <- 18

# Target share multiplier
TARGET_SHARE_MULTIPLIER <- 1.5     # Per 10% target share above average

# Situational adjustments
RECEIVING_YARDS_HOME_ADJ <- 4
RECEIVING_YARDS_DOME_ADJ <- 8

# =============================================================================
# TOUCHDOWN PARAMETERS
# =============================================================================

# Base touchdown probabilities (per game)
TD_PROB_QB_PASSING <- 0.65         # Probability of throwing 1+ TD
TD_PROB_RB_RUSHING <- 0.35         # Probability of rushing TD
TD_PROB_WR_RECEIVING <- 0.30       # Probability of receiving TD
TD_PROB_TE_RECEIVING <- 0.20       # Probability of receiving TD

# Variance parameters
TD_OVERDISPERSION <- 1.5           # Negative binomial overdispersion

# Red zone adjustments
RED_ZONE_BOOST <- 1.3              # Multiplier for high red zone usage

# =============================================================================
# ANYTIME TD MARKET ODDS DEFAULTS (v2.9.2)
# =============================================================================
# Position-based default odds when API unavailable
# Based on typical sportsbook lines (2023-2024 analysis)

# Default anytime TD odds by position (American format)
# These reflect realistic market pricing, not +100 for everyone
ANYTIME_TD_ODDS_QB <- 350          # QBs score in ~15% of games
ANYTIME_TD_ODDS_RB1 <- -110        # RB1s score in ~45% of games
ANYTIME_TD_ODDS_RB2 <- 200         # RB2s score in ~25% of games
ANYTIME_TD_ODDS_WR1 <- 140         # WR1s score in ~35% of games
ANYTIME_TD_ODDS_WR2 <- 250         # WR2s score in ~20% of games
ANYTIME_TD_ODDS_TE <- 200          # TE1s score in ~25% of games

# Default yard prop odds (industry standard)
DEFAULT_YARD_PROP_ODDS <- -110     # Standard -110 both sides

# =============================================================================
# PROP ODDS SOURCES + FALLBACK MARKET SETTINGS
# =============================================================================

# Source selection for prop odds:
#  - "odds_api": The Odds API (requires ODDS_API_KEY)
#  - "csv": Local CSV file (see PROP_ODDS_CSV_PATH)
#  - "model": Synthetic market derived from simulation distribution
if (!exists("PROP_ODDS_SOURCE")) PROP_ODDS_SOURCE <- "odds_api"

# Optional local CSV path for prop odds
if (!exists("PROP_ODDS_CSV_PATH")) {
  PROP_ODDS_CSV_PATH <- file.path(getwd(), "artifacts", "prop_odds.csv")
}

# Default vig (overround) used when synthesizing odds from model probabilities
if (!exists("PROP_MARKET_VIG")) PROP_MARKET_VIG <- 0.045

# Quantile used to set fallback lines from simulation distribution
# 0.50 = median (balanced)
if (!exists("PROP_FALLBACK_LINE_QUANTILE")) PROP_FALLBACK_LINE_QUANTILE <- 0.50

# =============================================================================
# RECEPTIONS PARAMETERS
# =============================================================================

# League averages
RECEPTIONS_WR_BASELINE <- 5.0      # WR average
RECEPTIONS_TE_BASELINE <- 3.5      # TE average
RECEPTIONS_RB_BASELINE <- 2.5      # RB average

# Standard deviations
RECEPTIONS_WR_SD <- 2.5
RECEPTIONS_TE_SD <- 2.0
RECEPTIONS_RB_SD <- 1.8

# =============================================================================
# COMPLETIONS PARAMETERS (QB)
# =============================================================================

# League averages
COMPLETIONS_BASELINE <- 22         # Average completions per game
COMPLETIONS_SD <- 6                # Standard deviation

# Completion rate adjustments
COMP_RATE_DOME_ADJ <- 0.02         # +2% completion rate indoors
COMP_RATE_WIND_COEF <- -0.005      # -0.5% per 5 mph over 15

# =============================================================================
# SIMULATION PARAMETERS
# =============================================================================

# Default trials for prop simulation
PROP_TRIALS <- 50000L

# Distribution choices
PROP_DISTRIBUTION_YARDS <- "normal"    # Yards are roughly normal
PROP_DISTRIBUTION_TD <- "negbin"       # TDs have overdispersion
PROP_DISTRIBUTION_COUNT <- "poisson"   # Receptions/completions

# =============================================================================
# EDGE CLASSIFICATION / RECOMMENDATION POLICY
# =============================================================================

# Single source of truth for edge-quality bins
PROP_EDGE_BIN_OK_MAX <- 0.05
PROP_EDGE_BIN_HIGH_MAX <- 0.10

# Recommendation thresholds
PROP_MIN_BET_EDGE <- 0.02

#' Validate prop row for model/runtime failures
#'
#' @param p_over Probability of over outcome
#' @param p_under Probability of under outcome
#' @param over_odds Market over odds
#' @param under_odds Market under odds (NA allowed for one-sided markets like anytime TD)
#' @param is_two_sided Whether both over/under odds are required
#' @return Logical scalar
is_prop_model_error <- function(p_over, p_under, over_odds, under_odds, is_two_sided = TRUE) {
  sum_probs <- p_over + p_under
  probs_valid <- is.finite(p_over) && is.finite(p_under) &&
    p_over >= 0 && p_over <= 1 && p_under >= 0 && p_under <= 1 &&
    sum_probs <= 1.02 && sum_probs >= 0.90

  odds_valid <- is.finite(over_odds) && (!is_two_sided || is.finite(under_odds))

  !(probs_valid && odds_valid)
}

#' Assign recommendation under unified prop policy
#'
#' @param ev_over EV for over side
#' @param ev_under EV for under side (NA for one-sided props)
#' @param model_error Logical indicating invalid inputs/runtime issue
#' @return Recommendation label
get_prop_recommendation <- function(ev_over, ev_under = NA_real_, model_error = FALSE) {
  if (isTRUE(model_error) || !is.finite(ev_over) || (!is.na(ev_under) && !is.finite(ev_under))) {
    return("MODEL ERROR")
  }

  best_candidates <- c(ev_over, ev_under)
  best_candidates <- best_candidates[is.finite(best_candidates) & best_candidates > 0]
  best_ev <- if (length(best_candidates)) max(best_candidates, na.rm = TRUE) else NA_real_

  if (is.finite(best_ev) && best_ev > PROP_EDGE_BIN_HIGH_MAX) {
    return("REVIEW")
  }

  if (is.finite(ev_over) && ev_over > PROP_MIN_BET_EDGE) return("OVER")
  if (is.finite(ev_under) && ev_under > PROP_MIN_BET_EDGE) return("UNDER")
  "PASS"
}

#' Classify edge quality under unified bins
#'
#' @param ev_over EV for over side
#' @param ev_under EV for under side (NA for one-sided props)
#' @param recommendation Recommendation label
#' @return Edge quality display label
classify_prop_edge_quality <- function(ev_over, ev_under = NA_real_, recommendation = NA_character_) {
  if (identical(recommendation, "MODEL ERROR")) return("MODEL ERROR")

  best_candidates <- c(ev_over, ev_under)
  best_candidates <- best_candidates[is.finite(best_candidates) & best_candidates > 0]
  best_ev <- if (length(best_candidates)) max(best_candidates, na.rm = TRUE) else NA_real_
  if (!is.finite(best_ev)) return("MODEL ERROR")

  if (best_ev <= PROP_EDGE_BIN_OK_MAX) return("OK")
  if (best_ev <= PROP_EDGE_BIN_HIGH_MAX) return("High")
  "Review"
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Get baseline projection for a prop type
#'
#' @param prop_type Character: "passing_yards", "rushing_yards", etc.
#' @param position Player position
#' @return Baseline projection value
get_prop_baseline <- function(prop_type, position = NULL) {
  switch(prop_type,
    passing_yards = PASSING_YARDS_BASELINE,
    rushing_yards = if (position == "QB") QB_RUSHING_YARDS_BASELINE else RUSHING_YARDS_BASELINE,
    receiving_yards = switch(position,
      WR = RECEIVING_YARDS_WR_BASELINE,
      TE = RECEIVING_YARDS_TE_BASELINE,
      RB = RECEIVING_YARDS_RB_BASELINE,
      RECEIVING_YARDS_WR_BASELINE
    ),
    receptions = switch(position,
      WR = RECEPTIONS_WR_BASELINE,
      TE = RECEPTIONS_TE_BASELINE,
      RB = RECEPTIONS_RB_BASELINE,
      RECEPTIONS_WR_BASELINE
    ),
    completions = COMPLETIONS_BASELINE,
    NA_real_
  )
}

#' Get standard deviation for a prop type
#'
#' @param prop_type Character: "passing_yards", "rushing_yards", etc.
#' @param position Player position
#' @return Standard deviation value
get_prop_sd <- function(prop_type, position = NULL) {
  switch(prop_type,
    passing_yards = PASSING_YARDS_SD,
    rushing_yards = if (position == "QB") QB_RUSHING_YARDS_SD else RUSHING_YARDS_SD,
    receiving_yards = switch(position,
      WR = RECEIVING_YARDS_WR_SD,
      TE = RECEIVING_YARDS_TE_SD,
      RB = RECEIVING_YARDS_RB_SD,
      RECEIVING_YARDS_WR_SD
    ),
    receptions = switch(position,
      WR = RECEPTIONS_WR_SD,
      TE = RECEPTIONS_TE_SD,
      RB = RECEPTIONS_RB_SD,
      RECEPTIONS_WR_SD
    ),
    completions = COMPLETIONS_SD,
    NA_real_
  )
}
