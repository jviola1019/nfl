# =============================================================================
# R/coaching_adjustments.R
# Coaching Change Adjustments
# =============================================================================
# Handles uncertainty adjustments for teams with new head coaches.
# First 4-8 weeks under new coaching staff show elevated variance.
#
# Usage:
#   source("R/coaching_adjustments.R")
#   adj <- get_coaching_adjustment("CHI", 2024, week = 3)
#
# =============================================================================

# Coaching changes by season (update annually before season starts)
# Format: list of team abbreviations that hired new head coaches
COACHING_CHANGES <- list(
  "2024" = c("CHI", "LAC", "LV", "SEA", "TEN", "WAS"),
  "2025" = c("NYJ", "JAX", "NE", "NO", "DAL", "CIN", "LV", "CHI")
)


#' Get Coaching Change Adjustment
#'
#' Returns a point adjustment for teams with new head coaches.
#' New coaches create uncertainty in the first 4-8 weeks of the season.
#'
#' @param team Team abbreviation
#' @param season Season year
#' @param week Week number (1-18 for regular season)
#' @return Point adjustment (typically negative for uncertainty, 0 for no change)
#' @export
get_coaching_adjustment <- function(team, season, week) {

  # Get list of teams with new coaches for this season
  season_key <- as.character(season)
  new_coach_teams <- COACHING_CHANGES[[season_key]]

  if (is.null(new_coach_teams)) {
    return(0)
  }

  # Check if this team has a new coach
  is_new_coach <- team %in% new_coach_teams

  if (!is_new_coach) {
    return(0)
  }

  # Uncertainty adjustment decreases through season
  # Weeks 1-4: High uncertainty (-0.5 points)
  # Weeks 5-8: Moderate uncertainty (-0.25 points)
  # Weeks 9+: No adjustment (team has established identity)
  if (week <= 4) {
    return(-0.5)
  } else if (week <= 8) {
    return(-0.25)
  }

  return(0)
}


#' Get Coaching Change Variance Multiplier
#'
#' Returns a variance multiplier for Monte Carlo simulations.
#' Teams with new coaches have more unpredictable outcomes.
#'
#' @param team Team abbreviation
#' @param season Season year
#' @param week Week number
#' @return Variance multiplier (>1 = more variance, 1 = normal)
#' @export
get_coaching_variance_mult <- function(team, season, week) {

  season_key <- as.character(season)
  new_coach_teams <- COACHING_CHANGES[[season_key]]

  if (is.null(new_coach_teams) || !(team %in% new_coach_teams)) {
    return(1.0)
  }

  # Higher variance early season, decreasing over time
  # Weeks 1-4: 1.3x variance
  # Weeks 5-8: 1.15x variance
  # Weeks 9+: Normal variance
  if (week <= 4) {
    return(1.3)
  } else if (week <= 8) {
    return(1.15)
  }

  return(1.0)
}


#' Check if Team Has New Coach
#'
#' @param team Team abbreviation
#' @param season Season year
#' @return TRUE if team has a new head coach this season
#' @export
has_new_coach <- function(team, season) {
  season_key <- as.character(season)
  new_coach_teams <- COACHING_CHANGES[[season_key]]

  if (is.null(new_coach_teams)) {
    return(FALSE)
  }

  team %in% new_coach_teams
}


#' Get All Teams with New Coaches
#'
#' @param season Season year
#' @return Vector of team abbreviations with new head coaches
#' @export
get_new_coach_teams <- function(season) {
  season_key <- as.character(season)
  COACHING_CHANGES[[season_key]] %||% character(0)
}


#' Add Coaching Change Data to Season
#'
#' Updates the COACHING_CHANGES list with a new season's data.
#' Call this at the start of each new season.
#'
#' @param season Season year
#' @param teams Vector of team abbreviations with new head coaches
#' @export
add_coaching_changes <- function(season, teams) {
  season_key <- as.character(season)
  COACHING_CHANGES[[season_key]] <<- teams
  invisible(TRUE)
}
