# =============================================================================
# R/red_zone_data.R
# Red Zone Efficiency Data Loading
# =============================================================================
# Loads and aggregates red zone efficiency metrics for NFL teams.
# Red zone TD% is partially independent of general offensive efficiency and
# affects score distributions (TD vs FG scoring).
#
# Usage:
#   source("R/red_zone_data.R")
#   rz_data <- load_red_zone_efficiency(seasons = 2023:2024)
#
# =============================================================================

#' Load Red Zone Efficiency Data
#'
#' Calculates offensive and defensive red zone TD percentages by team.
#' Red zone is defined as plays inside the opponent's 20-yard line.
#'
#' @param seasons Vector of seasons to load (default: last 2 years)
#' @return Tibble with team, rz_attempts, rz_tds, rz_td_pct_off,
#'         rz_attempts_vs, rz_tds_vs, rz_td_pct_def
#' @export
load_red_zone_efficiency <- function(seasons = NULL) {

  # Default to last 2 seasons if not specified
  if (is.null(seasons)) {
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    seasons <- (current_year - 1):current_year
  }

  # Load play-by-play data
 pbp <- tryCatch({
    if (requireNamespace("nflreadr", quietly = TRUE)) {
      nflreadr::load_pbp(seasons)
    } else {
      message("nflreadr package required for red zone data")
      return(tibble::tibble())
    }
  }, error = function(e) {
    message(sprintf("Could not load PBP data for red zone: %s", conditionMessage(e)))
    return(tibble::tibble())
  })

  if (!inherits(pbp, "data.frame") || nrow(pbp) == 0) {
    return(tibble::tibble(
      team = character(),
      rz_attempts = integer(),
      rz_tds = integer(),
      rz_td_pct_off = numeric(),
      rz_attempts_vs = integer(),
      rz_tds_vs = integer(),
      rz_td_pct_def = numeric()
    ))
  }

  # Filter to red zone plays (inside opponent's 20)
  rz_plays <- pbp %>%
    dplyr::filter(
      !is.na(yardline_100),
      yardline_100 <= 20,
      !is.na(posteam),
      play_type %in% c("run", "pass")
    )

  if (nrow(rz_plays) == 0) {
    message("No red zone plays found in data")
    return(tibble::tibble())
  }

  # Calculate offensive red zone efficiency (TD%)
  rz_off <- rz_plays %>%
    dplyr::group_by(posteam) %>%
    dplyr::summarise(
      rz_attempts = dplyr::n(),
      rz_tds = sum(touchdown == 1, na.rm = TRUE),
      rz_td_pct_off = rz_tds / pmax(rz_attempts, 1),
      .groups = "drop"
    ) %>%
    dplyr::rename(team = posteam)

  # Calculate defensive red zone efficiency (TD% allowed)
  rz_def <- rz_plays %>%
    dplyr::group_by(defteam) %>%
    dplyr::summarise(
      rz_attempts_vs = dplyr::n(),
      rz_tds_vs = sum(touchdown == 1, na.rm = TRUE),
      rz_td_pct_def = rz_tds_vs / pmax(rz_attempts_vs, 1),
      .groups = "drop"
    ) %>%
    dplyr::rename(team = defteam)

  # Combine offensive and defensive metrics
  result <- dplyr::full_join(rz_off, rz_def, by = "team") %>%
    dplyr::mutate(
      # Replace NA with league average
      rz_td_pct_off = dplyr::coalesce(rz_td_pct_off, mean(rz_td_pct_off, na.rm = TRUE)),
      rz_td_pct_def = dplyr::coalesce(rz_td_pct_def, mean(rz_td_pct_def, na.rm = TRUE))
    )

  result
}


#' Get Red Zone Adjustment Factor
#'
#' Calculates a scoring adjustment based on red zone efficiency differential.
#' Teams that convert more TDs in the red zone have higher scoring potential.
#'
#' @param team Team abbreviation
#' @param rz_data Red zone efficiency data from load_red_zone_efficiency()
#' @param league_avg_off League average offensive RZ TD% (default 0.55)
#' @param league_avg_def League average defensive RZ TD% (default 0.55)
#' @return Point adjustment (positive = better than average RZ efficiency)
#' @export
get_red_zone_adjustment <- function(team, rz_data, league_avg_off = 0.55, league_avg_def = 0.55) {

  if (is.null(rz_data) || !inherits(rz_data, "data.frame") || nrow(rz_data) == 0) {
    return(0)
  }

  team_row <- rz_data %>% dplyr::filter(team == !!team)

  if (nrow(team_row) == 0) {
    return(0)
  }

  # RZ TD differential from league average
  # Better offensive RZ% = more points
  # Worse defensive RZ% = more points allowed
  off_diff <- (team_row$rz_td_pct_off - league_avg_off)
  def_diff <- (team_row$rz_td_pct_def - league_avg_def)

  # Each 10% differential ~ 0.7 points per game (heuristic based on scoring patterns)
  # Offensive advantage adds points, defensive disadvantage adds opponent points
  rz_adjustment <- (off_diff - def_diff) * 7

  # Clamp to reasonable range
  pmin(pmax(rz_adjustment, -2), 2)
}
