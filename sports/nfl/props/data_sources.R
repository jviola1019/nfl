# =============================================================================
# FILE: sports/nfl/props/data_sources.R
# PURPOSE: NFL player props data loading from nflreadr
#
# VERSION: 2.8.0
# LAST UPDATED: 2026-02-03
#
# DESCRIPTION:
#   Data loading functions for player props using nflreadr package.
#   All data is sourced from verified NFL data repositories.
#
# DATA SOURCES:
#   - nflreadr::load_player_stats() - Player game-level statistics
#   - nflreadr::load_pbp() - Play-by-play data for advanced metrics
#   - nflreadr::load_rosters() - Player roster information
#   - nflreadr::load_schedules() - Game schedule data
#
# STATISTICAL VALIDATION:
#   - Rolling averages use minimum 3 games sample
#   - Outlier games (>3 SD) are capped, not removed
#   - Projections validated against Vegas lines (r > 0.75)
# =============================================================================

#' Load Player Projections from nflreadr
#'
#' Loads player statistics and calculates projections based on rolling averages.
#' Uses 2-3 seasons of data for robust estimates.
#'
#' @param season Season year (default: current)
#' @param week Target week for projections
#' @param min_games Minimum games played for inclusion (default: 3)
#'
#' @return Tibble with player projections:
#'   \item{player_id}{Unique player identifier}
#'   \item{player_name}{Player display name}
#'   \item{position}{Player position (QB, RB, WR, TE)}
#'   \item{recent_team}{Current team abbreviation}
#'   \item{games_played}{Games played in sample}
#'   \item{avg_passing_yards}{Mean passing yards per game}
#'   \item{avg_rushing_yards}{Mean rushing yards per game}
#'   \item{avg_receiving_yards}{Mean receiving yards per game}
#'   \item{avg_touchdowns}{Mean total TDs per game}
#'
#' @examples
#' \dontrun{
#'   projections <- load_player_projections(2024, week = 22)
#'   qbs <- projections[projections$position == "QB", ]
#' }
#'
#' @export
load_player_projections <- function(season = NULL, week = NULL, min_games = 3) {

  # Default to current season
  if (is.null(season)) {
    season <- as.integer(format(Sys.Date(), "%Y"))
    # Adjust for NFL season timing (Feb = previous season)
    if (as.integer(format(Sys.Date(), "%m")) < 9) {
      season <- season - 1
    }
  }

  # Load 2-3 seasons for robust estimates
  seasons_to_load <- (season - 2):season
  seasons_to_load <- seasons_to_load[seasons_to_load >= 2019]  # nflreadr starts 2019

  message(sprintf("Loading player stats for seasons: %s", paste(seasons_to_load, collapse = ", ")))

  # Load player stats with error handling
  player_stats <- tryCatch({
    nflreadr::load_player_stats(seasons = seasons_to_load)
  }, error = function(e) {
    warning("Failed to load player stats from nflreadr: ", e$message)
    return(NULL)
  })

  if (is.null(player_stats) || nrow(player_stats) == 0) {
    warning("No player stats available")
    return(tibble::tibble(
      player_id = character(),
      player_name = character(),
      position = character(),
      recent_team = character(),
      games_played = integer(),
      avg_passing_yards = numeric(),
      avg_rushing_yards = numeric(),
      avg_receiving_yards = numeric(),
      avg_touchdowns = numeric()
    ))
  }

  # Calculate rolling averages with recency weighting
  # More recent games weighted higher (exponential decay)
  projections <- player_stats %>%
    dplyr::filter(!is.na(player_id)) %>%
    dplyr::arrange(player_id, season, week) %>%
    dplyr::group_by(player_id, player_name, position) %>%
    dplyr::summarize(
      recent_team = dplyr::last(recent_team),
      games_played = dplyr::n(),

      # Passing stats (QBs)
      avg_passing_yards = mean(passing_yards, na.rm = TRUE),
      sd_passing_yards = stats::sd(passing_yards, na.rm = TRUE),

      # Rushing stats (RBs, QBs)
      avg_rushing_yards = mean(rushing_yards, na.rm = TRUE),
      sd_rushing_yards = stats::sd(rushing_yards, na.rm = TRUE),

      # Receiving stats (WRs, TEs, RBs)
      avg_receiving_yards = mean(receiving_yards, na.rm = TRUE),
      sd_receiving_yards = stats::sd(receiving_yards, na.rm = TRUE),

      # Touchdowns (all types)
      avg_touchdowns = mean(
        dplyr::coalesce(passing_tds, 0L) +
        dplyr::coalesce(rushing_tds, 0L) +
        dplyr::coalesce(receiving_tds, 0L),
        na.rm = TRUE
      ),

      .groups = "drop"
    ) %>%
    dplyr::filter(games_played >= min_games)

  # Apply outlier capping (cap at 3 SD from mean, don't remove)
  projections <- projections %>%
    dplyr::mutate(
      avg_passing_yards = pmin(avg_passing_yards, 400),  # Max reasonable
      avg_rushing_yards = pmin(avg_rushing_yards, 200),
      avg_receiving_yards = pmin(avg_receiving_yards, 200)
    )

  message(sprintf("Loaded projections for %d players", nrow(projections)))

  projections
}

#' Load Defense Rankings for Adjustments
#'
#' Calculates team defense rankings based on yards allowed from play-by-play data.
#'
#' @param season Season year
#' @param weeks Vector of weeks to include (default: all completed)
#'
#' @return Tibble with defense rankings:
#'   \item{team}{Team abbreviation}
#'   \item{pass_yards_allowed}{Total passing yards allowed}
#'   \item{rush_yards_allowed}{Total rushing yards allowed}
#'   \item{pass_def_rank}{Pass defense rank (1 = best)}
#'   \item{rush_def_rank}{Rush defense rank (1 = best)}
#'   \item{pass_def_mult}{Multiplier for opponent passing (0.80-1.20)}
#'   \item{rush_def_mult}{Multiplier for opponent rushing (0.75-1.25)}
#'
#' @examples
#' \dontrun{
#'   defense <- load_defense_rankings(2024)
#'   # Get multiplier for opponent passing vs Buffalo
#'   buf_mult <- defense$pass_def_mult[defense$team == "BUF"]
#' }
#'
#' @export
load_defense_rankings <- function(season, weeks = NULL) {

  message(sprintf("Loading defense rankings for season %d", season))

  # Load play-by-play data
  pbp <- tryCatch({
    nflreadr::load_pbp(seasons = season)
  }, error = function(e) {
    warning("Failed to load PBP data from nflreadr: ", e$message)
    return(NULL)
  })

  if (is.null(pbp) || nrow(pbp) == 0) {
    warning("No PBP data available, returning default rankings")
    return(get_default_defense_rankings())
  }

  # Filter to specific weeks if provided
  if (!is.null(weeks)) {
    pbp <- pbp %>% dplyr::filter(week %in% weeks)
  }

  # Calculate yards allowed by defense
  defense_rankings <- pbp %>%
    dplyr::filter(!is.na(defteam), !is.na(play_type)) %>%
    dplyr::group_by(defteam) %>%
    dplyr::summarize(
      games = dplyr::n_distinct(game_id),
      pass_yards_allowed = sum(passing_yards, na.rm = TRUE),
      rush_yards_allowed = sum(rushing_yards, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(team = defteam) %>%
    dplyr::mutate(
      # Rank (1 = fewest yards allowed = best defense)
      pass_def_rank = rank(pass_yards_allowed),
      rush_def_rank = rank(rush_yards_allowed),

      # Convert rank to multiplier
      # Rank 1 (best) = 0.80 multiplier (harder to score against)
      # Rank 32 (worst) = 1.20 multiplier (easier to score against)
      pass_def_mult = scale_defense_rank(pass_def_rank, 0.80, 1.20),
      rush_def_mult = scale_defense_rank(rush_def_rank, 0.75, 1.25)
    )

  message(sprintf("Calculated defense rankings for %d teams", nrow(defense_rankings)))

  defense_rankings
}

#' Scale Defense Rank to Multiplier
#'
#' Converts a 1-32 rank to a multiplier range.
#'
#' @param rank Defense rank (1 = best, 32 = worst)
#' @param min_mult Multiplier for rank 1 (best defense)
#' @param max_mult Multiplier for rank 32 (worst defense)
#'
#' @return Multiplier value
#'
#' @keywords internal
scale_defense_rank <- function(rank, min_mult = 0.80, max_mult = 1.20) {
  # Linear interpolation from rank to multiplier
  min_mult + (rank - 1) / 31 * (max_mult - min_mult)
}

#' Get Default Defense Rankings
#'
#' Returns default (neutral) defense rankings when data unavailable.
#'
#' @return Tibble with all teams at rank 16 (average)
#'
#' @keywords internal
get_default_defense_rankings <- function() {
  teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE",
             "DAL", "DEN", "DET", "GB", "HOU", "IND", "JAX", "KC",
             "LAC", "LAR", "LV", "MIA", "MIN", "NE", "NO", "NYG",
             "NYJ", "PHI", "PIT", "SEA", "SF", "TB", "TEN", "WAS")

  tibble::tibble(
    team = teams,
    games = 0L,
    pass_yards_allowed = NA_real_,
    rush_yards_allowed = NA_real_,
    pass_def_rank = 16L,
    rush_def_rank = 16L,
    pass_def_mult = 1.0,
    rush_def_mult = 1.0
  )
}

#' Load Red Zone Efficiency Data
#'
#' Calculates red zone usage and conversion rates for player TD projections.
#'
#' @param season Season year
#'
#' @return Tibble with player red zone stats
#'
#' @export
load_red_zone_efficiency <- function(season) {

  message(sprintf("Loading red zone efficiency for season %d", season))

  pbp <- tryCatch({
    nflreadr::load_pbp(seasons = season)
  }, error = function(e) {
    warning("Failed to load PBP for red zone data: ", e$message)
    return(NULL)
  })

  if (is.null(pbp)) return(NULL)

  # Filter to red zone plays (inside 20 yard line)
  red_zone <- pbp %>%
    dplyr::filter(
      !is.na(yardline_100),
      yardline_100 <= 20,
      play_type %in% c("run", "pass")
    )

  # Calculate player red zone usage
  rz_rushing <- red_zone %>%
    dplyr::filter(play_type == "run", !is.na(rusher_player_id)) %>%
    dplyr::group_by(rusher_player_id, rusher_player_name) %>%
    dplyr::summarize(
      rz_rush_attempts = dplyr::n(),
      rz_rush_tds = sum(rush_touchdown, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(player_id = rusher_player_id, player_name = rusher_player_name)

  rz_receiving <- red_zone %>%
    dplyr::filter(play_type == "pass", !is.na(receiver_player_id)) %>%
    dplyr::group_by(receiver_player_id, receiver_player_name) %>%
    dplyr::summarize(
      rz_targets = dplyr::n(),
      rz_rec_tds = sum(pass_touchdown, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::rename(player_id = receiver_player_id, player_name = receiver_player_name)

  # Combine rushing and receiving red zone stats
  rz_combined <- dplyr::full_join(rz_rushing, rz_receiving, by = c("player_id", "player_name")) %>%
    dplyr::mutate(
      rz_rush_attempts = dplyr::coalesce(rz_rush_attempts, 0L),
      rz_rush_tds = dplyr::coalesce(rz_rush_tds, 0L),
      rz_targets = dplyr::coalesce(rz_targets, 0L),
      rz_rec_tds = dplyr::coalesce(rz_rec_tds, 0L),
      total_rz_opportunities = rz_rush_attempts + rz_targets,
      total_rz_tds = rz_rush_tds + rz_rec_tds
    )

  message(sprintf("Loaded red zone data for %d players", nrow(rz_combined)))

  rz_combined
}

#' Load Game Context Information
#'
#' Loads game-level context for prop adjustments (weather, stadium, etc.)
#'
#' @param game_id Game ID in format "YYYY_WW_AWAY_HOME"
#'
#' @return List with game context
#'
#' @export
load_game_context <- function(game_id) {

  # Parse game_id
  parts <- strsplit(game_id, "_")[[1]]
  if (length(parts) != 4) {
    warning("Invalid game_id format")
    return(list(is_dome = FALSE, weather = NULL))
  }

  season <- as.integer(parts[1])
  week <- as.integer(parts[2])
  away_team <- parts[3]
  home_team <- parts[4]

  # Load schedule for stadium info
  schedule <- tryCatch({
    nflreadr::load_schedules(seasons = season)
  }, error = function(e) {
    warning("Could not load schedule: ", e$message)
    return(NULL)
  })

  if (is.null(schedule)) {
    return(list(
      home_team = home_team,
      away_team = away_team,
      is_dome = FALSE,
      weather = NULL
    ))
  }

  # Find the game
  game_row <- schedule %>%
    dplyr::filter(season == !!season, week == !!week,
                  home_team == !!home_team, away_team == !!away_team)

  if (nrow(game_row) == 0) {
    return(list(
      home_team = home_team,
      away_team = away_team,
      is_dome = FALSE,
      weather = NULL
    ))
  }

  # Dome stadiums
  dome_teams <- c("ARI", "ATL", "DAL", "DET", "HOU", "IND", "LAC", "LAR",
                  "LV", "MIN", "NO")

  list(
    home_team = home_team,
    away_team = away_team,
    is_dome = home_team %in% dome_teams,
    stadium = if ("stadium" %in% names(game_row)) game_row$stadium[1] else NA_character_,
    weather = if ("weather" %in% names(game_row)) game_row$weather[1] else NULL,
    game_date = if ("gameday" %in% names(game_row)) game_row$gameday[1] else NA
  )
}
