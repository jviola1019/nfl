# =============================================================================
# R/sleeper_api.R - Sleeper API Integration for Injury Data
# =============================================================================
#
# Provides real-time NFL injury data from the Sleeper fantasy API
# https://api.sleeper.app/v1/players/nfl
#
# Features:
#   - Free, no authentication required
#   - Real-time injury status updates
#   - Comprehensive player database with injury_status field
#
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

# =============================================================================
# CONFIGURATION
# =============================================================================

# Sleeper API endpoint
SLEEPER_PLAYERS_URL <- "https://api.sleeper.app/v1/players/nfl"

# Cache settings
SLEEPER_CACHE_DIR <- file.path(path.expand("~"), ".cache", "nfl_sim_sleeper")
SLEEPER_CACHE_EXPIRY_HOURS <- 4  # Refresh every 4 hours

# Team abbreviation mapping (Sleeper uses some different codes)
SLEEPER_TEAM_MAP <- c(
  "ARI" = "ARI", "ATL" = "ATL", "BAL" = "BAL", "BUF" = "BUF",

"CAR" = "CAR", "CHI" = "CHI", "CIN" = "CIN", "CLE" = "CLE",
  "DAL" = "DAL", "DEN" = "DEN", "DET" = "DET", "GB" = "GB",
  "HOU" = "HOU", "IND" = "IND", "JAC" = "JAX", "JAX" = "JAX",
  "KC" = "KC", "LA" = "LA", "LAC" = "LAC", "LAR" = "LA",
  "LV" = "LV", "MIA" = "MIA", "MIN" = "MIN", "NE" = "NE",
  "NO" = "NO", "NYG" = "NYG", "NYJ" = "NYJ", "PHI" = "PHI",
  "PIT" = "PIT", "SEA" = "SEA", "SF" = "SF", "TB" = "TB",
  "TEN" = "TEN", "WAS" = "WAS"
)

# Injury status mapping to availability score
SLEEPER_STATUS_MAP <- list(
  "Active" = 1.00,
  "Inactive" = 0.00,
  "Questionable" = 0.75,
  "Doubtful" = 0.25,
  "Out" = 0.00,
  "IR" = 0.00,
  "IR-R" = 0.00,  # IR - Designated to Return
  "PUP" = 0.00,
  "Sus" = 0.00,   # Suspended
  "NFI" = 0.00,   # Non-Football Injury
  "COV" = 0.00    # COVID
)

# =============================================================================
# MAIN FUNCTIONS
# =============================================================================

#' Load injury data from Sleeper API
#'
#' @param teams Vector of team abbreviations to filter (NULL = all teams)
#' @param use_cache Use cached data if available and fresh (default: TRUE)
#' @param verbose Print progress messages (default: TRUE)
#' @return List with: data (tibble), source ("sleeper"), message
#' @export
load_injuries_sleeper <- function(teams = NULL, use_cache = TRUE, verbose = TRUE) {

  result <- list(
    data = tibble::tibble(),
    source = "sleeper",
    message = NULL,
    success = FALSE
  )

  # Check cache first
  if (isTRUE(use_cache)) {
    cached <- load_sleeper_cache()
    if (!is.null(cached)) {
      if (verbose) message("Using cached Sleeper data")
      result$data <- filter_sleeper_injuries(cached, teams)
      result$message <- sprintf("Loaded %d injuries from Sleeper cache", nrow(result$data))
      result$success <- TRUE
      return(result)
    }
  }

  # Fetch from API
  if (verbose) message("Fetching player data from Sleeper API...")

  players_data <- fetch_sleeper_players()

  if (is.null(players_data)) {
    result$message <- "Failed to fetch data from Sleeper API"
    return(result)
  }

  # Save to cache
  save_sleeper_cache(players_data)

  # Filter to injured players
  result$data <- filter_sleeper_injuries(players_data, teams)
  result$message <- sprintf("Loaded %d injuries from Sleeper API", nrow(result$data))
  result$success <- TRUE

  if (verbose) message(result$message)

  result
}

#' Fetch all players from Sleeper API
#'
#' @return Named list of player data or NULL on failure
fetch_sleeper_players <- function() {

  tryCatch({
    # Use httr if available, otherwise base R
    if (requireNamespace("httr", quietly = TRUE)) {
      response <- httr::GET(
        SLEEPER_PLAYERS_URL,
        httr::timeout(30),
        httr::user_agent("NFL-Prediction-Model/2.6")
      )

      if (httr::status_code(response) != 200) {
        warning(sprintf("Sleeper API returned status %d", httr::status_code(response)))
        return(NULL)
      }

      content <- httr::content(response, as = "text", encoding = "UTF-8")
      players <- jsonlite::fromJSON(content, simplifyVector = FALSE)

    } else {
      # Fallback to base R
      con <- url(SLEEPER_PLAYERS_URL)
      on.exit(close(con))
      content <- paste(readLines(con, warn = FALSE), collapse = "")
      players <- jsonlite::fromJSON(content, simplifyVector = FALSE)
    }

    players

  }, error = function(e) {
    warning(sprintf("Failed to fetch Sleeper players: %s", conditionMessage(e)))
    NULL
  })
}

#' Filter Sleeper player data to injured players
#'
#' @param players_list Raw list from Sleeper API
#' @param teams Vector of team abbreviations to filter (NULL = all)
#' @return Tibble with standardized injury columns
filter_sleeper_injuries <- function(players_list, teams = NULL) {

  if (is.null(players_list) || length(players_list) == 0) {
    return(tibble::tibble(
      team = character(),
      player = character(),
      position = character(),
      injury_status = character(),
      injury_body_part = character(),
      injury_notes = character(),
      game_status = character(),
      availability = numeric(),
      source = character()
    ))
  }

  # Extract relevant fields from each player
  records <- lapply(names(players_list), function(player_id) {
    p <- players_list[[player_id]]

    # Skip if not NFL or no team
    if (is.null(p$team) || p$team == "" || is.null(p$sport) || p$sport != "nfl") {
      return(NULL)
    }

    # Get injury status
    injury_status <- p$injury_status %||% NA_character_

    # Skip if no injury (Active or NA)
    if (is.na(injury_status) || injury_status == "" || injury_status == "Active") {
      return(NULL)
    }

    # Map team abbreviation
    team_raw <- toupper(p$team)
    team <- SLEEPER_TEAM_MAP[team_raw] %||% team_raw

    # Get availability score
    availability <- SLEEPER_STATUS_MAP[[injury_status]] %||% 0.50

    tibble::tibble(
      team = as.character(team),
      player = paste(p$first_name %||% "", p$last_name %||% "") %>% trimws(),
      position = toupper(p$position %||% NA_character_),
      injury_status = injury_status,
      injury_body_part = p$injury_body_part %||% NA_character_,
      injury_notes = p$injury_notes %||% NA_character_,
      game_status = injury_status,  # Map to game_status for compatibility
      availability = availability,
      source = "sleeper"
    )
  })

  # Combine into tibble
  result <- dplyr::bind_rows(records)

  # Filter by teams if specified
  if (!is.null(teams) && length(teams) > 0) {
    teams_upper <- toupper(teams)
    result <- result %>% dplyr::filter(team %in% teams_upper)
  }

  # Sort by impact (injured players first, then by position importance)
  pos_order <- c("QB", "WR", "RB", "TE", "OL", "OT", "OG", "C", "EDGE", "DE", "DT", "DL", "LB", "CB", "S", "K", "P")
  result <- result %>%
    dplyr::mutate(
      pos_rank = match(position, pos_order, nomatch = 99)
    ) %>%
    dplyr::arrange(availability, pos_rank) %>%
    dplyr::select(-pos_rank)

  result
}

#' Convert Sleeper injury data to standard format
#'
#' @param sleeper_df Tibble from load_injuries_sleeper()
#' @return Tibble compatible with injury_scalp.R normalize_injury_report output
normalize_sleeper_injuries <- function(sleeper_df) {

  if (!is.data.frame(sleeper_df) || nrow(sleeper_df) == 0) {
    return(tibble::tibble(
      team = character(),
      player = character(),
      position = character(),
      practice_wed = character(),
      practice_thu = character(),
      practice_fri = character(),
      game_status = character(),
      injury_desc = character(),
      availability = numeric(),
      source = character()
    ))
  }

  sleeper_df %>%
    dplyr::transmute(
      team = team,
      player = player,
      position = position,
      practice_wed = NA_character_,  # Sleeper doesn't have practice data
      practice_thu = NA_character_,
      practice_fri = NA_character_,
      game_status = game_status,
      injury_desc = dplyr::coalesce(injury_body_part, injury_notes, injury_status),
      availability = availability,
      source = "sleeper"
    )
}

# =============================================================================
# CACHE FUNCTIONS
# =============================================================================

#' Get cache file path
get_sleeper_cache_path <- function() {
  if (!dir.exists(SLEEPER_CACHE_DIR)) {
    dir.create(SLEEPER_CACHE_DIR, recursive = TRUE, showWarnings = FALSE)
  }
  file.path(SLEEPER_CACHE_DIR, "sleeper_players.rds")
}

#' Load from cache if fresh
load_sleeper_cache <- function() {
  cache_file <- get_sleeper_cache_path()

  if (!file.exists(cache_file)) {
    return(NULL)
  }

  # Check age
  file_age_hours <- difftime(Sys.time(), file.mtime(cache_file), units = "hours")
  if (file_age_hours > SLEEPER_CACHE_EXPIRY_HOURS) {
    return(NULL)  # Cache expired
  }

  tryCatch({
    readRDS(cache_file)
  }, error = function(e) {
    warning(sprintf("Failed to read Sleeper cache: %s", conditionMessage(e)))
    NULL
  })
}

#' Save to cache
save_sleeper_cache <- function(data) {
  cache_file <- get_sleeper_cache_path()

  tryCatch({
    saveRDS(data, cache_file)
  }, error = function(e) {
    warning(sprintf("Failed to save Sleeper cache: %s", conditionMessage(e)))
  })
}

#' Clear cache
clear_sleeper_cache <- function() {
  cache_file <- get_sleeper_cache_path()
  if (file.exists(cache_file)) {
    file.remove(cache_file)
    message("Sleeper cache cleared")
  }
}

# =============================================================================
# UTILITY FUNCTIONS
# =============================================================================

#' Print injury summary for a team
#'
#' @param team Team abbreviation (e.g., "DEN")
#' @param data Optional pre-loaded injury data
#' @return Invisible tibble of injuries
print_team_injuries <- function(team, data = NULL) {

  if (is.null(data)) {
    result <- load_injuries_sleeper(teams = team, verbose = FALSE)
    data <- result$data
  } else {
    data <- data %>% dplyr::filter(team == !!toupper(team))
  }

  if (nrow(data) == 0) {
    message(sprintf("No injuries reported for %s", toupper(team)))
    return(invisible(data))
  }

  cat(sprintf("\n=== %s Injuries (%d players) ===\n", toupper(team), nrow(data)))

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    status_emoji <- switch(row$injury_status,
      "Out" = "[OUT]",
      "IR" = "[IR]",
      "IR-R" = "[IR-R]",
      "Doubtful" = "[DOUBT]",
      "Questionable" = "[Q]",
      "[?]"
    )

    injury_info <- if (!is.na(row$injury_body_part) && row$injury_body_part != "") {
      sprintf(" - %s", row$injury_body_part)
    } else {
      ""
    }

    cat(sprintf("  %s %s (%s)%s\n",
        status_emoji,
        row$player,
        row$position,
        injury_info))
  }
  cat("\n")

  invisible(data)
}

#' Get all injuries for current week (for NFLsimulation.R integration)
#'
#' @param teams Vector of team abbreviations on the slate
#' @return List compatible with load_injury_data() return format
get_current_injuries_sleeper <- function(teams = NULL) {

  result <- load_injuries_sleeper(teams = teams, use_cache = TRUE, verbose = TRUE)

  if (!result$success || nrow(result$data) == 0) {
    return(list(
      data = tibble::tibble(),
      mode_used = "sleeper (no data)",
      fallback_season = NULL,
      message = result$message
    ))
  }

  # Normalize to standard format
  normalized <- normalize_sleeper_injuries(result$data)

  list(
    data = normalized,
    mode_used = "sleeper",
    fallback_season = NULL,
    message = result$message,
    raw_data = result$data  # Keep raw for debugging
  )
}

# =============================================================================
# TEST FUNCTION
# =============================================================================

#' Test Sleeper API connection and show sample data
#'
#' @return TRUE if successful, FALSE otherwise
test_sleeper_api <- function() {
  cat("Testing Sleeper API connection...\n")

  # Clear cache for fresh test
  clear_sleeper_cache()

  # Try to load
  result <- load_injuries_sleeper(use_cache = FALSE, verbose = TRUE)

  if (!result$success) {
    cat("FAILED: ", result$message, "\n")
    return(FALSE)
  }

  cat("\nSUCCESS: Loaded", nrow(result$data), "injured players\n")

  # Show some examples
  if (nrow(result$data) > 0) {
    cat("\nSample injuries (first 10):\n")
    sample_data <- result$data %>%
      dplyr::select(team, player, position, injury_status, injury_body_part) %>%
      dplyr::slice_head(n = 10)
    print(sample_data, n = 10)

    # Show Denver specifically (user mentioned Bo Nix)
    cat("\n")
    print_team_injuries("DEN", result$data)
  }

  TRUE
}

# =============================================================================
# NULL-COALESCING OPERATOR (if not already defined)
# =============================================================================

if (!exists("%||%")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}
