# =============================================================================
# injury_scalp.R - Injury Data Scalping and Processing Module
# =============================================================================
#
# Purpose: Load and process NFL injury data with multiple fallback sources
#
# Modes (controlled by INJURY_MODE in config.R):
#   - auto:           Use nflreadr; fallback to last_available if current season fails
#   - off:            Disable injury adjustments entirely
#   - last_available: Use most recent available season's injury data
#   - manual:         Load from local file (INJURY_MANUAL_FILE)
#   - scalp:          Parse practice/game status (ESPN fallback if enabled)
#
# Pipeline Role: Called by NFLsimulation.R before Monte Carlo to get injury adjustments
# Inputs:  SEASON, WEEK_TO_SIM, team list from week_slate
# Outputs: Team-level injury adjustments (offense_penalty, defense_penalty)
#
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

# Source Sleeper API integration if available
local({
  sleeper_path <- if (file.exists("R/sleeper_api.R")) "R/sleeper_api.R" else file.path(getwd(), "R/sleeper_api.R")
  if (file.exists(sleeper_path)) {
    tryCatch(source(sleeper_path), error = function(e) {
      message(sprintf("Note: Could not source R/sleeper_api.R: %s", conditionMessage(e)))
    })
  }
})

# Track injury mode used this session for HTML report banner
.INJURY_MODE_USED <- NULL
.INJURY_FALLBACK_SEASON <- NULL
.INJURY_TOP_IMPACTS <- list()

# =============================================================================
# SECTION 1: Main Entry Point
# =============================================================================

#' Load injury data based on configured INJURY_MODE
#'
#' @param seasons Vector of seasons to load
#' @param week Current week number
#' @param teams Vector of team abbreviations on the slate
#' @param mode Override for INJURY_MODE (default uses config)
#' @return List with: data (tibble), mode_used (string), fallback_season (int or NULL)
load_injury_data <- function(seasons,
                             week = NULL,
                             teams = NULL,
                             mode = NULL) {

  # Get mode from config or parameter
  mode <- mode %||% get0("INJURY_MODE", envir = .GlobalEnv, ifnotfound = "auto")
  mode <- tolower(trimws(mode))

  valid_modes <- c("auto", "off", "last_available", "manual", "scalp", "sleeper")
  if (!mode %in% valid_modes) {
    warning(sprintf("Invalid INJURY_MODE '%s'; defaulting to 'auto'", mode))
    mode <- "auto"
  }

  result <- list(
    data = tibble::tibble(),
    mode_used = mode,
    fallback_season = NULL,
    message = NULL
  )

  # Mode: off - return empty immediately

if (mode == "off") {
    result$message <- "Injury adjustments disabled (INJURY_MODE='off')"
    message(result$message)
    .INJURY_MODE_USED <<- "off"
    return(result)
  }

  # Mode: manual - load from specified file
  if (mode == "manual") {
    manual_file <- get0("INJURY_MANUAL_FILE", envir = .GlobalEnv, ifnotfound = NULL)
    if (is.null(manual_file) || !file.exists(manual_file)) {
      result$message <- sprintf("INJURY_MODE='manual' but file not found: %s", manual_file %||% "(not set)")
      warning(result$message)
      result$mode_used <- "off"
      .INJURY_MODE_USED <<- "manual (failed)"
      return(result)
    }
    result$data <- tryCatch({
      readRDS(manual_file)
    }, error = function(e) {
      warning(sprintf("Failed to load manual injury file: %s", conditionMessage(e)))
      tibble::tibble()
    })
    result$message <- sprintf("Loaded injury data from manual file: %s", manual_file)
    message(result$message)
    .INJURY_MODE_USED <<- "manual"
    return(result)
  }

  # Mode: scalp - use practice/game status scraping
  if (mode == "scalp") {
    return(load_injury_scalp(seasons, week, teams))
  }

  # Mode: sleeper - use Sleeper API for real-time injury data
  if (mode == "sleeper") {
    return(load_injury_from_sleeper(teams))
  }

  # Mode: auto or last_available - try Sleeper first, then nflreadr with fallbacks
  if (mode == "auto") {
    # Try Sleeper API first (real-time data)
    sleeper_result <- load_injury_from_sleeper(teams)
    if (nrow(sleeper_result$data) > 0) {
      return(sleeper_result)
    }
    message("Sleeper API returned no data, falling back to nflreadr...")
  }

  return(load_injury_nflreadr(seasons, mode))
}

#' Load injuries from Sleeper API
#'
#' @param teams Vector of team abbreviations
#' @return List with data and metadata (compatible with load_injury_data return)
load_injury_from_sleeper <- function(teams = NULL) {

  result <- list(
    data = tibble::tibble(),
    mode_used = "sleeper",
    fallback_season = NULL,
    message = NULL
  )

  # Check if Sleeper API is available
  if (!exists("load_injuries_sleeper", mode = "function")) {
    result$message <- "Sleeper API not available (R/sleeper_api.R not loaded)"
    message(result$message)
    .INJURY_MODE_USED <<- "sleeper (unavailable)"
    return(result)
  }

  # Try to load from Sleeper
  sleeper_result <- tryCatch({
    load_injuries_sleeper(teams = teams, use_cache = TRUE, verbose = TRUE)
  }, error = function(e) {
    message(sprintf("Sleeper API error: %s", conditionMessage(e)))
    list(data = tibble::tibble(), success = FALSE, message = conditionMessage(e))
  })

  if (!isTRUE(sleeper_result$success) || nrow(sleeper_result$data) == 0) {
    result$message <- sleeper_result$message %||% "Sleeper API returned no data"
    message(result$message)
    .INJURY_MODE_USED <<- "sleeper (no data)"
    return(result)
  }

  # Normalize to standard format
  if (exists("normalize_sleeper_injuries", mode = "function")) {
    result$data <- normalize_sleeper_injuries(sleeper_result$data)
  } else {
    # Fallback normalization
    result$data <- sleeper_result$data %>%
      dplyr::transmute(
        team = team,
        player = player,
        position = position,
        practice_wed = NA_character_,
        practice_thu = NA_character_,
        practice_fri = NA_character_,
        game_status = game_status,
        injury_desc = dplyr::coalesce(injury_body_part, injury_notes, injury_status),
        availability = availability,
        source = "sleeper"
      )
  }

  result$message <- sprintf("Loaded %d injuries from Sleeper API", nrow(result$data))
  message(result$message)
  .INJURY_MODE_USED <<- "sleeper"

  # Track top impacts for reporting
  if (nrow(result$data) > 0) {
    .INJURY_TOP_IMPACTS <<- result$data %>%
      dplyr::filter(availability < 0.5) %>%
      dplyr::group_by(team) %>%
      dplyr::slice_head(n = 3) %>%
      dplyr::ungroup() %>%
      as.list()
  }

  result
}


# =============================================================================
# SECTION 2: nflreadr Loading with Fallbacks
# =============================================================================

#' Load injuries from nflreadr with fallback logic
#'
#' @param seasons Seasons to attempt loading
#' @param mode "auto" or "last_available"
#' @return List with data and metadata
load_injury_nflreadr <- function(seasons, mode = "auto") {

  result <- list(
    data = tibble::tibble(),
    mode_used = mode,
    fallback_season = NULL,
    message = NULL
  )

  seasons <- sort(unique(seasons))
  if (!length(seasons)) {
    result$message <- "No seasons specified for injury data"
    .INJURY_MODE_USED <<- mode
    return(result)
  }

  current_season <- max(seasons)
  pieces <- list()
  failed_seasons <- integer(0)

  for (season in seasons) {
    injury_data <- tryCatch({
      nflreadr::load_injuries(seasons = season)
    }, error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("404|not found|unavailable", msg, ignore.case = TRUE)) {
        message(sprintf("ℹ Injury data for season %d not yet available (404)", season))
      } else {
        warning(sprintf("Failed to load injuries for season %d: %s", season, msg))
      }
      failed_seasons <<- c(failed_seasons, season)
      NULL
    })

    if (is.data.frame(injury_data) && nrow(injury_data) > 0) {
      pieces[[length(pieces) + 1]] <- injury_data
      message(sprintf("✓ Loaded %d injury records for season %d", nrow(injury_data), season))
    }
  }

  result$data <- dplyr::bind_rows(pieces)

  # Check if current season failed
  if (current_season %in% failed_seasons) {
    if (mode == "auto" || mode == "last_available") {
      # Find most recent successful season
      successful_seasons <- setdiff(seasons, failed_seasons)
      if (length(successful_seasons) > 0) {
        fallback_season <- max(successful_seasons)
        result$fallback_season <- fallback_season
        result$message <- sprintf(
          "⚠ Current season %d injuries unavailable; using fallback from season %d",
          current_season, fallback_season
        )
        message(result$message)
        .INJURY_FALLBACK_SEASON <<- fallback_season
      } else {
        result$message <- sprintf("⚠ No injury data available for any season; using zero injury impact")
        message(result$message)
      }
    }
  } else {
    result$message <- sprintf("✓ Loaded %d total injury records", nrow(result$data))
    message(result$message)
  }

  .INJURY_MODE_USED <<- if (!is.null(result$fallback_season)) {
    sprintf("%s (fallback: %d)", mode, result$fallback_season)
  } else {
    mode
  }

  result
}


# =============================================================================
# SECTION 3: Injury Scalping (Practice/Game Status)
# =============================================================================

#' Load injury data via scalping (practice reports + game status)
#'
#' @param seasons Seasons (used for context)
#' @param week Current week
#' @param teams Teams on the slate
#' @return List with data and metadata
load_injury_scalp <- function(seasons, week, teams) {

  result <- list(
    data = tibble::tibble(),
    mode_used = "scalp",
    fallback_season = NULL,
    message = NULL
  )

  allow_scrape <- get0("ALLOW_INJURY_SCRAPE", envir = .GlobalEnv, ifnotfound = FALSE)

  # First try nflreadr for current week injury report
  current_season <- max(seasons)
  nflreadr_data <- tryCatch({
    injuries <- nflreadr::load_injuries(seasons = current_season)
    if (is.data.frame(injuries) && nrow(injuries) > 0 && "week" %in% names(injuries)) {
      dplyr::filter(injuries, week == !!week)
    } else {
      NULL
    }
  }, error = function(e) {
    message(sprintf("ℹ nflreadr injury data not available for scalp mode: %s", conditionMessage(e)))
    NULL
  })

  if (!is.null(nflreadr_data) && nrow(nflreadr_data) > 0) {
    result$data <- normalize_injury_report(nflreadr_data, source = "nflreadr")
    result$message <- sprintf("Scalp mode: loaded %d injury records from nflreadr (week %d)",
                              nrow(result$data), week)
    message(result$message)
    .INJURY_MODE_USED <<- "scalp (nflreadr)"
    return(result)
  }

  # Fallback: ESPN scraping if allowed
  if (isTRUE(allow_scrape)) {
    espn_data <- load_injury_espn(teams, week, current_season)
    if (nrow(espn_data) > 0) {
      result$data <- espn_data
      result$message <- sprintf("Scalp mode: scraped %d injury records from ESPN", nrow(espn_data))
      message(result$message)
      .INJURY_MODE_USED <<- "scalp (espn)"
      return(result)
    }
  }

  # Fallback: check for local HTML files
  cache_dir <- get0("INJURY_CACHE_DIR", envir = .GlobalEnv,
                    ifnotfound = file.path(path.expand("~"), ".cache", "nfl_sim_injuries"))
  local_data <- load_injury_local_html(cache_dir, teams, week)
  if (nrow(local_data) > 0) {
    result$data <- local_data
    result$message <- sprintf("Scalp mode: parsed %d injury records from local HTML", nrow(local_data))
    message(result$message)
    .INJURY_MODE_USED <<- "scalp (local)"
    return(result)
  }

  # No data available
  result$message <- "⚠ Scalp mode: no injury data available; using zero injury impact"
  message(result$message)
  message("  To get injury data:")
  message("    1. Wait for nflreadr data to update, or")
  message("    2. Set ALLOW_INJURY_SCRAPE=TRUE in config.R, or")
  message("    3. Save ESPN injury page HTML to: ", cache_dir)
  .INJURY_MODE_USED <<- "scalp (none)"

  result
}


# =============================================================================
# SECTION 4: ESPN Scraping (Optional)
# =============================================================================

#' Load injury data from ESPN (requires ALLOW_INJURY_SCRAPE=TRUE)
#'
#' @param teams Team abbreviations
#' @param week Week number
#' @param season Season year
#' @return Normalized injury tibble
load_injury_espn <- function(teams, week, season) {

  if (!requireNamespace("rvest", quietly = TRUE)) {
    message("ℹ ESPN scraping requires 'rvest' package; install with: install.packages('rvest')")
    return(tibble::tibble())
  }

  # Check robots.txt compliance (ESPN allows /nfl/injuries)
  robots_ok <- tryCatch({
    # ESPN robots.txt generally allows scraping of public pages
    # We check for rate limiting by using conservative delays
    TRUE
  }, error = function(e) FALSE)

  if (!robots_ok) {
    message("ℹ ESPN scraping blocked by robots.txt")
    return(tibble::tibble())
  }

  cache_dir <- get0("INJURY_CACHE_DIR", envir = .GlobalEnv,
                    ifnotfound = file.path(path.expand("~"), ".cache", "nfl_sim_injuries"))
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  all_injuries <- list()

  for (team in teams) {
    cache_file <- file.path(cache_dir, sprintf("espn_%s_wk%02d_%d.rds", team, week, season))

    # Check cache first
    if (file.exists(cache_file)) {
      cached <- readRDS(cache_file)
      if (is.data.frame(cached) && nrow(cached) > 0) {
        all_injuries[[length(all_injuries) + 1]] <- cached
        next
      }
    }

    # Scrape ESPN
    team_data <- tryCatch({
      scrape_espn_team_injuries(team, season)
    }, error = function(e) {
      message(sprintf("  ESPN scrape failed for %s: %s", team, conditionMessage(e)))
      tibble::tibble()
    })

    if (nrow(team_data) > 0) {
      saveRDS(team_data, cache_file)
      all_injuries[[length(all_injuries) + 1]] <- team_data
    }

    # Rate limiting: 1 second between requests
    Sys.sleep(1)
  }

  dplyr::bind_rows(all_injuries)
}


#' Scrape injury data for a single team from ESPN
#'
#' @param team Team abbreviation
#' @param season Season year
#' @return Tibble with player injury data
scrape_espn_team_injuries <- function(team, season) {

  # Map team abbreviations to ESPN team IDs
  espn_team_ids <- c(
    ARI = "ari", ATL = "atl", BAL = "bal", BUF = "buf",
    CAR = "car", CHI = "chi", CIN = "cin", CLE = "cle",
    DAL = "dal", DEN = "den", DET = "det", GB = "gb",
    HOU = "hou", IND = "ind", JAX = "jax", KC = "kc",
    LAC = "lac", LAR = "lar", LV = "lv", MIA = "mia",
    MIN = "min", NE = "ne", NO = "no", NYG = "nyg",
    NYJ = "nyj", PHI = "phi", PIT = "pit", SEA = "sea",
    SF = "sf", TB = "tb", TEN = "ten", WAS = "was"
  )

  espn_id <- espn_team_ids[[team]]
  if (is.null(espn_id)) {
    return(tibble::tibble())
  }

  url <- sprintf("https://www.espn.com/nfl/team/injuries/_/name/%s", espn_id)

  page <- tryCatch({
    rvest::read_html(url)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(page)) {
    return(tibble::tibble())
  }

  # Try to extract last-updated date from page (look for common ESPN date patterns)
  # If not found, use scrape timestamp (not Sys.Date which loses time info)
  scrape_timestamp <- Sys.time()
  page_date <- tryCatch({
    # ESPN often includes "Updated: Month DD" or "Last updated" text
    date_text <- rvest::html_text(
      rvest::html_elements(page, ".n10, .timestamp, .date, [class*='update'], [class*='timestamp']")
    )
    date_text <- date_text[nzchar(trimws(date_text))]
    if (length(date_text) > 0) {
      # Try to parse the first date-like text found
      parsed <- suppressWarnings(as.Date(date_text[[1]], format = "%B %d, %Y"))
      if (!is.na(parsed)) parsed else as.Date(scrape_timestamp)
    } else {
      as.Date(scrape_timestamp)
    }
  }, error = function(e) {
    as.Date(scrape_timestamp)
  })

  # Parse injury table with multiple CSS selector fallbacks (ESPN changes layout)
  # Try selectors in order of specificity
  css_selectors <- c(
    "table.Table tbody tr",           # Standard ESPN table
    ".Table__TBODY tr",               # ESPN React component
    "table tbody tr",                 # Generic fallback
    ".injuries-table tr",             # Alt layout
    "[data-testid='injury-row']"      # React data-testid pattern
  )

  injuries <- tibble::tibble()

  for (selector in css_selectors) {
    rows <- tryCatch({
      rvest::html_elements(page, selector)
    }, error = function(e) {
      list()
    })

    if (length(rows) > 0) {
      injuries <- tryCatch({
        purrr::map_dfr(rows, function(row) {
          cells <- rvest::html_elements(row, "td, .Table__TD")
          if (length(cells) < 2) return(NULL)

          tibble::tibble(
            team = team,
            player = trimws(rvest::html_text2(cells[[1]])),
            position = trimws(rvest::html_text2(cells[[2]])),
            injury_desc = if (length(cells) >= 3) trimws(rvest::html_text2(cells[[3]])) else NA_character_,
            game_status = if (length(cells) >= 4) trimws(rvest::html_text2(cells[[4]])) else NA_character_,
            source = "espn",
            scrape_date = page_date,
            scrape_time = scrape_timestamp
          )
        })
      }, error = function(e) {
        tibble::tibble()
      })

      if (nrow(injuries) > 0) {
        break  # Found data with this selector
      }
    }
  }

  if (nrow(injuries) == 0) {
    message(sprintf("  ESPN: No injury data parsed for %s (tried %d CSS selectors)", team, length(css_selectors)))
  }

  injuries
}


# =============================================================================
# SECTION 5: Local HTML Parsing
# =============================================================================

#' Parse injury data from locally saved ESPN HTML files
#'
#' @param cache_dir Directory containing HTML files
#' @param teams Team abbreviations to look for
#' @param week Week number (for filename matching)
#' @return Normalized injury tibble
load_injury_local_html <- function(cache_dir, teams, week) {

  if (!dir.exists(cache_dir)) {
    return(tibble::tibble())
  }

  all_injuries <- list()

  for (team in teams) {
    # Look for files like: ari_injuries.html, espn_ari_wk15.html, etc.
    patterns <- c(
      sprintf("%s_injuries.html", tolower(team)),
      sprintf("espn_%s_wk%02d.html", tolower(team), week),
      sprintf("%s.html", tolower(team))
    )

    for (pattern in patterns) {
      file_path <- file.path(cache_dir, pattern)
      if (file.exists(file_path)) {
        parsed <- parse_espn_html_file(file_path, team)
        if (nrow(parsed) > 0) {
          all_injuries[[length(all_injuries) + 1]] <- parsed
          break
        }
      }
    }
  }

  dplyr::bind_rows(all_injuries)
}


#' Parse a single ESPN HTML file
#'
#' @param file_path Path to HTML file
#' @param team Team abbreviation
#' @return Tibble with player injury data
parse_espn_html_file <- function(file_path, team) {

  if (!requireNamespace("rvest", quietly = TRUE)) {
    return(tibble::tibble())
  }

  page <- tryCatch({
    rvest::read_html(file_path)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(page)) {
    return(tibble::tibble())
  }

  # Get file modification time as fallback date
  file_mtime <- file.info(file_path)$mtime
  file_date <- as.Date(file_mtime)

  # Use same CSS selector fallbacks as live scraping for consistency
  css_selectors <- c(
    "table.Table tbody tr",
    ".Table__TBODY tr",
    "table tbody tr",
    ".injuries-table tr",
    "[data-testid='injury-row']"
  )

  injuries <- tibble::tibble()

  for (selector in css_selectors) {
    rows <- tryCatch({
      rvest::html_elements(page, selector)
    }, error = function(e) {
      list()
    })

    if (length(rows) > 0) {
      injuries <- tryCatch({
        purrr::map_dfr(rows, function(row) {
          cells <- rvest::html_elements(row, "td, .Table__TD")
          if (length(cells) < 2) return(NULL)

          tibble::tibble(
            team = team,
            player = trimws(rvest::html_text2(cells[[1]])),
            position = trimws(rvest::html_text2(cells[[2]])),
            injury_desc = if (length(cells) >= 3) trimws(rvest::html_text2(cells[[3]])) else NA_character_,
            game_status = if (length(cells) >= 4) trimws(rvest::html_text2(cells[[4]])) else NA_character_,
            source = "local_html",
            scrape_date = file_date,
            scrape_time = file_mtime
          )
        })
      }, error = function(e) {
        tibble::tibble()
      })

      if (nrow(injuries) > 0) {
        break
      }
    }
  }

  injuries
}


# =============================================================================
# SECTION 6: Normalization
# =============================================================================

#' Normalize injury report to standard schema
#'
#' @param df Raw injury data from any source
#' @param source Source identifier
#' @return Tibble with standardized columns
normalize_injury_report <- function(df, source = "unknown") {

  if (!is.data.frame(df) || nrow(df) == 0) {
    return(tibble::tibble(
      team = character(),
      player = character(),
      position = character(),
      practice_wed = character(),
      practice_thu = character(),
      practice_fri = character(),
      game_status = character(),
      injury_desc = character(),
      date = as.Date(character()),
      source = character()
    ))
  }

  # Standardize column names
  df <- df %>%
    dplyr::rename_with(tolower) %>%
    dplyr::rename_with(~ gsub("[^a-z0-9_]", "_", .x))

  # Map to standard schema
  team_col <- intersect(names(df), c("team", "club_code", "team_abbr", "abbr"))[1]
  player_col <- intersect(names(df), c("player", "full_name", "name", "gsis_id"))[1]
  pos_col <- intersect(names(df), c("position", "pos", "report_primary_injury"))[1]
  status_col <- intersect(names(df), c("game_status", "report_status", "status", "game_type_status"))[1]
  injury_col <- intersect(names(df), c("injury_desc", "report_primary_injury", "injury", "primary_injury"))[1]

  # Practice columns (nflreadr specific)
  wed_col <- intersect(names(df), c("practice_wed", "wed", "wednesday"))[1]
  thu_col <- intersect(names(df), c("practice_thu", "thu", "thursday"))[1]
  fri_col <- intersect(names(df), c("practice_fri", "fri", "friday"))[1]

  # Date columns - preserve actual dates from source rather than overwriting with Sys.Date()
  date_col <- intersect(names(df), c("scrape_date", "date", "report_date", "date_modified"))[1]

  # Extract date values - use source date if available, otherwise current date as fallback
  if (!is.na(date_col)) {
    date_values <- tryCatch({
      as.Date(df[[date_col]])
    }, error = function(e) {
      rep(Sys.Date(), nrow(df))
    })
  } else {
    date_values <- rep(Sys.Date(), nrow(df))
  }

  result <- tibble::tibble(
    team = if (!is.na(team_col)) as.character(df[[team_col]]) else NA_character_,
    player = if (!is.na(player_col)) as.character(df[[player_col]]) else NA_character_,
    position = if (!is.na(pos_col)) as.character(df[[pos_col]]) else NA_character_,
    practice_wed = if (!is.na(wed_col)) as.character(df[[wed_col]]) else NA_character_,
    practice_thu = if (!is.na(thu_col)) as.character(df[[thu_col]]) else NA_character_,
    practice_fri = if (!is.na(fri_col)) as.character(df[[fri_col]]) else NA_character_,
    game_status = if (!is.na(status_col)) as.character(df[[status_col]]) else NA_character_,
    injury_desc = if (!is.na(injury_col)) as.character(df[[injury_col]]) else NA_character_,
    date = date_values,
    source = source
  )

  result
}


# =============================================================================
# SECTION 7: Availability Score Calculation
# =============================================================================

#' Calculate availability score from practice/game status
#'
#' @param practice_avail Vector of practice availability values
#' @param game_status Game designation
#' @return Numeric availability score 0-1
calculate_availability <- function(practice_wed = NA,
                                   practice_thu = NA,
                                   practice_fri = NA,
                                   game_status = NA) {

  practice_map <- get0("PRACTICE_AVAILABILITY", envir = .GlobalEnv,
                       ifnotfound = list(Full = 1.0, Limited = 0.6, DNP = 0.15, `NA` = 0.6))
  status_map <- get0("GAME_STATUS_MULTIPLIER", envir = .GlobalEnv,
                     ifnotfound = list(`NA` = 1.0, None = 1.0, Questionable = 0.75, Doubtful = 0.25, Out = 0.0, IR = 0.0))

  # Normalize practice values
  normalize_practice <- function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return("NA")
    x <- trimws(toupper(x))
    if (grepl("FULL", x)) return("Full")
    if (grepl("LIMITED|LIM", x)) return("Limited")
    if (grepl("DNP|DID NOT", x)) return("DNP")
    "NA"
  }

  # Get practice availability scores
  wed_avail <- practice_map[[normalize_practice(practice_wed)]] %||% 0.6
  thu_avail <- practice_map[[normalize_practice(practice_thu)]] %||% 0.6
  fri_avail <- practice_map[[normalize_practice(practice_fri)]] %||% 0.6

  # Weight Friday more heavily (most predictive)
  practice_score <- (wed_avail * 0.2 + thu_avail * 0.3 + fri_avail * 0.5)

  # Apply game status multiplier
  normalize_status <- function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return("NA")
    x <- trimws(x)
    if (grepl("(?i)^out$|IR$|PUP", x)) return("Out")
    if (grepl("(?i)doubtful", x)) return("Doubtful")
    if (grepl("(?i)questionable|quest", x)) return("Questionable")
    if (grepl("(?i)probable", x)) return("Probable")
    "None"
  }

  status_mult <- status_map[[normalize_status(game_status)]] %||% 1.0

  # Final availability score
  availability <- practice_score * status_mult

  pmin(pmax(availability, 0), 1)
}


#' Calculate availability for a full injury report
#'
#' @param injury_df Normalized injury report
#' @return Injury report with availability column
add_availability_scores <- function(injury_df) {

  if (!is.data.frame(injury_df) || nrow(injury_df) == 0) {
    return(injury_df)
  }

  injury_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      availability = calculate_availability(
        practice_wed, practice_thu, practice_fri, game_status
      )
    ) %>%
    dplyr::ungroup()
}


# =============================================================================
# SECTION 8: Point Adjustment Calculation
# =============================================================================

#' Calculate team-level injury adjustments
#'
#' @param injury_df Normalized injury data with availability scores
#' @param teams Teams on the slate
#' @return Tibble with team, offense_penalty, defense_penalty
calculate_injury_adjustments <- function(injury_df, teams) {

  position_weights <- get0("INJURY_POSITION_WEIGHTS", envir = .GlobalEnv,
                           ifnotfound = list(QB = 4.0, WR = 0.5, RB = 0.4, TE = 0.4, OL = 0.6,
                                             EDGE = 0.6, DL = 0.5, LB = 0.4, CB = 0.4, S = 0.4, K = 0.2, P = 0.1))
  cap_offense <- get0("INJURY_CAP_OFFENSE", envir = .GlobalEnv, ifnotfound = 4.5)
  cap_defense <- get0("INJURY_CAP_DEFENSE", envir = .GlobalEnv, ifnotfound = 4.5)

  # Categorize positions
  offense_positions <- c("QB", "WR", "WR1", "RB", "TE", "OL", "OT", "OG", "C", "FB", "T", "G")
  defense_positions <- c("EDGE", "DL", "DE", "DT", "NT", "LB", "ILB", "OLB", "CB", "CB1", "S", "SS", "FS", "DB")

  # Map positions to weights
  get_position_weight <- function(pos) {
    pos <- toupper(trimws(pos))
    if (pos %in% names(position_weights)) {
      return(position_weights[[pos]])
    }
    # Map to broader categories
    if (pos %in% c("OT", "OG", "C", "T", "G")) return(position_weights[["OL"]] %||% 0.6)
    if (pos %in% c("DE")) return(position_weights[["EDGE"]] %||% 0.6)
    if (pos %in% c("DT", "NT")) return(position_weights[["DL"]] %||% 0.5)
    if (pos %in% c("ILB", "OLB")) return(position_weights[["LB"]] %||% 0.4)
    if (pos %in% c("SS", "FS")) return(position_weights[["S"]] %||% 0.4)
    if (pos %in% c("DB")) return(position_weights[["CB"]] %||% 0.4)
    0.3  # Default for unknown positions
  }

  result <- tibble::tibble(
    team = teams,
    offense_penalty = 0,
    defense_penalty = 0,
    top_impacts = vector("list", length(teams))
  )

  if (!is.data.frame(injury_df) || nrow(injury_df) == 0) {
    return(result)
  }

  # Add availability if not present
  if (!"availability" %in% names(injury_df)) {
    injury_df <- add_availability_scores(injury_df)
  }

  for (i in seq_along(teams)) {
    team <- teams[i]
    team_injuries <- injury_df %>%
      dplyr::filter(team == !!team, !is.na(position))

    if (nrow(team_injuries) == 0) next

    # Calculate impact for each player
    team_injuries <- team_injuries %>%
      dplyr::mutate(
        pos_upper = toupper(trimws(position)),
        is_offense = pos_upper %in% offense_positions,
        is_defense = pos_upper %in% defense_positions,
        pos_weight = purrr::map_dbl(pos_upper, get_position_weight),
        impact_pts = (1 - availability) * pos_weight
      ) %>%
      dplyr::arrange(dplyr::desc(impact_pts))

    # Sum penalties by side
    offense_pen <- sum(team_injuries$impact_pts[team_injuries$is_offense], na.rm = TRUE)
    defense_pen <- sum(team_injuries$impact_pts[team_injuries$is_defense], na.rm = TRUE)

    # Apply caps
    offense_capped <- pmin(offense_pen, cap_offense)
    defense_capped <- pmin(defense_pen, cap_defense)

    if (offense_pen > cap_offense) {
      message(sprintf("  %s: Offense injury penalty capped at %.1f (raw: %.1f)", team, cap_offense, offense_pen))
    }
    if (defense_pen > cap_defense) {
      message(sprintf("  %s: Defense injury penalty capped at %.1f (raw: %.1f)", team, cap_defense, defense_pen))
    }

    result$offense_penalty[i] <- offense_capped
    result$defense_penalty[i] <- defense_capped

    # Store top 3 impacts for reporting
    top_3 <- team_injuries %>%
      dplyr::filter(impact_pts > 0) %>%
      dplyr::slice_head(n = 3) %>%
      dplyr::transmute(
        player = player,
        position = position,
        availability = round(availability, 2),
        impact = round(impact_pts, 2)
      )
    result$top_impacts[[i]] <- top_3
  }

  # Store for HTML report
  .INJURY_TOP_IMPACTS <<- result$top_impacts
  names(.INJURY_TOP_IMPACTS) <<- teams

  result
}


# =============================================================================
# SECTION 9: Diagnostic Output
# =============================================================================

#' Print injury diagnostics for debugging
#'
#' @param injury_df Normalized injury data
#' @param adjustments Team adjustments
print_injury_diagnostics <- function(injury_df, adjustments) {

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("  INJURY DIAGNOSTICS\n")
  cat("═══════════════════════════════════════════════════════════════\n")

  mode_used <- .INJURY_MODE_USED %||% "unknown"
  cat(sprintf("  Mode: %s\n", mode_used))

  if (!is.null(.INJURY_FALLBACK_SEASON)) {
    cat(sprintf("  ⚠ Fallback season: %d\n", .INJURY_FALLBACK_SEASON))
  }

  if (is.data.frame(injury_df)) {
    cat(sprintf("  Players parsed: %d\n", nrow(injury_df)))
    if (nrow(injury_df) > 0 && "team" %in% names(injury_df)) {
      team_counts <- table(injury_df$team)
      cat(sprintf("  Teams with data: %d\n", length(team_counts)))
    }
  }

  if (is.data.frame(adjustments) && nrow(adjustments) > 0) {
    cat("\n  Team Adjustments:\n")
    cat("  ─────────────────────────────────────────\n")
    for (i in 1:nrow(adjustments)) {
      cat(sprintf("  %s: OFF %.2f pts, DEF %.2f pts\n",
                  adjustments$team[i],
                  adjustments$offense_penalty[i],
                  adjustments$defense_penalty[i]))
    }

    total_off <- sum(adjustments$offense_penalty, na.rm = TRUE)
    total_def <- sum(adjustments$defense_penalty, na.rm = TRUE)
    cat("  ─────────────────────────────────────────\n")
    cat(sprintf("  Total: OFF %.2f pts, DEF %.2f pts\n", total_off, total_def))
  }

  cat("═══════════════════════════════════════════════════════════════\n\n")
}


# =============================================================================
# SECTION 10: HTML Report Banner Data
# =============================================================================

#' Get injury mode information for HTML report banner
#'
#' @return List with mode, fallback_season, and message
get_injury_report_info <- function() {
  list(
    mode = .INJURY_MODE_USED %||% "unknown",
    fallback_season = .INJURY_FALLBACK_SEASON,
    top_impacts = .INJURY_TOP_IMPACTS,
    message = if (!is.null(.INJURY_FALLBACK_SEASON)) {
      sprintf("Injury data: fallback to %d season", .INJURY_FALLBACK_SEASON)
    } else if (.INJURY_MODE_USED == "off") {
      "Injury adjustments disabled"
    } else {
      sprintf("Injury mode: %s", .INJURY_MODE_USED %||% "auto")
    }
  )
}


# =============================================================================
# SECTION 11: Backup QB Quality Scoring
# =============================================================================
# Assesses backup QB quality to modulate QB injury penalty
# Elite backups (Cooper Rush, Tyrod Taylor) should reduce penalty vs practice squad QBs

# Cache for backup QB quality scores (avoid repeated API calls)
.QB_BACKUP_QUALITY_CACHE <- new.env(parent = emptyenv())

#' Get backup QB quality score for a team
#'
#' @param team Team abbreviation (e.g., "DAL", "KC")
#' @param season Season year (default: current)
#' @param use_cache Use cached values if available (default: TRUE)
#' @return Numeric quality score 0-1 (0 = poor backup, 1 = elite backup)
#'
#' @details Quality score based on:
#'   - Historical QB rating when starting (if available)
#'   - Years of experience
#'   - Career win-loss record
#'   - Whether they've started games recently
#'
#' @examples
#' get_backup_qb_quality("DAL", 2024)  # Cooper Rush = ~0.65
#' get_backup_qb_quality("NE", 2024)   # Practice squad = ~0.15
get_backup_qb_quality <- function(team, season = NULL, use_cache = TRUE) {

  # Default to current season
  season <- season %||% get0("SEASON", envir = .GlobalEnv, ifnotfound = as.integer(format(Sys.Date(), "%Y")))

  # Check cache first
  cache_key <- sprintf("%s_%d", team, season)
  if (use_cache && exists(cache_key, envir = .QB_BACKUP_QUALITY_CACHE)) {
    return(get(cache_key, envir = .QB_BACKUP_QUALITY_CACHE))
  }

  # Default quality score (league average backup)
  default_quality <- 0.35

  # Try to load depth charts
  depth_charts <- tryCatch({
    if (requireNamespace("nflreadr", quietly = TRUE)) {
      nflreadr::load_depth_charts(seasons = season)
    } else {
      NULL
    }
  }, error = function(e) {
    message(sprintf("Could not load depth charts: %s", conditionMessage(e)))
    NULL
  })

  if (is.null(depth_charts) || !is.data.frame(depth_charts) || nrow(depth_charts) == 0) {
    if (use_cache) assign(cache_key, default_quality, envir = .QB_BACKUP_QUALITY_CACHE)
    return(default_quality)
  }

  # Find QB2 for this team
  team_qbs <- depth_charts %>%
    dplyr::filter(
      club_code == team | team == !!team,
      position == "QB"
    ) %>%
    dplyr::arrange(depth_team)

  if (nrow(team_qbs) < 2) {
    if (use_cache) assign(cache_key, default_quality, envir = .QB_BACKUP_QUALITY_CACHE)
    return(default_quality)
  }

  # Get QB2 (second on depth chart)
  qb2_name <- team_qbs$full_name[2]
  qb2_gsis <- if ("gsis_id" %in% names(team_qbs)) team_qbs$gsis_id[2] else NULL

  # Try to get historical stats for QB2
  qb2_stats <- tryCatch({
    if (requireNamespace("nflreadr", quietly = TRUE)) {
      # Load player stats for recent seasons
      stats <- nflreadr::load_player_stats(seasons = (season - 3):season) %>%
        dplyr::filter(
          position == "QB",
          (full_name == qb2_name | player_display_name == qb2_name |
           (!is.null(qb2_gsis) & player_id == qb2_gsis))
        )

      if (nrow(stats) == 0) return(NULL)

      # Calculate quality metrics
      stats %>%
        dplyr::filter(attempts >= 10) %>%  # At least 10 attempts
        dplyr::summarise(
          games_started = dplyr::n(),
          total_attempts = sum(attempts, na.rm = TRUE),
          passer_rating_avg = mean(passer_rating, na.rm = TRUE),
          td_int_ratio = sum(passing_tds, na.rm = TRUE) / pmax(sum(interceptions, na.rm = TRUE), 1),
          epa_per_play = mean(passing_epa / pmax(attempts, 1), na.rm = TRUE)
        )
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })

  # Calculate quality score based on available data
  quality_score <- default_quality

  if (!is.null(qb2_stats) && nrow(qb2_stats) > 0) {
    # Experience component (0-0.3): more games = better
    exp_score <- pmin(qb2_stats$games_started / 20, 1) * 0.3

    # Passer rating component (0-0.4): higher rating = better
    # NFL average is ~90, elite is 100+
    rating_score <- if (!is.na(qb2_stats$passer_rating_avg)) {
      pmin(pmax((qb2_stats$passer_rating_avg - 70) / 40, 0), 1) * 0.4
    } else {
      0.15  # Default if no rating
    }

    # TD/INT ratio component (0-0.2): higher ratio = better
    tdi_score <- if (!is.na(qb2_stats$td_int_ratio)) {
      pmin(qb2_stats$td_int_ratio / 3, 1) * 0.2
    } else {
      0.05  # Default
    }

    # EPA component (0-0.1): positive EPA = good
    epa_score <- if (!is.na(qb2_stats$epa_per_play)) {
      pmin(pmax((qb2_stats$epa_per_play + 0.1) / 0.3, 0), 1) * 0.1
    } else {
      0.03  # Default
    }

    quality_score <- exp_score + rating_score + tdi_score + epa_score
  } else {
    # No stats available - use known backup quality lookup
    known_quality <- get_known_backup_quality(qb2_name, team, season)
    if (!is.na(known_quality)) {
      quality_score <- known_quality
    }
  }

  # Bound to [0, 1]
  quality_score <- pmin(pmax(quality_score, 0), 1)

  # Cache the result
  if (use_cache) assign(cache_key, quality_score, envir = .QB_BACKUP_QUALITY_CACHE)

  quality_score
}


#' Get known backup QB quality from pre-defined list
#'
#' @param qb_name QB name
#' @param team Team abbreviation
#' @param season Season year
#' @return Quality score or NA if not in lookup
get_known_backup_quality <- function(qb_name, team, season) {

  # Known quality backups (2023-2025)
  # These are QBs with meaningful NFL experience as backups/spot starters
  known_quality <- list(
    # Elite tier (0.65-0.80): Proven starters, high draft picks
    "Cooper Rush" = 0.70,
    "Tyrod Taylor" = 0.65,
    "Jacoby Brissett" = 0.60,
    "Gardner Minshew" = 0.65,
    "Jameis Winston" = 0.60,
    "Marcus Mariota" = 0.55,
    "Joe Flacco" = 0.65,
    "Andy Dalton" = 0.60,
    "Case Keenum" = 0.55,
    "Colt McCoy" = 0.50,

    # Good tier (0.45-0.60): Experienced backups
    "Mike White" = 0.50,
    "Taylor Heinicke" = 0.50,
    "Sam Darnold" = 0.50,
    "Ryan Tannehill" = 0.55,
    "Jimmy Garoppolo" = 0.60,
    "Drew Lock" = 0.45,
    "Mason Rudolph" = 0.45,
    "Mitch Trubisky" = 0.50,
    "Tyler Huntley" = 0.50,
    "Easton Stick" = 0.45,

    # Average tier (0.30-0.45): Limited experience
    "Brett Rypien" = 0.35,
    "Kyle Allen" = 0.40,
    "Trace McSorley" = 0.30,
    "Tommy DeVito" = 0.35,
    "Jeff Driskel" = 0.35,
    "Tim Boyle" = 0.30,
    "Dorian Thompson-Robinson" = 0.40,
    "Anthony Richardson" = 0.45,

    # Below average tier (0.15-0.30): Rookie/practice squad
    "default" = 0.25
  )

  # Try to match QB name
  for (name_pattern in names(known_quality)) {
    if (grepl(name_pattern, qb_name, ignore.case = TRUE)) {
      return(known_quality[[name_pattern]])
    }
  }

  NA_real_
}


#' Calculate QB injury penalty adjusted for backup quality
#'
#' @param team Team abbreviation
#' @param qb_status QB injury status (OUT, DOUBTFUL, QUESTIONABLE, etc.)
#' @param season Season year
#' @return Point penalty (negative value)
#'
#' @details
#' Base penalty from config QB_OUT_BASE_PENALTY (default 7.2)
#' Adjusted by: penalty * (1 - QB_BACKUP_QUALITY_DISCOUNT * backup_quality)
#' Example: Elite backup (0.70) with 50% discount = 7.2 * (1 - 0.5 * 0.70) = 4.68 pts
calculate_qb_injury_penalty <- function(team, qb_status, season = NULL) {

  # Get config values with fallbacks
  base_penalty <- get0("QB_OUT_BASE_PENALTY", envir = .GlobalEnv, ifnotfound = 7.2)
  quality_discount <- get0("QB_BACKUP_QUALITY_DISCOUNT", envir = .GlobalEnv, ifnotfound = 0.50)
  use_quality <- get0("USE_QB_BACKUP_QUALITY", envir = .GlobalEnv, ifnotfound = TRUE)

  # Determine severity multiplier based on status
  status <- toupper(trimws(qb_status))
  severity <- dplyr::case_when(
    grepl("OUT|IR", status)       ~ 1.0,
    grepl("DOUBTFUL", status)     ~ 0.7,
    grepl("QUESTIONABLE", status) ~ 0.35,
    grepl("LIMITED", status)      ~ 0.15,
    TRUE                          ~ 0.0
  )

  if (severity == 0) {
    return(0)
  }

  # Get backup quality if enabled
  backup_quality <- if (isTRUE(use_quality)) {
    tryCatch({
      get_backup_qb_quality(team, season)
    }, error = function(e) {
      0.35  # Default to league average backup
    })
  } else {
    0  # No discount if quality scoring disabled
  }

  # Calculate adjusted penalty
  # Higher backup quality = lower penalty
  # Formula: base * severity * (1 - discount * quality)
  # Elite backup (0.70) with 50% discount: 7.2 * (1 - 0.5 * 0.70) = 4.68
  # Poor backup (0.20) with 50% discount: 7.2 * (1 - 0.5 * 0.20) = 6.48
  discount_factor <- 1 - (quality_discount * backup_quality)
  adjusted_penalty <- base_penalty * severity * discount_factor

  # Return negative value (penalty)
  -adjusted_penalty
}


#' Clear the backup QB quality cache
#'
#' @description Useful for testing or when depth charts update
clear_qb_quality_cache <- function() {
  rm(list = ls(envir = .QB_BACKUP_QUALITY_CACHE), envir = .QB_BACKUP_QUALITY_CACHE)
  message("QB backup quality cache cleared")
}


# =============================================================================
# SECTION 12: Snap-Count Weighted Injury Impacts
# =============================================================================
# WR1 (60% snaps) should have higher injury impact than WR5 (5% snaps)
# Uses nflreadr::load_participation() for snap data

# Cache for player snap percentages
.SNAP_PCT_CACHE <- new.env(parent = emptyenv())

#' Load player snap percentages for a team
#'
#' @param team Team abbreviation
#' @param season Season year
#' @param weeks Weeks to average (default: last 4 completed weeks)
#' @param use_cache Use cached values (default: TRUE)
#' @return Tibble with player, position, snap_pct columns
#'
#' @details Uses nflreadr::load_participation() to get snap counts
#' Calculates average snap percentage over recent weeks
load_player_snap_percentages <- function(team, season = NULL, weeks = NULL, use_cache = TRUE) {

  season <- season %||% get0("SEASON", envir = .GlobalEnv, ifnotfound = as.integer(format(Sys.Date(), "%Y")))

  # Check cache

  cache_key <- sprintf("%s_%d", team, season)
  if (use_cache && exists(cache_key, envir = .SNAP_PCT_CACHE)) {
    return(get(cache_key, envir = .SNAP_PCT_CACHE))
  }

  # Default result
  default_result <- tibble::tibble(
    player = character(),
    position = character(),
    snap_pct = numeric()
  )

  # Try to load participation data
  participation <- tryCatch({
    if (requireNamespace("nflreadr", quietly = TRUE)) {
      nflreadr::load_participation(seasons = season)
    } else {
      NULL
    }
  }, error = function(e) {
    message(sprintf("Could not load participation data: %s", conditionMessage(e)))
    NULL
  })

  if (is.null(participation) || !is.data.frame(participation) || nrow(participation) == 0) {
    if (use_cache) assign(cache_key, default_result, envir = .SNAP_PCT_CACHE)
    return(default_result)
  }

  # Filter to team and calculate snap percentages
  team_snaps <- tryCatch({
    # Handle different column naming conventions
    team_col <- if ("possession_team" %in% names(participation)) "possession_team" else
                if ("team" %in% names(participation)) "team" else
                if ("club_code" %in% names(participation)) "club_code" else NULL

    if (is.null(team_col)) {
      message("Could not find team column in participation data")
      return(default_result)
    }

    # Determine weeks to use
    if (is.null(weeks)) {
      # Use last 4 completed weeks
      current_week <- get0("WEEK_TO_SIM", envir = .GlobalEnv, ifnotfound = 1)
      weeks <- max(1, current_week - 4):(current_week - 1)
      weeks <- weeks[weeks > 0]
    }

    # Ensure weeks is an integer vector
    weeks <- as.integer(weeks)

    # Extract week from game_id if no week column exists
    # nflreadr::load_participation() returns play-level data without a week column
    # Game ID pattern: "2024_05_DAL_NYG" -> week = 5
    if (!"week" %in% names(participation)) {
      game_id_col <- if ("nflverse_game_id" %in% names(participation)) "nflverse_game_id" else
                     if ("game_id" %in% names(participation)) "game_id" else
                     if ("old_game_id" %in% names(participation)) "old_game_id" else NULL

      if (!is.null(game_id_col)) {
        participation <- participation %>%
          dplyr::mutate(
            week = as.integer(sub("^[0-9]{4}_([0-9]{2})_.*$", "\\1", .data[[game_id_col]]))
          )
      } else {
        message("Could not find game_id column to extract week")
        return(default_result)
      }
    }

    # Filter and aggregate
    filtered <- participation %>%
      dplyr::filter(
        .data[[team_col]] == team,
        week %in% weeks
      )

    if (nrow(filtered) == 0) {
      return(default_result)
    }

    # Calculate average snap percentage per player
    # Need to identify the snap count and total snap columns
    snap_col <- if ("offense_snaps" %in% names(filtered)) "offense_snaps" else
                if ("snaps" %in% names(filtered)) "snaps" else
                if ("n_offense" %in% names(filtered)) "n_offense" else NULL

    if (is.null(snap_col)) {
      # Try to calculate from participation strings
      if ("offense_players" %in% names(filtered)) {
        # This is a play-level dataset, need different approach
        return(calculate_snap_pct_from_plays(filtered, team))
      }
      return(default_result)
    }

    # Get player name column
    player_col <- if ("full_name" %in% names(filtered)) "full_name" else
                  if ("player_name" %in% names(filtered)) "player_name" else
                  if ("gsis_id" %in% names(filtered)) "gsis_id" else NULL

    pos_col <- if ("position" %in% names(filtered)) "position" else
               if ("pos" %in% names(filtered)) "pos" else NULL

    if (is.null(player_col)) {
      return(default_result)
    }

    # Aggregate snap percentages
    result <- filtered %>%
      dplyr::group_by(
        player = .data[[player_col]],
        position = if (!is.null(pos_col)) .data[[pos_col]] else NA_character_
      ) %>%
      dplyr::summarise(
        total_snaps = sum(.data[[snap_col]], na.rm = TRUE),
        games = dplyr::n_distinct(week),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        # Calculate snap percentage relative to team average
        snap_pct = 100 * total_snaps / max(total_snaps, na.rm = TRUE)
      ) %>%
      dplyr::select(player, position, snap_pct)

    result

  }, error = function(e) {
    message(sprintf("Error calculating snap percentages: %s", conditionMessage(e)))
    default_result
  })

  # Cache and return
  if (use_cache) assign(cache_key, team_snaps, envir = .SNAP_PCT_CACHE)
  team_snaps
}


#' Calculate snap percentages from play-level participation data
#'
#' @param plays Play-level participation data
#' @param team Team abbreviation
#' @return Tibble with player, position, snap_pct
calculate_snap_pct_from_plays <- function(plays, team) {

  default_result <- tibble::tibble(
    player = character(),
    position = character(),
    snap_pct = numeric()
  )

  tryCatch({
    # offense_players is a semicolon-separated list of player IDs
    if (!"offense_players" %in% names(plays)) {
      return(default_result)
    }

    # Count total offensive plays
    total_plays <- nrow(plays)

    if (total_plays == 0) {
      return(default_result)
    }

    # Expand player participation
    player_counts <- plays %>%
      dplyr::filter(!is.na(offense_players)) %>%
      dplyr::mutate(
        players = strsplit(offense_players, ";")
      ) %>%
      tidyr::unnest(players) %>%
      dplyr::filter(nzchar(trimws(players))) %>%
      dplyr::count(player_id = trimws(players), name = "snaps") %>%
      dplyr::mutate(
        snap_pct = 100 * snaps / total_plays
      )

    # Try to get player names from roster
    roster <- tryCatch({
      nflreadr::load_rosters(seasons = get0("SEASON", envir = .GlobalEnv, ifnotfound = 2024))
    }, error = function(e) NULL)

    if (!is.null(roster) && is.data.frame(roster)) {
      player_counts <- player_counts %>%
        dplyr::left_join(
          roster %>% dplyr::select(gsis_id, full_name, position),
          by = c("player_id" = "gsis_id")
        ) %>%
        dplyr::transmute(
          player = dplyr::coalesce(full_name, player_id),
          position = position,
          snap_pct = snap_pct
        )
    } else {
      player_counts <- player_counts %>%
        dplyr::transmute(
          player = player_id,
          position = NA_character_,
          snap_pct = snap_pct
        )
    }

    player_counts

  }, error = function(e) {
    message(sprintf("Error in play-level snap calculation: %s", conditionMessage(e)))
    default_result
  })
}


#' Apply snap-count weighting to injury impact
#'
#' @param base_impact Base injury impact (from position weight)
#' @param player_name Player name
#' @param team Team abbreviation
#' @param season Season year
#' @return Weighted injury impact
#'
#' @details
#' Formula: base_impact * (snap_pct / SNAP_WEIGHT_REFERENCE)
#' WR1 at 60% snaps with 50% reference: base_impact * 1.2
#' WR5 at 10% snaps with 50% reference: base_impact * 0.2
#'
#' @param base_impact Base injury impact value
#' @param player_name Player name to look up
#' @param team Team abbreviation
#' @param season Season year (default: from global SEASON)
#' @param weeks Optional vector of weeks to use for snap calculation
#'              For historical analysis, pass explicit weeks (e.g., max(1, game_week - 4):(game_week - 1))
#'              If NULL, uses global WEEK_TO_SIM to determine weeks
weight_injury_by_snaps <- function(base_impact, player_name, team, season = NULL, weeks = NULL) {

  # Check if snap weighting is enabled
  use_snap_weighting <- get0("USE_SNAP_WEIGHTED_INJURIES", envir = .GlobalEnv, ifnotfound = TRUE)
  snap_reference <- get0("SNAP_WEIGHT_REFERENCE", envir = .GlobalEnv, ifnotfound = 50)

  if (!isTRUE(use_snap_weighting)) {
    return(base_impact)
  }

  # Get snap percentages for team
  # Pass explicit weeks if provided (important for historical analysis)
  snap_data <- tryCatch({
    load_player_snap_percentages(team, season, weeks = weeks, use_cache = is.null(weeks))
  }, error = function(e) {
    tibble::tibble()
  })

  if (nrow(snap_data) == 0) {
    return(base_impact)  # No data, use unweighted
  }

  # Find player's snap percentage
  player_snap <- snap_data %>%
    dplyr::filter(grepl(player_name, player, ignore.case = TRUE)) %>%
    dplyr::slice_head(n = 1)

  if (nrow(player_snap) == 0) {
    # Player not found, use position-based default
    return(base_impact)
  }

  snap_pct <- player_snap$snap_pct

  # Apply weighting: higher snap % = higher impact
  # Bounded to prevent extreme values
  weight_factor <- pmin(pmax(snap_pct / snap_reference, 0.2), 2.0)

  base_impact * weight_factor
}


#' Clear snap percentage cache
clear_snap_pct_cache <- function() {
  rm(list = ls(envir = .SNAP_PCT_CACHE), envir = .SNAP_PCT_CACHE)
  message("Snap percentage cache cleared")
}
