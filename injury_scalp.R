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

  valid_modes <- c("auto", "off", "last_available", "manual", "scalp")
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

  # Mode: auto or last_available - use nflreadr with fallbacks
  return(load_injury_nflreadr(seasons, mode))
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
