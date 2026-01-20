# =============================================================================
# NFL Prediction Model - Date Resolver Module
# =============================================================================
# Foolproof DATE -> (season, week, phase, round) resolution using schedule
# as the authoritative source of truth.
# =============================================================================

# Required packages check
if (!requireNamespace("dplyr", quietly = TRUE)) {
  stop("Package 'dplyr' is required for date_resolver module. Install with: install.packages('dplyr')")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  stop("Package 'lubridate' is required for date_resolver module. Install with: install.packages('lubridate')")
}

# Source logging utilities if not already loaded
if (!exists("log_info", mode = "function")) {
  logging_path <- file.path(dirname(sys.frame(1)$ofile %||% "."), "logging.R")
  if (file.exists(logging_path)) {
    source(logging_path)
  } else if (file.exists("R/logging.R")) {
    source("R/logging.R")
  }
}

# =============================================================================
# CONFIGURATION
# =============================================================================

#' @title Date Resolver Configuration
#' @description Settings for date-to-week resolution
DATE_RESOLVER_CONFIG <- list(
  # Time buffer around games (in days)
  week_buffer_pre = 4,    # Days before earliest kickoff to include in week
  week_buffer_post = 2,   # Days after latest kickoff to include in week

  # Timezone for date comparisons
  timezone = "America/New_York",  # Eastern Time (NFL standard)

  # Season structure
  regular_season_weeks = 18,

  playoff_weeks = c(19, 20, 21, 22),

  # Cache settings
  cache_schedules = TRUE,
  cache_duration_hours = 24,
  cache_max_entries = 10   # Maximum number of cached schedule sets (prevents memory growth)
)

# Schedule cache environment
.schedule_cache <- new.env(parent = emptyenv())

# =============================================================================
# SCHEDULE LOADING
# =============================================================================

#' Load NFL schedules with caching
#' @param seasons Vector of season years
#' @param force_refresh Force reload even if cached
#' @return Combined schedule data frame
load_nfl_schedules <- function(seasons, force_refresh = FALSE) {
  cache_key <- paste(sort(seasons), collapse = "_")

  # Check cache
  if (!force_refresh && DATE_RESOLVER_CONFIG$cache_schedules) {
    cached <- .schedule_cache[[cache_key]]
    if (!is.null(cached)) {
      cache_time <- attr(cached, "cache_time")
      if (!is.null(cache_time)) {
        age_hours <- as.numeric(difftime(Sys.time(), cache_time, units = "hours"))
        if (age_hours < DATE_RESOLVER_CONFIG$cache_duration_hours) {
          return(cached)
        }
      }
    }
  }

  # Load fresh data
  schedule <- tryCatch({
    if (!requireNamespace("nflreadr", quietly = TRUE)) {
      stop("nflreadr package required for schedule loading")
    }
    nflreadr::load_schedules(seasons = seasons)
  }, error = function(e) {
    warning(sprintf("Failed to load schedules: %s", e$message))
    return(NULL)
  })

  if (is.null(schedule) || nrow(schedule) == 0) {
    return(NULL)
  }

  # Cache result with LRU eviction
  if (DATE_RESOLVER_CONFIG$cache_schedules) {
    # Enforce cache size limit by evicting oldest entries
    cache_keys <- ls(.schedule_cache)
    max_entries <- DATE_RESOLVER_CONFIG$cache_max_entries %||% 10

    if (length(cache_keys) >= max_entries) {
      # Find and remove oldest entries
      cache_times <- sapply(cache_keys, function(k) {
        cached <- .schedule_cache[[k]]
        ct <- attr(cached, "cache_time")
        if (is.null(ct)) Sys.time() - 999999 else ct
      })
      sorted_keys <- cache_keys[order(cache_times)]
      # Remove oldest entries to make room
      n_to_remove <- length(cache_keys) - max_entries + 1
      for (k in sorted_keys[1:n_to_remove]) {
        rm(list = k, envir = .schedule_cache)
      }
    }

    attr(schedule, "cache_time") <- Sys.time()
    .schedule_cache[[cache_key]] <- schedule
  }

  schedule
}

#' Clear schedule cache
clear_schedule_cache <- function() {
  rm(list = ls(.schedule_cache), envir = .schedule_cache)
  invisible(TRUE)
}

# =============================================================================
# DATE/TIME UTILITIES
# =============================================================================

#' Parse datetime with timezone handling
#' @param datetime_str Datetime string or POSIXct
#' @param tz Target timezone
#' @return POSIXct in target timezone
parse_datetime <- function(datetime_str, tz = DATE_RESOLVER_CONFIG$timezone) {
  if (inherits(datetime_str, "POSIXct")) {
    return(lubridate::with_tz(datetime_str, tz))
  }

  if (is.na(datetime_str) || is.null(datetime_str)) {
    return(NA)
  }

  # Try multiple formats
  parsed <- tryCatch({
    lubridate::ymd_hms(datetime_str, tz = tz)
  }, error = function(e) {
    tryCatch({
      lubridate::ymd(datetime_str, tz = tz)
    }, error = function(e2) {
      NA
    })
  })

  parsed
}

#' Parse kickoff times from schedule
#' @param schedule Schedule data frame with gameday and gametime columns
#' @return Schedule with kickoff_local POSIXct column
parse_kickoff_times <- function(schedule) {
  if (is.null(schedule) || nrow(schedule) == 0) {
    return(schedule)
  }

  schedule <- schedule %>%
    dplyr::mutate(
      kickoff_local = dplyr::case_when(
        # If already have POSIXct
        inherits(.data$gameday, "POSIXct") ~ .data$gameday,
        # Combine date and time
        !is.na(.data$gameday) & !is.na(.data$gametime) ~
          parse_datetime(paste(.data$gameday, .data$gametime)),
        # Date only
        !is.na(.data$gameday) ~
          parse_datetime(.data$gameday),
        TRUE ~ lubridate::NA_POSIXct_
      )
    )

  schedule
}

# =============================================================================
# WEEK BOUNDARY CALCULATION
# =============================================================================

#' Get week boundaries from schedule
#' @param schedule Schedule with kickoff_local column
#' @return Data frame with season, week, earliest, latest kickoffs
get_week_boundaries <- function(schedule) {
  if (is.null(schedule) || nrow(schedule) == 0) {
    return(NULL)
  }

  # Ensure kickoff times are parsed
  if (!"kickoff_local" %in% names(schedule)) {
    schedule <- parse_kickoff_times(schedule)
  }

  schedule %>%
    dplyr::filter(!is.na(.data$kickoff_local)) %>%
    dplyr::group_by(.data$season, .data$week) %>%
    dplyr::summarise(
      earliest_kickoff = min(.data$kickoff_local, na.rm = TRUE),
      latest_kickoff = max(.data$kickoff_local, na.rm = TRUE),
      n_games = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Add buffer windows
      window_start = .data$earliest_kickoff - lubridate::days(DATE_RESOLVER_CONFIG$week_buffer_pre),
      window_end = .data$latest_kickoff + lubridate::days(DATE_RESOLVER_CONFIG$week_buffer_post)
    ) %>%
    dplyr::arrange(.data$season, .data$week)
}

# =============================================================================
# SEASON YEAR DETERMINATION
# =============================================================================

#' Determine season year from a date
#' @description NFL seasons span calendar years. A game in January 2026 belongs
#' to the 2025 season. This function determines the correct season year.
#' @param date Date or datetime
#' @return Season year (integer)
determine_season_year <- function(date) {
  if (is.na(date) || is.null(date)) {
    return(NA_integer_)
  }

  date <- as.Date(date)
  year <- lubridate::year(date)
  month <- lubridate::month(date)

  # NFL season structure:
  # - Regular season: September - early January
  # - Playoffs: January - early February
  # - Super Bowl: Usually first Sunday in February
  #
  # Rule: Jan-Feb games belong to previous year's season
  if (month <= 2) {
    # January/February = previous year's season
    return(as.integer(year - 1))
  } else if (month >= 9) {
    # September onwards = current year's season
    return(as.integer(year))
  } else {
    # March-August = offseason, assume upcoming season
    return(as.integer(year))
  }
}

#' Get candidate seasons for a date
#' @param date Date to check
#' @return Vector of possible season years to search
get_candidate_seasons <- function(date) {
  date <- as.Date(date)
  year <- lubridate::year(date)
  month <- lubridate::month(date)

  # Always check current and adjacent seasons
  candidates <- c(year - 1, year, year + 1)

  # Prioritize based on month
  if (month <= 2) {
    # Early year: prioritize previous year's season
    candidates <- c(year - 1, year, year + 1)
  } else if (month >= 9) {
    # Late year: prioritize current year's season
    candidates <- c(year, year - 1, year + 1)
  } else {
    # Offseason: could be either
    candidates <- c(year, year - 1, year + 1)
  }

  # Filter to valid NFL seasons (nflreadr has data from ~2002)
  candidates <- candidates[candidates >= 2002 & candidates <= year + 1]

  unique(candidates)
}

# =============================================================================
# MAIN RESOLVER FUNCTION
# =============================================================================

#' Resolve NFL context from a date
#' @description Main function to resolve date -> (season, week, phase, round).
#' Uses NFL schedule as the authoritative source of truth.
#' @param target_date Date or datetime to resolve (default: Sys.time())
#' @param tz Timezone for comparisons
#' @return List with season, week, phase, round, schedule_rows
resolve_nfl_context <- function(target_date = Sys.time(),
                                tz = DATE_RESOLVER_CONFIG$timezone) {
  # Parse target date
  target <- parse_datetime(target_date, tz)
  if (is.na(target)) {
    return(list(
      success = FALSE,
      error = "Could not parse target date",
      season = NA_integer_,
      week = NA_integer_,
      phase = NA_character_,
      round = NA_character_,
      schedule_rows = NULL
    ))
  }

  # Determine candidate seasons
  candidate_seasons <- get_candidate_seasons(target)

  # Load schedules for candidate seasons
  schedule <- load_nfl_schedules(candidate_seasons)
  if (is.null(schedule) || nrow(schedule) == 0) {
    return(list(
      success = FALSE,
      error = "Could not load schedule data",
      season = NA_integer_,
      week = NA_integer_,
      phase = NA_character_,
      round = NA_character_,
      schedule_rows = NULL
    ))
  }

  # Parse kickoff times
  schedule <- parse_kickoff_times(schedule)

  # Get week boundaries
  boundaries <- get_week_boundaries(schedule)
  if (is.null(boundaries) || nrow(boundaries) == 0) {
    return(list(
      success = FALSE,
      error = "Could not calculate week boundaries",
      season = NA_integer_,
      week = NA_integer_,
      phase = NA_character_,
      round = NA_character_,
      schedule_rows = NULL
    ))
  }

  # Find matching week(s)
  matching_weeks <- boundaries %>%
    dplyr::filter(
      target >= .data$window_start & target <= .data$window_end
    )

  if (nrow(matching_weeks) == 0) {
    # No exact match - find nearest week
    boundaries <- boundaries %>%
      dplyr::mutate(
        days_to_start = as.numeric(difftime(.data$window_start, target, units = "days")),
        days_to_end = as.numeric(difftime(target, .data$window_end, units = "days"))
      )

    # Find closest upcoming week
    upcoming <- boundaries %>%
      dplyr::filter(.data$days_to_start > 0) %>%
      dplyr::arrange(.data$days_to_start) %>%
      dplyr::slice(1)

    if (nrow(upcoming) > 0 && upcoming$days_to_start[1] <= 7) {
      # Within a week of next slate
      matching_weeks <- upcoming
    } else {
      # Find most recent past week
      past <- boundaries %>%
        dplyr::filter(.data$days_to_end > 0) %>%
        dplyr::arrange(dplyr::desc(.data$days_to_end)) %>%
        dplyr::slice(1)

      if (nrow(past) > 0) {
        matching_weeks <- past
      } else {
        return(list(
          success = FALSE,
          error = sprintf("No matching week found for date %s", target),
          season = determine_season_year(target),
          week = NA_integer_,
          phase = "offseason",
          round = NA_character_,
          schedule_rows = NULL
        ))
      }
    }
  }

  # Use first match (should typically be only one)
  match <- matching_weeks[1, ]
  season <- match$season
  week <- match$week

  # Determine phase
  phase <- if (week <= DATE_RESOLVER_CONFIG$regular_season_weeks) {
    "regular_season"
  } else if (week %in% DATE_RESOLVER_CONFIG$playoff_weeks) {
    "playoffs"
  } else {
    "unknown"
  }

  # Determine playoff round if applicable
  round <- NA_character_
  if (phase == "playoffs") {
    round <- dplyr::case_when(
      week == 19 ~ "wild_card",
      week == 20 ~ "divisional",
      week == 21 ~ "conference",
      week == 22 ~ "super_bowl",
      TRUE ~ NA_character_
    )
  }

  # Get schedule rows for this week
  schedule_rows <- schedule %>%
    dplyr::filter(.data$season == !!season & .data$week == !!week)

  list(
    success = TRUE,
    error = NULL,
    season = as.integer(season),
    week = as.integer(week),
    phase = phase,
    round = round,
    target_date = target,
    window_start = match$window_start,
    window_end = match$window_end,
    n_games = match$n_games,
    schedule_rows = schedule_rows
  )
}

# =============================================================================
# CONVENIENCE FUNCTIONS
# =============================================================================

#' Resolve and apply NFL context to global config
#' @param target_date Date to resolve (default: Sys.time())
#' @param verbose Print resolution details
#' @return Resolved context (invisibly)
apply_resolved_context <- function(target_date = Sys.time(), verbose = TRUE) {
  context <- resolve_nfl_context(target_date)

  if (!context$success) {
    warning(sprintf("Context resolution failed: %s", context$error))
    return(invisible(context))
  }

  # Apply to global environment
  assign("SEASON", context$season, envir = .GlobalEnv)
  assign("WEEK_TO_SIM", context$week, envir = .GlobalEnv)
  assign("PHASE", context$phase, envir = .GlobalEnv)
  assign("PLAYOFF_ROUND", context$round, envir = .GlobalEnv)

  if (verbose) {
    # Use structured logging if available, otherwise fall back to cat
    if (exists("log_section", mode = "function") && exists("log_info", mode = "function")) {
      log_section("NFL CONTEXT RESOLVED")
      log_info("Target Date: %s", format(context$target_date, "%Y-%m-%d %H:%M %Z"))
      log_info("Season:      %d", context$season)
      log_info("Week:        %d", context$week)
      log_info("Phase:       %s", context$phase)
      if (!is.na(context$round)) {
        log_info("Round:       %s", context$round)
      }
      log_info("Games:       %d", context$n_games)
      log_info("Window:      %s to %s",
               format(context$window_start, "%Y-%m-%d"),
               format(context$window_end, "%Y-%m-%d"))
    } else {
      # Fallback to cat if logging not loaded
      cat("\n")
      cat("=================================================================\n")
      cat("  NFL CONTEXT RESOLVED\n")
      cat("=================================================================\n")
      cat(sprintf("  Target Date: %s\n", format(context$target_date, "%Y-%m-%d %H:%M %Z")))
      cat(sprintf("  Season:      %d\n", context$season))
      cat(sprintf("  Week:        %d\n", context$week))
      cat(sprintf("  Phase:       %s\n", context$phase))
      if (!is.na(context$round)) {
        cat(sprintf("  Round:       %s\n", context$round))
      }
      cat(sprintf("  Games:       %d\n", context$n_games))
      cat(sprintf("  Window:      %s to %s\n",
                  format(context$window_start, "%Y-%m-%d"),
                  format(context$window_end, "%Y-%m-%d")))
      cat("=================================================================\n\n")
    }
  }

  invisible(context)
}

#' Auto-resolve week based on current time
#' @description Convenience wrapper for automatic week detection.
#' @param verbose Print details
#' @return Resolved context
auto_resolve_week <- function(verbose = TRUE) {
  apply_resolved_context(Sys.time(), verbose = verbose)
}

#' Resolve context for a specific game day
#' @param gameday Date string (YYYY-MM-DD format)
#' @return Resolved context
resolve_gameday <- function(gameday) {
  date <- as.Date(gameday)
  # Add noon time to avoid timezone edge cases
  datetime <- as.POSIXct(paste(date, "12:00:00"), tz = DATE_RESOLVER_CONFIG$timezone)
  resolve_nfl_context(datetime)
}

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

#' Validate date resolution against known games
#' @param test_cases List of (date, expected_season, expected_week) tuples
#' @return Data frame with validation results
validate_date_resolution <- function(test_cases) {
  results <- lapply(test_cases, function(tc) {
    context <- resolve_nfl_context(tc$date)
    list(
      date = as.character(tc$date),
      expected_season = tc$season,
      actual_season = context$season,
      season_match = identical(tc$season, context$season),
      expected_week = tc$week,
      actual_week = context$week,
      week_match = identical(tc$week, context$week),
      phase = context$phase,
      success = context$success
    )
  })

  dplyr::bind_rows(results)
}

#' Run standard validation tests
#' @return List with pass/fail results
run_resolver_validation <- function() {
  # Standard test cases (historical known dates)
  test_cases <- list(
    # Mid-regular season
    list(date = "2024-11-10 13:00:00", season = 2024L, week = 10L),
    # Early January (still previous season playoffs)
    list(date = "2025-01-12 13:00:00", season = 2024L, week = 19L),  # Wild Card
    # Super Bowl Sunday (typically early February)
    list(date = "2025-02-09 18:30:00", season = 2024L, week = 22L),  # Super Bowl
    # Week 1 kickoff
    list(date = "2024-09-05 20:15:00", season = 2024L, week = 1L)
  )

  results <- validate_date_resolution(test_cases)

  n_pass <- sum(results$season_match & results$week_match)
  n_fail <- nrow(results) - n_pass

  list(
    passed = n_pass,
    failed = n_fail,
    all_passed = n_fail == 0,
    details = results
  )
}
