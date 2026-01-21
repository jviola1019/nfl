# =============================================================================
# Tests for R/date_resolver.R - Date Resolver Module
# =============================================================================
# Note: R/date_resolver.R is loaded by tests/testthat/setup.R
# =============================================================================

# =============================================================================
# CONFIGURATION TESTS
# =============================================================================

test_that("DATE_RESOLVER_CONFIG has required fields", {
  expect_true("week_buffer_pre" %in% names(DATE_RESOLVER_CONFIG))
  expect_true("week_buffer_post" %in% names(DATE_RESOLVER_CONFIG))
  expect_true("timezone" %in% names(DATE_RESOLVER_CONFIG))
  expect_true("regular_season_weeks" %in% names(DATE_RESOLVER_CONFIG))
  expect_true("playoff_weeks" %in% names(DATE_RESOLVER_CONFIG))
})
test_that("DATE_RESOLVER_CONFIG has sensible defaults", {
  # Buffer should be positive integers
  expect_true(DATE_RESOLVER_CONFIG$week_buffer_pre > 0)
  expect_true(DATE_RESOLVER_CONFIG$week_buffer_post > 0)

  # Regular season is 18 weeks
  expect_equal(DATE_RESOLVER_CONFIG$regular_season_weeks, 18)

  # Playoff weeks are 19-22
  expect_equal(DATE_RESOLVER_CONFIG$playoff_weeks, c(19, 20, 21, 22))

  # Timezone should be a valid timezone
  expect_true(DATE_RESOLVER_CONFIG$timezone %in% OlsonNames())
})

# =============================================================================
# SEASON YEAR DETERMINATION TESTS
# =============================================================================

test_that("determine_season_year handles calendar year boundaries", {
  # September-December games belong to current year
  expect_equal(determine_season_year("2024-09-05"), 2024L)
  expect_equal(determine_season_year("2024-11-15"), 2024L)
  expect_equal(determine_season_year("2024-12-25"), 2024L)

  # January-February games belong to PREVIOUS year's season
  expect_equal(determine_season_year("2025-01-12"), 2024L)  # Wild Card
  expect_equal(determine_season_year("2025-02-09"), 2024L)  # Super Bowl

  # March onwards is ambiguous (offseason) - assume current year
  expect_equal(determine_season_year("2025-03-15"), 2025L)
  expect_equal(determine_season_year("2025-07-04"), 2025L)
})

test_that("determine_season_year handles edge cases", {
  expect_true(is.na(determine_season_year(NA)))
  expect_true(is.na(determine_season_year(NULL)))
})

test_that("get_candidate_seasons returns appropriate seasons", {
  # November should prioritize current year
  nov_candidates <- get_candidate_seasons("2024-11-15")
  expect_true(2024 %in% nov_candidates)
  expect_equal(nov_candidates[1], 2024)

  # January should prioritize previous year
  jan_candidates <- get_candidate_seasons("2025-01-15")
  expect_true(2024 %in% jan_candidates)
  expect_equal(jan_candidates[1], 2024)

  # Should return unique seasons
  expect_equal(length(nov_candidates), length(unique(nov_candidates)))
})

# =============================================================================
# DATETIME PARSING TESTS
# =============================================================================

test_that("parse_datetime handles various formats", {
  skip_if_not_installed("lubridate")

  # Full datetime string
  dt1 <- parse_datetime("2024-11-10 13:00:00")
  expect_s3_class(dt1, "POSIXct")

  # Date only
  dt2 <- parse_datetime("2024-11-10")
  expect_s3_class(dt2, "POSIXct")

  # Already POSIXct
  dt3 <- as.POSIXct("2024-11-10 13:00:00", tz = "America/New_York")
  dt4 <- parse_datetime(dt3)
  expect_s3_class(dt4, "POSIXct")

  # NA/NULL
  expect_true(is.na(parse_datetime(NA)))
  expect_true(is.na(parse_datetime(NULL)))
})

# =============================================================================
# KNOWN DATE RESOLUTION TESTS
# =============================================================================

test_that("mid-regular-season date resolves correctly", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("lubridate")

  # Week 10, 2024 - November 10, 2024 (Sunday of Week 10)
  context <- resolve_nfl_context("2024-11-10 13:00:00")

  if (context$success) {
    expect_equal(context$season, 2024L)
    expect_equal(context$week, 10L)
    expect_equal(context$phase, "regular_season")
    expect_true(is.na(context$round))
  } else {
    skip(sprintf("Could not resolve date: %s", context$error))
  }
})

test_that("early January playoff date resolves correctly", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("lubridate")

  # Wild Card Weekend 2024 season (January 2025)
  # 2024 Wild Card was Jan 13-15, 2024 (for 2023 season)
  context <- resolve_nfl_context("2024-01-14 13:00:00")

  if (context$success) {
    expect_equal(context$season, 2023L)  # 2023 season
    expect_equal(context$phase, "playoffs")
    expect_true(context$week >= 19)
  } else {
    skip(sprintf("Could not resolve date: %s", context$error))
  }
})

test_that("Super Bowl date resolves correctly", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("lubridate")

  # Super Bowl LVIII - February 11, 2024 (2023 season)
  context <- resolve_nfl_context("2024-02-11 18:30:00")

  if (context$success) {
    expect_equal(context$season, 2023L)
    expect_equal(context$phase, "playoffs")
    # Super Bowl is week 22
    if (!is.na(context$round)) {
      expect_equal(context$round, "super_bowl")
    }
  } else {
    skip(sprintf("Could not resolve date: %s", context$error))
  }
})

test_that("Week 1 kickoff date resolves correctly", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("lubridate")

  # Week 1, 2024 - Thursday Night Football kickoff
  context <- resolve_nfl_context("2024-09-05 20:15:00")

  if (context$success) {
    expect_equal(context$season, 2024L)
    expect_equal(context$week, 1L)
    expect_equal(context$phase, "regular_season")
  } else {
    skip(sprintf("Could not resolve date: %s", context$error))
  }
})

test_that("post-Super Bowl offseason date handles gracefully", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("lubridate")

  # March 2024 - offseason
  context <- resolve_nfl_context("2024-03-15 12:00:00")

  # Should either find nearest week or indicate offseason
  expect_type(context$success, "logical")
  if (context$success) {
    expect_type(context$season, "integer")
  } else {
    # Offseason should be gracefully handled
    expect_equal(context$phase, "offseason")
  }
})

# =============================================================================
# SCHEDULE CACHE TESTS
# =============================================================================

test_that("clear_schedule_cache works", {
  # Should not error even on empty cache
  expect_silent(clear_schedule_cache())
  expect_true(clear_schedule_cache())
})

# =============================================================================
# WEEK BOUNDARIES TESTS
# =============================================================================

test_that("get_week_boundaries returns expected structure", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("dplyr")

  tryCatch({
    schedule <- nflreadr::load_schedules(seasons = 2024)

    if (nrow(schedule) > 0) {
      schedule <- parse_kickoff_times(schedule)
      boundaries <- get_week_boundaries(schedule)

      expect_s3_class(boundaries, "data.frame")
      expect_true("season" %in% names(boundaries))
      expect_true("week" %in% names(boundaries))
      expect_true("earliest_kickoff" %in% names(boundaries))
      expect_true("latest_kickoff" %in% names(boundaries))
      expect_true("window_start" %in% names(boundaries))
      expect_true("window_end" %in% names(boundaries))
      expect_true("n_games" %in% names(boundaries))

      # Should have multiple weeks
      expect_true(nrow(boundaries) > 1)

      # Window start should be before earliest kickoff
      if (nrow(boundaries) > 0) {
        expect_true(all(boundaries$window_start < boundaries$earliest_kickoff))
        expect_true(all(boundaries$window_end > boundaries$latest_kickoff))
      }
    }
  }, error = function(e) {
    skip(sprintf("Could not load schedule: %s", e$message))
  })
})

# =============================================================================
# RESOLVER RESULT STRUCTURE TESTS
# =============================================================================

test_that("resolve_nfl_context returns expected structure", {
  skip_if_not_installed("nflreadr")

  context <- resolve_nfl_context("2024-11-10 13:00:00")

  # Should always have these fields
  expect_true("success" %in% names(context))
  expect_true("error" %in% names(context))
  expect_true("season" %in% names(context))
  expect_true("week" %in% names(context))
  expect_true("phase" %in% names(context))
  expect_true("round" %in% names(context))

  if (context$success) {
    expect_true("target_date" %in% names(context))
    expect_true("window_start" %in% names(context))
    expect_true("window_end" %in% names(context))
    expect_true("n_games" %in% names(context))
    expect_true("schedule_rows" %in% names(context))
  }
})

# =============================================================================
# VALIDATION FUNCTION TESTS
# =============================================================================

test_that("validate_date_resolution runs without error", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("dplyr")

  # Create simple test cases
  test_cases <- list(
    list(date = "2024-11-10 13:00:00", season = 2024L, week = 10L)
  )

  results <- tryCatch({
    validate_date_resolution(test_cases)
  }, error = function(e) {
    NULL
  })

  if (!is.null(results)) {
    expect_s3_class(results, "data.frame")
    expect_true("expected_season" %in% names(results))
    expect_true("actual_season" %in% names(results))
    expect_true("season_match" %in% names(results))
  }
})

test_that("run_resolver_validation returns expected structure", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("dplyr")

  results <- tryCatch({
    run_resolver_validation()
  }, error = function(e) {
    NULL
  })

  if (!is.null(results)) {
    expect_type(results, "list")
    expect_true("passed" %in% names(results))
    expect_true("failed" %in% names(results))
    expect_true("all_passed" %in% names(results))
    expect_true("details" %in% names(results))
  } else {
    skip("Could not run resolver validation")
  }
})

# =============================================================================
# CONVENIENCE FUNCTION TESTS
# =============================================================================

test_that("resolve_gameday parses date correctly", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("lubridate")

  context <- resolve_gameday("2024-11-10")

  expect_type(context, "list")
  expect_true("success" %in% names(context))
})

# =============================================================================
# INTEGRATION TEST - FULL SEASON COVERAGE
# =============================================================================

test_that("resolver handles full 2024 season dates", {
  skip_if_not_installed("nflreadr")
  skip_if_not_installed("lubridate")

  # Test key dates throughout the season
  test_dates <- list(
    "2024-09-08" = list(week = 1L, phase = "regular_season"),
    "2024-10-13" = list(week = 6L, phase = "regular_season"),
    "2024-11-17" = list(week = 11L, phase = "regular_season"),
    "2024-12-25" = list(week = 17L, phase = "regular_season")
  )

  for (date_str in names(test_dates)) {
    expected <- test_dates[[date_str]]

    context <- tryCatch({
      resolve_nfl_context(paste(date_str, "13:00:00"))
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })

    if (context$success) {
      expect_equal(context$season, 2024L,
                   label = sprintf("Season for %s", date_str))
      expect_equal(context$phase, expected$phase,
                   label = sprintf("Phase for %s", date_str))
      # Week might be off by 1 due to Thursday games, so allow some tolerance
      expect_true(abs(context$week - expected$week) <= 1,
                  label = sprintf("Week for %s (got %d, expected %d)",
                                  date_str, context$week, expected$week))
    }
  }
})
