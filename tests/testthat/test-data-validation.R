# =============================================================================
# Tests for R/data_validation.R - Data Validation and Quality Tracking
# =============================================================================
# Note: R/data_validation.R is loaded by tests/testthat/setup.R
# =============================================================================

# =============================================================================
# DATA QUALITY TRACKING TESTS
# =============================================================================

test_that("reset_data_quality initializes tracking environment", {
  reset_data_quality()

  quality <- get_data_quality()
  # Uses nested structure: quality$injury$status, not quality$injury_status
  expect_equal(quality$injury$status, "unknown")
  expect_equal(quality$weather$status, "unknown")
  expect_equal(quality$market$status, "unknown")
  expect_equal(quality$calibration$method, "unknown")
  expect_false(quality$calibration$leakage_free)
})

test_that("update_injury_quality tracks injury data status", {
  reset_data_quality()

  # Test full status (valid status is "full", not "complete")
  update_injury_quality("full")
  quality <- get_data_quality()
  expect_equal(quality$injury$status, "full")
  expect_length(quality$injury$missing_seasons, 0)

  # Test partial status
  update_injury_quality("partial", missing_seasons = c("2025"))
  quality <- get_data_quality()
  expect_equal(quality$injury$status, "partial")
  expect_equal(quality$injury$missing_seasons, c("2025"))

  # Test unavailable status (valid status is "unavailable", not "missing")
  update_injury_quality("unavailable", missing_seasons = c("2024", "2025"))
  quality <- get_data_quality()
  expect_equal(quality$injury$status, "unavailable")
  expect_length(quality$injury$missing_seasons, 2)
})

test_that("update_weather_quality tracks weather data status", {
  reset_data_quality()

  # Test full status (valid status is "full", not "api")
  update_weather_quality("full")
  quality <- get_data_quality()
  expect_equal(quality$weather$status, "full")
  expect_length(quality$weather$fallback_games, 0)

  # Test partial_fallback status with fallback games
  update_weather_quality("partial_fallback", fallback_games = c("game1", "game2"))
  quality <- get_data_quality()
  expect_equal(quality$weather$status, "partial_fallback")
  expect_equal(quality$weather$fallback_games, c("game1", "game2"))
})

test_that("update_market_quality tracks market data status", {
  reset_data_quality()

  # Test full status (valid status is "full", not "complete")
  update_market_quality("full")
  quality <- get_data_quality()
  expect_equal(quality$market$status, "full")

  # Test unavailable with games (valid status is "unavailable", not "missing")
  update_market_quality("unavailable", missing_games = c("game1"))
  quality <- get_data_quality()
  expect_equal(quality$market$status, "unavailable")
  expect_equal(quality$market$missing_games, c("game1"))
})

test_that("update_calibration_quality tracks calibration method", {
  reset_data_quality()

  # Test nested CV (leakage-free)
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)
  quality <- get_data_quality()
  expect_equal(quality$calibration$method, "isotonic_nested_cv")
  expect_true(quality$calibration$leakage_free)

  # Test global (has leakage)
  update_calibration_quality("isotonic_global", leakage_free = FALSE)
  quality <- get_data_quality()
  expect_equal(quality$calibration$method, "isotonic_global")
  expect_false(quality$calibration$leakage_free)

  # Test none
  update_calibration_quality("none")
  quality <- get_data_quality()
  expect_equal(quality$calibration$method, "none")
})

test_that("compute_overall_quality returns correct ratings", {
  reset_data_quality()

  # All good - should be "HIGH" (uppercase)
  update_injury_quality("full")
  update_weather_quality("full")
  update_market_quality("full")
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)
  expect_equal(compute_overall_quality(), "HIGH")

  # Partial injury only = 1 issue, still "HIGH" (need >= 2 for MEDIUM)
  reset_data_quality()
  update_injury_quality("partial", missing_seasons = "2025")
  update_weather_quality("full")
  update_market_quality("full")
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)
  expect_equal(compute_overall_quality(), "HIGH")

  # Partial injury + not leakage free = 2 issues = "MEDIUM"
  reset_data_quality()
  update_injury_quality("partial", missing_seasons = "2025")
  update_weather_quality("full")
  update_market_quality("full")
  update_calibration_quality("isotonic_global", leakage_free = FALSE)
  expect_equal(compute_overall_quality(), "MEDIUM")

  # Missing critical data - should be "CRITICAL" (market unavailable)
  reset_data_quality()
  update_injury_quality("unavailable", missing_seasons = c("2024", "2025"))
  update_weather_quality("all_fallback")
  update_market_quality("unavailable")
  update_calibration_quality("none")
  expect_equal(compute_overall_quality(), "CRITICAL")
})

test_that("generate_quality_badge_html produces valid HTML", {
  reset_data_quality()
  update_injury_quality("full")
  update_weather_quality("full")
  update_market_quality("full")
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)

  html <- generate_quality_badge_html()

  # Should contain HTML elements (uses <div> not <section>)
  expect_true(grepl("<div", html))
  expect_true(grepl("Data Quality", html, ignore.case = TRUE))
  expect_true(grepl("</div>", html))
  expect_true(grepl("HIGH", html))
})

# =============================================================================
# VALIDATION FUNCTION TESTS
# =============================================================================

test_that("validate_required_columns checks column presence", {
  df <- data.frame(a = 1, b = 2, c = 3)

  # All present - returns TRUE
  result <- validate_required_columns(df, c("a", "b"), "test", strict = FALSE)
  expect_true(result)

  # Some missing - returns FALSE (non-strict mode)
  expect_warning({
    result <- validate_required_columns(df, c("a", "d"), "test", strict = FALSE)
  })
  expect_false(result)
})

test_that("validate_probability checks probability range", {
  # Valid probabilities - returns TRUE
  result <- validate_probability(c(0.1, 0.5, 0.9), "test")
  expect_true(result)

  # Invalid probabilities - throws error
  expect_error(validate_probability(c(-0.1, 0.5, 1.1), "test"))
})

test_that("validate_numeric_column checks numeric range", {
  # Valid range - returns TRUE
  result <- validate_numeric_column(c(1, 5, 10), "test", min_val = 0, max_val = 20)
  expect_true(result)

  # Out of range - throws error
  expect_error(validate_numeric_column(c(-5, 5, 25), "test", min_val = 0, max_val = 20))
})

test_that("create_validation_report generates report structure", {
  # Create minimal test schedule
  schedule <- data.frame(
    game_id = c("2024_01_KC_DEN"),
    season = 2024L,
    week = 1L,
    home_team = "KC",
    away_team = "DEN",
    gameday = "2024-09-05",
    stringsAsFactors = FALSE
  )

  report <- create_validation_report(schedule)

  expect_type(report, "list")
  expect_true("schedule" %in% names(report))
  expect_true("issues" %in% names(report))
})
