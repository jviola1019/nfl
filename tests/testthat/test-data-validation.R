# =============================================================================
# Tests for R/data_validation.R - Data Validation and Quality Tracking
# =============================================================================

# Load the utilities
source(file.path(dirname(dirname(dirname(testthat::test_path()))), "R", "data_validation.R"))

# =============================================================================
# DATA QUALITY TRACKING TESTS
# =============================================================================

test_that("reset_data_quality initializes tracking environment", {
  reset_data_quality()

  quality <- get_data_quality()
  expect_equal(quality$injury_status, "unknown")
  expect_equal(quality$weather_status, "unknown")
  expect_equal(quality$market_status, "unknown")
  expect_equal(quality$calibration_method, "unknown")
  expect_false(quality$calibration_leakage_free)
})

test_that("update_injury_quality tracks injury data status", {
  reset_data_quality()

  # Test complete status
  update_injury_quality("complete")
  quality <- get_data_quality()
  expect_equal(quality$injury_status, "complete")
  expect_length(quality$injury_seasons_missing, 0)

  # Test partial status
  update_injury_quality("partial", seasons_missing = c("2025"))
  quality <- get_data_quality()
  expect_equal(quality$injury_status, "partial")
  expect_equal(quality$injury_seasons_missing, c("2025"))

  # Test missing status
  update_injury_quality("missing", seasons_missing = c("2024", "2025"))
  quality <- get_data_quality()
  expect_equal(quality$injury_status, "missing")
  expect_length(quality$injury_seasons_missing, 2)
})

test_that("update_weather_quality tracks weather data status", {
  reset_data_quality()

  # Test API status
  update_weather_quality("api")
  quality <- get_data_quality()
  expect_equal(quality$weather_status, "api")
  expect_length(quality$weather_games_fallback, 0)

  # Test partial status with fallback games
  update_weather_quality("partial", games_fallback = c("game1", "game2"))
  quality <- get_data_quality()
  expect_equal(quality$weather_status, "partial")
  expect_equal(quality$weather_games_fallback, c("game1", "game2"))
})

test_that("update_market_quality tracks market data status", {
  reset_data_quality()

  # Test complete status
  update_market_quality("complete")
  quality <- get_data_quality()
  expect_equal(quality$market_status, "complete")

  # Test missing with games
  update_market_quality("missing", games_missing = c("game1"))
  quality <- get_data_quality()
  expect_equal(quality$market_status, "missing")
  expect_equal(quality$market_games_missing, c("game1"))
})

test_that("update_calibration_quality tracks calibration method", {
  reset_data_quality()

  # Test nested CV (leakage-free)
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)
  quality <- get_data_quality()
  expect_equal(quality$calibration_method, "isotonic_nested_cv")
  expect_true(quality$calibration_leakage_free)

  # Test global (has leakage)
  update_calibration_quality("isotonic_global", leakage_free = FALSE)
  quality <- get_data_quality()
  expect_equal(quality$calibration_method, "isotonic_global")
  expect_false(quality$calibration_leakage_free)

  # Test none
  update_calibration_quality("none")
  quality <- get_data_quality()
  expect_equal(quality$calibration_method, "none")
})

test_that("compute_overall_quality returns correct ratings", {
  reset_data_quality()

  # All good - should be "high"
  update_injury_quality("complete")
  update_weather_quality("api")
  update_market_quality("complete")
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)
  expect_equal(compute_overall_quality(), "high")

  # Partial data - should be "medium"
  reset_data_quality()
  update_injury_quality("partial", seasons_missing = "2025")
  update_weather_quality("api")
  update_market_quality("complete")
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)
  expect_equal(compute_overall_quality(), "medium")

  # Missing critical data - should be "low"
  reset_data_quality()
  update_injury_quality("missing", seasons_missing = c("2024", "2025"))
  update_weather_quality("default")
  update_market_quality("missing")
  update_calibration_quality("none")
  expect_equal(compute_overall_quality(), "low")
})

test_that("generate_quality_badge_html produces valid HTML", {
  reset_data_quality()
  update_injury_quality("complete")
  update_weather_quality("api")
  update_market_quality("complete")
  update_calibration_quality("isotonic_nested_cv", leakage_free = TRUE)

  html <- generate_quality_badge_html()

  # Should contain HTML elements
  expect_true(grepl("<section", html))
  expect_true(grepl("Data Quality", html, ignore.case = TRUE))
  expect_true(grepl("</section>", html))
})

# =============================================================================
# INJURY DATA VALIDATION TESTS
# =============================================================================

test_that("check_injury_availability returns expected structure", {
  # This test may require network access, so we'll just test the structure
  result <- check_injury_availability(2023)
  expect_type(result, "list")
  expect_true("available" %in% names(result))
  expect_true("season" %in% names(result))
  expect_equal(result$season, 2023)
})

# =============================================================================
# WEATHER FALLBACK TESTS
# =============================================================================

test_that("get_default_weather_conditions returns expected structure", {
  game_ids <- c("game1", "game2")
  defaults <- get_default_weather_conditions(game_ids)

  expect_s3_class(defaults, "data.frame")
  expect_equal(nrow(defaults), 2)
  expect_true("game_id" %in% names(defaults))
  expect_true("temp_f" %in% names(defaults))
  expect_true("wind_mph" %in% names(defaults))
  expect_true("dome" %in% names(defaults))
  expect_true("weather_source" %in% names(defaults))
  expect_equal(defaults$weather_source, c("default", "default"))
})

test_that("get_default_weather_conditions uses config values", {
  # Save original config
  original_config <- if (exists("DEFAULT_WEATHER_CONDITIONS", envir = .GlobalEnv)) {
    get("DEFAULT_WEATHER_CONDITIONS", envir = .GlobalEnv)
  } else {
    NULL
  }

  # Set custom config
  assign("DEFAULT_WEATHER_CONDITIONS",
         list(temp_f = 72, wind_mph = 5, dome = TRUE, precip_prob = 0),
         envir = .GlobalEnv)

  defaults <- get_default_weather_conditions("test_game")
  expect_equal(defaults$temp_f, 72)
  expect_equal(defaults$wind_mph, 5)
  expect_true(defaults$dome)

  # Restore original
  if (!is.null(original_config)) {
    assign("DEFAULT_WEATHER_CONDITIONS", original_config, envir = .GlobalEnv)
  } else {
    rm("DEFAULT_WEATHER_CONDITIONS", envir = .GlobalEnv)
  }
})
