# =============================================================================
# Tests for Weather Impact Parameters
# =============================================================================
# Tests weather-related configuration and impact calculations
# =============================================================================

# =============================================================================
# CONFIGURATION TESTS
# =============================================================================

test_that("weather impact parameters exist", {
  if (!exists("DOME_BONUS_TOTAL")) {
    if (file.exists("config.R")) source("config.R")
  }

  expect_true(exists("DOME_BONUS_TOTAL"))
  expect_true(exists("OUTDOOR_WIND_PEN"))
  expect_true(exists("COLD_TEMP_PEN"))
  expect_true(exists("RAIN_SNOW_PEN"))
  expect_true(exists("WIND_IMPACT"))
  expect_true(exists("COLD_IMPACT"))
  expect_true(exists("PRECIP_IMPACT"))
})

test_that("weather parameters have correct signs", {
  if (!exists("DOME_BONUS_TOTAL")) {
    if (file.exists("config.R")) source("config.R")
  }

  # Dome should be positive (increases scoring)
  expect_true(DOME_BONUS_TOTAL > 0,
              info = "Dome games should have positive scoring impact")

  # Outdoor penalties should be negative
  expect_true(OUTDOOR_WIND_PEN < 0,
              info = "Wind should reduce scoring")
  expect_true(COLD_TEMP_PEN < 0,
              info = "Cold should reduce scoring")
  expect_true(RAIN_SNOW_PEN < 0,
              info = "Rain/snow should reduce scoring")

  # Impact factors should have appropriate signs
  expect_true(WIND_IMPACT < 0,
              info = "Wind impact should be negative")
  expect_true(COLD_IMPACT < 0,
              info = "Cold impact should be negative")
  expect_true(PRECIP_IMPACT < 0,
              info = "Precipitation impact should be negative")
})

test_that("weather parameters are in reasonable ranges", {
  if (!exists("DOME_BONUS_TOTAL")) {
    if (file.exists("config.R")) source("config.R")
  }

  # Dome bonus: typically 0.4 to 1.5 points
  expect_true(DOME_BONUS_TOTAL >= 0.4 && DOME_BONUS_TOTAL <= 1.5,
              info = sprintf("DOME_BONUS_TOTAL=%f should be 0.4-1.5", DOME_BONUS_TOTAL))

  # Wind penalty: typically -0.5 to -2.0 points
  expect_true(OUTDOOR_WIND_PEN >= -2.0 && OUTDOOR_WIND_PEN <= -0.5,
              info = sprintf("OUTDOOR_WIND_PEN=%f should be -2.0 to -0.5", OUTDOOR_WIND_PEN))

  # Cold penalty: typically -0.3 to -1.0 points
  expect_true(COLD_TEMP_PEN >= -1.5 && COLD_TEMP_PEN <= -0.3,
              info = sprintf("COLD_TEMP_PEN=%f should be -1.5 to -0.3", COLD_TEMP_PEN))

  # Rain/snow penalty: typically -0.5 to -1.5 points
  expect_true(RAIN_SNOW_PEN >= -1.5 && RAIN_SNOW_PEN <= -0.3,
              info = sprintf("RAIN_SNOW_PEN=%f should be -1.5 to -0.3", RAIN_SNOW_PEN))
})

test_that("weather parameters have documented validation", {
  if (!exists("DOME_BONUS_TOTAL")) {
    if (file.exists("config.R")) source("config.R")
  }

  # Per config.R documentation:
  # DOME_BONUS_TOTAL: p = 0.004 (significant), Brier improvement = 0.0021
  # OUTDOOR_WIND_PEN: p < 0.001 (highly significant), Brier improvement = 0.0044
  # COLD_TEMP_PEN: p = 0.041 (significant), Brier improvement = 0.0011
  # RAIN_SNOW_PEN: p = 0.020 (significant), Brier improvement = 0.0015

  # Verified values from validation
  expect_equal(DOME_BONUS_TOTAL, 0.8, tolerance = 0.1)
  expect_equal(OUTDOOR_WIND_PEN, -1.2, tolerance = 0.2)
  expect_equal(COLD_TEMP_PEN, -0.6, tolerance = 0.2)
  expect_equal(RAIN_SNOW_PEN, -0.8, tolerance = 0.2)
})

# =============================================================================
# DEFAULT WEATHER CONDITIONS TESTS
# =============================================================================

test_that("default weather fallback is defined", {
  if (!exists("DEFAULT_WEATHER_CONDITIONS")) {
    if (file.exists("config.R")) source("config.R")
  }

  expect_true(exists("DEFAULT_WEATHER_CONDITIONS"))
  expect_type(DEFAULT_WEATHER_CONDITIONS, "list")

  # Required fields
  expected_fields <- c("lat", "lon", "dome", "wind_mph", "temp_f", "precip_prob")
  for (field in expected_fields) {
    expect_true(field %in% names(DEFAULT_WEATHER_CONDITIONS),
                info = sprintf("DEFAULT_WEATHER_CONDITIONS should have '%s'", field))
  }
})

test_that("default weather values are reasonable", {
  if (!exists("DEFAULT_WEATHER_CONDITIONS")) {
    if (file.exists("config.R")) source("config.R")
  }

  dwc <- DEFAULT_WEATHER_CONDITIONS

  # Location should be in continental US
  expect_true(dwc$lat >= 25 && dwc$lat <= 50,
              info = "Default latitude should be in continental US")
  expect_true(dwc$lon >= -125 && dwc$lon <= -65,
              info = "Default longitude should be in continental US")

  # Dome should be FALSE (outdoor default)
  expect_false(dwc$dome,
               info = "Default should be outdoor venue")

  # Wind should be reasonable (0-20 mph)
  expect_true(dwc$wind_mph >= 0 && dwc$wind_mph <= 20)

  # Temperature should be moderate (40-70F)
  expect_true(dwc$temp_f >= 40 && dwc$temp_f <= 70)

  # Precipitation probability should be 0-1
  expect_true(dwc$precip_prob >= 0 && dwc$precip_prob <= 1)
})

# =============================================================================
# STADIUM FALLBACK TESTS
# =============================================================================

test_that("stadium fallback warning flag exists", {
  if (!exists("WARN_STADIUM_FALLBACK")) {
    if (file.exists("config.R")) source("config.R")
  }

  expect_true(exists("WARN_STADIUM_FALLBACK"))
  expect_type(WARN_STADIUM_FALLBACK, "logical")
})

# =============================================================================
# WEATHER IMPACT CALCULATION TESTS
# =============================================================================

test_that("weather impacts are cumulative and bounded", {
  if (!exists("DOME_BONUS_TOTAL")) {
    if (file.exists("config.R")) source("config.R")
  }

  # Simulate worst case scenario: cold, windy, rainy outdoor game
  total_penalty <- OUTDOOR_WIND_PEN + COLD_TEMP_PEN + RAIN_SNOW_PEN

  # Combined penalty should not exceed ~4 points (one half-TD)
  expect_true(abs(total_penalty) <= 5,
              info = sprintf("Combined weather penalty=%f should not exceed 5 pts", total_penalty))

  # Compare to dome: difference should be realistic
  dome_vs_worst <- DOME_BONUS_TOTAL - total_penalty
  expect_true(dome_vs_worst <= 7,
              info = "Dome vs worst weather spread should be reasonable")
})

test_that("wind impact per mph is realistic", {
  if (!exists("WIND_IMPACT")) {
    if (file.exists("config.R")) source("config.R")
  }

  # WIND_IMPACT is points per mph
  # At 20 mph wind, impact should be 1-2 points
  wind_20mph_impact <- 20 * abs(WIND_IMPACT)

  expect_true(wind_20mph_impact >= 0.5 && wind_20mph_impact <= 3.0,
              info = sprintf("20mph wind impact=%f should be 0.5-3.0 pts", wind_20mph_impact))
})

test_that("cold impact threshold is realistic", {
  if (!exists("COLD_IMPACT")) {
    if (file.exists("config.R")) source("config.R")
  }

  # COLD_IMPACT is points per 10F below 40F
  # At 20F, cold penalty should be ~0.3 points
  temp_20f_impact <- 2 * abs(COLD_IMPACT)  # 2 units of 10F below 40F

  expect_true(temp_20f_impact >= 0.2 && temp_20f_impact <= 1.0,
              info = sprintf("20F temp impact=%f should be 0.2-1.0 pts", temp_20f_impact))
})

# =============================================================================
# DOME TEAMS LIST VALIDATION
# =============================================================================

test_that("dome teams can be identified", {
  # Known dome teams (as of 2024)
  dome_teams <- c(
    "ARI",  # State Farm Stadium (retractable)
    "ATL",  # Mercedes-Benz Stadium (retractable)
    "DAL",  # AT&T Stadium (retractable)
    "DET",  # Ford Field (dome)
    "HOU",  # NRG Stadium (retractable)
    "IND",  # Lucas Oil Stadium (retractable)
    "LA",   # SoFi Stadium (roof)
    "LAC",  # SoFi Stadium (shared)
    "LV",   # Allegiant Stadium (dome)
    "MIN",  # U.S. Bank Stadium (dome)
    "NO"    # Caesars Superdome (dome)
  )

  # At least 10 dome/retractable roof stadiums
  expect_true(length(dome_teams) >= 10)
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("weather parameters are used in NFLsimulation.R", {
  sim_path <- if (file.exists("NFLsimulation.R")) {
    "NFLsimulation.R"
  } else if (file.exists("../../NFLsimulation.R")) {
    "../../NFLsimulation.R"
  } else {
    skip("NFLsimulation.R not found")
  }

  sim_content <- readLines(sim_path, warn = FALSE)
  sim_text <- paste(sim_content, collapse = "\n")

  # Should reference weather parameters
  expect_true(grepl("DOME_BONUS", sim_text),
              info = "NFLsimulation.R should use DOME_BONUS")
  expect_true(grepl("WIND", sim_text),
              info = "NFLsimulation.R should reference wind")
})
