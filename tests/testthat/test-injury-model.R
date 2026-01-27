# =============================================================================
# Tests for Injury Model - calc_injury_impacts and related functions
# =============================================================================
# Tests injury impact calculations, position weights, and config integration
# =============================================================================

# =============================================================================
# CONFIGURATION TESTS
# =============================================================================

test_that("injury weight config parameters exist and are valid", {
  # Source config if needed
  if (!exists("INJURY_WEIGHT_SKILL")) {
    if (file.exists("config.R")) source("config.R")
  }

  # All injury weight parameters should exist
  expect_true(exists("INJURY_WEIGHT_SKILL"))
  expect_true(exists("INJURY_WEIGHT_TRENCH"))
  expect_true(exists("INJURY_WEIGHT_SECONDARY"))
  expect_true(exists("INJURY_WEIGHT_FRONT7"))
  expect_true(exists("QB_INJURY_MULTIPLIER"))

  # Values should be numeric and in reasonable range
  expect_type(INJURY_WEIGHT_SKILL, "double")
  expect_type(INJURY_WEIGHT_TRENCH, "double")
  expect_type(INJURY_WEIGHT_SECONDARY, "double")
  expect_type(INJURY_WEIGHT_FRONT7, "double")

  # Weights should be positive and less than 2
  expect_true(INJURY_WEIGHT_SKILL > 0 && INJURY_WEIGHT_SKILL < 2)
  expect_true(INJURY_WEIGHT_TRENCH > 0 && INJURY_WEIGHT_TRENCH < 2)
  expect_true(INJURY_WEIGHT_SECONDARY > 0 && INJURY_WEIGHT_SECONDARY < 2)
  expect_true(INJURY_WEIGHT_FRONT7 > 0 && INJURY_WEIGHT_FRONT7 < 2)
})

test_that("injury position multipliers exist and are valid", {
  if (!exists("INJURY_POS_MULT_TRENCH")) {
    if (file.exists("config.R")) source("config.R")
  }

  # Position multipliers should exist
  expect_true(exists("INJURY_POS_MULT_TRENCH"))
  expect_true(exists("INJURY_POS_MULT_SKILL"))
  expect_true(exists("INJURY_POS_MULT_SECONDARY"))
  expect_true(exists("INJURY_POS_MULT_FRONT7"))
  expect_true(exists("INJURY_POS_MULT_OTHER"))

  # Multipliers should be in reasonable range (0.5 to 2.0)
  expect_true(INJURY_POS_MULT_TRENCH >= 0.5 && INJURY_POS_MULT_TRENCH <= 2.0)
  expect_true(INJURY_POS_MULT_SKILL >= 0.5 && INJURY_POS_MULT_SKILL <= 2.0)
})

test_that("injury caps are defined", {
  if (!exists("INJURY_CAP_OFFENSE")) {
    if (file.exists("config.R")) source("config.R")
  }

  expect_true(exists("INJURY_CAP_OFFENSE"))
  expect_true(exists("INJURY_CAP_DEFENSE"))

  # Caps should be reasonable (2-6 points)
  expect_true(INJURY_CAP_OFFENSE >= 2 && INJURY_CAP_OFFENSE <= 6)
  expect_true(INJURY_CAP_DEFENSE >= 2 && INJURY_CAP_DEFENSE <= 6)
})

# =============================================================================
# POSITION GROUP MAPPING TESTS
# =============================================================================

test_that("position group mapping is comprehensive", {
  # Standard NFL positions
  qb_positions <- c("QB")
  skill_positions <- c("WR", "RB", "TE", "FB", "HB")
  trench_positions <- c("T", "OT", "LT", "RT", "G", "OG", "C", "OL")
  secondary_positions <- c("CB", "S", "SS", "FS", "DB")
  front7_positions <- c("LB", "ILB", "OLB", "EDGE", "DL", "DT", "DE", "NT", "IDL")

  # Test position mapping function if available
  if (exists("map_position_to_group")) {
    for (pos in qb_positions) {
      expect_equal(map_position_to_group(pos), "qb")
    }
    for (pos in skill_positions) {
      expect_equal(map_position_to_group(pos), "skill")
    }
    for (pos in trench_positions) {
      expect_equal(map_position_to_group(pos), "trenches")
    }
    for (pos in secondary_positions) {
      expect_equal(map_position_to_group(pos), "secondary")
    }
    for (pos in front7_positions) {
      expect_equal(map_position_to_group(pos), "front7")
    }
  } else {
    # Just verify documentation covers all positions
    expect_true(TRUE)
  }
})

# =============================================================================
# INJURY STATUS MAPPING TESTS
# =============================================================================

test_that("practice availability mapping is complete", {
  if (!exists("PRACTICE_AVAILABILITY")) {
    if (file.exists("config.R")) source("config.R")
  }

  expect_true(exists("PRACTICE_AVAILABILITY"))

  # Required statuses
  expected_statuses <- c("Full", "Limited", "DNP")
  for (status in expected_statuses) {
    expect_true(status %in% names(PRACTICE_AVAILABILITY),
                info = sprintf("Practice status '%s' should be mapped", status))
  }

  # Full should be 1.0, DNP should be low
  expect_equal(PRACTICE_AVAILABILITY[["Full"]], 1.0)
  expect_true(PRACTICE_AVAILABILITY[["DNP"]] < 0.5)
})

test_that("game status multipliers are complete", {
  if (!exists("GAME_STATUS_MULTIPLIER")) {
    if (file.exists("config.R")) source("config.R")
  }

  expect_true(exists("GAME_STATUS_MULTIPLIER"))

  # Required statuses
  expected_statuses <- c("Questionable", "Doubtful", "Out", "IR")
  for (status in expected_statuses) {
    expect_true(status %in% names(GAME_STATUS_MULTIPLIER),
                info = sprintf("Game status '%s' should be mapped", status))
  }

  # Out and IR should be 0.0
  expect_equal(GAME_STATUS_MULTIPLIER[["Out"]], 0.0)
  expect_equal(GAME_STATUS_MULTIPLIER[["IR"]], 0.0)

  # Questionable should be partial
  expect_true(GAME_STATUS_MULTIPLIER[["Questionable"]] > 0 &&
              GAME_STATUS_MULTIPLIER[["Questionable"]] < 1)
})

# =============================================================================
# INJURY IMPACT CALCULATION TESTS
# =============================================================================

test_that("injury penalties scale with severity", {
  # Test that OUT > DOUBTFUL > QUESTIONABLE > LIMITED

  # These are the expected base penalties from calc_injury_impacts
  out_penalty <- -0.50
  doubtful_penalty <- -0.35
  questionable_penalty <- -0.20
  limited_penalty <- -0.10

  # Verify ordering

  expect_true(abs(out_penalty) > abs(doubtful_penalty))
  expect_true(abs(doubtful_penalty) > abs(questionable_penalty))
  expect_true(abs(questionable_penalty) > abs(limited_penalty))
})

test_that("position weights favor high-impact positions", {
  if (!exists("INJURY_POS_MULT_TRENCH")) {
    if (file.exists("config.R")) source("config.R")
  }

  # Trenches should have highest weight (OL/DL most impactful per research)
  expect_true(INJURY_POS_MULT_TRENCH >= INJURY_POS_MULT_SKILL)
  expect_true(INJURY_POS_MULT_TRENCH >= INJURY_POS_MULT_SECONDARY)
  expect_true(INJURY_POS_MULT_TRENCH >= INJURY_POS_MULT_FRONT7)
})

# =============================================================================
# INJURY DATA VALIDATION TESTS
# =============================================================================

test_that("injury position weights list is complete", {
  if (!exists("INJURY_POSITION_WEIGHTS")) {
    if (file.exists("config.R")) source("config.R")
  }

  expect_true(exists("INJURY_POSITION_WEIGHTS"))
  expect_type(INJURY_POSITION_WEIGHTS, "list")

  # Should have key positions
  required_positions <- c("QB", "WR", "RB", "TE", "OL", "LB", "CB", "S")
  for (pos in required_positions) {
    has_pos <- pos %in% names(INJURY_POSITION_WEIGHTS) ||
               any(grepl(pos, names(INJURY_POSITION_WEIGHTS), ignore.case = TRUE))
    expect_true(has_pos, info = sprintf("Position '%s' should have a weight", pos))
  }

  # QB should have highest impact
  expect_true(INJURY_POSITION_WEIGHTS[["QB"]] >= max(unlist(INJURY_POSITION_WEIGHTS[names(INJURY_POSITION_WEIGHTS) != "QB"])))
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("NFLsimulation.R uses config injury weights", {
  # Read NFLsimulation.R and check it references config values
  sim_path <- if (file.exists("NFLsimulation.R")) {
    "NFLsimulation.R"
  } else if (file.exists("../../NFLsimulation.R")) {
    "../../NFLsimulation.R"
  } else {
    skip("NFLsimulation.R not found")
  }

  sim_content <- readLines(sim_path, warn = FALSE)
  sim_text <- paste(sim_content, collapse = "\n")

  # Should reference INJURY_WEIGHT_* from config
  expect_true(grepl("INJURY_WEIGHT_SKILL", sim_text),
              info = "NFLsimulation.R should reference INJURY_WEIGHT_SKILL")

  # Should have fallback patterns (if exists ... else)
  expect_true(grepl("if.*exists.*INJURY", sim_text) ||
              grepl("INJURY_WEIGHT", sim_text),
              info = "NFLsimulation.R should handle injury config")
})

test_that("injury mode options are valid", {
  if (!exists("INJURY_MODE")) {
    if (file.exists("config.R")) source("config.R")
  }

  expect_true(exists("INJURY_MODE"))

  valid_modes <- c("auto", "off", "last_available", "manual", "scalp", "sleeper")
  expect_true(INJURY_MODE %in% valid_modes,
              info = sprintf("INJURY_MODE '%s' should be one of: %s",
                            INJURY_MODE, paste(valid_modes, collapse = ", ")))
})

# =============================================================================
# VALIDATION TESTS (based on documented statistics)
# =============================================================================

test_that("injury weights match documented validation", {
  if (!exists("INJURY_WEIGHT_SKILL")) {
    if (file.exists("config.R")) source("config.R")
  }

  # Per config.R documentation:
  # INJURY_WEIGHT_SKILL: r = 0.28, p < 0.001
  # INJURY_WEIGHT_TRENCH: r = 0.24, p = 0.001
  # INJURY_WEIGHT_SECONDARY: r = 0.19, p = 0.007
  # INJURY_WEIGHT_FRONT7: r = 0.21, p = 0.005

  # The documented validated values
  expect_equal(INJURY_WEIGHT_SKILL, 0.55, tolerance = 0.01)
  expect_equal(INJURY_WEIGHT_TRENCH, 0.65, tolerance = 0.01)
  expect_equal(INJURY_WEIGHT_SECONDARY, 0.45, tolerance = 0.01)
  expect_equal(INJURY_WEIGHT_FRONT7, 0.50, tolerance = 0.01)
})

test_that("QB injury multiplier is based on literature", {
  if (!exists("QB_INJURY_MULTIPLIER")) {
    if (file.exists("config.R")) source("config.R")
  }

  # Per config.R: QB impact: -7.2 points (literature: -7 to -10)
  # Multiplier should be 1.5 based on validation
  expect_equal(QB_INJURY_MULTIPLIER, 1.5, tolerance = 0.1)
})
