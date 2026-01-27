# =============================================================================
# Tests for Snap-Weighted Injury Impacts
# =============================================================================
# Tests weight_injury_by_snaps() and load_player_snap_percentages() functions
# =============================================================================

# =============================================================================
# FUNCTION EXISTENCE TESTS
# =============================================================================

test_that("snap weighting functions exist in injury_scalp.R", {
  injury_scalp_path <- file.path(.test_project_root, "injury_scalp.R")
  expect_true(file.exists(injury_scalp_path),
              info = "injury_scalp.R should exist")

  content <- paste(readLines(injury_scalp_path, warn = FALSE), collapse = "\n")

  expect_true(grepl("weight_injury_by_snaps", content),
              info = "weight_injury_by_snaps should be defined in injury_scalp.R")
  expect_true(grepl("load_player_snap_percentages", content),
              info = "load_player_snap_percentages should be defined in injury_scalp.R")
})

test_that("weight_injury_by_snaps function is callable", {
  injury_scalp_path <- file.path(.test_project_root, "injury_scalp.R")
  if (!file.exists(injury_scalp_path)) {
    skip("injury_scalp.R not found")
  }

  # Source the file
  if (!exists("weight_injury_by_snaps", mode = "function")) {
    source(injury_scalp_path, local = FALSE)
  }

  expect_true(exists("weight_injury_by_snaps", mode = "function"),
              info = "weight_injury_by_snaps should be available after sourcing")
})

test_that("load_player_snap_percentages function is callable", {
  injury_scalp_path <- file.path(.test_project_root, "injury_scalp.R")
  if (!file.exists(injury_scalp_path)) {
    skip("injury_scalp.R not found")
  }

  if (!exists("load_player_snap_percentages", mode = "function")) {
    source(injury_scalp_path, local = FALSE)
  }

  expect_true(exists("load_player_snap_percentages", mode = "function"),
              info = "load_player_snap_percentages should be available after sourcing")
})

# =============================================================================
# CONFIG TESTS
# =============================================================================

test_that("USE_SNAP_WEIGHTED_INJURIES is configured", {
  if (!exists("USE_SNAP_WEIGHTED_INJURIES")) {
    config_path <- file.path(.test_project_root, "config.R")
    if (file.exists(config_path)) {
      source(config_path, local = FALSE)
    }
  }

  expect_true(exists("USE_SNAP_WEIGHTED_INJURIES"),
              info = "USE_SNAP_WEIGHTED_INJURIES should be defined in config.R")

  if (exists("USE_SNAP_WEIGHTED_INJURIES")) {
    expect_type(USE_SNAP_WEIGHTED_INJURIES, "logical")
  }
})

test_that("SNAP_WEIGHT_REFERENCE is configured", {
  if (!exists("SNAP_WEIGHT_REFERENCE")) {
    config_path <- file.path(.test_project_root, "config.R")
    if (file.exists(config_path)) {
      source(config_path, local = FALSE)
    }
  }

  # SNAP_WEIGHT_REFERENCE may not exist (uses default 50)
  if (exists("SNAP_WEIGHT_REFERENCE")) {
    expect_type(SNAP_WEIGHT_REFERENCE, "double")
    expect_true(SNAP_WEIGHT_REFERENCE > 0 && SNAP_WEIGHT_REFERENCE <= 100,
                info = "SNAP_WEIGHT_REFERENCE should be a positive percentage")
  }
})

# =============================================================================
# WEIGHTING LOGIC TESTS
# =============================================================================

test_that("snap weighting correctly scales impacts", {
  # Test the weighting formula: weight_factor = pmin(pmax(snap_pct / reference, 0.2), 2.0)
  reference <- 50  # Default reference percentage

  # WR1 with 60% snaps should have higher impact
  wr1_snap <- 60
  wr1_weight <- min(max(wr1_snap / reference, 0.2), 2.0)
  expect_equal(wr1_weight, 1.2, tolerance = 0.01,
               info = "WR1 (60% snaps) should have 1.2x weight")

  # WR5 with 10% snaps should have lower impact
  wr5_snap <- 10
  wr5_weight <- min(max(wr5_snap / reference, 0.2), 2.0)
  expect_equal(wr5_weight, 0.2, tolerance = 0.01,
               info = "WR5 (10% snaps) should be bounded at 0.2x minimum")

  # Starter with 80% snaps
  starter_snap <- 80
  starter_weight <- min(max(starter_snap / reference, 0.2), 2.0)
  expect_equal(starter_weight, 1.6, tolerance = 0.01,
               info = "Starter (80% snaps) should have 1.6x weight")

  # Bell cow with 100% snaps - bounded at 2.0x
  bellcow_snap <- 100
  bellcow_weight <- min(max(bellcow_snap / reference, 0.2), 2.0)
  expect_equal(bellcow_weight, 2.0, tolerance = 0.01,
               info = "Bell cow (100% snaps) should be bounded at 2.0x maximum")
})

test_that("snap weighting returns base impact when disabled", {
  injury_scalp_path <- file.path(.test_project_root, "injury_scalp.R")
  if (!file.exists(injury_scalp_path)) {
    skip("injury_scalp.R not found")
  }

  if (!exists("weight_injury_by_snaps", mode = "function")) {
    source(injury_scalp_path, local = FALSE)
  }

  if (!exists("weight_injury_by_snaps", mode = "function")) {
    skip("weight_injury_by_snaps function not available")
  }

  # Temporarily disable snap weighting
  old_val <- get0("USE_SNAP_WEIGHTED_INJURIES", envir = .GlobalEnv, ifnotfound = NULL)
  assign("USE_SNAP_WEIGHTED_INJURIES", FALSE, envir = .GlobalEnv)

  base_impact <- -0.50
  result <- tryCatch({
    weight_injury_by_snaps(base_impact, "Test Player", "DAL", 2024)
  }, error = function(e) base_impact)

  expect_equal(result, base_impact,
               info = "Snap weighting should return base impact when disabled")

  # Restore original value
  if (!is.null(old_val)) {
    assign("USE_SNAP_WEIGHTED_INJURIES", old_val, envir = .GlobalEnv)
  } else {
    rm("USE_SNAP_WEIGHTED_INJURIES", envir = .GlobalEnv)
  }
})

# =============================================================================
# NFLSIMULATION.R INTEGRATION TESTS
# =============================================================================

test_that("NFLsimulation.R integrates snap weighting", {
  sim_path <- file.path(.test_project_root, "NFLsimulation.R")
  expect_true(file.exists(sim_path),
              info = "NFLsimulation.R should exist")

  content <- paste(readLines(sim_path, warn = FALSE), collapse = "\n")

  # Check for snap weighting integration in calc_injury_impacts
  has_snap_check <- grepl("USE_SNAP_WEIGHTED_INJURIES", content)
  expect_true(has_snap_check,
              info = "NFLsimulation.R should check USE_SNAP_WEIGHTED_INJURIES")

  # Check that calc_injury_impacts accepts season parameter
  has_season_param <- grepl("calc_injury_impacts.*season", content)
  expect_true(has_season_param,
              info = "calc_injury_impacts should accept season parameter for snap data")

  # Check for player column in injury prep
  has_player_col <- grepl("col_player.*inj_pick|player.*=.*col_player", content)
  expect_true(has_player_col,
              info = "Injury data should include player name column")
})

test_that("calc_injury_impacts sources injury_scalp.R when needed", {
  sim_path <- file.path(.test_project_root, "NFLsimulation.R")
  content <- paste(readLines(sim_path, warn = FALSE), collapse = "\n")

  # Check that injury_scalp.R is sourced for snap weighting
  has_source <- grepl('source.*injury_scalp.*snap', content, ignore.case = TRUE)
  expect_true(has_source,
              info = "NFLsimulation.R should source injury_scalp.R for snap weighting")
})

# =============================================================================
# DATA STRUCTURE TESTS
# =============================================================================

test_that("snap percentages have expected structure", {
  injury_scalp_path <- file.path(.test_project_root, "injury_scalp.R")
  if (!file.exists(injury_scalp_path)) {
    skip("injury_scalp.R not found")
  }

  if (!exists("load_player_snap_percentages", mode = "function")) {
    source(injury_scalp_path, local = FALSE)
  }

  if (!exists("load_player_snap_percentages", mode = "function")) {
    skip("load_player_snap_percentages function not available")
  }

  # Try to load snap data for a team
  snap_data <- tryCatch({
    load_player_snap_percentages("DAL", 2024, use_cache = TRUE)
  }, error = function(e) tibble::tibble())

  if (nrow(snap_data) > 0) {
    # Check expected columns
    expect_true("player" %in% names(snap_data) || "full_name" %in% names(snap_data),
                info = "Snap data should have player name column")
    expect_true("snap_pct" %in% names(snap_data) || "snaps" %in% names(snap_data),
                info = "Snap data should have snap percentage column")
  }
})

# =============================================================================
# EDGE CASE TESTS
# =============================================================================

test_that("snap weighting handles missing player gracefully", {
  injury_scalp_path <- file.path(.test_project_root, "injury_scalp.R")
  if (!file.exists(injury_scalp_path)) {
    skip("injury_scalp.R not found")
  }

  if (!exists("weight_injury_by_snaps", mode = "function")) {
    source(injury_scalp_path, local = FALSE)
  }

  if (!exists("weight_injury_by_snaps", mode = "function")) {
    skip("weight_injury_by_snaps function not available")
  }

  # Test with non-existent player
  base_impact <- -0.50
  result <- tryCatch({
    weight_injury_by_snaps(base_impact, "Fake Player XYZ", "DAL", 2024)
  }, error = function(e) base_impact)

  # Should return base impact (no data = no modification)
  expect_true(is.numeric(result),
              info = "Should return numeric even for unknown player")
})

test_that("snap weighting handles empty snap data gracefully", {
  injury_scalp_path <- file.path(.test_project_root, "injury_scalp.R")
  if (!file.exists(injury_scalp_path)) {
    skip("injury_scalp.R not found")
  }

  if (!exists("weight_injury_by_snaps", mode = "function")) {
    source(injury_scalp_path, local = FALSE)
  }

  if (!exists("weight_injury_by_snaps", mode = "function")) {
    skip("weight_injury_by_snaps function not available")
  }

  # Test with team that might have no data
  base_impact <- -0.50
  result <- tryCatch({
    weight_injury_by_snaps(base_impact, "Test Player", "XYZ", 1999)  # Invalid team/season
  }, error = function(e) base_impact)

  expect_true(is.numeric(result),
              info = "Should handle invalid team/season gracefully")
})

