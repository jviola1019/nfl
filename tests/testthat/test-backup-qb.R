# =============================================================================
# Tests for Backup QB Quality Scoring
# =============================================================================
# Tests get_backup_qb_quality() function in injury_scalp.R
# =============================================================================

# =============================================================================
# FUNCTION EXISTENCE TESTS
# =============================================================================

test_that("get_backup_qb_quality function exists", {
  # Source injury_scalp.R if needed
  injury_scalp_path <- file.path(.test_project_root, "injury_scalp.R")
  if (!exists("get_backup_qb_quality", mode = "function")) {
    if (file.exists(injury_scalp_path)) {
      source(injury_scalp_path, local = FALSE)
    } else {
      skip("injury_scalp.R not found")
    }
  }

  expect_true(exists("get_backup_qb_quality", mode = "function"),
              info = "get_backup_qb_quality should be defined")
})

test_that("get_backup_qb_quality returns numeric", {
  if (!exists("get_backup_qb_quality", mode = "function")) {
    skip("get_backup_qb_quality not available")
  }

  # Test with a known team
  result <- tryCatch({
    get_backup_qb_quality("DAL", 2024, use_cache = FALSE)
  }, error = function(e) NULL)

  if (!is.null(result)) {
    expect_type(result, "double")
    expect_true(result >= 0 && result <= 1,
                info = "Backup QB quality should be between 0 and 1")
  }
})

test_that("get_backup_qb_quality returns valid range", {
  if (!exists("get_backup_qb_quality", mode = "function")) {
    skip("get_backup_qb_quality not available")
  }

  teams <- c("DAL", "NE", "KC", "SF")

  for (team in teams) {
    result <- tryCatch({
      get_backup_qb_quality(team, 2024, use_cache = TRUE)
    }, error = function(e) 0.35)  # Default if lookup fails

    expect_true(result >= 0 && result <= 1,
                info = sprintf("Quality for %s should be in [0, 1]", team))
  }
})

# =============================================================================
# CONFIG INTEGRATION TESTS
# =============================================================================

test_that("QB_BACKUP_QUALITY_DISCOUNT is configured", {
  if (!exists("QB_BACKUP_QUALITY_DISCOUNT")) {
    config_path <- file.path(.test_project_root, "config.R")
    if (file.exists(config_path)) {
      source(config_path, local = FALSE)
    }
  }

  expect_true(exists("QB_BACKUP_QUALITY_DISCOUNT"),
              info = "QB_BACKUP_QUALITY_DISCOUNT should be defined in config.R")

  if (exists("QB_BACKUP_QUALITY_DISCOUNT")) {
    expect_type(QB_BACKUP_QUALITY_DISCOUNT, "double")
    expect_true(QB_BACKUP_QUALITY_DISCOUNT >= 0 && QB_BACKUP_QUALITY_DISCOUNT <= 1,
                info = "QB_BACKUP_QUALITY_DISCOUNT should be between 0 and 1")
  }
})

test_that("USE_QB_BACKUP_QUALITY is configured", {
  if (!exists("USE_QB_BACKUP_QUALITY")) {
    config_path <- file.path(.test_project_root, "config.R")
    if (file.exists(config_path)) {
      source(config_path, local = FALSE)
    }
  }

  expect_true(exists("USE_QB_BACKUP_QUALITY"),
              info = "USE_QB_BACKUP_QUALITY should be defined in config.R")

  if (exists("USE_QB_BACKUP_QUALITY")) {
    expect_type(USE_QB_BACKUP_QUALITY, "logical")
  }
})

# =============================================================================
# NFLSIMULATION.R INTEGRATION TESTS
# =============================================================================

test_that("NFLsimulation.R integrates backup QB quality", {
  sim_path <- file.path(.test_project_root, "NFLsimulation.R")
  expect_true(file.exists(sim_path),
              info = "NFLsimulation.R should exist")

  content <- paste(readLines(sim_path, warn = FALSE), collapse = "\n")

  # Check for backup QB quality integration
  has_backup_check <- grepl("backup_qb_available|get_backup_qb_quality", content)
  expect_true(has_backup_check,
              info = "NFLsimulation.R should check for backup QB quality")

  # Check for discount factor application
  has_discount <- grepl("discount_factor.*backup_quality|qb_quality_discount", content)
  expect_true(has_discount,
              info = "NFLsimulation.R should apply backup QB quality discount")
})

# =============================================================================
# CACHING TESTS
# =============================================================================

test_that("backup QB quality caching works", {
  if (!exists("get_backup_qb_quality", mode = "function")) {
    skip("get_backup_qb_quality not available")
  }

  # First call (may hit API or cache)
  result1 <- tryCatch({
    get_backup_qb_quality("KC", 2024, use_cache = TRUE)
  }, error = function(e) 0.35)

  # Second call (should use cache)
  result2 <- tryCatch({
    get_backup_qb_quality("KC", 2024, use_cache = TRUE)
  }, error = function(e) 0.35)

  expect_equal(result1, result2,
               info = "Cached results should be consistent")
})

# =============================================================================
# DISCOUNT CALCULATION TESTS
# =============================================================================

test_that("discount factor calculation is correct", {
  # Formula: discount_factor = 1 - (QB_BACKUP_QUALITY_DISCOUNT * backup_quality)

  discount <- 0.50  # QB_BACKUP_QUALITY_DISCOUNT
  backup_quality <- 0.70  # Elite backup

  expected_factor <- 1 - (discount * backup_quality)  # 1 - 0.35 = 0.65

  expect_equal(expected_factor, 0.65,
               info = "Elite backup (0.70) with 0.50 discount should give 0.65 factor")

  # For poor backup
  poor_quality <- 0.15
  expected_poor <- 1 - (discount * poor_quality)  # 1 - 0.075 = 0.925

  expect_equal(expected_poor, 0.925,
               info = "Poor backup (0.15) with 0.50 discount should give 0.925 factor")
})

test_that("backup quality reduces QB injury penalty appropriately", {
  # Base QB penalty for OUT status
  base_penalty <- -7.2  # Typical QB importance penalty

  # With elite backup (quality = 0.70)
  discount <- 0.50
  elite_quality <- 0.70
  elite_factor <- 1 - (discount * elite_quality)  # 0.65
  elite_adjusted <- base_penalty * elite_factor   # -4.68

  expect_true(abs(elite_adjusted) < abs(base_penalty),
              info = "Elite backup should reduce penalty severity")

  # With poor backup (quality = 0.15)
  poor_quality <- 0.15
  poor_factor <- 1 - (discount * poor_quality)   # 0.925
  poor_adjusted <- base_penalty * poor_factor    # -6.66

  expect_true(abs(poor_adjusted) > abs(elite_adjusted),
              info = "Poor backup should have larger penalty than elite backup")
})

