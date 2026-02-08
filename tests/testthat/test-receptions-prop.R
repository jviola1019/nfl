# =============================================================================
# FILE: tests/testthat/test-receptions-prop.R
# PURPOSE: Validate receptions prop model
# =============================================================================

library(testthat)

props_dir <- file.path(.test_project_root, "sports", "nfl", "props")
if (dir.exists(props_dir)) {
  source(file.path(props_dir, "props_config.R"), local = FALSE)
  source(file.path(props_dir, "receptions.R"), local = FALSE)
}

test_that("simulate_receptions returns non-negative counts", {
  skip_if_not(exists("simulate_receptions"), "simulate_receptions not loaded")

  set.seed(42)
  sim <- simulate_receptions(player_receptions = 5, position = "WR", n_trials = 5000)
  expect_true(length(sim$simulated_receptions) == 5000)
  expect_true(all(sim$simulated_receptions >= 0))
})

test_that("receptions_over_under produces valid probabilities and EV", {
  skip_if_not(exists("receptions_over_under"), "receptions_over_under not loaded")

  set.seed(42)
  sim <- simulate_receptions(player_receptions = 5, position = "WR", n_trials = 5000)
  ou <- receptions_over_under(sim, line = 4.5, over_odds = -115, under_odds = -105)

  expect_true(is.finite(ou$p_over))
  expect_true(is.finite(ou$p_under))
  expect_true(ou$p_over >= 0 && ou$p_over <= 1)
  expect_true(ou$p_under >= 0 && ou$p_under <= 1)
  expect_true(is.finite(ou$ev_over))
  expect_true(is.finite(ou$ev_under))
})
