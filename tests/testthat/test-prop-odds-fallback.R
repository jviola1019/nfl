# =============================================================================
# FILE: tests/testthat/test-prop-odds-fallback.R
# PURPOSE: Validate synthetic prop odds fallback logic
# =============================================================================

library(testthat)

# Source prop odds helpers
prop_odds_path <- file.path(.test_project_root, "R", "prop_odds_api.R")
if (file.exists(prop_odds_path)) {
  source(prop_odds_path, local = FALSE)
}

test_that("derive_prop_market_from_sim returns valid line and odds", {
  skip_if_not(exists("derive_prop_market_from_sim"), "derive_prop_market_from_sim not loaded")

  set.seed(2026)
  sims <- c(rnorm(500, 40, 8), rnorm(500, 60, 10))
  derived <- derive_prop_market_from_sim(sims, line_quantile = 0.40, vig = 0.045)

  expect_true(is.finite(derived$line))
  expect_true(is.finite(derived$over_odds))
  expect_true(is.finite(derived$under_odds))
  expect_false(isTRUE(derived$over_odds == derived$under_odds))
})

test_that("derive_two_way_odds_from_probs produces finite odds", {
  skip_if_not(exists("derive_two_way_odds_from_probs"), "derive_two_way_odds_from_probs not loaded")

  odds <- derive_two_way_odds_from_probs(0.60, 0.40, vig = 0.045)
  expect_true(is.finite(odds$over_odds))
  expect_true(is.finite(odds$under_odds))
})

test_that("load_prop_odds_csv parses required columns", {
  skip_if_not(exists("load_prop_odds_csv"), "load_prop_odds_csv not loaded")

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  sample <- data.frame(
    player = "Test Player",
    prop_type = "passing_yards",
    line = 250.5,
    over_odds = -110,
    under_odds = -110,
    book = "draftkings",
    stringsAsFactors = FALSE
  )
  utils::write.csv(sample, tmp, row.names = FALSE)

  parsed <- load_prop_odds_csv(tmp)
  expect_true(nrow(parsed) == 1)
  expect_equal(parsed$player[1], "Test Player")
  expect_true("source" %in% names(parsed))
  expect_equal(parsed$source[1], "csv")
})
