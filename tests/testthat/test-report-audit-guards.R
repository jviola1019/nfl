# =============================================================================
# Report Audit Guardrails (schema + governance labels)
# =============================================================================

context("Report Audit Guardrails")

test_that("game report columns use explicit market probability semantics", {
  market_file <- file.path(.test_project_root, "NFLmarket.R")
  expect_true(file.exists(market_file))

  txt <- paste(readLines(market_file, warn = FALSE), collapse = "\n")

  expect_match(txt, "Market Home Win % \\(Fair, Devig=proportional\\)")
  expect_match(txt, "ML Implied Home % \\(Raw\\)")
})

test_that("pass reasons include required deterministic governance reasons", {
  market_file <- file.path(.test_project_root, "NFLmarket.R")
  utils_file <- file.path(.test_project_root, "R", "utils.R")

  txt <- paste(readLines(market_file, warn = FALSE), collapse = "\n")
  utils_txt <- paste(readLines(utils_file, warn = FALSE), collapse = "\n")
  combined <- paste(txt, utils_txt, collapse = "\n")

  expect_match(combined, "Market odds missing/placeholder")
  expect_match(combined, "Negative EV")
  expect_match(combined, "Stake below minimum")
})

test_that("props review status is not mislabeled as hard model failure only", {
  market_file <- file.path(.test_project_root, "NFLmarket.R")
  txt <- paste(readLines(market_file, warn = FALSE), collapse = "\n")

  expect_match(txt, "MODEL ERROR / REVIEW")
})
