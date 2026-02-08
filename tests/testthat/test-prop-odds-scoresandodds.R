# =============================================================================
# FILE: tests/testthat/test-prop-odds-scoresandodds.R
# PURPOSE: Validate ScoresAndOdds market-comparison parsing
# =============================================================================

library(testthat)

prop_odds_path <- file.path(.test_project_root, "R", "prop_odds_api.R")
if (file.exists(prop_odds_path)) {
  source(prop_odds_path, local = FALSE)
}

test_that("parse_scoresandodds_market_json extracts book rows", {
  skip_if_not(exists("parse_scoresandodds_market_json"), "ScoresAndOdds parser not loaded")

  payload <- list(
    event = list(
      id = "nfl/11683820",
      home = list(key = "NE"),
      away = list(key = "SEA")
    ),
    markets = list(
      list(
        player = list(
          first_name = "Sam",
          last_name = "Darnold",
          team = list(key = "SEA")
        ),
        comparison = list(
          draftkings = list(value = 230.5, over = -113, under = -111, available = TRUE),
          fanduel = list(value = 229.5, over = -110, under = -110, available = TRUE)
        )
      )
    )
  )

  rows <- parse_scoresandodds_market_json(payload, "passing_yards")
  expect_true(nrow(rows) == 2)
  expect_true(all(c("player", "prop_type", "line", "over_odds", "under_odds", "book") %in% names(rows)))
  expect_true(any(rows$book == "draftkings"))
  expect_true(any(rows$book == "fanduel"))
})

test_that("get_market_prop_line honors preferred books", {
  skip_if_not(exists("get_market_prop_line"), "get_market_prop_line not loaded")

  props <- tibble::tibble(
    player = c("Sam Darnold", "Sam Darnold"),
    prop_type = c("passing_yards", "passing_yards"),
    line = c(230.5, 229.5),
    line_over = c(230.5, 229.5),
    line_under = c(230.5, 229.5),
    over_odds = c(-113, -110),
    under_odds = c(-111, -110),
    book = c("draftkings", "fanduel")
  )

  dk_line <- get_market_prop_line("Sam Darnold", "passing_yards", props, preferred_books = "draftkings")
  expect_equal(dk_line$book, "draftkings")
  expect_equal(dk_line$line, 230.5)
})
