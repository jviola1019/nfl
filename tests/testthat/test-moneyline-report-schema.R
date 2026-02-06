# =============================================================================
# Moneyline report schema regression tests
# =============================================================================

context("Moneyline report schema contract")

tryCatch({
  source(file.path(.test_project_root, "NFLmarket.R"))
}, error = function(e) {
  # Tests will skip if NFLmarket.R fails to source in this environment
})

sample_moneyline_report_row <- function() {
  tibble::tibble(
    Season = 2024,
    Week = 12,
    Date = as.Date("2024-11-24"),
    Matchup = "DAL @ PHI",
    Winner = "PHI",
    `Blend Pick` = "PHI",
    `Blend Recommendation` = "Bet PHI moneyline",
    `Pass Reason` = "",
    `Blend Beat Market?` = "Yes",
    `Blend Beat Market Basis` = "realized_units",
    `Raw Kelly (%)` = 0.014,
    `Blend Stake (Units)` = 0.01,
    `EV Edge (%)` = 0.038,
    `Total EV (Units)` = 0.00038,
    `Edge Quality` = "✓ OK",
    `Blend Pick Win % (Shrunk)` = 0.57,
    `Market Pick Win % (Devig)` = 0.54,
    `Prob Edge on Pick (pp)` = 0.03,
    `Blend Home Win % (Shrunk)` = 0.57,
    `Market Home Win % (Fair, Devig=proportional)` = 0.54,
    `ML Implied Home % (Raw)` = 0.56,
    `Blend Median Margin` = 2.1,
    `Market Home Spread` = -1.5,
    `Blend Total` = 47.2,
    `Market Total` = 45.5,
    `Total O/U` = "OVER",
    `Market Home Moneyline` = -130,
    `Market Away Moneyline` = 110,
    `Blend Home Moneyline` = -138,
    `Blend Away Moneyline` = 118
  )
}

test_that("schema contract includes explicit probability column names", {
  skip_if_not(exists("moneyline_report_schema_contract", mode = "function"),
              "moneyline_report_schema_contract() not available")

  schema <- moneyline_report_schema_contract()
  expect_true(all(c(
    "ML Implied Home % (Raw)",
    "Market Home Win % (Fair, Devig=proportional)",
    "Blend Home Win % (Shrunk)"
  ) %in% schema$column))
})

test_that("validate_moneyline_report_schema passes on valid table", {
  skip_if_not(exists("validate_moneyline_report_schema", mode = "function"),
              "validate_moneyline_report_schema() not available")

  tbl <- sample_moneyline_report_row()
  expect_invisible(validate_moneyline_report_schema(tbl))
})

test_that("validate_moneyline_report_schema fails on missing or renamed columns", {
  skip_if_not(exists("validate_moneyline_report_schema", mode = "function"),
              "validate_moneyline_report_schema() not available")

  tbl <- sample_moneyline_report_row() %>%
    dplyr::rename(`ML Implied Home %` = `ML Implied Home % (Raw)`)

  expect_error(
    validate_moneyline_report_schema(tbl),
    "missing columns"
  )
})

test_that("validate_moneyline_report_schema fails on wrong type", {
  skip_if_not(exists("validate_moneyline_report_schema", mode = "function"),
              "validate_moneyline_report_schema() not available")

  tbl <- sample_moneyline_report_row() %>%
    dplyr::mutate(`Blend Stake (Units)` = as.character(`Blend Stake (Units)`))

  expect_error(
    validate_moneyline_report_schema(tbl),
    "expected type 'numeric'"
  )
})
