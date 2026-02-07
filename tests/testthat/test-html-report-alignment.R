# =============================================================================
# HTML report alignment tests
# =============================================================================

library(testthat)

if (!exists("export_moneyline_comparison_html", mode = "function")) {
  tryCatch({
    old_dir <- getwd()
    setwd(.test_project_root)
    on.exit(setwd(old_dir), add = TRUE)
    source(file.path(.test_project_root, "NFLmarket.R"))
  }, error = function(e) NULL)
}

test_that("moneyline HTML headers include governance columns and percent formatting", {
  skip_if_not(exists("export_moneyline_comparison_html", mode = "function"),
              "export_moneyline_comparison_html() not available")
  skip_if_not(requireNamespace("gt", quietly = TRUE), "gt not available")
  skip_if_not(requireNamespace("xml2", quietly = TRUE), "xml2 not available")

  comparison_tbl <- tibble::tibble(
    game_id = "2024_01_SEA_NE",
    season = 2024L,
    week = 1L,
    game_date = as.Date("2024-09-01"),
    home_team = "NE",
    away_team = "SEA",
    matchup = "SEA @ NE",
    actual_winner = "NE",
    blend_pick = "NE",
    blend_pick_type = "ev_positive",
    blend_recommendation = "Bet NE moneyline",
    blend_beats_market = TRUE,
    blend_beats_market_basis = "realized_units",
    blend_prob_pick_shrunk = 0.55,
    blend_prob_pick = 0.57,
    market_prob_pick = 0.52,
    blend_home_prob = 0.57,
    blend_home_prob_shrunk = 0.55,
    blend_away_prob_shrunk = 0.45,
    market_home_prob = 0.52,
    market_home_prob_fair = 0.52,
    market_home_prob_raw = 0.54,
    ml_implied_home_prob = 0.54,
    blend_pick_side = "home",
    blend_ev_units = 0.04,
    blend_favorite_side = "home",
    market_home_ml = -110,
    market_away_ml = 100,
    market_moneyline = -110,
    blend_median_margin = 2.1,
    market_home_spread = -1.5,
    blend_total_median = 47.2,
    market_total = 45.5,
    blend_home_ml = -122,
    blend_home_ml_vig = -110,
    blend_away_ml_vig = 105
  )

  tmp <- tempfile(fileext = ".html")
  export_moneyline_comparison_html(
    comparison_tbl,
    file = tmp,
    auto_open = FALSE,
    verbose = FALSE
  )

  html_doc <- xml2::read_html(tmp)
  table_node <- xml2::xml_find_first(html_doc, ".//table[@id='moneyline-table']")
  header_nodes <- xml2::xml_find_all(table_node, ".//th[contains(@class,'gt_col_heading')]")
  header_labels <- trimws(xml2::xml_text(header_nodes))

  expect_true(all(c(
    "Pass Reason",
    "EV Edge (Raw)",
    "EV Edge (Displayed, Capped)",
    "Min Stake (%)",
    "ML Implied Home % (Raw)"
  ) %in% header_labels))

  body_row <- xml2::xml_find_first(table_node, ".//tbody/tr")
  cells <- xml2::xml_find_all(body_row, "./td")
  cell_values <- xml2::xml_text(cells)

  idx <- function(name) match(name, header_labels)
  for (col in c(
    "EV Edge (Raw)",
    "EV Edge (Displayed, Capped)",
    "Raw Kelly (%)",
    "Capped Stake (%)",
    "Final Stake (%)",
    "Min Stake (%)"
  )) {
    pos <- idx(col)
    expect_true(is.finite(pos) && pos > 0, label = paste("missing column", col))
    expect_true(grepl("%", cell_values[[pos]]), label = paste("missing percent for", col))
  }
})
