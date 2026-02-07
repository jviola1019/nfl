source('NFLmarket.R')
if (!requireNamespace('xml2', quietly = TRUE)) stop('xml2 not installed')

comparison_tbl <- tibble::tibble(
  game_id = '2024_01_SEA_NE',
  season = 2024L,
  week = 1L,
  game_date = as.Date('2024-09-01'),
  home_team = 'NE',
  away_team = 'SEA',
  matchup = 'SEA @ NE',
  actual_winner = 'NE',
  blend_pick = 'NE',
  blend_pick_type = 'ev_positive',
  blend_recommendation = 'Bet NE moneyline',
  blend_beats_market = TRUE,
  blend_beats_market_basis = 'realized_units',
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
  blend_pick_side = 'home',
  blend_ev_units = 0.04,
  blend_favorite_side = 'home',
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

out_file <- tempfile(fileext = '.html')
export_moneyline_comparison_html(comparison_tbl, file = out_file, auto_open = FALSE, verbose = FALSE)

html_doc <- xml2::read_html(out_file)
mtable <- xml2::xml_find_first(html_doc, ".//table[@id='moneyline-table']")
headers <- trimws(xml2::xml_text(xml2::xml_find_all(mtable, './/th[contains(@class,"gt_col_heading")]')))
row <- xml2::xml_find_first(mtable, './/tbody/tr')
values <- xml2::xml_text(xml2::xml_find_all(row, './td'))

cat('headers:', length(headers), 'values:', length(values), '\n')
