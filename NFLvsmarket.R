# ──────────────────────────────────────────────────────────────────────────────
# NFL_verify_vs_market.R
# Standalone verification & best-bet helper for your simulation.
# - Learns a spread→probability map when MLs are missing
# - Compares Model (and Blend, if present) vs Market with week-block bootstrap CIs
# - Builds a readable Best Bets table for the current slate
#
# EXPECTED INPUTS (preferred, but script can auto-source fallbacks):
#   sched  : schedules/odds data.frame (from nflreadr::load_schedules)
#   res    : list with $per_game tibble (has p2_cal; optional p_blend)
#   final  : tibble of current-slate outputs (has home_p_2w_cal, tie_prob, matchup, date)
#
# Optional convenience: if 'res' is not in memory, will try to load newest
# RDS from ~/.cache/nfl_sim_scores/. If 'sched' missing, will load schedules.
# If 'final' missing, you can point FINAL_RDS to a saved object.
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(nflreadr)
  library(scales)
  library(glue)
  library(htmltools)
})

gt_available <- tryCatch(requireNamespace("gt", quietly = TRUE), error = function(e) FALSE)
reactable_available <- tryCatch(requireNamespace("reactable", quietly = TRUE), error = function(e) FALSE)
htmlwidgets_available <- tryCatch(requireNamespace("htmlwidgets", quietly = TRUE), error = function(e) FALSE)

if (!gt_available) {
  message("Package 'gt' is not available (or requires a newer 'xfun'); falling back to plain data-frame output for tables.")
}
if (!reactable_available) {
  message("Package 'reactable' is not available; skipping interactive odds table.")
}
if (!htmlwidgets_available) {
  message("Package 'htmlwidgets' is not available; HTML report will be saved without widget dependencies.")
}

# -------------------------- Config knobs --------------------------------------
# Set this if you saved 'final' to disk; leave NULL to skip:
FINAL_RDS <- NULL        # e.g., FINAL_RDS <- "final_latest.rds"
N_BOOT    <- 2000        # bootstrap resamples for week-block CIs
FOCUS_MATCHUP <- NULL    # e.g., "SF @ KC" to print only one game in Best Bets

# -------------------------- Helpers -------------------------------------------
.clp <- function(x, eps=1e-12) pmin(pmax(x, eps), 1-eps)
.lgt <- function(p) log(p/(1-p))
.inv <- function(z) 1/(1+exp(-z))

american_to_prob <- function(odds) {
  odds <- suppressWarnings(as.numeric(odds))
  ifelse(odds < 0, (-odds)/((-odds)+100), 100/(odds+100))
}
prob_to_american <- function(p) {
  p <- .clp(p)
  ifelse(p >= 0.5, -round(100 * p/(1-p)), round(100 * (1-p)/p))
}

american_to_decimal <- function(odds) {
  odds <- suppressWarnings(as.numeric(odds))
  ifelse(odds < 0, 1 + 100/abs(odds), 1 + odds/100)
}

expected_value_units <- function(prob, odds) {
  dec <- american_to_decimal(odds)
  prob <- .clp(prob)
  ifelse(is.finite(dec), prob * (dec - 1) - (1 - prob), NA_real_)
}

format_line <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(abs(x - round(x)) < 1e-6,
           sprintf("%+d", as.integer(round(x))),
           sprintf("%+.1f", x))
  )
}

cover_probability_norm <- function(mean_margin, sd_margin, spread, side = c("home","away")) {
  side <- match.arg(side)
  if (!is.finite(mean_margin) || !is.finite(sd_margin) || sd_margin <= 0 || !is.finite(spread)) {
    return(NA_real_)
  }
  if (side == "home") {
    stats::pnorm(-spread, mean = mean_margin, sd = sd_margin, lower.tail = FALSE)
  } else {
    stats::pnorm(spread, mean = mean_margin, sd = sd_margin, lower.tail = TRUE)
  }
}

pick_col <- function(df, cands) {
  nm <- intersect(cands, names(df))
  if (length(nm)) nm[1] else NA_character_
}

safe_load_lines <- function(seasons) {
  tryCatch(nflreadr::load_lines(seasons = seasons), error = function(e) NULL)
}

closing_spreads_tbl <- function(sched_df) {
  sp_col <- pick_col(sched_df, c("close_spread","spread_close","home_spread_close","spread_line","spread","home_spread"))
  if (is.na(sp_col)) return(tibble(game_id = character(), home_main_spread = numeric()))
  sched_df %>%
    filter(game_type %in% c("REG","Regular")) %>%
    transmute(game_id, home_main_spread = suppressWarnings(as.numeric(.data[[sp_col]])))
}

standardize_side <- function(side_raw, home_team, away_team) {
  if (is.na(side_raw)) return(NA_character_)
  sr <- toupper(trimws(as.character(side_raw)))
  home <- toupper(home_team)
  away <- toupper(away_team)

  if (sr %in% c("HOME","H","HOST")) return("home")
  if (sr %in% c("AWAY","VISITOR","ROAD","A")) return("away")
  if (nzchar(home) && grepl(home, sr, fixed = TRUE)) return("home")
  if (nzchar(away) && grepl(away, sr, fixed = TRUE)) return("away")
  if (nzchar(home) && startsWith(sr, substr(home, 1, 3))) return("home")
  if (nzchar(away) && startsWith(sr, substr(away, 1, 3))) return("away")
  NA_character_
}

build_line_catalog <- function(final_df, lines_df, sched_df) {
  if (is.null(lines_df) || !nrow(lines_df)) return(tibble())

  bt_col  <- pick_col(lines_df, c("bet_type","market_type","type","wager_type"))
  side_col<- pick_col(lines_df, c("side","team","participant","selection"))
  odds_col<- pick_col(lines_df, c("american_odds","odds_american","price","line_price","odds"))
  line_col<- pick_col(lines_df, c("line","spread_line","handicap","points","spread"))
  book_col<- pick_col(lines_df, c("provider","book","bookmaker","sportsbook"))
  alt_col <- pick_col(lines_df, c("is_alternate","alternate","is_alt","alt_line"))

  if (is.na(bt_col) || is.na(side_col) || is.na(odds_col)) return(tibble())

  base <- final_df %>%
    transmute(game_id, date = as.Date(date), matchup, home_team, away_team,
              home_prob = dplyr::coalesce(home_p_2w_blend, home_p_2w_model, home_p_2w_cal, home_win_prob_cal),
              away_prob = 1 - home_prob,
              margin_mean, margin_sd) %>%
    distinct()

  if (!"game_id" %in% names(lines_df)) return(tibble())

  lines_df %>%
    filter(game_id %in% base$game_id) %>%
    transmute(
      game_id,
      bet_type = tolower(as.character(.data[[bt_col]])),
      side_raw = .data[[side_col]],
      odds = suppressWarnings(as.numeric(.data[[odds_col]])),
      line = if (!is.na(line_col)) suppressWarnings(as.numeric(.data[[line_col]])) else NA_real_,
      book = if (!is.na(book_col)) as.character(.data[[book_col]]) else NA_character_,
      is_alt = if (!is.na(alt_col)) as.logical(.data[[alt_col]]) else NA
    ) %>%
    left_join(base, by = "game_id") %>%
    mutate(
      side = standardize_side(side_raw, home_team, away_team),
      is_alt = ifelse(is.na(is_alt), FALSE, is_alt)
    ) %>%
    filter(is.finite(odds), !is.na(side)) %>%
    left_join(closing_spreads_tbl(sched_df), by = "game_id") %>%
    mutate(
      main_spread_side = case_when(
        !is.na(home_main_spread) & side == "home" ~ home_main_spread,
        !is.na(home_main_spread) & side == "away" ~ -home_main_spread,
        TRUE ~ NA_real_
      ),
      is_alt = ifelse(!is_alt & !is.na(main_spread_side) & !is.na(line), abs(line - main_spread_side) > 0.05, is_alt),
      bet_bucket = case_when(
        grepl("moneyline", bet_type) ~ "Moneyline",
        grepl("spread", bet_type) & is_alt ~ "Alt Spread",
        grepl("spread", bet_type) ~ "Spread",
        TRUE ~ stringr::str_to_title(bet_type)
      ),
      team_label = ifelse(side == "home", home_team, away_team),
      model_prob = case_when(
        bet_bucket == "Moneyline" & side == "home" ~ home_prob,
        bet_bucket == "Moneyline" & side == "away" ~ away_prob,
        bet_bucket %in% c("Spread","Alt Spread") & side == "home" ~ cover_probability_norm(margin_mean, margin_sd, line, "home"),
        bet_bucket %in% c("Spread","Alt Spread") & side == "away" ~ cover_probability_norm(margin_mean, margin_sd, line, "away"),
        TRUE ~ NA_real_
      ),
      market_prob = .clp(american_to_prob(odds)),
      edge = model_prob - market_prob,
      ev_units = expected_value_units(model_prob, odds),
      line_display = dplyr::case_when(
        bet_bucket == "Moneyline" ~ "ML",
        bet_bucket %in% c("Spread","Alt Spread") ~ format_line(line),
        TRUE ~ as.character(line)
      ),
      selection = as.character(glue("{team_label} {line_display}")),
      odds_fmt = ifelse(is.finite(odds), sprintf("%+d", as.integer(round(odds))), NA_character_)
    ) %>%
    filter(is.finite(model_prob))
}

best_offer_rows <- function(catalog) {
  if (!nrow(catalog)) return(catalog)

  catalog %>%
    group_by(game_id, bet_bucket, side, line_display) %>%
    slice_max(order_by = ev_units, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(game_id, bet_bucket) %>%
    slice_max(order_by = ev_units, n = 6, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(date, matchup, bet_bucket, desc(ev_units))
}

# ------------------ Load/locate inputs if not present -------------------------
if (!exists("sched")) {
  message("Loading schedules via nflreadr (last 10 seasons)…")
  seasons <- (lubridate::year(Sys.Date()) - 9):(lubridate::year(Sys.Date()))
  sched <- nflreadr::load_schedules(seasons = seasons)
}

if (!exists("res")) {
  cache_dir <- file.path(path.expand("~"), ".cache", "nfl_sim_scores")
  if (dir.exists(cache_dir)) {
    files <- list.files(cache_dir, pattern="\\.rds$", full.names = TRUE)
    if (length(files)) {
      newest <- files[order(file.info(files)$mtime, decreasing = TRUE)][1]
      message(sprintf("Loading latest res cache: %s", newest))
      res <- readRDS(newest)
    }
  }
}

if (!exists("final") && !is.null(FINAL_RDS) && file.exists(FINAL_RDS)) {
  message(sprintf("Loading final from: %s", FINAL_RDS))
  final <- readRDS(FINAL_RDS)
}

# ------------------ Outcome extraction (2-way home win) -----------------------
home_pts_col <- pick_col(sched, c("home_score","home_points","score_home","home_pts"))
away_pts_col <- pick_col(sched, c("away_score","away_points","score_away","away_pts"))
stopifnot(!is.na(home_pts_col), !is.na(away_pts_col))

outcomes <- sched %>%
  filter(game_type %in% c("REG","Regular")) %>%
  transmute(
    game_id, season, week,
    y2 = as.integer(.data[[home_pts_col]] > .data[[away_pts_col]])
  )

# ------------------ Learn spread -> 2-way home-win probability ----------------
learn_spread_map <- function(sched_df) {
  sp_col <- pick_col(sched_df, c("close_spread","spread_close","home_spread_close",
                                 "spread_line","spread","home_spread"))
  ml_h   <- pick_col(sched_df, c("home_ml_close","ml_home_close","moneyline_home_close",
                                 "home_moneyline_close","home_ml","ml_home","moneyline_home"))
  ml_a   <- pick_col(sched_df, c("away_ml_close","ml_away_close","moneyline_away_close",
                                 "away_moneyline_close","away_ml","ml_away","moneyline_away"))
  if (is.na(sp_col)) return(NULL)
  
  df <- sched_df %>%
    filter(game_type %in% c("REG","Regular")) %>%
    transmute(
      spread = suppressWarnings(as.numeric(.data[[sp_col]])),
      ph = if (!is.na(ml_h) && !is.na(ml_a))
        american_to_prob(.data[[ml_h]]) else NA_real_,
      pa = if (!is.na(ml_h) && !is.na(ml_a))
        american_to_prob(.data[[ml_a]]) else NA_real_
    ) %>%
    mutate(p_home = ifelse(is.finite(ph+pa) & (ph+pa) > 0, ph/(ph+pa), NA_real_)) %>%
    filter(is.finite(spread), is.finite(p_home))
  
  if (nrow(df) < 400) {
    warning("Not enough ML+spread history to learn map; will fall back to Normal SD.")
    return(NULL)
  }
  
  # Use flexible logistic polynomial (3rd degree) to map spread -> p_home_2w
  fit <- glm(p_home ~ poly(spread, 3, raw = TRUE),
             data = df, family = binomial(), weights = pmax(1, round(1000*abs(p_home-0.5))))
  list(
    predict = function(sp) {
      sp <- as.numeric(sp)
      p <- suppressWarnings(predict(fit, newdata = data.frame(spread = sp), type = "response"))
      .clp(as.numeric(p))
    },
    model = fit
  )
}

# ------------------ Market probs from schedule (ML first, else spread map) ----
market_probs_from_sched <- function(sched_df, spread_mapper = NULL) {
  sp_col <- pick_col(sched_df, c("close_spread","spread_close","home_spread_close",
                                 "spread_line","spread","home_spread"))
  ml_h   <- pick_col(sched_df, c("home_ml_close","ml_home_close","moneyline_home_close",
                                 "home_moneyline_close","home_ml","ml_home","moneyline_home"))
  ml_a   <- pick_col(sched_df, c("away_ml_close","ml_away_close","moneyline_away_close",
                                 "away_moneyline_close","away_ml","ml_away","moneyline_away"))
  
  base <- sched_df %>%
    filter(game_type %in% c("REG","Regular")) %>%
    transmute(game_id, season, week)
  
  # 1) Moneyline (preferred)
  if (!is.na(ml_h) && !is.na(ml_a)) {
    out <- sched_df %>%
      transmute(
        game_id, season, week,
        ph = american_to_prob(.data[[ml_h]]),
        pa = american_to_prob(.data[[ml_a]])
      ) %>%
      filter(is.finite(ph), is.finite(pa)) %>%
      mutate(den = ph + pa,
             p_home_mkt_2w = .clp(ifelse(is.finite(den) & den > 0, ph/den, NA_real_))) %>%
      select(game_id, season, week, p_home_mkt_2w)
    if (sum(is.finite(out$p_home_mkt_2w)) > 0) return(out)
  }
  
  # 2) Spread → probability mapping
  if (!is.na(sp_col)) {
    if (is.null(spread_mapper)) spread_mapper <- learn_spread_map(sched_df)
    
    if (!is.null(spread_mapper)) {
      out <- sched_df %>%
        transmute(
          game_id, season, week,
          home_spread = suppressWarnings(as.numeric(.data[[sp_col]]))
        ) %>%
        mutate(p_home_mkt_2w = spread_mapper$predict(home_spread)) %>%
        select(game_id, season, week, p_home_mkt_2w)
      return(out)
    } else {
      # Fallback: Normal with fixed SD (industry heuristic). Keep as last resort.
      SD_MARGIN <- 13.86
      out <- sched_df %>%
        transmute(
          game_id, season, week,
          home_spread = suppressWarnings(as.numeric(.data[[sp_col]]))
        ) %>%
        filter(is.finite(home_spread)) %>%
        mutate(p_home_mkt_2w = .clp(pnorm(-home_spread / SD_MARGIN))) %>%
        select(game_id, season, week, p_home_mkt_2w)
      return(out)
    }
  }
  
  stop("No usable moneyline or spread columns found to derive market probabilities.")
}

# ------------------ Metrics + week-block bootstrap ----------------------------
brier <- function(p,y) mean((.clp(p)-y)^2, na.rm = TRUE)
logloss <- function(p,y) { p <- .clp(p); -mean(y*log(p) + (1-y)*log(1-p), na.rm = TRUE) }

score_metrics <- function(p, y) {
  keep <- is.finite(p) & is.finite(y)
  if (!any(keep)) {
    return(list(brier = NA_real_, logloss = NA_real_))
  }
  p_use <- .clp(p[keep])
  y_use <- y[keep]
  list(
    brier = mean((p_use - y_use)^2),
    logloss = -mean(y_use * log(p_use) + (1 - y_use) * log(1 - p_use))
  )
}

bootstrap_week_ci <- function(df, p_col_model, p_col_mkt, y_col = "y2",
                              n_boot = 2000, seed = 42) {
  set.seed(seed)
  weeks <- df %>% distinct(season, week)
  Brier_d <- numeric(n_boot)
  LogL_d  <- numeric(n_boot)
  
  for (b in seq_len(n_boot)) {
    samp <- weeks[sample(nrow(weeks), replace = TRUE), , drop = FALSE]
    tmp  <- df %>% inner_join(samp, by = c("season","week"), relationship = "many-to-many")
    Brier_d[b] <- brier(tmp[[p_col_model]], tmp[[y_col]]) - brier(tmp[[p_col_mkt]], tmp[[y_col]])
    LogL_d[b]  <- logloss(tmp[[p_col_model]], tmp[[y_col]]) - logloss(tmp[[p_col_mkt]], tmp[[y_col]])
  }
  
  ci <- function(v) quantile(v, c(0.025, 0.975), na.rm = TRUE)
  tibble(
    metric = c("Brier (Model - Market)","LogLoss (Model - Market)"),
    delta  = c(mean(Brier_d), mean(LogL_d)),
    lo     = c(ci(Brier_d)[1], ci(LogL_d)[1]),
    hi     = c(ci(Brier_d)[2], ci(LogL_d)[2]),
    verdict = ifelse(hi < 0, "WIN",
                     ifelse(lo > 0, "LOSE", "TIE"))
  )
}

# ------------------ Assemble evaluation dataset -------------------------------
stopifnot("per_game" %in% names(res))

market_prob_col <- pick_col(res$per_game, c("p_home_mkt_2w","p_mkt","market_prob_home","p_mkt_2w","home_p_mkt","p2_market","market_p_home"))

eval_df <- res$per_game %>%
  # keep just what we need
  transmute(game_id, season, week,
            p_model = .clp(p2_cal),
            p_blend = if ("p_blend" %in% names(res$per_game)) .clp(p_blend) else NA_real_,
            p_mkt_res = if (!is.na(market_prob_col)) .clp(.data[[market_prob_col]]) else NA_real_) %>%
  inner_join(outcomes, by = c("game_id","season","week"))

# Market probs (closing ML preferred; else spread mapping)
spread_mapper <- learn_spread_map(sched)  # may return NULL -> script falls back gracefully
mkt_df <- market_probs_from_sched(sched, spread_mapper = spread_mapper)

comp <- eval_df %>%
  left_join(mkt_df, by = c("game_id","season","week")) %>%
  mutate(
    p_mkt = dplyr::coalesce(
      if ("p_home_mkt_2w" %in% names(.)) .clp(p_home_mkt_2w) else NA_real_,
      .clp(p_mkt_res)
    )
  ) %>%
  filter(is.finite(p_mkt))

model_candidates <- tibble::tibble(
  column = c("p_model", "p_blend"),
  label = c("Model", "Blend")
) %>%
  dplyr::filter(.data$column %in% names(comp)) %>%
  dplyr::mutate(has_data = purrr::map_lgl(.data$column, ~ any(is.finite(comp[[.x]])))) %>%
  dplyr::filter(.data$has_data)

best_prob_choice <- NULL
if (nrow(model_candidates)) {
  model_scores <- model_candidates %>%
    dplyr::mutate(metrics = purrr::map(.data$column, ~ score_metrics(comp[[.x]], comp$y2))) %>%
    tidyr::unnest_wider(metrics)

  best_prob_choice <- model_scores %>%
    dplyr::arrange(.data$brier, .data$logloss) %>%
    dplyr::slice(1)

  message(sprintf(
    "Best-bet probabilities sourced from %s column (Brier=%.6f, LogLoss=%.6f).",
    best_prob_choice$label, best_prob_choice$brier, best_prob_choice$logloss
  ))
}

preferred_final_prob_col <- NULL
if (!is.null(best_prob_choice)) {
  preferred_final_prob_col <- dplyr::case_when(
    best_prob_choice$column == "p_blend" ~ "home_p_2w_blend",
    best_prob_choice$column == "p_model" ~ "home_p_2w_cal",
    TRUE ~ NA_character_
  )
  if (is.na(preferred_final_prob_col)) {
    preferred_final_prob_col <- NULL
  }
}

# ------------------ Print headline table (Model vs Market) --------------------
overall_tbl <- comp %>%
  group_by(season, week) %>%
  summarise(
    n_games = n(),
    Brier_model = brier(p_model, y2),
    Brier_mkt   = brier(p_mkt,   y2),
    LogL_model  = logloss(p_model, y2),
    LogL_mkt    = logloss(p_mkt,   y2),
    .groups = "drop"
  ) %>%
  summarise(
    n_weeks = n(),
    Brier_model = stats::weighted.mean(Brier_model, n_games, na.rm = TRUE),
    Brier_mkt   = stats::weighted.mean(Brier_mkt,   n_games, na.rm = TRUE),
    LogL_model  = stats::weighted.mean(LogL_model,  n_games, na.rm = TRUE),
    LogL_mkt    = stats::weighted.mean(LogL_mkt,    n_games, na.rm = TRUE),
    total_games = sum(n_games),
    .groups = "drop"
  ) %>%
  mutate(
    n_games = total_games,
    Brier_delta = Brier_model - Brier_mkt,
    LogL_delta  = LogL_model  - LogL_mkt
  ) %>%
  select(-total_games)

message("\n=== Overall (Model vs Market) ===")
print(overall_tbl)

ci_tbl <- bootstrap_week_ci(comp, p_col_model = "p_model", p_col_mkt = "p_mkt", n_boot = N_BOOT)
message("\n=== Week-block bootstrap CI (Model – Market) ===")
print(ci_tbl)

overall_gt <- NULL
ci_gt <- NULL
overall_blend <- NULL
ci_tbl_blend <- NULL
overall_blend_gt <- NULL
ci_blend_gt <- NULL

if (gt_available) {
  overall_gt <- overall_tbl %>%
    gt::gt() %>%
    gt::fmt_number(columns = c("Brier_model", "Brier_mkt", "Brier_delta", "LogL_model", "LogL_mkt", "LogL_delta"), decimals = 6) %>%
    gt::fmt_number(columns = "n_games", decimals = 0)

  ci_gt <- ci_tbl %>%
    gt::gt() %>%
    gt::fmt_number(columns = c("delta", "lo", "hi"), decimals = 6)
}

if ("p_blend" %in% names(comp) && any(is.finite(comp$p_blend))) {
  overall_blend <- comp %>%
    group_by(season, week) %>%
    summarise(
      n_games = n(),
      Brier_blend = brier(p_blend, y2),
      Brier_mkt   = brier(p_mkt,   y2),
      LogL_blend  = logloss(p_blend, y2),
      LogL_mkt    = logloss(p_mkt,   y2),
      .groups = "drop"
    ) %>%
    summarise(
      n_weeks = n(),
      Brier_blend = stats::weighted.mean(Brier_blend, n_games, na.rm = TRUE),
      Brier_mkt   = stats::weighted.mean(Brier_mkt,   n_games, na.rm = TRUE),
      LogL_blend  = stats::weighted.mean(LogL_blend,  n_games, na.rm = TRUE),
      LogL_mkt    = stats::weighted.mean(LogL_mkt,    n_games, na.rm = TRUE),
      total_games = sum(n_games),
      .groups = "drop"
    ) %>%
    mutate(
      n_games = total_games,
      Brier_delta = Brier_blend - Brier_mkt,
      LogL_delta  = LogL_blend  - LogL_mkt
    ) %>%
    select(-total_games)
  message("\n=== Overall (Blend vs Market) ===")
  print(overall_blend)
  
  ci_tbl_blend <- bootstrap_week_ci(comp, p_col_model = "p_blend", p_col_mkt = "p_mkt", n_boot = N_BOOT)
  message("\n=== Week-block bootstrap CI (Blend – Market) ===")
  print(ci_tbl_blend)

  if (gt_available) {
    overall_blend_gt <- overall_blend %>%
      gt::gt() %>%
      gt::fmt_number(columns = c("Brier_blend", "Brier_mkt", "Brier_delta", "LogL_blend", "LogL_mkt", "LogL_delta"), decimals = 6) %>%
      gt::fmt_number(columns = "n_games", decimals = 0)

    ci_blend_gt <- ci_tbl_blend %>%
      gt::gt() %>%
      gt::fmt_number(columns = c("delta", "lo", "hi"), decimals = 6)
  }
}

# ------------------ Best Bets table for current slate -------------------------
build_best_bets <- function(final_df, sched_df, spread_mapper = NULL, focus_matchup = NULL, line_catalog = NULL,
                            preferred_prob_col = NULL) {
  stopifnot(all(c("matchup", "date") %in% names(final_df)))

  # Reconstruct calibrated two-way probabilities if needed
  if (!("home_p_2w_cal" %in% names(final_df)) &&
      all(c("home_win_prob_cal", "away_win_prob_cal", "tie_prob") %in% names(final_df))) {
    final_df <- final_df %>%
      mutate(two_way_mass = pmax(1 - tie_prob, 1e-9),
             home_p_2w_cal = .clp(home_win_prob_cal / two_way_mass))
  }

  if (!("home_p_2w_blend" %in% names(final_df)) && "home_p_2w_cal" %in% names(final_df)) {
    final_df$home_p_2w_blend <- final_df$home_p_2w_cal
  }

  if (!("home_p_2w_mkt" %in% names(final_df))) {
    final_df$home_p_2w_mkt <- NA_real_
  }

  missing_market <- !is.finite(final_df$home_p_2w_mkt)
  if (any(missing_market)) {
    mkt_now <- tryCatch(
      market_probs_from_sched(sched_df, spread_mapper = spread_mapper) %>%
        dplyr::select(game_id, p_home_mkt_2w),
      error = function(e) tibble::tibble(game_id = character(), p_home_mkt_2w = numeric())
    )
    if (nrow(mkt_now)) {
      final_df <- final_df %>%
        left_join(mkt_now, by = "game_id") %>%
        mutate(home_p_2w_mkt = dplyr::coalesce(home_p_2w_mkt, p_home_mkt_2w)) %>%
        select(-p_home_mkt_2w)
    }
  }

  spread_col <- pick_col(final_df, c("home_main_spread", "spread_close", "spread", "home_spread"))
  if (!is.na(spread_col)) {
    missing_market <- !is.finite(final_df$home_p_2w_mkt)
    if (any(missing_market)) {
      spreads <- suppressWarnings(as.numeric(final_df[[spread_col]]))
      if (!is.null(spread_mapper)) {
        final_df$home_p_2w_mkt[missing_market] <- spread_mapper$predict(spreads[missing_market])
      } else {
        SD_MARGIN <- 13.86
        final_df$home_p_2w_mkt[missing_market] <- .clp(stats::pnorm(-spreads[missing_market] / SD_MARGIN))
      }
    }
  }

  prob_priority <- c("home_p_2w_blend", "home_p_2w_cal", "home_p_2w_model")
  if (!is.null(preferred_prob_col) && isTRUE(nzchar(preferred_prob_col))) {
    prob_priority <- unique(c(preferred_prob_col, prob_priority))
  }
  prob_priority <- prob_priority[prob_priority %in% names(final_df)]

  pick_prob_with_source <- function(df, columns) {
    res <- rep(NA_real_, nrow(df))
    src <- rep(NA_character_, nrow(df))
    for (col in columns) {
      vals <- suppressWarnings(as.numeric(df[[col]]))
      take <- is.na(res) & is.finite(vals)
      if (any(take)) {
        res[take] <- vals[take]
        src[take] <- col
      }
    }
    list(values = res, source = src)
  }

  picked <- pick_prob_with_source(final_df, prob_priority)
  source_map <- c(
    home_p_2w_blend = "Blend",
    home_p_2w_cal = "Model",
    home_p_2w_model = "Model (raw)"
  )
  source_labels <- source_map[picked$source]
  source_labels[is.na(source_labels)] <- picked$source[is.na(source_labels)]

  final_df <- final_df %>%
    separate(matchup, into = c("away", "home"), sep = " @ ", remove = FALSE) %>%
    mutate(
      date = as.Date(date),
      home_prob_model = .clp(picked$values),
      home_prob_source = source_labels,
      home_prob_mkt   = dplyr::coalesce(.clp(home_p_2w_mkt), NA_real_),
      away_prob_model = 1 - home_prob_model,
      away_prob_mkt   = ifelse(is.finite(home_prob_mkt), 1 - home_prob_mkt, NA_real_),
      home_team = home,
      away_team = away
    )

  bets <- bind_rows(
    final_df %>%
      transmute(game_id, date, matchup, opponent = away_team, team = home_team,
                side_key = "home", model_prob = home_prob_model, market_prob = home_prob_mkt,
                model_source = home_prob_source),
    final_df %>%
      transmute(game_id, date, matchup, opponent = home_team, team = away_team,
                side_key = "away", model_prob = away_prob_model, market_prob = away_prob_mkt,
                model_source = home_prob_source)
  ) %>%
    mutate(
      model_prob = .clp(model_prob),
      market_prob = ifelse(is.finite(market_prob), .clp(market_prob), NA_real_),
      side = paste0(team, " ML"),
      fair_ml_model = prob_to_american(model_prob),
      fair_ml_mkt   = ifelse(is.finite(market_prob), prob_to_american(market_prob), NA_real_)
    )

  if (!is.null(focus_matchup)) {
    bets <- bets %>% filter(matchup == focus_matchup)
  }

  if (!is.null(line_catalog) && nrow(line_catalog)) {
    ml_best <- line_catalog %>%
      filter(bet_bucket == "Moneyline") %>%
      group_by(game_id, side) %>%
      slice_max(order_by = ev_units, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        game_id,
        side_key = side,
        best_book = book,
        best_odds = ifelse(is.finite(odds), sprintf("%+d", as.integer(round(odds))), NA_character_),
        best_odds_num = odds,
        best_market_prob = market_prob
      )

    bets <- bets %>%
      left_join(ml_best, by = c("game_id", "side_key")) %>%
      mutate(
        market_prob = dplyr::coalesce(best_market_prob,
                                      ifelse(is.finite(best_odds_num), american_to_prob(best_odds_num), NA_real_),
                                      market_prob),
        best_odds_num = dplyr::coalesce(best_odds_num,
                                        ifelse(is.finite(market_prob), prob_to_american(market_prob), NA_real_)),
        best_odds = ifelse(is.na(best_odds) & is.finite(best_odds_num),
                           sprintf("%+d", as.integer(round(best_odds_num))), best_odds)
      )
  } else {
    bets <- bets %>%
      mutate(
        best_book = NA_character_,
        best_odds_num = ifelse(is.finite(market_prob), prob_to_american(market_prob), NA_real_),
        best_odds = ifelse(is.finite(best_odds_num), sprintf("%+d", as.integer(round(best_odds_num))), NA_character_)
      )
  }

  bets <- bets %>%
    mutate(
      market_prob = ifelse(is.finite(market_prob), .clp(market_prob), NA_real_),
      edge = model_prob - market_prob,
      fair_ml_mkt = ifelse(is.finite(market_prob), prob_to_american(market_prob), NA_real_),
      ev_units = ifelse(is.finite(best_odds_num), expected_value_units(model_prob, best_odds_num), NA_real_)
    ) %>%
    filter(is.finite(market_prob)) %>%
    arrange(desc(edge))

  bets %>%
    select(game_id, date, matchup, side_key, side, team, opponent, model_prob, market_prob,
           edge, ev_units, best_book, best_odds, best_odds_num, fair_ml_model, fair_ml_mkt,
           model_source)
}

bets_gt <- NULL
bets_table <- tibble::tibble()
best_offers_widget <- NULL

if (exists("final")) {
  seasons_needed <- sort(unique(final$season))
  lines_raw <- safe_load_lines(seasons_needed)
  line_catalog <- build_line_catalog(final, lines_raw, sched)
  best_offers <- best_offer_rows(line_catalog)

  bets <- build_best_bets(final, sched, spread_mapper, FOCUS_MATCHUP, line_catalog,
                          preferred_prob_col = preferred_final_prob_col)
  message("\n=== Best Bets (current slate) ===")
  if (nrow(bets)) {
    bets_table <- bets %>%
      mutate(
        `Side` = side,
        `Edge` = edge,
        `Model` = model_prob,
        `Market` = market_prob,
        `Best Odds` = best_odds_num,
        `Best Book` = best_book,
        `EV (1u)` = ev_units,
        `Fair ML (Model)` = fair_ml_model,
        `Fair ML (Market)` = fair_ml_mkt,
        `Model Source` = model_source
      ) %>%
      select(date, matchup, `Side`, `Edge`, `Model`, `Market`, `Best Odds`, `Best Book`, `EV (1u)`,
             `Fair ML (Model)`, `Fair ML (Market)`, `Model Source`)

    edge_domain <- range(bets_table$`Edge`, na.rm = TRUE)
    if (!all(is.finite(edge_domain))) edge_domain <- c(-0.1, 0.1)
    edge_domain <- c(min(edge_domain[1], -0.1), max(edge_domain[2], 0.1))
    palette_edge <- scales::col_numeric("RdYlGn", domain = edge_domain)

    if (gt_available) {
      bets_gt <- bets_table %>%
        gt::gt() %>%
        gt::tab_header(
          title = gt::md("**Best Bets vs Market**"),
          subtitle = "Moneyline focus"
        ) %>%
        gt::fmt_date(columns = date) %>%
        gt::fmt_percent(columns = c(`Edge`, `Model`, `Market`), decimals = 1) %>%
        gt::fmt_number(columns = `EV (1u)`, decimals = 3, drop_trailing_zeros = TRUE) %>%
        gt::fmt(
          columns = c(`Best Odds`, `Fair ML (Model)`, `Fair ML (Market)`),
          fns = function(x) ifelse(is.na(x), "—", sprintf("%+d", as.integer(round(x))))
        ) %>%
        gt::data_color(
          columns = `Edge`,
          colors = function(x) palette_edge(scales::squish(x, edge_domain))
        )

      print(bets_gt)
    } else {
      print(bets_table)
    }
  } else {
    message("No moneyline opportunities surfaced for the current slate.")
  }

  if (reactable_available && nrow(best_offers)) {
    message("\n=== Best Available Odds (spread & alt) ===")
    ev_domain <- range(best_offers$ev_units, na.rm = TRUE)
    if (!all(is.finite(ev_domain))) ev_domain <- c(-0.05, 0.05)
    ev_domain <- c(min(ev_domain[1], -0.05), max(ev_domain[2], 0.05))
    palette_ev <- scales::col_numeric("RdYlGn", domain = ev_domain)

    best_offers_widget <- reactable::reactable(
      best_offers %>%
        mutate(date = as.Date(date)),
      searchable = TRUE,
      defaultPageSize = 20,
      pagination = TRUE,
      defaultSorted = list("date" = "asc"),
      defaultColDef = reactable::colDef(align = "center"),
      columns = list(
        date = reactable::colDef(name = "Date", sticky = "left", format = reactable::colFormat(date = TRUE)),
        matchup = reactable::colDef(name = "Matchup", sticky = "left", align = "left"),
        bet_bucket = reactable::colDef(name = "Bet Type"),
        selection = reactable::colDef(name = "Selection", align = "left", sticky = "left"),
        odds_fmt = reactable::colDef(name = "Odds"),
        book = reactable::colDef(name = "Book"),
        model_prob = reactable::colDef(name = "Model %", format = reactable::colFormat(percent = TRUE, digits = 1)),
        market_prob = reactable::colDef(name = "Market %", format = reactable::colFormat(percent = TRUE, digits = 1)),
        edge = reactable::colDef(name = "Edge %", format = reactable::colFormat(percent = TRUE, digits = 1),
                      cell = function(value) {
                        col <- palette_ev(value)
                        list(style = list(background = col, color = ifelse(value > 0.04, "white", "black")))
                      }),
        ev_units = reactable::colDef(name = "EV (1u)", format = reactable::colFormat(digits = 3),
                          cell = function(value) {
                            col <- palette_ev(value)
                            list(style = list(background = col, color = ifelse(value > 0.02, "white", "black")))
                          })
      ),
      highlight = TRUE
    )
    print(best_offers_widget)
  } else if (nrow(best_offers)) {
    message("Best available odds table skipped because 'reactable' is unavailable in this environment.")
  } else {
    message("No sportsbook lines available via nflreadr::load_lines for the requested slate.")
  }
} else {
  message("\nNo `final` object found. To see Best Bets, run your simulation first (or save `final` to RDS and set FINAL_RDS).")
}

# ------------------ Plain “win/lose” verdict line (nice & loud) ---------------
verdict_line <- function(ci_tbl_row) {
  sprintf("%s: Δ=%.4f  (95%% CI: [%.4f, %.4f])  → %s",
          ci_tbl_row$metric, ci_tbl_row$delta, ci_tbl_row$lo, ci_tbl_row$hi, ci_tbl_row$verdict)
}
message("\n=== Verdicts (Model – Market) ===")
apply(ci_tbl, 1, function(r) message(verdict_line(as.list(r))))

preformatted_df <- function(df) {
  htmltools::tags$pre(paste0(utils::capture.output(print(df)), collapse = "\n"))
}

report_sections <- htmltools::tagList(htmltools::tags$h1("NFL Model vs Market"))

report_sections <- htmltools::tagAppendChildren(
  report_sections,
  htmltools::tags$h2("Overall (Model vs Market)"),
  if (!is.null(overall_gt)) htmltools::HTML(gt::as_raw_html(overall_gt)) else preformatted_df(overall_tbl)
)

report_sections <- htmltools::tagAppendChildren(
  report_sections,
  htmltools::tags$h2("Week-block bootstrap CI (Model – Market)"),
  if (!is.null(ci_gt)) htmltools::HTML(gt::as_raw_html(ci_gt)) else preformatted_df(ci_tbl)
)

if (!is.null(overall_blend)) {
  report_sections <- htmltools::tagAppendChildren(
    report_sections,
    htmltools::tags$h2("Overall (Blend vs Market)"),
    if (!is.null(overall_blend_gt)) htmltools::HTML(gt::as_raw_html(overall_blend_gt)) else preformatted_df(overall_blend)
  )
}

if (!is.null(ci_tbl_blend)) {
  report_sections <- htmltools::tagAppendChildren(
    report_sections,
    htmltools::tags$h2("Week-block bootstrap CI (Blend – Market)"),
    if (!is.null(ci_blend_gt)) htmltools::HTML(gt::as_raw_html(ci_blend_gt)) else preformatted_df(ci_tbl_blend)
  )
}

if (!is.null(bets_gt)) {
  report_sections <- htmltools::tagAppendChildren(
    report_sections,
    htmltools::tags$h2("Best Bets vs Market"),
    htmltools::HTML(gt::as_raw_html(bets_gt))
  )
} else if (exists("bets_table") && nrow(bets_table)) {
  report_sections <- htmltools::tagAppendChildren(
    report_sections,
    htmltools::tags$h2("Best Bets vs Market"),
    preformatted_df(bets_table)
  )
}

if (!is.null(best_offers_widget)) {
  report_sections <- htmltools::tagAppendChildren(
    report_sections,
    htmltools::tags$h2("Best Available Odds"),
    best_offers_widget
  )
} else if (exists("best_offers") && nrow(best_offers)) {
  report_sections <- htmltools::tagAppendChildren(
    report_sections,
    htmltools::tags$h2("Best Available Odds"),
    preformatted_df(best_offers)
  )
}

report_file <- file.path(getwd(), "NFLvsmarket_report.html")
if (htmlwidgets_available) {
  htmlwidgets::saveWidget(htmlwidgets::browsable(report_sections), report_file, selfcontained = TRUE)
} else {
  htmltools::save_html(report_sections, file = report_file)
}
message(sprintf("Saved HTML report to: %s", report_file))
try(utils::browseURL(report_file), silent = TRUE)
