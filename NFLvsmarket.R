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
  library(gt)
  library(nflreadr)
})

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

pick_col <- function(df, cands) {
  nm <- intersect(cands, names(df))
  if (length(nm)) nm[1] else NA_character_
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
brier <- function(p,y) mean((.clp(p)-y)^2)
logloss <- function(p,y) { p <- .clp(p); -mean(y*log(p) + (1-y)*log(1-p)) }

bootstrap_week_ci <- function(df, p_col_model, p_col_mkt, y_col = "y2",
                              n_boot = 2000, seed = 42) {
  set.seed(seed)
  weeks <- df %>% distinct(season, week)
  Brier_d <- numeric(n_boot)
  LogL_d  <- numeric(n_boot)
  
  for (b in seq_len(n_boot)) {
    samp <- weeks[sample(nrow(weeks), replace = TRUE), , drop = FALSE]
    tmp  <- df %>% inner_join(samp, by = c("season","week"))
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

eval_df <- res$per_game %>%
  # keep just what we need
  transmute(game_id, season, week,
            p_model = .clp(p2_cal),
            p_blend = if ("p_blend" %in% names(res$per_game)) .clp(p_blend) else NA_real_) %>%
  inner_join(outcomes, by = c("game_id","season","week"))

# Market probs (closing ML preferred; else spread mapping)
spread_mapper <- learn_spread_map(sched)  # may return NULL -> script falls back gracefully
mkt_df <- market_probs_from_sched(sched, spread_mapper = spread_mapper)

comp <- eval_df %>%
  inner_join(mkt_df, by = c("game_id","season","week")) %>%
  mutate(p_mkt = .clp(p_home_mkt_2w))

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
    n_games = sum(n_games),
    Brier_model = weighted.mean(Brier_model, n_games),
    Brier_mkt   = weighted.mean(Brier_mkt,   n_games),
    LogL_model  = weighted.mean(LogL_model,  n_games),
    LogL_mkt    = weighted.mean(LogL_mkt,    n_games),
    .groups = "drop"
  ) %>%
  mutate(
    Brier_delta = Brier_model - Brier_mkt,
    LogL_delta  = LogL_model  - LogL_mkt
  )

message("\n=== Overall (Model vs Market) ===")
print(overall_tbl)

ci_tbl <- bootstrap_week_ci(comp, p_col_model = "p_model", p_col_mkt = "p_mkt", n_boot = N_BOOT)
message("\n=== Week-block bootstrap CI (Model – Market) ===")
print(ci_tbl)

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
      n_games = sum(n_games),
      Brier_blend = weighted.mean(Brier_blend, n_games),
      Brier_mkt   = weighted.mean(Brier_mkt,   n_games),
      LogL_blend  = weighted.mean(LogL_blend,  n_games),
      LogL_mkt    = weighted.mean(LogL_mkt,    n_games),
      .groups = "drop"
    ) %>%
    mutate(
      Brier_delta = Brier_blend - Brier_mkt,
      LogL_delta  = LogL_blend  - LogL_mkt
    )
  message("\n=== Overall (Blend vs Market) ===")
  print(overall_blend)
  
  ci_tbl_blend <- bootstrap_week_ci(comp, p_col_model = "p_blend", p_col_mkt = "p_mkt", n_boot = N_BOOT)
  message("\n=== Week-block bootstrap CI (Blend – Market) ===")
  print(ci_tbl_blend)
}

# ------------------ Best Bets table for current slate -------------------------
build_best_bets <- function(final_df, sched_df, spread_mapper=NULL, focus_matchup=NULL) {
  stopifnot(all(c("matchup","date") %in% names(final_df)))
  # final_df should carry home_p_2w_cal; if not, reconstruct from calibrated 3-way
  if (!("home_p_2w_cal" %in% names(final_df)) && all(c("home_win_prob_cal","away_win_prob_cal","tie_prob") %in% names(final_df))) {
    final_df <- final_df %>%
      mutate(two_way_mass = pmax(1 - tie_prob, 1e-9),
             home_p_2w_cal = .clp(home_win_prob_cal / two_way_mass))
  }
  
  mkt_now <- market_probs_from_sched(sched_df, spread_mapper = spread_mapper) %>%
    select(game_id, p_home_mkt_2w)
  
  out <- final_df %>%
    separate(matchup, into = c("away","home"), sep = " @ ", remove = FALSE) %>%
    mutate(date = as.Date(date)) %>%
    left_join(mkt_now, by = "game_id") %>%
    mutate(
      p_model = .clp(home_p_2w_cal),
      p_mkt   = .clp(p_home_mkt_2w),
      edge    = p_model - p_mkt,
      side    = ifelse(edge >= 0, paste0(home, " ML"), paste0(away, " ML")),
      fair_ml_model = prob_to_american(p_model),
      fair_ml_mkt   = prob_to_american(p_mkt)
    ) %>%
    select(date, matchup, p_model, p_mkt, edge, side, fair_ml_model, fair_ml_mkt) %>%
    arrange(desc(edge))
  
  if (!is.null(focus_matchup)) {
    out <- out %>% filter(matchup == focus_matchup)
  }
  out
}

if (exists("final")) {
  bets <- build_best_bets(final, sched, spread_mapper, FOCUS_MATCHUP)
  message("\n=== Best Bets (current slate) ===")
  bets %>%
    mutate(
      `Model %`  = scales::percent(p_model, accuracy = 0.1),
      `Market %` = scales::percent(p_mkt,   accuracy = 0.1),
      `Edge %`   = scales::percent(edge,    accuracy = 0.1),
      `Fair ML (Model)`  = ifelse(is.finite(fair_ml_model), sprintf("%+d", fair_ml_model), NA),
      `Fair ML (Market)` = ifelse(is.finite(fair_ml_mkt),   sprintf("%+d", fair_ml_mkt),   NA)
    ) %>%
    select(date, matchup, side, `Edge %`, `Model %`, `Market %`, `Fair ML (Model)`, `Fair ML (Market)`) %>%
    gt() %>%
    tab_header(title = "Best Bets vs Market (2-way ML)") %>%
    fmt_date(columns = "date") %>%
    print()
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
