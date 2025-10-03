compare_to_market <- function(res, sched) {
  stopifnot(is.list(res), is.data.frame(sched))
  # --- helpers (scoped to this function) ---
  .clamp01 <- function(x, eps = 1e-12) pmin(pmax(x, eps), 1 - eps)
  .pick_col <- function(df, cands) { nm <- intersect(cands, names(df)); if (length(nm)) nm[1] else NA_character_ }
  american_to_prob <- function(odds) ifelse(odds < 0, (-odds)/((-odds)+100), 100/(odds+100))
  devig_2way <- function(p_home_raw, p_away_raw){
    den <- p_home_raw + p_away_raw
    tibble::tibble(
      p_home_mkt_2w = .clamp01(p_home_raw/den),
      p_away_mkt_2w = .clamp01(p_away_raw/den)
    )
  }
  brier2 <- function(p,y) mean((p - y)^2, na.rm = TRUE)
  logloss2 <- function(p,y,eps=1e-12){
    p <- .clamp01(p, eps); -mean(y*log(p) + (1-y)*log(1-p), na.rm = TRUE)
  }
  
  seasons_eval <- sort(unique(res$by_week$season))
  sched_eval <- sched %>% dplyr::filter(season %in% seasons_eval, game_type %in% c("REG","Regular"))
  
  # Fallback: if no market columns on sched, join ESPN consensus by date/teams
  need_lines <- !any(c("home_ml","ml_home","moneyline_home","home_moneyline",
                       "away_ml","ml_away","moneyline_away","away_moneyline",
                       "spread_line","spread","home_spread","close_spread","spread_close") %in% names(sched_eval))
  if (need_lines) {
    # Pull the dates you need, then join into sched_eval
    dates_to_pull <- sort(unique(as.Date(sched_eval$game_date)))
    espn_tbl <- purrr::map_dfr(dates_to_pull, espn_odds_for_date)
    sched_eval <- sched_eval %>%
      dplyr::left_join(
        espn_tbl %>% dplyr::transmute(
          game_date = date,
          home_team, away_team,
          spread_line = home_spread_cons
        ),
        by = c("game_date","home_team","away_team")
      )
  }
  
  
  home_pts_col <- .pick_col(sched_eval, c("home_score","home_points","score_home","home_pts"))
  away_pts_col <- .pick_col(sched_eval, c("away_score","away_points","score_away","away_pts"))
  stopifnot(!is.na(home_pts_col), !is.na(away_pts_col))
  
  # moneylines preferred; else spread
  ml_home_col <- .pick_col(sched_eval, c("home_ml","ml_home","moneyline_home","home_moneyline"))
  ml_away_col <- .pick_col(sched_eval, c("away_ml","ml_away","moneyline_away","away_moneyline"))
  
  if (!is.na(ml_home_col) && !is.na(ml_away_col)) {
    mkt_tbl <- sched_eval %>%
      dplyr::transmute(
        game_id, season, week,
        home_ml = suppressWarnings(as.numeric(.data[[ml_home_col]])),
        away_ml = suppressWarnings(as.numeric(.data[[ml_away_col]]))
      ) %>%
      dplyr::filter(is.finite(home_ml), is.finite(away_ml)) %>%
      dplyr::mutate(
        p_home_raw = american_to_prob(home_ml),
        p_away_raw = american_to_prob(away_ml)
      ) %>%
      dplyr::bind_cols(devig_2way(.$p_home_raw, .$p_away_raw)) %>%
      dplyr::select(game_id, season, week, p_home_mkt_2w)
    msg <- "Market comparison: using moneylines (de-vigged)."
  } else {
    spread_col <- .pick_col(sched_eval, c("spread_line","spread","home_spread","close_spread","spread_close","spread_favorite"))
    if (is.na(spread_col)) stop("No moneyline or spread column on `sched` for market comparison.")
    SD_MARGIN <- 13.86
    mkt_tbl <- sched_eval %>%
      dplyr::transmute(game_id, season, week, home_spread = suppressWarnings(as.numeric(.data[[spread_col]]))) %>%
      dplyr::filter(is.finite(home_spread)) %>%
      dplyr::mutate(p_home_mkt_2w = .clamp01(pnorm(-home_spread / SD_MARGIN)))
    msg <- "Market comparison: using spreads (Normal margin model)."
  }
  
  outcomes <- sched_eval %>%
    dplyr::transmute(game_id, season, week,
                     y2 = as.integer(.data[[home_pts_col]] > .data[[away_pts_col]]))
  
  preds_src <- if ("per_game" %in% names(res)) res$per_game
  else if ("preds" %in% names(res)) res$preds
  else if ("eval" %in% names(res) && "per_game" %in% names(res$eval)) res$eval$per_game
  else stop("Couldn’t find predictions in `res`.")
  
  pcol <- .pick_col(preds_src, c("p2_cal","home_p_2w_cal","p2_home_cal","home_p2w_cal"))
  stopifnot(!is.na(pcol))
  
  comp <- preds_src %>%
    dplyr::transmute(game_id, season, week, p_model = .clamp01(.data[[pcol]])) %>%
    dplyr::inner_join(mkt_tbl,  by = c("game_id","season","week")) %>%
    dplyr::inner_join(outcomes, by = c("game_id","season","week")) %>%
    dplyr::transmute(game_id, season, week, p_model, p_mkt = .clamp01(p_home_mkt_2w), y2)
  
  overall <- tibble::tibble(
    model_Brier2 = brier2(comp$p_model, comp$y2),
    mkt_Brier2   = brier2(comp$p_mkt,   comp$y2),
    model_LogL2  = logloss2(comp$p_model, comp$y2),
    mkt_LogL2    = logloss2(comp$p_mkt,   comp$y2),
    n_games      = nrow(comp)
  ) %>%
    dplyr::mutate(
      d_Brier2 = model_Brier2 - mkt_Brier2,
      d_LogL2  = model_LogL2  - mkt_LogL2
    )
  # ---- Week-block bootstrap CIs for deltas (model - market) ----
  set.seed(123)
  wk_keys <- comp %>% dplyr::distinct(season, week)
  B <- 1000L
  boot <- replicate(B, {
    samp <- wk_keys[sample(nrow(wk_keys), replace = TRUE), ]
    jj   <- comp %>% dplyr::inner_join(samp, by = c("season","week"))
    c(
      dB = brier2(jj$p_model, jj$y2) - brier2(jj$p_mkt, jj$y2),
      dL = logloss2(jj$p_model, jj$y2) - logloss2(jj$p_mkt, jj$y2)
    )
  })
  
  dBS <- c(
    mean = mean(boot["dB",], na.rm = TRUE),
    lo   = unname(quantile(boot["dB",], 0.025, na.rm = TRUE)),
    hi   = unname(quantile(boot["dB",], 0.975, na.rm = TRUE))
  )
  dLL <- c(
    mean = mean(boot["dL",], na.rm = TRUE),
    lo   = unname(quantile(boot["dL",], 0.025, na.rm = TRUE)),
    hi   = unname(quantile(boot["dL",], 0.975, na.rm = TRUE))
  )
  
  cat(sprintf("\nΔLogLoss (model - market): mean=%.6f, 95%% CI [%.6f, %.6f]\n",
              dLL["mean"], dLL["lo"], dLL["hi"]))
  cat(sprintf("ΔBrier2  (model - market): mean=%.6f, 95%% CI [%.6f, %.6f]\n",
              dBS["mean"], dBS["lo"], dBS["hi"]))
  # ---- END bootstrap CI block ----
  
  by_season <- comp %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      model_Brier2 = brier2(p_model, y2),
      mkt_Brier2   = brier2(p_mkt,   y2),
      model_LogL2  = logloss2(p_model, y2),
      mkt_LogL2    = logloss2(p_mkt,   y2),
      n_games      = dplyr::n(), .groups = "drop"
    ) %>%
    dplyr::mutate(
      d_Brier2 = model_Brier2 - mkt_Brier2,
      d_LogL2  = model_LogL2  - mkt_LogL2
    )
  
  bins <- comp %>%
    dplyr::mutate(bin = cut(p_mkt, breaks = seq(0,1,0.1), include.lowest = TRUE)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(p_hat = mean(p_mkt), y_bar = mean(y2), n = dplyr::n(), .groups="drop")
  
  # print like before
  message(msg)
  print(overall)

  cat("\n--- By season (model vs market) ---\n"); print(by_season)
  cat("\nMarket reliability (2-way bins):\n"); print(bins)
  
  invisible(list(overall = overall, deltas = list(LogLoss = dLL, Brier = dBS),
                 by_season = by_season, bins = bins, comp = comp))
}
