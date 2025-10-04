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
    dplyr::transmute(
      game_id, season, week,
      p_model = .clamp01(p_model),
      p_mkt   = .clamp01(p_home_mkt_2w),
      y2
    ) %>%
    dplyr::mutate(
      b_model = (p_model - y2)^2,
      b_mkt   = (p_mkt - y2)^2,
      ll_model = -(y2 * log(p_model) + (1 - y2) * log(1 - p_model)),
      ll_mkt   = -(y2 * log(p_mkt)   + (1 - y2) * log(1 - p_mkt))
    )

  wk_stats <- comp %>%
    dplyr::group_by(season, week) %>%
    dplyr::summarise(
      n_games = dplyr::n(),
      b_model_sum = sum(b_model),
      b_mkt_sum   = sum(b_mkt),
      ll_model_sum = sum(ll_model),
      ll_mkt_sum   = sum(ll_mkt),
      .groups = "drop"
    ) %>%
    dplyr::arrange(season, week)
  
  overall <- tibble::tibble(
    model_Brier2 = mean(comp$b_model, na.rm = TRUE),
    mkt_Brier2   = mean(comp$b_mkt,   na.rm = TRUE),
    model_LogL2  = mean(comp$ll_model, na.rm = TRUE),
    mkt_LogL2    = mean(comp$ll_mkt,   na.rm = TRUE),
    n_games      = nrow(comp)
  ) %>%
    dplyr::mutate(
      d_Brier2 = model_Brier2 - mkt_Brier2,
      d_LogL2  = model_LogL2  - mkt_LogL2
    )
  # ---- Week-block bootstrap CIs for deltas (model - market) ----
  .bootstrap_deltas <- function(stats_tbl, B, seed) {
    if (!nrow(stats_tbl)) return(matrix(numeric(), nrow = 2L, dimnames = list(c("dB", "dL"), NULL)))
    if (!is.null(seed)) set.seed(seed)
    n_weeks <- nrow(stats_tbl)
    idx_mat <- matrix(sample.int(n_weeks, size = n_weeks * B, replace = TRUE), nrow = n_weeks, ncol = B)
    games_mat <- matrix(stats_tbl$n_games[idx_mat], nrow = n_weeks, ncol = B)
    total_games <- colSums(games_mat)
    b_diff <- colSums(matrix(stats_tbl$b_model_sum[idx_mat] - stats_tbl$b_mkt_sum[idx_mat], nrow = n_weeks, ncol = B))
    ll_diff <- colSums(matrix(stats_tbl$ll_model_sum[idx_mat] - stats_tbl$ll_mkt_sum[idx_mat], nrow = n_weeks, ncol = B))
    rbind(dB = b_diff / total_games, dL = ll_diff / total_games)
  }

  B <- 1000L
  boot <- .bootstrap_deltas(wk_stats, B = B, seed = 123)

  rolling_week_bootstrap <- function(stats_tbl, window_sizes = c(8, 17), B = 400, seed = 123) {
    if (!nrow(stats_tbl)) return(tibble::tibble())
    purrr::map_dfr(window_sizes, function(win) {
      if (nrow(stats_tbl) < win) return(tibble::tibble())
      purrr::map_dfr(seq(win, nrow(stats_tbl)), function(end_idx) {
        sel <- stats_tbl[(end_idx - win + 1):end_idx, , drop = FALSE]
        seed_offset <- seed + end_idx + win
        boot_sub <- .bootstrap_deltas(sel, B = B, seed = seed_offset)
        total_games <- sum(sel$n_games)
        tibble::tibble(
          window_weeks = win,
          end_season = sel$season[nrow(sel)],
          end_week   = sel$week[nrow(sel)],
          games_in_window = total_games,
          dB_mean = mean(boot_sub["dB",], na.rm = TRUE),
          dB_lo   = unname(stats::quantile(boot_sub["dB",], 0.025, na.rm = TRUE)),
          dB_hi   = unname(stats::quantile(boot_sub["dB",], 0.975, na.rm = TRUE)),
          dL_mean = mean(boot_sub["dL",], na.rm = TRUE),
          dL_lo   = unname(stats::quantile(boot_sub["dL",], 0.025, na.rm = TRUE)),
          dL_hi   = unname(stats::quantile(boot_sub["dL",], 0.975, na.rm = TRUE))
        )
      })
    })
  }

  rolling_ci <- rolling_week_bootstrap(wk_stats)

  .paired_ci <- function(x, conf = 0.95) {
    x <- stats::na.omit(x)
    n <- length(x)
    if (!n) {
      return(c(mean = NA_real_, lo = NA_real_, hi = NA_real_))
    }
    mu <- mean(x)
    if (n == 1L) {
      return(c(mean = mu, lo = mu, hi = mu))
    }
    se <- stats::sd(x)/sqrt(n)
    crit <- stats::qt(0.5 + conf/2, df = n - 1)
    c(mean = mu, lo = mu - crit * se, hi = mu + crit * se)
  }

  paired_dB <- .paired_ci(comp$b_model - comp$b_mkt)
  paired_dL <- .paired_ci(comp$ll_model - comp$ll_mkt)

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
  
  cat(sprintf("\nΔLogLoss (model - market, week-block bootstrap): mean=%.6f, 95%% CI [%.6f, %.6f]\n",
              dLL["mean"], dLL["lo"], dLL["hi"]))
  cat(sprintf("ΔBrier2  (model - market, week-block bootstrap): mean=%.6f, 95%% CI [%.6f, %.6f]\n",
              dBS["mean"], dBS["lo"], dBS["hi"]))
  if (is.finite(paired_dL["mean"])) {
    cat(sprintf("ΔLogLoss (model - market, paired t): mean=%.6f, 95%% CI [%.6f, %.6f]\n",
                paired_dL["mean"], paired_dL["lo"], paired_dL["hi"]))
  }
  if (is.finite(paired_dB["mean"])) {
    cat(sprintf("ΔBrier2  (model - market, paired t): mean=%.6f, 95%% CI [%.6f, %.6f]\n",
                paired_dB["mean"], paired_dB["lo"], paired_dB["hi"]))
  }
  # ---- END bootstrap CI block ----
  
  by_season <- comp %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      model_Brier2 = mean(b_model),
      mkt_Brier2   = mean(b_mkt),
      model_LogL2  = mean(ll_model),
      mkt_LogL2    = mean(ll_mkt),
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

  if (nrow(rolling_ci)) {
    latest_roll <- rolling_ci %>%
      dplyr::group_by(window_weeks) %>%
      dplyr::slice_tail(n = 1) %>%
      dplyr::ungroup()
    cat("\nRolling week-block bootstrap deltas (latest windows):\n")
    print(latest_roll)
  }

  invisible(list(
    overall = overall,
    deltas = list(LogLoss = dLL, Brier = dBS),
    paired_ci = list(LogLoss = paired_dL, Brier = paired_dB),
    by_season = by_season,
    bins = bins,
    comp = comp,
    rolling = rolling_ci
  ))
}
