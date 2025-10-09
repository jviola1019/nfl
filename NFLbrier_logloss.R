compare_to_market <- function(res,
                              sched,
                              peers = NULL,
                              conf_level = 0.95,
                              B = 1000L,
                              seed = 123,
                              rolling_window_sizes = c(8, 17),
                              rolling_B = NULL) {
  stopifnot(is.list(res), is.data.frame(sched))
  if (!is.null(conf_level)) {
    stopifnot(length(conf_level) == 1, is.numeric(conf_level), conf_level > 0, conf_level < 1)
  } else {
    conf_level <- 0.95
  }
  stopifnot(length(B) == 1, is.numeric(B), B > 0)
  if (is.null(rolling_B)) {
    rolling_B <- min(400L, max(200L, as.integer(B)))
  }
  stopifnot(length(rolling_B) == 1, is.numeric(rolling_B), rolling_B > 0)
  stopifnot(is.null(rolling_window_sizes) ||
              (length(rolling_window_sizes) > 0 &&
                 all(is.numeric(rolling_window_sizes)) &&
                 all(is.finite(rolling_window_sizes)) &&
                 all(rolling_window_sizes > 0)))

  B <- as.integer(B)
  rolling_B <- as.integer(rolling_B)

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

  # Prefer an existing helper that already knows how to source market probs.
  mkt_tbl <- NULL
  msg <- NULL

  has_sched_market_cols <- any(c("home_ml","ml_home","moneyline_home","home_moneyline",
                                 "away_ml","ml_away","moneyline_away","away_moneyline",
                                 "spread_line","spread","home_spread","close_spread","spread_close") %in% names(sched_eval))

  if (!has_sched_market_cols && exists("market_probs_from_sched", mode = "function")) {
    mkt_tbl <- tryCatch({
      market_probs_from_sched(sched_eval)
    }, error = function(e) {
      message("compare_to_market(): market_probs_from_sched() failed: ", conditionMessage(e))
      NULL
    })

    if (!is.null(mkt_tbl)) {
      if (all(c("game_id", "season", "week", "p_home_mkt_2w") %in% names(mkt_tbl))) {
        mkt_tbl <- mkt_tbl %>%
          dplyr::transmute(game_id, season, week, p_home_mkt_2w = .clamp01(p_home_mkt_2w))
        msg <- "Market comparison: using provided market probabilities."
      } else {
        message("compare_to_market(): market_probs_from_sched() did not return expected columns; falling back to schedule columns if available.")
        mkt_tbl <- NULL
      }
    }
  }

  # Fallback: if no helper output and no market columns on sched, try ESPN consensus by date/teams
  if (is.null(mkt_tbl) && !has_sched_market_cols) {
    if (!exists("espn_odds_for_date", mode = "function")) {
      message("compare_to_market(): no market columns and espn_odds_for_date() unavailable; skipping market comparison.")
      return(invisible(NULL))
    }

    # Pull the dates, then join into sched_eval
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
  
  ml_home_col <- .pick_col(sched_eval, c("home_ml","ml_home","moneyline_home","home_moneyline"))
  ml_away_col <- .pick_col(sched_eval, c("away_ml","ml_away","moneyline_away","away_moneyline"))
  
  if (is.null(mkt_tbl)) {
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
      if (is.na(spread_col)) {
        message("compare_to_market(): no market information available after fallbacks; skipping.")
        return(invisible(NULL))
      }
      SD_MARGIN <- 13.86
      mkt_tbl <- sched_eval %>%
        dplyr::transmute(game_id, season, week, home_spread = suppressWarnings(as.numeric(.data[[spread_col]]))) %>%
        dplyr::filter(is.finite(home_spread)) %>%
        dplyr::mutate(p_home_mkt_2w = .clamp01(pnorm(-home_spread / SD_MARGIN)))
      msg <- "Market comparison: using spreads (Normal margin model)."
    }
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
  
  dedupe_join_keys <- c("game_id", "season", "week")
  dedupe_key_syms <- rlang::syms(dedupe_join_keys)

  summarise_prob <- function(df, value_col, name) {
    stopifnot(value_col %in% names(df))

    df <- df %>%
      dplyr::filter(
        dplyr::if_all(dplyr::all_of(dedupe_join_keys), ~ !is.na(.)),
        is.finite(.data[[value_col]])
      )

    dup <- df %>%
      dplyr::count(!!!dedupe_key_syms) %>%
      dplyr::filter(.data$n > 1L)

    if (nrow(dup)) {
      msg <- sprintf(
        "compare_to_market(): collapsing %s duplicate rows for %d game/week combos using the mean.",
        name,
        nrow(dup)
      )
      message(msg)
    }

    df %>%
      dplyr::group_by(!!!dedupe_key_syms) %>%
      dplyr::summarise(
        !!rlang::sym(value_col) := mean(.data[[value_col]], na.rm = TRUE),
        .groups = "drop"
      )
  }

  preds_comp <- preds_src %>%
    dplyr::transmute(game_id, season, week, p_model = .clamp01(.data[[pcol]])) %>%
    summarise_prob("p_model", name = "model probability")

  mkt_tbl <- mkt_tbl %>%
    dplyr::transmute(game_id, season, week, p_home_mkt_2w = .clamp01(p_home_mkt_2w)) %>%
    summarise_prob("p_home_mkt_2w", name = "market probability")

  outcomes <- outcomes %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(dedupe_join_keys), ~ !is.na(.))) %>%
    dplyr::group_by(!!!dedupe_key_syms) %>%
    dplyr::summarise(
      y2 = {
        vals <- unique(y2[!is.na(y2)])
        if (length(vals) > 1L) {
          stop("compare_to_market(): conflicting outcomes for a single game/week combination.")
        }
        if (length(vals) == 0L) NA_integer_ else vals
      },
      .groups = "drop"
    )

  comp <- preds_comp %>%
    dplyr::inner_join(mkt_tbl,  by = dedupe_join_keys) %>%
    dplyr::inner_join(outcomes, by = dedupe_join_keys) %>%
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

  if (!nrow(comp)) {
    message("compare_to_market(): no overlapping games between predictions and market data; returning empty summary.")
    empty_delta <- c(mean = NA_real_, lo = NA_real_, hi = NA_real_)
    empty_overall <- tibble::tibble(
      model_Brier2 = NA_real_,
      mkt_Brier2   = NA_real_,
      model_LogL2  = NA_real_,
      mkt_LogL2    = NA_real_,
      n_games      = 0L,
      d_Brier2     = NA_real_,
      d_LogL2      = NA_real_
    )
    return(invisible(list(
      overall = empty_overall,
      deltas = list(LogLoss = empty_delta, Brier = empty_delta),
      paired = list(deltas = list(LogLoss = numeric(), Brier = numeric()), stats = list(), ci = list()),
      paired_ci = list(LogLoss = empty_delta, Brier = empty_delta),
      paired_stats = tibble::tibble(),
      paired_dL = empty_delta,
      paired_dB = empty_delta,
      by_season = tibble::tibble(),
      bins = tibble::tibble(),
      comp = comp,
      rolling = tibble::tibble(),
      peers = tibble::tibble()
    )))
  }

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
  # Week-block bootstrap CIs for deltas (model - market)
  .bootstrap_deltas <- function(stats_tbl, B, seed) {
    if (!nrow(stats_tbl) || B <= 0) {
      return(matrix(numeric(), nrow = 2L, dimnames = list(c("dB", "dL"), NULL)))
    }

    if (!is.null(seed)) set.seed(seed)

    n_weeks <- nrow(stats_tbl)
    counts <- stats::rmultinom(
      n = B,
      size = n_weeks,
      prob = rep.int(1 / n_weeks, n_weeks)
    )

    n_games_vec <- stats_tbl$n_games
    diff_b <- stats_tbl$b_model_sum - stats_tbl$b_mkt_sum
    diff_ll <- stats_tbl$ll_model_sum - stats_tbl$ll_mkt_sum

    total_games <- as.numeric(crossprod(n_games_vec, counts))
    b_diff <- as.numeric(crossprod(diff_b, counts))
    ll_diff <- as.numeric(crossprod(diff_ll, counts))

    total_games[total_games <= 0] <- NA_real_

    rbind(
      dB = b_diff / total_games,
      dL = ll_diff / total_games
    )
  }

  alpha <- (1 - conf_level)/2
  boot <- .bootstrap_deltas(wk_stats, B = B, seed = seed)

  rolling_window_defaults <- rolling_window_sizes
  rolling_B_default <- rolling_B
  seed_default <- seed

  rolling_week_bootstrap <- function(stats_tbl,
                                     window_sizes = rolling_window_defaults,
                                     B = rolling_B_default,
                                     seed = seed_default) {
    if (!nrow(stats_tbl) || is.null(window_sizes) || !length(window_sizes)) {
      return(tibble::tibble())
    }

    out <- vector("list", length(window_sizes) * max(1, nrow(stats_tbl)))
    out_idx <- 0L

    for (win in window_sizes) {
      if (is.na(win) || win <= 0 || nrow(stats_tbl) < win) {
        next
      }

      for (end_idx in seq.int(win, nrow(stats_tbl))) {
        sel <- stats_tbl[(end_idx - win + 1):end_idx, , drop = FALSE]
        seed_offset <- if (is.null(seed)) NULL else seed + end_idx + win
        boot_sub <- .bootstrap_deltas(sel, B = B, seed = seed_offset)
        total_games <- sum(sel$n_games)

        out_idx <- out_idx + 1L
        out[[out_idx]] <- tibble::tibble(
          window_weeks = win,
          end_season = sel$season[nrow(sel)],
          end_week   = sel$week[nrow(sel)],
          games_in_window = total_games,
          dB_mean = mean(boot_sub["dB",], na.rm = TRUE),
          dB_lo   = unname(stats::quantile(boot_sub["dB",], alpha, na.rm = TRUE)),
          dB_hi   = unname(stats::quantile(boot_sub["dB",], 1 - alpha, na.rm = TRUE)),
          dL_mean = mean(boot_sub["dL",], na.rm = TRUE),
          dL_lo   = unname(stats::quantile(boot_sub["dL",], alpha, na.rm = TRUE)),
          dL_hi   = unname(stats::quantile(boot_sub["dL",], 1 - alpha, na.rm = TRUE))
        )
      }
    }

    if (!out_idx) {
      tibble::tibble()
    } else {
      out[seq_len(out_idx)] %>% dplyr::bind_rows()
    }
  }

  rolling_ci <- rolling_week_bootstrap(wk_stats)

  .paired_summary <- function(delta, conf = 0.95) {
    delta <- stats::na.omit(delta)
    n <- length(delta)
    if (!n) {
      return(list(
        mean = NA_real_, lo = NA_real_, hi = NA_real_, se = NA_real_, sd = NA_real_,
        n = 0L, df = NA_integer_, t_stat = NA_real_, p_value = NA_real_,
        effect_size = NA_real_, conf_level = conf, contains_zero = NA,
        required_n = NA_real_
      ))
    }
    mu <- mean(delta)
    if (n == 1L) {
      se <- 0
      sdv <- 0
      ci_lo <- mu
      ci_hi <- mu
      t_stat <- NA_real_
      p_val <- NA_real_
    } else {
      sdv <- stats::sd(delta)
      se <- if (sdv > 0) sdv / sqrt(n) else 0
      crit <- stats::qt(0.5 + conf/2, df = n - 1)
      ci_lo <- mu - crit * se
      ci_hi <- mu + crit * se
      t_stat <- if (se > 0) mu / se else NA_real_
      p_val <- if (se > 0) 2 * stats::pt(-abs(t_stat), df = n - 1) else NA_real_
    }
    if (n == 1L) {
      sdv <- 0
    } else if (!exists("sdv", inherits = FALSE)) {
      sdv <- stats::sd(delta)
    }
    contains_zero <- !is.na(ci_lo) && !is.na(ci_hi) && ci_lo <= 0 && ci_hi >= 0
    req_n <- if (!is.na(mu) && !is.na(sdv) && sdv > 0 && abs(mu) > 0) {
      crit_norm <- stats::qnorm(0.5 + conf/2)
      ceiling((crit_norm * sdv / abs(mu))^2)
    } else {
      NA_real_
    }
    list(
      mean = mu,
      lo = ci_lo,
      hi = ci_hi,
      se = se,
      sd = sdv,
      n = n,
      df = if (n > 0) n - 1L else NA_integer_,
      t_stat = t_stat,
      p_value = p_val,
      effect_size = if (!is.na(sdv) && sdv > 0) mu / sdv else NA_real_,
      conf_level = conf,
      contains_zero = contains_zero,
      required_n = req_n
    )
  }

  delta_logloss <- comp$ll_model - comp$ll_mkt
  delta_brier <- comp$b_model - comp$b_mkt

  paired_raw <- list(
    LogLoss = delta_logloss,
    Brier = delta_brier
  )

  paired_stats <- purrr::map(paired_raw, .paired_summary, conf = conf_level)

  paired_ci <- purrr::map(paired_stats, ~c(mean = .x$mean, lo = .x$lo, hi = .x$hi))

  dBS <- c(
    mean = mean(boot["dB",], na.rm = TRUE),
    lo   = unname(stats::quantile(boot["dB",], alpha, na.rm = TRUE)),
    hi   = unname(stats::quantile(boot["dB",], 1 - alpha, na.rm = TRUE))
  )
  dLL <- c(
    mean = mean(boot["dL",], na.rm = TRUE),
    lo   = unname(stats::quantile(boot["dL",], alpha, na.rm = TRUE)),
    hi   = unname(stats::quantile(boot["dL",], 1 - alpha, na.rm = TRUE))
  )

  cat(sprintf("\nΔLogLoss (model - market, week-block bootstrap): mean=%.6f, %.0f%% CI [%.6f, %.6f]\n",
              dLL["mean"], conf_level * 100, dLL["lo"], dLL["hi"]))
  cat(sprintf("ΔBrier2  (model - market, week-block bootstrap): mean=%.6f, %.0f%% CI [%.6f, %.6f]\n",
              dBS["mean"], conf_level * 100, dBS["lo"], dBS["hi"]))

  paired_tbl <- purrr::imap_dfr(paired_stats, function(stat, metric) {
    tibble::tibble(
      metric = metric,
      mean = stat$mean,
      lo = stat$lo,
      hi = stat$hi,
      se = stat$se,
      sd = stat$sd,
      n = stat$n,
      df = stat$df,
      t_stat = stat$t_stat,
      p_value = stat$p_value,
      effect_size = stat$effect_size,
      contains_zero = stat$contains_zero,
      required_n_for_significance = stat$required_n
    )
  })

  if (any(purrr::map_lgl(paired_stats, ~ .x$n > 0))) {
    cat(sprintf("ΔLogLoss (model - market, paired t): mean=%.6f, %.0f%% CI [%.6f, %.6f] (p=%.4f)\n",
                paired_stats$LogLoss$mean, conf_level * 100,
                paired_stats$LogLoss$lo, paired_stats$LogLoss$hi,
                paired_stats$LogLoss$p_value))
    cat(sprintf("ΔBrier2  (model - market, paired t): mean=%.6f, %.0f%% CI [%.6f, %.6f] (p=%.4f)\n",
                paired_stats$Brier$mean, conf_level * 100,
                paired_stats$Brier$lo, paired_stats$Brier$hi,
                paired_stats$Brier$p_value))
    needs_msg <- paired_tbl %>% dplyr::filter(isTRUE(contains_zero))
    if (nrow(needs_msg)) {
      cat("\nPaired-delta intervals include 0 because the observed mean differences are small relative to their variability.\n")
      needs_msg %>%
        dplyr::mutate(
          explanation = dplyr::case_when(
            is.na(required_n_for_significance) ~ "More data or lower-variance predictions are required.",
            TRUE ~ sprintf("Approximately %d paired games would be needed to exclude 0 at %.0f%% confidence assuming identical variance.",
                           as.integer(required_n_for_significance), conf_level * 100)
          )
        ) %>%
        dplyr::select(metric, mean, sd, se, n, explanation) %>%
        print()
    }
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
  
  if (!is.null(msg)) message(msg)
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

  peer_stats <- tibble::tibble()
  if (!is.null(peers)) {
    stopifnot(is.data.frame(peers))
    base_cols <- c("game_id", "season", "week")
    if (!all(base_cols %in% names(peers))) {
      stop("`peers` must include game_id, season, and week columns.")
    }
    model_col <- .pick_col(peers, c("model", "model_name", "source", "name"))
    prob_col <- .pick_col(peers, c("p_model", "prob", "p_home", "home_prob", "prediction", "pred", "p"))
    if (is.na(model_col) || is.na(prob_col)) {
      stop("`peers` must include a model identifier column and a probability column.")
    }
    peers_clean <- peers %>%
      dplyr::transmute(
        game_id, season, week,
        model = as.character(.data[[model_col]]),
        p_peer = .clamp01(as.numeric(.data[[prob_col]]))
      ) %>%
      dplyr::filter(!is.na(model), is.finite(p_peer))

    base_comp <- comp %>%
      dplyr::select(game_id, season, week, y2, p_mkt, b_mkt, ll_mkt,
                    p_model_base = p_model, b_model_base = b_model, ll_model_base = ll_model)

    peer_comp <- peers_clean %>%
      dplyr::inner_join(base_comp, by = c("game_id", "season", "week")) %>%
      dplyr::mutate(
        b_peer = (p_peer - y2)^2,
        ll_peer = -(y2 * log(p_peer) + (1 - y2) * log(1 - p_peer)),
        dB_market = b_peer - b_mkt,
        dL_market = ll_peer - ll_mkt,
        dB_model  = b_peer - b_model_base,
        dL_model  = ll_peer - ll_model_base
      )

    if (nrow(peer_comp)) {
      peer_stats <- peer_comp %>%
        dplyr::group_by(model) %>%
        dplyr::group_modify(function(df, key) {
          market_ll <- .paired_summary(df$dL_market, conf = conf_level)
          market_br <- .paired_summary(df$dB_market, conf = conf_level)
          model_ll  <- .paired_summary(df$dL_model, conf = conf_level)
          model_br  <- .paired_summary(df$dB_model, conf = conf_level)
          tibble::tibble(
            n_games = nrow(df),
            LogLoss = mean(df$ll_peer),
            Brier = mean(df$b_peer),
            market_LogLoss = mean(df$ll_mkt),
            market_Brier = mean(df$b_mkt),
            model_LogLoss = mean(df$ll_model_base),
            model_Brier = mean(df$b_model_base),
            delta_market_LogLoss = market_ll$mean,
            delta_market_LogLoss_lo = market_ll$lo,
            delta_market_LogLoss_hi = market_ll$hi,
            delta_market_LogLoss_p = market_ll$p_value,
            delta_market_LogLoss_effect = market_ll$effect_size,
            delta_market_Brier = market_br$mean,
            delta_market_Brier_lo = market_br$lo,
            delta_market_Brier_hi = market_br$hi,
            delta_market_Brier_p = market_br$p_value,
            delta_market_Brier_effect = market_br$effect_size,
            delta_model_LogLoss = model_ll$mean,
            delta_model_LogLoss_lo = model_ll$lo,
            delta_model_LogLoss_hi = model_ll$hi,
            delta_model_LogLoss_p = model_ll$p_value,
            delta_model_LogLoss_effect = model_ll$effect_size,
            delta_model_Brier = model_br$mean,
            delta_model_Brier_lo = model_br$lo,
            delta_model_Brier_hi = model_br$hi,
            delta_model_Brier_p = model_br$p_value,
            delta_model_Brier_effect = model_br$effect_size
          )
        }) %>%
        dplyr::ungroup()

      cat("\nPeer model comparison (paired differences vs market and your model):\n")
      print(peer_stats)
    }
  }

  paired_dL <- if (!is.null(paired_ci$LogLoss)) paired_ci$LogLoss else c(mean = NA_real_, lo = NA_real_, hi = NA_real_)
  paired_dB <- if (!is.null(paired_ci$Brier))   paired_ci$Brier   else c(mean = NA_real_, lo = NA_real_, hi = NA_real_)

  invisible(list(
    overall = overall,
    deltas = list(LogLoss = dLL, Brier = dBS),
    paired = list(deltas = paired_raw, stats = paired_stats, ci = paired_ci),
    paired_ci = paired_ci,
    paired_stats = paired_tbl,
    paired_dL = paired_dL,
    paired_dB = paired_dB,
    by_season = by_season,
    bins = bins,
    comp = comp,
    rolling = rolling_ci,
    peers = peer_stats
  ))
}
