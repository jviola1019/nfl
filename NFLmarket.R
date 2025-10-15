# ------------------------------------------------------------------------------
# NFLmarket.R
# Utility helpers for attaching blended probabilities to historic results,
# enriching schedules with pre-kickoff ESPN market data, and comparing simulation
# output against the betting market with per-game Brier/log-loss diagnostics.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  source("NFLbrier_logloss.R")
  library(tidyverse)
})

# ------------------------------------------------------------------------------
# Shared join-key metadata ------------------------------------------------------
# ------------------------------------------------------------------------------

if (!exists("JOIN_KEY_ALIASES", inherits = FALSE)) {
  JOIN_KEY_ALIASES <- list(
    game_id = c("game_id", "gameid", "gameId", "gid"),
    season  = c("season", "season_std", "Season", "season_year", "seasonYear", "year"),
    week    = c("week", "week_std", "Week", "game_week", "gameWeek", "gameday_week", "wk")
  )
}

if (!exists("PREDICTION_JOIN_KEYS", inherits = FALSE)) {
  PREDICTION_JOIN_KEYS <- names(JOIN_KEY_ALIASES)
}

# ------------------------------------------------------------------------------
# General-purpose helpers -------------------------------------------------------
# ------------------------------------------------------------------------------

first_non_missing_typed <- function(x) {
  if (!length(x)) {
    return(x)
  }
  is_valid <- if (is.numeric(x)) {
    which(is.finite(x))
  } else {
    which(!is.na(x))
  }
  if (!length(is_valid)) {
    return(x[NA_integer_])
  }
  x[[is_valid[1L]]]
}

collapse_by_keys_relaxed <- function(df, keys, label = "data frame") {
  if (is.null(df) || !nrow(df) || !length(keys)) {
    return(df)
  }

  missing_keys <- setdiff(keys, names(df))
  if (length(missing_keys)) {
    warning(sprintf(
      "%s is missing required key columns: %s; skipping duplicate collapse.",
      label,
      paste(missing_keys, collapse = ", ")
    ))
    return(df)
  }

  complete_mask <- stats::complete.cases(df[keys])
  df_complete <- df[complete_mask, , drop = FALSE]
  df_incomplete <- df[!complete_mask, , drop = FALSE]

  if (!nrow(df_complete)) {
    return(df)
  }

  dup <- df_complete %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::filter(.data$n > 1L)

  if (!nrow(dup)) {
    return(df)
  }

  message(sprintf(
    "%s: collapsing %d duplicate rows keyed by %s (relaxed).",
    label,
    nrow(dup),
    paste(keys, collapse = ", ")
  ))

  non_key_cols <- setdiff(names(df_complete), keys)

  collapsed_complete <- df_complete %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(non_key_cols),
        ~ first_non_missing_typed(.x),
        .names = "{.col}"
      ),
      .groups = "drop"
    )

  out <- dplyr::bind_rows(collapsed_complete, df_incomplete) %>%
    dplyr::select(dplyr::all_of(names(df)))

  dup_check <- out %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(keys), ~ !is.na(.))) %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::filter(.data$n > 1L)

  if (nrow(dup_check)) {
    warning(sprintf(
      "%s: duplicates remain for %d key combinations after relaxed collapse.",
      label,
      nrow(dup_check)
    ))
  }

  out
}

standardize_join_keys <- function(df, key_alias = JOIN_KEY_ALIASES) {
  if (is.null(df) || !inherits(df, "data.frame")) {
    return(df)
  }

  out <- df
  for (canonical in names(key_alias)) {
    if (canonical %in% names(out)) next
    alt_names <- unique(c(key_alias[[canonical]], canonical))
    alt_names <- alt_names[alt_names != canonical]
    match <- alt_names[alt_names %in% names(out)]
    if (length(match)) {
      out <- dplyr::rename(out, !!canonical := !!rlang::sym(match[1]))
    }
  }

  out
}

select_first_column <- function(df, candidates) {
  intersect(candidates, names(df))[1]
}

copy_column_if_missing <- function(df, target, source, overwrite = FALSE) {
  if (!source %in% names(df)) {
    return(df)
  }
  needs_copy <- overwrite || !target %in% names(df) || all(is.na(df[[target]]))
  if (needs_copy) {
    df[[target]] <- df[[source]]
  }
  df
}

coerce_numeric_safely <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  suppressWarnings(as.numeric(x))
}

normalize_provider <- function(x) {
  if (is.null(x)) {
    return(character())
  }
  gsub("[^a-z]", "", tolower(trimws(as.character(x))))
}

standardize_side <- function(side_raw, home_team, away_team) {
  if (is.na(side_raw)) {
    return(NA_character_)
  }

  sr <- toupper(trimws(as.character(side_raw)))
  home <- toupper(home_team)
  away <- toupper(away_team)

  if (sr %in% c("HOME", "H", "HOST")) return("home")
  if (sr %in% c("AWAY", "VISITOR", "ROAD", "A")) return("away")
  if (nzchar(home) && grepl(home, sr, fixed = TRUE)) return("home")
  if (nzchar(away) && grepl(away, sr, fixed = TRUE)) return("away")
  if (nzchar(home) && startsWith(sr, substr(home, 1, 3))) return("home")
  if (nzchar(away) && startsWith(sr, substr(away, 1, 3))) return("away")
  NA_character_
}

format_line <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(
      abs(x - round(x)) < 1e-6,
      sprintf("%+d", as.integer(round(x))),
      sprintf("%+.1f", x)
    )
  )
}

cover_probability_norm <- function(mean_margin, sd_margin, spread, side = c("home", "away")) {
  if (missing(side) || !length(side)) {
    side <- "home"
  }

  side <- tolower(as.character(side))
  side[!side %in% c("home", "away")] <- "home"

  len <- max(length(mean_margin), length(sd_margin), length(spread), length(side))
  if (!len) {
    return(numeric())
  }

  mean_margin <- coerce_numeric_safely(mean_margin)
  sd_margin <- coerce_numeric_safely(sd_margin)
  spread <- coerce_numeric_safely(spread)

  mean_margin <- rep_len(mean_margin, len)
  sd_margin <- rep_len(sd_margin, len)
  spread <- rep_len(spread, len)
  side <- rep_len(side, len)

  out <- rep(NA_real_, len)
  valid <- is.finite(mean_margin) & is.finite(sd_margin) & sd_margin > 0 & is.finite(spread)
  if (!any(valid)) {
    return(out)
  }

  home_idx <- valid & side == "home"
  if (any(home_idx)) {
    out[home_idx] <- stats::pnorm(
      -spread[home_idx],
      mean = mean_margin[home_idx],
      sd = sd_margin[home_idx],
      lower.tail = FALSE
    )
  }

  away_idx <- valid & side == "away"
  if (any(away_idx)) {
    out[away_idx] <- stats::pnorm(
      spread[away_idx],
      mean = mean_margin[away_idx],
      sd = sd_margin[away_idx],
      lower.tail = TRUE
    )
  }

  out
}

safe_load_lines <- function(seasons) {
  seasons <- unique(stats::na.omit(as.integer(seasons)))
  if (!length(seasons)) {
    return(tibble::tibble())
  }
  tryCatch(nflreadr::load_lines(seasons = seasons), error = function(e) tibble::tibble())
}

closing_spreads_tbl <- function(sched_df) {
  sched_df <- standardize_join_keys(sched_df)
  sp_col <- select_first_column(sched_df, c(
    "close_spread", "spread_close", "home_spread_close",
    "spread_line", "spread", "home_spread"
  ))
  if (is.na(sp_col)) {
    return(tibble::tibble(game_id = character(), home_main_spread = numeric()))
  }
  sched_df %>%
    dplyr::filter(game_type %in% c("REG", "Regular")) %>%
    dplyr::transmute(game_id, home_main_spread = suppressWarnings(as.numeric(.data[[sp_col]])))
}

market_probs_from_lines <- function(lines_df, sched_df, provider = "ESPN BET") {
  if (is.null(lines_df) || !nrow(lines_df)) {
    return(tibble::tibble())
  }

  sched_df <- standardize_join_keys(sched_df)

  provider_col <- select_first_column(lines_df, c("provider", "book", "bookmaker", "sportsbook"))
  bet_col <- select_first_column(lines_df, c("bet_type", "market_type", "type", "wager_type"))
  side_col <- select_first_column(lines_df, c("side", "team", "participant", "selection"))
  odds_col <- select_first_column(lines_df, c("american_odds", "odds_american", "price", "line_price", "odds"))
  ts_col <- select_first_column(lines_df, c("timestamp", "last_update", "line_timestamp", "updated_at", "lastUpdated"))
  alt_col <- select_first_column(lines_df, c("is_alternate", "alternate", "is_alt", "alt_line"))

  if (is.na(provider_col) || is.na(bet_col) || is.na(side_col) ||
      is.na(odds_col) || !"game_id" %in% names(lines_df)) {
    return(tibble::tibble())
  }

  base <- sched_df %>%
    dplyr::filter(game_type %in% c("REG", "Regular")) %>%
    dplyr::transmute(game_id, season, week, home_team, away_team)

  provider_key <- normalize_provider(provider)
  if (!nzchar(provider_key)) {
    return(tibble::tibble())
  }

  lines_df %>%
    dplyr::mutate(provider_norm = normalize_provider(.data[[provider_col]])) %>%
    dplyr::filter(provider_norm == provider_key, game_id %in% base$game_id) %>%
    dplyr::transmute(
      game_id,
      bet_type = tolower(as.character(.data[[bet_col]])),
      side_raw = .data[[side_col]],
      odds = suppressWarnings(as.numeric(.data[[odds_col]])),
      is_alt = if (!is.na(alt_col)) as.logical(.data[[alt_col]]) else FALSE,
      ts_raw = if (!is.na(ts_col)) .data[[ts_col]] else NA,
      provider_norm
    ) %>%
    dplyr::left_join(base, by = "game_id") %>%
    dplyr::mutate(
      side = standardize_side(side_raw, home_team, away_team),
      ts_num = suppressWarnings(as.numeric(lubridate::ymd_hms(ts_raw, quiet = TRUE)))
    ) %>%
    dplyr::mutate(ts_num = ifelse(is.na(ts_num), suppressWarnings(as.numeric(as.POSIXct(ts_raw, tz = "UTC"))), ts_num)) %>%
    dplyr::mutate(ts_num = ifelse(is.na(ts_num), as.numeric(dplyr::row_number()), ts_num)) %>%
    dplyr::filter(
      grepl("moneyline", bet_type),
      !is.na(side),
      side %in% c("home", "away"),
      is.finite(odds),
      !isTRUE(is_alt)
    ) %>%
    dplyr::group_by(game_id, side) %>%
    dplyr::arrange(dplyr::desc(ts_num)) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(game_id, season, week, side, odds) %>%
    tidyr::pivot_wider(names_from = side, values_from = odds, names_prefix = "odds_") %>%
    dplyr::mutate(
      ph = american_to_probability(odds_home),
      pa = american_to_probability(odds_away),
      den = ph + pa,
      p_home_mkt_2w = clamp_probability(dplyr::if_else(is.finite(den) & den > 0, ph / den, NA_real_))
    ) %>%
    dplyr::select(game_id, season, week, p_home_mkt_2w) %>%
    dplyr::filter(is.finite(p_home_mkt_2w))
}

learn_spread_map <- function(sched_df) {
  sched_df <- standardize_join_keys(sched_df)
  sp_col <- select_first_column(sched_df, c(
    "close_spread", "spread_close", "home_spread_close",
    "spread_line", "spread", "home_spread"
  ))
  ml_h <- select_first_column(sched_df, c(
    "home_ml_close", "ml_home_close", "moneyline_home_close",
    "home_moneyline_close", "home_ml", "ml_home", "moneyline_home",
    "home_moneyline"
  ))
  ml_a <- select_first_column(sched_df, c(
    "away_ml_close", "ml_away_close", "moneyline_away_close",
    "away_moneyline_close", "away_ml", "ml_away", "moneyline_away",
    "away_moneyline"
  ))

  if (is.na(sp_col)) {
    return(NULL)
  }

  df <- sched_df %>%
    dplyr::filter(game_type %in% c("REG", "Regular")) %>%
    dplyr::transmute(
      spread = suppressWarnings(as.numeric(.data[[sp_col]])),
      ph = if (!is.na(ml_h) && !is.na(ml_a)) american_to_probability(.data[[ml_h]]) else NA_real_,
      pa = if (!is.na(ml_h) && !is.na(ml_a)) american_to_probability(.data[[ml_a]]) else NA_real_
    ) %>%
    dplyr::mutate(
      p_home = dplyr::if_else(is.finite(ph + pa) & (ph + pa) > 0, ph / (ph + pa), NA_real_)
    ) %>%
    dplyr::filter(is.finite(spread), is.finite(p_home)) %>%
    dplyr::mutate(
      weight = pmax(1L, round(1000 * abs(p_home - 0.5))),
      success = pmin(pmax(round(p_home * weight), 0L), weight)
    )

  if (nrow(df) < 400) {
    message("learn_spread_map(): insufficient ML+spread history; falling back to Normal approximation.")
    return(NULL)
  }

  fit <- stats::glm(
    cbind(success, weight - success) ~ stats::poly(spread, 3, raw = TRUE),
    data = df,
    family = stats::binomial()
  )

  list(
    predict = function(sp) {
      sp <- as.numeric(sp)
      preds <- suppressWarnings(stats::predict(fit, newdata = data.frame(spread = sp), type = "response"))
      clamp_probability(as.numeric(preds))
    },
    model = fit
  )
}

market_probs_from_sched <- function(sched_df, spread_mapper = NULL, lines_df = NULL, provider = "ESPN BET") {
  sched_df <- standardize_join_keys(sched_df)
  sp_col <- select_first_column(sched_df, c(
    "close_spread", "spread_close", "home_spread_close",
    "spread_line", "spread", "home_spread"
  ))
  ml_h <- select_first_column(sched_df, c(
    "home_ml_close", "ml_home_close", "moneyline_home_close",
    "home_moneyline_close", "home_ml", "ml_home", "moneyline_home",
    "home_moneyline"
  ))
  ml_a <- select_first_column(sched_df, c(
    "away_ml_close", "ml_away_close", "moneyline_away_close",
    "away_moneyline_close", "away_ml", "ml_away", "moneyline_away",
    "away_moneyline"
  ))

  base <- sched_df %>%
    dplyr::filter(game_type %in% c("REG", "Regular")) %>%
    dplyr::transmute(game_id, season, week)

  ml_tbl <- tibble::tibble(game_id = character(), season = integer(), week = integer(), p_home_mkt_2w_ml = numeric())
  if (!is.na(ml_h) && !is.na(ml_a)) {
    ml_tbl <- sched_df %>%
      dplyr::transmute(
        game_id, season, week,
        ph = american_to_probability(.data[[ml_h]]),
        pa = american_to_probability(.data[[ml_a]])
      ) %>%
      dplyr::filter(is.finite(ph), is.finite(pa)) %>%
      dplyr::mutate(
        den = ph + pa,
        p_home_mkt_2w_ml = clamp_probability(dplyr::if_else(is.finite(den) & den > 0, ph / den, NA_real_))
      ) %>%
      dplyr::select(game_id, season, week, p_home_mkt_2w_ml)
  }

  spread_tbl <- tibble::tibble(game_id = character(), season = integer(), week = integer(), p_home_mkt_2w_spread = numeric())
  if (!is.na(sp_col)) {
    spreads_df <- sched_df %>%
      dplyr::transmute(
        game_id, season, week,
        home_spread = suppressWarnings(as.numeric(.data[[sp_col]]))
      )

    mapper <- spread_mapper
    if (is.null(mapper)) {
      mapper <- learn_spread_map(sched_df)
    }

    if (!is.null(mapper)) {
      spread_tbl <- spreads_df %>%
        dplyr::mutate(p_home_mkt_2w_spread = mapper$predict(home_spread)) %>%
        dplyr::select(game_id, season, week, p_home_mkt_2w_spread)
    } else {
      SD_MARGIN <- 13.86
      spread_tbl <- spreads_df %>%
        dplyr::filter(is.finite(home_spread)) %>%
        dplyr::mutate(p_home_mkt_2w_spread = clamp_probability(stats::pnorm(-home_spread / SD_MARGIN))) %>%
        dplyr::select(game_id, season, week, p_home_mkt_2w_spread)
    }
  }

  fallback_tbl <- base %>%
    dplyr::left_join(spread_tbl, by = c("game_id", "season", "week")) %>%
    dplyr::left_join(ml_tbl, by = c("game_id", "season", "week")) %>%
    dplyr::mutate(p_home_mkt_2w = dplyr::coalesce(p_home_mkt_2w_ml, p_home_mkt_2w_spread)) %>%
    dplyr::select(game_id, season, week, p_home_mkt_2w)

  if (is.null(lines_df)) {
    seasons_lines <- sort(unique(base$season))
    lines_df <- safe_load_lines(seasons_lines)
  }

  provider_tbl <- market_probs_from_lines(lines_df, sched_df, provider = provider)

  out <- fallback_tbl
  if (!nrow(out)) {
    out <- base %>% dplyr::mutate(p_home_mkt_2w = NA_real_)
  }

  if (nrow(provider_tbl)) {
    out <- out %>%
      dplyr::left_join(
        provider_tbl %>% dplyr::rename(p_home_mkt_2w_provider = p_home_mkt_2w),
        by = c("game_id", "season", "week")
      ) %>%
      dplyr::mutate(p_home_mkt_2w = dplyr::coalesce(p_home_mkt_2w_provider, p_home_mkt_2w)) %>%
      dplyr::select(-p_home_mkt_2w_provider)
  }

  if (!"p_home_mkt_2w" %in% names(out)) {
    out$p_home_mkt_2w <- NA_real_
  }

  out
}

best_offer_rows <- function(catalog) {
  if (is.null(catalog) || !nrow(catalog)) {
    return(tibble::tibble())
  }

  catalog %>%
    dplyr::group_by(game_id, bet_bucket, side, line_display) %>%
    dplyr::slice_max(order_by = ev_units, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(game_id, bet_bucket) %>%
    dplyr::slice_max(order_by = ev_units, n = 6, with_ties = FALSE) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(date, matchup, bet_bucket, dplyr::desc(ev_units))
}

build_line_catalog <- function(final_df, lines_df, sched_df, provider = "ESPN BET") {
  if (is.null(lines_df) || !nrow(lines_df)) {
    return(tibble::tibble())
  }

  sched_df <- standardize_join_keys(sched_df)

  bt_col <- select_first_column(lines_df, c("bet_type", "market_type", "type", "wager_type"))
  side_col <- select_first_column(lines_df, c("side", "team", "participant", "selection"))
  odds_col <- select_first_column(lines_df, c("american_odds", "odds_american", "price", "line_price", "odds"))
  line_col <- select_first_column(lines_df, c("line", "spread_line", "handicap", "points", "spread"))
  book_col <- select_first_column(lines_df, c("provider", "book", "bookmaker", "sportsbook"))
  alt_col <- select_first_column(lines_df, c("is_alternate", "alternate", "is_alt", "alt_line"))

  if (is.na(bt_col) || is.na(side_col) || is.na(odds_col)) {
    return(tibble::tibble())
  }

  if (!"home_p_2w_blend" %in% names(final_df)) {
    stop("build_line_catalog(): final_df must include 'home_p_2w_blend'.")
  }

  base <- final_df %>%
    dplyr::transmute(
      game_id,
      date = as.Date(date),
      matchup,
      home_team,
      away_team,
      home_prob = clamp_probability(home_p_2w_blend),
      away_prob = 1 - home_prob,
      margin_mean,
      margin_sd
    ) %>%
    dplyr::distinct()

  if (!"game_id" %in% names(lines_df)) {
    return(tibble::tibble())
  }

  provider_key <- normalize_provider(provider)

  lines_df %>%
    dplyr::filter(game_id %in% base$game_id) %>%
    dplyr::transmute(
      game_id,
      bet_type = tolower(as.character(.data[[bt_col]])),
      side_raw = .data[[side_col]],
      odds = suppressWarnings(as.numeric(.data[[odds_col]])),
      line = if (!is.na(line_col)) suppressWarnings(as.numeric(.data[[line_col]])) else NA_real_,
      book = if (!is.na(book_col)) as.character(.data[[book_col]]) else NA_character_,
      is_alt = if (!is.na(alt_col)) as.logical(.data[[alt_col]]) else NA
    ) %>%
    dplyr::mutate(book_norm = normalize_provider(book)) %>%
    dplyr::filter(is.na(provider_key) | book_norm == provider_key) %>%
    dplyr::left_join(base, by = "game_id") %>%
    dplyr::mutate(
      side = standardize_side(side_raw, home_team, away_team),
      is_alt = dplyr::if_else(is.na(is_alt), FALSE, is_alt)
    ) %>%
    dplyr::filter(is.finite(odds), !is.na(side)) %>%
    dplyr::left_join(closing_spreads_tbl(sched_df), by = "game_id") %>%
    dplyr::mutate(
      main_spread_side = dplyr::case_when(
        !is.na(home_main_spread) & side == "home" ~ home_main_spread,
        !is.na(home_main_spread) & side == "away" ~ -home_main_spread,
        TRUE ~ NA_real_
      ),
      is_alt = dplyr::if_else(
        !is_alt & !is.na(main_spread_side) & !is.na(line),
        abs(line - main_spread_side) > 0.05,
        is_alt
      ),
      bet_bucket = dplyr::case_when(
        grepl("moneyline", bet_type) ~ "Moneyline",
        grepl("spread", bet_type) & is_alt ~ "Alt Spread",
        grepl("spread", bet_type) ~ "Spread",
        TRUE ~ stringr::str_to_title(bet_type)
      ),
      team_label = ifelse(side == "home", home_team, away_team),
      model_prob = dplyr::case_when(
        bet_bucket == "Moneyline" & side == "home" ~ home_prob,
        bet_bucket == "Moneyline" & side == "away" ~ away_prob,
        bet_bucket %in% c("Spread", "Alt Spread") & side == "home" ~
          cover_probability_norm(margin_mean, margin_sd, line, "home"),
        bet_bucket %in% c("Spread", "Alt Spread") & side == "away" ~
          cover_probability_norm(margin_mean, margin_sd, line, "away"),
        TRUE ~ NA_real_
      ),
      market_prob = clamp_probability(american_to_probability(odds)),
      edge = model_prob - market_prob,
      ev_units = expected_value_units(model_prob, odds),
      line_display = dplyr::case_when(
        bet_bucket == "Moneyline" ~ "ML",
        bet_bucket %in% c("Spread", "Alt Spread") ~ format_line(line),
        TRUE ~ as.character(line)
      ),
      selection = as.character(glue::glue("{team_label} {line_display}")),
      odds_fmt = ifelse(is.finite(odds), sprintf("%+d", as.integer(round(odds))), NA_character_)
    ) %>%
    dplyr::filter(is.finite(model_prob)) %>%
    dplyr::select(-book_norm)
}

build_best_bets <- function(final_df,
                            sched_df,
                            spread_mapper = NULL,
                            focus_matchup = NULL,
                            line_catalog = NULL,
                            preferred_prob_col = NULL,
                            lines_df = NULL,
                            provider = "ESPN BET") {
  stopifnot(all(c("matchup", "date") %in% names(final_df)))

  if (!"home_p_2w_blend" %in% names(final_df)) {
    stop("build_best_bets(): final_df must include 'home_p_2w_blend'.")
  }

  if (!("home_p_2w_mkt" %in% names(final_df))) {
    final_df$home_p_2w_mkt <- NA_real_
  }

  missing_market <- !is.finite(final_df$home_p_2w_mkt)
  if (any(missing_market)) {
    mkt_now <- tryCatch({
      market_probs_from_sched(
        sched_df,
        spread_mapper = spread_mapper,
        lines_df = lines_df,
        provider = provider
      ) %>% dplyr::select(game_id, p_home_mkt_2w)
    }, error = function(e) tibble::tibble(game_id = character(), p_home_mkt_2w = numeric()))

    if (nrow(mkt_now)) {
      final_df <- final_df %>%
        dplyr::left_join(mkt_now, by = "game_id") %>%
        dplyr::mutate(home_p_2w_mkt = dplyr::coalesce(home_p_2w_mkt, p_home_mkt_2w)) %>%
        dplyr::select(-p_home_mkt_2w)
    }
  }

  spread_col <- select_first_column(final_df, c("home_main_spread", "spread_close", "spread", "home_spread"))
  if (!is.na(spread_col)) {
    missing_market <- !is.finite(final_df$home_p_2w_mkt)
    if (any(missing_market)) {
      spreads <- suppressWarnings(as.numeric(final_df[[spread_col]]))
      if (!is.null(spread_mapper)) {
        final_df$home_p_2w_mkt[missing_market] <- spread_mapper$predict(spreads[missing_market])
      } else {
        SD_MARGIN <- 13.86
        final_df$home_p_2w_mkt[missing_market] <- clamp_probability(stats::pnorm(-spreads[missing_market] / SD_MARGIN))
      }
    }
  }

  prob_priority <- c("home_p_2w_blend")
  if (!is.null(preferred_prob_col) && isTRUE(nzchar(preferred_prob_col)) &&
      grepl("_blend$", preferred_prob_col) && preferred_prob_col %in% names(final_df)) {
    prob_priority <- unique(c(preferred_prob_col, prob_priority))
  }

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
  source_map <- c(home_p_2w_blend = "Blend")
  source_labels <- source_map[picked$source]
  source_labels[is.na(source_labels)] <- picked$source[is.na(source_labels)]

  final_df <- final_df %>%
    tidyr::separate(matchup, into = c("away", "home"), sep = " @ ", remove = FALSE) %>%
    dplyr::mutate(
      date = as.Date(date),
      home_prob_model = clamp_probability(picked$values),
      home_prob_source = source_labels,
      home_prob_mkt = dplyr::coalesce(clamp_probability(home_p_2w_mkt), NA_real_),
      away_prob_model = 1 - home_prob_model,
      away_prob_mkt = ifelse(is.finite(home_prob_mkt), 1 - home_prob_mkt, NA_real_),
      home_team = home,
      away_team = away
    )

  bets <- dplyr::bind_rows(
    final_df %>%
      dplyr::transmute(
        game_id,
        date,
        matchup,
        opponent = away_team,
        team = home_team,
        side_key = "home",
        model_prob = home_prob_model,
        market_prob = home_prob_mkt,
        model_source = home_prob_source
      ),
    final_df %>%
      dplyr::transmute(
        game_id,
        date,
        matchup,
        opponent = home_team,
        team = away_team,
        side_key = "away",
        model_prob = away_prob_model,
        market_prob = away_prob_mkt,
        model_source = home_prob_source
      )
  ) %>%
    dplyr::mutate(
      model_prob = clamp_probability(model_prob),
      market_prob = ifelse(is.finite(market_prob), clamp_probability(market_prob), NA_real_),
      side = paste0(team, " ML"),
      fair_ml_model = ifelse(
        is.finite(model_prob),
        vig_moneyline_from_prob(model_prob, side_key),
        NA_real_
      ),
      fair_ml_mkt = ifelse(is.finite(market_prob), vig_moneyline_from_prob(market_prob, side_key), NA_real_)
    )

  if (!is.null(focus_matchup)) {
    bets <- bets %>% dplyr::filter(matchup == focus_matchup)
  }

  if (!is.null(line_catalog) && nrow(line_catalog)) {
    ml_best <- line_catalog %>%
      dplyr::filter(bet_bucket == "Moneyline") %>%
      dplyr::group_by(game_id, side) %>%
      dplyr::slice_max(order_by = ev_units, n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(
        game_id,
        side_key = side,
        best_book = book,
        best_odds = ifelse(is.finite(odds), sprintf("%+d", as.integer(round(odds))), NA_character_),
        best_odds_num = odds,
        best_market_prob = market_prob
      )

    bets <- bets %>%
      dplyr::left_join(ml_best, by = c("game_id", "side_key")) %>%
      dplyr::mutate(
        market_prob = dplyr::coalesce(best_market_prob, market_prob),
        best_odds_num = dplyr::coalesce(
          best_odds_num,
          ifelse(is.finite(market_prob), vig_moneyline_from_prob(market_prob, side_key), NA_real_)
        ),
        best_odds = ifelse(
          is.na(best_odds) & is.finite(best_odds_num),
          sprintf("%+d", as.integer(round(best_odds_num))),
          best_odds
        )
      )
  } else {
    bets <- bets %>%
      dplyr::mutate(
        best_book = provider,
        best_odds_num = ifelse(is.finite(market_prob), vig_moneyline_from_prob(market_prob, side_key), NA_real_),
        best_odds = ifelse(is.finite(best_odds_num), sprintf("%+d", as.integer(round(best_odds_num))), NA_character_)
      )
  }

  bets %>%
    dplyr::mutate(
      best_book = ifelse(is.na(best_book) & nzchar(provider), provider, best_book),
      market_prob = ifelse(is.finite(market_prob), clamp_probability(market_prob), NA_real_),
      edge = model_prob - market_prob,
      fair_ml_mkt = ifelse(is.finite(market_prob), vig_moneyline_from_prob(market_prob, side_key), NA_real_),
      ev_units = ifelse(is.finite(best_odds_num), expected_value_units(model_prob, best_odds_num), NA_real_)
    ) %>%
    dplyr::filter(is.finite(market_prob)) %>%
    dplyr::arrange(dplyr::desc(edge)) %>%
    dplyr::select(
      game_id,
      date,
      matchup,
      side_key,
      side,
      team,
      opponent,
      model_prob,
      market_prob,
      edge,
      ev_units,
      best_book,
      best_odds,
      best_odds_num,
      fair_ml_model,
      fair_ml_mkt,
      model_source
    )
}

compute_market_summary_tables <- function(comparison,
                                          model_label = "Model",
                                          market_label = "Market") {
  comp_tbl <- comparison$comp
  if (is.null(comp_tbl) || !nrow(comp_tbl)) {
    empty <- tibble::tibble()
    return(list(overall = empty, ci = empty, paired = empty))
  }

  overall_weekly <- comp_tbl %>%
    dplyr::group_by(season, week) %>%
    dplyr::summarise(
      n_games = dplyr::n(),
      Brier_model = mean(b_model, na.rm = TRUE),
      LogL_model = mean(ll_model, na.rm = TRUE),
      Brier_mkt = mean(b_mkt, na.rm = TRUE),
      LogL_mkt = mean(ll_mkt, na.rm = TRUE),
      .groups = "drop"
    )

  overall_tbl <- overall_weekly %>%
    dplyr::summarise(
      n_weeks = dplyr::n(),
      n_games = sum(n_games),
      Brier_model = stats::weighted.mean(Brier_model, n_games, na.rm = TRUE),
      LogL_model = stats::weighted.mean(LogL_model, n_games, na.rm = TRUE),
      Brier_mkt = stats::weighted.mean(Brier_mkt, n_games, na.rm = TRUE),
      LogL_mkt = stats::weighted.mean(LogL_mkt, n_games, na.rm = TRUE),
      Brier_model_median = stats::median(Brier_model, na.rm = TRUE),
      LogL_model_median = stats::median(LogL_model, na.rm = TRUE),
      Brier_mkt_median = stats::median(Brier_mkt, na.rm = TRUE),
      LogL_mkt_median = stats::median(LogL_mkt, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Brier_model_delta = Brier_model - Brier_mkt,
      LogL_model_delta = LogL_model - LogL_mkt,
      Brier_model_delta_median = Brier_model_median - Brier_mkt_median,
      LogL_model_delta_median = LogL_model_median - LogL_mkt_median
    ) %>%
    dplyr::rename(
      !!paste(model_label, "Brier (mean)") := Brier_model,
      !!paste(market_label, "Brier (mean)") := Brier_mkt,
      `ΔBrier (mean)` = Brier_model_delta,
      !!paste(model_label, "Brier (median)") := Brier_model_median,
      !!paste(market_label, "Brier (median)") := Brier_mkt_median,
      `ΔBrier (median)` = Brier_model_delta_median,
      !!paste(model_label, "LogLoss (mean)") := LogL_model,
      !!paste(market_label, "LogLoss (mean)") := LogL_mkt,
      `ΔLogLoss (mean)` = LogL_model_delta,
      !!paste(model_label, "LogLoss (median)") := LogL_model_median,
      !!paste(market_label, "LogLoss (median)") := LogL_mkt_median,
      `ΔLogLoss (median)` = LogL_model_delta_median
    )

  deltas <- comparison$deltas
  ci_tbl <- tibble::tibble()
  if (!is.null(deltas) && all(c("Brier", "LogLoss") %in% names(deltas))) {
    ci_tbl <- tibble::tibble(
      metric = c("Brier (Model - Market)", "LogLoss (Model - Market)"),
      delta = c(deltas$Brier["mean"], deltas$LogLoss["mean"]),
      lo = c(deltas$Brier["lo"], deltas$LogLoss["lo"]),
      hi = c(deltas$Brier["hi"], deltas$LogLoss["hi"])
    ) %>%
      dplyr::mutate(
        verdict = dplyr::case_when(
          is.finite(hi) & hi < 0 ~ "WIN",
          is.finite(lo) & lo > 0 ~ "LOSE",
          TRUE ~ "TIE"
        )
      )
  }

  paired_tbl <- comparison$paired_stats
  if (is.null(paired_tbl)) {
    paired_tbl <- tibble::tibble()
  }

  list(overall = overall_tbl, ci = ci_tbl, paired = paired_tbl)
}

html_escape <- function(text) {
  text <- as.character(text)
  text <- gsub("&", "&amp;", text, fixed = TRUE)
  text <- gsub("<", "&lt;", text, fixed = TRUE)
  text <- gsub(">", "&gt;", text, fixed = TRUE)
  text <- gsub('"', "&quot;", text, fixed = TRUE)
  text
}

df_to_html_table <- function(df, classes = "data-table", row_class = NULL, escape_cols = names(df)) {
  if (is.null(df) || !nrow(df)) {
    return(character())
  }

  headers <- paste0(
    "<tr>",
    paste0(sprintf("<th>%s</th>", html_escape(names(df))), collapse = ""),
    "</tr>"
  )

  rows <- purrr::map_chr(seq_len(nrow(df)), function(i) {
    cells <- purrr::map_chr(seq_along(df), function(j) {
      col_name <- names(df)[j]
      raw_value <- df[[j]][i]
      value <- if (is.na(raw_value)) "—" else as.character(raw_value)
      if (col_name %in% escape_cols) {
        value <- html_escape(value)
      }
      sprintf("<td>%s</td>", value)
    })
    row_cls <- if (!is.null(row_class) && length(row_class) >= i) row_class[i] else ""
    if (nzchar(row_cls)) {
      sprintf("<tr class=\"%s\">%s</tr>", row_cls, paste0(cells, collapse = ""))
    } else {
      sprintf("<tr>%s</tr>", paste0(cells, collapse = ""))
    }
  })

  c(
    "<div class=\"table-container\">",
    sprintf("<table class=\"%s\">", classes),
    headers,
    rows,
    "</table>",
    "</div>"
  )
}

format_overall_for_html <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  num_cols <- setdiff(names(df)[vapply(df, is.numeric, logical(1))], c("n_weeks", "n_games"))
  if (length(num_cols)) {
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(num_cols), ~ ifelse(is.na(.), NA_character_, formatC(., format = "f", digits = 6))))
  }
  for (col in intersect(c("n_weeks", "n_games"), names(df))) {
    df[[col]] <- ifelse(is.na(df[[col]]), "—", formatC(df[[col]], format = "d"))
  }
  df %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
}

format_ci_for_html <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  df %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ ifelse(is.na(.), NA_character_, formatC(., format = "f", digits = 6))
      ),
      dplyr::across(dplyr::everything(), as.character)
    )
}

format_paired_for_html <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  df %>%
    dplyr::mutate(
      dplyr::across(
        where(is.numeric),
        ~ ifelse(is.na(.), NA_character_, formatC(., format = "f", digits = 6))
      ),
      dplyr::across(dplyr::everything(), as.character)
    )
}

format_best_bets_for_html <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(tibble::tibble())
  }

  df %>%
    dplyr::mutate(
      date = format(as.Date(date), "%a %b %d"),
      `Edge` = ifelse(is.na(edge), NA_character_, scales::percent(edge, accuracy = 0.1)),
      `Model` = ifelse(is.na(model_prob), NA_character_, scales::percent(model_prob, accuracy = 0.1)),
      `Market` = ifelse(is.na(market_prob), NA_character_, scales::percent(market_prob, accuracy = 0.1)),
      `Best Odds` = ifelse(is.na(best_odds_num), "—", sprintf("%+d", as.integer(round(best_odds_num)))),
      `Best Book` = ifelse(is.na(best_book) | !nzchar(best_book), "—", best_book),
      `EV (1u)` = ifelse(is.na(ev_units), NA_character_, formatC(ev_units, format = "f", digits = 3)),
      `Fair ML (Model)` = ifelse(is.na(fair_ml_model), "—", sprintf("%+d", as.integer(round(fair_ml_model)))),
      `Fair ML (Market)` = ifelse(is.na(fair_ml_mkt), "—", sprintf("%+d", as.integer(round(fair_ml_mkt)))),
      `Model Source` = ifelse(is.na(model_source) | !nzchar(model_source), "Blend", model_source),
      `Value Tier` = dplyr::case_when(
        ev_units >= 0.05 ~ "<span class=\"tag tier-high\">High Value</span>",
        ev_units >= 0.02 ~ "<span class=\"tag tier-medium\">Worth Watching</span>",
        ev_units >= 0     ~ "<span class=\"tag tier-lean\">Lean</span>",
        TRUE ~ "<span class=\"tag tier-pass\">Pass</span>"
      ),
      tier_class = dplyr::case_when(
        ev_units >= 0.05 ~ "tier-row-high",
        ev_units >= 0.02 ~ "tier-row-medium",
        ev_units >= 0    ~ "tier-row-lean",
        TRUE ~ "tier-row-pass"
      )
    ) %>%
    dplyr::transmute(
      date,
      matchup,
      side,
      `Edge`,
      `Model`,
      `Market`,
      `Best Odds`,
      `Best Book`,
      `EV (1u)`,
      `Fair ML (Model)`,
      `Fair ML (Market)`,
      `Model Source`,
      `Value Tier`,
      tier_class
    )
}

format_best_offers_for_html <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(tibble::tibble())
  }

  df %>%
    dplyr::mutate(
      date = format(as.Date(date), "%a %b %d"),
      model_prob = ifelse(is.na(model_prob), NA_character_, scales::percent(model_prob, accuracy = 0.1)),
      market_prob = ifelse(is.na(market_prob), NA_character_, scales::percent(market_prob, accuracy = 0.1)),
      odds = ifelse(is.na(odds_fmt), "—", odds_fmt),
      edge = ifelse(is.na(edge), NA_character_, scales::percent(edge, accuracy = 0.1)),
      ev_units = ifelse(is.na(ev_units), NA_character_, formatC(ev_units, format = "f", digits = 3))
    ) %>%
    dplyr::select(
      date,
      matchup,
      bet_bucket,
      selection,
      odds,
      book,
      model_prob,
      market_prob,
      edge,
      ev_units
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
}

render_market_report_html <- function(html_file,
                                      html_title,
                                      overall_tbl,
                                      ci_tbl,
                                      paired_tbl,
                                      best_bets_tbl,
                                      best_offers_tbl) {
  css_block <- paste(
    "body {font-family: 'Source Sans Pro', Arial, sans-serif; background-color: #111827; color: #e5e7eb; margin: 0; padding: 0;}",
    ".page {max-width: 1100px; margin: 0 auto; padding: 24px 32px 48px 32px;}",
    "h1 {font-size: 2rem; margin-bottom: 0.75rem; color: #ecfdf5;}",
    "h2 {font-size: 1.35rem; margin-top: 2rem; margin-bottom: 0.75rem; color: #a7f3d0;}",
    "p {line-height: 1.5;}",
    ".table-container {overflow-x: auto; margin-bottom: 1.5rem;}",
    "table {width: 100%; border-collapse: collapse; background-color: #f9fafb; color: #111827;}",
    "th {background-color: #15803d; color: #ecfdf5; text-transform: uppercase; letter-spacing: 0.08em; padding: 10px 12px;}",
    "td {padding: 10px 12px; border-bottom: 1px solid #d1d5db; text-align: center;}",
    "tr:nth-child(even) {background-color: #e5e7eb;}",
    "tr.tier-row-high {background-color: #064e3b; color: #ecfdf5;}",
    "tr.tier-row-medium {background-color: #166534; color: #ecfdf5;}",
    "tr.tier-row-lean {background-color: #22c55e; color: #111827;}",
    "tr.tier-row-pass {background-color: #bbf7d0; color: #111827;}",
    ".tag {display: inline-block; padding: 2px 8px; border-radius: 999px; font-size: 0.75rem; font-weight: 600;}",
    ".tier-high {background-color: #064e3b; color: #ecfdf5;}",
    ".tier-medium {background-color: #166534; color: #ecfdf5;}",
    ".tier-lean {background-color: #22c55e; color: #111827;}",
    ".tier-pass {background-color: #bbf7d0; color: #111827;}",
    sep = "\n"
  )

  sections <- list()

  if (!is.null(overall_tbl) && nrow(overall_tbl)) {
    overall_fmt <- format_overall_for_html(overall_tbl)
    sections <- c(
      sections,
      list("<h2>Long-Run Accuracy (Weighted by Games)</h2>"),
      list(df_to_html_table(overall_fmt))
    )
  }

  if (!is.null(ci_tbl) && nrow(ci_tbl)) {
    ci_fmt <- format_ci_for_html(ci_tbl)
    sections <- c(
      sections,
      list("<h2>Week-Block Bootstrap Δ (Model − Market)</h2>"),
      list(df_to_html_table(ci_fmt))
    )
  }

  if (!is.null(paired_tbl) && nrow(paired_tbl)) {
    paired_fmt <- format_paired_for_html(paired_tbl)
    sections <- c(
      sections,
      list("<h2>Paired Differences (Model − Market)</h2>"),
      list(df_to_html_table(paired_fmt))
    )
  }

  if (!is.null(best_bets_tbl) && nrow(best_bets_tbl)) {
    bets_fmt <- format_best_bets_for_html(best_bets_tbl)
    row_classes <- bets_fmt$tier_class
    bets_display <- bets_fmt %>% dplyr::select(-tier_class)
    sections <- c(
      sections,
      list("<h2>Moneyline Best Bets</h2>"),
      list(df_to_html_table(bets_display, row_class = row_classes, escape_cols = setdiff(names(bets_display), "Value Tier")))
    )
  }

  if (!is.null(best_offers_tbl) && nrow(best_offers_tbl)) {
    offers_fmt <- format_best_offers_for_html(best_offers_tbl)
    sections <- c(
      sections,
      list("<h2>Top Market Offers</h2>"),
      list(df_to_html_table(offers_fmt))
    )
  }

  body_inner <- paste(unlist(sections), collapse = "\n")
  doc <- paste0(
    "<html><head><meta charset=\"utf-8\"/><title>",
    html_escape(html_title),
    "</title><style>",
    css_block,
    "</style></head><body><div class=\"page\"><h1>",
    html_escape(html_title),
    "</h1>",
    body_inner,
    "</div></body></html>"
  )

  writeLines(doc, con = html_file)
  invisible(html_file)
}

generate_market_report <- function(res,
                                   final,
                                   sched,
                                   blend_oos = NULL,
                                   espn_lines = NULL,
                                   lines_df = NULL,
                                   provider = "ESPN BET",
                                   focus_matchup = NULL,
                                   html_file = file.path(getwd(), "NFLmarket_report.html"),
                                   html_title = "Market Verification & Best Bets",
                                   moneyline_html = file.path(getwd(), "NFLmarket_moneylines.html"),
                                   moneyline_title = "Model vs Market Moneylines",
                                   moneyline_vig = 0.10,
                                   comparison_args = list(),
                                   verbose = TRUE) {
  stopifnot(is.list(res), is.data.frame(final), is.data.frame(sched))

  eval_args <- c(
    list(
      res = res,
      blend_oos = blend_oos,
      sched = sched,
      espn_lines = espn_lines,
      verbose = verbose,
      html_output_path = moneyline_html,
      html_vig = moneyline_vig,
      html_title = moneyline_title
    ),
    comparison_args
  )

  comparison <- tryCatch(
    rlang::exec(evaluate_market_vs_blend, !!!eval_args),
    error = function(e) {
      if (verbose) {
        message(sprintf("generate_market_report(): evaluate_market_vs_blend failed: %s", conditionMessage(e)))
      }
      NULL
    }
  )

  if (is.null(comparison)) {
    if (verbose) {
      message("generate_market_report(): falling back to direct compare_to_market() evaluation.")
    }
    comparison <- tryCatch(
      rlang::exec(compare_to_market, res, sched, !!!comparison_args),
      error = function(e) {
        if (verbose) {
          message(sprintf("generate_market_report(): compare_to_market() fallback failed: %s", conditionMessage(e)))
        }
        NULL
      }
    )
    if (is.null(comparison)) {
      stop("generate_market_report(): market comparison could not be computed; ensure inputs are valid.")
    }
  }

  summary_tables <- compute_market_summary_tables(comparison)

  spread_mapper <- tryCatch(learn_spread_map(sched), error = function(e) {
    if (verbose) {
      message(sprintf("generate_market_report(): learn_spread_map() failed: %s", conditionMessage(e)))
    }
    NULL
  })

  seasons_needed <- stats::na.omit(unique(as.integer(final$season)))
  if (!length(seasons_needed)) {
    seasons_needed <- stats::na.omit(unique(as.integer(sched$season)))
  }

  if (is.null(lines_df)) {
    lines_df <- safe_load_lines(seasons_needed)
  }

  line_catalog <- tryCatch(
    build_line_catalog(final, lines_df, sched, provider = provider),
    error = function(e) {
      if (verbose) {
        message(sprintf("generate_market_report(): build_line_catalog() failed: %s", conditionMessage(e)))
      }
      tibble::tibble()
    }
  )

  best_offers <- tryCatch(best_offer_rows(line_catalog), error = function(e) tibble::tibble())

  best_bets <- tryCatch(
    build_best_bets(
      final_df = final,
      sched_df = sched,
      spread_mapper = spread_mapper,
      focus_matchup = focus_matchup,
      line_catalog = line_catalog,
      preferred_prob_col = "home_p_2w_blend",
      lines_df = lines_df,
      provider = provider
    ),
    error = function(e) {
      if (verbose) {
        message(sprintf("generate_market_report(): build_best_bets() failed: %s", conditionMessage(e)))
      }
      tibble::tibble()
    }
  )

  if (!is.null(html_file)) {
    tryCatch(
      render_market_report_html(
        html_file = html_file,
        html_title = html_title,
        overall_tbl = summary_tables$overall,
        ci_tbl = summary_tables$ci,
        paired_tbl = summary_tables$paired,
        best_bets_tbl = best_bets,
        best_offers_tbl = best_offers
      ),
      error = function(e) {
        if (verbose) {
          message(sprintf("generate_market_report(): failed to write HTML report: %s", conditionMessage(e)))
        }
      }
    )
  }

  list(
    comparison = comparison,
    summary = summary_tables,
    best_bets = best_bets,
    best_offers = best_offers,
    line_catalog = line_catalog,
    html_report = html_file,
    moneyline_report = moneyline_html
  )
}

american_to_probability <- function(odds) {
  odds <- coerce_numeric_safely(odds)
  ifelse(odds < 0, (-odds) / ((-odds) + 100), 100 / (odds + 100))
}

clamp_probability <- function(p, eps = 1e-06) {
  p <- coerce_numeric_safely(p)
  p <- dplyr::if_else(is.na(p), NA_real_, p)
  pmin(pmax(p, eps), 1 - eps)
}

probability_to_american <- function(prob) {
  prob <- clamp_probability(prob)
  dplyr::if_else(
    prob >= 0.5,
    -round(100 * prob / (1 - prob)),
    round(100 * (1 - prob) / prob)
  )
}

apply_moneyline_vig <- function(odds, vig = 0.10) {
  odds <- coerce_numeric_safely(odds)
  vig <- coerce_numeric_safely(vig)
  vig[is.na(vig)] <- 0
  dplyr::case_when(
    !is.finite(odds) ~ NA_real_,
    odds < 0        ~ -as.numeric(round(abs(odds) * (1 + vig))),
    TRUE            ~ as.numeric(round(odds / (1 + vig)))
  )
}

expected_value_units <- function(prob, odds) {
  prob <- clamp_probability(prob)
  odds <- coerce_numeric_safely(odds)
  dec <- dplyr::if_else(
    odds < 0,
    1 + 100 / abs(odds),
    1 + odds / 100
  )
  prob * (dec - 1) - (1 - prob)
}

vig_moneyline_from_prob <- function(prob, side_key = c("home", "away"), vig = 0.10) {
  prob <- clamp_probability(prob)
  if (missing(side_key) || !length(side_key)) {
    side_key <- "home"
  }

  side_key <- tolower(as.character(side_key))
  side_key[!side_key %in% c("home", "away")] <- "home"

  if (length(side_key) == 1L) {
    side_key <- rep(side_key, length(prob))
  } else {
    side_key <- rep_len(side_key, length(prob))
  }

  base <- probability_to_american(prob)
  adj <- apply_moneyline_vig(base, vig = vig)

  valid <- is.finite(adj)
  underdogs <- valid & side_key == "away"
  favorites <- valid & side_key == "home"

  if (any(underdogs)) {
    adj[underdogs] <- abs(adj[underdogs])
    adj[underdogs] <- ifelse(adj[underdogs] < 100, 100, adj[underdogs])
  }

  if (any(favorites)) {
    adj[favorites] <- ifelse(adj[favorites] > -100, -100, adj[favorites])
  }

  adj
}

devig_two_way_probabilities <- function(p_home_raw, p_away_raw) {
  total <- p_home_raw + p_away_raw
  tibble::tibble(
    p_home = dplyr::if_else(total > 0, p_home_raw / total, NA_real_),
    p_away = dplyr::if_else(total > 0, p_away_raw / total, NA_real_)
  )
}

# ------------------------------------------------------------------------------
# ESPN line preparation ---------------------------------------------------------
# ------------------------------------------------------------------------------

enrich_with_pre_kickoff_espn_lines <- function(sched,
                                               espn_lines = NULL,
                                               join_keys = PREDICTION_JOIN_KEYS,
                                               team_keys = c("home_team", "away_team"),
                                               verbose = TRUE) {
  if (is.null(sched) || !nrow(sched)) {
    if (verbose) message("enrich_with_pre_kickoff_espn_lines(): schedule empty; returning input unchanged.")
    return(sched)
  }

  sched_std <- standardize_join_keys(sched)

  if (!is.null(espn_lines) && nrow(espn_lines)) {
    espn_std <- standardize_join_keys(espn_lines)

    join_cols <- intersect(join_keys, intersect(names(sched_std), names(espn_std)))
    join_args <- list(x = sched_std, y = espn_std)

    if (length(join_cols)) {
      join_args$by <- join_cols
    } else {
      if (all(team_keys %in% names(sched_std)) &&
          all(team_keys %in% names(espn_std)) &&
          "game_date" %in% names(sched_std) &&
          any(c("date", "game_date", "match_date") %in% names(espn_std))) {
        date_col <- select_first_column(espn_std, c("game_date", "date", "match_date"))
        join_args$by <- c(game_date = date_col, setNames(team_keys, team_keys))
      } else if ("espn_game_id" %in% names(espn_std) && "espn_game_id" %in% names(sched_std)) {
        join_args$by <- "espn_game_id"
      } else {
        if (verbose) {
          message("enrich_with_pre_kickoff_espn_lines(): unable to determine join columns; skipping ESPN merge.")
        }
        join_args$by <- NULL
      }
    }

    if (!is.null(join_args$by)) {
      if ("relationship" %in% names(formals(dplyr::left_join))) {
        join_args$relationship <- "many-to-many"
        join_args$multiple <- "all"
      }
      sched_std <- tryCatch(
        {
          rlang::exec(dplyr::left_join, !!!join_args)
        },
        error = function(e) {
          if (verbose) {
            message("enrich_with_pre_kickoff_espn_lines(): left_join failed; reason: ", conditionMessage(e))
          }
          sched_std
        }
      )
    }
  }

  column_map <- list(
    espn_final_home_ml = c(
      "espn_final_home_ml", "espn_home_moneyline_final", "espn_home_ml_final",
      "home_ml_final", "home_moneyline_final", "home_ml_close", "home_moneyline_close",
      "espn_home_moneyline"
    ),
    espn_final_away_ml = c(
      "espn_final_away_ml", "espn_away_moneyline_final", "espn_away_ml_final",
      "away_ml_final", "away_moneyline_final", "away_ml_close", "away_moneyline_close",
      "espn_away_moneyline"
    ),
    espn_final_home_spread = c(
      "espn_final_home_spread", "espn_home_spread_final", "home_spread_final",
      "spread_final", "home_spread_close", "spread_close", "espn_home_spread"
    )
  )

  for (canonical in names(column_map)) {
    existing <- select_first_column(sched_std, column_map[[canonical]])
    if (!is.na(existing)) {
      sched_std[[canonical]] <- coerce_numeric_safely(sched_std[[existing]])
    } else if (!canonical %in% names(sched_std)) {
      sched_std[[canonical]] <- NA_real_
    }
  }

  sched_std <- copy_column_if_missing(sched_std, "home_ml", "espn_final_home_ml")
  sched_std <- copy_column_if_missing(sched_std, "away_ml", "espn_final_away_ml")
  sched_std <- copy_column_if_missing(sched_std, "spread_line", "espn_final_home_spread")

  if ("espn_final_home_ml" %in% names(sched_std) &&
      "espn_final_away_ml" %in% names(sched_std)) {
    raw_probs <- tibble::tibble(
      p_home_raw = american_to_probability(sched_std$espn_final_home_ml),
      p_away_raw = american_to_probability(sched_std$espn_final_away_ml)
    )
    devig <- devig_two_way_probabilities(raw_probs$p_home_raw, raw_probs$p_away_raw)
    sched_std$espn_final_home_prob <- devig$p_home
    sched_std$espn_final_away_prob <- devig$p_away
  }

  sched_std
}

# ------------------------------------------------------------------------------
# Model blend helpers -----------------------------------------------------------
# ------------------------------------------------------------------------------

build_res_blend <- function(res,
                            blend_oos,
                            join_keys = PREDICTION_JOIN_KEYS,
                            prob_candidates = c("p2_cal", "home_p_2w_cal", "p2_home_cal", "home_p2w_cal"),
                            verbose = TRUE) {
  if (is.null(res) || !is.list(res) || !"per_game" %in% names(res)) {
    if (verbose) message("build_res_blend(): res lacks a per_game component; skipping blend attachment.")
    return(NULL)
  }
  if (is.null(blend_oos) || !nrow(blend_oos)) {
    if (verbose) message("build_res_blend(): no blend history available; skipping blend attachment.")
    return(NULL)
  }

  per_game <- standardize_join_keys(res$per_game)
  prob_col <- select_first_column(per_game, prob_candidates)
  if (is.na(prob_col)) {
    if (verbose) message("build_res_blend(): no calibrated probability column found on res$per_game; skipping blend attachment.")
    return(NULL)
  }

  base_per_game <- collapse_by_keys_relaxed(per_game, join_keys, label = "Backtest per-game table")
  base_cols <- names(base_per_game)

  blend_join <- blend_oos %>%
    standardize_join_keys() %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(join_keys), ~ !is.na(.))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) %>%
    dplyr::summarise(p_blend = mean(p_blend, na.rm = TRUE), .groups = "drop")

  if (!nrow(blend_join)) {
    if (verbose) message("build_res_blend(): blend history has no complete join keys; skipping blend attachment.")
    return(NULL)
  }

  cast_column <- function(col, template, col_name) {
    if (!requireNamespace("vctrs", quietly = TRUE)) {
      return(col)
    }
    tryCatch(
      vctrs::vec_cast(col, template),
      vctrs_error_incompatible_type = function(e) {
        warning(sprintf(
          "build_res_blend(): lossy cast prevented for column %s; keeping original type.",
          col_name
        ))
        col
      },
      error = function(e) {
        warning(sprintf(
          "build_res_blend(): unable to align column %s: %s",
          col_name,
          conditionMessage(e)
        ))
        col
      }
    )
  }

  for (col_name in join_keys) {
    if (!col_name %in% names(blend_join) || !col_name %in% names(base_per_game)) next
    blend_join[[col_name]] <- cast_column(blend_join[[col_name]], base_per_game[[col_name]], col_name)
  }

  join_args <- list(x = base_per_game, y = blend_join, by = join_keys)
  if ("relationship" %in% names(formals(dplyr::left_join))) {
    join_args$relationship <- "many-to-one"
  }

  per_game_with_blend <- tryCatch(
    {
      rlang::exec(dplyr::left_join, !!!join_args)
    },
    error = function(e) {
      warning(sprintf("build_res_blend(): left_join failed; keeping original probabilities. Reason: %s", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(per_game_with_blend)) {
    return(NULL)
  }

  if (!"p_blend" %in% names(per_game_with_blend)) {
    if (verbose) message("build_res_blend(): joined table missing p_blend column; skipping blend attachment.")
    return(NULL)
  }

  per_game_with_blend[[prob_col]] <- dplyr::if_else(
    is.finite(per_game_with_blend$p_blend),
    per_game_with_blend$p_blend,
    per_game_with_blend[[prob_col]]
  )

  per_game_with_blend <- dplyr::select(per_game_with_blend, -p_blend)

  missing_cols <- setdiff(base_cols, names(per_game_with_blend))
  if (length(missing_cols)) {
    for (col_name in missing_cols) {
      per_game_with_blend[[col_name]] <- base_per_game[[col_name]][0]
    }
  }

  per_game_with_blend <- dplyr::select(per_game_with_blend, dplyr::all_of(base_cols))

  res_out <- res
  res_out$per_game <- per_game_with_blend

  if (verbose) {
    message(sprintf("build_res_blend(): attached blended probabilities using column '%s'.", prob_col))
  }

  res_out
}

# ------------------------------------------------------------------------------
# File utilities ----------------------------------------------------------------
# ------------------------------------------------------------------------------

find_latest_rds <- function(prefix, directory = "run_logs") {
  files <- list.files(directory, pattern = sprintf("^%s_.*\\.rds$", prefix), full.names = TRUE)
  if (!length(files)) {
    return(NA_character_)
  }
  files[order(file.info(files)$mtime, decreasing = TRUE)][1]
}

load_latest_market_inputs <- function(directory = "run_logs") {
  paths <- list(
    config = find_latest_rds("config", directory),
    final  = find_latest_rds("final", directory),
    games  = find_latest_rds("games_ready", directory)
  )

  loaded <- purrr::compact(purrr::map(paths, ~ if (is.na(.x)) NULL else readRDS(.x)))
  c(loaded, list(paths = paths))
}

# ------------------------------------------------------------------------------
# Market evaluation -------------------------------------------------------------
# ------------------------------------------------------------------------------

evaluate_market_vs_blend <- function(res,
                                     blend_oos,
                                     sched,
                                     espn_lines = NULL,
                                     join_keys = PREDICTION_JOIN_KEYS,
                                     prob_candidates = c("p2_cal", "home_p_2w_cal", "p2_home_cal", "home_p2w_cal"),
                                     verbose = TRUE,
                                     html_output_path = NULL,
                                     html_vig = 0.10,
                                     html_title = "Model vs Market Moneylines",
                                     ...) {
  res_blend <- build_res_blend(
    res = res,
    blend_oos = blend_oos,
    join_keys = join_keys,
    prob_candidates = prob_candidates,
    verbose = verbose
  )
  if (is.null(res_blend)) {
    if (verbose) message("evaluate_market_vs_blend(): unable to build res_blend; returning NULL.")
    return(NULL)
  }

  sched_prepared <- enrich_with_pre_kickoff_espn_lines(
    sched = sched,
    espn_lines = espn_lines,
    join_keys = join_keys,
    verbose = verbose
  )

  compare_args <- c(list(res_blend, sched_prepared), list(...))
  comparison <- rlang::exec(compare_to_market, !!!compare_args)

  if (!is.null(html_output_path)) {
    report_tbl <- build_moneyline_comparison_table(
      market_comparison_result = comparison,
      enriched_schedule = sched_prepared,
      join_keys = join_keys,
      vig = html_vig,
      verbose = verbose
    )

    if (nrow(report_tbl)) {
      export_moneyline_comparison_html(
        comparison_tbl = report_tbl,
        file = html_output_path,
        title = html_title,
        verbose = verbose
      )
    } else if (verbose) {
      message("evaluate_market_vs_blend(): no rows available for HTML report; skipping export.")
    }
  }

  if (is.list(comparison)) {
    comparison$enriched_schedule <- sched_prepared
  }

  comparison
}

extract_game_level_scores <- function(market_comparison_result) {
  if (is.null(market_comparison_result) ||
      !is.list(market_comparison_result) ||
      !"comp" %in% names(market_comparison_result)) {
    return(tibble::tibble())
  }

  market_comparison_result$comp %>%
    dplyr::transmute(
      game_id,
      season,
      week,
      model_prob = p_model,
      market_prob = p_mkt,
      actual_home_win = y2,
      brier_model = b_model,
      brier_market = b_mkt,
      logloss_model = ll_model,
      logloss_market = ll_mkt
    )
}

build_moneyline_comparison_table <- function(market_comparison_result,
                                             enriched_schedule,
                                             join_keys = PREDICTION_JOIN_KEYS,
                                             vig = 0.10,
                                             verbose = TRUE) {
  scores <- extract_game_level_scores(market_comparison_result)
  if (!nrow(scores)) {
    if (verbose) message("build_moneyline_comparison_table(): no comparison scores available; returning empty tibble.")
    return(tibble::tibble())
  }

  if (is.null(enriched_schedule) || !nrow(enriched_schedule)) {
    if (verbose) message("build_moneyline_comparison_table(): schedule input is empty; returning scores without context.")
    return(scores)
  }

  schedule_std <- standardize_join_keys(enriched_schedule)
  join_cols <- intersect(join_keys, intersect(names(schedule_std), names(scores)))

  if (!length(join_cols)) {
    if (verbose) message("build_moneyline_comparison_table(): no shared join keys between schedule and scores; returning scores.")
    return(scores)
  }

  schedule_collapsed <- collapse_by_keys_relaxed(
    schedule_std,
    keys = join_cols,
    label = "HTML schedule context"
  )

  pull_or_default <- function(df, col_name, default = NA) {
    if (is.na(col_name) || !col_name %in% names(df)) {
      rep(default, nrow(df))
    } else {
      df[[col_name]]
    }
  }

  home_team_col <- select_first_column(schedule_collapsed, c("home_team", "team_home", "home"))
  away_team_col <- select_first_column(schedule_collapsed, c("away_team", "team_away", "away"))
  date_col <- select_first_column(schedule_collapsed, c("game_date", "gameDate", "date"))
  home_ml_col <- select_first_column(schedule_collapsed, c("espn_final_home_ml", "home_ml", "ml_home", "home_moneyline"))
  away_ml_col <- select_first_column(schedule_collapsed, c("espn_final_away_ml", "away_ml", "ml_away", "away_moneyline"))

  schedule_context <- schedule_collapsed %>%
    dplyr::mutate(
      home_team = as.character(pull_or_default(schedule_collapsed, home_team_col, NA_character_)),
      away_team = as.character(pull_or_default(schedule_collapsed, away_team_col, NA_character_)),
      game_date = suppressWarnings(as.Date(pull_or_default(schedule_collapsed, date_col, NA_character_))),
      market_home_ml = coerce_numeric_safely(pull_or_default(schedule_collapsed, home_ml_col, NA_real_)),
      market_away_ml = coerce_numeric_safely(pull_or_default(schedule_collapsed, away_ml_col, NA_real_))
    ) %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(join_cols)),
      home_team,
      away_team,
      game_date,
      market_home_ml,
      market_away_ml
    )

  combined <- scores %>%
    dplyr::inner_join(schedule_context, by = join_cols) %>%
    dplyr::mutate(
      matchup = dplyr::if_else(
        is.na(home_team) | is.na(away_team),
        NA_character_,
        paste(away_team, "@", home_team)
      ),
      market_home_ml = dplyr::if_else(
        is.na(market_home_ml),
        probability_to_american(market_prob),
        market_home_ml
      ),
      market_away_ml = dplyr::if_else(
        is.na(market_away_ml),
        probability_to_american(1 - market_prob),
        market_away_ml
      ),
      model_home_ml = probability_to_american(model_prob),
      model_home_ml_vig = apply_moneyline_vig(model_home_ml, vig = vig),
      model_away_ml = probability_to_american(1 - model_prob),
      model_away_ml_vig = apply_moneyline_vig(model_away_ml, vig = vig),
      model_edge_prob = model_prob - market_prob,
      model_ev_units = expected_value_units(model_prob, market_home_ml),
      market_beats_model = brier_market < brier_model,
      actual_winner = dplyr::case_when(
        is.na(actual_home_win) ~ NA_character_,
        actual_home_win == 1L  ~ home_team,
        actual_home_win == 0L  ~ away_team,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::arrange(season, week, game_date, matchup)

  combined
}

export_moneyline_comparison_html <- function(comparison_tbl,
                                             file,
                                             title = "Model vs Market Moneylines",
                                             verbose = TRUE) {
  if (missing(file) || !length(file) || is.na(file)) {
    stop("export_moneyline_comparison_html(): 'file' must be a valid output path.")
  }

  if (!nrow(comparison_tbl)) {
    if (verbose) message("export_moneyline_comparison_html(): comparison table empty; skipping export.")
    return(invisible(NULL))
  }

  dir_path <- dirname(file)
  if (nzchar(dir_path) && dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  display_tbl <- comparison_tbl %>%
    dplyr::transmute(
      Season = season,
      Week = week,
      Date = game_date,
      Matchup = matchup,
      `Model Home Prob` = model_prob,
      `Market Home Prob` = market_prob,
      `Probability Edge` = model_edge_prob,
      `Market Home ML` = market_home_ml,
      `Market Away ML` = market_away_ml,
      `Model Home ML` = model_home_ml,
      `Model Home ML (vig 10%)` = model_home_ml_vig,
      `Model Away ML` = model_away_ml,
      `Model Away ML (vig 10%)` = model_away_ml_vig,
      `Model EV Units` = model_ev_units,
      `Market beat Model?` = dplyr::if_else(market_beats_model, "Yes", "No"),
      Winner = actual_winner
    )

  saved <- FALSE

  if (requireNamespace("gt", quietly = TRUE)) {
    gt_tbl <- display_tbl %>%
      gt::gt() %>%
      gt::fmt_percent(
        columns = c("Model Home Prob", "Market Home Prob", "Probability Edge"),
        decimals = 1
      ) %>%
      gt::fmt_number(
        columns = c(
          "Market Home ML", "Market Away ML",
          "Model Home ML", "Model Home ML (vig 10%)",
          "Model Away ML", "Model Away ML (vig 10%)"
        ),
        decimals = 0,
        drop_trailing_zeros = TRUE,
        use_seps = FALSE
      ) %>%
      gt::fmt_number(columns = "Model EV Units", decimals = 3) %>%
      gt::cols_align(
        align = "center",
        columns = c("Season", "Week", "Market beat Model?")
      ) %>%
      gt::cols_label(
        `Probability Edge` = "Prob Edge",
        `Model EV Units` = "Model EV (units)"
      ) %>%
      gt::tab_header(title = title) %>%
      gt::tab_options(
        table.font.names = c("Source Sans Pro", "Helvetica Neue", "Arial", "sans-serif"),
        table.background.color = "#f9fafb",
        heading.background.color = "#0f172a",
        heading.title.color = "#ecfdf5",
        column_labels.background.color = "#15803d",
        column_labels.font.weight = "bold",
        column_labels.text_transform = "uppercase",
        row.striping.background_color = "#e5e7eb"
      ) %>%
      gt::opt_row_striping() %>%
      gt::data_color(
        columns = "Market beat Model?",
        colors = scales::col_factor(
          palette = c("No" = "#d1d5db", "Yes" = "#166534"),
          domain = c("No", "Yes")
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#14532d"),
          gt::cell_text(color = "#f9fafb", weight = "bold")
        ),
        locations = gt::cells_column_labels(columns = gt::everything())
      )

    try({
      gt::gtsave(gt_tbl, file = file)
      saved <- TRUE
    }, silent = TRUE)
  }

  if (!saved) {
    css_block <- "body {font-family: 'Source Sans Pro', Arial, sans-serif; background-color: #111827; color: #e5e7eb;}\n"
    css_block <- paste0(
      css_block,
      "table {width: 100%; border-collapse: collapse; background-color: #f9fafb; color: #111827;}\n",
      "th {background-color: #15803d; color: #ecfdf5; text-transform: uppercase; letter-spacing: 0.08em;}\n",
      "td, th {padding: 10px 12px; border-bottom: 1px solid #d1d5db; text-align: center;}\n",
      "tr:nth-child(even) {background-color: #e5e7eb;}\n",
      "tr.market-win {background-color: #dcfce7;}\n",
      "caption {caption-side: top; font-size: 1.25rem; font-weight: 600; margin-bottom: 0.75rem; color: #ecfdf5;}\n"
    )

    formatted_tbl <- display_tbl %>%
      dplyr::mutate(
        `Model Home Prob` = scales::percent(`Model Home Prob`, accuracy = 0.1),
        `Market Home Prob` = scales::percent(`Market Home Prob`, accuracy = 0.1),
        `Probability Edge` = scales::percent(`Probability Edge`, accuracy = 0.1),
        `Model EV Units` = format(round(`Model EV Units`, 3), nsmall = 3),
        dplyr::across(
          c(`Market Home ML`, `Market Away ML`, `Model Home ML`, `Model Home ML (vig 10%)`,
            `Model Away ML`, `Model Away ML (vig 10%)`),
          ~ ifelse(is.na(.x), "", format(round(.x, 0), trim = TRUE))
        )
      )

    if (requireNamespace("htmltools", quietly = TRUE)) {
      rows <- purrr::map(
        seq_len(nrow(formatted_tbl)),
        ~ {
          row_vals <- formatted_tbl[.x, , drop = FALSE]
          row_list <- as.list(row_vals)
          row_class <- ifelse(row_list[["Market beat Model?"]] == "Yes", "market-win", "")
          htmltools::tags$tr(
            class = row_class,
            purrr::map(row_list, ~ htmltools::tags$td(.x))
          )
        }
      )

      table_html <- htmltools::tags$table(
        htmltools::tags$caption(title),
        htmltools::tags$thead(htmltools::tags$tr(purrr::map(names(formatted_tbl), htmltools::tags$th))),
        htmltools::tags$tbody(rows)
      )

      doc <- htmltools::tags$html(
        htmltools::tags$head(htmltools::tags$style(css_block)),
        htmltools::tags$body(table_html)
      )

      htmltools::save_html(doc, file = file)
      saved <- TRUE
    } else {
      header <- paste(names(formatted_tbl), collapse = "</th><th>")
      body <- purrr::map_chr(
        seq_len(nrow(formatted_tbl)),
        function(idx) {
          row_vals <- formatted_tbl[idx, , drop = FALSE]
          row_list <- as.list(row_vals)
          row_class <- ifelse(row_list[["Market beat Model?"]] == "Yes", " class=\"market-win\"", "")
          cells <- paste(unlist(row_list, use.names = FALSE), collapse = "</td><td>")
          sprintf("<tr%s><td>%s</td></tr>", row_class, cells)
        }
      )
      html <- paste0(
        "<html><head><style>",
        css_block,
        "</style></head><body><table><caption>",
        title,
        "</caption><thead><tr><th>",
        header,
        "</th></tr></thead><tbody>",
        paste(body, collapse = ""),
        "</tbody></table></body></html>"
      )
      writeLines(html, con = file)
      saved <- TRUE
    }
  }

  if (saved && verbose) {
    message(sprintf("export_moneyline_comparison_html(): wrote HTML report to %s", normalizePath(file, winslash = "/", mustWork = FALSE)))
  }

  invisible(NULL)
}

if (interactive()) {
  message("NFLmarket.R loaded. Use load_latest_market_inputs(), enrich_with_pre_kickoff_espn_lines(), and evaluate_market_vs_blend() as needed.")
}
