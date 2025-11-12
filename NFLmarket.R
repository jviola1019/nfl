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

# NOTE: This helper is also defined in NFLbrier_logloss.R (lines 14-27)
# Intentionally duplicated to avoid external dependencies
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
    emit_safe_join_signal(
      sprintf(
        "%s is missing required key columns: %s; skipping duplicate collapse.",
        label,
        paste(missing_keys, collapse = ", ")
      ),
      label,
      severity = "warn"
    )
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
    emit_safe_join_signal(
      sprintf(
        "%s: duplicates remain for %d key combinations after relaxed collapse.",
        label,
        nrow(dup_check)
      ),
      label,
      severity = "warn"
    )
  }

  out
}

ensure_unique_join_keys <- function(df, keys, label = "data frame") {
  if (is.null(df) || !inherits(df, "data.frame") || !nrow(df) || !length(keys)) {
    return(df)
  }

  missing_keys <- setdiff(keys, names(df))
  if (length(missing_keys)) {
    emit_safe_join_signal(
      sprintf(
        "%s: cannot check join uniqueness because keys are missing: %s",
        label,
        paste(missing_keys, collapse = ", ")
      ),
      label,
      severity = "warn"
    )
    return(df)
  }

  dup_keys <- df %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(keys), ~ !is.na(.))) %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::filter(.data$n > 1L)

  if (!nrow(dup_keys)) {
    return(df)
  }

  emit_safe_join_signal(
    sprintf(
      "%s: detected %d duplicate join key combinations; keeping first occurrence after sorting.",
      label,
      nrow(dup_keys)
    ),
    label,
    severity = "inform"
  )

  df %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)
}

count_duplicate_join_rows <- function(df, keys) {
  if (is.null(df) || !inherits(df, "data.frame") || !nrow(df) || !length(keys)) {
    return(NA_integer_)
  }

  missing_keys <- setdiff(keys, names(df))
  if (length(missing_keys)) {
    return(NA_integer_)
  }

  df %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(keys), ~ !is.na(.))) %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::filter(.data$n > 1L) %>%
    nrow()
}

infer_join_relationship <- function(x, y, keys, label = NULL, default = NULL) {
  result <- list(
    relationship = default,
    x_duplicates = NA_integer_,
    y_duplicates = NA_integer_
  )

  if (!length(keys)) {
    return(result)
  }

  dups_x <- count_duplicate_join_rows(x, keys)
  dups_y <- count_duplicate_join_rows(y, keys)

  result$x_duplicates <- dups_x
  result$y_duplicates <- dups_y

  if (is.na(dups_x) || is.na(dups_y)) {
    return(result)
  }

  relationship <- default
  if (dups_x > 0L && dups_y > 0L) {
    relationship <- "many-to-many"
  } else if (dups_x > 0L) {
    relationship <- "one-to-many"
  } else if (dups_y > 0L) {
    relationship <- "many-to-one"
  } else if (is.null(default)) {
    relationship <- "one-to-one"
  }

  result$relationship <- relationship

  if ((dups_x > 0L || dups_y > 0L) && !is.null(label)) {
    msg <- sprintf(
      "%s: inferred %s join on keys %s (duplicates: x=%d, y=%d).",
      label,
      relationship,
      paste(keys, collapse = ", "),
      dups_x,
      dups_y
    )
    if (exists("emit_safe_join_signal", mode = "function")) {
      emit_safe_join_signal(msg, label, severity = "inform")
    } else {
      message(msg)
    }
  }

  result
}

resolve_join_key_map <- function(by, x, y) {
  dplyr_ns <- asNamespace("dplyr")
  common_by <- NULL
  if (exists("common_by", envir = dplyr_ns, inherits = FALSE)) {
    common_by <- get("common_by", envir = dplyr_ns)
  }

  if (!is.null(common_by)) {
    mapped <- tryCatch(common_by(by, x, y), error = function(e) NULL)
    if (!is.null(mapped) && all(c("x", "y") %in% names(mapped))) {
      return(list(x = mapped$x, y = mapped$y))
    }
  }

  if (is.null(by)) {
    return(list(x = character(), y = character()))
  }

  if (is.character(by)) {
    if (is.null(names(by)) || !length(names(by))) {
      return(list(x = by, y = by))
    }
    return(list(x = names(by), y = unname(by)))
  }

  list(x = character(), y = character())
}

safe_left_join <- function(x, y, by = NULL, relationship = NULL, label = "left_join", ...) {
  join_args <- c(list(x = x, y = y, by = by), list(...))
  has_relationship <- !is.null(relationship) && "relationship" %in% names(formals(dplyr::left_join))
  if (has_relationship) {
    join_args$relationship <- relationship
  }

  join_exec <- function(args) {
    rlang::exec(dplyr::left_join, !!!args)
  }

  tryCatch(
    join_exec(join_args),
    error = function(e) {
      msg <- conditionMessage(e)
      dup_error <- grepl("Each row in `y` must be matched at most once", msg, fixed = TRUE) ||
        grepl("Many-to-many relationship", msg, fixed = TRUE)

      if (!dup_error) {
        stop(e)
      }

      key_map <- resolve_join_key_map(by, x, y)
      y_keys <- key_map$y

      if (!length(y_keys) || !all(y_keys %in% names(y))) {
        warning(sprintf(
          "%s: duplicate join keys detected but unable to determine right-table keys; retrying without relationship enforcement. Original error: %s",
          label,
          msg
        ))
        if (has_relationship) {
          join_args$relationship <- NULL
        }
        return(join_exec(join_args))
      }

      y_dedup <- y %>%
        dplyr::arrange(dplyr::across(dplyr::all_of(y_keys))) %>%
        dplyr::distinct(dplyr::across(dplyr::all_of(y_keys)), .keep_all = TRUE)

      removed <- nrow(y) - nrow(y_dedup)
      warning(sprintf(
        "%s: resolved %d duplicate rows on right join keys after %s; original error: %s",
        label,
        max(removed, 0L),
        label,
        msg
      ))

      join_args$y <- y_dedup
      if (has_relationship) {
        join_args$relationship <- relationship
      }

      join_exec(join_args)
    }
  )
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

ensure_columns_with_defaults <- function(df, defaults) {
  if (is.null(df)) {
    df <- tibble::tibble()
  }
  if (!inherits(df, "data.frame")) {
    df <- tibble::as_tibble(df)
  }
  missing <- setdiff(names(defaults), names(df))
  if (!length(missing)) {
    return(df)
  }
  for (col in missing) {
    df[[col]] <- defaults[[col]]
  }
  df
}

SCORE_DEFAULTS <- list(
  blend_home_prob = NA_real_,
  market_home_prob = NA_real_,
  home_team = NA_character_,
  away_team = NA_character_,
  game_date = as.Date(NA_character_),
  blend_home_median = NA_real_,
  blend_away_median = NA_real_,
  blend_total_median = NA_real_,
  blend_median_margin = NA_real_,
  actual_winner = NA_character_
)

SCHEDULE_DEFAULTS <- list(
  home_team_sched = NA_character_,
  away_team_sched = NA_character_,
  game_date_sched = as.Date(NA_character_),
  market_home_ml_sched = NA_real_,
  market_away_ml_sched = NA_real_,
  blend_home_median_sched = NA_real_,
  blend_away_median_sched = NA_real_,
  blend_total_median_sched = NA_real_,
  market_home_spread_sched = NA_real_,
  market_total_line_sched = NA_real_,
  winner_sched = NA_character_
)

ensure_score_defaults <- function(df) {
  ensure_columns_with_defaults(df, SCORE_DEFAULTS)
}

ensure_schedule_defaults <- function(df) {
  ensure_columns_with_defaults(df, SCHEDULE_DEFAULTS)
}

american_to_probability <- function(odds) {
  odds <- coerce_numeric_safely(odds)
  dplyr::case_when(
    is.na(odds) ~ NA_real_,
    !is.finite(odds) ~ NA_real_,
    odds == 0 ~ NA_real_,
    odds < 0 ~ (-odds) / ((-odds) + 100),
    TRUE ~ 100 / (odds + 100)
  )
}

american_to_decimal <- function(odds) {
  odds <- coerce_numeric_safely(odds)
  dec <- rep(NA_real_, length(odds))
  valid <- is.finite(odds) & odds != 0
  neg_mask <- valid & odds < 0
  pos_mask <- valid & odds > 0
  dec[neg_mask] <- 1 + 100 / abs(odds[neg_mask])
  dec[pos_mask] <- 1 + odds[pos_mask] / 100
  dec[!valid] <- NA_real_
  dec
}

clamp_probability <- function(p, eps = 1e-06) {
  p <- coerce_numeric_safely(p)
  p <- dplyr::if_else(is.na(p), NA_real_, p)
  pmin(pmax(p, eps), 1 - eps)
}

resolve_home_probability <- function(home_prob,
                                     home_ml = NA_real_,
                                     away_ml = NA_real_,
                                     home_spread = NA_real_,
                                     tolerance = 5e-03) {
  home_prob <- suppressWarnings(as.numeric(home_prob))
  ml_home <- coerce_numeric_safely(home_ml)
  ml_away <- coerce_numeric_safely(away_ml)
  spread_home <- coerce_numeric_safely(home_spread)

  needs_prob <- !is.finite(home_prob)
  if (any(needs_prob, na.rm = TRUE)) {
    fallback <- american_to_probability(ml_home)
    fallback <- clamp_probability(fallback)
    replace_mask <- needs_prob & is.finite(fallback)
    if (any(replace_mask, na.rm = TRUE)) {
      home_prob[replace_mask] <- fallback[replace_mask]
    }
  }

  needs_prob <- !is.finite(home_prob)
  if (any(needs_prob, na.rm = TRUE)) {
    fallback <- american_to_probability(ml_away)
    fallback <- clamp_probability(fallback)
    if (length(fallback)) {
      fallback <- 1 - fallback
      replace_mask <- needs_prob & is.finite(fallback)
      if (any(replace_mask, na.rm = TRUE)) {
        home_prob[replace_mask] <- fallback[replace_mask]
      }
    }
  }

  needs_prob <- !is.finite(home_prob)
  if (any(needs_prob, na.rm = TRUE)) {
    fallback <- spread_to_win_probability(spread_home)
    replace_mask <- needs_prob & is.finite(fallback)
    if (any(replace_mask, na.rm = TRUE)) {
      home_prob[replace_mask] <- fallback[replace_mask]
    }
  }

  prob <- clamp_probability(home_prob)

  ml_prob <- american_to_probability(ml_home)
  ml_prob <- clamp_probability(ml_prob)
  reconcile_mask <- is.finite(prob) & is.finite(ml_prob) & abs(prob - ml_prob) > tolerance
  if (any(reconcile_mask, na.rm = TRUE)) {
    prob[reconcile_mask] <- ml_prob[reconcile_mask]
  }

  ml_prob_away <- american_to_probability(ml_away)
  ml_prob_away <- clamp_probability(ml_prob_away)
  if (length(ml_prob_away)) {
    ml_prob_from_away <- clamp_probability(1 - ml_prob_away)
    reconcile_mask <- is.finite(prob) & is.finite(ml_prob_from_away) & abs(prob - ml_prob_from_away) > tolerance
    if (any(reconcile_mask, na.rm = TRUE)) {
      prob[reconcile_mask] <- ml_prob_from_away[reconcile_mask]
    }
  }

  prob
}

harmonize_home_spread <- function(spread, home_prob, tolerance = 5e-03) {
  spread <- coerce_numeric_safely(spread)
  home_prob <- suppressWarnings(as.numeric(home_prob))

  mask <- is.finite(spread) & is.finite(home_prob)
  if (!any(mask)) {
    return(spread)
  }

  prob <- home_prob[mask]
  spr <- spread[mask]
  flip_mask <- (spr < 0 & prob < 0.5 - tolerance) | (spr > 0 & prob > 0.5 + tolerance)
  if (any(flip_mask)) {
    spr[flip_mask] <- -spr[flip_mask]
    spread[mask] <- spr
  }
  spread
}

harmonize_home_margin <- function(margin, home_prob, tolerance = 5e-03) {
  margin <- coerce_numeric_safely(margin)
  home_prob <- suppressWarnings(as.numeric(home_prob))

  mask <- is.finite(margin) & is.finite(home_prob)
  if (!any(mask)) {
    return(margin)
  }

  prob <- home_prob[mask]
  mar <- margin[mask]
  flip_mask <- (mar < 0 & prob > 0.5 + tolerance) | (mar > 0 & prob < 0.5 - tolerance)
  if (any(flip_mask)) {
    mar[flip_mask] <- -mar[flip_mask]
    margin[mask] <- mar
  }
  margin
}

spread_to_win_probability <- function(spread, sigma = 13.86) {  # 13.86 = Historical NFL margin standard deviation
  spread <- coerce_numeric_safely(spread)
  sigma <- coerce_numeric_safely(sigma)
  if (!length(sigma) || !is.finite(sigma[[1L]]) || sigma[[1L]] <= 0) {
    sigma <- 13.86  # Historical NFL margin standard deviation
  } else {
    sigma <- sigma[[1L]]
  }

  implied <- rep(NA_real_, length(spread))
  mask <- is.finite(spread)
  if (any(mask)) {
    implied[mask] <- 1 - stats::pnorm(spread[mask] / sigma)
  }

  clamp_probability(implied)
}

summarize_probability_gap <- function(gap, prefix, tolerance = 0.005) {
  gap <- suppressWarnings(as.numeric(gap))
  if (!length(gap) || !is.finite(gap) || is.na(gap)) {
    return(NA_character_)
  }

  if (abs(gap) <= tolerance) {
    return(NA_character_)
  }

  delta <- round(gap * 100, 1)
  emphasis <- if (abs(gap) >= 0.1) {
    " (!!)"
  } else if (abs(gap) >= 0.05) {
    " (!)"
  } else {
    ""
  }

  sprintf("%s Δ %+.1fpp%s", prefix, delta, emphasis)
}

build_probability_alignment_note <- function(market_gap, blend_gap, tolerance = 0.005) {
  notes <- c(
    summarize_probability_gap(market_gap, "Market", tolerance = tolerance),
    summarize_probability_gap(blend_gap, "Blend", tolerance = tolerance)
  )

  notes <- notes[!is.na(notes) & nzchar(notes)]
  if (!length(notes)) {
    return(NA_character_)
  }

  paste(notes, collapse = " | ")
}

probability_to_american <- function(prob) {
  prob <- clamp_probability(prob)
  dplyr::case_when(
    is.na(prob) ~ NA_real_,
    prob >= 0.5 ~ -round(100 * prob / (1 - prob)),
    TRUE ~ round(100 * (1 - prob) / prob)
  )
}

apply_moneyline_vig <- function(odds, vig = 0.10) {
  odds <- coerce_numeric_safely(odds)
  vig <- coerce_numeric_safely(vig)
  vig[is.na(vig)] <- 0
  dplyr::case_when(
    is.na(odds) ~ NA_real_,
    !is.finite(odds) ~ NA_real_,
    odds == 0 ~ NA_real_,
    odds < 0 ~ -as.numeric(round(abs(odds) * (1 + vig))),
    TRUE ~ as.numeric(round(odds / (1 + vig)))
  )
}

expected_value_units <- function(prob, odds) {
  prob <- clamp_probability(prob)
  dec <- american_to_decimal(odds)
  b <- dec - 1
  out <- prob * b - (1 - prob)
  # Check for invalid values including near-zero b to avoid numerical instability
  invalid <- is.na(prob) | is.na(dec) | !is.finite(dec) | b <= 0 | abs(b) < 1e-6
  out[invalid] <- NA_real_
  out
}

realized_moneyline_units <- function(pick_side, winner_side, odds, stake = 1) {
  pick_side <- tolower(as.character(pick_side))
  winner_side <- tolower(as.character(winner_side))
  odds <- coerce_numeric_safely(odds)
  stake <- coerce_numeric_safely(stake)
  if (!length(stake)) {
    stake <- 1
  }
  stake <- rep(stake, length.out = length(odds))

  out <- rep(NA_real_, length(odds))

  valid <- pick_side %in% c("home", "away") &
    winner_side %in% c("home", "away") &
    is.finite(odds) & odds != 0 &
    is.finite(stake) & !is.na(stake)

  if (!any(valid)) {
    return(out)
  }

  dec <- american_to_decimal(odds[valid])
  stake_valid <- stake[valid]
  dec_invalid <- is.na(dec) | !is.finite(dec)
  wins <- pick_side[valid] == winner_side[valid]

  out_valid <- rep(NA_real_, sum(valid))
  out_valid[dec_invalid] <- NA_real_

  if (any(!dec_invalid & wins)) {
    out_valid[!dec_invalid & wins] <- stake_valid[!dec_invalid & wins] * (dec[!dec_invalid & wins] - 1)
  }

  if (any(!dec_invalid & !wins)) {
    out_valid[!dec_invalid & !wins] <- -stake_valid[!dec_invalid & !wins]
  }

  out[which(valid)] <- out_valid
  out
}

assess_blend_vs_market <- function(
    blend_actual_units,
    market_actual_units,
    blend_ev_units,
    market_ev_units,
    blend_prob_pick,
    market_prob_pick,
    blend_pick_label,
    blend_moneyline,
    market_moneyline,
    blend_edge_moneyline) {
  eps <- sqrt(.Machine$double.eps)

  fmt_units <- function(x) {
    ifelse(is.na(x), "n/a", sprintf("%+.3f", as.numeric(x)))
  }

  fmt_prob <- function(x) {
    ifelse(is.na(x), "n/a", sprintf("%.1f%%", 100 * as.numeric(x)))
  }

  fmt_odds <- function(x) {
    if (is.na(x) || !is.finite(x)) {
      return("n/a")
    }
    odds_int <- as.integer(round(x))
    if (odds_int > 0) {
      sprintf("+%d", odds_int)
    } else {
      sprintf("%d", odds_int)
    }
  }

  safe_label <- if (is.null(blend_pick_label) || is.na(blend_pick_label) || !nzchar(blend_pick_label)) {
    "the blend side"
  } else {
    blend_pick_label
  }

  basis <- NA_character_
  detail <- NA_character_
  result <- NA

  if (!is.na(blend_actual_units) && !is.na(market_actual_units)) {
    diff_units <- blend_actual_units - market_actual_units
    result <- ifelse(diff_units > eps, TRUE, ifelse(diff_units < -eps, FALSE, NA))
    basis <- "Final score"
    detail <- sprintf(
      "Final result: blend %s vs market %s units (diff %+.3f).",
      fmt_units(blend_actual_units),
      fmt_units(market_actual_units),
      diff_units
    )
    return(list(result = result, basis = basis, detail = detail))
  }

  if (!is.na(blend_actual_units)) {
    result <- ifelse(blend_actual_units > eps, TRUE, ifelse(blend_actual_units < -eps, FALSE, NA))
    basis <- "Final score"
    detail <- sprintf(
      "Blend realized %s units; market result unavailable.",
      fmt_units(blend_actual_units)
    )
    return(list(result = result, basis = basis, detail = detail))
  }

  if (!is.na(blend_ev_units)) {
    result <- ifelse(blend_ev_units > eps, TRUE, ifelse(blend_ev_units < -eps, FALSE, NA))
    basis <- "Expected value"
    detail <- sprintf(
      "EV on %s: blend %s units vs market %s.",
      safe_label,
      fmt_units(blend_ev_units),
      fmt_units(market_ev_units)
    )
    if (!is.na(result)) {
      return(list(result = result, basis = basis, detail = detail))
    }
  }

  if (!is.na(blend_prob_pick) && !is.na(market_prob_pick)) {
    prob_diff <- blend_prob_pick - market_prob_pick
    result <- ifelse(prob_diff > eps, TRUE, ifelse(prob_diff < -eps, FALSE, NA))
    basis <- "Win probability"
    detail <- sprintf(
      "%s win probability: blend %s vs market %s.",
      safe_label,
      fmt_prob(blend_prob_pick),
      fmt_prob(market_prob_pick)
    )
    if (!is.na(result)) {
      return(list(result = result, basis = basis, detail = detail))
    }
  }

  if (!is.na(blend_edge_moneyline)) {
    result <- ifelse(blend_edge_moneyline > eps, TRUE, ifelse(blend_edge_moneyline < -eps, FALSE, NA))
    basis <- "Moneyline price"
    detail <- sprintf(
      "Price comparison: market %s vs blend %s.",
      fmt_odds(market_moneyline),
      fmt_odds(blend_moneyline)
    )
    if (!is.na(result)) {
      return(list(result = result, basis = basis, detail = detail))
    }
  }

  list(
    result = NA,
    basis = "Insufficient data",
    detail = "No outcome, EV, probability, or pricing edge available."
  )
}

shorten_market_note <- function(note, max_chars = 72L) {
  if (is.null(note) || !length(note)) {
    return(note)
  }

  note_chr <- as.character(note)
  is_na <- is.na(note_chr)
  note_chr[is_na] <- NA_character_
  to_process <- !is_na & nzchar(trimws(note_chr))

  if (any(to_process)) {
    cleaned <- stringr::str_squish(note_chr[to_process])
    cleaned <- gsub("Final result:", "Result:", cleaned, fixed = TRUE)
    cleaned <- gsub("Blend realized", "Blend", cleaned, fixed = TRUE)
    cleaned <- gsub("Expected value", "EV", cleaned, fixed = TRUE)
    cleaned <- gsub("Win probability", "Win prob", cleaned, fixed = TRUE)
    cleaned <- gsub("Price comparison", "Price", cleaned, fixed = TRUE)
    cleaned <- gsub("market result unavailable", "market n/a", cleaned, fixed = TRUE)
    cleaned <- gsub(" units", "u", cleaned, fixed = TRUE)
    cleaned <- gsub("\u00a0", " ", cleaned, fixed = TRUE)
    cleaned <- gsub("\\s+", " ", cleaned)

    needs_trunc <- stringr::str_length(cleaned) > max_chars
    if (any(needs_trunc)) {
      cleaned[needs_trunc] <- stringr::str_trunc(cleaned[needs_trunc], max_chars, ellipsis = "…")
    }

    note_chr[to_process] <- cleaned
  }

  note_chr
}

devig_two_way_probabilities <- function(p_home_raw, p_away_raw) {
  p_home_raw <- coerce_numeric_safely(p_home_raw)
  p_away_raw <- coerce_numeric_safely(p_away_raw)
  total <- p_home_raw + p_away_raw
  valid <- is.finite(total) & total > 0
  tibble::tibble(
    p_home = dplyr::if_else(valid, p_home_raw / total, NA_real_),
    p_away = dplyr::if_else(valid, p_away_raw / total, NA_real_)
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

  join_args <- list(x = sched_std)

  if (!is.null(espn_lines) && nrow(espn_lines)) {
    espn_std <- standardize_join_keys(espn_lines)

    join_cols <- intersect(join_keys, intersect(names(sched_std), names(espn_std)))
    join_args$y <- espn_std

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
      sched_std <- tryCatch(
        {
          safe_left_join(
            x = sched_std,
            y = join_args$y,
            by = join_args$by,
            relationship = "many-to-many",
            label = "enrich_with_pre_kickoff_espn_lines()",
            multiple = "all"
          )
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
    ),
    espn_final_total = c(
      "espn_final_total", "espn_total_final", "total_final", "total_close",
      "market_total", "total_line", "total", "over_under", "over_under_close",
      "espn_total"
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
  sched_std <- copy_column_if_missing(sched_std, "home_spread", "espn_final_home_spread")
  sched_std <- copy_column_if_missing(sched_std, "market_spread", "spread_line")
  sched_std <- copy_column_if_missing(sched_std, "total_line", "espn_final_total")
  sched_std <- copy_column_if_missing(sched_std, "market_total", "total_line")

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
    dplyr::summarise(p_blend = mean(p_blend, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(.blend_prob = p_blend)

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

  per_game_with_blend <- tryCatch(
    {
      safe_left_join(
        x = base_per_game,
        y = blend_join,
        by = join_keys,
        relationship = "many-to-one",
        label = "build_res_blend()"
      )
    },
    error = function(e) {
      warning(sprintf("build_res_blend(): left_join failed; keeping original probabilities. Reason: %s", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(per_game_with_blend)) {
    return(NULL)
  }

  blend_col <- ".blend_prob"
  if (!blend_col %in% names(per_game_with_blend)) {
    alt_cols <- intersect(c("p_blend", "p_blend.y", "p_blend.x"), names(per_game_with_blend))
    if (length(alt_cols)) {
      per_game_with_blend[[blend_col]] <- per_game_with_blend[[alt_cols[1L]]]
    }
  }

  if (!blend_col %in% names(per_game_with_blend)) {
    if (verbose) message("build_res_blend(): joined table missing p_blend column; skipping blend attachment.")
    return(NULL)
  }

  per_game_with_blend[[prob_col]] <- dplyr::if_else(
    is.finite(per_game_with_blend[[blend_col]]),
    per_game_with_blend[[blend_col]],
    per_game_with_blend[[prob_col]]
  )

  if ("p_blend" %in% base_cols) {
    per_game_with_blend$p_blend <- per_game_with_blend[[blend_col]]
  }

  per_game_with_blend <- dplyr::select(per_game_with_blend, -dplyr::any_of(c(blend_col, "p_blend.x", "p_blend.y")))

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
                                     html_title = "Blend vs Market Moneylines",
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

  comparison
}

extract_game_level_scores <- function(market_comparison_result) {
  if (is.null(market_comparison_result) ||
      !is.list(market_comparison_result) ||
      !"comp" %in% names(market_comparison_result)) {
    return(tibble::tibble())
  }

  comp <- tibble::as_tibble(market_comparison_result$comp)
  if (!nrow(comp)) {
    return(tibble::tibble())
  }

  # NOTE: This helper is also defined in NFLsimulation.R (lines ~495-507)
  # Intentionally duplicated as a local helper to avoid external dependencies
  coalesce_numeric_cols <- function(df, cols) {
    cols <- intersect(cols, names(df))
    if (!length(cols)) {
      return(rep(NA_real_, nrow(df)))
    }
    out <- coerce_numeric_safely(df[[cols[1L]]])
    if (length(cols) > 1L) {
      for (col in cols[-1L]) {
        out <- dplyr::coalesce(out, coerce_numeric_safely(df[[col]]))
      }
    }
    out
  }

  coalesce_character_cols <- function(df, cols) {
    cols <- intersect(cols, names(df))
    if (!length(cols)) {
      return(rep(NA_character_, nrow(df)))
    }
    as_chr <- function(x) {
      if (inherits(x, "Date")) {
        return(as.character(x))
      }
      if (inherits(x, "POSIXt")) {
        return(as.character(as.Date(x)))
      }
      as.character(x)
    }
    out <- as_chr(df[[cols[1L]]])
    out[is.na(out) | !nzchar(out)] <- NA_character_
    if (length(cols) > 1L) {
      for (col in cols[-1L]) {
        candidate <- as_chr(df[[col]])
        candidate[is.na(candidate) | !nzchar(candidate)] <- NA_character_
        out <- dplyr::coalesce(out, candidate)
      }
    }
    out
  }

  coalesce_date_cols <- function(df, cols) {
    cols <- intersect(cols, names(df))
    if (!length(cols)) {
      return(rep(as.Date(NA_character_), nrow(df)))
    }
    as_date <- function(x) {
      if (inherits(x, "Date")) {
        return(x)
      }
      if (inherits(x, "POSIXt")) {
        return(as.Date(x))
      }
      suppressWarnings(as.Date(as.character(x)))
    }
    out <- as_date(df[[cols[1L]]])
    if (length(cols) > 1L) {
      for (col in cols[-1L]) {
        candidate <- as_date(df[[col]])
        out <- dplyr::coalesce(out, candidate)
      }
    }
    out
  }

  blend_home_median <- coalesce_numeric_cols(comp, c(
    "blend_home_median", "home_median_blend", "home_median", "home_median_pts"
  ))
  blend_away_median <- coalesce_numeric_cols(comp, c(
    "blend_away_median", "away_median_blend", "away_median", "away_median_pts"
  ))
  blend_total_median <- coalesce_numeric_cols(comp, c(
    "blend_total_median", "total_median_blend", "total_median", "total_median_pts"
  ))
  blend_median_margin <- coalesce_numeric_cols(comp, c(
    "blend_median_margin", "margin_blend", "margin_median"
  ))

  home_team <- coalesce_character_cols(comp, c(
    "home_team", "home", "home_team_name", "home_abbr"
  ))
  away_team <- coalesce_character_cols(comp, c(
    "away_team", "away", "away_team_name", "away_abbr"
  ))
  matchup <- coalesce_character_cols(comp, c("matchup"))
  actual_winner <- coalesce_character_cols(comp, c("actual_winner", "winner"))
  game_date <- coalesce_date_cols(comp, c("game_date", "date", "gameDate", "match_date"))

  comp_aug <- comp %>%
    dplyr::mutate(
      blend_home_prob = clamp_probability(p_model),
      market_home_prob = clamp_probability(p_mkt),
      # legacy aliases retained for callers expecting `model_prob` / `market_prob`
      model_prob = blend_home_prob,
      market_prob = market_home_prob,
      actual_home_win = y2,
      brier_model = (blend_home_prob - actual_home_win)^2,
      brier_market = (market_home_prob - actual_home_win)^2,
      logloss_model = -(actual_home_win * log(blend_home_prob) +
        (1 - actual_home_win) * log(1 - blend_home_prob)),
      logloss_market = -(actual_home_win * log(market_home_prob) +
        (1 - actual_home_win) * log(1 - market_home_prob)),
      home_team = home_team,
      away_team = away_team,
      matchup = matchup,
      game_date = game_date,
      blend_home_median = blend_home_median,
      blend_away_median = blend_away_median,
      blend_total_median = blend_total_median,
      blend_median_margin = blend_median_margin,
      actual_winner = actual_winner
    )

  comp_aug$blend_median_margin <- dplyr::if_else(
    is.na(comp_aug$blend_median_margin) &
      is.finite(comp_aug$blend_home_median) &
      is.finite(comp_aug$blend_away_median),
    comp_aug$blend_home_median - comp_aug$blend_away_median,
    comp_aug$blend_median_margin
  )

  comp_aug$blend_total_median <- dplyr::if_else(
    is.na(comp_aug$blend_total_median) &
      is.finite(comp_aug$blend_home_median) &
      is.finite(comp_aug$blend_away_median),
    comp_aug$blend_home_median + comp_aug$blend_away_median,
    comp_aug$blend_total_median
  )

  comp_aug %>%
    dplyr::select(
      game_id,
      season,
      week,
      blend_home_prob,
      market_home_prob,
      model_prob,
      market_prob,
      actual_home_win,
      brier_model,
      brier_market,
      logloss_model,
      logloss_market,
      dplyr::any_of(c(
        "home_team",
        "away_team",
        "matchup",
        "game_date",
        "blend_home_median",
        "blend_away_median",
        "blend_total_median",
        "blend_median_margin",
        "actual_winner"
      ))
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

  home_team_col <- select_first_column(schedule_collapsed, c("home_team", "team_home", "home", "home_abbr"))
  away_team_col <- select_first_column(schedule_collapsed, c("away_team", "team_away", "away", "away_abbr"))
  date_col <- select_first_column(schedule_collapsed, c("game_date", "gameDate", "date", "match_date"))
  home_ml_col <- select_first_column(schedule_collapsed, c("espn_final_home_ml", "home_ml", "ml_home", "home_moneyline"))
  away_ml_col <- select_first_column(schedule_collapsed, c("espn_final_away_ml", "away_ml", "ml_away", "away_moneyline"))
  home_median_col <- select_first_column(schedule_collapsed, c("blend_home_median", "home_median_blend", "home_median"))
  away_median_col <- select_first_column(schedule_collapsed, c("blend_away_median", "away_median_blend", "away_median"))
  total_median_col <- select_first_column(schedule_collapsed, c("blend_total_median", "total_median_blend", "total_median"))
  spread_col <- select_first_column(schedule_collapsed, c(
    "espn_final_home_spread", "home_spread", "market_spread", "spread_line", "spread"
  ))
  total_col <- select_first_column(schedule_collapsed, c(
    "espn_final_total", "market_total", "total_line", "total", "over_under"
  ))

  schedule_context <- schedule_collapsed %>%
    dplyr::mutate(
      home_team = as.character(pull_or_default(schedule_collapsed, home_team_col, NA_character_)),
      away_team = as.character(pull_or_default(schedule_collapsed, away_team_col, NA_character_)),
      game_date = suppressWarnings(as.Date(pull_or_default(schedule_collapsed, date_col, NA_character_))),
      market_home_ml = coerce_numeric_safely(pull_or_default(schedule_collapsed, home_ml_col, NA_real_)),
      market_away_ml = coerce_numeric_safely(pull_or_default(schedule_collapsed, away_ml_col, NA_real_)),
      blend_home_median = coerce_numeric_safely(pull_or_default(schedule_collapsed, home_median_col, NA_real_)),
      blend_away_median = coerce_numeric_safely(pull_or_default(schedule_collapsed, away_median_col, NA_real_)),
      blend_total_median = coerce_numeric_safely(pull_or_default(schedule_collapsed, total_median_col, NA_real_)),
      market_home_spread = coerce_numeric_safely(pull_or_default(schedule_collapsed, spread_col, NA_real_)),
      market_total_line = coerce_numeric_safely(pull_or_default(schedule_collapsed, total_col, NA_real_))
    ) %>%
    dplyr::transmute(
      dplyr::across(dplyr::all_of(join_cols)),
      home_team,
      away_team,
      game_date,
      market_home_ml,
      market_away_ml,
      blend_home_median,
      blend_away_median,
      blend_total_median,
      market_home_spread,
      market_total_line
    ) %>%
    dplyr::rename_with(
      ~ paste0(.x, "_sched"),
      -dplyr::all_of(join_cols)
    )

  scores_collapsed <- collapse_by_keys_relaxed(
    scores,
    keys = join_cols,
    label = "Moneyline score table"
  )

  score_defaults <- list(
    blend_home_prob = NA_real_,
    market_home_prob = NA_real_,
    home_team = NA_character_,
    away_team = NA_character_,
    game_date = as.Date(NA_character_),
    blend_home_median = NA_real_,
    blend_away_median = NA_real_,
    blend_total_median = NA_real_,
    blend_median_margin = NA_real_,
    actual_winner = NA_character_
  )

  ensure_score_defaults <- function(df) {
    for (col in names(score_defaults)) {
      if (!col %in% names(df)) {
        df[[col]] <- score_defaults[[col]]
      }
    }
    df
  }

  schedule_defaults <- list(
    home_team_sched = NA_character_,
    away_team_sched = NA_character_,
    game_date_sched = as.Date(NA_character_),
    market_home_ml_sched = NA_real_,
    market_away_ml_sched = NA_real_,
    blend_home_median_sched = NA_real_,
    blend_away_median_sched = NA_real_,
    blend_total_median_sched = NA_real_,
    market_home_spread_sched = NA_real_,
    market_total_line_sched = NA_real_,
    winner_sched = NA_character_
  )

  ensure_schedule_defaults <- function(df) {
    for (col in names(schedule_defaults)) {
      if (!col %in% names(df)) {
        df[[col]] <- schedule_defaults[[col]]
      }
    }
    df
  }

  scores_ready <- scores_collapsed %>%
    ensure_score_defaults() %>%
    dplyr::mutate(
      blend_home_prob = dplyr::coalesce(blend_home_prob, model_prob),
      market_home_prob = dplyr::coalesce(market_home_prob, market_prob)
    )

  scores_ready <- ensure_unique_join_keys(
    scores_ready,
    keys = join_cols,
    label = "Moneyline score table (post-defaults)"
  )

  schedule_context <- ensure_unique_join_keys(
    schedule_context,
    keys = join_cols,
    label = "Schedule context table"
  )

  join_args <- list(x = scores_ready, y = schedule_context, by = join_cols)
  join_meta <- infer_join_relationship(
    scores_ready,
    schedule_context,
    join_cols,
    label = "build_moneyline_comparison_table()",
    default = "many-to-one"
  )
  if ("relationship" %in% names(formals(dplyr::inner_join))) {
    if (!is.null(join_meta$relationship)) {
      join_args$relationship <- join_meta$relationship
    }
    if ("multiple" %in% names(formals(dplyr::inner_join)) &&
        (isTRUE(join_meta$x_duplicates > 0L) || isTRUE(join_meta$y_duplicates > 0L))) {
      join_args$multiple <- "all"
    }
  }

  combined <- tryCatch(
    rlang::exec(dplyr::inner_join, !!!join_args),
    error = function(e) {
      warning(sprintf(
        "build_moneyline_comparison_table(): strict inner_join failed (%s); falling back to distinct join keys.",
        conditionMessage(e)
      ))
      fallback_scores <- scores_ready %>%
        dplyr::distinct(dplyr::across(dplyr::all_of(join_cols)), .keep_all = TRUE)
      fallback_sched <- schedule_context %>%
        dplyr::distinct(dplyr::across(dplyr::all_of(join_cols)), .keep_all = TRUE)
      fallback_args <- join_args
      fallback_args$x <- fallback_scores
      fallback_args$y <- fallback_sched
      rlang::exec(dplyr::inner_join, !!!fallback_args)
    }
  ) %>%
    ensure_schedule_defaults() %>%
    dplyr::mutate(
      market_home_ml = market_home_ml_sched,
      market_away_ml = market_away_ml_sched,
      market_home_spread = market_home_spread_sched,
      market_total_line = market_total_line_sched,
      home_team = dplyr::coalesce(home_team, home_team_sched),
      away_team = dplyr::coalesce(away_team, away_team_sched),
      game_date = dplyr::coalesce(game_date, game_date_sched),
      blend_home_median = dplyr::coalesce(blend_home_median, blend_home_median_sched),
      blend_away_median = dplyr::coalesce(blend_away_median, blend_away_median_sched),
      blend_total_median = dplyr::coalesce(
        blend_total_median,
        blend_total_median_sched,
        dplyr::if_else(
          is.finite(blend_home_median) & is.finite(blend_away_median),
          blend_home_median + blend_away_median,
          NA_real_
        )
      ),
      blend_median_margin = dplyr::coalesce(
        blend_median_margin,
        dplyr::if_else(
          is.finite(blend_home_median) & is.finite(blend_away_median),
          blend_home_median - blend_away_median,
          NA_real_
        )
      ),
      actual_winner = dplyr::coalesce(actual_winner, winner_sched)
    ) %>%
    dplyr::select(-dplyr::ends_with("_sched")) %>%
    dplyr::mutate(
      game_date = suppressWarnings(as.Date(game_date)),
      matchup = dplyr::if_else(
        is.na(home_team) | is.na(away_team),
        NA_character_,
        paste(away_team, "@", home_team)
      ),
      blend_home_prob = clamp_probability(blend_home_prob),
      blend_away_prob = clamp_probability(1 - blend_home_prob),
      market_home_spread = coerce_numeric_safely(market_home_spread),
      market_home_prob = resolve_home_probability(
        market_home_prob,
        market_home_ml,
        market_away_ml,
        market_home_spread
      ),
      market_away_prob = clamp_probability(1 - market_home_prob),
      market_home_spread = harmonize_home_spread(market_home_spread, market_home_prob),
      market_total_line = coerce_numeric_safely(market_total_line),
      blend_median_margin = dplyr::if_else(
        is.na(blend_home_median) | is.na(blend_away_median),
        NA_real_,
        blend_home_median - blend_away_median
      ),
      blend_median_margin = harmonize_home_margin(blend_median_margin, blend_home_prob),
      blend_spread_equiv = dplyr::if_else(
        is.na(blend_median_margin),
        NA_real_,
        -blend_median_margin
      ),
      market_spread_win_prob = spread_to_win_probability(market_home_spread),
      blend_spread_win_prob = spread_to_win_probability(blend_spread_equiv),
      market_spread_win_prob_away = clamp_probability(1 - market_spread_win_prob),
      blend_spread_win_prob_away = clamp_probability(1 - blend_spread_win_prob),
      market_prob_spread_gap_home = market_home_prob - market_spread_win_prob,
      market_prob_spread_gap_away = market_away_prob - market_spread_win_prob_away,
      blend_prob_spread_gap_home = blend_home_prob - blend_spread_win_prob,
      blend_prob_spread_gap_away = blend_away_prob - blend_spread_win_prob_away,
      market_implied_margin = dplyr::if_else(
        is.na(market_home_spread),
        NA_real_,
        -market_home_spread
      ),
      market_total = market_total_line,
      market_home_ml = dplyr::if_else(
        is.na(market_home_ml),
        probability_to_american(market_home_prob),
        market_home_ml
      ),
      market_away_ml = dplyr::if_else(
        is.na(market_away_ml),
        probability_to_american(market_away_prob),
        market_away_ml
      ),
      blend_home_ml = probability_to_american(blend_home_prob),
      blend_home_ml_vig = apply_moneyline_vig(blend_home_ml, vig = vig),
      blend_away_ml = probability_to_american(blend_away_prob),
      blend_away_ml_vig = apply_moneyline_vig(blend_away_ml, vig = vig),
      blend_edge_prob_home = blend_home_prob - market_home_prob,
      blend_edge_prob_away = blend_away_prob - market_away_prob,
      blend_ev_units_home = expected_value_units(blend_home_prob, market_home_ml),
      blend_ev_units_away = expected_value_units(blend_away_prob, market_away_ml),
      blend_favorite_side = dplyr::if_else(blend_home_prob >= blend_away_prob, "home", "away"),
      blend_favorite = dplyr::if_else(blend_favorite_side == "home", home_team, away_team),
      blend_best_ev = dplyr::case_when(
        !is.na(blend_ev_units_home) | !is.na(blend_ev_units_away) ~ pmax(
          dplyr::coalesce(blend_ev_units_home, -Inf),
          dplyr::coalesce(blend_ev_units_away, -Inf)
        ),
        TRUE ~ NA_real_
      ),
      blend_best_ev = dplyr::if_else(
        is.na(blend_best_ev) | blend_best_ev == -Inf,
        NA_real_,
        blend_best_ev
      ),
      blend_ev_units = blend_best_ev,
      blend_pick_side = dplyr::case_when(
        is.na(blend_best_ev) ~ NA_character_,
        blend_best_ev <= 0 ~ NA_character_,
        dplyr::near(blend_best_ev, blend_ev_units_home) ~ "home",
        dplyr::near(blend_best_ev, blend_ev_units_away) ~ "away",
        TRUE ~ NA_character_
      ),
      blend_pick_side = dplyr::if_else(
        is.na(blend_pick_side) & !is.na(blend_best_ev) & blend_best_ev > 0,
        dplyr::if_else(
          dplyr::coalesce(blend_ev_units_home, -Inf) >= dplyr::coalesce(blend_ev_units_away, -Inf),
          "home",
          "away"
        ),
        blend_pick_side
      ),
      blend_pick = dplyr::case_when(
        blend_pick_side == "home" ~ home_team,
        blend_pick_side == "away" ~ away_team,
        TRUE ~ NA_character_
      ),
      blend_prob_pick = dplyr::case_when(
        blend_pick_side == "home" ~ blend_home_prob,
        blend_pick_side == "away" ~ blend_away_prob,
        TRUE ~ NA_real_
      ),
      blend_moneyline = dplyr::case_when(
        blend_pick_side == "home" ~ blend_home_ml,
        blend_pick_side == "away" ~ blend_away_ml,
        TRUE ~ NA_real_
      ),
      blend_moneyline_vig = dplyr::case_when(
        blend_pick_side == "home" ~ blend_home_ml_vig,
        blend_pick_side == "away" ~ blend_away_ml_vig,
        TRUE ~ NA_real_
      ),
      market_moneyline = dplyr::case_when(
        blend_pick_side == "home" ~ market_home_ml,
        blend_pick_side == "away" ~ market_away_ml,
        TRUE ~ NA_real_
      ),
      market_moneyline = dplyr::if_else(
        is.na(market_moneyline) & !is.na(blend_pick_side),
        probability_to_american(
          dplyr::case_when(
            blend_pick_side == "home" ~ market_home_prob,
            blend_pick_side == "away" ~ market_away_prob,
            TRUE ~ NA_real_
          )
        ),
        market_moneyline
      ),
      market_prob_pick = dplyr::case_when(
        blend_pick_side == "home" ~ dplyr::coalesce(
          market_home_prob,
          american_to_probability(market_home_ml)
        ),
        blend_pick_side == "away" ~ dplyr::coalesce(
          market_away_prob,
          american_to_probability(market_away_ml)
        ),
        TRUE ~ NA_real_
      ),
      market_prob_pick = clamp_probability(market_prob_pick),
      blend_prob_fav = dplyr::case_when(
        is.na(blend_home_prob) & is.na(blend_away_prob) ~ NA_real_,
        blend_home_prob >= blend_away_prob ~ blend_home_prob,
        TRUE ~ blend_away_prob
      ),
      market_prob_fav = dplyr::case_when(
        is.na(market_home_prob) & is.na(market_away_prob) ~ NA_real_,
        market_home_prob >= market_away_prob ~ market_home_prob,
        TRUE ~ market_away_prob
      ),
      market_pick_side = dplyr::case_when(
        is.na(market_home_prob) ~ NA_character_,
        market_home_prob >= market_away_prob ~ "home",
        TRUE ~ "away"
      ),
      market_pick = dplyr::case_when(
        market_pick_side == "home" ~ home_team,
        market_pick_side == "away" ~ away_team,
        TRUE ~ NA_character_
      ),
      market_pick_moneyline = dplyr::case_when(
        market_pick_side == "home" ~ market_home_ml,
        market_pick_side == "away" ~ market_away_ml,
        TRUE ~ NA_real_
      ),
      actual_winner_side = dplyr::case_when(
        is.na(actual_home_win) ~ NA_character_,
        actual_home_win == 1L ~ "home",
        actual_home_win == 0L ~ "away",
        TRUE ~ NA_character_
      ),
      blend_actual_units = realized_moneyline_units(
        pick_side = blend_pick_side,
        winner_side = actual_winner_side,
        odds = market_moneyline
      ),
      market_actual_units = realized_moneyline_units(
        pick_side = market_pick_side,
        winner_side = actual_winner_side,
        odds = market_pick_moneyline
      ),
      blend_edge_prob = dplyr::case_when(
        is.na(blend_prob_pick) | is.na(market_prob_pick) ~ NA_real_,
        TRUE ~ blend_prob_pick - market_prob_pick
      ),
      blend_edge_moneyline = dplyr::case_when(
        is.na(blend_pick_side) ~ NA_real_,
        blend_pick_side == "home" ~ market_home_ml - blend_home_ml,
        blend_pick_side == "away" ~ market_away_ml - blend_away_ml,
        TRUE ~ NA_real_
      ),
      market_ev_units = expected_value_units(market_prob_pick, market_moneyline),
      blend_beats_market_assessment = purrr::pmap(
        list(
          blend_actual_units = blend_actual_units,
          market_actual_units = market_actual_units,
          blend_ev_units = blend_ev_units,
          market_ev_units = market_ev_units,
          blend_prob_pick = blend_prob_pick,
          market_prob_pick = market_prob_pick,
          blend_pick_label = blend_pick,
          blend_moneyline = blend_moneyline,
          market_moneyline = market_moneyline,
          blend_edge_moneyline = blend_edge_moneyline
        ),
        assess_blend_vs_market
      ),
      blend_beats_market = purrr::map_lgl(
        blend_beats_market_assessment,
        ~ {
          res <- .x$result
          if (is.na(res)) NA else as.logical(res)
        }
      ),
      blend_beats_market_basis = purrr::map_chr(
        blend_beats_market_assessment,
        ~ .x$basis
      ),
      blend_beats_market_note_raw = purrr::map_chr(
        blend_beats_market_assessment,
        ~ .x$detail
      ),
      market_winning = {
        realized_market <- dplyr::case_when(
          is.na(market_actual_units) ~ NA,
          market_actual_units > 0 ~ TRUE,
          market_actual_units < 0 ~ FALSE,
          TRUE ~ NA
        )
        dplyr::coalesce(
          realized_market,
          dplyr::case_when(
            is.na(market_ev_units) ~ NA,
            market_ev_units > 0 ~ TRUE,
            TRUE ~ FALSE
          )
        )
      },
      blend_recommendation = dplyr::case_when(
        is.na(blend_ev_units) ~ "No Play",
        blend_ev_units <= 0 ~ "Pass",
        is.na(blend_pick) | !nzchar(blend_pick) ~ "Bet moneyline",
        TRUE ~ paste("Bet", blend_pick, "moneyline")
      ),
      blend_prob_pick_spread_prob = dplyr::case_when(
        blend_pick_side == "home" ~ blend_spread_win_prob,
        blend_pick_side == "away" ~ blend_spread_win_prob_away,
        TRUE ~ NA_real_
      ),
      market_prob_pick_spread_prob = dplyr::case_when(
        blend_pick_side == "home" ~ market_spread_win_prob,
        blend_pick_side == "away" ~ market_spread_win_prob_away,
        TRUE ~ NA_real_
      ),
      blend_prob_spread_gap_pick = dplyr::case_when(
        blend_pick_side == "home" ~ blend_prob_spread_gap_home,
        blend_pick_side == "away" ~ blend_prob_spread_gap_away,
        TRUE ~ NA_real_
      ),
      market_prob_spread_gap_pick = dplyr::case_when(
        blend_pick_side == "home" ~ market_prob_spread_gap_home,
        blend_pick_side == "away" ~ market_prob_spread_gap_away,
        TRUE ~ NA_real_
      ),
      probability_alignment_note = purrr::map2_chr(
        market_prob_spread_gap_pick,
        blend_prob_spread_gap_pick,
        ~ build_probability_alignment_note(.x, .y)
      ),
      probability_alignment_note = dplyr::if_else(
        is.na(blend_pick_side) | blend_recommendation %in% c("Pass", "No Play"),
        NA_character_,
        probability_alignment_note
      ),
      blend_beats_market_note = dplyr::case_when(
        is.na(probability_alignment_note) ~ blend_beats_market_note_raw,
        is.na(blend_beats_market_note_raw) ~ probability_alignment_note,
        !nzchar(blend_beats_market_note_raw) ~ probability_alignment_note,
        TRUE ~ paste(blend_beats_market_note_raw, probability_alignment_note, sep = " | ")
      ),
      blend_beats_market_note = shorten_market_note(blend_beats_market_note),
      blend_kelly_fraction = dplyr::case_when(
        is.na(blend_pick_side) ~ NA_real_,
        TRUE ~ {
          dec <- american_to_decimal(market_moneyline)
          b <- dec - 1
          stake <- (blend_prob_pick * b - (1 - blend_prob_pick)) / b
          # Check for invalid values including near-zero b to avoid numerical instability
          invalid <- is.na(dec) | !is.finite(dec) | b <= 0 | abs(b) < 1e-6 | is.na(stake)
          stake[invalid] <- NA_real_
          stake
        }
      ),
      blend_confidence = dplyr::case_when(
        is.na(blend_kelly_fraction) ~ NA_real_,
        blend_kelly_fraction < 0 ~ 0,
        TRUE ~ blend_kelly_fraction
      ),
      actual_winner = dplyr::case_when(
        is.na(actual_home_win) ~ actual_winner,
        actual_home_win == 1L  ~ home_team,
        actual_home_win == 0L  ~ away_team,
        TRUE ~ actual_winner
      ),
      blend_home_median = coerce_numeric_safely(blend_home_median),
      blend_away_median = coerce_numeric_safely(blend_away_median),
      blend_total_median = coerce_numeric_safely(blend_total_median),
      # legacy aliases preserved for downstream code expecting model_* naming
      model_prob = blend_home_prob,
      market_prob = market_home_prob,
      model_home_ml = blend_home_ml,
      model_home_ml_vig = blend_home_ml_vig,
      model_away_ml = blend_away_ml,
      model_away_ml_vig = blend_away_ml_vig,
      model_edge_prob = blend_edge_prob_home,
      model_ev_units = blend_ev_units_home,
      market_beats_model = brier_market < brier_model
    ) %>%
    dplyr::select(-dplyr::any_of(c(
      "blend_best_ev",
      "blend_spread_equiv",
      "blend_prob_pick_spread_prob",
      "market_prob_pick_spread_prob",
      "probability_alignment_note",
      "blend_beats_market_note_raw"
    ))) %>%
    dplyr::arrange(season, week, game_date, matchup)

  combined
}

export_moneyline_comparison_html <- function(comparison_tbl,
                                             file = NULL,
                                             title = "Blend vs Market Moneylines",
                                             verbose = TRUE,
                                             auto_open = TRUE,
                                             season = NULL,
                                             week = NULL) {
  if (missing(file) || is.null(file) || !length(file) || all(is.na(file)) || !nzchar(file[[1L]])) {
    file <- file.path(getwd(), "NFLvsmarket_report.html")
    if (verbose) {
      message(sprintf(
        "export_moneyline_comparison_html(): using default output path %s",
        file.path(normalizePath(dirname(file), winslash = "/", mustWork = FALSE), basename(file))
      ))
    }
  } else {
    file <- file[[1L]]
  }

  if (!nzchar(file)) {
    stop("export_moneyline_comparison_html(): 'file' must resolve to a non-empty path.")
  }

  if (!nrow(comparison_tbl)) {
    if (verbose) message("export_moneyline_comparison_html(): comparison table empty; skipping export.")
    return(invisible(file))
  }

  season_filter <- if (!length(season)) {
    get0("SEASON", ifnotfound = NULL, inherits = TRUE)
  } else {
    season
  }
  week_filter <- if (!length(week)) {
    get0("WEEK_TO_SIM", ifnotfound = NULL, inherits = TRUE)
  } else {
    week
  }

  filtered_tbl <- comparison_tbl
  if (!is.null(season_filter) && length(season_filter) && "season" %in% names(filtered_tbl)) {
    filtered_tbl <- dplyr::filter(filtered_tbl, .data$season %in% season_filter)
  }
  if (!is.null(week_filter) && length(week_filter) && "week" %in% names(filtered_tbl)) {
    filtered_tbl <- dplyr::filter(filtered_tbl, .data$week %in% week_filter)
  }

  if (nrow(filtered_tbl)) {
    comparison_tbl <- filtered_tbl
  } else if (verbose) {
    message(
      "export_moneyline_comparison_html(): no rows matched the requested season/week filter; exporting unfiltered table."
    )
  }

  dir_path <- dirname(file)
  if (nzchar(dir_path) && dir_path != "." && !dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  if (file.exists(file)) {
    unlink(file, force = TRUE)
  }

  moneyline_cols <- c(
    "Blend Home Moneyline (vig)",
    "Blend Away Moneyline (vig)",
    "Market Home Moneyline",
    "Market Away Moneyline"
  )

  format_signed_spread <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    out <- rep("", length(x))
    mask <- !is.na(num) & is.finite(num)
    if (any(mask)) {
      out[mask] <- sprintf("%+.1f", round(num[mask], 1))
    }
    out
  }

  format_moneyline_strings <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    out <- rep("", length(x))
    mask <- !is.na(num) & is.finite(num)
    if (any(mask)) {
      out[mask] <- sprintf("%+d", as.integer(round(num[mask])))
    }
    out
  }

  format_probability_leader <- function(team, prob) {
    team <- as.character(team)
    prob <- suppressWarnings(as.numeric(prob))
    out <- rep(NA_character_, length(prob))
    valid <- !is.na(prob) & is.finite(prob) & !is.na(team) & nzchar(team)
    if (any(valid)) {
      out[valid] <- sprintf("%s %.1f%%", team[valid], round(prob[valid] * 100, 1))
    }
    out
  }

  intro_html <- paste0(
    "<section class=\"report-intro\">",
    "<h2>📊 NFL Blend vs Market Analysis Report</h2>",
    "<p class=\"report-subtitle\">Comprehensive comparison of blended model predictions against betting market consensus</p>",

    "<div class=\"intro-section\" style=\"background: rgba(22,101,52,0.2); border-left: 4px solid #22c55e;\">",
    "<h3>✅ CALCULATION ERRORS FIXED</h3>",
    "<p><strong>Previous Issue:</strong> The table was incorrectly showing probability differences as \"Blend Edge\" instead of true expected value edge.</p>",
    "<p><strong>Fixed:</strong></p>",
    "<ul>",
    "<li>✅ <strong>EV Edge (%)</strong> now correctly calculated as: (Blend Probability × Decimal Odds) - 1</li>",
    "<li>✅ <strong>Total EV (Units)</strong> now correctly calculated as: Stake × EV Edge</li>",
    "<li>✅ <strong>Prob Advantage (pp)</strong> added as separate column for reference</li>",
    "</ul>",
    "<p style=\"color: #fbbf24; font-weight: 600;\">⚠️ DO NOT use any reports generated before this fix for betting decisions!</p>",
    "</div>",

    "<div class=\"intro-section\">",
    "<h3>🎯 Understanding the Blend</h3>",
    "<p>This report compares a <strong>blended probabilistic model</strong> (combining multiple prediction sources) against the <strong>betting market consensus</strong> (derived from moneylines and spreads). Each row represents one NFL game with detailed analytics.</p>",
    "</div>",

    "<div class=\"intro-section critical-concept\">",
    "<h3>⚡ CRITICAL: What Does \"Blend Beat Market?\" Mean?</h3>",
    "<p class=\"emphasis\">This is <strong>NOT</strong> about which team is favored to win!</p>",
    "<p>\"Blend Beat Market?\" compares <strong>which assessment is better</strong>, not which team wins. Examples:</p>",
    "<ul class=\"examples-list\">",
    "<li><strong>Scenario 1:</strong> Both favor Team A, but Market has Team A at 78% and Blend has 75%<br/>",
    "→ <span class=\"result-no\">Market beats Blend</span> (market is more confident in the correct side)</li>",
    "<li><strong>Scenario 2:</strong> Market favors Team A at 60%, Blend favors Team B at 55%, Team B wins<br/>",
    "→ <span class=\"result-yes\">Blend beats Market</span> (blend picked the actual winner)</li>",
    "<li><strong>Scenario 3:</strong> Blend has higher EV (+0.15 units) than Market (-0.05 units) on same side<br/>",
    "→ <span class=\"result-yes\">Blend beats Market</span> (better expected value)</li>",
    "</ul>",
    "<p class=\"key-point\">🔑 <strong>Key Point:</strong> A favorite can lose to the market if the market assigns an even <em>higher</em> probability to that same favorite!</p>",
    "</div>",

    "<div class=\"intro-section\">",
    "<h3>📈 Key Metrics Explained</h3>",
    "<ul class=\"metrics-list\">",
    "<li><span class=\"metric-icon\">💡</span> <strong>Blend Pick:</strong> The team the blended model favors to win.</li>",
    "<li><span class=\"metric-icon\">🎲</span> <strong>Blend Recommendation:</strong> Suggested betting action based on positive expected value (EV > 0).</li>",
    "<li><span class=\"metric-icon\">✅</span> <strong>Blend Beat Market?:</strong> Did the blend's assessment outperform the market's?",
    "<ul class=\"basis-list\">",
    "<li>For completed games: Compares <em>actual money won/lost</em> by each side's pick</li>",
    "<li>For upcoming games: Compares <em>expected value, win probability, or pricing</em></li>",
    "</ul></li>",
    "<li><span class=\"metric-icon\">📊</span> <strong>Basis:</strong> How we determined the winner:",
    "<ul class=\"basis-list\">",
    "<li><strong>Final score</strong> — Most reliable: actual results determine which assessment was better</li>",
    "<li><strong>Expected value</strong> — Blend has higher EV on its pick vs market's pick</li>",
    "<li><strong>Win probability</strong> — Blend assigns higher win % to its pick vs market's pick</li>",
    "<li><strong>Moneyline price</strong> — Blend offers better pricing for the same pick</li>",
    "</ul></li>",
    "<li><span class=\"metric-icon\">📈</span> <strong>EV Edge (%):</strong> Expected return per dollar bet, calculated as (Blend Probability × Decimal Odds) - 1. This is the TRUE betting edge. Example: 10% edge means you expect to profit $0.10 for every $1 bet. <em>Color coded: green (positive edge), red (negative edge).</em></li>",
    "<li><span class=\"metric-icon\">💰</span> <strong>Total EV (Units):</strong> Total expected profit = Stake × EV Edge. This is your actual expected profit in units for the recommended bet size. <em>Color coded: green (profitable), red (unprofitable).</em></li>",
    "<li><span class=\"metric-icon\">📊</span> <strong>Prob Advantage (pp):</strong> Simple probability difference (in percentage points) between blend and market assessments. This shows confidence differential but is NOT the same as betting edge. For reference only.</li>",
    "<li><span class=\"metric-icon\">🎯</span> <strong>Probabilities (Blend/Market Home Win %):</strong> Win probability for the home team according to blend model vs market. <em>Color intensity shows confidence level.</em></li>",
    "<li><span class=\"metric-icon\">🏈</span> <strong>Spreads:</strong> All spreads shown from home team's perspective:",
    "<ul class=\"basis-list\">",
    "<li><strong>Market Home Spread</strong> — Betting line from sportsbooks (+ = underdog, − = favorite)</li>",
    "<li><strong>Blend Median Margin</strong> — Blend's predicted point differential (negative spread equivalent)</li>",
    "<li><em>Color coded: red (home underdog), green (home favorite)</em></li>",
    "</ul></li>",
    "<li><span class=\"metric-icon\">💵</span> <strong>Moneylines:</strong> American odds format (e.g., +150 = win $150 on $100 bet, -150 = bet $150 to win $100).</li>",
    "</ul>",
    "</div>",

    "<div class=\"intro-section\">",
    "<h3>🎨 Color Coding</h3>",
    "<ul class=\"color-guide\">",
    "<li><span class=\"color-box\" style=\"background:#166534;\"></span> <strong>Green:</strong> Blend beats market (better assessment)</li>",
    "<li><span class=\"color-box\" style=\"background:#991B1B;\"></span> <strong>Red:</strong> Market beats blend (market's assessment was better)</li>",
    "<li><span class=\"color-box\" style=\"background:#1D4ED8;\"></span> <strong>Blue:</strong> Recommended bet action</li>",
    "<li><span class=\"color-box\" style=\"background:#94a3b8;\"></span> <strong>Gray:</strong> N/A or insufficient data</li>",
    "</ul>",
    "</div>",
    "</section>"
  )

  display_tbl <- comparison_tbl %>%
    dplyr::transmute(
      Season = season,
      Week = week,
      Date = game_date,
      Matchup = matchup,
      Winner = dplyr::coalesce(actual_winner, "TBD"),
      `Blend Pick` = blend_pick,
      `Blend Recommendation` = blend_recommendation,
      `Blend Beat Market?` = dplyr::case_when(
        is.na(blend_beats_market) ~ "N/A",
        blend_beats_market ~ "Yes",
        TRUE ~ "No"
      ),
      `Blend Beat Market Basis` = blend_beats_market_basis,
      `Blend Stake (Units)` = blend_confidence,
      `EV Edge (%)` = blend_ev_units,  # FIXED: Was blend_edge_prob (probability diff), now shows actual EV edge
      `Total EV (Units)` = dplyr::if_else(
        is.na(blend_confidence) | is.na(blend_ev_units),
        NA_real_,
        blend_confidence * blend_ev_units  # FIXED: Total expected profit = Stake × Edge
      ),
      `Prob Advantage (pp)` = blend_edge_prob,  # Probability difference for reference
      `Blend Home Win %` = blend_home_prob,
      `Market Home Win %` = market_home_prob,
      `Blend Median Margin` = blend_median_margin,
      `Market Home Spread` = market_home_spread,
      `Market Total` = market_total,
      `Market Home Moneyline` = market_home_ml,
      `Market Away Moneyline` = market_away_ml
    )

  saved <- FALSE

  if (requireNamespace("gt", quietly = TRUE)) {
    display_cols <- names(display_tbl)

    gt_apply_if_columns <- function(gt_tbl, columns, fn, ...) {
      existing <- intersect(columns, display_cols)
      if (!length(existing)) {
        return(gt_tbl)
      }
      args <- c(list(gt_tbl, columns = dplyr::all_of(existing)), list(...))
      do.call(fn, args)
    }

    gt_apply_labels <- function(gt_tbl, label_map) {
      label_map <- label_map[names(label_map) %in% display_cols]
      if (!length(label_map)) {
        return(gt_tbl)
      }
      do.call(gt::cols_label, c(list(gt_tbl), as.list(label_map)))
    }

    gt_tbl <- gt::gt(display_tbl)
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c(
        "EV Edge (%)",
        "Prob Advantage (pp)",
        "Blend Home Win %", "Market Home Win %"
      ),
      gt::fmt_percent,
      decimals = 1
    )
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c("Blend Median Margin", "Market Home Spread"),
      gt::fmt,
      fns = function(x) format_signed_spread(x)
    )
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c("Market Total"),
      gt::fmt_number,
      decimals = 1,
      drop_trailing_zeros = TRUE
    )
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c("Total EV (Units)", "Blend Stake (Units)"),
      gt::fmt_number,
      decimals = 3,
      drop_trailing_zeros = FALSE
    )
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c("Market Home Moneyline", "Market Away Moneyline"),
      gt::fmt,
      fns = function(x) format_moneyline_strings(x)
    )
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c(
        "Matchup", "Winner",
        "Blend Pick", "Blend Recommendation", "Blend Beat Market Basis"
      ),
      gt::cols_align,
      align = "left"
    )
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c("Season", "Week", "Blend Beat Market?"),
      gt::cols_align,
      align = "center"
    )

    # Add visual data bars for probability columns
    if (all(c("Blend Home Win %", "Market Home Win %") %in% display_cols)) {
      gt_tbl <- tryCatch({
        gt::data_color(
          gt_tbl,
          columns = c("Blend Home Win %", "Market Home Win %"),
          colors = scales::col_numeric(
            palette = c("#1e3a8a", "#2563eb", "#3b82f6", "#60a5fa", "#93c5fd"),
            domain = c(0, 1),
            na.color = "#374151"
          )
        )
      }, error = function(e) gt_tbl)
    }

    # Add color coding for Total EV column (green for positive, red for negative)
    if ("Total EV (Units)" %in% display_cols) {
      gt_tbl <- tryCatch({
        gt::data_color(
          gt_tbl,
          columns = "Total EV (Units)",
          colors = scales::col_numeric(
            palette = c("#991B1B", "#dc2626", "#1f2937", "#15803d", "#166534"),
            domain = c(-0.05, 0.05),  # Adjusted for total EV (smaller values)
            na.color = "#374151"
          )
        )
      }, error = function(e) gt_tbl)
    }

    # Add color coding for EV Edge column (green for positive, red for negative)
    if ("EV Edge (%)" %in% display_cols) {
      gt_tbl <- tryCatch({
        gt::data_color(
          gt_tbl,
          columns = "EV Edge (%)",
          colors = scales::col_numeric(
            palette = c("#991B1B", "#dc2626", "#1f2937", "#15803d", "#166534"),
            domain = c(-0.15, 0.15),
            na.color = "#374151"
          )
        )
      }, error = function(e) gt_tbl)
    }

    # Add subtle highlighting for Prob Advantage column
    if ("Prob Advantage (pp)" %in% display_cols) {
      gt_tbl <- tryCatch({
        gt::data_color(
          gt_tbl,
          columns = "Prob Advantage (pp)",
          colors = scales::col_numeric(
            palette = c("#7f1d1d", "#991b1b", "#1f2937", "#14532d", "#15532d"),
            domain = c(-0.30, 0.30),
            na.color = "#374151"
          )
        )
      }, error = function(e) gt_tbl)
    }

    # Add color coding for spread columns
    if (all(c("Market Home Spread", "Blend Median Margin") %in% display_cols)) {
      gt_tbl <- tryCatch({
        gt::data_color(
          gt_tbl,
          columns = c("Market Home Spread", "Blend Median Margin"),
          colors = scales::col_numeric(
            palette = c("#dc2626", "#f87171", "#1f2937", "#4ade80", "#22c55e"),
            domain = c(-14, 14),
            na.color = "#374151"
          )
        )
      }, error = function(e) gt_tbl)
    }
    gt_tbl <- gt::tab_header(
      gt_tbl,
      title = title,
      subtitle = "✅ CORRECTED CALCULATIONS • 🎯 Spreads • 📊 Probabilities • 💰 True EV Edge • 📈 Total Expected Value"
    )

    # Add column spanners for better organization
    gt_tbl <- tryCatch({
      if ("tab_spanner" %in% getNamespaceExports("gt")) {
        gt_tbl <- gt::tab_spanner(gt_tbl, label = "📅 Game Info", columns = c("Season", "Week", "Date", "Matchup"))
        if ("Winner" %in% display_cols) {
          gt_tbl <- gt::tab_spanner(gt_tbl, label = "🏆 Result", columns = "Winner")
        }
        gt_tbl <- gt::tab_spanner(gt_tbl, label = "🎯 Blend Analysis", columns = dplyr::matches("^(Blend|EV|Total|Prob)"))
        gt_tbl <- gt::tab_spanner(gt_tbl, label = "📊 Market Data", columns = dplyr::matches("^Market"))
      }
      gt_tbl
    }, error = function(e) gt_tbl)

    gt_tbl <- gt::tab_source_note(
      gt_tbl,
      source_note = "✅ FIXED: EV Edge = (Prob × Decimal Odds) - 1 | Total EV = Stake × EV Edge | Prob Advantage shown for reference only"
    )
    gt_tbl <- gt::tab_options(
      gt_tbl,
      table.font.names = c("Inter", "Source Sans Pro", "Helvetica Neue", "Arial", "sans-serif"),
      table.font.color = "#e2e8f0",
      table.background.color = "transparent",
      heading.background.color = "#0b1120",
      heading.align = "center",
      column_labels.background.color = "#111c2f",
      column_labels.font.weight = "600",
      column_labels.text_transform = "uppercase",
      column_labels.border.top.style = "solid",
      column_labels.border.top.color = "#1f2a44",
      column_labels.border.bottom.color = "#1f2a44",
      row.striping.background_color = "rgba(15,23,42,0.55)",
      data_row.padding = gt::px(10),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.left.color = "transparent",
      table.border.right.color = "transparent",
      table.font.size = gt::px(14)
    )
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(color = "#f8fafc"),
      locations = gt::cells_title(groups = "title")
    )
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(color = "#cbd5f5"),
      locations = gt::cells_title(groups = "subtitle")
    )
    gt_tbl <- gt::opt_row_striping(gt_tbl)
    if ("Blend Beat Market?" %in% display_cols) {
      gt_tbl <- gt::data_color(
        gt_tbl,
        columns = "Blend Beat Market?",
        colors = function(values) {
          palette <- c(Yes = "#166534", No = "#1f2937", `N/A` = "#374151")
          mapped <- palette[as.character(values)]
          default <- palette[["N/A"]]
          mapped[is.na(mapped)] <- default
          unname(mapped)
        }
      )
    }
    diff_cols <- intersect(c("Market Prob Δ", "Blend Prob Δ"), display_cols)
    if (length(diff_cols)) {
      palette <- scales::col_numeric(
        palette = c("#ef4444", "#f59e0b", "#22c55e"),
        domain = c(-0.15, 0.15)
      )
      gt_tbl <- gt::data_color(
        gt_tbl,
        columns = dplyr::all_of(diff_cols),
        colors = palette
      )
    }
    if ("Blend Recommendation" %in% display_cols) {
      gt_tbl <- gt::tab_style(
        gt_tbl,
        style = list(
          gt::cell_fill(color = "#1d4ed8"),
          gt::cell_text(color = "#f8fafc", weight = "bold")
        ),
        locations = gt::cells_body(
          columns = "Blend Recommendation",
          rows = !is.na(`Blend Recommendation`) & `Blend Recommendation` != "Pass"
        )
      )
    }
    if ("Winner" %in% display_cols) {
      gt_tbl <- gt::tab_style(
        gt_tbl,
        style = list(
          gt::cell_fill(color = "#0f172a"),
          gt::cell_text(color = "#fcd34d", weight = "bold")
        ),
        locations = gt::cells_body(
          columns = "Winner",
          rows = !is.na(Winner) & Winner != "TBD"
        )
      )
    }
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = list(
        gt::cell_fill(color = "#1e293b"),
        gt::cell_text(color = "#f8fafc", weight = "bold")
      ),
      locations = gt::cells_column_labels(columns = gt::everything())
    )

    if ("opt_interactive" %in% getNamespaceExports("gt")) {
      gt_tbl <- tryCatch(
        do.call(
          getFromNamespace("opt_interactive", "gt"),
          list(
            data = gt_tbl,
            use_search = TRUE,
            use_filters = TRUE,
            use_pagination = TRUE,
            search_placeholder = "Search teams, wagers, or math checks...",
            filter_placeholder = "Filter column"
          )
        ),
        error = function(e) gt_tbl
      )
    }

    if ("opt_css" %in% getNamespaceExports("gt")) {
      custom_css <- paste(
        ".gt_table { border-radius: 18px; overflow: hidden; box-shadow: 0 28px 60px rgba(15, 23, 42, 0.55); background-color: rgba(15, 23, 42, 0.9); }",
        ".gt_table thead th { position: sticky; top: 0; z-index: 2; backdrop-filter: blur(6px); background-color: rgba(15, 23, 42, 0.92); font-size: 0.85rem; padding: 12px 8px; border-bottom: 2px solid rgba(59, 130, 246, 0.3); }",
        ".gt_table tbody tr:hover { background-color: rgba(37, 99, 235, 0.18); transition: background-color 180ms ease-in-out; transform: scale(1.005); }",
        ".gt_table tbody td { padding: 10px 8px; font-size: 0.9rem; border-bottom: 1px solid rgba(30, 41, 59, 0.5); }",
        ".gt_table tbody td[style*='background'] { font-weight: 600; text-shadow: 0 1px 2px rgba(0, 0, 0, 0.3); }",
        "@media (max-width: 768px) { .gt_table thead th { font-size: 0.7rem; padding: 8px 4px; } .gt_table tbody td { font-size: 0.8rem; padding: 8px 4px; } }",
        sep = "\n"
      )
      gt_tbl <- tryCatch(
        gt::opt_css(gt_tbl, css = custom_css),
        error = function(e) gt_tbl
      )
    }

    gt_html <- NULL
    if ("as_raw_html" %in% getNamespaceExports("gt")) {
      gt_as_raw_html <- getFromNamespace("as_raw_html", "gt")
      gt_html <- tryCatch({
        args <- list(data = gt_tbl)
        if ("inline_css" %in% names(formals(gt_as_raw_html))) {
          args$inline_css <- TRUE
        }
        do.call(gt_as_raw_html, args)
      }, error = function(e) NULL)
    }

    if (!is.null(gt_html) && requireNamespace("htmltools", quietly = TRUE)) {
      table_id <- "moneyline-table"
      if (!grepl(sprintf("id=\\\"%s\\\"", table_id), gt_html, fixed = FALSE)) {
        gt_html <- sub("<table", sprintf("<table id=\"%s\"", table_id), gt_html)
      }

      css_block_gt <- paste0(
        "body {font-family: 'Inter','Source Sans Pro','Helvetica Neue',Arial,sans-serif; background: radial-gradient(circle at top,#172554 0%,#020617 70%); color: #e2e8f0; margin: 0; padding-top: 85px;}\n",
        ".search-container {position: fixed; top: 0; left: 0; right: 0; z-index: 1000; background: linear-gradient(135deg, rgba(2, 6, 23, 0.98), rgba(15, 23, 42, 0.95)); backdrop-filter: blur(10px); border-bottom: 2px solid rgba(59, 130, 246, 0.4); padding: 1.25rem 0; box-shadow: 0 4px 16px rgba(0, 0, 0, 0.4);}\n",
        ".search-inner {max-width: 1200px; margin: 0 auto; padding: 0 1.5rem;}\n",
        ".page-wrapper {max-width: 1200px; margin: 0 auto; padding: 1rem 1.5rem 4rem;}\n",
        ".table-wrapper {overflow-x: auto; border-radius: 18px;}\n",
        ".report-intro {max-width: 960px; margin: 0 auto 2rem; background: linear-gradient(135deg,rgba(15,23,42,0.95),rgba(30,41,59,0.95)); padding: 1.5rem 1.75rem; border-radius: 18px; border: 1px solid rgba(148,163,184,0.25); box-shadow: 0 24px 56px rgba(15,23,42,0.55);}\n",
        ".report-intro h2 {margin: 0 0 0.75rem; font-size: 1.3rem; color: #f8fafc; letter-spacing: 0.02em;}\n",
        ".report-intro p {margin: 0 0 1rem; color: #cbd5f5; font-size: 0.95rem;}\n",
        ".report-intro ul {margin: 0; padding-left: 1.25rem; color: #e2e8f0; line-height: 1.5;}\n",
        ".report-intro li {margin-bottom: 0.4rem;}\n",
        ".gt_table {border-radius: 18px; overflow: hidden; box-shadow: 0 28px 60px rgba(15,23,42,0.55);}\n",
        ".gt_table thead th {position: sticky; top: 85px; z-index: 100; background: rgba(15,23,42,0.92); backdrop-filter: blur(6px);}\n",
        ".gt_table tbody tr:hover {background-color: rgba(37,99,235,0.18) !important;}\n",
        "#table-search {width: 100%; max-width: 500px; padding: 0.85rem 1.25rem; margin: 0 auto; border-radius: 999px; border: 1px solid rgba(59, 130, 246, 0.4); background-color: rgba(15,23,42,0.9); color: #f8fafc; display: block; box-shadow: 0 4px 12px rgba(59, 130, 246, 0.2); transition: all 0.2s ease; font-size: 1rem;}\n",
        "#table-search:focus {outline: none; border-color: #60a5fa; box-shadow: 0 0 0 3px rgba(96,165,250,0.4), 0 8px 20px rgba(59, 130, 246, 0.3); transform: translateY(-1px);}\n",
        "#table-search::placeholder {color: rgba(148, 163, 184, 0.7);}\n",
        "@media (max-width: 768px) { body {padding-top: 75px;} .gt_table {font-size: 0.88rem;} .gt_table thead th {font-size: 0.7rem; top: 75px;} .report-intro {padding: 1.25rem;} #table-search {font-size: 0.9rem; padding: 0.75rem 1rem;} }\n"
      )

      search_box <- htmltools::tags$div(
        class = "search-container",
        htmltools::tags$div(
          class = "search-inner",
          htmltools::tags$input(
            id = "table-search",
            type = "search",
            class = "table-search",
            placeholder = "🔍 Search teams, picks, or betting data...",
            `aria-label` = "Search moneyline table"
          )
        )
      )

      intro_block <- htmltools::HTML(intro_html)

      content_wrapper <- htmltools::tags$div(
        class = "page-wrapper",
        intro_block,
        htmltools::tags$div(
          class = "table-wrapper",
          htmltools::HTML(gt_html)
        )
      )

      script_block <- htmltools::tags$script(htmltools::HTML(
        sprintf(
          "(function(){var input=document.getElementById('table-search');var table=document.getElementById('%s');if(!input||!table){return;}var rows=table.getElementsByTagName('tbody')[0].rows;input.addEventListener('input',function(){var query=this.value.toLowerCase();Array.prototype.forEach.call(rows,function(row){var text=row.textContent.toLowerCase();row.style.display=text.indexOf(query)>-1?'':'none';});});})();",
          table_id
        )
      ))

      doc <- htmltools::tags$html(
        htmltools::tags$head(htmltools::tags$style(css_block_gt)),
        htmltools::tags$body(search_box, content_wrapper, script_block)
      )

      htmltools::save_html(doc, file = file)
      saved <- TRUE
    }
  }

  if (!saved) {
    css_block <- "body {font-family: 'Inter','Source Sans Pro','Helvetica Neue',Arial,sans-serif; background: radial-gradient(circle at top,#172554 0%,#020617 70%); color: #e2e8f0; margin: 0;}\n"
    css_block <- paste0(
      css_block,
      ".page-wrapper {max-width: 1200px; margin: 0 auto; padding: 3rem 1.5rem 4rem;}\n",
      ".table-wrapper {overflow-x: auto; border-radius: 18px;}\n",
      ".report-intro {max-width: 960px; margin: 0 auto 2rem; background: linear-gradient(135deg,rgba(15,23,42,0.95),rgba(30,41,59,0.95)); padding: 1.5rem 1.75rem; border-radius: 18px; border: 1px solid rgba(148,163,184,0.25); box-shadow: 0 24px 56px rgba(15,23,42,0.55);}\n",
      ".report-intro h2 {margin: 0 0 0.75rem; font-size: 1.3rem; color: #f8fafc; letter-spacing: 0.02em;}\n",
      ".report-intro p {margin: 0 0 1rem; color: #cbd5f5; font-size: 0.95rem;}\n",
      ".report-intro ul {margin: 0; padding-left: 1.25rem; color: #e2e8f0; line-height: 1.5;}\n",
      ".report-intro li {margin-bottom: 0.4rem;}\n",
      "table {width: 100%; border-collapse: separate; border-spacing: 0; background-color: rgba(15,23,42,0.94); color: #e2e8f0; border-radius: 18px; overflow: hidden; box-shadow: 0 28px 60px rgba(15,23,42,0.55); border: 1px solid rgba(148,163,184,0.2);}\n",
      "thead th {background-color: rgba(17,28,47,0.95); color: #f8fafc; text-transform: uppercase; letter-spacing: 0.08em; position: sticky; top: 0; z-index: 2;}\n",
      "td, th {padding: 12px 14px; border-bottom: 1px solid rgba(30,41,59,0.75); text-align: center;}\n",
      "td.text-left {text-align: left;}\n",
      "td.note-cell {max-width: 260px; white-space: normal; word-wrap: break-word;}\n",
      "tr:nth-child(even) {background-color: rgba(15,23,42,0.65);}\n",
      "tr.blend-win {background: linear-gradient(135deg,rgba(22,101,52,0.75),rgba(21,128,61,0.6));}\n",
      "tr.blend-win td {color: #ecfdf5;}\n",
      "td.blend-reco {background-color: rgba(37,99,235,0.75) !important; color: #f8fafc !important; font-weight: 600;}\n",
      "td.winner-cell {color: #fcd34d; font-weight: 600;}\n",
      "tbody tr:hover {background-color: rgba(37,99,235,0.18);}\n",
      "caption {caption-side: top; font-size: 1.35rem; font-weight: 600; margin-bottom: 0.75rem; color: #f8fafc;}\n",
      "#table-search {width: 100%; max-width: 420px; padding: 0.75rem 1rem; margin: 0 auto 1.5rem; border-radius: 999px; border: 1px solid rgba(148,163,184,0.35); background-color: rgba(15,23,42,0.85); color: #f8fafc; display: block; box-shadow: 0 12px 30px rgba(15,23,42,0.45);}\n",
      "#table-search:focus {outline: none; border-color: #60a5fa; box-shadow: 0 0 0 3px rgba(96,165,250,0.35);}\n",
      "@media (max-width: 768px) { table {font-size: 0.88rem;} thead th {font-size: 0.7rem;} }\n"
    )

    formatted_tbl <- display_tbl %>%
      dplyr::mutate(
        `Blend Edge` = scales::percent(`Blend Edge`, accuracy = 0.1),
        `Market Home Prob` = scales::percent(`Market Home Prob`, accuracy = 0.1),
        `Blend Home Prob` = scales::percent(`Blend Home Prob`, accuracy = 0.1),
        `Market Spread Win%` = scales::percent(`Market Spread Win%`, accuracy = 0.1),
        `Blend Spread Win%` = scales::percent(`Blend Spread Win%`, accuracy = 0.1),
        `Market Prob Δ` = scales::percent(`Market Prob Δ`, accuracy = 0.1),
        `Blend Prob Δ` = scales::percent(`Blend Prob Δ`, accuracy = 0.1),
        `Market Away Prob` = scales::percent(`Market Away Prob`, accuracy = 0.1),
        `Blend Away Prob` = scales::percent(`Blend Away Prob`, accuracy = 0.1),
        `Blend Median Margin` = format_signed_spread(`Blend Median Margin`),
        `Market Home Spread` = format_signed_spread(`Market Home Spread`),
        `Market Implied Margin` = format_signed_spread(`Market Implied Margin`),
        `Market Total` = format(round(`Market Total`, 1), nsmall = 1),
        `Blend EV Units` = format(round(`Blend EV Units`, 3), nsmall = 3),
        `Market EV Units` = format(round(`Market EV Units`, 3), nsmall = 3),
        `Blend Stake (Units)` = dplyr::if_else(
          is.na(`Blend Stake (Units)`),
          "",
          format(round(`Blend Stake (Units)`, 3), nsmall = 3)
        ),
        `Blend Beat Market Basis` = dplyr::coalesce(`Blend Beat Market Basis`, ""),
        Date = suppressWarnings(format(as.Date(Date), "%b %d, %Y")),
        Date = dplyr::if_else(is.na(Date) | Date == "NA", "", Date),
        `Blend Favorite` = dplyr::if_else(is.na(`Blend Favorite`), "", `Blend Favorite`),
        `Market Favorite` = dplyr::if_else(is.na(`Market Favorite`), "", `Market Favorite`),
        `Blend Prob` = dplyr::if_else(is.na(`Blend Prob`), "", `Blend Prob`),
        `Market Prob` = dplyr::if_else(is.na(`Market Prob`), "", `Market Prob`),
        `Blend Pick` = dplyr::if_else(is.na(`Blend Pick`), "", `Blend Pick`),
        Winner = dplyr::if_else(is.na(Winner) | Winner == "", "TBD", Winner),
        dplyr::across(
          dplyr::all_of(moneyline_cols),
          format_moneyline_strings
        )
      )

    if (requireNamespace("htmltools", quietly = TRUE)) {
      left_align_cols <- c(
        "Matchup", "Winner", "Blend Favorite", "Market Favorite",
        "Blend Prob", "Market Prob", "Blend Pick", "Blend Recommendation",
        "Blend Beat Market Basis"
      )
      rows <- purrr::map(
        seq_len(nrow(formatted_tbl)),
        ~ {
          row_vals <- formatted_tbl[.x, , drop = FALSE]
          row_list <- as.list(row_vals)
          recommendation_val <- row_list[["Blend Recommendation"]]
          beat_market_val <- row_list[["Blend Beat Market?"]]
          row_classes <- c()
          if (!is.null(beat_market_val) && identical(beat_market_val, "Yes")) {
            row_classes <- c(row_classes, "blend-win")
          }
          if (!is.null(recommendation_val) && !identical(recommendation_val, "Pass") && !is.na(recommendation_val)) {
            row_classes <- c(row_classes, "blend-action")
          }
          htmltools::tags$tr(
            class = paste(row_classes, collapse = " "),
            purrr::imap(row_list, function(value, col_name) {
              cell_classes <- character(0)
              if (identical(col_name, "Blend Recommendation") && !is.null(recommendation_val) &&
                  !identical(recommendation_val, "Pass") && !is.na(recommendation_val)) {
                cell_classes <- c(cell_classes, "blend-reco")
              }
              if (col_name %in% left_align_cols) {
                cell_classes <- c(cell_classes, "text-left")
              }
              cell_value <- ifelse(is.na(value), "", value)
              display_value <- cell_value
              if (identical(col_name, "Winner") && nzchar(cell_value) && cell_value != "TBD") {
                cell_classes <- c(cell_classes, "winner-cell")
              }
              if (length(cell_classes)) {
                htmltools::tags$td(class = paste(cell_classes, collapse = " "), display_value)
              } else {
                htmltools::tags$td(display_value)
              }
            })
          )
        }
      )

      table_id <- "moneyline-table"
      table_html <- htmltools::tags$table(
        id = table_id,
        htmltools::tags$caption(title),
        htmltools::tags$thead(htmltools::tags$tr(purrr::map(names(formatted_tbl), htmltools::tags$th))),
        htmltools::tags$tbody(rows)
      )

      search_box <- htmltools::tags$input(
        id = "table-search",
        type = "search",
        class = "table-search",
        placeholder = "Search teams, wagers, or math checks...",
        `aria-label` = "Search moneyline table"
      )

      wrapper <- htmltools::tags$div(
        class = "page-wrapper",
        search_box,
        htmltools::tags$div(class = "table-wrapper", table_html)
      )

      script_block <- htmltools::tags$script(htmltools::HTML(
        sprintf(
          "(function(){var input=document.getElementById('table-search');var table=document.getElementById('%s');if(!input||!table){return;}var rows=table.getElementsByTagName('tbody')[0].rows;input.addEventListener('input',function(){var query=this.value.toLowerCase();Array.prototype.forEach.call(rows,function(row){var text=row.textContent.toLowerCase();row.style.display=text.indexOf(query)>-1?'':'none';});});})();",
          table_id
        )
      ))

      doc <- htmltools::tags$html(
        htmltools::tags$head(htmltools::tags$style(css_block)),
        htmltools::tags$body(wrapper, script_block)
      )

      htmltools::save_html(doc, file = file)
      saved <- TRUE
    } else {
      table_id <- "moneyline-table"
      header <- paste(names(formatted_tbl), collapse = "</th><th>")
      left_align_cols <- c(
        "Matchup", "Winner", "Blend Favorite", "Market Favorite",
        "Blend Prob", "Market Prob", "Blend Pick", "Blend Recommendation",
        "Blend Beat Market Basis"
      )
      body <- purrr::map_chr(
        seq_len(nrow(formatted_tbl)),
        function(idx) {
          row_vals <- formatted_tbl[idx, , drop = FALSE]
          row_list <- as.list(row_vals)
          recommendation_val <- row_list[["Blend Recommendation"]]
          beat_market_val <- row_list[["Blend Beat Market?"]]
          row_classes <- c()
          if (!is.null(beat_market_val) && identical(beat_market_val, "Yes")) {
            row_classes <- c(row_classes, "blend-win")
          }
          if (!is.null(recommendation_val) && !identical(recommendation_val, "Pass") && !is.na(recommendation_val)) {
            row_classes <- c(row_classes, "blend-action")
          }
          row_class_attr <- if (length(row_classes)) sprintf(" class=\"%s\"", paste(row_classes, collapse = " ")) else ""
          cells <- purrr::imap_chr(row_list, function(value, col_name) {
            cell_classes <- character(0)
            if (identical(col_name, "Blend Recommendation") && !is.null(recommendation_val) &&
                !identical(recommendation_val, "Pass") && !is.na(recommendation_val)) {
              cell_classes <- c(cell_classes, "blend-reco")
            }
            if (col_name %in% left_align_cols) {
              cell_classes <- c(cell_classes, "text-left")
            }
            cell_value <- ifelse(is.na(value), "", value)
            display_value <- cell_value
            if (identical(col_name, "Winner") && nzchar(cell_value) && cell_value != "TBD") {
              cell_classes <- c(cell_classes, "winner-cell")
            }
            if (length(cell_classes)) {
              sprintf("<td class=\"%s\">%s</td>", paste(cell_classes, collapse = " "), display_value)
            } else {
              sprintf("<td>%s</td>", display_value)
            }
          })
          sprintf("<tr%s>%s</tr>", row_class_attr, paste(cells, collapse = ""))
        }
      )
      html <- paste0(
        "<html><head><style>",
        css_block,
        "</style></head><body><div class=\"page-wrapper\"><input id=\"table-search\" type=\"search\" placeholder=\"Search teams, wagers, or math checks...\" aria-label=\"Search moneyline table\"/>",
        intro_html,
        "<div class=\"table-wrapper\"><table id=\"",
        table_id,
        "\"><caption>",
        title,
        "</caption><thead><tr><th>",
        header,
        "</th></tr></thead><tbody>",
        paste(body, collapse = ""),
        "</tbody></table></div></div><script>(function(){var input=document.getElementById('table-search');var table=document.getElementById('",
        table_id,
        "');if(!input||!table){return;}var rows=table.getElementsByTagName('tbody')[0].rows;input.addEventListener('input',function(){var query=this.value.toLowerCase();Array.prototype.forEach.call(rows,function(row){var text=row.textContent.toLowerCase();row.style.display=text.indexOf(query)>-1?'':'none';});});})();</script></body></html>"
      )
      writeLines(html, con = file)
      saved <- TRUE
    }
  }

  normalized_path <- normalizePath(file, winslash = "/", mustWork = FALSE)

  if (saved && verbose) {
    message(sprintf("export_moneyline_comparison_html(): wrote HTML report to %s", normalized_path))
  }

  if (saved && auto_open) {
    opened <- FALSE
    if (exists("open_moneyline_report", inherits = TRUE)) {
      opened <- isTRUE(tryCatch(
        open_moneyline_report(normalized_path, prefer_viewer = TRUE, verbose = verbose),
        error = function(e) FALSE
      ))
    }

    if (!opened) {
      tryCatch(utils::browseURL(normalized_path), error = function(e) NULL)
    }
  }

  invisible(normalized_path)
}
open_moneyline_report <- function(file, prefer_viewer = TRUE, verbose = TRUE) {
  if (missing(file) || is.null(file) || !nzchar(file)) {
    if (verbose) warning("open_moneyline_report(): file path is missing or empty.")
    return(invisible(FALSE))
  }

  normalized <- tryCatch(normalizePath(file, winslash = "/", mustWork = TRUE),
                         error = function(e) file)
  if (!file.exists(normalized)) {
    if (verbose) warning(sprintf("open_moneyline_report(): %s does not exist.", normalized))
    return(invisible(FALSE))
  }

  opened <- FALSE

  if (prefer_viewer && requireNamespace("rstudioapi", quietly = TRUE)) {
    opened <- tryCatch({
      rstudioapi::viewer(normalized)
      TRUE
    }, error = function(e) FALSE)
  }

  if (!opened) {
    opened <- tryCatch({
      utils::browseURL(normalized)
      TRUE
    }, error = function(e) FALSE)
  }

  if (verbose) {
    if (opened) {
      message(sprintf("open_moneyline_report(): opened %s", normalized))
    } else {
      message(sprintf("open_moneyline_report(): unable to automatically open %s", normalized))
    }
  }

  invisible(opened)
}

moneyline_report <- function(market_comparison_result,
                             schedule,
                             file = NULL,
                             title = NULL,
                             join_keys = PREDICTION_JOIN_KEYS,
                             vig = 0.10,
                             verbose = TRUE,
                             auto_open = TRUE) {
  join_keys <- unique(join_keys)
  table <- build_moneyline_comparison_table(
    market_comparison_result = market_comparison_result,
    enriched_schedule = schedule,
    join_keys = join_keys,
    vig = vig,
    verbose = verbose
  )

  if (!nrow(table)) {
    if (verbose) {
      message("moneyline_report(): comparison table is empty; skipping export.")
    }
    return(invisible(NULL))
  }

  if (is.null(title) || !nzchar(title)) {
    seasons <- unique(table$season)
    weeks <- unique(table$week)
    if (length(seasons) == 1L && length(weeks) == 1L) {
      title <- sprintf("Blend vs Market Moneylines — Week %s, %s", weeks, seasons)
    } else {
      title <- "Blend vs Market Moneyline Comparison"
    }
  }

  season_filter <- if ("season" %in% names(table)) unique(table$season[!is.na(table$season)]) else NULL
  week_filter <- if ("week" %in% names(table)) unique(table$week[!is.na(table$week)]) else NULL

  export_moneyline_comparison_html(
    comparison_tbl = table,
    file = file,
    title = title,
    verbose = verbose,
    auto_open = auto_open,
    season = season_filter,
    week = week_filter
  )
}

if (interactive()) {
  message(
    "NFLmarket.R loaded. Use load_latest_market_inputs(), enrich_with_pre_kickoff_espn_lines(), and evaluate_market_vs_blend() as needed."
  )
}
