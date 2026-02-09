# =============================================================================
# FILE: NFLmarket.R
# PURPOSE: Market comparison and betting analytics engine for NFL predictions.
#          Handles market data integration, probability shrinkage, Kelly staking,
#          and HTML report generation for moneyline comparison.
#
# AUTHOR: Data Analyst <analyst@example.com>
# VERSION: 2.7.0
# LAST UPDATED: 2026-02-02
#
# DEPENDENCIES:
#   - tidyverse: Data manipulation and visualization
#   - gt (>= 0.8.0): HTML table generation
#   - NFLbrier_logloss.R: Scoring metrics
#   - R/utils.R: Core utility functions
#
# EXPORTS:
#   - build_moneyline_comparison_table(): Main market comparison engine
#   - shrink_probability_toward_market(): Probability shrinkage (60% market)
#   - expected_value_units(): EV calculation for betting decisions
#   - conservative_kelly_stake(): 1/8 Kelly with edge caps
#   - render_moneyline_comparison_html(): HTML report generation
#
# KEY PARAMETERS:
#   - SHRINKAGE (config.R): Market weight for probability blending (default: 0.6)
#   - KELLY_FRACTION (config.R): Kelly criterion fraction (default: 0.125)
#   - MAX_EDGE (config.R): Maximum believable edge cap (default: 0.10)
#
# VALIDATION:
#   - Test file: tests/testthat/test-market.R
#   - Edge quality flags: >15% flagged as "Implausible"
#   - EV uses shrunk probabilities (lines 2863-2870)
# =============================================================================

suppressPackageStartupMessages({
  # Source canonical utilities from R/utils.R first
  local({
    utils_path <- if (file.exists("R/utils.R")) "R/utils.R" else file.path(getwd(), "R/utils.R")
    if (file.exists(utils_path)) {
      tryCatch(source(utils_path), error = function(e) {
        message(sprintf("Note: Could not source R/utils.R: %s", conditionMessage(e)))
      })
    }
  })
  source("NFLbrier_logloss.R")
  # Source props policy helpers if available
  local({
    props_config_path <- if (file.exists("sports/nfl/props/props_config.R")) {
      "sports/nfl/props/props_config.R"
    } else {
      file.path(getwd(), "sports", "nfl", "props", "props_config.R")
    }
    if (file.exists(props_config_path)) {
      tryCatch(source(props_config_path), error = function(e) {
        message(sprintf("Note: Could not source props_config.R: %s", conditionMessage(e)))
      })
    }
  })
  library(tidyverse)
})

# Provide a no-op join signal emitter when not available from logging utilities.
if (!exists("emit_safe_join_signal", mode = "function", inherits = TRUE)) {
  emit_safe_join_signal <- function(message, label = NULL, severity = "inform") {
    message(message)
  }
}

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
# Note: first_non_missing_typed() is defined in NFLbrier_logloss.R (sourced above)

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
  # NOTE: Canonical version is in R/utils.R. This definition exists for standalone

  # sourcing. Includes CRITICAL type coercion for reliable joins.
  if (is.null(df) || !inherits(df, "data.frame")) {
    return(df)
  }

  out <- df

  # Rename aliases to canonical names
  for (canonical in names(key_alias)) {
    if (canonical %in% names(out)) next
    alt_names <- unique(c(key_alias[[canonical]], canonical))
    alt_names <- alt_names[alt_names != canonical]
    match <- alt_names[alt_names %in% names(out)]
    if (length(match)) {
      out <- dplyr::rename(out, !!canonical := !!rlang::sym(match[1]))
    }
  }

  # CRITICAL: Coerce to standard types for reliable joins
  # Without this, integer vs character mismatches cause silent join failures
  if ("game_id" %in% names(out)) {
    out$game_id <- as.character(out$game_id)
  }
  if ("season" %in% names(out)) {
    out$season <- as.integer(out$season)
  }
  if ("week" %in% names(out)) {
    out$week <- as.integer(out$week)
  }
  if ("game_type" %in% names(out)) {
    out$game_type <- as.character(out$game_type)
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

# NOTE: american_to_probability, american_to_decimal, clamp_probability,
# expected_value_units, shrink_probability_toward_market, classify_edge_magnitude
# are now defined in NFLbrier_logloss.R (sourced above) as canonical versions.

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
    prob == 0.5 ~ 100,
    prob > 0.5 ~ -round(100 * prob / (1 - prob)),
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

# ==============================================================================
# PROFESSIONAL ANALYTICS: CLV, Brier Score, Confidence Intervals, Market Movement
# ==============================================================================
# These functions implement professional-grade sports betting analytics to
# evaluate model performance and provide meaningful insights.

#' Calculate Closing Line Value (CLV)
#'
#' CLV measures whether the model consistently beats closing market prices.
#' Positive CLV over time is one of the strongest indicators of a winning model.
#'
#' @param model_prob Model's probability estimate at time of prediction
#' @param closing_prob Market closing probability (after all line movement)
#' @param opening_prob Optional: Market opening probability for movement analysis
#' @return List with CLV metrics
calculate_clv <- function(model_prob, closing_prob, opening_prob = NULL) {
  model_prob <- clamp_probability(model_prob)
  closing_prob <- clamp_probability(closing_prob)

  # Logit transformation for proper probability comparison
  logit <- function(p) log(p / (1 - p))

  # CLV in logit space (standard measure)
  clv_logit <- logit(model_prob) - logit(closing_prob)

  # CLV in probability points (more interpretable)
  clv_prob <- model_prob - closing_prob

  # Direction agreement: did model and closing price agree on favorite?
  model_favorite <- model_prob > 0.5
  closing_favorite <- closing_prob > 0.5
  direction_agree <- model_favorite == closing_favorite

  result <- list(
    clv_logit = clv_logit,
    clv_prob = clv_prob,
    direction_agree = direction_agree,
    model_prob = model_prob,
    closing_prob = closing_prob
  )

  # If opening line available, calculate line movement analysis
  if (!is.null(opening_prob)) {
    opening_prob <- clamp_probability(opening_prob)
    line_movement <- closing_prob - opening_prob
    model_captured_movement <- (model_prob > opening_prob & closing_prob > opening_prob) |
                               (model_prob < opening_prob & closing_prob < opening_prob)
    result$opening_prob <- opening_prob
    result$line_movement <- line_movement
    result$model_captured_movement <- model_captured_movement
  }

  result
}

#' Summarize CLV metrics across multiple games
#'
#' @param clv_results List of CLV results from calculate_clv()
#' @return Summary tibble with aggregate CLV statistics
summarize_clv <- function(clv_results) {
  if (!length(clv_results)) {
    return(tibble::tibble(
      mean_clv_logit = NA_real_,
      mean_clv_prob = NA_real_,
      clv_positive_pct = NA_real_,
      direction_agree_pct = NA_real_,
      n_games = 0L
    ))
  }

  clv_logits <- sapply(clv_results, function(x) x$clv_logit)
  clv_probs <- sapply(clv_results, function(x) x$clv_prob)
  direction_agree <- sapply(clv_results, function(x) x$direction_agree)

  tibble::tibble(
    mean_clv_logit = mean(clv_logits, na.rm = TRUE),
    mean_clv_prob = mean(clv_probs, na.rm = TRUE),
    clv_positive_pct = mean(clv_logits > 0, na.rm = TRUE),
    direction_agree_pct = mean(direction_agree, na.rm = TRUE),
    n_games = sum(!is.na(clv_logits))
  )
}

#' Calculate Brier Score with decomposition
#'
#' Brier score measures probabilistic calibration. Lower is better.
#' Decomposition shows reliability, resolution, and uncertainty components.
#'
#' @param prob Vector of predicted probabilities
#' @param outcome Vector of actual outcomes (0/1)
#' @param n_bins Number of bins for calibration analysis (default 10)
#' @return List with Brier score and components
calculate_brier_decomposition <- function(prob, outcome, n_bins = 10) {
  prob <- clamp_probability(prob)
  outcome <- as.numeric(outcome)

  valid <- !is.na(prob) & !is.na(outcome)
  prob <- prob[valid]
  outcome <- outcome[valid]

  n <- length(prob)
  if (n == 0) {
    return(list(
      brier_score = NA_real_,
      reliability = NA_real_,
      resolution = NA_real_,
      uncertainty = NA_real_,
      calibration_bins = tibble::tibble()
    ))
  }

  # Overall Brier score
  brier_score <- mean((prob - outcome)^2)

  # Base rate (uncertainty)
  base_rate <- mean(outcome)
  uncertainty <- base_rate * (1 - base_rate)

  # Bin predictions for calibration analysis
  breaks <- seq(0, 1, length.out = n_bins + 1)
  bin_idx <- cut(prob, breaks = breaks, include.lowest = TRUE, labels = FALSE)

  calibration_bins <- tibble::tibble(prob = prob, outcome = outcome, bin = bin_idx) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(
      mean_prob = mean(prob),
      actual_rate = mean(outcome),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      deviation = actual_rate - mean_prob,
      abs_deviation = abs(deviation)
    )

  # Reliability (calibration error)
  reliability <- calibration_bins %>%
    dplyr::summarise(rel = sum(n * (mean_prob - actual_rate)^2) / sum(n)) %>%
    dplyr::pull(rel)

  # Resolution (how much predictions vary from base rate)
  resolution <- calibration_bins %>%
    dplyr::summarise(res = sum(n * (actual_rate - base_rate)^2) / sum(n)) %>%
    dplyr::pull(res)

  list(
    brier_score = brier_score,
    reliability = reliability,
    resolution = resolution,
    uncertainty = uncertainty,
    base_rate = base_rate,
    n_games = n,
    calibration_bins = calibration_bins
  )
}

#' Format Brier score results for display
#'
#' @param brier_result Result from calculate_brier_decomposition()
#' @return Character string with formatted metrics
format_brier_display <- function(brier_result) {
  if (is.na(brier_result$brier_score)) {
    return("Brier Score: N/A (insufficient data)")
  }

  sprintf(
    "Brier Score: %.4f (Reliability: %.4f, Resolution: %.4f, Uncertainty: %.4f) | N=%d",
    brier_result$brier_score,
    brier_result$reliability,
    brier_result$resolution,
    brier_result$uncertainty,
    brier_result$n_games
  )
}

#' Calculate confidence intervals using bootstrap
#'
#' @param estimate Point estimate (e.g., EV, probability)
#' @param se Standard error (if known)
#' @param n Sample size (for t-based CI if se provided)
#' @param conf_level Confidence level (default 0.95)
#' @return Named vector with lo, estimate, hi
calculate_confidence_interval <- function(estimate, se = NULL, n = NULL, conf_level = 0.95) {
  if (is.na(estimate)) {
    return(c(lo = NA_real_, estimate = NA_real_, hi = NA_real_))
  }

  alpha <- (1 - conf_level) / 2

  if (!is.null(se) && !is.na(se) && !is.null(n) && n > 1) {
    # t-based confidence interval
    df <- n - 1
    t_crit <- stats::qt(1 - alpha, df)
    margin <- t_crit * se
    c(lo = estimate - margin, estimate = estimate, hi = estimate + margin)
  } else if (!is.null(se) && !is.na(se)) {
    # Z-based confidence interval (large sample)
    z_crit <- stats::qnorm(1 - alpha)
    margin <- z_crit * se
    c(lo = estimate - margin, estimate = estimate, hi = estimate + margin)
  } else {
    # Return point estimate with NA bounds
    c(lo = NA_real_, estimate = estimate, hi = NA_real_)
  }
}

#' Calculate probability confidence interval using Wilson score
#'
#' Wilson score interval is better for probabilities near 0 or 1
#'
#' @param prob Probability estimate
#' @param n Sample size
#' @param conf_level Confidence level (default 0.95)
#' @return Named vector with lo, estimate, hi
wilson_confidence_interval <- function(prob, n, conf_level = 0.95) {
  if (is.na(prob) || is.na(n) || n <= 0) {
    return(c(lo = NA_real_, estimate = NA_real_, hi = NA_real_))
  }

  prob <- clamp_probability(prob)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  z2 <- z^2

  # Wilson score interval
  denom <- 1 + z2 / n
  center <- (prob + z2 / (2 * n)) / denom
  margin <- z * sqrt((prob * (1 - prob) + z2 / (4 * n)) / n) / denom

  c(
    lo = clamp_probability(center - margin),
    estimate = prob,
    hi = clamp_probability(center + margin)
  )
}

#' Analyze market line movement
#'
#' Tracks how betting lines moved from open to close and compares to model
#'
#' @param open_prob Opening market probability
#' @param close_prob Closing market probability
#' @param model_prob Model's probability at time of prediction
#' @return List with movement analysis
analyze_line_movement <- function(open_prob, close_prob, model_prob) {
  open_prob <- clamp_probability(open_prob)
  close_prob <- clamp_probability(close_prob)
  model_prob <- clamp_probability(model_prob)

  # Raw movement
  movement <- close_prob - open_prob

  # Direction of movement
  movement_direction <- dplyr::case_when(
    is.na(movement) ~ NA_character_,
    abs(movement) < 0.005 ~ "stable",     # Less than 0.5pp movement
    movement > 0 ~ "toward_home",          # Line moved toward home team
    TRUE ~ "toward_away"                   # Line moved toward away team
  )

  # Did model agree with sharp money direction?
  model_vs_open <- model_prob - open_prob
  sharp_agreement <- (model_vs_open > 0 & movement > 0) |
                     (model_vs_open < 0 & movement < 0)

  # Movement magnitude classification
  movement_magnitude <- dplyr::case_when(
    is.na(movement) ~ NA_character_,
    abs(movement) < 0.01 ~ "minimal",      # <1pp
    abs(movement) < 0.03 ~ "moderate",     # 1-3pp
    abs(movement) < 0.05 ~ "significant",  # 3-5pp
    TRUE ~ "major"                          # >5pp
  )

  list(
    movement = movement,
    movement_pct = movement * 100,
    direction = movement_direction,
    magnitude = movement_magnitude,
    model_vs_open = model_vs_open,
    sharp_agreement = sharp_agreement,
    open_prob = open_prob,
    close_prob = close_prob,
    model_prob = model_prob
  )
}

#' Historical backtest framework
#'
#' Run model predictions against multiple historical seasons
#'
#' @param predictions Data frame with model predictions
#' @param actuals Data frame with actual outcomes
#' @param seasons Vector of seasons to include
#' @param join_keys Keys for joining predictions to actuals
#' @return List with backtest results by season and overall
run_historical_backtest <- function(predictions, actuals, seasons = NULL,
                                   join_keys = c("game_id", "season", "week")) {
  predictions <- standardize_join_keys(predictions)
  actuals <- standardize_join_keys(actuals)

  if (is.null(seasons) && "season" %in% names(predictions)) {
    seasons <- sort(unique(predictions$season))
  }

  if (!is.null(seasons) && "season" %in% names(predictions)) {
    predictions <- dplyr::filter(predictions, season %in% seasons)
    actuals <- dplyr::filter(actuals, season %in% seasons)
  }

  # Find probability column
  prob_col <- intersect(
    c("p2_cal", "home_p_2w_cal", "p_model", "home_prob", "blend_home_prob"),
    names(predictions)
  )[1]

  if (is.na(prob_col)) {
    warning("run_historical_backtest(): no probability column found")
    return(NULL)
  }

  # Join predictions with actuals
  combined <- dplyr::inner_join(predictions, actuals, by = join_keys) %>%
    dplyr::mutate(
      prob = clamp_probability(.data[[prob_col]]),
      outcome = dplyr::case_when(
        "home_win" %in% names(.) ~ as.numeric(home_win),
        "y2" %in% names(.) ~ as.numeric(y2),
        "home_score" %in% names(.) & "away_score" %in% names(.) ~
          as.numeric(home_score > away_score),
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(prob), !is.na(outcome))

  if (!nrow(combined)) {
    return(list(
      overall = tibble::tibble(),
      by_season = tibble::tibble(),
      n_games = 0L
    ))
  }

  # Overall metrics
  overall_brier <- calculate_brier_decomposition(combined$prob, combined$outcome)
  overall_accuracy <- mean((combined$prob > 0.5) == combined$outcome, na.rm = TRUE)
  overall_logloss <- -mean(
    combined$outcome * log(combined$prob) +
    (1 - combined$outcome) * log(1 - combined$prob),
    na.rm = TRUE
  )

  overall <- tibble::tibble(
    n_games = nrow(combined),
    brier_score = overall_brier$brier_score,
    log_loss = overall_logloss,
    accuracy = overall_accuracy,
    reliability = overall_brier$reliability,
    resolution = overall_brier$resolution
  )

  # By-season breakdown
  by_season <- combined %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      n_games = dplyr::n(),
      brier_score = mean((prob - outcome)^2, na.rm = TRUE),
      log_loss = -mean(outcome * log(prob) + (1 - outcome) * log(1 - prob), na.rm = TRUE),
      accuracy = mean((prob > 0.5) == outcome, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(season)

  list(
    overall = overall,
    by_season = by_season,
    calibration_bins = overall_brier$calibration_bins,
    n_games = nrow(combined)
  )
}

#' Format backtest results for display
#'
#' @param backtest_result Result from run_historical_backtest()
#' @return Character string with formatted summary
format_backtest_display <- function(backtest_result) {
  if (is.null(backtest_result) || backtest_result$n_games == 0) {
    return("Backtest: No data available")
  }

  overall <- backtest_result$overall
  seasons <- backtest_result$by_season

  summary_lines <- c(
    sprintf("=== HISTORICAL BACKTEST RESULTS (%d games) ===", overall$n_games),
    sprintf("Overall Brier Score: %.4f", overall$brier_score),
    sprintf("Overall Log Loss: %.4f", overall$log_loss),
    sprintf("Overall Accuracy: %.1f%%", overall$accuracy * 100),
    "",
    "By Season:"
  )

  season_lines <- purrr::map_chr(seq_len(nrow(seasons)), function(i) {
    row <- seasons[i, ]
    sprintf("  %d: Brier=%.4f, LogLoss=%.4f, Accuracy=%.1f%% (n=%d)",
            row$season, row$brier_score, row$log_loss, row$accuracy * 100, row$n_games)
  })

  paste(c(summary_lines, season_lines), collapse = "\n")
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

  # Compute available join keys by intersecting with columns present in blend_oos
  blend_oos_std <- standardize_join_keys(blend_oos)
  available_keys <- intersect(join_keys, names(blend_oos_std))
  if (length(available_keys) == 0) {
    if (verbose) message("build_res_blend(): no join keys available in blend_oos; skipping blend attachment.")
    return(NULL)
  }

  blend_join <- blend_oos_std %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(available_keys), ~ !is.na(.))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(available_keys))) %>%
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
        by = available_keys,
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

#' Build comprehensive moneyline comparison table
#'
#' Core function that combines simulation predictions with market data to produce
#' betting recommendations. Applies probability shrinkage, calculates expected value,
#' and generates Kelly-optimal stake sizes.
#'
#' @param market_comparison_result Output from compare_to_market() function
#' @param enriched_schedule Schedule data enriched with market odds
#' @param join_keys Character vector of keys for joining (default: game_id, season, week)
#' @param vig Assumed market vig/juice (default: 0.10 = 10%)
#' @param verbose Print progress messages (default: TRUE)
#'
#' @return Tibble with columns:
#'   \item{game_id}{Unique game identifier}
#'   \item{season, week}{Game timing}
#'   \item{home_team, away_team}{Team abbreviations}
#'   \item{blend_home_prob}{Raw model probability}
#'   \item{blend_home_prob_shrunk}{Shrunk probability (60% market weight)}
#'   \item{market_home_prob}{Market-implied probability}
#'   \item{blend_ev_units_home}{Expected value in units (using shrunk prob)}
#'   \item{blend_recommendation}{"Bet" or "Pass" based on EV}
#'   \item{blend_confidence}{Kelly stake size (1/8 Kelly)}
#'
#' @details
#' The function applies professional-grade shrinkage to raw model probabilities:
#' - **Shrinkage formula**: shrunk = 0.4 × model + 0.6 × market
#' - **EV calculation**: Uses shrunk probabilities (not raw)
#' - **Staking**: 1/8 Kelly with 10% max edge cap
#'
#' Edge quality classification (shown in HTML report):
#' - ≤5%: ✓ OK (realistic)
#' - ≤10%: ⚠ High (optimistic)
#' - ≤15%: ⚠⚠ Suspicious
#' - >15%: 🚫 Implausible
#'
#' @seealso \code{\link{render_moneyline_comparison_html}}, \code{\link{compare_to_market}}
#' @export
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

  # CRITICAL: Standardize scores join keys (type coercion for game_id, season, week)
  scores <- standardize_join_keys(scores)

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
  game_type_col <- select_first_column(schedule_collapsed, c("game_type", "game_type_std", "season_type"))

  schedule_context <- schedule_collapsed %>%
    dplyr::mutate(
      home_team = as.character(pull_or_default(schedule_collapsed, home_team_col, NA_character_)),
      away_team = as.character(pull_or_default(schedule_collapsed, away_team_col, NA_character_)),
      game_date = suppressWarnings(as.Date(pull_or_default(schedule_collapsed, date_col, NA_character_))),
      game_type = as.character(pull_or_default(schedule_collapsed, game_type_col, "REG")),
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
      game_type,
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
  )

  # ===========================================================================
  # DIAGNOSTIC: Empty join result detection with actionable messaging
  # ===========================================================================
  if (!nrow(combined)) {
    # Gather diagnostic information for troubleshooting
    diag_info <- list(
      scores_n = nrow(scores_ready),
      schedule_n = nrow(schedule_context),
      join_cols = join_cols,
      scores_keys = if (nrow(scores_ready) > 0) {
        utils::head(scores_ready[join_cols], 5)
      } else {
        tibble::tibble()
      },
      schedule_keys = if (nrow(schedule_context) > 0) {
        utils::head(schedule_context[join_cols], 5)
      } else {
        tibble::tibble()
      }
    )

    # Check for type mismatches in join keys
    type_mismatches <- character()
    for (col in join_cols) {
      if (col %in% names(scores_ready) && col %in% names(schedule_context)) {
        t1 <- typeof(scores_ready[[col]])
        t2 <- typeof(schedule_context[[col]])
        if (t1 != t2) {
          type_mismatches <- c(type_mismatches, sprintf("  - %s: scores=%s, schedule=%s", col, t1, t2))
        }
      }
    }

    # Check for overlapping values
    overlap_info <- character()
    for (col in join_cols) {
      if (col %in% names(scores_ready) && col %in% names(schedule_context)) {
        v1 <- unique(scores_ready[[col]])
        v2 <- unique(schedule_context[[col]])
        overlap <- length(intersect(as.character(v1), as.character(v2)))
        overlap_info <- c(overlap_info, sprintf("  - %s: %d values in scores, %d in schedule, %d overlap",
                                                 col, length(v1), length(v2), overlap))
      }
    }

    diag_msg <- sprintf(
      paste0(
        "build_moneyline_comparison_table(): JOIN PRODUCED ZERO ROWS\n",
        "  Scores rows: %d\n",
        "  Schedule rows: %d\n",
        "  Join columns: %s\n",
        "%s%s",
        "  Possible causes:\n",
        "    1. No overlapping game_id/season/week between scores and schedule\n",
        "    2. Column type mismatch (e.g., integer vs character for week)\n",
        "    3. Different naming conventions for join keys\n",
        "  Resolution: Check that both tables have matching values in: %s"
      ),
      diag_info$scores_n,
      diag_info$schedule_n,
      paste(join_cols, collapse = ", "),
      if (length(type_mismatches)) paste0("  Type mismatches:\n", paste(type_mismatches, collapse = "\n"), "\n") else "",
      if (length(overlap_info)) paste0("  Key overlaps:\n", paste(overlap_info, collapse = "\n"), "\n") else "",
      paste(join_cols, collapse = ", ")
    )

    if (verbose) message(diag_msg)

    # Return empty tibble with diagnostic attribute
    empty_result <- tibble::tibble()
    attr(empty_result, "join_diagnostic") <- diag_info
    return(empty_result)
  }

  combined <- combined %>%
    ensure_schedule_defaults() %>%
    dplyr::mutate(
      market_home_ml = market_home_ml_sched,
      market_away_ml = market_away_ml_sched,
      market_home_spread = market_home_spread_sched,
      market_total_line = market_total_line_sched,
      home_team = dplyr::coalesce(home_team, home_team_sched),
      away_team = dplyr::coalesce(away_team, away_team_sched),
      game_date = dplyr::coalesce(game_date, game_date_sched),
      # game_type comes from schedule (authoritative source)
      game_type = game_type_sched,
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
      # Raw implied probabilities from moneylines (pre-devig)
      market_home_prob_raw = clamp_probability(american_to_probability(market_home_ml)),
      market_away_prob_raw = clamp_probability(american_to_probability(market_away_ml)),
      # Devig fair probabilities (proportional normalization)
      market_home_prob_fair = devig_two_way_probabilities(market_home_prob_raw, market_away_prob_raw)$p_home,
      market_away_prob_fair = devig_two_way_probabilities(market_home_prob_raw, market_away_prob_raw)$p_away,
      # Use devig fair probabilities for betting math when available
      market_home_prob = dplyr::coalesce(market_home_prob_fair, market_home_prob),
      market_away_prob = dplyr::coalesce(market_away_prob_fair, market_away_prob),
      # Preserve raw implied for display (separate from spread-derived)
      ml_implied_home_prob = market_home_prob_raw,
      ml_implied_away_prob = market_away_prob_raw,
      # Flag if market_home_prob differs significantly from moneyline-implied (spread-derived)
      prob_source = dplyr::case_when(
        is.na(market_home_ml) | !is.finite(market_home_ml) ~ "spread",
        is.na(ml_implied_home_prob) | !is.finite(ml_implied_home_prob) ~ "spread",
        abs(market_home_prob - ml_implied_home_prob) < 0.01 ~ "moneyline",
        TRUE ~ "moneyline"  # We prioritize moneyline when available
      ),
      market_home_spread = harmonize_home_spread(market_home_spread, market_home_prob),
      market_total_line = coerce_numeric_safely(market_total_line),
      blend_median_margin = dplyr::if_else(
        is.na(blend_home_median) | is.na(blend_away_median),
        NA_real_,
        blend_home_median - blend_away_median
      ),
      # Note: harmonize_home_margin moved to AFTER shrunk probability computation (v2.9.4 G8 fix)
      # Raw margin kept here for internal spread calculations
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

      # ===========================================================================
      # PROFESSIONAL CALIBRATION: Shrink probabilities toward market consensus
      # ===========================================================================
      # NFL markets are extremely efficient. Raw model probabilities that diverge
      # significantly from market are almost certainly overfit. We shrink toward
      # market to produce more realistic betting recommendations.
      #
      # Shrinkage levels (validated):
      #   - Regular season: 60% market weight (SHRINKAGE default)
      #   - Playoffs (WC/DIV/CON): 70% market weight (more efficient markets)
      #   - Super Bowl: 75% market weight (most efficient market of the year)
      #
      # Use coalesce to handle potential NA values in game_type
      .game_type_safe = dplyr::coalesce(game_type, "REG"),
      .game_shrinkage = dplyr::case_when(
        .game_type_safe %in% c("SB") ~ get0("SUPER_BOWL_SHRINKAGE", envir = .GlobalEnv, ifnotfound = 0.75),
        .game_type_safe %in% c("WC", "DIV", "CON") ~ get0("PLAYOFF_SHRINKAGE", envir = .GlobalEnv, ifnotfound = 0.70),
        TRUE ~ SHRINKAGE  # Regular season: 0.60
      ),
      blend_home_prob_shrunk = shrink_probability_toward_market(
        blend_home_prob, market_home_prob, shrinkage = .game_shrinkage
      ),
      blend_away_prob_shrunk = shrink_probability_toward_market(
        blend_away_prob, market_away_prob, shrinkage = .game_shrinkage
      ),

      # v2.9.4 G8 fix: Harmonize margin with SHRUNK probability for display coherence
      # sign(margin) must agree with sign(shrunk_prob - 0.5) to avoid contradictions
      blend_median_margin = harmonize_home_margin(blend_median_margin, blend_home_prob_shrunk),

      # Model MLs derived from SHRUNK probabilities for consistency with displayed probabilities
      # (v2.9.3 fix: ensures ML and displayed probability match)
      blend_home_ml = probability_to_american(blend_home_prob_shrunk),
      blend_home_ml_vig = apply_moneyline_vig(blend_home_ml, vig = vig),
      blend_away_ml = probability_to_american(blend_away_prob_shrunk),
      blend_away_ml_vig = apply_moneyline_vig(blend_away_ml, vig = vig),

      # Raw probability edge (for display - shows model's raw view)
      blend_edge_prob_home = blend_home_prob - market_home_prob,
      blend_edge_prob_away = blend_away_prob - market_away_prob,

      # Shrunk probability edge (for betting decisions - more conservative)
      blend_edge_prob_home_shrunk = blend_home_prob_shrunk - market_home_prob,
      blend_edge_prob_away_shrunk = blend_away_prob_shrunk - market_away_prob,

      # EV calculations using SHRUNK probabilities (more realistic)
      blend_ev_units_home = expected_value_units(blend_home_prob_shrunk, market_home_ml),
      blend_ev_units_away = expected_value_units(blend_away_prob_shrunk, market_away_ml),

      # Also calculate raw EV for comparison/display
      blend_ev_units_home_raw = expected_value_units(blend_home_prob, market_home_ml),
      blend_ev_units_away_raw = expected_value_units(blend_away_prob, market_away_ml),
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
      # blend_pick now shows the team with best EV, or falls back to blend's favorite for Pass games
      blend_pick = dplyr::case_when(
        blend_pick_side == "home" ~ home_team,
        blend_pick_side == "away" ~ away_team,
        # For Pass games (negative EV), show the blend's favorite team for reference
        blend_favorite_side == "home" ~ home_team,
        blend_favorite_side == "away" ~ away_team,
        TRUE ~ NA_character_
      ),
      # Track whether this is an EV-based pick or just the blend's favorite
      blend_pick_type = dplyr::case_when(
        !is.na(blend_pick_side) ~ "ev_positive",
        !is.na(blend_favorite_side) ~ "favorite_only",
        TRUE ~ NA_character_
      ),
      # Raw blend probability for the picked side (for display)
      blend_prob_pick = dplyr::case_when(
        blend_pick_side == "home" ~ blend_home_prob,
        blend_pick_side == "away" ~ blend_away_prob,
        # For Pass games, use the favorite's probability
        blend_favorite_side == "home" ~ blend_home_prob,
        blend_favorite_side == "away" ~ blend_away_prob,
        TRUE ~ NA_real_
      ),
      # Shrunk probability for the picked side (for betting decisions)
      blend_prob_pick_shrunk = dplyr::case_when(
        blend_pick_side == "home" ~ blend_home_prob_shrunk,
        blend_pick_side == "away" ~ blend_away_prob_shrunk,
        blend_favorite_side == "home" ~ blend_home_prob_shrunk,
        blend_favorite_side == "away" ~ blend_away_prob_shrunk,
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
          market_home_prob_raw,
          american_to_probability(market_home_ml)
        ),
        blend_pick_side == "away" ~ dplyr::coalesce(
          market_away_prob,
          market_away_prob_raw,
          american_to_probability(market_away_ml)
        ),
        # For Pass games, use market prob for blend's favorite
        blend_favorite_side == "home" ~ dplyr::coalesce(
          market_home_prob,
          market_home_prob_raw,
          american_to_probability(market_home_ml)
        ),
        blend_favorite_side == "away" ~ dplyr::coalesce(
          market_away_prob,
          market_away_prob_raw,
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
      # FIXED: Override blend_beats_market for Pass/No Play recommendations
      # Passing on negative EV is CORRECT behavior, not a "loss" to the market
      blend_beats_market = dplyr::case_when(
        blend_recommendation %in% c("Pass", "No Play") ~ NA,
        TRUE ~ blend_beats_market
      ),
      blend_beats_market_basis = dplyr::case_when(
        blend_recommendation %in% c("Pass", "No Play") ~ NA_character_,
        TRUE ~ blend_beats_market_basis
      ),
      # Add explanatory note for Pass recommendations with negative EV
      blend_beats_market_note_raw = dplyr::case_when(
        blend_recommendation == "Pass" & is.finite(blend_ev_units) & blend_ev_units < 0 ~
          sprintf("Pass: correctly avoided %.1f%% negative EV", abs(blend_ev_units * 100)),
        blend_recommendation == "No Play" ~
          "No Play: insufficient data",
        TRUE ~ blend_beats_market_note_raw
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
      # ===========================================================================
      # CONSERVATIVE KELLY STAKING
      # ===========================================================================
      # Use 1/8 Kelly with edge skepticism to account for model uncertainty.
      # Larger "edges" are more likely model errors, so we apply progressive
      # penalties for implausibly large edges.

      # Raw Kelly (for reference only - NOT used for staking)
      blend_kelly_fraction_raw = dplyr::case_when(
        is.na(blend_pick_side) ~ NA_real_,
        TRUE ~ {
          dec <- american_to_decimal(market_moneyline)
          b <- dec - 1
          stake <- (blend_prob_pick * b - (1 - blend_prob_pick)) / b
          invalid <- is.na(dec) | !is.finite(dec) | b <= 0 | abs(b) < 1e-6 | is.na(stake)
          stake[invalid] <- NA_real_
          stake
        }
      ),

      # Conservative Kelly using SHRUNK probabilities
      # Uses 1/8 Kelly (12.5%), max 10% believable edge, max 2% stake
      blend_confidence = dplyr::case_when(
        is.na(blend_pick_side) ~ NA_real_,
        TRUE ~ conservative_kelly_stake(
          prob = blend_prob_pick_shrunk,
          odds = market_moneyline,
          kelly_fraction = 0.125,   # 1/8 Kelly
          max_edge = 0.10,          # Cap believable edge at 10%
          max_stake = 0.02          # Max 2% of bankroll per bet
        )
      ),

      # Edge magnitude classification for display warnings
      edge_classification = classify_edge_magnitude(blend_ev_units),
      edge_classification_raw = classify_edge_magnitude(
        dplyr::case_when(
          blend_pick_side == "home" ~ blend_ev_units_home_raw,
          blend_pick_side == "away" ~ blend_ev_units_away_raw,
          TRUE ~ NA_real_
        )
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

  # ===========================================================================
  # INVARIANT CHECKS: Validate EV/pick/odds alignment

  # These assertions ensure the calculations are internally consistent
  # Enable debug trace with: options(nfl.ev_debug = TRUE)
  # ===========================================================================

  ev_debug <- getOption("nfl.ev_debug", default = FALSE)

  if (ev_debug && nrow(combined) > 0) {
    message("\n=== EV CALCULATION DEBUG TRACE ===")
    sample_rows <- head(combined, 3)
    for (i in seq_len(nrow(sample_rows))) {
      r <- sample_rows[i, ]
      message(sprintf("\nGame: %s @ %s", r$away_team, r$home_team))
      message(sprintf("  Pick side: %s", ifelse(is.na(r$blend_pick_side), "NA (Pass)", r$blend_pick_side)))
      message(sprintf("  Shrunk prob (pick): %.4f", ifelse(is.na(r$blend_prob_pick_shrunk), NA, r$blend_prob_pick_shrunk)))
      message(sprintf("  Market moneyline: %+d", ifelse(is.na(r$market_moneyline), NA, as.integer(r$market_moneyline))))
      message(sprintf("  Decimal odds: %.4f", american_to_decimal(r$market_moneyline)))
      message(sprintf("  Expected EV: %.4f", r$blend_prob_pick_shrunk * american_to_decimal(r$market_moneyline) - 1))
      message(sprintf("  Actual EV: %.4f", r$blend_ev_units))
      message(sprintf("  Recommendation: %s", r$blend_recommendation))
    }
    message("\n=== END DEBUG TRACE ===\n")
  }

  # Check 1: For Bet recommendations (positive EV), verify pick_side matches odds
  bet_rows <- !is.na(combined$blend_pick_side) & combined$blend_ev_units > 0
  if (any(bet_rows, na.rm = TRUE)) {
    # Verify home pick uses home odds
    home_picks <- combined$blend_pick_side == "home" & bet_rows
    if (any(home_picks, na.rm = TRUE)) {
      odds_match <- all(
        combined$market_moneyline[home_picks] == combined$market_home_ml[home_picks],
        na.rm = TRUE
      )
      if (!odds_match) {
        warning("INVARIANT VIOLATION: Home picks using wrong odds! Check market_moneyline vs market_home_ml")
      }
    }

    # Verify away pick uses away odds
    away_picks <- combined$blend_pick_side == "away" & bet_rows
    if (any(away_picks, na.rm = TRUE)) {
      odds_match <- all(
        combined$market_moneyline[away_picks] == combined$market_away_ml[away_picks],
        na.rm = TRUE
      )
      if (!odds_match) {
        warning("INVARIANT VIOLATION: Away picks using wrong odds! Check market_moneyline vs market_away_ml")
      }
    }
  }

  # Check 2: Verify EV calculation matches formula: EV = prob × decimal_odds - 1
  # Allow small tolerance for floating point
  if (any(bet_rows, na.rm = TRUE)) {
    ev_tolerance <- 0.001
    decimal_odds <- american_to_decimal(combined$market_moneyline[bet_rows])
    expected_ev <- combined$blend_prob_pick_shrunk[bet_rows] * decimal_odds - 1
    actual_ev <- combined$blend_ev_units[bet_rows]
    ev_mismatch <- abs(expected_ev - actual_ev) > ev_tolerance
    if (any(ev_mismatch, na.rm = TRUE)) {
      n_mismatches <- sum(ev_mismatch, na.rm = TRUE)
      warning(sprintf(
        "INVARIANT VIOLATION: %d games have EV mismatch! Expected EV != prob × odds - 1",
        n_mismatches
      ))
    }
  }

  # Check 3: Sanity bounds - if shrunk prob is low (<25%), EV can't be huge (>20%)
  # unless odds are extremely high (which we'll warn about)
  extreme_ev_low_prob <- bet_rows &
    !is.na(combined$blend_prob_pick_shrunk) &
    combined$blend_prob_pick_shrunk < 0.25 &
    combined$blend_ev_units > 0.20
  if (any(extreme_ev_low_prob, na.rm = TRUE)) {
    n_extreme <- sum(extreme_ev_low_prob, na.rm = TRUE)
    message(sprintf(
      "INFO: %d games have low probability (<25%%) but high EV (>20%%). These are underdog bets - verify odds are correct.",
      n_extreme
    ))
  }

  combined
}



moneyline_report_schema_contract <- function() {
  tibble::tribble(
    ~column, ~type, ~meaning,
    "Season", "numeric", "Season identifier for the game.",
    "Week", "numeric", "Week identifier for the game.",
    "Date", "Date", "Game date.",
    "Matchup", "character", "Away @ Home matchup string.",
    "Winner", "character", "Observed winner or TBD.",
    "Blend Pick", "character", "Model-selected side for display.",
    "Blend Recommendation", "character", "Bet/Pass recommendation.",
    "Pass Reason", "character", "Reason recommendation was forced to pass.",
    "Blend Beat Market?", "character", "Outcome of blend pick vs market benchmark.",
    "Blend Beat Market Basis", "character", "Basis used to judge blend vs market.",
    "Raw Kelly (%)", "numeric", "Uncapped Kelly fraction for audit.",
    "Capped Stake (%)", "numeric", "Capped Kelly stake after skepticism and caps.",
    "Final Stake (%)", "numeric", "Final stake after governance (0 if Pass).",
    "Min Stake (%)", "numeric", "Minimum stake threshold used by governance.",
    "Blend Stake (Units)", "numeric", "Final capped stake in bankroll units.",
    "EV Edge (Raw)", "numeric", "Expected value edge for displayed pick (uncapped).",
    "EV Edge (Displayed, Capped)", "numeric", "Display-only EV edge capped at MAX_EDGE.",
    "Total EV (Units)", "numeric", "Stake-weighted expected value.",
    "Edge Quality", "character", "Edge quality tier label.",
    "Blend Pick Win % (Shrunk)", "numeric", "Shrunk blend probability for picked side.",
    "Market Pick Win % (Devig)", "numeric", "Market fair probability for picked side.",
    "Prob Edge on Pick (pp)", "numeric", "Blend minus market probability gap.",
    "Blend Home Win % (Shrunk)", "numeric", "Shrunk blend home win probability.",
    "Market Home Win % (Fair, Devig=proportional)", "numeric", "Devigged market home win probability using proportional method.",
    "ML Implied Home % (Raw)", "numeric", "Raw implied home win probability from moneyline odds.",
    "Blend Median Margin", "numeric", "Blend median expected home margin.",
    "Market Home Spread", "numeric", "Market home spread line.",
    "Blend Total", "numeric", "Blend median total points.",
    "Market Total", "numeric", "Market total points line.",
    "Total O/U", "character", "Over/Under lean from blend vs market total.",
    "Market Home Moneyline", "numeric", "Market home moneyline.",
    "Market Away Moneyline", "numeric", "Market away moneyline.",
    "Blend Home ML (Fair, from Shrunk Prob)", "numeric", "Blend home moneyline from shrunk probability.",
    "Blend Home ML (Vigged, +X%)", "numeric", "Blend home moneyline after vig adjustment.",
    "Blend Away Moneyline (Vigged)", "numeric", "Blend away moneyline after vig adjustment."
  )
}

validate_moneyline_report_schema <- function(tbl, strict = TRUE) {
  schema <- moneyline_report_schema_contract()
  expected_cols <- schema$column

  missing_cols <- setdiff(expected_cols, names(tbl))
  if (length(missing_cols)) {
    stop(sprintf(
      "Moneyline report schema violation: missing columns: %s",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }

  if (isTRUE(strict)) {
    unexpected_cols <- setdiff(names(tbl), expected_cols)
    if (length(unexpected_cols)) {
      stop(sprintf(
        "Moneyline report schema violation: unexpected columns: %s",
        paste(unexpected_cols, collapse = ", ")
      ), call. = FALSE)
    }
  }

  for (i in seq_len(nrow(schema))) {
    col_name <- schema$column[[i]]
    expected_type <- schema$type[[i]]
    values <- tbl[[col_name]]

    type_ok <- switch(
      expected_type,
      numeric = is.numeric(values),
      character = is.character(values),
      Date = inherits(values, "Date"),
      logical = is.logical(values),
      FALSE
    )

    if (!isTRUE(type_ok)) {
      actual_type <- paste(class(values), collapse = "/")
      stop(sprintf(
        "Moneyline report schema violation: column '%s' expected type '%s' but got '%s'.",
        col_name, expected_type, actual_type
      ), call. = FALSE)
    }
  }

  invisible(tbl)
}

export_moneyline_comparison_html <- function(comparison_tbl,
                                             file = NULL,
                                             title = "Blend vs Market Moneylines",
                                             verbose = TRUE,
                                             auto_open = TRUE,
                                             season = NULL,
                                             week = NULL,
                                             props_data = NULL) {
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
    # Generate informational HTML instead of crashing
    if (verbose) {
      message("export_moneyline_comparison_html(): No games to display - generating placeholder report.")
    }

    empty_html <- paste0(
      '<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8">',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0">',
      '<title>NFL Model vs Market - No Games</title>',
      '<style>',
      'body { font-family: system-ui, sans-serif; background: #1a1a2e; color: #f0f0f0; ',
      'display: flex; justify-content: center; align-items: center; min-height: 100vh; margin: 0; }',
      '.container { text-align: center; padding: 2rem; background: rgba(255,255,255,0.05); ',
      'border-radius: 1rem; max-width: 500px; }',
      'h1 { color: #667eea; margin-bottom: 1rem; }',
      'p { line-height: 1.6; opacity: 0.8; }',
      '.icon { font-size: 4rem; margin-bottom: 1rem; }',
      '</style></head><body>',
      '<div class="container">',
      '<div class="icon">📊</div>',
      '<h1>No Games Available</h1>',
      '<p>No games were found for the selected week. This could mean:</p>',
      '<ul style="text-align: left; opacity: 0.8;">',
      '<li>The schedule data is not yet available</li>',
      '<li>Market odds have not been published</li>',
      '<li>All games were filtered out during processing</li>',
      '<li>The week number may be invalid</li>',
      '</ul>',
      '<p style="margin-top: 1rem; font-size: 0.9rem;">',
      'Generated: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), '</p>',
      '</div></body></html>'
    )

    tryCatch({
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
      writeLines(empty_html, file)
      if (verbose) {
        message(sprintf("  → Placeholder report saved to: %s", file))
      }
      return(invisible(file))
    }, error = function(e) {
      warning(sprintf("Could not write placeholder report: %s", e$message), call. = FALSE)
      return(invisible(NULL))
    })
  }

  # =============================================================================
  # PRE-EXPORT VALIDATION
  # =============================================================================
  # Check for required columns, valid data, and common issues that cause empty tables

  required_cols <- c("game_id", "home_team", "away_team")
  missing_required <- setdiff(required_cols, names(comparison_tbl))
  if (length(missing_required) > 0) {
    if (verbose) {
      warning(sprintf(
        "export_moneyline_comparison_html(): Missing required columns: %s. Report may be incomplete.",
        paste(missing_required, collapse = ", ")
      ), call. = FALSE)
    }
  }

  # Check for odds/probability columns
  odds_cols <- c("market_home_ml", "blend_home_prob", "market_home_prob")
  has_odds <- any(odds_cols %in% names(comparison_tbl))
  if (!has_odds && verbose) {
    warning("export_moneyline_comparison_html(): No odds or probability columns found. EV calculations will be unavailable.", call. = FALSE)
  }

  # Check for EV columns and validate values
  ev_cols <- intersect(c("blend_ev_units", "blend_ev", "ev_edge"), names(comparison_tbl))
  if (length(ev_cols) > 0) {
    for (col in ev_cols) {
      col_data <- comparison_tbl[[col]]
      if (all(is.na(col_data))) {
        if (verbose) warning(sprintf("export_moneyline_comparison_html(): Column '%s' is entirely NA", col), call. = FALSE)
      } else if (is.numeric(col_data)) {
        finite_vals <- col_data[is.finite(col_data)]
        if (length(finite_vals) > 0 && any(abs(finite_vals) > 1)) {
          # EV values over 100% are suspicious
          suspicious <- sum(abs(finite_vals) > 1)
          if (suspicious > 0 && verbose) {
            message(sprintf("export_moneyline_comparison_html(): %d rows have |EV| > 100%% in '%s' - verify data integrity", suspicious, col))
          }
        }
      }
    }
  }

  # Validate probability columns are in 0-1 range
  prob_cols <- intersect(c("blend_home_prob", "market_home_prob", "blend_pick_prob", "shrunk_prob"), names(comparison_tbl))
  for (col in prob_cols) {
    col_data <- comparison_tbl[[col]]
    if (is.numeric(col_data)) {
      out_of_range <- sum(!is.na(col_data) & (col_data < 0 | col_data > 1), na.rm = TRUE)
      if (out_of_range > 0 && verbose) {
        warning(sprintf("export_moneyline_comparison_html(): %d values in '%s' outside [0,1] range", out_of_range, col), call. = FALSE)
      }
    }
  }

  # Summary diagnostic
  if (verbose) {
    message(sprintf(
      "export_moneyline_comparison_html(): Exporting %d games (%d columns) to %s",
      nrow(comparison_tbl), ncol(comparison_tbl), basename(file)
    ))
  }
  # =============================================================================

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
    "Blend Home ML (Fair, from Shrunk Prob)",
    "Blend Home ML (Vigged, +X%)",
    "Blend Away Moneyline (Vigged)",
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

  # Professional, concise intro section - designed for quick reference
  intro_html <- paste0(
    "<section class=\"report-intro\">",
    "<h2>NFL Model vs Market Analysis</h2>",
    "<p class=\"report-subtitle\">Probabilistic model comparison with professional risk management</p>",

    "<div class=\"intro-section warning-box\">",
    "<h3>Risk Management Applied</h3>",
    "<p>This report uses conservative settings appropriate for efficient markets:</p>",
    "<ul>",
    "<li><strong>60% Shrinkage</strong> — Model probability pulled 60% toward market consensus (e.g., Model: 65% → Shrunk: 59% if market is 55%)</li>",
    "<li><strong>1/8 Kelly</strong> — Fractional staking (accounts for estimation error)</li>",
    "<li><strong>2% Max Stake</strong> — Position size cap per game</li>",
    "</ul>",
    "<p class=\"edge-warning\">Edge Guide: 0-5% = realistic | 5-10% = optimistic | 10%+ = likely model error</p>",
    "</div>",

    "<div class=\"intro-section\">",
    "<h3>Key Columns</h3>",
        "<table class=\"metrics-table\">",
    "<tr><td><strong>EV Edge (Raw)</strong></td><td>Expected return per unit for the displayed pick: (Shrunk Prob x Decimal Odds) - 1</td></tr>",
    "<tr><td><strong>EV Edge (Displayed, Capped)</strong></td><td>Display-only cap of EV Edge (Raw) at 10% for readability; governance uses raw EV.</td></tr>",
    "<tr><td><strong>Min Stake (%)</strong></td><td>Minimum stake threshold required to place a bet (default 1%).</td></tr>",
    "<tr><td><strong>Final Stake (%)</strong></td><td>Kelly-based stake after caps; zero when Pass.</td></tr>",
    "<tr><td><strong>Total EV</strong></td><td>Expected profit: Final Stake x EV Edge (Raw)</td></tr>",
    "<tr><td><strong>Blend Pick</strong></td><td>Model favorite (* = Pass game, no positive EV)</td></tr>",
    "<tr><td><strong>Blend Pick Win % (Shrunk)</strong></td><td>Shrunk probability for the displayed pick (used for EV)</td></tr>",
    "<tr><td><strong>Market Pick Win % (Devig)</strong></td><td>Devig market win probability for the displayed pick</td></tr>",
    "<tr><td><strong>Market Home Win % (Fair, Devig=proportional)</strong></td><td>Vig-free market home win probability (proportional devig)</td></tr>",
    "<tr><td><strong>ML Implied Home % (Raw)</strong></td><td>Raw implied probability from the home moneyline before devig</td></tr>",
    "<tr><td><strong>Blend Home ML (Fair, from Shrunk Prob)</strong></td><td>Computed as probability_to_american(blend_home_prob_shrunk)</td></tr>",
    "<tr><td><strong>Blend Home ML (Vigged, +X%)</strong></td><td>Computed from fair ML using apply_moneyline_vig(fair_ml, vig); excluded from coherence checks</td></tr>",
    "</table>",

    "</div>",

    "<div class=\"intro-section\">",
    "<h3>Interpreting Results</h3>",
    "<p><strong>\"Blend Beat Market?\"</strong> compares assessment quality, not which team was favored:</p>",
    "<ul class=\"compact-list\">",
    "<li><span class=\"result-yes\">Yes</span> — Model's probability estimate was more accurate than market's</li>",
    "<li><span class=\"result-no\">No</span> — Market's assessment was better calibrated</li>",
    "<li><strong>Pass</strong> — No positive EV bet (correct behavior for negative edge games)</li>",
    "</ul>",
    "</div>",

    "<div class=\"intro-section color-legend\">",
    "<h3>Color Coding</h3>",
    "<span class=\"color-chip green\"></span> Positive edge / Blend wins ",
    "<span class=\"color-chip coral\"></span> Recommended action ",
    "<span class=\"color-chip muted\"></span> Market wins / No edge ",
    "<span class=\"color-chip gray\"></span> N/A",
    "</div>",

    "<div class=\"intro-section\">",
    "<h3>Example Game Interpretation</h3>",
    "<p>If you see: <strong>EV Edge = 4.2%</strong>, <strong>Edge Quality = ✓ OK</strong>, <strong>Stake = 0.015</strong></p>",
    "<ul class=\"compact-list\">",
    "<li>Model finds 4.2% expected return per unit bet</li>",
    "<li>Edge is in the realistic range (0-5%)</li>",
    "<li>Recommended stake: 0.015 units (1.5% of 1/8 Kelly)</li>",
    "</ul>",
    "<p><strong>* = Pass game</strong>: No positive EV found; the asterisk shows the favorite but recommends not betting.</p>",
    "</div>",
    "</section>"
  )

  # Add data quality badge if available
  quality_badge_html <- ""
  if (exists("generate_quality_badge_html", mode = "function")) {
    quality_badge_html <- tryCatch(
      generate_quality_badge_html(),
      error = function(e) ""
    )
  }
  if (nzchar(quality_badge_html)) {
    intro_html <- paste0(intro_html, quality_badge_html)
  }

  # Governance constants for ordered stake decisions
  MIN_STAKE_THRESHOLD <- 0.01
  MAX_EDGE_THRESHOLD <- get0("MAX_EDGE", ifnotfound = 0.10, inherits = TRUE)
  KELLY_FRACTION_USE <- get0("KELLY_FRACTION", ifnotfound = 0.125, inherits = TRUE)
  MAX_STAKE_USE <- get0("MAX_STAKE", ifnotfound = 0.02, inherits = TRUE)

  # === TYPE GUARDS: Ensure probability columns are numeric ===
  # These guards prevent column type contamination
  ensure_numeric_prob <- function(x) {
    if (is.character(x) || !is.numeric(x)) {
      warning("Column type contamination detected: non-numeric value in probability column")
      # Try to coerce, replacing non-numeric with NA
      result <- suppressWarnings(as.numeric(x))
      return(result)
    }
    as.numeric(x)
  }

  # Detect placeholder/future game data with missing market odds
  # These games should show Pass with clear indication
  if ("market_home_ml" %in% names(comparison_tbl) && "market_away_ml" %in% names(comparison_tbl)) {
    missing_odds <- is.na(comparison_tbl$market_home_ml) | is.na(comparison_tbl$market_away_ml)
    if (any(missing_odds)) {
      n_missing <- sum(missing_odds)
      message(sprintf(
        "ℹ %d game(s) have missing market odds - showing as Pass (No Odds)",
        n_missing
      ))
    }
  }

  comparison_tbl <- ensure_columns_with_defaults(comparison_tbl, list(
    market_home_prob_fair = NA_real_,
    market_away_prob_fair = NA_real_,
    market_home_prob_raw = NA_real_,
    market_away_prob_raw = NA_real_
  ))

  display_tbl <- comparison_tbl %>%
    dplyr::mutate(
      # Ensure all probability columns are numeric (guard against contamination)
      # CRITICAL FIX: Use SHRUNK probabilities for display (matches EV calculation)
      blend_prob_pick_shrunk_safe = ensure_numeric_prob(blend_prob_pick_shrunk),
      blend_prob_pick_raw_safe = ensure_numeric_prob(blend_prob_pick),
      market_prob_pick_safe = ensure_numeric_prob(market_prob_pick),
      blend_home_prob_safe = ensure_numeric_prob(blend_home_prob),
      blend_home_prob_shrunk_safe = ensure_numeric_prob(blend_home_prob_shrunk),
      market_home_prob_safe = ensure_numeric_prob(dplyr::coalesce(market_home_prob_fair, market_home_prob)),
      ml_implied_home_prob_safe = ensure_numeric_prob(dplyr::coalesce(ml_implied_home_prob, market_home_prob_raw)),

      # CRITICAL FIX: For Pass games, calculate EV for the DISPLAYED pick (favorite)
      # not the best EV side, to maintain consistency between columns
      display_ev_raw = dplyr::case_when(
        # For Bet games (positive EV), use blend_ev_units (already correct)
        !is.na(blend_pick_side) & blend_ev_units > 0 ~ blend_ev_units,
        # For Pass games, calculate EV for the favorite (which is what we display)
        blend_favorite_side == "home" ~ expected_value_units(blend_home_prob_shrunk, market_home_ml),
        blend_favorite_side == "away" ~ expected_value_units(blend_away_prob_shrunk, market_away_ml),
        TRUE ~ blend_ev_units
      ),
      display_ev_capped = dplyr::case_when(
        is.na(display_ev_raw) ~ NA_real_,
        TRUE ~ pmax(pmin(display_ev_raw, MAX_EDGE_THRESHOLD), -MAX_EDGE_THRESHOLD)
      ),

      missing_market_odds = is.na(market_home_ml) | is.na(market_away_ml) |
        market_home_ml == 0 | market_away_ml == 0,

      governance = purrr::pmap(
        list(
          ev = display_ev_raw,
          prob = blend_prob_pick_shrunk,
          odds = market_moneyline,
          is_placeholder_odds = missing_market_odds
        ),
        ~ apply_bet_governance(
          ev = ..1,
          prob = ..2,
          odds = ..3,
          min_stake = MIN_STAKE_THRESHOLD,
          kelly_fraction = KELLY_FRACTION_USE,
          max_stake = MAX_STAKE_USE,
          max_edge = MAX_EDGE_THRESHOLD,
          is_placeholder_odds = ..4
        )
      ),
      governance_recommendation = purrr::map_chr(governance, ~ .x$recommendation[[1]]),
      governance_raw_kelly = purrr::map_dbl(governance, ~ .x$raw_kelly_pct[[1]]),
      governance_capped_stake = purrr::map_dbl(governance, ~ .x$capped_stake_pct[[1]]),
      governance_final_stake = purrr::map_dbl(governance, ~ .x$final_stake_pct[[1]]),
      pass_reason_raw = purrr::map_chr(governance, ~ .x$pass_reason[[1]]),
      effective_recommendation = dplyr::case_when(
        blend_recommendation %in% c("Pass", "No Play") ~ blend_recommendation,
        governance_recommendation == "Pass" ~ "Pass",
        TRUE ~ blend_recommendation
      ),
      effective_stake = dplyr::case_when(
        effective_recommendation %in% c("Pass", "No Play") ~ 0,
        TRUE ~ governance_final_stake
      ),
      # Show reason when EV is overridden to Pass
      pass_reason = dplyr::case_when(
        effective_recommendation %in% c("Pass", "No Play") & !is.na(pass_reason_raw) & nzchar(pass_reason_raw) ~ pass_reason_raw,
        effective_recommendation %in% c("Pass", "No Play") ~ "Governance pass",
        TRUE ~ ""
      )
    ) %>%
    dplyr::transmute(
      Season = season,
      Week = week,
      Date = game_date,
      Matchup = matchup,
      Winner = dplyr::coalesce(actual_winner, "TBD"),
      # Show blend pick with indicator for Pass games (favorite_only vs ev_positive)
      `Blend Pick` = dplyr::case_when(
        blend_pick_type == "favorite_only" ~ paste0(blend_pick, "*"),
        TRUE ~ blend_pick
      ),
      `Blend Recommendation` = effective_recommendation,
      `Pass Reason` = pass_reason,
      # v2.9.4 G7 fix: TBD games must show N/A (outcome unknown → can't evaluate)
      `Blend Beat Market?` = dplyr::case_when(
        is.na(blend_beats_market) | effective_recommendation %in% c("Pass", "No Play") ~ "N/A",
        is.na(actual_winner) | actual_winner == "TBD" ~ "N/A",
        blend_beats_market ~ "Yes",
        TRUE ~ "No"
      ),
      `Blend Beat Market Basis` = dplyr::if_else(
        effective_recommendation %in% c("Pass", "No Play"),
        NA_character_,
        blend_beats_market_basis
      ),
      `Raw Kelly (%)` = governance_raw_kelly,
      `Capped Stake (%)` = governance_capped_stake,
      `Final Stake (%)` = governance_final_stake,
      `Min Stake (%)` = MIN_STAKE_THRESHOLD,
      `Blend Stake (Units)` = effective_stake,
      # EV Edge: Show EV for the DISPLAYED pick (consistent with Blend Pick column)
      # For Bet games: the positive EV side
      # For Pass games: the favorite's EV (matches displayed team with *)
      # Raw EV drives governance; capped EV is display-only
      `EV Edge (Raw)` = display_ev_raw,
      `EV Edge (Displayed, Capped)` = display_ev_capped,
      # Total EV: 0 for Pass (no bet = no EV), stake × EV for Bet
      `Total EV (Units)` = dplyr::case_when(
        effective_recommendation %in% c("Pass", "No Play") ~ 0,
        is.na(effective_stake) | is.na(display_ev_raw) ~ NA_real_,
        TRUE ~ effective_stake * display_ev_raw
      ),
      # Edge Quality: driven by displayed (capped) EV only for visual flagging
      `Edge Quality` = dplyr::case_when(
        is.na(display_ev_capped) ~ "N/A",
        effective_recommendation %in% c("Pass", "No Play") ~ "Pass",
        display_ev_capped <= 0 ~ "Pass",
        display_ev_capped <= 0.05 ~ "??? OK",
        display_ev_capped < MAX_EDGE_THRESHOLD ~ "??? High",
        TRUE ~ "MODEL ERROR / REVIEW"
      ),
      # Pick-side probabilities - SHRUNK values (matches EV calculation)
      # This ensures: displayed_prob × decimal_odds - 1 ≈ displayed_EV
      `Blend Pick Win % (Shrunk)` = blend_prob_pick_shrunk_safe,
      `Market Pick Win % (Devig)` = market_prob_pick_safe,
      # Prob Edge on Pick: shrunk blend - market probability difference
      `Prob Edge on Pick (pp)` = dplyr::case_when(
        is.na(blend_prob_pick_shrunk_safe) | is.na(market_prob_pick_safe) ~ NA_real_,
        TRUE ~ blend_prob_pick_shrunk_safe - market_prob_pick_safe
      ),
      `Blend Home Win % (Shrunk)` = blend_home_prob_shrunk_safe,
      `Market Home Win % (Fair, Devig=proportional)` = market_home_prob_safe,
      `ML Implied Home % (Raw)` = ml_implied_home_prob_safe,
      `Blend Median Margin` = blend_median_margin,
      `Market Home Spread` = market_home_spread,
      `Blend Total` = blend_total_median,
      `Market Total` = market_total,
      `Total O/U` = dplyr::case_when(
        is.na(blend_total_median) | is.na(market_total) ~ "N/A",
        blend_total_median > market_total + 0.5 ~ "OVER",
        blend_total_median < market_total - 0.5 ~ "UNDER",
        TRUE ~ "PUSH"
      ),
      `Market Home Moneyline` = market_home_ml,
      `Market Away Moneyline` = market_away_ml,
      `Blend Home ML (Fair, from Shrunk Prob)` = blend_home_ml,
      `Blend Home ML (Vigged, +X%)` = blend_home_ml_vig,
      `Blend Away Moneyline (Vigged)` = blend_away_ml_vig
    )

  plot_to_base64_img <- function(plot_fn, width = 900, height = 520, title = NULL) {
    if (!requireNamespace("base64enc", quietly = TRUE) ||
        !requireNamespace("htmltools", quietly = TRUE)) {
      return(NULL)
    }
    tryCatch({
      tmp <- tempfile(fileext = ".png")
      grDevices::png(filename = tmp, width = width, height = height, res = 140, bg = "transparent")
      on.exit({
        if (grDevices::dev.cur() > 1) {
          grDevices::dev.off()
        }
      }, add = TRUE)
      plot_fn()
      if (grDevices::dev.cur() > 1) {
        grDevices::dev.off()
      }
      uri <- base64enc::dataURI(file = tmp, mime = "image/png")
      unlink(tmp)
      alt_text <- if (!is.null(title)) title else "chart"
      htmltools::tags$div(
        class = "chart-card",
        if (!is.null(title)) htmltools::tags$div(class = "chart-title", title),
        htmltools::tags$img(src = uri, class = "chart-img", alt = alt_text)
      )
    }, error = function(e) NULL)
  }

  build_games_charts <- function(tbl) {
    if (!requireNamespace("htmltools", quietly = TRUE)) return(NULL)
    market_col <- "Market Home Win % (Fair, Devig=proportional)"
    model_col <- "Blend Home Win % (Shrunk)"
    ev_col <- "EV Edge (Raw)"
    if (!all(c(market_col, model_col, ev_col) %in% names(tbl))) return(NULL)

    chart_df <- data.frame(
      market = as.numeric(tbl[[market_col]]),
      model = as.numeric(tbl[[model_col]]),
      ev = as.numeric(tbl[[ev_col]])
    )
    chart_df <- chart_df[is.finite(chart_df$market) & is.finite(chart_df$model), , drop = FALSE]
    ev_df <- data.frame(ev = as.numeric(tbl[[ev_col]]))
    ev_df <- ev_df[is.finite(ev_df$ev), , drop = FALSE]

    htmltools::tags$div(
      class = "charts-row",
      plot_to_base64_img(function() {
        par(mar = c(4, 4, 2, 1), bg = "transparent", fg = "#F5F4F0", col.axis = "#C9C5BE", col.lab = "#C9C5BE")
        plot(chart_df$market, chart_df$model,
             pch = 19, col = "#FCA5A5", xlab = "Market Home Win %", ylab = "Blend Home Win %",
             xlim = c(0, 1), ylim = c(0, 1), axes = FALSE)
        abline(0, 1, lty = 2, col = "#E89A7A")
        axis(1, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"), col = "#C9C5BE", col.axis = "#C9C5BE")
        axis(2, at = seq(0, 1, 0.2), labels = paste0(seq(0, 100, 20), "%"), col = "#C9C5BE", col.axis = "#C9C5BE")
        box(col = "#E89A7A")
      }, title = "Model vs Market Win %"),
      plot_to_base64_img(function() {
        par(mar = c(4, 4, 2, 1), bg = "transparent", fg = "#F5F4F0", col.axis = "#C9C5BE", col.lab = "#C9C5BE")
        hist(ev_df$ev, col = "#D97757", border = NA, breaks = 12, main = "", xlab = "EV Edge (Raw)",
             axes = FALSE)
        abline(v = 0, lty = 2, col = "#FCA5A5")
        axis(1, at = pretty(ev_df$ev), labels = paste0(round(pretty(ev_df$ev) * 100), "%"),
             col = "#C9C5BE", col.axis = "#C9C5BE")
        axis(2, col = "#C9C5BE", col.axis = "#C9C5BE")
        box(col = "#E89A7A")
      }, title = "EV Edge Distribution")
    )
  }

  build_props_charts <- function(props_df) {
    if (is.null(props_df) || !is.data.frame(props_df)) return(NULL)
    if (!requireNamespace("htmltools", quietly = TRUE)) return(NULL)
    props_df$ev_over <- suppressWarnings(as.numeric(props_df$ev_over))
    props_df$ev_under <- suppressWarnings(as.numeric(props_df$ev_under))
    props_df$p_over <- suppressWarnings(as.numeric(props_df$p_over))
    props_df$line <- suppressWarnings(as.numeric(props_df$line))
    props_df$projection <- suppressWarnings(as.numeric(props_df$projection))

    props_df$best_ev <- pmax(props_df$ev_over, props_df$ev_under, na.rm = TRUE)
    ev_df <- props_df[is.finite(props_df$best_ev), , drop = FALSE]
    p_df <- props_df[is.finite(props_df$p_over), , drop = FALSE]
    scatter_df <- props_df[
      is.finite(props_df$line) & is.finite(props_df$projection) &
        props_df$prop_type != "anytime_td",
      , drop = FALSE
    ]

    rec_df <- as.data.frame(table(props_df$prop_type, props_df$recommendation), stringsAsFactors = FALSE)
    names(rec_df) <- c("prop_type", "recommendation", "n")

    chart_nodes <- list()

    if (nrow(scatter_df) > 0) {
      chart_nodes[[length(chart_nodes) + 1]] <- plot_to_base64_img(function() {
        par(mar = c(4, 4, 2, 1), bg = "transparent", fg = "#F5F4F0", col.axis = "#C9C5BE", col.lab = "#C9C5BE")
        plot(scatter_df$line, scatter_df$projection,
             pch = 19, col = "#60A5FA", xlab = "Market Line", ylab = "Model Projection",
             axes = FALSE)
        axis(1, col = "#C9C5BE", col.axis = "#C9C5BE")
        axis(2, col = "#C9C5BE", col.axis = "#C9C5BE")
        abline(0, 1, lty = 2, col = "#FCA5A5")
        box(col = "#E89A7A")
      }, title = "Projection vs Market Line")
    }

    if (nrow(ev_df) > 0) {
      chart_nodes[[length(chart_nodes) + 1]] <- plot_to_base64_img(function() {
        par(mar = c(4, 4, 2, 1), bg = "transparent", fg = "#F5F4F0", col.axis = "#C9C5BE", col.lab = "#C9C5BE")
        hist(ev_df$best_ev, col = "#10b981", border = NA, breaks = 12, main = "", xlab = "Best EV (Over/Under)",
             axes = FALSE)
        abline(v = 0, lty = 2, col = "#FCA5A5")
        axis(1, at = pretty(ev_df$best_ev), labels = paste0(round(pretty(ev_df$best_ev) * 100), "%"),
             col = "#C9C5BE", col.axis = "#C9C5BE")
        axis(2, col = "#C9C5BE", col.axis = "#C9C5BE")
        box(col = "#E89A7A")
      }, title = "Props EV Distribution")
    } else if (nrow(p_df) > 0) {
      chart_nodes[[length(chart_nodes) + 1]] <- plot_to_base64_img(function() {
        par(mar = c(4, 4, 2, 1), bg = "transparent", fg = "#F5F4F0", col.axis = "#C9C5BE", col.lab = "#C9C5BE")
        hist(p_df$p_over, col = "#10b981", border = NA, breaks = 12, main = "", xlab = "P(Over/Yes)",
             axes = FALSE)
        abline(v = 0.5, lty = 2, col = "#FCA5A5")
        axis(1, at = pretty(p_df$p_over), labels = paste0(round(pretty(p_df$p_over) * 100), "%"),
             col = "#C9C5BE", col.axis = "#C9C5BE")
        axis(2, col = "#C9C5BE", col.axis = "#C9C5BE")
        box(col = "#E89A7A")
      }, title = "P(Over/Yes) Distribution")
    }

    if (nrow(rec_df) > 0) {
      chart_nodes[[length(chart_nodes) + 1]] <- plot_to_base64_img(function() {
        par(mar = c(6, 4, 2, 1), bg = "transparent", fg = "#F5F4F0", col.axis = "#C9C5BE", col.lab = "#C9C5BE")
        types <- unique(rec_df$prop_type)
        rec_levels <- unique(rec_df$recommendation)
        counts <- matrix(0, nrow = length(rec_levels), ncol = length(types),
                         dimnames = list(rec_levels, types))
        for (i in seq_len(nrow(rec_df))) {
          counts[rec_df$recommendation[i], rec_df$prop_type[i]] <- rec_df$n[i]
        }
        bp <- barplot(counts, beside = FALSE,
                      col = c("#D97757", "#10b981", "#FCA5A5", "#4A4640")[seq_len(nrow(counts))],
                      ylab = "Count", xaxt = "n")
        axis(1, at = bp, labels = types, las = 2, col = "#C9C5BE", col.axis = "#C9C5BE")
        axis(2, col = "#C9C5BE", col.axis = "#C9C5BE")
        box(col = "#E89A7A")
      }, title = "Recommendations by Prop Type")
    }

    if (!length(chart_nodes)) return(NULL)

    htmltools::tags$div(
      class = "charts-row",
      htmltools::tagList(chart_nodes)
    )
  }

  # === FINAL TYPE VALIDATION ===
  # Verify all probability columns are numeric (fail early if contaminated)
  prob_cols <- c("Blend Pick Win % (Shrunk)", "Market Pick Win % (Devig)", "Prob Edge on Pick (pp)",
                 "Blend Home Win % (Shrunk)", "Market Home Win % (Fair, Devig=proportional)", "ML Implied Home % (Raw)")
  for (col in prob_cols) {
    if (col %in% names(display_tbl)) {
      if (!is.numeric(display_tbl[[col]])) {
        warning(sprintf("COLUMN TYPE ERROR: '%s' contains non-numeric values", col))
        display_tbl[[col]] <- as.numeric(display_tbl[[col]])
      }
    }
  }

  games_charts_html <- tryCatch(build_games_charts(display_tbl), error = function(e) NULL)

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
      c("EV Edge (Raw)", "EV Edge (Displayed, Capped)", "Prob Edge on Pick (pp)",
        "Raw Kelly (%)", "Capped Stake (%)", "Final Stake (%)", "Min Stake (%)"),
      gt::fmt_percent,
      decimals = 2
    )
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c("Blend Home Win % (Shrunk)", "Market Home Win % (Fair, Devig=proportional)", "ML Implied Home % (Raw)",
        "Blend Pick Win % (Shrunk)", "Market Pick Win % (Devig)"),
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
    # Total EV with high precision (5 decimals), Stake with 4 decimals
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c("Total EV (Units)"),
      gt::fmt_number,
      decimals = 5,
      drop_trailing_zeros = FALSE
    )
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c("Blend Stake (Units)"),
      gt::fmt_number,
      decimals = 4,
      drop_trailing_zeros = FALSE
    )
    gt_tbl <- gt_apply_if_columns(
      gt_tbl,
      c(
        "Market Home Moneyline", "Market Away Moneyline",
        "Blend Home ML (Fair, from Shrunk Prob)", "Blend Home ML (Vigged, +X%)",
        "Blend Away Moneyline (Vigged)"
      ),
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

    # Helper function to apply color coding safely
    # Uses scales::squish to clamp values to domain range, avoiding warnings
    apply_color <- function(gt_tbl, columns, palette, domain) {
      cols_exist <- if (length(columns) == 1) {
        columns %in% display_cols
      } else {
        all(columns %in% display_cols)
      }
      if (!cols_exist) return(gt_tbl)
      tryCatch(
        suppressWarnings(
          gt::data_color(gt_tbl, columns = columns,
            fn = scales::col_numeric(
              palette = palette,
              domain = domain,
              na.color = "#374151",
              oob = scales::squish  # Clamp values to domain instead of warning
            ))
        ),
        error = function(e) gt_tbl
      )
    }

    # Apply color coding for all metric columns
    # Home win probabilities (blue scale, expanded domain to handle edge cases)
    gt_tbl <- apply_color(gt_tbl, c("Blend Home Win % (Shrunk)", "Market Home Win % (Fair, Devig=proportional)", "ML Implied Home % (Raw)"),
      c("#1e3a8a", "#2563eb", "#3b82f6", "#60a5fa", "#93c5fd"), c(0, 1))
    # Pick win probabilities (same blue scale)
    gt_tbl <- apply_color(gt_tbl, c("Blend Pick Win % (Shrunk)", "Market Pick Win % (Devig)"),
      c("#1e3a8a", "#2563eb", "#3b82f6", "#60a5fa", "#93c5fd"), c(0, 1))
    # Total EV (expanded domain to avoid clipping warnings)
    gt_tbl <- apply_color(gt_tbl, "Total EV (Units)",
      c("#991B1B", "#dc2626", "#1f2937", "#15803d", "#166534"), c(-0.10, 0.10))
    # EV Edge (expanded domain to avoid clipping warnings)
    gt_tbl <- apply_color(gt_tbl, "EV Edge (Displayed, Capped)",
      c("#991B1B", "#dc2626", "#1f2937", "#15803d", "#166534"), c(-0.25, 0.25))
    # Prob Edge on Pick (expanded domain)
    gt_tbl <- apply_color(gt_tbl, "Prob Edge on Pick (pp)",
      c("#7f1d1d", "#991b1b", "#1f2937", "#14532d", "#15532d"), c(-0.40, 0.40))
    gt_tbl <- apply_color(gt_tbl, c("Market Home Spread", "Blend Median Margin"),
      c("#dc2626", "#f87171", "#1f2937", "#4ade80", "#22c55e"), c(-21, 21))
    gt_tbl <- gt::tab_header(
      gt_tbl,
      title = title,
      subtitle = "⚠️ PROFESSIONAL CALIBRATION • 60% Shrinkage • 1/8 Kelly • Edge Skepticism • Market-Adjusted Probabilities"
    )

    # Add column spanners for better organization
    gt_tbl <- tryCatch({
      if ("tab_spanner" %in% getNamespaceExports("gt")) {
        add_spanner <- function(tbl, label, cols) {
          cols <- intersect(cols, display_cols)
          if (!length(cols)) return(tbl)
          gt::tab_spanner(tbl, label = label, columns = cols)
        }
        gt_tbl <- add_spanner(gt_tbl, label = "???? Game Info", cols = c("Season", "Week", "Date", "Matchup"))
        if ("Winner" %in% display_cols) {
          gt_tbl <- add_spanner(gt_tbl, label = "???? Result", cols = c("Winner"))
        }
        gt_tbl <- add_spanner(
          gt_tbl,
          label = "Governance",
          cols = c("Pass Reason", "Raw Kelly (%)", "Capped Stake (%)", "Final Stake (%)", "Min Stake (%)",
                   "Blend Stake (Units)", "EV Edge (Raw)", "EV Edge (Displayed, Capped)", "Total EV (Units)", "Edge Quality")
        )
        gt_tbl <- add_spanner(
          gt_tbl,
          label = "???? Blend Analysis",
          cols = c("Blend Pick", "Blend Recommendation", "Blend Beat Market?", "Blend Beat Market Basis",
                   "Blend Pick Win % (Shrunk)", "Prob Edge on Pick (pp)", "Blend Home Win % (Shrunk)",
                   "Blend Median Margin", "Blend Total", "Total O/U",
                   "Blend Home ML (Fair, from Shrunk Prob)", "Blend Home ML (Vigged, +X%)", "Blend Away Moneyline (Vigged)")
        )
        gt_tbl <- add_spanner(
          gt_tbl,
          label = "???? Market Data",
          cols = c("Market Pick Win % (Devig)", "Market Home Win % (Fair, Devig=proportional)", "ML Implied Home % (Raw)",
                   "Market Home Spread", "Market Total", "Market Home Moneyline", "Market Away Moneyline")
        )
      }
      gt_tbl
    }, error = function(e) gt_tbl)

    gt_tbl <- gt::tab_source_note(
      gt_tbl,
      source_note = "⚠️ CALIBRATED: Playoff shrinkage 70-75% | 1/8 Kelly | Max edge 10% (Review) | Min stake 1% | * = Pass | ✓ OK = 0-5% | ⚠ High = 5-10%"
    )
    gt_tbl <- gt::tab_options(
      gt_tbl,
      table.font.names = c("Inter", "Söhne", "Source Sans Pro", "Helvetica Neue", "Arial", "sans-serif"),
      table.font.color = "#F5F4F0",
      table.background.color = "transparent",
      table.width = gt::pct(100),
      heading.background.color = "#1A1815",
      heading.align = "center",
      column_labels.background.color = "#1A1815",
      column_labels.font.weight = "600",
      column_labels.text_transform = "uppercase",
      column_labels.border.top.style = "solid",
      column_labels.border.top.color = "rgba(217, 119, 87, 0.3)",
      column_labels.border.bottom.color = "#D97757",
      row.striping.background_color = "rgba(45, 42, 38, 0.5)",
      data_row.padding = gt::px(12),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.left.color = "transparent",
      table.border.right.color = "transparent",
      table.font.size = gt::px(14)
    )
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(color = "#FAF9F6"),
      locations = gt::cells_title(groups = "title")
    )
    gt_tbl <- gt::tab_style(
      gt_tbl,
      style = gt::cell_text(color = "#E89A7A"),
      locations = gt::cells_title(groups = "subtitle")
    )
    gt_tbl <- gt::opt_row_striping(gt_tbl)
    if ("Blend Beat Market?" %in% display_cols) {
      gt_tbl <- gt::data_color(
        gt_tbl,
        columns = "Blend Beat Market?",
        fn = function(values) {
          # Claude-themed colors: warm green for yes, muted for no/NA
          palette <- c(Yes = "#059669", No = "#3D3A36", `N/A` = "#4A4640")
          mapped <- palette[as.character(values)]
          default <- palette[["N/A"]]
          mapped[is.na(mapped)] <- default
          unname(mapped)
        }
      )
    }
    diff_cols <- intersect(c("Market Prob Δ", "Blend Prob Δ"), display_cols)
    if (length(diff_cols)) {
      palette_fn <- scales::col_numeric(
        palette = c("#ef4444", "#f59e0b", "#22c55e"),
        domain = c(-0.15, 0.15)
      )
      gt_tbl <- gt::data_color(
        gt_tbl,
        columns = dplyr::all_of(diff_cols),
        fn = palette_fn
      )
    }
    if ("Blend Recommendation" %in% display_cols) {
      gt_tbl <- gt::tab_style(
        gt_tbl,
        style = list(
          gt::cell_fill(color = "#D97757"),
          gt::cell_text(color = "#FAF9F6", weight = "bold")
        ),
        locations = gt::cells_body(
          columns = "Blend Recommendation",
          rows = !is.na(`Blend Recommendation`) & `Blend Recommendation` != "Pass"
        )
      )
    }
    # Edge Quality color coding
    if ("Edge Quality" %in% display_cols) {
      gt_tbl <- gt::data_color(
        gt_tbl,
        columns = "Edge Quality",
        fn = function(values) {
          palette <- c(
            "Pass" = "#4A4640",
            "Pass (capped)" = "#92400e",
            "✓ OK" = "#059669",
            "⚠ High" = "#d97706",
            "MODEL ERROR / REVIEW" = "#b91c1c",
            "N/A" = "#374151"
          )
          mapped <- palette[as.character(values)]
          default <- palette[["N/A"]]
          mapped[is.na(mapped)] <- default
          unname(mapped)
        }
      )
    }
    if ("Winner" %in% display_cols) {
      gt_tbl <- gt::tab_style(
        gt_tbl,
        style = list(
          gt::cell_fill(color = "#2D2A26"),
          gt::cell_text(color = "#E89A7A", weight = "bold")
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
        gt::cell_fill(color = "#1A1815"),
        gt::cell_text(color = "#FAF9F6", weight = "bold")
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
        ".gt_table { border-radius: 24px; overflow: hidden; box-shadow: 0 30px 80px rgba(0, 0, 0, 0.4); background-color: rgba(45, 42, 38, 0.9); width: max-content; min-width: 100%; }",
        ".gt_table thead tr.gt_col_spanners th { position: sticky; top: var(--sticky-offset, 72px); z-index: 3; backdrop-filter: blur(10px); background-color: rgba(26, 24, 21, 0.95); font-size: 0.78rem; padding: 8px 10px; border-bottom: 1px solid #D97757; color: #FAF9F6; }",
        ".gt_table thead tr.gt_col_headings th { position: sticky; top: calc(var(--sticky-offset, 72px) + var(--sticky-spanner, 26px)); z-index: 2; backdrop-filter: blur(10px); background-color: rgba(26, 24, 21, 0.95); font-size: 0.82rem; padding: 12px 10px; border-bottom: 2px solid #D97757; color: #FAF9F6; }",
        ".gt_table tbody tr:hover { background-color: rgba(217, 119, 87, 0.12); transition: all 0.2s ease; transform: scale(1.002); }",
        ".gt_table tbody td { padding: 12px 10px; font-size: 0.9rem; border-bottom: 1px solid rgba(217, 119, 87, 0.1); font-variant-numeric: tabular-nums; }",
        ".gt_table tbody td[style*='background'] { font-weight: 600; text-shadow: 0 1px 2px rgba(0, 0, 0, 0.3); }",
        "@media (max-width: 768px) { .gt_table thead th { font-size: 0.7rem; padding: 10px 6px; } .gt_table tbody td { font-size: 0.8rem; padding: 10px 6px; } }",
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

      # Modern Claude-inspired color scheme with ColorBends animated gradient background
      css_block_gt <- paste0(
        # CSS Variables for theming
        ":root {--claude-coral: #D97757; --claude-coral-light: #E89A7A; --claude-cream: #FAF9F6; --claude-warm-gray: #2D2A26; --claude-dark: #1A1815; --accent-glow: rgba(217, 119, 87, 0.3); --sticky-offset: 72px; --sticky-spanner: 26px;}\n",
        # Body with warm gradient and canvas background
        "body {font-family: 'Inter','Söhne','Source Sans Pro','Helvetica Neue',Arial,sans-serif; background: linear-gradient(135deg, #1A1815 0%, #2D2A26 50%, #1A1815 100%); color: #F5F4F0; margin: 0; padding-top: 96px; min-height: 100vh; overflow-x: hidden;}\n",
        # ColorBends canvas background
        "#colorbends-canvas {position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: -2; pointer-events: none;}\n",
        # Overlay for better text readability over animated background
        ".canvas-overlay {position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: linear-gradient(135deg, rgba(26, 24, 21, 0.85) 0%, rgba(45, 42, 38, 0.75) 50%, rgba(26, 24, 21, 0.85) 100%); pointer-events: none; z-index: -1;}\n",
        # Search container with glass morphism
        ".search-container {position: fixed; top: 0; left: 0; right: 0; z-index: 1000; background: rgba(26, 24, 21, 0.85); backdrop-filter: blur(20px); -webkit-backdrop-filter: blur(20px); border-bottom: 1px solid rgba(217, 119, 87, 0.3); padding: 1.25rem 0; box-shadow: 0 4px 30px rgba(0, 0, 0, 0.3);}\n",
        ".search-inner {max-width: 1400px; margin: 0 auto; padding: 0 1.5rem;}\n",
        ".page-wrapper {max-width: 1400px; margin: 0 auto; padding: 1rem 1.5rem 4rem; position: relative; z-index: 1;}\n",
        ".table-bleed {width: 100vw; margin-left: calc(50% - 50vw); padding: 0 1.5rem;}\n",
        ".table-wrapper {overflow-x: auto; border-radius: 24px; box-shadow: 0 25px 80px rgba(0, 0, 0, 0.4); width: 100%; max-width: 100%;}\n",
        ".gt_table {width: max-content !important; min-width: 100%; table-layout: auto;}\n",
        ".charts-row {display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 1.25rem; margin: 1rem 0 1.75rem;}\n",
        ".chart-card {background: rgba(45, 42, 38, 0.7); border: 1px solid rgba(217, 119, 87, 0.2); border-radius: 18px; padding: 1rem; box-shadow: 0 18px 40px rgba(0,0,0,0.3);} \n",
        ".chart-title {color: #E89A7A; font-size: 0.85rem; margin-bottom: 0.6rem; letter-spacing: 0.02em; text-transform: uppercase;}\n",
        ".chart-img {width: 100%; height: auto; display: block;}\n",
        # Report intro with glass morphism
        ".report-intro {max-width: 1000px; margin: 0 auto 2.5rem; background: rgba(45, 42, 38, 0.8); backdrop-filter: blur(16px); -webkit-backdrop-filter: blur(16px); padding: 2rem 2.25rem; border-radius: 24px; border: 1px solid rgba(217, 119, 87, 0.25); box-shadow: 0 20px 60px rgba(0, 0, 0, 0.4), inset 0 1px 0 rgba(255, 255, 255, 0.08);}\n",
        ".report-intro h2 {margin: 0 0 1rem; font-size: 1.5rem; color: var(--claude-cream); letter-spacing: -0.01em; font-weight: 600;}\n",
        ".report-intro h3 {color: var(--claude-coral-light); font-size: 1.1rem; margin: 1.5rem 0 0.75rem; font-weight: 600;}\n",
        ".report-intro p {margin: 0 0 1rem; color: #C9C5BE; font-size: 0.95rem; line-height: 1.6;}\n",
        ".report-intro ul {margin: 0; padding-left: 1.25rem; color: #E8E6E1; line-height: 1.7;}\n",
        ".report-intro li {margin-bottom: 0.5rem;}\n",
        ".report-intro strong {color: var(--claude-cream);}\n",
        # Intro section styling
        ".intro-section {padding: 1.25rem; margin: 1rem 0; border-radius: 16px; background: rgba(26, 24, 21, 0.6); border: 1px solid rgba(217, 119, 87, 0.18); backdrop-filter: blur(8px);}\n",
        ".intro-section.warning-box {background: rgba(220, 38, 38, 0.1); border-left: 3px solid #ef4444;}\n",
        ".edge-warning {color: #fca5a5; font-weight: 500; font-size: 0.9rem; margin-top: 0.75rem; padding: 0.5rem; background: rgba(0,0,0,0.2); border-radius: 8px;}\n",
        ".metrics-table {width: 100%; border-collapse: collapse; margin: 0.75rem 0; font-size: 0.9rem;}\n",
        ".metrics-table td {padding: 0.5rem 0.75rem; border-bottom: 1px solid rgba(217, 119, 87, 0.15);}\n",
        ".metrics-table td:first-child {width: 140px; color: var(--claude-coral-light);}\n",
        ".compact-list {margin: 0.5rem 0; padding-left: 1.25rem;}\n",
        ".compact-list li {margin: 0.35rem 0; line-height: 1.5;}\n",
        ".color-legend {display: flex; flex-wrap: wrap; gap: 1rem; align-items: center;}\n",
        ".color-legend h3 {width: 100%; margin-bottom: 0.5rem;}\n",
        ".color-chip {display: inline-block; width: 14px; height: 14px; border-radius: 4px; margin-right: 4px; vertical-align: middle;}\n",
        ".color-chip.green {background: #059669;}\n",
        ".color-chip.coral {background: #D97757;}\n",
        ".color-chip.muted {background: #3D3A36;}\n",
        ".color-chip.gray {background: #4A4640;}\n",
        # GT Table styling
        ".gt_table {border-radius: 20px; overflow: hidden; box-shadow: 0 30px 80px rgba(0, 0, 0, 0.5); background: rgba(45, 42, 38, 0.92) !important; backdrop-filter: blur(12px);}\n",
        ".gt_table thead tr.gt_col_spanners th {position: sticky; top: var(--sticky-offset, 72px); z-index: 110; background: rgba(26, 24, 21, 0.96) !important; backdrop-filter: blur(10px); color: var(--claude-cream) !important; font-weight: 600; letter-spacing: 0.03em; border-bottom: 1px solid var(--claude-coral) !important; font-size: 0.72rem; padding-top: 6px; padding-bottom: 6px;}
",
        ".gt_table thead tr.gt_col_headings th {position: sticky; top: calc(var(--sticky-offset, 72px) + var(--sticky-spanner, 26px)); z-index: 105; background: rgba(26, 24, 21, 0.96) !important; backdrop-filter: blur(10px); color: var(--claude-cream) !important; font-weight: 600; letter-spacing: 0.03em; border-bottom: 2px solid var(--claude-coral) !important;}
",
        ".gt_table tbody tr {transition: all 0.2s ease;}\n",
        ".gt_table tbody tr:hover {background-color: rgba(217, 119, 87, 0.15) !important; transform: scale(1.002);}\n",
        ".gt_table tbody td {border-bottom: 1px solid rgba(217, 119, 87, 0.1) !important; font-variant-numeric: tabular-nums;}\n",
        # Search input with coral accent
        "#table-search {width: 100%; max-width: 550px; padding: 1rem 1.5rem; margin: 0 auto; border-radius: 999px; border: 1px solid rgba(217, 119, 87, 0.35); background: rgba(45, 42, 38, 0.85); color: var(--claude-cream); display: block; box-shadow: 0 4px 20px rgba(217, 119, 87, 0.12), inset 0 1px 0 rgba(255, 255, 255, 0.06); transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1); font-size: 1rem; backdrop-filter: blur(8px);}\n",
        "#table-search:focus {outline: none; border-color: var(--claude-coral); box-shadow: 0 0 0 4px var(--accent-glow), 0 8px 30px rgba(217, 119, 87, 0.25); transform: translateY(-2px);}\n",
        "#table-search::placeholder {color: rgba(201, 197, 190, 0.6);}\n",
        # Color guide boxes
        ".color-box {display: inline-block; width: 16px; height: 16px; border-radius: 4px; vertical-align: middle; margin-right: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.2);}\n",
        # Result styling
        ".result-yes {color: #6EE7B7; font-weight: 600;}\n",
        ".result-no {color: #FCA5A5; font-weight: 600;}\n",
        # Search hint and filter buttons
        ".search-hint {display: block; text-align: center; font-size: 0.75rem; color: rgba(201, 197, 190, 0.5); margin-top: 0.5rem;}\n",
        ".filter-container {display: flex; justify-content: center; gap: 0.5rem; margin-top: 0.75rem; flex-wrap: wrap;}\n",
        ".filter-btn {padding: 0.4rem 0.9rem; border-radius: 999px; border: 1px solid rgba(217, 119, 87, 0.3); background: rgba(45, 42, 38, 0.7); color: #C9C5BE; cursor: pointer; font-size: 0.8rem; transition: all 0.2s ease;}\n",
        ".filter-btn:hover {border-color: var(--claude-coral); color: var(--claude-cream);}\n",
        ".filter-btn.active {background: rgba(217, 119, 87, 0.25); border-color: var(--claude-coral); color: var(--claude-cream);}\n",
        # Reduced motion fallback - respect user preference
        "@media (prefers-reduced-motion: reduce) { ",
        "  #colorbends-canvas {display: none !important;} ",
        "  .canvas-overlay {background: linear-gradient(135deg, #1A1815 0%, #2D2A26 50%, #1A1815 100%) !important;} ",
        "  .gt_table tbody tr:hover {transform: none !important;} ",
        "  * {animation: none !important; transition-duration: 0.01ms !important;} ",
        "}\n",
        # Tab navigation styles
        ".report-tabs {display: flex; gap: 0.5rem; margin-bottom: 1.5rem; margin-top: 0.5rem; padding: 0.5rem; background: rgba(26, 24, 21, 0.6); border-radius: 16px; justify-content: center;}\n",
        ".tab-btn {padding: 0.75rem 1.5rem; border: 1px solid rgba(217, 119, 87, 0.3); background: rgba(45, 42, 38, 0.5); color: #C9C5BE; border-radius: 12px; cursor: pointer; font-size: 0.95rem; font-weight: 500; transition: all 0.2s ease;}\n",
        ".tab-btn:hover {background: rgba(217, 119, 87, 0.15); color: var(--claude-cream);}\n",
        ".tab-btn.active {background: var(--claude-coral); color: var(--claude-cream); border-color: var(--claude-coral);}\n",
        ".tab-content {display: none;}\n",
        ".tab-content.active {display: block;}\n",
        # Props section styles
        ".props-section {border-left: 3px solid #10b981;}\n",
        ".props-table {margin-top: 0.35rem;}\n",
        ".props-content {margin-top: 0.35rem;}\n",
        ".props-content .gt_table tbody td {color: #F5F4F0 !important; font-size: 0.92rem;}\n",
        ".props-content .gt_table thead th {color: #FAF9F6 !important; padding-top: 4px; padding-bottom: 4px;}\n",
        ".props-content .gt_table thead tr.gt_col_spanners th {position: sticky; top: var(--sticky-offset, 72px); z-index: 5; background: rgba(26, 24, 21, 0.96) !important;}\n",
        ".props-content .gt_table thead tr.gt_col_headings th {position: sticky; top: calc(var(--sticky-offset, 72px) + var(--sticky-spanner, 26px)); z-index: 4; background: rgba(26, 24, 21, 0.96) !important;}\n",
        # Responsive adjustments
        "@media (max-width: 768px) { body {padding-top: 90px;} .gt_table {font-size: 0.88rem;} .gt_table thead tr.gt_col_spanners th {font-size: 0.65rem;} .gt_table thead tr.gt_col_headings th {font-size: 0.7rem;} .report-intro {padding: 1.5rem; margin: 0 0.5rem 2rem;} #table-search {font-size: 0.9rem; padding: 0.85rem 1.25rem;} .filter-btn {font-size: 0.7rem; padding: 0.3rem 0.7rem;} .report-tabs {flex-direction: column;} .tab-btn {width: 100%;} .table-bleed {padding: 0 1rem;} .charts-row {grid-template-columns: 1fr;} }\n"
      )

      # ColorBends Three.js animated gradient background script
      colorbends_script <- paste0(
        # Three.js CDN and ColorBends shader implementation
        "<!-- ColorBends Three.js Animated Gradient Background -->",
        "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js\"></script>",
        "<script>",
        "(function(){",
        "  if(!window.THREE){console.warn('Three.js not loaded');return;}",
        "  const container=document.getElementById('colorbends-canvas');",
        "  if(!container)return;",
        "  const scene=new THREE.Scene();",
        "  const camera=new THREE.OrthographicCamera(-1,1,1,-1,0,1);",
        "  const renderer=new THREE.WebGLRenderer({alpha:true,antialias:true});",
        "  renderer.setSize(window.innerWidth,window.innerHeight);",
        "  renderer.setPixelRatio(Math.min(window.devicePixelRatio,2));",
        "  container.appendChild(renderer.domElement);",
        "  const colors=['#D97757','#E89A7A','#8B5A3C'];", # Claude coral palette
        "  const uniforms={",
        "    time:{value:0},",
        "    resolution:{value:new THREE.Vector2(window.innerWidth,window.innerHeight)},",
        "    color1:{value:new THREE.Color(colors[0])},",
        "    color2:{value:new THREE.Color(colors[1])},",
        "    color3:{value:new THREE.Color(colors[2])},",
        "    mouse:{value:new THREE.Vector2(0.5,0.5)}",
        "  };",
        "  const vertexShader=`varying vec2 vUv;void main(){vUv=uv;gl_Position=vec4(position,1.0);}`;",
        "  const fragmentShader=`",
        "    uniform float time;uniform vec2 resolution;uniform vec3 color1;uniform vec3 color2;uniform vec3 color3;uniform vec2 mouse;varying vec2 vUv;",
        "    float noise(vec2 p){return fract(sin(dot(p,vec2(12.9898,78.233)))*43758.5453);}",
        "    float smoothNoise(vec2 p){vec2 i=floor(p);vec2 f=fract(p);f=f*f*(3.0-2.0*f);float a=noise(i);float b=noise(i+vec2(1.0,0.0));float c=noise(i+vec2(0.0,1.0));float d=noise(i+vec2(1.0,1.0));return mix(mix(a,b,f.x),mix(c,d,f.x),f.y);}",
        "    float fbm(vec2 p){float v=0.0;float a=0.5;for(int i=0;i<4;i++){v+=a*smoothNoise(p);p*=2.0;a*=0.5;}return v;}",
        "    void main(){",
        "      vec2 uv=vUv;vec2 q=uv;",
        "      q.x+=0.1*sin(time*0.3+uv.y*3.0);q.y+=0.1*cos(time*0.2+uv.x*4.0);",
        "      float n1=fbm(q*2.0+time*0.1);float n2=fbm(q*3.0-time*0.15);float n3=fbm(q*1.5+time*0.08);",
        "      vec3 c1=mix(color1,color2,n1);vec3 c2=mix(c1,color3,n2*0.6);",
        "      float dist=length(uv-mouse)*1.5;float glow=exp(-dist*3.0)*0.15;",
        "      vec3 finalColor=c2*(0.3+n3*0.2)+vec3(glow)*color1;",
        "      finalColor*=0.4;", # Reduce intensity for subtlety
        "      gl_FragColor=vec4(finalColor,0.6);",
        "    }`;",
        "  const geometry=new THREE.PlaneGeometry(2,2);",
        "  const material=new THREE.ShaderMaterial({uniforms:uniforms,vertexShader:vertexShader,fragmentShader:fragmentShader,transparent:true});",
        "  const mesh=new THREE.Mesh(geometry,material);",
        "  scene.add(mesh);",
        "  let mouseX=0.5,mouseY=0.5;",
        "  document.addEventListener('mousemove',function(e){mouseX=e.clientX/window.innerWidth;mouseY=1.0-e.clientY/window.innerHeight;});",
        "  window.addEventListener('resize',function(){renderer.setSize(window.innerWidth,window.innerHeight);uniforms.resolution.value.set(window.innerWidth,window.innerHeight);});",
        "  function animate(){",
        "    requestAnimationFrame(animate);",
        "    uniforms.time.value+=0.01;",
        "    uniforms.mouse.value.x+=(mouseX-uniforms.mouse.value.x)*0.05;",
        "    uniforms.mouse.value.y+=(mouseY-uniforms.mouse.value.y)*0.05;",
        "    renderer.render(scene,camera);",
        "  }",
        "  animate();",
        "})();",
        "</script>"
      )

      search_box <- htmltools::tags$div(
        class = "search-container",
        htmltools::tags$div(
          class = "search-inner",
          htmltools::tags$input(
            id = "table-search",
            type = "search",
            class = "table-search",
            placeholder = "Search teams, picks, or betting data...",
            `aria-label` = "Search moneyline table"
          ),
          # Quick filter buttons
          htmltools::tags$div(
            class = "filter-container",
            htmltools::tags$button(
              class = "filter-btn",
              `data-filter` = "ev",
              onclick = "toggleFilter('ev')",
              "+EV Only"
            ),
            htmltools::tags$button(
              class = "filter-btn",
              `data-filter` = "suspicious",
              onclick = "toggleFilter('suspicious')",
              "Suspicious Edges"
            ),
            htmltools::tags$button(
              class = "filter-btn",
              `data-filter` = "pass",
              onclick = "toggleFilter('pass')",
              "Pass Games"
            )
          )
        )
      )

      intro_block <- htmltools::HTML(intro_html)

      # === PLAYER PROPS SECTION (always show - with data or informative message) ===
      props_section <- NULL
      # Prefer explicit props_data parameter, fall back to .GlobalEnv
      props_data_local <- if (!is.null(props_data)) {
        props_data
      } else if (exists("props_results", envir = .GlobalEnv)) {
        get("props_results", envir = .GlobalEnv)
      } else {
        NULL
      }
      props_available <- !is.null(props_data_local) &&
                         is.data.frame(props_data_local) &&
                         nrow(props_data_local) > 0

      if (props_available) {
        props_data <- props_data_local
        if (exists("PROP_REQUIRE_MARKET_ODDS") && isTRUE(PROP_REQUIRE_MARKET_ODDS) &&
            "odds_source" %in% names(props_data)) {
          market_only <- props_data %>% dplyr::filter(.data$odds_source == "market")
          if (nrow(market_only) > 0) {
            props_data <- market_only
          }
        }
        # Ensure optional market columns exist to prevent mutate failures
        props_data <- ensure_columns_with_defaults(props_data, list(
          team = NA_character_,
          line = NA_real_,
          line_over = NA_real_,
          line_under = NA_real_,
          over_odds = NA_real_,
          under_odds = NA_real_,
          book = NA_character_,
          dk_line = NA_real_,
          dk_line_over = NA_real_,
          dk_line_under = NA_real_,
          dk_over_odds = NA_real_,
          dk_under_odds = NA_real_,
          fd_line = NA_real_,
          fd_line_over = NA_real_,
          fd_line_under = NA_real_,
          fd_over_odds = NA_real_,
          fd_under_odds = NA_real_,
          line_source = NA_character_,
          odds_source = NA_character_,
          edge_quality = NA_character_
        ))
        # Build props intro HTML
        props_intro <- paste0(
            "<section class=\"report-intro props-section\">",
            "<h2>Player Props Analysis</h2>",
            "<p class=\"report-subtitle\">Correlated with game simulation outcomes via Gaussian copula</p>",
            "<div class=\"intro-section\">",
            "<h3>Correlation Model</h3>",
            "<ul class=\"compact-list\">",
            "<li><strong>QB Passing</strong> \u2194 Game Total: r = 0.40</li>",
            "<li><strong>RB Rushing</strong> \u2194 Game Total: r = 0.09</li>",
            "<li><strong>WR/TE Receiving</strong> \u2194 Team Passing: r = 0.30</li>",
            "<li><strong>TD Probability</strong> \u2194 Game Total: r = 0.17</li>",
            "</ul>",
            "<p>Props are simulated using the same game outcomes, ensuring consistent same-game correlation.</p>",
            "<p><strong>Odds source:</strong> DK/FD market odds via ScoresAndOdds market-comparison or The Odds API; otherwise odds are left blank unless PROP_ALLOW_MODEL_ODDS=TRUE (model-derived with fixed vig).</p>",
            "<p><strong>Market filter:</strong> When PROP_REQUIRE_MARKET_ODDS=TRUE, only rows with market odds are shown.</p>",
            "</div>",
            "</section>"
          )

          # Format props for display
          # Helper to format American odds
          format_odds <- function(x) {
            num <- suppressWarnings(as.numeric(x))
            dplyr::case_when(
              is.na(num) ~ "-",
              num >= 0 ~ sprintf("+%d", round(num)),
              TRUE ~ sprintf("%d", round(num))
            )
          }

          format_line <- function(x) {
            num <- suppressWarnings(as.numeric(x))
            if (is.na(num) || !is.finite(num)) return("-")
            sprintf("%.1f", num)
          }

          format_book <- function(x) {
            if (length(x) > 1) {
              return(vapply(x, format_book, character(1)))
            }
            if (is.list(x)) {
              x <- if (length(x) && !is.null(x[[1]])) as.character(x[[1]]) else NA_character_
            }
            if (is.na(x) || !nzchar(x)) return("-")
            key <- tolower(x)
            if (key == "draftkings") return("DK")
            if (key == "fanduel") return("FD")
            toupper(key)
          }

          coerce_numeric <- function(x) {
            if (is.list(x)) {
              return(vapply(x, function(v) {
                if (is.null(v) || length(v) == 0) return(NA_real_)
                suppressWarnings(as.numeric(v[[1]]))
              }, numeric(1)))
            }
            suppressWarnings(as.numeric(x))
          }

          format_market_cell <- function(line_over, line_under, over_odds, under_odds, prop_type) {
            if (length(line_over) > 1 || length(line_under) > 1 ||
                length(over_odds) > 1 || length(under_odds) > 1 ||
                length(prop_type) > 1) {
              return(mapply(
                function(lo, lu, oo, uu, pt) format_market_cell(lo, lu, oo, uu, pt),
                line_over, line_under, over_odds, under_odds, prop_type,
                SIMPLIFY = TRUE, USE.NAMES = FALSE
              ))
            }

            if (is.list(prop_type)) {
              prop_type <- if (length(prop_type) && !is.null(prop_type[[1]])) {
                as.character(prop_type[[1]])
              } else {
                NA_character_
              }
            }

            line_over <- coerce_numeric(line_over)
            line_under <- coerce_numeric(line_under)
            over_odds <- coerce_numeric(over_odds)
            under_odds <- coerce_numeric(under_odds)
            has_over <- is.finite(over_odds)
            has_under <- is.finite(under_odds)
            if (!has_over && !has_under) return("-")

            if (!is.null(prop_type) && prop_type == "anytime_td") {
              if (has_over) {
                implied <- if (over_odds >= 0) {
                  100 / (over_odds + 100)
                } else {
                  abs(over_odds) / (abs(over_odds) + 100)
                }
                return(sprintf("Yes %s (%.1f%%)", format_odds(over_odds), implied * 100))
              }
              return("-")
            }

            parts <- character(0)
            if (is.finite(line_over) || has_over) {
              parts <- c(parts, sprintf("O%s (%s)", format_line(line_over), format_odds(over_odds)))
            }
            if (is.finite(line_under) || has_under) {
              parts <- c(parts, sprintf("U%s (%s)", format_line(line_under), format_odds(under_odds)))
            }
            paste(parts, collapse = " / ")
          }

          props_display <- props_data %>%
            dplyr::transmute(
              Player = player,
              Position = position,
              Team = team,
              `Prop Type` = dplyr::case_when(
                prop_type == "passing_yards" ~ "Passing Yards",
                prop_type == "rushing_yards" ~ "Rushing Yards",
                prop_type == "receiving_yards" ~ "Receiving Yards",
                prop_type == "receptions" ~ "Receptions",
                prop_type == "anytime_td" ~ "Anytime TD",
                TRUE ~ gsub("_", " ", stringr::str_to_title(prop_type))
              ),
              `Market DK` = format_market_cell(
                dplyr::coalesce(dk_line_over, dk_line),
                dplyr::coalesce(dk_line_under, dk_line),
                dk_over_odds,
                dk_under_odds,
                prop_type
              ),
              `Market FD` = format_market_cell(
                dplyr::coalesce(fd_line_over, fd_line),
                dplyr::coalesce(fd_line_under, fd_line),
                fd_over_odds,
                fd_under_odds,
                prop_type
              ),
              `Market Used` = format_market_cell(
                dplyr::coalesce(line_over, line),
                dplyr::coalesce(line_under, line),
                over_odds,
                under_odds,
                prop_type
              ),
              `Book Used` = format_book(book),
              Model = dplyr::case_when(
                prop_type == "anytime_td" ~ sprintf("%.1f%%", projection * 100),
                TRUE ~ sprintf("%.1f", projection)
              ),
              `P(Over/Yes)` = dplyr::if_else(
                is.na(p_over), "-", sprintf("%.1f%%", p_over * 100)
              ),
              `P(Under/No)` = dplyr::if_else(
                is.na(p_under), "-", sprintf("%.1f%%", p_under * 100)
              ),
              `EV Over` = dplyr::if_else(
                is.na(ev_over), "-", sprintf("%+.1f%%", ev_over * 100)
              ),
              `EV Under` = dplyr::if_else(
                is.na(ev_under), "-", sprintf("%+.1f%%", ev_under * 100)
              ),
              edge_quality_display = dplyr::coalesce(
                edge_quality,
                dplyr::case_when(
                  recommendation == "PASS" ~ "Pass",
                  recommendation == "REVIEW" ~ "Review",
                  recommendation == "MODEL ERROR" ~ "MODEL ERROR",
                  TRUE ~ "N/A"
                )
              ),
              `Edge Quality` = edge_quality_display,
              Recommendation = recommendation,
              Source = dplyr::case_when(
                is.na(line_source) & is.na(odds_source) ~ "-",
                TRUE ~ paste0(dplyr::coalesce(line_source, "-"),
                              "/",
                              dplyr::coalesce(odds_source, "-"))
              )
            ) %>%
            dplyr::select(-edge_quality_display)

          props_charts_html <- tryCatch(build_props_charts(props_data), error = function(e) NULL)

          # Create props gt table
          props_gt <- tryCatch({
            gt::gt(props_display) %>%
              gt::tab_header(
                title = "Player Props Recommendations",
                subtitle = sprintf("Monte Carlo: %s trials | Correlated with game simulation",
                                   format(if(exists("N_TRIALS")) N_TRIALS else 50000, big.mark = ","))
              ) %>%
              gt::cols_width(
                Player ~ gt::px(150),
                Position ~ gt::px(70),
                Team ~ gt::px(60),
                `Prop Type` ~ gt::px(140),
                `Market DK` ~ gt::px(170),
                `Market FD` ~ gt::px(170),
                `Market Used` ~ gt::px(180),
                `Book Used` ~ gt::px(90),
                Model ~ gt::px(90),
                `P(Over/Yes)` ~ gt::px(95),
                `P(Under/No)` ~ gt::px(95),
                `EV Over` ~ gt::px(85),
                `EV Under` ~ gt::px(85),
                `Edge Quality` ~ gt::px(90),
                Recommendation ~ gt::px(110),
                Source ~ gt::px(90)
              ) %>%
              gt::tab_spanner(
                label = "Market",
                columns = c("Market DK", "Market FD", "Market Used", "Book Used")
              ) %>%
              gt::tab_spanner(
                label = "Model",
                columns = c("Model", "P(Over/Yes)", "P(Under/No)")
              ) %>%
              gt::tab_spanner(
                label = "Value",
                columns = c("EV Over", "EV Under", "Edge Quality", "Recommendation")
              ) %>%
              gt::tab_spanner(
                label = "Meta",
                columns = c("Source")
              ) %>%
              gt::tab_style(
                style = gt::cell_fill(color = "#22c55e40"),
                locations = gt::cells_body(
                  columns = "Recommendation",
                  rows = Recommendation %in% c("OVER", "UNDER", "BET")
                )
              ) %>%
              gt::tab_options(
                table.width = gt::pct(100)
              ) %>%
              gt::tab_source_note(
                source_note = "Props correlated with game simulation | Normal dist (yards) | Poisson (receptions) | Negative Binomial (TDs)"
              )
          }, error = function(e) NULL)

          render_props_fallback_table <- function(df) {
            if (!requireNamespace("htmltools", quietly = TRUE)) return(NULL)
            cols <- names(df)
            head_row <- htmltools::tags$tr(lapply(cols, htmltools::tags$th))
            body_rows <- lapply(seq_len(nrow(df)), function(i) {
              htmltools::tags$tr(lapply(cols, function(col) {
                val <- df[[col]][i]
                if (is.na(val)) val <- ""
                htmltools::tags$td(as.character(val))
              }))
            })
            htmltools::tags$table(
              class = "gt_table",
              htmltools::tags$thead(head_row),
              htmltools::tags$tbody(body_rows)
            )
          }

          if (!is.null(props_gt)) {
            props_gt_html <- tryCatch(
              as.character(gt::as_raw_html(props_gt)),
              error = function(e) NULL
            )

            if (!is.null(props_gt_html)) {
              props_section <- htmltools::tags$div(
                id = "props-section",
                class = "tab-content props-content",
                htmltools::HTML(props_intro),
                if (!is.null(props_charts_html)) props_charts_html,
                htmltools::tags$div(
                  class = "table-bleed",
                  htmltools::tags$div(
                    class = "table-wrapper props-table",
                    htmltools::HTML(props_gt_html)
                  )
                )
              )
            }
          }
          if (is.null(props_section)) {
            fallback_table <- render_props_fallback_table(props_display)
            props_section <- htmltools::tags$div(
              id = "props-section",
              class = "tab-content props-content",
              htmltools::HTML(props_intro),
              if (!is.null(props_charts_html)) props_charts_html,
              htmltools::tags$div(
                class = "table-bleed",
                htmltools::tags$div(
                  class = "table-wrapper props-table",
                  if (!is.null(fallback_table)) fallback_table else htmltools::HTML(
                    "<div class=\"report-intro props-section\"><h2>Player Props Analysis</h2><p>Props table could not render. Verify gt/htmltools availability and props schema.</p></div>"
                  )
                )
              )
            )
          }
      } else {
        # Show informative message when props not available
        props_unavailable_html <- paste0(
          "<section class=\"report-intro props-section\">",
          "<h2>Player Props Analysis</h2>",
          "<p class=\"report-subtitle\">Correlated with game simulation outcomes via Gaussian copula</p>",
          "<div class=\"intro-section\" style=\"background-color: #fff3cd; border: 1px solid #ffc107; border-radius: 8px; padding: 20px; margin: 20px 0;\">",
          "<h3 style=\"color: #856404;\">Props Not Available for This Week</h3>",
          "<p style=\"color: #856404;\">Player prop projections could not be generated for this game.</p>",
          "<p style=\"color: #856404;\"><strong>Possible reasons:</strong></p>",
          "<ul style=\"color: #856404;\">",
          "<li>No player statistics available for the current or upcoming season</li>",
          "<li>Playoff/Super Bowl game with limited historical player data</li>",
          "<li>RUN_PLAYER_PROPS may be set to FALSE in config.R</li>",
          "</ul>",
          "<p style=\"color: #856404; margin-top: 15px;\">",
          "<em>The game prediction model still provides accurate win probabilities and spreads.</em>",
          "</p>",
          "</div>",
          "</section>"
        )
        props_section <- htmltools::tags$div(
          id = "props-section",
          class = "tab-content props-content",
          htmltools::HTML(props_unavailable_html)
        )
      }

      # Tab navigation (always show - props section now always exists)
      tab_nav <- htmltools::tags$nav(
        class = "report-tabs",
        htmltools::tags$button(
          class = "tab-btn active",
          `data-tab` = "games",
          onclick = "switchTab('games')",
          "Game Predictions"
        ),
        htmltools::tags$button(
          class = "tab-btn",
          `data-tab` = "props",
          onclick = "switchTab('props')",
          if (props_available) "Player Props" else "Player Props (N/A)"
        )
      )

      content_wrapper <- htmltools::tags$div(
        class = "page-wrapper",
        tab_nav,
        htmltools::tags$div(
          id = "games-section",
          class = "tab-content active",
          intro_block,
          if (!is.null(games_charts_html)) games_charts_html,
          htmltools::tags$div(
            class = "table-bleed",
            htmltools::tags$div(
              class = "table-wrapper",
              htmltools::HTML(gt_html)
            )
          )
        ),
        props_section
      )

      # Enhanced script with keyboard shortcuts, quick filters, and accessibility
      script_block <- htmltools::tags$script(htmltools::HTML(
        sprintf(
          paste0(
            "(function(){",
            # Core setup
            "var input=document.getElementById('table-search');",
            "var table=document.getElementById('%s');",
            "if(!input||!table){return;}",
            "var rows=table.getElementsByTagName('tbody')[0].rows;",
            "var activeFilter=null;",
            # Search function
            "function filterRows(query,filterFn){",
            "  Array.prototype.forEach.call(rows,function(row){",
            "    var text=row.textContent.toLowerCase();",
            "    var matchQuery=!query||text.indexOf(query)>-1;",
            "    var matchFilter=!filterFn||filterFn(row);",
            "    row.style.display=(matchQuery&&matchFilter)?'':'none';",
            "  });",
            "}",
            # Search input handler
            "input.addEventListener('input',function(){",
            "  filterRows(this.value.toLowerCase(),activeFilter?filters[activeFilter]:null);",
            "});",
            # Keyboard shortcuts: / to focus, Esc to clear
            "document.addEventListener('keydown',function(e){",
            "  if(e.key==='/'&&document.activeElement!==input){",
            "    e.preventDefault();input.focus();input.select();",
            "  }",
            "  if(e.key==='Escape'){",
            "    input.value='';input.blur();",
            "    activeFilter=null;updateFilterBtns();filterRows('',null);",
            "  }",
            "});",
            # Quick filter definitions
            "var filters={",
            "  'ev':function(row){",
            "    var cells=row.getElementsByTagName('td');",
            "    for(var i=0;i<cells.length;i++){",
            "      var text=cells[i].textContent;",
            "      if(text.indexOf('Bet ')===0||text.indexOf('Aggressive')>-1)return true;",
            "    }return false;",
            "  },",
            "  'suspicious':function(row){",
            "    var text=row.textContent.toLowerCase();",
            "    return text.indexOf('suspicious')>-1||text.indexOf('implausible')>-1||text.indexOf('optimistic')>-1;",
            "  },",
            "  'pass':function(row){",
            "    var text=row.textContent;",
            "    return text.indexOf('Pass')>-1||text.indexOf('No Play')>-1;",
            "  }",
            "};",
            # Filter button handler
            "function updateFilterBtns(){",
            "  var btns=document.querySelectorAll('.filter-btn');",
            "  btns.forEach(function(btn){",
            "    btn.classList.toggle('active',btn.dataset.filter===activeFilter);",
            "  });",
            "}",
            "window.toggleFilter=function(name){",
            "  activeFilter=(activeFilter===name)?null:name;",
            "  updateFilterBtns();",
            "  filterRows(input.value.toLowerCase(),activeFilter?filters[activeFilter]:null);",
            "};",
            # Search hint
            "var hint=document.createElement('span');",
            "hint.className='search-hint';",
            "hint.textContent='Press / to search, Esc to clear';",
            "input.parentNode.appendChild(hint);",
            "function setStickyOffset(){",
            "  var search=document.querySelector('.search-container');",
            "  if(!search){return;}",
            "  var offset=search.getBoundingClientRect().height;",
            "  document.documentElement.style.setProperty('--sticky-offset',offset+'px');",
            "  var spanner=document.querySelector('.tab-content.active .gt_table thead tr.gt_col_spanners');",
            "  if(!spanner){spanner=document.querySelector('.gt_table thead tr.gt_col_spanners');}",
            "  if(spanner){",
            "    var spannerHeight=spanner.getBoundingClientRect().height;",
            "    if(spannerHeight>0){document.documentElement.style.setProperty('--sticky-spanner',spannerHeight+'px');}",
            "  }",
            "}",
            "window.setStickyOffset=setStickyOffset;",
            "setStickyOffset();",
            "window.addEventListener('resize',setStickyOffset);",
            "})();"
          ),
          table_id
        )
      ))

      # Tab switching script (for games/props navigation)
      tab_script <- htmltools::tags$script(htmltools::HTML(
        paste0(
          "function switchTab(tabName){",
          "  var tabs=document.querySelectorAll('.tab-content');",
          "  var btns=document.querySelectorAll('.tab-btn');",
          "  tabs.forEach(function(t){t.classList.remove('active');});",
          "  btns.forEach(function(b){b.classList.remove('active');});",
          "  var targetSection=document.getElementById(tabName+'-section');",
          "  var targetBtn=document.querySelector('[data-tab=\"'+tabName+'\"]');",
          "  if(targetSection)targetSection.classList.add('active');",
          "  if(targetBtn)targetBtn.classList.add('active');",
          "  if(window.setStickyOffset){window.setStickyOffset();}",
          "}"
        )
      ))

      # ColorBends animated gradient canvas container
      colorbends_canvas <- htmltools::tags$div(id = "colorbends-canvas")
      canvas_overlay <- htmltools::tags$div(class = "canvas-overlay")

      doc <- htmltools::tags$html(
        htmltools::tags$head(
          htmltools::tags$meta(charset = "UTF-8"),
          htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
          htmltools::tags$title("NFL Blend vs Market Analysis - Games & Player Props"),
          htmltools::tags$style(css_block_gt)
        ),
        htmltools::tags$body(
          colorbends_canvas,
          canvas_overlay,
          search_box,
          content_wrapper,
          tab_script,
          script_block,
          htmltools::HTML(colorbends_script)
        )
      )

      htmltools::save_html(doc, file = file)
      saved <- TRUE
    }
  }

  if (!saved) {
    # Fallback CSS with Claude-inspired color scheme and ColorBends canvas
    css_block <- ":root {--claude-coral: #D97757; --claude-coral-light: #E89A7A; --claude-cream: #FAF9F6; --claude-warm-gray: #2D2A26; --claude-dark: #1A1815;}\n"
    css_block <- paste0(
      css_block,
      "body {font-family: 'Inter','Söhne','Source Sans Pro','Helvetica Neue',Arial,sans-serif; background: linear-gradient(135deg, #1A1815 0%, #2D2A26 50%, #1A1815 100%); color: #F5F4F0; margin: 0; min-height: 100vh; overflow-x: hidden;}\n",
      "#colorbends-canvas {position: fixed; top: 0; left: 0; width: 100%; height: 100%; z-index: -2; pointer-events: none;}\n",
      ".canvas-overlay {position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: linear-gradient(135deg, rgba(26, 24, 21, 0.85) 0%, rgba(45, 42, 38, 0.75) 50%, rgba(26, 24, 21, 0.85) 100%); pointer-events: none; z-index: -1;}\n",
        ".page-wrapper {max-width: 1400px; margin: 0 auto; padding: 3rem 1.5rem 4rem; position: relative; z-index: 1;}\n",
        ".table-bleed {width: 100vw; margin-left: calc(50% - 50vw); padding: 0 1.5rem;}\n",
      ".table-bleed {width: 100vw; margin-left: calc(50% - 50vw); padding: 0 1.5rem;}\n",
      ".table-wrapper {overflow-x: auto; border-radius: 24px; box-shadow: 0 25px 80px rgba(0, 0, 0, 0.4);}\n",
      ".charts-row {display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 1.25rem; margin: 1rem 0 1.75rem;}\n",
      ".chart-card {background: rgba(45, 42, 38, 0.7); border: 1px solid rgba(217, 119, 87, 0.2); border-radius: 18px; padding: 1rem; box-shadow: 0 18px 40px rgba(0,0,0,0.3);} \n",
      ".chart-title {color: #E89A7A; font-size: 0.85rem; margin-bottom: 0.6rem; letter-spacing: 0.02em; text-transform: uppercase;}\n",
      ".chart-img {width: 100%; height: auto; display: block;}\n",
      ".report-intro {max-width: 1000px; margin: 0 auto 2.5rem; background: rgba(45, 42, 38, 0.8); backdrop-filter: blur(16px); padding: 2rem 2.25rem; border-radius: 24px; border: 1px solid rgba(217, 119, 87, 0.25); box-shadow: 0 20px 60px rgba(0, 0, 0, 0.4);}\n",
      ".report-intro h2 {margin: 0 0 1rem; font-size: 1.5rem; color: var(--claude-cream); font-weight: 600;}\n",
      ".report-intro h3 {color: var(--claude-coral-light); font-size: 1.1rem; margin: 1.5rem 0 0.75rem; font-weight: 600;}\n",
      ".report-intro p {margin: 0 0 1rem; color: #C9C5BE; font-size: 0.95rem; line-height: 1.6;}\n",
      ".report-intro ul {margin: 0; padding-left: 1.25rem; color: #E8E6E1; line-height: 1.7;}\n",
      ".report-intro li {margin-bottom: 0.5rem;}\n",
      ".intro-section {padding: 1.25rem; margin: 1rem 0; border-radius: 16px; background: rgba(26, 24, 21, 0.6); border: 1px solid rgba(217, 119, 87, 0.18); backdrop-filter: blur(8px);}\n",
      "table {width: 100%; border-collapse: separate; border-spacing: 0; background-color: rgba(45, 42, 38, 0.92); color: #F5F4F0; border-radius: 24px; overflow: hidden; box-shadow: 0 30px 80px rgba(0, 0, 0, 0.5); border: 1px solid rgba(217, 119, 87, 0.2); backdrop-filter: blur(12px);}\n",
      "thead th {background-color: rgba(26, 24, 21, 0.96); color: var(--claude-cream); text-transform: uppercase; letter-spacing: 0.08em; position: sticky; top: 0; z-index: 2; border-bottom: 2px solid var(--claude-coral);}\n",
      "td, th {padding: 14px 12px; border-bottom: 1px solid rgba(217, 119, 87, 0.1); text-align: center;}\n",
      "td.text-left {text-align: left;}\n",
      "td.note-cell {max-width: 260px; white-space: normal; word-wrap: break-word;}\n",
      "tr:nth-child(even) {background-color: rgba(45, 42, 38, 0.5);}\n",
      "tr.blend-win {background: linear-gradient(135deg, rgba(5, 150, 105, 0.4), rgba(16, 185, 129, 0.3));}\n",
      "tr.blend-win td {color: #D1FAE5;}\n",
      "td.blend-reco {background-color: var(--claude-coral) !important; color: var(--claude-cream) !important; font-weight: 600;}\n",
      "td.winner-cell {color: var(--claude-coral-light); font-weight: 600;}\n",
      "tbody tr:hover {background-color: rgba(217, 119, 87, 0.15); transition: all 0.2s ease;}\n",
      "caption {caption-side: top; font-size: 1.35rem; font-weight: 600; margin-bottom: 0.75rem; color: var(--claude-cream);}\n",
      "#table-search {width: 100%; max-width: 550px; padding: 1rem 1.5rem; margin: 0 auto 1.5rem; border-radius: 999px; border: 1px solid rgba(217, 119, 87, 0.35); background-color: rgba(45, 42, 38, 0.85); color: var(--claude-cream); display: block; box-shadow: 0 4px 20px rgba(217, 119, 87, 0.12); transition: all 0.3s ease; backdrop-filter: blur(8px);}\n",
      "#table-search:focus {outline: none; border-color: var(--claude-coral); box-shadow: 0 0 0 4px rgba(217, 119, 87, 0.3);}\n",
      ".color-box {display: inline-block; width: 16px; height: 16px; border-radius: 4px; vertical-align: middle; margin-right: 8px;}\n",
      "@media (max-width: 768px) { table {font-size: 0.88rem;} thead th {font-size: 0.7rem;} .report-intro {padding: 1.5rem; margin: 0 0.5rem 2rem;} }\n"
    )

    # ColorBends Three.js script for fallback HTML
    colorbends_script_fallback <- paste0(
      "<script src=\"https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js\"></script>",
      "<script>",
      "(function(){",
      "  if(!window.THREE){console.warn('Three.js not loaded');return;}",
      "  var container=document.getElementById('colorbends-canvas');",
      "  if(!container)return;",
      "  var scene=new THREE.Scene();",
      "  var camera=new THREE.OrthographicCamera(-1,1,1,-1,0,1);",
      "  var renderer=new THREE.WebGLRenderer({alpha:true,antialias:true});",
      "  renderer.setSize(window.innerWidth,window.innerHeight);",
      "  renderer.setPixelRatio(Math.min(window.devicePixelRatio,2));",
      "  container.appendChild(renderer.domElement);",
      "  var colors=['#D97757','#E89A7A','#8B5A3C'];",
      "  var uniforms={time:{value:0},resolution:{value:new THREE.Vector2(window.innerWidth,window.innerHeight)},color1:{value:new THREE.Color(colors[0])},color2:{value:new THREE.Color(colors[1])},color3:{value:new THREE.Color(colors[2])},mouse:{value:new THREE.Vector2(0.5,0.5)}};",
      "  var vertexShader='varying vec2 vUv;void main(){vUv=uv;gl_Position=vec4(position,1.0);}';",
      "  var fragmentShader='uniform float time;uniform vec2 resolution;uniform vec3 color1;uniform vec3 color2;uniform vec3 color3;uniform vec2 mouse;varying vec2 vUv;float noise(vec2 p){return fract(sin(dot(p,vec2(12.9898,78.233)))*43758.5453);}float smoothNoise(vec2 p){vec2 i=floor(p);vec2 f=fract(p);f=f*f*(3.0-2.0*f);float a=noise(i);float b=noise(i+vec2(1.0,0.0));float c=noise(i+vec2(0.0,1.0));float d=noise(i+vec2(1.0,1.0));return mix(mix(a,b,f.x),mix(c,d,f.x),f.y);}float fbm(vec2 p){float v=0.0;float a=0.5;for(int i=0;i<4;i++){v+=a*smoothNoise(p);p*=2.0;a*=0.5;}return v;}void main(){vec2 uv=vUv;vec2 q=uv;q.x+=0.1*sin(time*0.3+uv.y*3.0);q.y+=0.1*cos(time*0.2+uv.x*4.0);float n1=fbm(q*2.0+time*0.1);float n2=fbm(q*3.0-time*0.15);float n3=fbm(q*1.5+time*0.08);vec3 c1=mix(color1,color2,n1);vec3 c2=mix(c1,color3,n2*0.6);float dist=length(uv-mouse)*1.5;float glow=exp(-dist*3.0)*0.15;vec3 finalColor=c2*(0.3+n3*0.2)+vec3(glow)*color1;finalColor*=0.4;gl_FragColor=vec4(finalColor,0.6);}';",
      "  var geometry=new THREE.PlaneGeometry(2,2);",
      "  var material=new THREE.ShaderMaterial({uniforms:uniforms,vertexShader:vertexShader,fragmentShader:fragmentShader,transparent:true});",
      "  var mesh=new THREE.Mesh(geometry,material);",
      "  scene.add(mesh);",
      "  var mouseX=0.5,mouseY=0.5;",
      "  document.addEventListener('mousemove',function(e){mouseX=e.clientX/window.innerWidth;mouseY=1.0-e.clientY/window.innerHeight;});",
      "  window.addEventListener('resize',function(){renderer.setSize(window.innerWidth,window.innerHeight);uniforms.resolution.value.set(window.innerWidth,window.innerHeight);});",
      "  function animate(){requestAnimationFrame(animate);uniforms.time.value+=0.01;uniforms.mouse.value.x+=(mouseX-uniforms.mouse.value.x)*0.05;uniforms.mouse.value.y+=(mouseY-uniforms.mouse.value.y)*0.05;renderer.render(scene,camera);}",
      "  animate();",
      "})();",
      "</script>"
    )

    # Format columns that exist in display_tbl (fallback for when gt unavailable)
    formatted_tbl <- display_tbl
    display_cols_fb <- names(display_tbl)

    # Helper to safely format percent columns
    safe_percent <- function(df, col) {
      if (col %in% names(df)) {
        df[[col]] <- scales::percent(df[[col]], accuracy = 0.1)
      }
      df
    }

    # Format probability columns (updated column names with Shrunk suffix)
    for (pcol in c("EV Edge (Raw)", "EV Edge (Displayed, Capped)", "Prob Edge on Pick (pp)",
                   "Raw Kelly (%)", "Capped Stake (%)", "Final Stake (%)", "Min Stake (%)",
                   "Blend Home Win % (Shrunk)", "Market Home Win % (Fair, Devig=proportional)", "ML Implied Home % (Raw)",
                   "Blend Pick Win % (Shrunk)", "Market Pick Win % (Devig)")) {
      formatted_tbl <- safe_percent(formatted_tbl, pcol)
    }

    # Format spread columns
    if ("Blend Median Margin" %in% display_cols_fb) {
      formatted_tbl[["Blend Median Margin"]] <- format_signed_spread(formatted_tbl[["Blend Median Margin"]])
    }
    if ("Market Home Spread" %in% display_cols_fb) {
      formatted_tbl[["Market Home Spread"]] <- format_signed_spread(formatted_tbl[["Market Home Spread"]])
    }

    # Format numeric columns
    if ("Market Total" %in% display_cols_fb) {
      formatted_tbl[["Market Total"]] <- format(round(formatted_tbl[["Market Total"]], 1), nsmall = 1)
    }
    # Increase precision for Total EV to 5 decimals to show small values clearly
    if ("Total EV (Units)" %in% display_cols_fb) {
      formatted_tbl[["Total EV (Units)"]] <- format(round(formatted_tbl[["Total EV (Units)"]], 5), nsmall = 5)
    }
    if ("Blend Stake (Units)" %in% display_cols_fb) {
      formatted_tbl[["Blend Stake (Units)"]] <- dplyr::if_else(
        is.na(formatted_tbl[["Blend Stake (Units)"]]),
        "",
        format(round(formatted_tbl[["Blend Stake (Units)"]], 4), nsmall = 4)
      )
    }

    # Format text columns
    if ("Blend Beat Market Basis" %in% display_cols_fb) {
      formatted_tbl[["Blend Beat Market Basis"]] <- dplyr::coalesce(formatted_tbl[["Blend Beat Market Basis"]], "")
    }
    if ("Date" %in% display_cols_fb) {
      formatted_tbl[["Date"]] <- suppressWarnings(format(as.Date(formatted_tbl[["Date"]]), "%b %d, %Y"))
      formatted_tbl[["Date"]] <- dplyr::if_else(is.na(formatted_tbl[["Date"]]) | formatted_tbl[["Date"]] == "NA", "", formatted_tbl[["Date"]])
    }
    if ("Blend Pick" %in% display_cols_fb) {
      formatted_tbl[["Blend Pick"]] <- dplyr::if_else(is.na(formatted_tbl[["Blend Pick"]]), "", formatted_tbl[["Blend Pick"]])
    }
    if ("Winner" %in% display_cols_fb) {
      formatted_tbl[["Winner"]] <- dplyr::if_else(is.na(formatted_tbl[["Winner"]]) | formatted_tbl[["Winner"]] == "", "TBD", formatted_tbl[["Winner"]])
    }

    # Format moneyline columns
    moneyline_cols_fb <- intersect(c("Market Home Moneyline", "Market Away Moneyline", "Blend Home ML (Fair, from Shrunk Prob)", "Blend Home ML (Vigged, +X%)", "Blend Away Moneyline (Vigged)"), display_cols_fb)
    if (length(moneyline_cols_fb)) {
      formatted_tbl <- dplyr::mutate(
        formatted_tbl,
        dplyr::across(dplyr::all_of(moneyline_cols_fb), format_moneyline_strings)
      )
    }

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
        htmltools::tags$div(
          class = "table-bleed",
          htmltools::tags$div(class = "table-wrapper", table_html)
        )
      )

      script_block <- htmltools::tags$script(htmltools::HTML(
        sprintf(
          "(function(){var input=document.getElementById('table-search');var table=document.getElementById('%s');if(!input||!table){return;}var rows=table.getElementsByTagName('tbody')[0].rows;input.addEventListener('input',function(){var query=this.value.toLowerCase();Array.prototype.forEach.call(rows,function(row){var text=row.textContent.toLowerCase();row.style.display=text.indexOf(query)>-1?'':'none';});});})();",
          table_id
        )
      ))

      # ColorBends canvas elements
      colorbends_canvas_fb <- htmltools::tags$div(id = "colorbends-canvas")
      canvas_overlay_fb <- htmltools::tags$div(class = "canvas-overlay")

      doc <- htmltools::tags$html(
        htmltools::tags$head(
          htmltools::tags$meta(charset = "UTF-8"),
          htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
          htmltools::tags$title("NFL Blend vs Market Analysis"),
          htmltools::tags$style(css_block)
        ),
        htmltools::tags$body(
          colorbends_canvas_fb,
          canvas_overlay_fb,
          wrapper,
          script_block,
          htmltools::HTML(colorbends_script_fallback)
        )
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
        "<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\"><title>NFL Blend vs Market Analysis</title><style>",
        css_block,
        "</style></head><body>",
        "<div id=\"colorbends-canvas\"></div>",
        "<div class=\"canvas-overlay\"></div>",
        "<div class=\"page-wrapper\"><input id=\"table-search\" type=\"search\" placeholder=\"Search teams, wagers, or math checks...\" aria-label=\"Search moneyline table\"/>",
        intro_html,
        "<div class=\"table-bleed\"><div class=\"table-wrapper\"><table id=\"",
        table_id,
        "\"><caption>",
        title,
        "</caption><thead><tr><th>",
        header,
        "</th></tr></thead><tbody>",
        paste(body, collapse = ""),
        "</tbody></table></div></div></div><script>(function(){var input=document.getElementById('table-search');var table=document.getElementById('",
        table_id,
        "');if(!input||!table){return;}var rows=table.getElementsByTagName('tbody')[0].rows;input.addEventListener('input',function(){var query=this.value.toLowerCase();Array.prototype.forEach.call(rows,function(row){var text=row.textContent.toLowerCase();row.style.display=text.indexOf(query)>-1?'':'none';});});})();</script>",
        colorbends_script_fallback,
        "</body></html>"
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
    # Extract diagnostic information if available
    diag <- attr(table, "join_diagnostic")
    diag_msg <- if (!is.null(diag)) {
      sprintf(
        paste0(
          "\n  Diagnostic info:\n",
          "    - Scores input had %d rows\n",
          "    - Schedule input had %d rows\n",
          "    - Join columns: %s"
        ),
        diag$scores_n %||% 0L,
        diag$schedule_n %||% 0L,
        paste(diag$join_cols %||% "unknown", collapse = ", ")
      )
    } else {
      ""
    }

    stop(sprintf(
      paste0(
        "moneyline_report(): COMPARISON TABLE IS EMPTY - cannot generate report.\n",
        "%s\n",
        "Common causes:\n",
        "  1. market_comparison_result$comp has no rows (no games evaluated)\n",
        "  2. schedule has no overlapping games (check season/week filter)\n",
        "  3. Join key mismatch between market_comparison_result and schedule\n",
        "  4. All rows filtered out due to missing data\n",
        "Resolution steps:\n",
        "  - Verify compare_to_market() returned non-empty results\n",
        "  - Check that schedule contains games for the requested season/week\n",
        "  - Ensure both inputs use consistent game_id, season, week columns"
      ),
      diag_msg
    ), call. = FALSE)
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
