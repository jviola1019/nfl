# ====================================================================================================
# NFL Week Simulation - SoS-weighted + QB Toggles + Outside Factors
# Requires: tidyverse, lubridate, nflreadr
# ====================================================================================================

load_market_helpers <- local({
  sourced <- FALSE
  function(strict = TRUE) {
    if (isTRUE(sourced)) {
      return(invisible(TRUE))
    }

    candidates <- character(0)
    wd_candidate <- file.path(getwd(), "NFLmarket.R")
    if (file.exists(wd_candidate)) {
      candidates <- c(candidates, wd_candidate)
    }

    this_ofile <- tryCatch({
      candidate <- sys.frames()[[1]]$ofile
      if (!length(candidate) || is.null(candidate)) {
        ""
      } else if (!is.character(candidate)) {
        as.character(candidate[[1L]])
      } else {
        candidate[[1L]]
      }
    }, error = function(e) "")
    if (is.character(this_ofile) && length(this_ofile) && nzchar(this_ofile[[1L]])) {
      this_ofile <- this_ofile[[1L]]
      script_candidate <- file.path(dirname(this_ofile), "NFLmarket.R")
      if (file.exists(script_candidate)) {
        candidates <- c(candidates, script_candidate)
      }
    }

    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      editor_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
      if (nzchar(editor_path)) {
        rstudio_candidate <- file.path(dirname(editor_path), "NFLmarket.R")
        if (file.exists(rstudio_candidate)) {
          candidates <- c(candidates, rstudio_candidate)
        }
      }
    }

    candidates <- unique(candidates)
    success <- FALSE
    last_error <- NULL

    for (path in candidates) {
      tryCatch({
        source(path, chdir = TRUE)
        success <- TRUE
        sourced <<- TRUE
        break
      }, error = function(e) {
        last_error <<- e
      })
    }

    if (!success) {
      msg <- sprintf(
        "Unable to automatically source NFLmarket.R from %s.",
        paste(candidates, collapse = ", ")
      )
      if (!is.null(last_error)) {
        msg <- sprintf("%s Last error: %s", msg, conditionMessage(last_error))
      }
      if (isTRUE(strict)) {
        stop(msg, call. = FALSE)
      }
      warning(msg, call. = FALSE)
    }

    invisible(success)
  }
})

load_market_helpers(strict = TRUE)

if (!exists("shorten_market_note", inherits = TRUE)) {
  shorten_market_note <- function(note, max_chars = 72L) {
    if (is.null(note) || !length(note)) {
      return(note)
    }

    note_chr <- as.character(note)
    is_missing <- is.na(note_chr)
    note_chr[is_missing] <- NA_character_

    to_process <- !is_missing & nzchar(trimws(note_chr))
    if (!any(to_process)) {
      return(note_chr)
    }

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
    note_chr
  }
}

if (!exists("count_duplicate_join_rows", inherits = FALSE)) {
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
}

if (!exists("infer_join_relationship", inherits = FALSE)) {
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
}

if (!exists("collapse_by_keys_strict", inherits = FALSE)) {
  collapse_by_keys_strict <- function(df, keys, label = "data frame") {
    if (is.null(df) || !nrow(df) || !length(keys)) {
      return(df)
    }

    missing_keys <- setdiff(keys, names(df))
    if (length(missing_keys)) {
      stop(sprintf(
        "%s is missing required key columns: %s",
        label,
        paste(missing_keys, collapse = ", ")
      ))
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
      "%s: collapsing %d duplicate rows keyed by %s.",
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
      stop(sprintf(
        "%s: unable to resolve duplicates for %d key combinations.",
        label,
        nrow(dup_check)
      ))
    }

    out
  }
}

if (!exists("parse_datetime_vector", inherits = FALSE)) {
  parse_datetime_vector <- function(x, tz = "UTC") {
    if (is.null(x)) {
      return(as.POSIXct(rep(NA_real_, 0), tz = tz))
    }

    if (inherits(x, "POSIXt")) {
      return(lubridate::with_tz(x, tz))
    }

    x_chr <- as.character(x)
    n <- length(x_chr)
    out <- as.POSIXct(rep(NA_real_, n), tz = tz)
    if (!n) {
      return(out)
    }

    mask <- !is.na(x_chr) & nzchar(x_chr)
    if (!any(mask)) {
      return(out)
    }

    orders <- c(
      "Y-m-d H:M:S z", "Y-m-d H:M:S", "Y-m-d H:M", "Y-m-d I:M:S p", "Y-m-d I:M p",
      "Ymd HMS", "Ymd HM", "Ymd IMS p"
    )
    parsed <- suppressWarnings(lubridate::parse_date_time(x_chr[mask], orders = orders, tz = tz))
    out[mask] <- parsed
    out
  }
}

if (!exists("parse_time_components", inherits = FALSE)) {
  parse_time_components <- function(time_chr) {
    n <- length(time_chr)
    hour <- rep(NA_integer_, n)
    minute <- rep(NA_integer_, n)
    if (!n) {
      return(list(hour = hour, minute = minute))
    }

    vals <- as.character(time_chr)
    vals[!nzchar(vals)] <- NA_character_
    vals[grepl("(?i)^(tbd|tba)$", vals, perl = TRUE)] <- NA_character_
    vals <- stringr::str_replace_all(vals, "(?i)\n", " ")
    vals <- stringr::str_trim(vals)
    vals <- stringr::str_replace_all(vals, "(?i)(am|pm)$", " \1")
    vals <- stringr::str_replace_all(vals, "(?i)\b(ET|EST|EDT|CT|CST|CDT|MT|MST|MDT|PT|PST|PDT)\b", "")
    mask <- !is.na(vals) & nzchar(vals)

    if (any(mask)) {
      combos <- paste("1970-01-01", vals[mask])
      orders <- c("Y-m-d H:M:S", "Y-m-d I:M:S p", "Y-m-d H:M", "Y-m-d I:M p")
      parsed <- suppressWarnings(lubridate::parse_date_time(combos, orders = orders, tz = "UTC"))
      hour[mask] <- suppressWarnings(lubridate::hour(parsed))
      minute[mask] <- suppressWarnings(lubridate::minute(parsed))
    }

    list(hour = hour, minute = minute)
  }
}

if (!exists("select_first_column", inherits = FALSE)) {
  select_first_column <- function(df, candidates) {
    intersect(candidates, names(df))[1]
  }
}

if (!exists("coerce_numeric_safely", inherits = FALSE)) {
  coerce_numeric_safely <- function(x) {
    if (is.numeric(x)) {
      return(x)
    }
    suppressWarnings(as.numeric(x))
  }
}

if (!exists("clamp_probability", inherits = FALSE)) {
  clamp_probability <- function(p, eps = 1e-06) {
    p <- coerce_numeric_safely(p)
    p <- dplyr::if_else(is.na(p), NA_real_, p)
    pmin(pmax(p, eps), 1 - eps)
  }
}

if (!exists("resolve_home_probability", inherits = FALSE)) {
  resolve_home_probability <- function(home_prob,
                                       home_ml = NA_real_,
                                       away_ml = NA_real_,
                                       tolerance = 5e-03) {
    home_prob <- suppressWarnings(as.numeric(home_prob))
    ml_home <- coerce_numeric_safely(home_ml)
    ml_away <- coerce_numeric_safely(away_ml)

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
}

if (!exists("harmonize_home_spread", inherits = FALSE)) {
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
}

if (!exists("harmonize_home_margin", inherits = FALSE)) {
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
}

if (!exists("probability_to_american", inherits = FALSE)) {
  probability_to_american <- function(prob) {
    prob <- clamp_probability(prob)
    dplyr::if_else(
      prob >= 0.5,
      -round(100 * prob / (1 - prob)),
      round(100 * (1 - prob) / prob)
    )
  }
}

if (!exists("apply_moneyline_vig", inherits = FALSE)) {
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
}

if (!exists("expected_value_units", inherits = FALSE)) {
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
}

if (!exists("extract_game_level_scores", inherits = FALSE)) {
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

    # NOTE: This helper is also defined in NFLmarket.R (lines ~1185-1197)
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
}

if (!exists("build_snapshot_moneyline_result", inherits = FALSE)) {
  build_snapshot_moneyline_result <- function(final_df,
                                              season,
                                              week,
                                              model_prob_candidates = c(
                                                "home_p_2w_blend",
                                                "home_win_prob_blend",
                                                "home_p_2w_cal"
                                              ),
                                              market_prob_candidates = c(
                                                "home_p_2w_mkt",
                                                "market_home_prob",
                                                "market_prob"
                                              ),
                                              verbose = TRUE) {
    if (is.null(final_df) || !nrow(final_df)) {
      if (verbose) message("build_snapshot_moneyline_result(): final slate is empty; skipping snapshot.")
      return(tibble::tibble())
    }

    required_keys <- c("game_id", "season", "week")
    missing_keys <- setdiff(required_keys, names(final_df))
    if (length(missing_keys)) {
      if (verbose) {
        message(sprintf(
          "build_snapshot_moneyline_result(): final slate missing required columns: %s",
          paste(missing_keys, collapse = ", ")
        ))
      }
      return(tibble::tibble())
    }

    snapshot <- final_df %>%
      dplyr::filter(.data$season == season, .data$week == week) %>%
      dplyr::ungroup()

    if (!nrow(snapshot)) {
      if (verbose) {
        message(sprintf(
          "build_snapshot_moneyline_result(): no rows for season %s week %s.",
          season,
          week
        ))
      }
      return(tibble::tibble())
    }

    model_prob_col <- select_first_column(snapshot, model_prob_candidates)
    market_prob_col <- select_first_column(snapshot, market_prob_candidates)

    if (is.na(model_prob_col)) {
      if (verbose) {
        message("build_snapshot_moneyline_result(): unable to locate a model probability column on the final slate.")
      }
      return(tibble::tibble())
    }

    if (is.na(market_prob_col)) {
      if (verbose) {
        message("build_snapshot_moneyline_result(): unable to locate a market probability column on the final slate.")
      }
      return(tibble::tibble())
    }

    model_prob <- clamp_probability(snapshot[[model_prob_col]])
    market_prob <- clamp_probability(snapshot[[market_prob_col]])

    tibble::tibble(
      game_id = snapshot$game_id,
      season = snapshot$season,
      week = snapshot$week,
      p_model = model_prob,
      p_mkt = market_prob,
      y2 = as.integer(rep(NA_integer_, nrow(snapshot))),
      b_model = rep(NA_real_, nrow(snapshot)),
      b_mkt = rep(NA_real_, nrow(snapshot)),
      ll_model = rep(NA_real_, nrow(snapshot)),
      ll_mkt = rep(NA_real_, nrow(snapshot)),
      home_team = snapshot$home_team,
      away_team = snapshot$away_team,
      game_date = suppressWarnings(as.Date(snapshot$date)),
      blend_home_median = coerce_numeric_safely(snapshot$home_median_blend),
      blend_away_median = coerce_numeric_safely(snapshot$away_median_blend),
      blend_total_median = coerce_numeric_safely(snapshot$total_median_blend),
      actual_winner = rep(NA_character_, nrow(snapshot))
    )
  }
}

if (!exists("build_moneyline_comparison_table", inherits = FALSE)) {
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

    combined <- rlang::exec(dplyr::inner_join, !!!join_args) %>%
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
        market_home_prob = resolve_home_probability(market_home_prob, market_home_ml, market_away_ml),
        market_away_prob = clamp_probability(1 - market_home_prob),
        market_home_spread = coerce_numeric_safely(market_home_spread),
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
        blend_vs_market_info = purrr::pmap(
          list(
            blend_actual_units,
            market_actual_units,
            blend_ev_units,
            market_ev_units,
            blend_prob_pick,
            market_prob_pick,
            blend_pick,
            blend_moneyline,
            market_moneyline,
            blend_edge_moneyline
          ),
          assess_blend_vs_market
        ),
        blend_beats_market = vapply(
          blend_vs_market_info,
          function(x) {
            res <- x$result
            if (is.null(res)) {
              return(NA)
            }
            res
          },
          logical(1)
        ),
        blend_beats_market_basis = purrr::map_chr(
          blend_vs_market_info,
          function(x) {
            basis <- x$basis
            if (is.null(basis) || is.na(basis)) {
              return(NA_character_)
            }
            basis
          }
        ),
        blend_beats_market_note_raw = purrr::map_chr(
          blend_vs_market_info,
          function(x) {
            detail <- x$detail
            if (is.null(detail) || is.na(detail)) {
              return(NA_character_)
            }
            detail
          }
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
        blend_kelly_fraction = dplyr::case_when(
          is.na(blend_pick_side) ~ NA_real_,
          TRUE ~ {
            dec <- american_to_decimal(market_moneyline)
            b <- dec - 1
            stake <- (blend_prob_pick * b - (1 - blend_prob_pick)) / b
            invalid <- is.na(dec) | !is.finite(dec) | b <= 0 | is.na(stake)
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
  
  
}

if (!exists("export_moneyline_comparison_html", inherits = FALSE)) {
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
      "Blend Away Moneyline (vig)"
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
      "<li><span class=\"metric-icon\">📊</span> <strong>Prob Advantage (pp):</strong> Simple probability difference (in percentage points) between blend and market assessments. <strong>IMPORTANT:</strong> This is NOT the same as EV Edge! Prob Advantage doesn't account for odds/pricing, while EV Edge does. Two games can have similar EV Edge (13.5%) but very different Prob Advantage (4.6% vs 8.0%) because of different odds. Use EV Edge for betting decisions, not Prob Advantage.</li>",
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
        c("EV Edge (%)", "Prob Advantage (pp)"),
        gt::fmt_percent,
        decimals = 2
      )
      gt_tbl <- gt_apply_if_columns(
        gt_tbl,
        c("Blend Home Win %", "Market Home Win %"),
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

      # Helper function to apply color coding safely
      apply_color <- function(gt_tbl, columns, palette, domain) {
        cols_exist <- if (length(columns) == 1) {
          columns %in% display_cols
        } else {
          all(columns %in% display_cols)
        }
        if (!cols_exist) return(gt_tbl)
        tryCatch(
          gt::data_color(gt_tbl, columns = columns,
            colors = scales::col_numeric(palette = palette, domain = domain, na.color = "#374151")),
          error = function(e) gt_tbl
        )
      }

      # Apply color coding for all metric columns
      gt_tbl <- apply_color(gt_tbl, c("Blend Home Win %", "Market Home Win %"),
        c("#1e3a8a", "#2563eb", "#3b82f6", "#60a5fa", "#93c5fd"), c(0, 1))
      gt_tbl <- apply_color(gt_tbl, "Total EV (Units)",
        c("#991B1B", "#dc2626", "#1f2937", "#15803d", "#166534"), c(-0.05, 0.05))
      gt_tbl <- apply_color(gt_tbl, "EV Edge (%)",
        c("#991B1B", "#dc2626", "#1f2937", "#15803d", "#166534"), c(-0.15, 0.15))
      gt_tbl <- apply_color(gt_tbl, "Prob Advantage (pp)",
        c("#7f1d1d", "#991b1b", "#1f2937", "#14532d", "#15532d"), c(-0.30, 0.30))
      gt_tbl <- apply_color(gt_tbl, c("Market Home Spread", "Blend Median Margin"),
        c("#dc2626", "#f87171", "#1f2937", "#4ade80", "#22c55e"), c(-14, 14))
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
        table.font.names = c("Source Sans Pro", "Helvetica Neue", "Arial", "sans-serif"),
        table.background.color = "#020617",
        table.font.color = "#e2e8f0",
        heading.background.color = "#0f172a",
        column_labels.background.color = "#1e293b",
        column_labels.font.weight = "bold",
        column_labels.text_transform = "uppercase",
        row.striping.background_color = "#111827"
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
          colors = scales::col_factor(
            palette = c("No" = "#1f2937", "Yes" = "#166534", "N/A" = "#374151"),
            domain = c("No", "Yes", "N/A")
          )
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
      gt_tbl <- gt::tab_style(
        gt_tbl,
        style = list(
          gt::cell_fill(color = "#1e293b"),
          gt::cell_text(color = "#f8fafc", weight = "bold")
        ),
        locations = gt::cells_column_labels(columns = gt::everything())
      )

      try({
        gt::gtsave(gt_tbl, file = file)
        saved <- TRUE
      }, silent = TRUE)
    }
  
    if (!saved) {
      css_block <- paste0(
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
        "#table-search {width: 100%; max-width: 500px; padding: 0.85rem 1.25rem; margin: 0 auto; border-radius: 999px; border: 1px solid rgba(59, 130, 246, 0.4); background-color: rgba(15,23,42,0.9); color: #f8fafc; display: block; box-shadow: 0 4px 12px rgba(59, 130, 246, 0.2); transition: all 0.2s ease; font-size: 1rem;}\n",
        "#table-search:focus {outline: none; border-color: #60a5fa; box-shadow: 0 0 0 3px rgba(96,165,250,0.4), 0 8px 20px rgba(59, 130, 246, 0.3); transform: translateY(-1px);}\n",
        "#table-search::placeholder {color: rgba(148, 163, 184, 0.7);}\n",
        "table {width: 100%; border-collapse: collapse; background-color: rgba(15,23,42,0.94); color: #e2e8f0; border-radius: 18px;}\n",
        "thead th {background-color: #1e293b; color: #f8fafc; text-transform: uppercase; letter-spacing: 0.08em; position: sticky; top: 85px; z-index: 100;}\n",
        "td, th {padding: 10px 12px; border-bottom: 1px solid rgba(30, 41, 59, 0.5); text-align: center;}\n",
        "td.text-left {text-align: left;}\n",
        "td.winner-cell {color: #fcd34d; font-weight: 600;}\n",
        "tr:nth-child(even) {background-color: rgba(15,23,42,0.65);}\n",
        "tbody tr:hover {background-color: rgba(37,99,235,0.18); transition: background-color 180ms ease;}\n",
        "tr.blend-win {background-color: rgba(22,101,52,0.75);}\n",
        "tr.blend-win td {color: #ecfdf5;}\n",
        "td.blend-reco {background-color: #1d4ed8 !important; color: #f8fafc !important; font-weight: 600;}\n",
        "caption {caption-side: top; font-size: 1.25rem; font-weight: 600; margin-bottom: 0.75rem; color: #f8fafc;}\n",
        "@media (max-width: 768px) { body {padding-top: 75px;} thead th {top: 75px;} table {font-size: 0.88rem;} th {font-size: 0.7rem;} .report-intro {padding: 1.25rem;} #table-search {font-size: 0.9rem; padding: 0.75rem 1rem;} }\n"
      )
  
      formatted_tbl <- display_tbl %>%
        dplyr::mutate(
          `EV Edge (%)` = scales::percent(`EV Edge (%)`, accuracy = 0.1),
          `Prob Advantage (pp)` = scales::percent(`Prob Advantage (pp)`, accuracy = 0.1),
          `Market Home Win %` = scales::percent(`Market Home Win %`, accuracy = 0.1),
          `Blend Home Win %` = scales::percent(`Blend Home Win %`, accuracy = 0.1),
          `Blend Median Margin` = format_signed_spread(`Blend Median Margin`),
          `Market Home Spread` = format_signed_spread(`Market Home Spread`),
          `Market Total` = format(round(`Market Total`, 1), nsmall = 1),
          `Total EV (Units)` = format(round(`Total EV (Units)`, 3), nsmall = 3),
          `Blend Stake (Units)` = dplyr::if_else(
            is.na(`Blend Stake (Units)`),
            "",
            format(round(`Blend Stake (Units)`, 3), nsmall = 3)
          ),
          dplyr::across(
            c("Market Home Moneyline", "Market Away Moneyline"),
            format_moneyline_strings
          ),
          `Blend Pick` = dplyr::if_else(is.na(`Blend Pick`), "", `Blend Pick`),
          `Blend Prob` = dplyr::if_else(is.na(`Blend Prob`), "", `Blend Prob`),
          `Market Prob` = dplyr::if_else(is.na(`Market Prob`), "", `Market Prob`),
          `Blend Recommendation` = dplyr::if_else(
            is.na(`Blend Recommendation`),
            "",
            `Blend Recommendation`
          ),
          Winner = dplyr::coalesce(Winner, "")
        )
  
      table_id <- "simulation-moneyline-table"

      if (requireNamespace("htmltools", quietly = TRUE)) {
        left_align_cols <- c(
          "Matchup", "Winner", "Blend Favorite", "Market Favorite",
          "Blend Prob", "Market Prob", "Blend Recommendation",
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
                if (identical(col_name, "Winner") && !is.na(value) && nzchar(value) && value != "TBD") {
                  cell_classes <- c(cell_classes, "winner-cell")
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

        table_html <- htmltools::tags$table(
          id = table_id,
          htmltools::tags$caption(title),
          htmltools::tags$thead(htmltools::tags$tr(purrr::map(names(formatted_tbl), htmltools::tags$th))),
          htmltools::tags$tbody(rows)
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
            table_html
          )
        )

        script_block <- htmltools::tags$script(htmltools::HTML(
          sprintf(
            "(function(){var input=document.getElementById('table-search');var table=document.getElementById('%s');if(!input||!table){return;}var rows=table.getElementsByTagName('tbody')[0].rows;input.addEventListener('input',function(){var query=this.value.toLowerCase();Array.prototype.forEach.call(rows,function(row){var text=row.textContent.toLowerCase();row.style.display=text.indexOf(query)>-1?'':'none';});});})();",
            table_id
          )
        ))

        doc <- htmltools::tags$html(
          htmltools::tags$head(htmltools::tags$style(css_block)),
          htmltools::tags$body(search_box, content_wrapper, script_block)
        )

        htmltools::save_html(doc, file = file)
        saved <- TRUE
      } else {
        header <- paste(names(formatted_tbl), collapse = "</th><th>")
        left_align_cols <- c(
          "Matchup", "Winner", "Blend Favorite", "Blend Recommendation",
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
              if (identical(col_name, "Winner") && !is.null(value) && !is.na(value) && nzchar(value) && value != "TBD") {
                cell_classes <- c(cell_classes, "winner-cell")
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
          "</style></head><body><div class=\"page-wrapper\"><input id=\"table-search\" type=\"search\" placeholder=\"Search teams, winners, or picks...\" aria-label=\"Search moneyline table\"/>",
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
  open_
}


if (!exists("build_res_blend", inherits = FALSE)) {
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
    prob_col <- prob_candidates[prob_candidates %in% names(per_game)][1]
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
}

suppressPackageStartupMessages({
  source("NFLbrier_logloss.R")
  library(tidyverse)
  library(lubridate)
  library(nflreadr)
  library(scales)
  library(digest)
  library(glmmTMB)   # NB GLMM
  library(nnet)      # multinomial calibration
  library(randtoolbox) # Sobol QMC
  library(httr2)
  options(nflreadr.cache = TRUE)
  options(live_refresh = TRUE)
  library(dplyr, warn.conflicts = FALSE)
})

xfun_meets_min <- tryCatch({
  if (!requireNamespace("xfun", quietly = TRUE)) {
    FALSE
  } else {
    isTRUE(utils::packageVersion("xfun") >= "0.52")
  }
}, error = function(e) FALSE)

has_namespace <- function(pkg, needs_new_xfun = FALSE) {
  if (needs_new_xfun && !xfun_meets_min) {
    return(FALSE)
  }
  tryCatch(requireNamespace(pkg, quietly = TRUE), error = function(e) FALSE)
}

gt_available <- has_namespace("gt", needs_new_xfun = TRUE)
reactable_available <- has_namespace("reactable")

if (!gt_available) {
  if (!xfun_meets_min) {
    message("Package 'gt' skipped because 'xfun' >= 0.52 is unavailable; falling back to simple tables.")
  } else {
    message("Package 'gt' is not available; skipping gt-based tables.")
  }
}
if (!reactable_available) {
  message("Package 'reactable' is not available; interactive slate table will be skipped.")
}

# Only define if it doesn't already exist
if (!exists(".get_this_file", inherits = FALSE)) {
  .get_this_file <- function() {
    # script path if running via source()
    if (!is.null(sys.frames()[[1]]$ofile)) {
      return(normalizePath(sys.frames()[[1]]$ofile))
    }
    # RStudio editor path if available
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
      if (nzchar(p)) return(normalizePath(p))
    }
    # fallback -> safe placeholder, adjust if you want
    "NFLsimulation.R"
  }
}

.calib_cache_dir <- file.path(path.expand("~"), ".cache", "nfl_sim_calib_cache")
if (!dir.exists(.calib_cache_dir)) dir.create(.calib_cache_dir, recursive = TRUE)

# ---- Backtest (score_weeks) cache ----
.score_cache_dir <- file.path(path.expand("~"), ".cache", "nfl_sim_scores")
if (!dir.exists(.score_cache_dir)) dir.create(.score_cache_dir, recursive = TRUE)

score_cache_key <- function(start_season, end_season, weeks, trials, seed, rho) {
  digest::digest(list(
    tag="score_weeks",
    start_season, end_season, weeks = paste0(weeks, collapse=","),
    trials, seed, rho,
    N_RECENT, USE_SOS, SOS_STRENGTH, RECENCY_HALFLIFE
  ))
}

calib_cache_key <- function(season, n_years, halflife, use_sos, sos_pow, trials, rho, seed){
  digest::digest(list(
    tag = "calib_sim_df_nb",
    season = season,
    n_years = n_years,
    halflife = halflife,
    use_sos = use_sos,
    sos_pow = sos_pow,
    trials = trials,
    rho = round(rho, 6),
    seed = seed
  ))
}

# --- Color palettes for cell backgrounds ---
pal_fav <- function(p) {
  ifelse(is.na(p), "#ffffff", scales::col_numeric("RdYlGn", domain = c(0,1))(p))
}


pal_total <- function(total) {
  # Blue scale for totals, 35-55 typical range
  scales::col_numeric("Blues", domain = c(35,55))(total)
}

clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)


# Margin-to-probability conversion -------------------------------------------------
# NFL margin distributions are noisy; to keep win probabilities aligned with the
# projected scoring gap we shrink the simulation margin standard deviation toward
# a league-wide prior before mapping to a 2-way win chance.  This keeps a
# 2-point median edge from translating into an implausible 70% win probability
# while still letting large gaps push probabilities toward the extremes.
MARGIN_PROB_PRIOR_SD <- 6.5   # empirically tuned vs. 2015-2023 results
MARGIN_PROB_MIN_SD   <- 4.5   # minimum effective spread volatility (points)
MARGIN_PROB_MAX_SD   <- 18.0  # cap to avoid runaway certainty on blowouts

margin_probs_from_summary <- function(margin_mean, margin_sd, tie_prob) {
  mm <- ifelse(is.finite(margin_mean), margin_mean, 0)
  sd_raw <- ifelse(is.finite(margin_sd) & margin_sd > 0, margin_sd, MARGIN_PROB_MIN_SD)
  sd_eff <- sqrt(sd_raw^2 + MARGIN_PROB_PRIOR_SD^2)
  sd_eff <- clamp(sd_eff, MARGIN_PROB_MIN_SD, MARGIN_PROB_MAX_SD)

  tie_prob <- ifelse(is.finite(tie_prob), tie_prob, 0)
  tie_prob <- clamp(tie_prob, 0, 0.25)
  two_way_mass <- clamp(1 - tie_prob, 1e-9, 1)

  home_p_2w <- stats::pnorm(mm / sd_eff)
  home_p_2w <- clamp(home_p_2w, 1e-6, 1 - 1e-6)

  home_win <- clamp(home_p_2w * two_way_mass, 0, two_way_mass)
  away_win <- two_way_mass - home_win

  list(
    home_win_prob = home_win,
    away_win_prob = away_win,
    tie_prob      = 1 - two_way_mass,
    home_p_2w     = home_p_2w,
    away_p_2w     = 1 - home_p_2w,
    sd_effective  = sd_eff
  )
}
# =============================================================================
# CONFIGURATION - Load from config.R
# =============================================================================
# **TO CHANGE THE WEEK: Edit config.R and change WEEK_TO_SIM value**
# All configuration is centralized in config.R for easy management

if (file.exists("config.R")) {
  source("config.R")
  cat(sprintf("\n✓ Configuration loaded: Season %d, Week %d\n\n", SEASON, WEEK_TO_SIM))
} else {
  # Fallback defaults if config.R doesn't exist
  cat("\n⚠ Warning: config.R not found. Using default configuration.\n")
  cat("  Create config.R file to customize settings.\n\n")

  SEASON      <- year(Sys.Date())
  WEEK_TO_SIM <- 11
  N_TRIALS    <- 100000
  N_RECENT    <- 6
  SEED        <- 471

  # R 4.5.1 compatibility
  if (getRversion() >= "4.5.0") {
    suppressWarnings(RNGversion("4.5.0"))
  }
  set.seed(SEED)

  GLMM_BLEND_W <- 0.38
  BLEND_META_MODEL    <- getOption("nfl_sim.blend_model",    default = "glmnet")
  BLEND_ALPHA         <- getOption("nfl_sim.blend_alpha",    default = 0.25)
  CALIBRATION_METHOD  <- getOption("nfl_sim.calibration",    default = "isotonic")
  USE_SOS            <- TRUE
  SOS_STRENGTH       <- 0.45
  USE_RECENCY_DECAY  <- TRUE
  RECENCY_HALFLIFE   <- 3

  # Situational adjustments
  REST_SHORT_PENALTY <- -0.85
  REST_LONG_BONUS    <- 0.0
  BYE_BONUS          <- +1.0
  DEN_ALTITUDE_BONUS <- 0.0
  DIVISION_GAME_ADJUST   <- -0.2
  CONFERENCE_GAME_ADJUST <- 0.0

  # Weather parameters
  DOME_BONUS_TOTAL <- 0.8
  OUTDOOR_WIND_PEN <- -1.0
  COLD_TEMP_PEN    <- -0.5
  RAIN_SNOW_PEN    <- -0.8
  WIND_IMPACT      <- -0.08
  COLD_IMPACT      <- -0.15
  PRECIP_IMPACT    <- -1.5
}

# =============================================================================
# Parameter Validation - Catch missing required parameters early
# =============================================================================

# Validate all required parameters exist
.required_params <- c(
  "SEASON", "WEEK_TO_SIM", "N_TRIALS", "N_RECENT", "SEED",
  "GLMM_BLEND_W", "SOS_STRENGTH", "RECENCY_HALFLIFE",
  "REST_SHORT_PENALTY", "BYE_BONUS", "DIVISION_GAME_ADJUST",
  "DOME_BONUS_TOTAL", "OUTDOOR_WIND_PEN", "COLD_TEMP_PEN", "RAIN_SNOW_PEN",
  "WIND_IMPACT", "COLD_IMPACT", "PRECIP_IMPACT"
)
.missing_params <- .required_params[!sapply(.required_params, exists, envir = .GlobalEnv)]
if (length(.missing_params)) {
  stop("Missing required config parameters: ", paste(.missing_params, collapse = ", "),
       "\nEnsure config.R defines and exports all required parameters.", call. = FALSE)
}
rm(.required_params, .missing_params)

# =============================================================================
# Parameters below are set by config.R or fallback defaults above
# Do not modify here - edit config.R instead
# =============================================================================

# Player availability impact scalars (points per aggregated severity unit)
# These are derived from INJURY_WEIGHT_* parameters in config.R
SKILL_AVAIL_POINT_PER_FLAG     <- 0.55
TRENCH_AVAIL_POINT_PER_FLAG    <- 0.65
SECONDARY_AVAIL_POINT_PER_FLAG <- 0.45
FRONT7_AVAIL_POINT_PER_FLAG    <- 0.50

# Statistical rigor constants (added for code quality and reproducibility)
NB_SIZE_MIN         <- 5        # Minimum overdispersion parameter (NFL scores typically k ∈ [5,15])
NB_SIZE_MAX         <- 50       # Maximum overdispersion parameter (tighter than 1e4 for stability)
ISOTONIC_EPSILON    <- 0.01     # Isotonic calibration bounds (keep predictions in [0.01, 0.99])
BETA_PRIOR_STRENGTH <- 6        # Beta prior strength for Bayesian smoothing
CONFIDENCE_LEVEL    <- 0.95     # Confidence level for prediction intervals
MIN_SAMPLE_SIZE     <- 500      # Minimum sample for reliable estimates
REGRESSION_BASE     <- 0.3      # Base regression to mean factor
REGRESSION_GAMES    <- 6        # Games required to reduce regression (halflife)

# Reference sheet for common tuning knobs.  Call `show_tuning_help()` from an
# interactive session to review the levers and the metrics they typically move.
.tuning_parameters <- tibble::tribble(
  ~parameter, ~default, ~recommended_range, ~metrics_to_watch, ~notes,
  "GLMM_BLEND_W", GLMM_BLEND_W, "0.25 to 0.55", "Win rate, Brier, log loss",
  "Increase to trust the GLMM's structured priors; decrease to lean on pace + EPA base. Higher weights can steady calibration when market data are noisy but may cap upside if the priors lag current form.",
  "SOS_STRENGTH", SOS_STRENGTH, "0.4 to 0.8", "Win rate, ret_total",
  "Controls how aggressively schedule strength shapes opponent adjustments. Raising it helps when the model underrates teams with tough slates but can overshoot if weighted too heavily after upsets.",
  "RECENCY_HALFLIFE", RECENCY_HALFLIFE, "2 to 5", "Win rate, Brier",
  "Shorter halflife doubles down on the latest form; longer halflife smooths week-to-week noise. Use smaller values when injuries or scheme changes shift performance quickly.",
  "REST_SHORT_PENALTY", REST_SHORT_PENALTY, "-1.0 to -0.4", "ret_total",
  "More negative numbers punish teams on <=6 days rest. Strengthen the penalty when the sim overestimates tired road teams. VALIDATED: p=0.003, significant effect.",
  "BYE_BONUS", BYE_BONUS, "0.6 to 1.4", "Win rate, ret_total",
  "Adjust when bye-week teams fail to cover expected improvements. Higher values help capture coordinators' self-scouting gains but may inflate totals if stacked with other bonuses. VALIDATED: p=0.009, significant effect.",
  "DOME_BONUS_TOTAL", DOME_BONUS_TOTAL, "0.4 to 1.2", "Totals, ret_total",
  "Positive values lift scoring expectations indoors. Increase when indoor unders show value because the model stays too low on dome efficiency.",
  "OUTDOOR_WIND_PEN", OUTDOOR_WIND_PEN, "-1.4 to -0.6", "Totals, Brier",
  "More negative values cut totals in windy games. Tighten when the blend misses weather-driven unders; ease off if it overreacts to moderate breezes.",
  "RAIN_SNOW_PEN", RAIN_SNOW_PEN, "-1.2 to -0.4", "Totals, log loss",
  "Rain/snow adjustment applied via `game_modifiers`. Increase magnitude when sloppy games still go over the projected total.",
  "SKILL_AVAIL_POINT_PER_FLAG", SKILL_AVAIL_POINT_PER_FLAG, "0.4 to 0.7", "Win rate, Brier",
  "Translates aggregated WR/RB/TE injury flags into point adjustments. Raise when talent gaps fail to move projections enough; lower if the sim double-counts absences with market odds.",
  "TRENCH_AVAIL_POINT_PER_FLAG", TRENCH_AVAIL_POINT_PER_FLAG, "0.5 to 0.8", "ret_total",
  "Impacts OL/DL cluster injuries. Stronger penalties help when line mismatches drive ATS losses; weaker when the model overreacts to questionable tags.",
  "SECONDARY_AVAIL_POINT_PER_FLAG", SECONDARY_AVAIL_POINT_PER_FLAG, "0.35 to 0.6", "Totals, log loss",
  "Raise to bump overs when secondaries are depleted; reduce if the market already prices in those matchups and the model overstates shootout risk.",
  "FRONT7_AVAIL_POINT_PER_FLAG", FRONT7_AVAIL_POINT_PER_FLAG, "0.35 to 0.65", "Totals, ret_total",
  "Controls front-seven injury effects. Increase when run-stopping issues aren't reflected; decrease if defensive depth masks absences.",
  "DIVISION_GAME_ADJUST", DIVISION_GAME_ADJUST, "-0.3 to -0.1", "Win rate, Totals, Brier",
  "Negative values reduce totals in division games due to familiarity and defensive preparation. VALIDATED: p=0.078 (marginally significant), reduced from -0.4 to -0.2 as effect was weaker than expected."
)

show_tuning_help <- function(metric = NULL) {
  stopifnot(length(metric) <= 1)
  guide <- .tuning_parameters
  if (!is.null(metric) && nzchar(metric)) {
    metric_pattern <- stringr::str_to_lower(metric)
    guide <- guide %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(metrics_to_watch), metric_pattern))
  }
  if (nrow(guide) == 0) {
    message("No tuning rows matched. Available metrics: ",
            paste(sort(unique(.tuning_parameters$metrics_to_watch)), collapse = ", "))
    return(invisible(guide))
  }
  print(guide)
  invisible(guide)
}

if (interactive() && !isTRUE(getOption("nfl_sim.quiet_tuning_help", FALSE))) {
  message("Tuning reference: run show_tuning_help() for parameter guidance or pass a metric name (e.g. 'ret_total').")
}

# Optional: per-game manual adjustments (apply to mu &/or sd after all calcs)
# Columns:
#   game_id (or use away/home to match), mu_home_adj, mu_away_adj, sd_home_adj, sd_away_adj
# Positive mu_*_adj raises expected points; sd_*_adj widens spread.
game_modifiers <- tibble(
  game_id     = character(),
  mu_home_adj = numeric(),
  mu_away_adj = numeric(),
  sd_home_adj = numeric(),
  sd_away_adj = numeric(),
  dome        = logical(),   # TRUE/FALSE for dome/closed roof
  windy       = logical(),   # if TRUE apply wind penalty
  cold        = logical(),   # if TRUE apply cold penalty
  precip      = logical(),   # if TRUE apply rain/snow penalty
  wind_mph    = numeric(),
  temp_f      = numeric(),
  precip_prob = numeric()
)

# ------------------------ DATA LOAD -------------------------------------------
seasons_hfa <- (SEASON - 9):(SEASON - 0)
sched <- load_schedules(seasons = seasons_hfa) |>
  mutate(game_completed = !is.na(home_score) & !is.na(away_score))

# CRITICAL: Verify season column exists (fail fast with clear error if missing)
if (!"season" %in% names(sched)) {
  stop("CRITICAL ERROR: load_schedules() did not return a 'season' column.\n",
       "This may indicate an nflreadr package version mismatch.\n",
       "Available columns: ", paste(sort(names(sched)), collapse = ", "), "\n",
       "Please ensure nflreadr is up to date: install.packages('nflreadr')")
}


# --- NEW: normalize date column to 'game_date' (Date) no matter what the package gives us
date_col <- dplyr::case_when(
  "game_date" %in% names(sched) ~ "game_date",
  "gameday" %in% names(sched) ~ "gameday",
  "game_date_time" %in% names(sched) ~ "game_date_time",
  TRUE ~ NA_character_
)
if (is.na(date_col)) stop("Could not find a game date column in schedules.")

sched <- sched |>
  mutate(game_date = as.Date(.data[[date_col]]))  # creates/overwrites a proper Date column

sched_dates <- sched %>%
  transmute(game_id, sched_date = as.Date(game_date))

# --- normalize the venue/stadium column to 'venue' (character)
venue_col <- dplyr::case_when(
  "venue" %in% names(sched) ~ "venue",
  "stadium" %in% names(sched) ~ "stadium",
  "site" %in% names(sched) ~ "site",
  "game_site" %in% names(sched) ~ "game_site",
  TRUE ~ NA_character_
)
sched <- sched |>
  mutate(venue = if (!is.na(venue_col)) as.character(.data[[venue_col]]) else NA_character_)

# --- Load team division/conference data for division game indicators
team_info <- tryCatch({
  nflreadr::load_teams() %>%
    dplyr::select(team_abbr, team_conf, team_division) %>%
    dplyr::rename(team = team_abbr)
}, error = function(e) {
  warning("Could not load team division/conference data: ", conditionMessage(e))
  tibble::tibble(team = character(), team_conf = character(), team_division = character())
})

# Helper to pick the first existing column among candidates
pick_col <- function(df, candidates, label){
  nm <- intersect(candidates, names(df))
  if (length(nm) == 0) stop(sprintf("Could not find a column for %s. Have: %s",
                                    label, paste(names(df), collapse = ", ")))
  nm[1]
}

# Internal helper used throughout the simulation/reporting pipeline to pull the
# first available column name from a set of aliases without throwing if none
# exist. This mirrors the late-file definition historically relied upon by the
# market helpers but needs to be present before first use when the script is
# sourced end-to-end.
.pick_col2 <- function(df, cands){
  nm <- intersect(cands, names(df))
  if (length(nm)) nm[1] else NA_character_
}

season_col    <- pick_col(sched, c("season","Season","season_year","year"), "season")
week_col      <- pick_col(sched, c("week","Week","game_week","gameday"), "week")
gametype_col  <- pick_col(sched, c("game_type","season_type","season_type_name"), "game type")
home_col      <- pick_col(sched, c("home_team","home_team_abbr","team_home","home"), "home team")
away_col      <- pick_col(sched, c("away_team","away_team_abbr","team_away","away"), "away team")

# Normalize to standard names we'll use everywhere
sched <- sched %>%
  mutate(
    season_std   = .data[[season_col]],
    week_std     = .data[[week_col]],
    game_type_std= .data[[gametype_col]],
    home_team    = .data[[home_col]],
    away_team    = .data[[away_col]]
  )


week_slate <- sched %>%
  filter(season_std == SEASON, week_std == WEEK_TO_SIM, game_type_std == "REG") %>%
  transmute(
    game_id,
    season = season_std,
    week   = week_std,
    game_type = game_type_std,
    game_date,
    home_team,
    away_team,
    venue = as.character(venue)   # <-- only the normalized column
  ) %>%
  distinct()

kickoff_time_col  <- .pick_col2(sched, c("game_time_et", "gametime_et", "game_time", "gametime", "start_time", "kickoff"))
kickoff_tz_col    <- .pick_col2(sched, c("game_time_tz", "gametime_tz", "game_time_zone", "kickoff_tz"))
kickoff_utc_col   <- .pick_col2(sched, c("game_datetime", "game_date_time", "game_time_utc", "kickoff_utc"))

n_sched <- nrow(sched)
kickoff_time_raw <- if (!is.na(kickoff_time_col)) as.character(sched[[kickoff_time_col]]) else rep(NA_character_, n_sched)
kickoff_tz_raw   <- if (!is.na(kickoff_tz_col)) as.character(sched[[kickoff_tz_col]]) else rep(NA_character_, n_sched)
kickoff_utc_vec  <- if (!is.na(kickoff_utc_col)) {
  parse_datetime_vector(sched[[kickoff_utc_col]], tz = "UTC")
} else {
  as.POSIXct(rep(NA_real_, n_sched), tz = "UTC")
}

time_parts <- parse_time_components(kickoff_time_raw)
hour_vals <- time_parts$hour
minute_vals <- time_parts$minute
hour_clean <- ifelse(is.na(hour_vals), 0L, hour_vals)
minute_clean <- ifelse(is.na(minute_vals), 0L, minute_vals)

base_dates <- as.Date(sched$game_date)
base_et <- as.POSIXct(base_dates, tz = "America/New_York")
fallback_et <- base_et + lubridate::hours(hour_clean) + lubridate::minutes(minute_clean)
fallback_et[is.na(base_dates) | is.na(hour_vals)] <- NA

kickoff_et <- fallback_et
mask_utc <- !is.na(kickoff_utc_vec)
if (any(mask_utc)) {
  kickoff_et[mask_utc] <- lubridate::with_tz(kickoff_utc_vec[mask_utc], "America/New_York")
}

kickoff_utc <- kickoff_utc_vec
missing_utc <- is.na(kickoff_utc) & !is.na(kickoff_et)
if (any(missing_utc)) {
  kickoff_utc[missing_utc] <- lubridate::with_tz(kickoff_et[missing_utc], "UTC")
}

kickoff_info <- tibble::tibble(
  game_id = sched$game_id,
  kickoff_time_raw = kickoff_time_raw,
  kickoff_tz_raw = kickoff_tz_raw,
  kickoff_utc = kickoff_utc,
  kickoff_et = kickoff_et,
  kickoff_et_hour = ifelse(
    !is.na(kickoff_et),
    as.integer(lubridate::hour(lubridate::with_tz(kickoff_et, "America/New_York"))),
    NA_integer_
  ),
  kickoff_display_et = ifelse(
    !is.na(kickoff_et),
    format(lubridate::with_tz(kickoff_et, "America/New_York"), "%a %b %d %I:%M %p ET"),
    NA_character_
  )
)

week_slate <- week_slate %>%
  dplyr::left_join(
    kickoff_info %>%
      dplyr::select(game_id, kickoff_et, kickoff_utc, kickoff_et_hour, kickoff_time_raw, kickoff_tz_raw, kickoff_display_et),
    by = "game_id"
  )

# Add division/conference indicators
if (nrow(team_info) > 0) {
  week_slate <- week_slate %>%
    dplyr::left_join(team_info %>% dplyr::rename(home_division = team_division, home_conf = team_conf),
                     by = c("home_team" = "team")) %>%
    dplyr::left_join(team_info %>% dplyr::rename(away_division = team_division, away_conf = team_conf),
                     by = c("away_team" = "team")) %>%
    dplyr::mutate(
      division_game = !is.na(home_division) & !is.na(away_division) & home_division == away_division,
      conference_game = !is.na(home_conf) & !is.na(away_conf) & home_conf == away_conf
    ) %>%
    dplyr::select(-home_division, -away_division, -home_conf, -away_conf)
} else {
  week_slate <- week_slate %>%
    dplyr::mutate(division_game = FALSE, conference_game = FALSE)
}

sched <- sched %>%
  mutate(
    season_std    = .data[[season_col]],
    week_std      = .data[[week_col]],
    game_type_std = .data[[gametype_col]],
    home_team     = .data[[home_col]],
    away_team     = .data[[away_col]]
  ) %>%
  # TODO: make the select/any_of calls explicit
  dplyr::select(dplyr::everything(), -dplyr::any_of(c("season", "week", "game_type"))) %>%
  dplyr::rename(
    season    = season_std,
    week      = week_std,
    game_type = game_type_std
  )

# Add division/conference indicators to full schedule (not just week_slate)
if (nrow(team_info) > 0) {
  sched <- sched %>%
    dplyr::left_join(team_info %>% dplyr::rename(home_division = team_division, home_conf = team_conf),
                     by = c("home_team" = "team")) %>%
    dplyr::left_join(team_info %>% dplyr::rename(away_division = team_division, away_conf = team_conf),
                     by = c("away_team" = "team")) %>%
    dplyr::mutate(
      division_game = !is.na(home_division) & !is.na(away_division) & home_division == away_division,
      conference_game = !is.na(home_conf) & !is.na(away_conf) & home_conf == away_conf
    ) %>%
    dplyr::select(-home_division, -away_division, -home_conf, -away_conf)
} else {
  sched <- sched %>%
    dplyr::mutate(division_game = FALSE, conference_game = FALSE)
}

sched <- standardize_join_keys(sched)

sched <- collapse_by_keys_strict(
  sched,
  c("game_id", "season", "week"),
  label = "Simulation schedule"
)

dup_sched_keys <- sched %>%
  dplyr::filter(dplyr::if_all(dplyr::all_of(c("game_id", "season", "week")), ~ !is.na(.))) %>%
  dplyr::count(dplyr::across(dplyr::all_of(c("game_id", "season", "week")))) %>%
  dplyr::filter(.data$n > 1L)

if (nrow(dup_sched_keys)) {
  stop(sprintf(
    "Simulation schedule still has %d duplicate game/week combinations after collapsing.",
    nrow(dup_sched_keys)
  ))
}

stadium_coords <- tribble(
  ~venue, ~lat, ~lon, ~dome,
  "GEHA Field at Arrowhead Stadium", 39.0490, -94.4839, FALSE,
  "Highmark Stadium",                42.7738, -78.7868, FALSE,
  "Soldier Field",                   41.8625, -87.6166, FALSE,
  "Paycor Stadium",                  39.0954, -84.5160, FALSE,
  "Cleveland Browns Stadium",        41.5061, -81.6995, FALSE,
  "AT&T Stadium",                    32.7473, -97.0945, TRUE,
  "Empower Field at Mile High",      39.7439, -105.0201, FALSE,
  "Ford Field",                      42.3400, -83.0456, TRUE,
  "Lambeau Field",                   44.5013, -88.0622, FALSE,
  "NRG Stadium",                     29.6847, -95.4107, TRUE,
  "Lucas Oil Stadium",               39.7601, -86.1639, TRUE,
  "EverBank Stadium",                30.3240, -81.6387, FALSE,
  "Allegiant Stadium",               36.0909, -115.1830, TRUE,
  "SoFi Stadium",                    33.9535, -118.3391, TRUE,
  "Hard Rock Stadium",               25.9580, -80.2389, FALSE,
  "U.S. Bank Stadium",               44.9736, -93.2572, TRUE,
  "Gillette Stadium",                42.0909, -71.2643, FALSE,
  "Caesars Superdome",               29.9509, -90.0815, TRUE,
  "MetLife Stadium",                 40.8136, -74.0745, FALSE,
  "Lincoln Financial Field",         39.9008, -75.1675, FALSE,
  "Acrisure Stadium",                40.4468, -80.0158, FALSE,
  "Levi's Stadium",                  37.4030, -121.9690, FALSE,
  "Lumen Field",                     47.5952, -122.3316, FALSE,
  "Raymond James Stadium",           27.9759, -82.5033, FALSE,
  "Nissan Stadium",                  36.1665, -86.7713, FALSE,
  "FedExField",                      38.9078, -76.8645, FALSE,
  "M&T Bank Stadium",                39.2779, -76.6227, FALSE,
  "Bank of America Stadium",         35.2251, -80.8526, FALSE,
  "State Farm Stadium",              33.5277, -112.2626, TRUE
) %>%
  mutate(venue_key = stringr::str_to_lower(stringr::str_replace_all(venue, "[^a-zA-Z0-9]+", " ")))
week_slate <- week_slate %>%
  dplyr::mutate(
    venue_key = stringr::str_to_lower(stringr::str_replace_all(venue, "[^a-zA-Z0-9]+", " "))
  ) %>%
  dplyr::left_join(stadium_coords %>% dplyr::select(venue_key, dome), by = "venue_key")

# guarantee the column exists before coalesce()
if (!"dome" %in% names(week_slate)) {
  week_slate <- week_slate %>% mutate(dome = FALSE)
}

week_slate <- week_slate %>% mutate(dome = coalesce(dome, FALSE))

# ============================== TIME ZONES / SIMPLE TRAVEL TAX =============================
team_tz <- tribble(
  ~team, ~tz,
  "ARI","America/Phoenix",
  "ATL","America/New_York",
  "BAL","America/New_York",
  "BUF","America/New_York",
  "CAR","America/New_York",
  "CHI","America/Chicago",
  "CIN","America/New_York",
  "CLE","America/New_York",
  "DAL","America/Chicago",
  "DEN","America/Denver",
  "DET","America/Detroit",
  "GB","America/Chicago",
  "HOU","America/Chicago",
  "IND","America/Indiana/Indianapolis",
  "JAX","America/New_York",
  "KC","America/Chicago",
  "LAC","America/Los_Angeles",
  "LAR","America/Los_Angeles",
  "LV","America/Los_Angeles",
  "MIA","America/New_York",
  "MIN","America/Chicago",
  "NE","America/New_York",
  "NO","America/Chicago",
  "NYG","America/New_York",
  "NYJ","America/New_York",
  "PHI","America/New_York",
  "PIT","America/New_York",
  "SEA","America/Los_Angeles",
  "SF","America/Los_Angeles",
  "TB","America/New_York",
  "TEN","America/Chicago",
  "WAS","America/New_York"
)

week_slate <- week_slate %>%
  dplyr::left_join(team_tz %>% dplyr::rename(home_tz = tz), by = c("home_team" = "team")) %>%
  dplyr::left_join(team_tz %>% dplyr::rename(away_tz = tz), by = c("away_team" = "team")) %>%
  dplyr::mutate(
    kickoff_tz = dplyr::coalesce(kickoff_tz_raw, home_tz),
    kickoff_et_hour = dplyr::coalesce(
      kickoff_et_hour,
      ifelse(
        !is.na(kickoff_et),
        as.integer(lubridate::hour(lubridate::with_tz(kickoff_et, "America/New_York"))),
        NA_integer_
      )
    ),
    kickoff_local = dplyr::case_when(
      !is.na(kickoff_utc) & !is.na(kickoff_tz) ~ lubridate::with_tz(kickoff_utc, kickoff_tz),
      !is.na(kickoff_et) & !is.na(kickoff_tz) ~ lubridate::with_tz(kickoff_et, kickoff_tz),
      TRUE ~ kickoff_et
    ),
    kickoff_local_hour = ifelse(!is.na(kickoff_local), as.integer(lubridate::hour(kickoff_local)), NA_integer_),
    kickoff_display_local = dplyr::case_when(
      !is.na(kickoff_local) ~ format(kickoff_local, "%a %b %d %I:%M %p %Z"),
      !is.na(kickoff_display_et) ~ kickoff_display_et,
      TRUE ~ NA_character_
    )
  )

#tz_penalty() vector:
tz_penalty <- function(home_tz, away_tz, kick_et_hour = 13L){
  west <- c("America/Los_Angeles","America/Denver","America/Phoenix")
  east <- c("America/New_York","America/Detroit","America/Indiana/Indianapolis")
  is_we <- (away_tz %in% west) & (home_tz %in% east)
  hour_clean <- suppressWarnings(as.integer(kick_et_hour))
  hour_clean[is.na(hour_clean)] <- 13L
  is_early <- hour_clean <= 13L
  pen <- ifelse(!is.na(home_tz) & !is.na(away_tz) & is_we & is_early, -0.4, 0)  # tax (negative)
  pen
}

# Then build travel_tbl WITHOUT multiplying by -1:
travel_tbl <- week_slate %>%
  dplyr::mutate(
    kickoff_et_hour = dplyr::coalesce(kickoff_et_hour, 13L),
    travel_mu_adj_home = 0,
    travel_mu_adj_away = tz_penalty(home_tz, away_tz, kickoff_et_hour)  # already negative when taxed
  ) %>%
  dplyr::select(game_id, travel_mu_adj_home, travel_mu_adj_away)

if (nrow(week_slate) == 0) stop(sprintf("No games found for %s Wk %s.", SEASON, WEEK_TO_SIM))

teams_on_slate <- sort(unique(c(week_slate$home_team, week_slate$away_team)))

# =============================== INJURIES (schema-agnostic + safe) ===============================
# Load injury data with multiple fallback sources
# Data sources (in order of preference):
# 1. nflreadr::load_injuries() - Primary source (recommended as of 2025)
#    https://nflreadr.nflverse.com/reference/load_injuries.html
# 2. nflfastR::fast_scraper_injuries() - Alternative scraper
# 3. Returns empty tibble if no data available (model defaults to zero injury impact)
safe_load_injuries <- function(seasons, prefer_fast = TRUE, ...) {
  seasons <- sort(unique(seasons))
  if (!length(seasons)) {
    message("No seasons specified for injury data; returning empty tibble.")
    return(tibble::tibble())
  }

  message(sprintf("Loading injury data for seasons: %s", paste(seasons, collapse = ", ")))

  use_fast <- isTRUE(prefer_fast) && requireNamespace("nflfastR", quietly = TRUE)
  if (isTRUE(prefer_fast) && !use_fast) {
    message("nflfastR not installed; using nflreadr::load_injuries() instead.")
  }

  fast_env <- if (use_fast) asNamespace("nflfastR") else NULL
  fast_bulk <- NULL
  fast_scraper <- NULL

  if (!is.null(fast_env)) {
    fast_bulk <- get0("load_injuries", envir = fast_env, inherits = FALSE)
    if (is.function(fast_bulk)) {
      fast_bulk <- tryCatch(
        fast_bulk(seasons = seasons, ...),
        error = function(e) {
          warning(
            sprintf(
              "nflfastR::load_injuries failed (%s); falling back to per-season loaders.",
              conditionMessage(e)
            ),
            call. = FALSE
          )
          NULL
        }
      )
    } else {
      fast_bulk <- NULL
    }

    fast_scraper <- get0("fast_scraper_injuries", envir = fast_env, inherits = FALSE)
    if (!is.function(fast_scraper)) {
      fast_scraper <- NULL
    }
  }

  missing <- integer(0)
  pieces <- lapply(seasons, function(season) {
    if (is.data.frame(fast_bulk) && nrow(fast_bulk)) {
      bulk_slice <- fast_bulk
      if ("season" %in% names(bulk_slice)) {
        bulk_slice <- dplyr::filter(bulk_slice, .data$season %in% season)
      }
      if (nrow(bulk_slice)) {
        return(bulk_slice)
      }
    }

    fast_result <- NULL

    if (is.function(fast_scraper)) {
      fast_result <- tryCatch(
        fast_scraper(season = season),
        error = function(e) {
          msg <- conditionMessage(e)
          if (grepl("404", msg, fixed = TRUE)) {
            missing <<- c(missing, season)
            message(sprintf(
              "Injury release for season %s is unavailable yet; skipping it.",
              season
            ))
            return(tibble::tibble())
          }
          warning(
            sprintf(
              "nflfastR::fast_scraper_injuries failed for season %s (%s); falling back to nflreadr::load_injuries().",
              season, msg
            ),
            call. = FALSE
          )
          return(NULL)
        }
      )

      if (is.data.frame(fast_result)) {
        return(fast_result)
      }
    }

    tryCatch(
      {
        result <- nflreadr::load_injuries(seasons = season, ...)
        if (is.data.frame(result) && nrow(result) > 0) {
          message(sprintf("✓ Loaded %d injury records for season %s from nflreadr", nrow(result), season))
        }
        result
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("404", msg, fixed = TRUE) || grepl("not found", msg, ignore.case = TRUE)) {
          missing <<- c(missing, season)
          message(sprintf(
            "Injury release for season %s is unavailable yet; model will use zero injury impact for this season.",
            season
          ))
          return(tibble::tibble())
        }
        warning(sprintf(
          "nflreadr::load_injuries failed for season %s (%s). Model will use zero injury impact.",
          season, msg
        ), call. = FALSE)
        return(tibble::tibble())
      }
    )
  })

  injuries <- dplyr::bind_rows(pieces)

  if (length(missing)) {
    message(
      sprintf(
        "ℹ Injury data not available for seasons: %s. Model will use zero injury impact for these seasons.",
        paste(missing, collapse = ", ")
      )
    )
  }

  if (nrow(injuries) == 0) {
    message("⚠ No injury data loaded. Model will run with zero injury impact for all teams.")
    message("  To get injury data, ensure nflreadr is updated: install.packages('nflreadr')")
    message("  Source: https://nflreadr.nflverse.com/reference/load_injuries.html")
  } else {
    message(sprintf("✓ Successfully loaded %d total injury records", nrow(injuries)))
  }

  injuries
}

inj_all <- safe_load_injuries(
  seasons = sort(unique(sched$season)),
  file_type = getOption("nflreadr.prefer", default = "rds")
)

inj_pick <- function(df, candidates) {
  nm <- intersect(candidates, names(df))
  if (length(nm)) nm[1] else NA_character_
}

calc_injury_impacts <- function(df, group_vars = c("team")) {
  if (!is.data.frame(df) || !nrow(df)) {
    return(tibble())
  }

  required <- c("team", "position", "status")
  if (!all(required %in% names(df))) return(tibble())

  missing_groups <- setdiff(group_vars, names(df))
  if (length(missing_groups)) return(tibble())

  df %>%
    dplyr::mutate(
      status = toupper(trimws(.data$status))
    ) %>%
    dplyr::mutate(
      base_pen = dplyr::case_when(
        grepl("OUT|IR", status)       ~ -0.50,
        grepl("DOUBTFUL", status)     ~ -0.35,
        grepl("QUESTIONABLE", status) ~ -0.20,
        grepl("LIMITED", status)      ~ -0.10,
        TRUE                          ~  0
      ),
      pos_group = dplyr::case_when(
        position %in% c("QB")                                        ~ "qb",
        position %in% c("T","OT","LT","RT","G","OG","C","OL")  ~ "trenches",
        position %in% c("WR","TE","RB","FB","HB")              ~ "skill",
        position %in% c("CB","S","SS","FS","DB")               ~ "secondary",
        position %in% c("LB","ILB","OLB","EDGE")               ~ "front7",
        position %in% c("DL","DT","DE","NT","IDL")             ~ "front7",
        TRUE                                                         ~ "other"
      ),
      pos_wt = dplyr::case_when(
        pos_group == "qb"        ~ 0.0,
        pos_group == "trenches"  ~ 1.3,
        pos_group == "skill"     ~ 1.05,
        pos_group == "secondary" ~ 0.95,
        pos_group == "front7"    ~ 0.85,
        TRUE                     ~ 0.6
      ),
      severity = dplyr::case_when(
        grepl("OUT|IR", status)       ~ 1.0,
        grepl("DOUBTFUL", status)     ~ 0.7,
        grepl("QUESTIONABLE", status) ~ 0.4,
        grepl("LIMITED", status)      ~ 0.2,
        TRUE                          ~ 0.0
      ),
      pen = base_pen * pos_wt
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
    dplyr::summarise(
      inj_off_pts_raw = sum(dplyr::if_else(position == "QB", 0, pen), na.rm = TRUE),
      inj_def_pts_raw = sum(dplyr::if_else(position %in% c("CB","S","SS","FS","DB","LB","ILB","OLB","EDGE","DL","DT","DE","NT","IDL"),
                                           -pen, 0), na.rm = TRUE),
      n_listed = dplyr::n(),
      skill_avail_pen = sum(dplyr::if_else(pos_group == "skill", severity, 0), na.rm = TRUE),
      trench_avail_pen = sum(dplyr::if_else(pos_group == "trenches", severity, 0), na.rm = TRUE),
      secondary_avail_pen = sum(dplyr::if_else(pos_group == "secondary", severity, 0), na.rm = TRUE),
      front7_avail_pen = sum(dplyr::if_else(pos_group == "front7", severity, 0), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      w = n_listed / (n_listed + 8),
      inj_off_pts = pmax(pmin(inj_off_pts_raw * w,  -1.5), -4.0),
      inj_def_pts = pmax(pmin(inj_def_pts_raw * w,  +1.5),  0.0),
      skill_avail_pen = pmin(skill_avail_pen, 6),
      trench_avail_pen = pmin(trench_avail_pen, 6),
      secondary_avail_pen = pmin(secondary_avail_pen, 6),
      front7_avail_pen = pmin(front7_avail_pen, 6)
    ) %>%
    dplyr::select(dplyr::all_of(group_vars), inj_off_pts, inj_def_pts,
                  skill_avail_pen, trench_avail_pen, secondary_avail_pen,
                  front7_avail_pen)
}

inj_hist_features <- tibble()

if (!is.data.frame(inj_all) || nrow(inj_all) == 0) {
  inj_team_effects <- tibble(
    team = teams_on_slate,
    inj_off_pts = 0, inj_def_pts = 0,
    skill_avail_pen = 0, trench_avail_pen = 0,
    secondary_avail_pen = 0, front7_avail_pen = 0
  )
} else {
  col_season <- inj_pick(inj_all, c("season","Season","season_year","year"))
  col_week   <- inj_pick(inj_all, c("week","Week","game_week","gameday"))
  col_team   <- inj_pick(inj_all, c("team","team_abbr","club_code","club"))
  col_pos    <- inj_pick(inj_all, c("position","pos"))
  col_status <- inj_pick(inj_all, c("game_status","status","practice_status",
                                    "player_status","report_status"))

  if (is.na(col_team)) {
    # no team column -> nothing we can do; default to zeros
    inj_team_effects <- tibble(
      team = teams_on_slate,
      inj_off_pts = 0, inj_def_pts = 0,
      skill_avail_pen = 0, trench_avail_pen = 0,
      secondary_avail_pen = 0, front7_avail_pen = 0
    )
  } else {
    inj_prepped <- inj_all %>%
      transmute(
        season   = if (!is.na(col_season)) .data[[col_season]] else NA_integer_,
        week     = if (!is.na(col_week))   .data[[col_week]]   else NA_integer_,
        team     = toupper(as.character(.data[[col_team]])),
        position = toupper(as.character(coalesce(if (!is.na(col_pos))    .data[[col_pos]]    else NA_character_, ""))),
        status   = toupper(as.character(coalesce(if (!is.na(col_status)) .data[[col_status]] else NA_character_, "")))
      )

    inj_hist_features <- calc_injury_impacts(
      inj_prepped %>% dplyr::filter(!is.na(season), !is.na(week), nzchar(team)),
      group_vars = c("season", "week", "team")
    )

    inj_week <- inj_prepped %>%
      { if (!is.na(col_season)) dplyr::filter(., season == SEASON) else . } %>%
      { if (!is.na(col_week))   dplyr::filter(., week   == WEEK_TO_SIM) else . } %>%
      dplyr::filter(team %in% teams_on_slate)

    inj_team_effects <- calc_injury_impacts(inj_week, group_vars = c("team"))

    if (!nrow(inj_team_effects)) {
      inj_team_effects <- tibble(
        team = teams_on_slate,
        inj_off_pts = 0, inj_def_pts = 0,
        skill_avail_pen = 0, trench_avail_pen = 0,
        secondary_avail_pen = 0, front7_avail_pen = 0
      )
    }
  }
}


# ------------------------ TEAM GAME ROWS (for recent form & SoS) --------------
# Helper to find the first existing column among candidates
first_col <- function(df, candidates, label){
  nm <- intersect(candidates, names(df))
  if (length(nm) == 0) stop(sprintf("Could not find a column for %s. Have: %s",
                                    label, paste(names(df), collapse=", ")))
  nm[1]
}

# Map possible schemas -> standardized columns
home_team_col <- first_col(sched, c("home_team","home_team_abbr","team_home","home"), "home team")
away_team_col <- first_col(sched, c("away_team","away_team_abbr","team_away","away"), "away team")
home_pts_col  <- first_col(sched, c("home_score","home_points","score_home","home_pts"), "home score")
away_pts_col  <- first_col(sched, c("away_score","away_points","score_away","away_pts"), "away score")

# --- League home-field advantage (points), data-driven over recent seasons ---
# pick neutral-site flag if present
neutral_col <- intersect(c("neutral_site","neutral","is_neutral"), names(sched))

# start with REG games in target seasons
sched_hfa <- sched |>
  dplyr::filter(game_type == "REG", season %in% seasons_hfa)

# remove neutral-site games if a flag exists
if (length(neutral_col)) {
  sched_hfa <- sched_hfa |>
    dplyr::filter(!.data[[neutral_col[1]]])
}

# build margins and compute league HFA
hfa_sample <- sched_hfa |>
  dplyr::filter(is.finite(.data[[home_pts_col]]), is.finite(.data[[away_pts_col]])) |>
  dplyr::transmute(margin = .data[[home_pts_col]] - .data[[away_pts_col]])

league_hfa <- mean(hfa_sample$margin, na.rm = TRUE)
if (!is.finite(league_hfa)) league_hfa <- 1.6  # safe fallback

# Team-specific HFA with shrinkage toward league mean
team_hfa_tbl <- sched_hfa %>%
  transmute(
    team_home = .data[[home_team_col]],
    team_away = .data[[away_team_col]],
    margin    = .data[[home_pts_col]] - .data[[away_pts_col]]
  ) %>%
  group_by(team_home) %>%
  summarise(
    hfa_raw = mean(margin, na.rm = TRUE),
    n       = n(),
    .groups = "drop"
  ) %>%
  mutate(
    w   = n / (n + 60),                   # shrinkage weight (tune 60 if you want)
    hfa = w * hfa_raw + (1 - w) * league_hfa
  ) %>%
  transmute(team = team_home, hfa)

# Guarantee a value for every slate team; apply DEN altitude flavor
hfa_tbl <- tibble(team = teams_on_slate) %>%
  left_join(team_hfa_tbl, by = "team") %>%
  mutate(
    hfa = coalesce(hfa, league_hfa),
    hfa = ifelse(team == "DEN", hfa + DEN_ALTITUDE_BONUS, hfa)
  )

# Create a standardized view
sched_std <- sched |>
  mutate(
    home_team_std  = .data[[home_team_col]],
    away_team_std  = .data[[away_team_col]],
    home_score_std = .data[[home_pts_col]],
    away_score_std = .data[[away_pts_col]]
  )

# Build team-by-game rows (no pivot_longer needed)
team_games_home <- sched_std |>
  filter(game_type %in% c("REG","POST"), game_completed) |>
  transmute(
    game_id, season, week, game_date,
    team           = home_team_std,
    opponent       = away_team_std,
    is_home        = TRUE,
    points_for     = home_score_std,
    points_against = away_score_std
  )

team_games_away <- sched_std |>
  filter(game_type %in% c("REG","POST"), game_completed) |>
  transmute(
    game_id, season, week, game_date,
    team           = away_team_std,
    opponent       = home_team_std,
    is_home        = FALSE,
    points_for     = away_score_std,
    points_against = home_score_std
  )

team_games <- bind_rows(team_games_home, team_games_away) |>
  arrange(game_date)

# Initialize RHO_SCORE (score correlation parameter) if not already defined
if (!exists("RHO_SCORE")) RHO_SCORE <- NA_real_

# Estimate league rho from data
if (is.na(RHO_SCORE)) {
  score_pairs <- sched_std |>
    dplyr::filter(game_type == "REG", game_completed) |>
    dplyr::transmute(h = home_score_std, a = away_score_std)
  rho_hat <- suppressWarnings(cor(score_pairs$h, score_pairs$a, use = "complete.obs"))

  # Seasonal distribution of rho to set empirical bounds
  rho_season <- sched_std |>
    dplyr::filter(game_type == "REG", game_completed) |>
    dplyr::group_by(season) |>
    dplyr::summarise(rho = suppressWarnings(cor(home_score_std, away_score_std, use = "complete.obs")),
                     .groups = "drop")

  RHO_LO <- quantile(rho_season$rho, 0.05, na.rm = TRUE)
  RHO_HI <- quantile(rho_season$rho, 0.95, na.rm = TRUE)

  RHO_SCORE <- if (is.na(RHO_SCORE)) rho_hat else RHO_SCORE
  RHO_SCORE <- pmin(pmax(RHO_SCORE, RHO_LO), RHO_HI)
}

# Data-driven points cap: 99.9th percentile of single-team scores (last 10y)
PTS_CAP_HI <- team_games |>
  summarise(cap = ceiling(quantile(points_for, 0.999, na.rm = TRUE))) |>
  pull(cap)

# Keep a sensible floor and a small cushion
PTS_CAP_HI <- max(60, PTS_CAP_HI + 2)

# league offensive/defensive baselines for SoS
league_off_ppg <- team_games |>
  group_by(game_id) |>
  summarise(total = sum(points_for), .groups = "drop") |>
  summarise(mean_total = mean(total)) |>
  pull(mean_total) / 2

league_def_ppg <- league_off_ppg  # symmetry in points for/against


# Helper: recency weights
recency_weights <- function(n, halflife = RECENCY_HALFLIFE) {
  if (!USE_RECENCY_DECAY || n <= 1) return(rep(1, n))
  i <- 0:(n-1)  # most recent is index 0
  (1/2)^(i/halflife)
}

# Helper: last N_RECENT games up to week cutoff
get_recent <- function(team_abbr, season, week, n_recent = N_RECENT) {
  tg <- team_games |>
    filter(team == team_abbr) |>
    filter((season < !!season) | (season == !!season & week < !!week)) |>
    arrange(desc(game_date)) |>
    slice_head(n = n_recent)
  tg
}

# Opponent strength tables (rolling last 16 games per opponent)
opp_strength <- function(cut_season, cut_week) {
  opp_rows <- map_dfr(
    teams_on_slate,
    ~{
      g <- team_games |>
        filter(team == .x) |>
        filter((season < !!cut_season) | (season == !!cut_season & week < !!cut_week)) |>
        arrange(desc(game_date)) |>
        slice_head(n = 16)
      if (nrow(g) == 0) tibble(team = .x, off_ppg = league_off_ppg, def_ppg = league_def_ppg)
      else tibble(team = .x,
                  off_ppg = mean(g$points_for),
                  def_ppg = mean(g$points_against))
    }
  )
  opp_rows
}

opp_tab <- opp_strength(SEASON, WEEK_TO_SIM)

# ------------------------ RECENT FORM with SoS & Recency ----------------------
recent_form <- map_dfr(
  teams_on_slate,
  ~{
    g <- get_recent(.x, SEASON, WEEK_TO_SIM, N_RECENT)
    if (nrow(g) == 0) {
      tibble(team = .x,
             off_mean_raw = 21, def_mean_raw = 21,
             off_sd_raw = 7,   def_sd_raw = 7,
             n_games = 0)
    } else {
      # Recency weights (most recent first)
      w <- recency_weights(nrow(g))
      # Normalize weights to sum 1
      w <- w / sum(w)

      # SoS adjustment factors (opponent defensive strength vs league)
      if (USE_SOS) {
        g2 <- g |>
          dplyr::left_join(
            opp_tab |>
              dplyr::select(team, def_ppg, off_ppg) |>
              dplyr::rename(opp_def_ppg = def_ppg, opp_off_ppg = off_ppg),
            by = c("opponent" = "team")
          )

        # If opponent rows missing, fill with league baselines
        g2 <- g2 |>
          mutate(
            opp_def_ppg = ifelse(is.na(opp_def_ppg), league_def_ppg, opp_def_ppg),
            opp_off_ppg = ifelse(is.na(opp_off_ppg), league_off_ppg, opp_off_ppg),
            # Factors <1 for better defenses; >1 for weaker defenses
            def_factor  = (opp_def_ppg / league_def_ppg),
            off_factor  = (opp_off_ppg / league_off_ppg)
          )

        # Blend raw PF/PA to an SoS-neutralized estimate using factors^SOS_STRENGTH
        pf_adj <- g2$points_for     * (g2$def_factor ^ SOS_STRENGTH)
        pa_adj <- g2$points_against * (g2$off_factor ^ SOS_STRENGTH)

        off_mean_raw <- sum(w * pf_adj)
        def_mean_raw <- sum(w * pa_adj)
        off_sd_raw   <- sqrt(weighted.mean((pf_adj - off_mean_raw)^2, w))
        def_sd_raw   <- sqrt(weighted.mean((pa_adj - def_mean_raw)^2, w))
      } else {
        off_mean_raw <- sum(w * g$points_for)
        def_mean_raw <- sum(w * g$points_against)
        off_sd_raw   <- sqrt(weighted.mean((g$points_for     - off_mean_raw)^2, w))
        def_sd_raw   <- sqrt(weighted.mean((g$points_against - def_mean_raw)^2, w))
      }

      tibble(team = .x,
             off_mean_raw = off_mean_raw,
             def_mean_raw = def_mean_raw,
             off_sd_raw   = ifelse(is.finite(off_sd_raw), off_sd_raw, 7),
             def_sd_raw   = ifelse(is.finite(def_sd_raw), def_sd_raw, 7),
             n_games      = nrow(g))
    }
  }
)

# Ensure we have PBP loaded for the last 10 completed seasons
if (!exists("pbp_hist")) {
  seasons_pbp <- (SEASON - 9):SEASON
  pbp_hist <- nflreadr::load_pbp(seasons = seasons_pbp)
}

# Optional: refresh PBP and injuries right now (bypass cache once)
# Turn on with: options(live_refresh = TRUE)
if (isTRUE(getOption("live_refresh", FALSE))) {
  old_cache <- getOption("nflreadr.cache", TRUE)
  options(nflreadr.cache = FALSE)
  pbp_hist <- nflreadr::load_pbp(seasons = seasons_pbp)
  inj_all  <- safe_load_injuries(
    seasons  = SEASON,
    file_type = getOption("nflreadr.prefer", default = "rds")
  )
  options(nflreadr.cache = old_cache)
}


# --- AS-OF DATE for this slate (season/week min date) ---
slate_date <- min(as.Date(week_slate$game_date), na.rm = TRUE)

# ---- Guard: define PBP selectors BEFORE QB block ----
if (!exists("posteam_col")) {
  pick_col <- function(df, candidates, label){
    available <- names(df)
    if (is.null(available)) {
      available <- character(0)
    }
    nm <- intersect(candidates, available)
    if (!length(nm)) {
      stop(
        sprintf(
          "Could not find a column for %s. Have: %s",
          label,
          if (length(available)) paste(available, collapse = ", ") else "<none>"
        )
      )
    }
    nm[1]
  }
  stype_col   <- intersect(c("season_type","game_type","season_type_name"), names(pbp_hist))
  posteam_col <- pick_col(
    pbp_hist,
    c(
      "posteam","pos_team","offense","offense_team",
      "possession_team","possessionTeam","team"
    ),
    "offense team (posteam)"
  )
  defteam_col <- pick_col(
    pbp_hist,
    c(
      "defteam","def_team","defense","defense_team",
      "defensive_team","possession_team_def","defTeam"
    ),
    "defense team (defteam)"
  )
  drive_col   <- pick_col(pbp_hist, c("drive","fixed_drive","Drive"), "drive number")
}

explosive_off <- pbp_hist %>%
  dplyr::filter(qb_dropback == 1 | rush == 1) %>%
  dplyr::mutate(expl = yards_gained >= 20) %>%
  dplyr::group_by(team = .data[[posteam_col]]) %>%
  dplyr::summarise(expl_rate_off = mean(expl, na.rm = TRUE), .groups = "drop")

explosive_def <- pbp_hist %>%
  dplyr::filter(qb_dropback == 1 | rush == 1) %>%
  dplyr::mutate(expl = yards_gained >= 20) %>%
  dplyr::group_by(team = .data[[defteam_col]]) %>%
  dplyr::summarise(expl_rate_def = mean(expl, na.rm = TRUE), .groups = "drop")


# ============================== DATA-DRIVEN QB IMPACT =============================
# Uses nflfastR PBP to estimate each team's QB importance from on/off EPA/play,
# then multiplies by a plays-per-game scalar to convert to points.
# Injury status for QB comes from nflreadr::load_injuries for the current week.

# 1) Identify each team's primary QB up to the slate date, and compute on/off EPA/play
qb_onoff <- pbp_hist %>%
  # Restrict to plays before the slate (your slate_date is already computed above)
  inner_join(sched_dates, by = "game_id") %>%
  filter(sched_date < slate_date) %>%
  # Use dropbacks to capture both passes & sacks/scrambles attributed to QB
  filter(.data[[posteam_col]] %in% teams_on_slate, !is.na(qb_dropback), qb_dropback == 1) %>%
  transmute(
    team   = .data[[posteam_col]],
    passer = coalesce(passer_id, passer_player_id, passer_player_name),
    epa    = epa
  ) %>%
  filter(!is.na(team), !is.na(passer))

# Primary QB = most dropbacks YTD (before slate)
primary_qb <- qb_onoff %>%
  count(team, passer, name = "db") %>%
  group_by(team) %>%
  slice_max(db, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(primary_passer = passer)

# On/off EPA/play for each team relative to primary QB
qb_importance <- qb_onoff %>%
  left_join(primary_qb, by = "team") %>%
  mutate(is_primary = passer == primary_passer) %>%
  group_by(team) %>%
  summarise(
    epa_per_play_on  = mean(epa[is_primary], na.rm = TRUE),
    epa_per_play_off = mean(epa[!is_primary], na.rm = TRUE),
    n_on  = sum(is_primary, na.rm = TRUE),
    n_off = sum(!is_primary, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # If there's no off-sample yet, shrink toward league with a small penalty
    league_epa = mean(qb_onoff$epa, na.rm = TRUE),
    epa_per_play_off = ifelse(is.finite(epa_per_play_off), epa_per_play_off, league_epa - 0.02),
    epa_diff = epa_per_play_on - epa_per_play_off
  )

# Convert EPA/play difference into points per game.
# Rule of thumb: 1 EPA/play over ~55 team offensive plays -> 55 points (too big),
# but QB only affects the *dropback* slice. A conservative scalar works well:
EPA_TO_POINTS_SCALAR <- 34   # ~0.15 EPA/play -> ~5 points, ~0.10 -> ~3.4 points
qb_importance <- qb_importance %>%
  mutate(
    qb_points_importance = pmin(pmax(epa_diff * EPA_TO_POINTS_SCALAR, 0), 10)  # cap at +10
  )

# 2) Read this week's injury report and extract QB status for teams on the slate
qb_status_from_inj <- {
  col_team   <- inj_pick(inj_all, c("team","team_abbr","club_code","club"))
  col_pos    <- inj_pick(inj_all, c("position","pos"))
  col_status <- inj_pick(inj_all, c("game_status","status","practice_status",
                                    "player_status","report_status"))
  col_season <- inj_pick(inj_all, c("season","Season","season_year","year"))
  col_week   <- inj_pick(inj_all, c("week","Week","game_week","gameday"))

  if (is.na(col_team) || is.na(col_pos) || is.na(col_status)) {
    tibble(team = character(), qb_flag = character())
  } else {
    inj_all %>%
      transmute(
        season = if (!is.na(col_season)) .data[[col_season]] else NA_integer_,
        week   = if (!is.na(col_week))   .data[[col_week]]   else NA_integer_,
        team   = toupper(as.character(.data[[col_team]])),
        pos    = toupper(as.character(.data[[col_pos]])),
        status = toupper(as.character(.data[[col_status]]))
      ) %>%
      { if (!is.na(col_season)) filter(., season == SEASON) else . } %>%
      { if (!is.na(col_week))   filter(., week   == WEEK_TO_SIM) else . } %>%
      filter(team %in% teams_on_slate, pos == "QB") %>%
      # Coalesce multiple QB entries to the most severe flag
      mutate(qb_flag = case_when(
        grepl("IR|OUT", status)        ~ "OUT",
        grepl("DOUBTFUL", status)      ~ "DOUBTFUL",
        grepl("QUESTIONABLE", status)  ~ "QUESTIONABLE",
        grepl("LIMITED|PART", status)  ~ "LIMITED",
        TRUE                           ~ "OK"
      )) %>%
      group_by(team) %>%
      summarise(
        qb_flag = case_when(
          any(qb_flag == "OUT")        ~ "OUT",
          any(qb_flag == "DOUBTFUL")   ~ "DOUBTFUL",
          any(qb_flag == "QUESTIONABLE") ~ "QUESTIONABLE",
          any(qb_flag == "LIMITED")    ~ "LIMITED",
          TRUE                         ~ "OK"
        ),
        .groups = "drop"
      )
  }
}

# 3) Map QB flag importance to points & uncertainty adjustments
sev_mult <- c(OUT = 1.00, DOUBTFUL = 0.70, QUESTIONABLE = 0.40, LIMITED = 0.25, OK = 0.00)

qb_adjustments <- tibble(team = teams_on_slate) %>%
  left_join(dplyr::select(qb_importance, team, qb_points_importance), by = "team") %>%
  left_join(qb_status_from_inj, by = "team") %>%
  mutate(
    qb_flag = coalesce(qb_flag, "OK"),
    qb_points_importance = coalesce(qb_points_importance, 2.0),  # gentle default
    sev = unname(sev_mult[qb_flag]),
    off_points_adj = - sev * qb_points_importance,
    sd_points_adj  = 0.4 * sev + 0.6 * (abs(off_points_adj) / 6),
    sd_points_adj  = pmin(pmax(sd_points_adj, 0), 2.0)
  ) %>%
  dplyr::select(team, status = qb_flag, off_points_adj, sd_points_adj)

# 4) Use these QB adjustments in place of the manual qb_status
qb_status <- qb_adjustments
# ============================== DATA-DRIVEN QB IMPACT =============================


# Merge HFA and QB status
recent_form <- recent_form |>
  left_join(hfa_tbl, by = "team") |>
  left_join(qb_status, by = "team") |>
  mutate(
    status = coalesce(status, "starter_ok"),
    off_points_adj = coalesce(off_points_adj, 0),
    sd_points_adj  = coalesce(sd_points_adj, 0)
  )

recent_form <- recent_form %>%
  mutate(
    off_sd_raw = pmax(off_sd_raw, 5.5),
    def_sd_raw = pmax(def_sd_raw, 5.5)
  )

# Join into recent_form (so we can use home_/away_ prefixed columns later)
recent_form <- recent_form %>%
  left_join(inj_team_effects, by = c("team" = "team")) %>%
  mutate(
    inj_off_pts = coalesce(inj_off_pts, 0),
    inj_def_pts = coalesce(inj_def_pts, 0),
    skill_avail_pen = coalesce(skill_avail_pen, 0),
    trench_avail_pen = coalesce(trench_avail_pen, 0),
    secondary_avail_pen = coalesce(secondary_avail_pen, 0),
    front7_avail_pen = coalesce(front7_avail_pen, 0)
  )


# ------------------------ REST / BYE EFFECTS ----------------------------------
# Compute days since last game for each team (before the slate week)
last_game <- team_games |>
  group_by(team) |>
  filter((season < SEASON) | (season == SEASON & week < WEEK_TO_SIM)) |>
  arrange(desc(game_date)) |>
  slice_head(n = 1) |>
  ungroup() |>
  dplyr::select(team, last_date = game_date, last_season = season, last_week = week)

rest_tbl <- tibble(team = teams_on_slate) |>
  left_join(last_game, by = "team") |>
  mutate(
    # If no prior game this season, guess ~10 days rest (neutral)
    days_rest = as.numeric(ifelse(is.na(last_date),
                                  10,
                                  as.Date(week_slate$game_date[1]) - as.Date(last_date))),
    short_rest = !is.na(days_rest) & days_rest <= 6,
    long_rest  = !is.na(days_rest) & days_rest >= 9,
    bye_prev   = !is.na(last_season) & (last_season == SEASON) &
      (ifelse(is.na(last_week), FALSE, (WEEK_TO_SIM - last_week) >= 2))
  ) |>
  mutate(
    rest_points = 0 +
      ifelse(short_rest, REST_SHORT_PENALTY, 0) +
      ifelse(long_rest & !bye_prev, REST_LONG_BONUS, 0) +
      ifelse(bye_prev, BYE_BONUS, 0)
  ) |>
  dplyr::select(team, days_rest, rest_points)

recent_form <- recent_form |>
  left_join(rest_tbl, by = "team") |>
  mutate(rest_points = coalesce(rest_points, 0))
# ------------------------ PACE (Drives/Game) ----------------------------------
# Filter REG depending on available season type column
pbp_for_pace <- if (length(stype_col)) {
  sname <- stype_col[1]
  pbp_hist |>
    dplyr::mutate(.stype = .data[[sname]]) |>
    dplyr::filter(.stype %in% c("REG","Regular"))
} else {
  # If the column does not exist, keep all rows (older dumps sometimes miss it)
  pbp_hist
}

# ============================== WEATHER (Open-Meteo; hourly forecast) =============================
get_hourly_weather <- function(lat, lon, date_iso, hours = c(13)) {
  # Open-Meteo uses 'windspeed_10m' (no extra underscore) and returns local times like "2025-10-01T13:00"
  req <- httr2::request("https://api.open-meteo.com/v1/forecast") |>
    httr2::req_url_query(
      latitude  = lat,
      longitude = lon,
      hourly    = "temperature_2m,precipitation_probability,windspeed_10m",
      timezone  = "auto",
      start_date = date_iso,
      end_date   = date_iso
    )

  resp <- try(httr2::req_perform(req), silent = TRUE)
  if (inherits(resp, "try-error")) return(NULL)

  js <- try(httr2::resp_body_json(resp, simplifyVector = TRUE), silent = TRUE)
  if (inherits(js, "try-error") || is.null(js$hourly$time)) return(NULL)

  wx <- dplyr::as_tibble(js$hourly) |>
    dplyr::mutate(
      # Parse ISO times robustly
      datetime = suppressWarnings(lubridate::ymd_hms(paste0(time, ":00"))),
      hr = lubridate::hour(datetime)
    )

  # choose first requested hour that exists; else fallback to middle of the day
  pick <- wx |> dplyr::filter(hr %in% hours)
  if (nrow(pick) == 0) pick <- wx |> dplyr::slice(round(n()/2))

  pick |>
    dplyr::transmute(
      wind_mph    = as.numeric(windspeed_10m) * 0.621371,  # km/h -> mph
      temp_f      = as.numeric(temperature_2m) * 9/5 + 32,
      precip_prob = as.numeric(precipitation_probability) / 100
    )
}

# make sure stadium_coords is defined above this point and has columns: venue_key, lat, lon, dome

# Default weather conditions for neutral sites / missing stadium data (Kansas City as neutral baseline)
DEFAULT_STADIUM_CONDITIONS <- list(
  lat = 39.0489,    # Kansas City (Arrowhead Stadium)
  lon = -94.4839,
  dome = FALSE,
  wind_mph = 8,     # Typical outdoor conditions
  temp_f = 55,      # Mild temperature
  precip_prob = 0.1 # Low precipitation probability
)

# helper so we avoid inline braces inside mutate()
.wx_cache_dir <- file.path(path.expand("~"), ".cache", "nfl_sim_weather")
if (!dir.exists(.wx_cache_dir)) dir.create(.wx_cache_dir, recursive = TRUE)

safe_hourly <- function(lat, lon, date_iso, kickoff_hour_local = NA_integer_) {
  # Guard against missing or non-finite inputs which can bubble up from
  # incomplete stadium metadata or neutral-site games. `is.finite()` returns
  # `NA` for `NA_real_`, which caused an error inside the `if` statement when
  # this helper was called via `purrr::pmap()`.  Using `isTRUE(all(...))` keeps
  # the check scalar and safely handles `NA`s.
  # FIX: Use default coordinates for neutral sites instead of returning NULL
  if (!isTRUE(all(is.finite(c(lat, lon))))) {
    lat <- DEFAULT_STADIUM_CONDITIONS$lat
    lon <- DEFAULT_STADIUM_CONDITIONS$lon
    message(sprintf("Using default location (KC) for missing stadium data"))
  }
  if (is.na(date_iso) || !nzchar(date_iso)) return(NULL)
  key  <- digest::digest(list(round(lat,4), round(lon,4), date_iso))
  path <- file.path(.wx_cache_dir, paste0(key, ".rds"))
  if (file.exists(path)) return(readRDS(path))
  hours <- if (!is.na(kickoff_hour_local)) {
    candidates <- kickoff_hour_local + c(-1L, 0L, 1L)
    candidates <- candidates[candidates >= 0 & candidates <= 23]
    if (!length(candidates)) {
      candidates <- kickoff_hour_local
    }
    unique(as.integer(candidates))
  } else {
    c(13L, 16L, 20L)
  }
  out <- get_hourly_weather(lat, lon, date_iso, hours = hours)
  if (is.data.frame(out)) saveRDS(out, path)
  out
}


extract_first <- function(x, nm) {
  if (is.null(x) || !is.data.frame(x) || nrow(x) == 0 || !(nm %in% names(x))) return(NA_real_)
  as.numeric(x[[nm]][1])
}

weather_inputs <- week_slate %>%
  mutate(
    venue_key = stringr::str_to_lower(stringr::str_replace_all(venue, "[^a-zA-Z0-9]+", " ")),
    date_iso  = format(as.Date(game_date), "%Y-%m-%d"),
    kickoff_local_hour = kickoff_local_hour
  ) %>%
  dplyr::left_join(stadium_coords %>% dplyr::select(venue_key, lat, lon, dome), by = "venue_key") %>%
  mutate(
    lat = as.numeric(lat),
    lon = as.numeric(lon)
  )

weather_lookup <- purrr::pmap(
  list(weather_inputs$lat, weather_inputs$lon, weather_inputs$date_iso, weather_inputs$kickoff_local_hour),
  safe_hourly
)

weather_rows <- weather_inputs %>%
  mutate(.wx = weather_lookup) %>%
  transmute(
    game_id,
    wind_mph    = purrr::map_dbl(.wx, function(x) extract_first(x, "wind_mph")),
    temp_f      = purrr::map_dbl(.wx, function(x) extract_first(x, "temp_f")),
    precip_prob = purrr::map_dbl(.wx, function(x) extract_first(x, "precip_prob"))
  )


# Build per-game drive counts restricted to games BEFORE the slate_date
off_drives_game <- pbp_for_pace %>%
  filter(!is.na(.data[[posteam_col]]), !is.na(.data[[drive_col]])) %>%
  inner_join(sched_dates, by = "game_id") %>%
  filter(sched_date < slate_date) %>%
  distinct(game_id, team = .data[[posteam_col]], drive = .data[[drive_col]]) %>%
  count(team, game_id, name = "off_drives")

def_drives_game <- pbp_for_pace %>%
  filter(!is.na(.data[[defteam_col]]), !is.na(.data[[drive_col]])) %>%
  inner_join(sched_dates, by = "game_id") %>%
  filter(sched_date < slate_date) %>%
  distinct(game_id, team = .data[[defteam_col]], drive = .data[[drive_col]]) %>%
  count(team, game_id, name = "def_drives")

# Season-to-date drives per game (as-of)
off_drives <- off_drives_game |>
  dplyr::group_by(team) |>
  dplyr::summarise(off_drives_pg = mean(off_drives, na.rm = TRUE), .groups = "drop")

def_drives <- def_drives_game |>
  dplyr::group_by(team) |>
  dplyr::summarise(def_drives_pg = mean(def_drives, na.rm = TRUE), .groups = "drop")

# Points per game BEFORE slate_date (for PPD)
team_points_game <- sched |>
  dplyr::filter(game_completed, game_type %in% c("REG","POST"), as.Date(game_date) < slate_date) |>
  dplyr::transmute(
    game_id,
    home_team = .data[[home_team_col]],
    away_team = .data[[away_team_col]],
    home_pts  = .data[[home_pts_col]],
    away_pts  = .data[[away_pts_col]]
  )
tp_home <- team_points_game |>
  dplyr::transmute(game_id, team = home_team, points_for = home_pts, points_against = away_pts)
tp_away <- team_points_game |>
  dplyr::transmute(game_id, team = away_team, points_for = away_pts, points_against = home_pts)
team_points_game <- dplyr::bind_rows(tp_home, tp_away)

off_ppd_game <- off_drives_game |>
  dplyr::left_join(team_points_game |> dplyr::select(game_id, team, points_for), by = c("game_id","team")) |>
  dplyr::mutate(off_ppd = points_for / off_drives)

def_ppd_game <- def_drives_game |>
  dplyr::left_join(team_points_game |> dplyr::select(game_id, team, points_against), by = c("game_id","team")) |>
  dplyr::mutate(def_ppd = points_against / def_drives)

# REPLACE your off_ppd_tbl / def_ppd_tbl mean(...) blocks with this:
gid_date <- sched_dates

off_ppd_tbl <- off_ppd_game %>%
  left_join(gid_date, by = "game_id") %>%
  arrange(team, desc(sched_date)) %>%
  group_by(team) %>%
  summarise(
    off_ppd = {
      w <- recency_weights(n(), halflife = RECENCY_HALFLIFE)
      stats::weighted.mean(off_ppd, w, na.rm = TRUE)
    },
    .groups = "drop"
  )

def_ppd_tbl <- def_ppd_game %>%
  left_join(gid_date, by = "game_id") %>%
  arrange(team, desc(sched_date)) %>%
  group_by(team) %>%
  summarise(
    def_ppd = {
      w <- recency_weights(n(), halflife = RECENCY_HALFLIFE)
      stats::weighted.mean(def_ppd, w, na.rm = TRUE)
    },
    .groups = "drop"
  )

league_off_ppd <- mean(off_ppd_tbl$off_ppd, na.rm = TRUE)
league_def_ppd <- mean(def_ppd_tbl$def_ppd, na.rm = TRUE)

off_ppd_tbl <- off_ppd_tbl %>% mutate(off_ppd = coalesce(off_ppd, league_off_ppd))
def_ppd_tbl <- def_ppd_tbl %>% mutate(def_ppd = coalesce(def_ppd, league_def_ppd))

# --- Pressure rates (season-to-date; fallback to league) ---
# Ensure the three columns exist even if the PBP schema lacks them
for (nm in c("pressure","qb_hit","sack")) {
  if (!nm %in% names(pbp_hist)) pbp_hist[[nm]] <- NA_real_
}

# Single proxy flag = pressure OR qb_hit OR sack
pbp_hist <- pbp_hist %>%
  mutate(
    press_ind = as.numeric(
      tidyr::replace_na(pressure == 1, FALSE) |
        tidyr::replace_na(qb_hit   == 1, FALSE) |
        tidyr::replace_na(sack     == 1, FALSE)
    )
  )

# If you have a source, replace the mock with real rates in [0,1].
# For now, use simple proxies from PBP: QB hits + sacks over dropbacks.
passrush_rates <- pbp_hist %>%
  dplyr::filter(!is.na(!!sym(defteam_col)), qb_dropback == 1) %>%
  dplyr::group_by(team = .data[[defteam_col]]) %>%
  dplyr::summarise(press_rate_def = mean(press_ind, na.rm = TRUE), .groups = "drop")

passpro_rates <- pbp_hist %>%
  dplyr::filter(!is.na(!!sym(posteam_col)), qb_dropback == 1) %>%
  dplyr::group_by(team = .data[[posteam_col]]) %>%
  dplyr::summarise(press_allowed_off = mean(press_ind, na.rm = TRUE), .groups = "drop")

league_press_def  <- mean(passrush_rates$press_rate_def, na.rm = TRUE)
league_press_off  <- mean(passpro_rates$press_allowed_off, na.rm = TRUE)

passrush_rates <- passrush_rates %>% dplyr::mutate(press_rate_def = dplyr::coalesce(press_rate_def, league_press_def))
passpro_rates  <- passpro_rates  %>% dplyr::mutate(press_allowed_off = dplyr::coalesce(press_allowed_off, league_press_off))


# Anchors for the PPD blend + the blend function
L_OFF <- league_off_ppd
L_DEF <- league_def_ppd

ppd_blend <- function(off_ppd, opp_def_ppd, w_off = 0.65) {
  w_def <- 1 - w_off
  L_OFF * ((off_ppd / L_OFF)^w_off * (opp_def_ppd / L_DEF)^w_def)
}

# Pace table and league average drives
pace_tbl <- dplyr::full_join(off_drives, def_drives, by = "team")
league_drives_avg <- mean(pace_tbl$off_drives_pg, na.rm = TRUE)
if (!is.finite(league_drives_avg) || league_drives_avg <= 0) league_drives_avg <- 11.5

# ----- NB-GLMM team ratings as a mean prior (pre-slate) -----
df_hist <- sched %>%
  filter(game_type == "REG", game_completed, as.Date(game_date) < slate_date) %>%
  transmute(home = .data[[home_team_col]], away = .data[[away_team_col]],
            y_home = .data[[home_pts_col]], y_away = .data[[away_pts_col]])

stacked <- bind_rows(
  df_hist %>% transmute(team = home, opp = away, is_home = 1L, points = y_home),
  df_hist %>% transmute(team = away, opp = home, is_home = 0L, points = y_away)
)

# Fast, stable NB GLMM
.nb_cache_dir <- file.path(path.expand("~"), ".cache", "nfl_sim_nb")
if (!dir.exists(.nb_cache_dir)) dir.create(.nb_cache_dir, recursive = TRUE)
.nb_key  <- digest::digest(list("nb2", as.Date(slate_date), nrow(stacked)))
.nb_path <- file.path(.nb_cache_dir, paste0(.nb_key, ".rds"))

if (file.exists(.nb_path)) {
  fit_nb <- readRDS(.nb_path)
} else {
  fit_nb <- try(
    glmmTMB(points ~ is_home + (1|team) + (1|opp), family = nbinom2, data = stacked),
    silent = TRUE
  )
  if (!inherits(fit_nb, "try-error")) saveRDS(fit_nb, .nb_path)
}



# Predict means for this slate (home and away roles)
glmm_preds <- week_slate %>%
  transmute(home_team, away_team,
            mu_home_glmm = if (inherits(fit_nb,"try-error")) NA_real_ else
              as.numeric(predict(fit_nb, newdata = tibble(team=home_team, opp=away_team, is_home=1L), type="response")),
            mu_away_glmm = if (inherits(fit_nb,"try-error")) NA_real_ else
              as.numeric(predict(fit_nb, newdata = tibble(team=away_team, opp=home_team, is_home=0L), type="response"))
  )

# --- Turnover & ST small priors (season-to-date, shrunk) ---------------------
to_game <- sched %>%
  filter(game_completed, game_type == "REG") %>%
  transmute(
    game_id,
    home = .data[[home_team_col]], away = .data[[away_team_col]],
    h_to = if ("turnovers_home" %in% names(sched)) turnovers_home else NA_real_,
    a_to = if ("turnovers_away" %in% names(sched)) turnovers_away else NA_real_
  )

to_team <- bind_rows(
  to_game %>% transmute(team = home, to = h_to),
  to_game %>% transmute(team = away, to = a_to)
) %>%
  group_by(team) %>%
  summarise(to_pg = mean(to, na.rm = TRUE), n = n(), .groups="drop")

league_to_pg <- mean(to_team$to_pg, na.rm = TRUE)
to_team <- to_team %>%
  mutate(w = n / (n + 8), to_pg = coalesce(to_pg, league_to_pg),
         to_adj_pts = -0.40 * (to_pg - league_to_pg) * w)  # ~0.4 pts per TO above lg avg (shrunk)

st_game <- NULL # (optional) if you have ST EPA/DVOA, build similar small adjustment, +/-1.0 cap


# ------------------------ GAME SETUP (with Pace + PPD) ------------------------
games_ready <- week_slate |>
  # recent form, HFA, QB/rest
  dplyr::left_join(recent_form |> dplyr::rename_with(~paste0("home_", .x), -team),
                   by = c("home_team" = "team")) |>
  dplyr::left_join(recent_form |> dplyr::rename_with(~paste0("away_", .x), -team),
                   by = c("away_team" = "team")) |>
  # Pace joins (season avg drives per game)
  dplyr::left_join(pace_tbl |> dplyr::rename(home_off_drives_pg = off_drives_pg,
                                             home_def_drives_pg = def_drives_pg),
                   by = c("home_team" = "team")) |>
  dplyr::left_join(pace_tbl |> dplyr::rename(away_off_drives_pg = off_drives_pg,
                                             away_def_drives_pg = def_drives_pg),
                   by = c("away_team" = "team")) |>
  # NEW: join team-level PPDs (from game-level PPD averages)
  dplyr::left_join(off_ppd_tbl |> dplyr::rename(home_off_ppd = off_ppd),
                   by = c("home_team" = "team")) |>
  dplyr::left_join(def_ppd_tbl |> dplyr::rename(home_def_ppd = def_ppd),
                   by = c("home_team" = "team")) |>
  dplyr::left_join(off_ppd_tbl |> dplyr::rename(away_off_ppd = off_ppd),
                   by = c("away_team" = "team")) |>
  dplyr::left_join(def_ppd_tbl |> dplyr::rename(away_def_ppd = def_ppd),
                   by = c("away_team" = "team")) |>
  dplyr::left_join(glmm_preds, by = c("home_team","away_team")) |>
  dplyr::mutate(
    # Expected drives this game = average of team offense and opponent defense
    exp_drives_home = (home_off_drives_pg + away_def_drives_pg)/2,
    exp_drives_away = (away_off_drives_pg + home_def_drives_pg)/2,

    exp_ppd_home = ppd_blend(home_off_ppd, away_def_ppd, w_off = 0.65),
    exp_ppd_away = ppd_blend(away_off_ppd, home_def_ppd, w_off = 0.65),

    home_skill_penalty = coalesce(home_skill_avail_pen, 0) * SKILL_AVAIL_POINT_PER_FLAG,
    home_trench_penalty = coalesce(home_trench_avail_pen, 0) * TRENCH_AVAIL_POINT_PER_FLAG,
    home_secondary_penalty = coalesce(home_secondary_avail_pen, 0) * SECONDARY_AVAIL_POINT_PER_FLAG,
    home_front7_penalty = coalesce(home_front7_avail_pen, 0) * FRONT7_AVAIL_POINT_PER_FLAG,
    away_skill_penalty = coalesce(away_skill_avail_pen, 0) * SKILL_AVAIL_POINT_PER_FLAG,
    away_trench_penalty = coalesce(away_trench_avail_pen, 0) * TRENCH_AVAIL_POINT_PER_FLAG,
    away_secondary_penalty = coalesce(away_secondary_avail_pen, 0) * SECONDARY_AVAIL_POINT_PER_FLAG,
    away_front7_penalty = coalesce(away_front7_avail_pen, 0) * FRONT7_AVAIL_POINT_PER_FLAG,

    # FIX: Use inj_off/def_pts directly (already position-weighted) to avoid double-counting
    home_injury_off_total = home_inj_off_pts,  # Already includes all offensive position penalties
    away_injury_off_total = away_inj_off_pts,
    home_injury_def_total = home_inj_def_pts,  # Already includes all defensive position penalties
    away_injury_def_total = away_inj_def_pts
  ) %>%
  # <<< add this mutate right after the previous one >>>
  dplyr::mutate(
    exp_drives_home = dplyr::coalesce(exp_drives_home, league_drives_avg),
    exp_drives_away = dplyr::coalesce(exp_drives_away, league_drives_avg),
    exp_ppd_home    = dplyr::coalesce(exp_ppd_home,  league_off_ppd),
    exp_ppd_away    = dplyr::coalesce(exp_ppd_away,  league_off_ppd),
    # Base expected points = expected drives * expected PPD (now safe)
    mu_home_model   = exp_drives_home * exp_ppd_home,
    mu_away_model   = exp_drives_away * exp_ppd_away
  ) %>%
  # continue with your GLMM blend using these new *_model columns
  dplyr::mutate(
    # GLMM blend (convex; fallback to model values if GLMM is NA)
    glmm_w = dplyr::case_when(
      week <= 2 ~ 0.70,
      week <= 4 ~ 0.55,
      week <= 8 ~ 0.40,
      TRUE      ~ 0.35
    ),
    mu_home_base = (1 - glmm_w) * mu_home_model + glmm_w * dplyr::coalesce(mu_home_glmm, mu_home_model),
    mu_away_base = (1 - glmm_w) * mu_away_model + glmm_w * dplyr::coalesce(mu_away_glmm, mu_away_model),

    # Variability from both sides (keep your existing blend)
    sd_home_raw = sqrt((home_off_sd_raw^2 + away_def_sd_raw^2)/2),
    sd_away_raw = sqrt((away_off_sd_raw^2 + home_def_sd_raw^2)/2),

    # Home-field margin shift (team-vs-team HFA delta), split
    margin_shift = (home_hfa - away_hfa)/2,

    # QB & rest adjustments (additive to means)
    mu_home_qb_rest = mu_home_base + home_off_points_adj + home_rest_points + home_injury_off_total,
    mu_away_qb_rest = mu_away_base + away_off_points_adj + away_rest_points + away_injury_off_total,

    # Apply HFA split & non-negativity
    mu_home = pmax(mu_home_qb_rest, 0),
    mu_away = pmax(mu_away_qb_rest, 0),
    mu_home = pmax(mu_home + away_injury_def_total, 0),  # away defense ding -> home scores more
    mu_away = pmax(mu_away + home_injury_def_total, 0),


    # SD guardrails and QB-related spread tweak
    sd_home = pmax(sd_home_raw + home_sd_points_adj, 5.5),
    sd_away = pmax(sd_away_raw + away_sd_points_adj, 5.5)
  )

games_ready <- games_ready %>%
  left_join(travel_tbl,  by = "game_id") %>%
  left_join(weather_rows, by = "game_id") %>%
  mutate(
    mu_home = pmax(mu_home + coalesce(travel_mu_adj_home, 0), 0),
    mu_away = pmax(mu_away + coalesce(travel_mu_adj_away, 0), 0)
  )

# Apply division/conference game adjustments to totals (split evenly between teams)
games_ready <- games_ready %>%
  mutate(
    div_conf_adj = ifelse(coalesce(division_game, FALSE), DIVISION_GAME_ADJUST / 2,
                          ifelse(coalesce(conference_game, FALSE), CONFERENCE_GAME_ADJUST / 2, 0)),
    mu_home = pmax(mu_home + div_conf_adj, 0),
    mu_away = pmax(mu_away + div_conf_adj, 0)
  )

# --- Manual HFA (team-specific; use your 5-year shrinked/capped estimate) ---
# If you already built a per-team HFA table elsewhere, join it here as `home_hfa_pts`.
# For now, reuse league_hfa as a fallback and add DEN altitude flavor already in hfa_tbl.
games_ready <- games_ready %>%
  mutate(
    HFA_pts = coalesce(home_hfa, league_hfa),  # if you have team-specific, prefer that
    HFA_pts = pmin(pmax(HFA_pts, -6), 6)       # cap at +/-6
  ) %>%
  mutate(
    mu_home = pmax(mu_home + HFA_pts, 0)
  )

# NA-proofing and sensible bounds (light touch)
games_ready <- games_ready |>
  dplyr::mutate(
    # replace any missing components
    exp_drives_home = dplyr::coalesce(exp_drives_home, league_drives_avg),
    exp_drives_away = dplyr::coalesce(exp_drives_away, league_drives_avg),
    exp_ppd_home    = dplyr::coalesce(exp_ppd_home, league_off_ppd),
    exp_ppd_away    = dplyr::coalesce(exp_ppd_away, league_off_ppd),
    mu_home         = pmax(mu_home, 0),
    mu_away         = pmax(mu_away, 0)
  )

games_ready <- games_ready %>%
  dplyr::left_join(passpro_rates,  by = c("home_team" = "team")) %>%
  dplyr::rename(home_press_allowed = press_allowed_off) %>%
  dplyr::left_join(passrush_rates, by = c("away_team" = "team")) %>%
  dplyr::rename(away_press_rate = press_rate_def) %>%
  dplyr::left_join(passpro_rates,  by = c("away_team" = "team")) %>%
  dplyr::rename(away_press_allowed = press_allowed_off) %>%
  dplyr::left_join(passrush_rates, by = c("home_team" = "team")) %>%
  dplyr::rename(home_press_rate = press_rate_def) %>%
  dplyr::mutate(
    home_press_mismatch = (away_press_rate - home_press_allowed),
    away_press_mismatch = (home_press_rate - away_press_allowed),
    # translate into points: ~0.6 pts per +10% pressure mismatch
    mu_home = pmax(mu_home + (-0.06) * 10 * home_press_mismatch, 0),
    mu_away = pmax(mu_away + (-0.06) * 10 * away_press_mismatch, 0)
  )


games_ready <- games_ready %>%
  dplyr::left_join(explosive_off, by = c("home_team" = "team")) %>% dplyr::rename(home_expl = expl_rate_off) %>%
  dplyr::left_join(explosive_def, by = c("away_team" = "team")) %>% dplyr::rename(away_expl_def = expl_rate_def) %>%
  dplyr::left_join(explosive_off, by = c("away_team" = "team")) %>% dplyr::rename(away_expl = expl_rate_off) %>%
  dplyr::left_join(explosive_def, by = c("home_team" = "team")) %>% dplyr::rename(home_expl_def = expl_rate_def) %>%
  dplyr::mutate(
    expl_edge_home = (home_expl - away_expl_def),
    expl_edge_away = (away_expl - home_expl_def),
    # convert to points: ~0.8 pts per +5% explosive edge
    mu_home = pmax(mu_home + 0.16 * 5 * expl_edge_home, 0),
    mu_away = pmax(mu_away + 0.16 * 5 * expl_edge_away, 0)
  )


rz_off <- pbp_hist %>%
  dplyr::filter(!is.na(yardline_100)) %>%
  dplyr::mutate(in_rz = yardline_100 <= 20, td = touchdown == 1) %>%
  dplyr::group_by(team = .data[[posteam_col]]) %>%
  dplyr::summarise(rz_td_off = mean(td[in_rz], na.rm = TRUE), .groups = "drop")

rz_def <- pbp_hist %>%
  dplyr::filter(!is.na(yardline_100)) %>%
  dplyr::mutate(in_rz = yardline_100 <= 20, td = touchdown == 1) %>%
  dplyr::group_by(team = .data[[defteam_col]]) %>%
  dplyr::summarise(rz_td_def = mean(td[in_rz], na.rm = TRUE), .groups = "drop")

# Enhanced Red Zone metrics: trip rate and goal-to-go efficiency
rz_enhanced <- pbp_hist %>%
  dplyr::filter(!is.na(yardline_100), !is.na(drive)) %>%
  dplyr::group_by(game_id, drive, team = .data[[posteam_col]]) %>%
  dplyr::summarise(
    reached_rz = any(yardline_100 <= 20, na.rm = TRUE),
    reached_goalline = any(yardline_100 <= 5, na.rm = TRUE),
    scored_td = any(touchdown == 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(
    rz_trip_rate = mean(reached_rz, na.rm = TRUE),          # % of drives reaching RZ
    goal_to_go_td_rate = sum(scored_td & reached_goalline) / pmax(sum(reached_goalline), 1),
    .groups = "drop"
  )

# Pass rate for weather interaction effects
team_pass_rate <- pbp_hist %>%
  dplyr::filter(!is.na(play_type), play_type %in% c("pass", "run"), !is.na(down)) %>%
  dplyr::group_by(team = .data[[posteam_col]]) %>%
  dplyr::summarise(
    pass_rate = mean(play_type == "pass", na.rm = TRUE),
    .groups = "drop"
  )

league_avg_pass_rate <- mean(team_pass_rate$pass_rate, na.rm = TRUE)

# Home/Away Split Performance Metrics (CRITICAL MISSING DATA)
# Teams perform differently at home vs away - this captures EPA and success rate splits
home_away_splits <- pbp_hist %>%
  dplyr::filter(!is.na(.data[[posteam_col]]), !is.na(epa), !is.na(success)) %>%
  dplyr::mutate(
    is_home = (.data[[posteam_col]] == .data[[home_team_col]])
  ) %>%
  dplyr::group_by(team = .data[[posteam_col]], is_home) %>%
  dplyr::summarise(
    epa_mean = mean(epa, na.rm = TRUE),
    success_rate = mean(success, na.rm = TRUE),
    plays = n(),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = is_home,
    values_from = c(epa_mean, success_rate, plays),
    names_prefix = "loc_"
  ) %>%
  dplyr::mutate(
    # Home advantage in EPA/play
    epa_home_adv = coalesce(epa_mean_loc_TRUE, 0) - coalesce(epa_mean_loc_FALSE, 0),
    # Success rate advantage at home
    sr_home_adv = coalesce(success_rate_loc_TRUE, 0.45) - coalesce(success_rate_loc_FALSE, 0.45)
  )

# Third down conversion rates (highly predictive of scoring)
third_down_metrics <- pbp_hist %>%
  dplyr::filter(!is.na(down), down == 3, !is.na(yards_gained), !is.na(ydstogo)) %>%
  dplyr::mutate(
    converted = yards_gained >= ydstogo,
    situation = dplyr::case_when(
      ydstogo <= 3 ~ "short",
      ydstogo <= 7 ~ "medium",
      TRUE ~ "long"
    )
  ) %>%
  dplyr::group_by(team = .data[[posteam_col]]) %>%
  dplyr::summarise(
    third_down_conv_rate = mean(converted, na.rm = TRUE),
    third_down_short_rate = mean(converted[situation == "short"], na.rm = TRUE),
    third_down_long_rate = mean(converted[situation == "long"], na.rm = TRUE),
    .groups = "drop"
  )

# Defensive third down (allow conversion rate)
third_down_def <- pbp_hist %>%
  dplyr::filter(!is.na(down), down == 3, !is.na(yards_gained), !is.na(ydstogo)) %>%
  dplyr::mutate(converted = yards_gained >= ydstogo) %>%
  dplyr::group_by(team = .data[[defteam_col]]) %>%
  dplyr::summarise(
    third_down_def_rate = mean(converted, na.rm = TRUE),
    .groups = "drop"
  )

# Turnover tendencies (sustainable skill component)
turnover_metrics <- pbp_hist %>%
  dplyr::filter(!is.na(play_type), play_type %in% c("pass", "run")) %>%
  dplyr::group_by(team = .data[[posteam_col]]) %>%
  dplyr::summarise(
    interception_rate = mean(interception == 1, na.rm = TRUE),
    fumble_rate = mean(fumble == 1, na.rm = TRUE),
    turnover_rate = mean((interception == 1) | (fumble == 1), na.rm = TRUE),
    .groups = "drop"
  )

# Defensive turnovers (forced TOs)
turnover_def <- pbp_hist %>%
  dplyr::filter(!is.na(play_type), play_type %in% c("pass", "run")) %>%
  dplyr::group_by(team = .data[[defteam_col]]) %>%
  dplyr::summarise(
    forced_turnover_rate = mean((interception == 1) | (fumble == 1), na.rm = TRUE),
    .groups = "drop"
  )

# Penalty rates by team (drive sustainability impact)
penalty_metrics <- pbp_hist %>%
  dplyr::filter(!is.na(penalty)) %>%
  dplyr::group_by(team = .data[[posteam_col]]) %>%
  dplyr::summarise(
    penalty_rate = mean(penalty == 1, na.rm = TRUE),
    penalty_yds_per_play = mean(penalty_yards[penalty == 1], na.rm = TRUE),
    .groups = "drop"
  )

# Defensive penalties (helping opponent)
penalty_def <- pbp_hist %>%
  dplyr::filter(!is.na(penalty)) %>%
  dplyr::group_by(team = .data[[defteam_col]]) %>%
  dplyr::summarise(
    def_penalty_rate = mean(penalty == 1, na.rm = TRUE),
    .groups = "drop"
  )

# Two-minute drill efficiency (critical situations)
two_min_metrics <- pbp_hist %>%
  dplyr::filter(!is.na(half_seconds_remaining), half_seconds_remaining <= 120, half_seconds_remaining > 0) %>%
  dplyr::group_by(team = .data[[posteam_col]]) %>%
  dplyr::summarise(
    two_min_epa = mean(epa, na.rm = TRUE),
    two_min_success_rate = mean(success, na.rm = TRUE),
    .groups = "drop"
  )

# Scripted plays efficiency (first 15 plays of game - prepared plays)
scripted_plays <- pbp_hist %>%
  dplyr::filter(!is.na(drive), !is.na(play_id)) %>%
  dplyr::group_by(game_id, team = .data[[posteam_col]]) %>%
  dplyr::arrange(play_id) %>%
  dplyr::mutate(play_num = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::filter(play_num <= 15) %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(
    scripted_epa = mean(epa, na.rm = TRUE),
    scripted_success_rate = mean(success, na.rm = TRUE),
    .groups = "drop"
  )

# Momentum indicators (3-game rolling averages for hot/cold streaks)
momentum_metrics <- sched %>%
  dplyr::filter(game_completed, game_type == "REG") %>%
  dplyr::arrange(season, week) %>%
  dplyr::transmute(
    season, week, game_date,
    home_team, away_team,
    home_pts = home_score, away_pts = away_score,
    home_margin = home_score - away_score
  ) %>%
  dplyr::select(-any_of("location")) %>%  # Remove any existing location column before pivot
  tidyr::pivot_longer(
    cols = c(home_team, away_team),
    names_to = "location",
    values_to = "team"
  ) %>%
  dplyr::mutate(
    points = ifelse(location == "home_team", home_pts, away_pts),
    margin = ifelse(location == "home_team", home_margin, -home_margin),
    won = margin > 0
  ) %>%
  dplyr::select(-location) %>%  # Remove location column to prevent duplicates in joins
  dplyr::group_by(team) %>%
  dplyr::arrange(game_date) %>%
  dplyr::mutate(
    # 3-game rolling averages (R 4.5.1 compatible - use scalar defaults)
    # For first 1-2 games, use current value when lag is NA
    lag1_points = dplyr::lag(points, 1, default = NA_real_),
    lag2_points = dplyr::lag(points, 2, default = NA_real_),
    lag1_margin = dplyr::lag(margin, 1, default = NA_real_),
    lag2_margin = dplyr::lag(margin, 2, default = NA_real_),
    lag1_won = dplyr::lag(as.numeric(won), 1, default = NA_real_),
    lag2_won = dplyr::lag(as.numeric(won), 2, default = NA_real_),

    momentum_ppg = (points + coalesce(lag1_points, points) + coalesce(lag2_points, points)) / 3,
    momentum_margin = (margin + coalesce(lag1_margin, margin) + coalesce(lag2_margin, margin)) / 3,
    momentum_win_rate = (as.numeric(won) + coalesce(lag1_won, 0) + coalesce(lag2_won, 0)) / 3
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(
    recent_momentum = mean(momentum_margin, na.rm = TRUE),
    hot_streak = mean(momentum_win_rate >= 0.67, na.rm = TRUE),  # 2+ wins in last 3
    .groups = "drop"
  )

# Division game performance (familiarity effects)
division_performance <- sched %>%
  dplyr::filter(game_completed, game_type == "REG", division_game == TRUE) %>%
  dplyr::transmute(
    home_team, away_team,
    home_margin = home_score - away_score
  ) %>%
  dplyr::select(-any_of("location")) %>%  # Remove any existing location column before pivot
  tidyr::pivot_longer(
    cols = c(home_team, away_team),
    names_to = "location",
    values_to = "team"
  ) %>%
  dplyr::mutate(
    margin = ifelse(location == "home_team", home_margin, -home_margin)
  ) %>%
  dplyr::select(-location) %>%  # Remove location column to prevent duplicates in joins
  dplyr::group_by(team) %>%
  dplyr::summarise(
    div_game_margin_avg = mean(margin, na.rm = TRUE),
    div_game_win_rate = mean(margin > 0, na.rm = TRUE),
    .groups = "drop"
  )

# Post-bye performance (coaching preparation effect)
post_bye_performance <- sched %>%
  dplyr::filter(game_completed, game_type == "REG") %>%
  dplyr::arrange(season, week) %>%
  dplyr::group_by(season, home_team) %>%
  dplyr::mutate(
    home_had_bye = week - dplyr::lag(week, default = 0) > 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(season, away_team) %>%
  dplyr::mutate(
    away_had_bye = week - dplyr::lag(week, default = 0) > 1
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(home_had_bye | away_had_bye) %>%
  dplyr::select(-any_of("location")) %>%  # Remove any existing location column before pivot
  tidyr::pivot_longer(
    cols = c(home_team, away_team),
    names_to = "location",
    values_to = "team"
  ) %>%
  dplyr::mutate(
    had_bye = ifelse(location == "home_team", home_had_bye, away_had_bye),
    margin = ifelse(location == "home_team",
                    home_score - away_score,
                    away_score - home_score)
  ) %>%
  dplyr::select(-location) %>%  # Remove location column to prevent duplicates in joins
  dplyr::filter(had_bye) %>%
  dplyr::group_by(team) %>%
  dplyr::summarise(
    post_bye_margin = mean(margin, na.rm = TRUE),
    post_bye_win_rate = mean(margin > 0, na.rm = TRUE),
    .groups = "drop"
  )

# Special teams field position metrics (1-2 pt impact per user)
special_teams_fp <- pbp_hist %>%
  dplyr::filter(!is.na(play_type)) %>%
  dplyr::group_by(team = .data[[posteam_col]]) %>%
  dplyr::summarise(
    # Punting: net field position gained
    avg_punt_yds = mean(kick_distance[play_type == "punt"], na.rm = TRUE),
    # Kick return: average starting field position
    avg_ko_return_yds = mean(return_yards[play_type == "kickoff"], na.rm = TRUE),
    # Average drive start (offensive field position)
    avg_drive_start = mean(yardline_100[!is.na(drive) & drive_play_count == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    # Field position advantage: better starting position + better punting
    fp_advantage = (100 - avg_drive_start) + avg_punt_yds/100
  )

games_ready <- games_ready %>%
  dplyr::left_join(rz_off, by = c("home_team" = "team")) %>% dplyr::rename(home_rz_off = rz_td_off) %>%
  dplyr::left_join(rz_def, by = c("away_team" = "team")) %>% dplyr::rename(away_rz_def = rz_td_def) %>%
  dplyr::left_join(rz_off, by = c("away_team" = "team")) %>% dplyr::rename(away_rz_off = rz_td_off) %>%
  dplyr::left_join(rz_def, by = c("home_team" = "team")) %>% dplyr::rename(home_rz_def = rz_td_def) %>%
  dplyr::left_join(rz_enhanced, by = c("home_team" = "team")) %>%
    dplyr::rename(home_rz_trip_rate = rz_trip_rate, home_goal_to_go_td = goal_to_go_td_rate) %>%
  dplyr::left_join(rz_enhanced, by = c("away_team" = "team")) %>%
    dplyr::rename(away_rz_trip_rate = rz_trip_rate, away_goal_to_go_td = goal_to_go_td_rate) %>%
  dplyr::left_join(team_pass_rate, by = c("home_team" = "team")) %>% dplyr::rename(home_pass_rate = pass_rate) %>%
  dplyr::left_join(team_pass_rate, by = c("away_team" = "team")) %>% dplyr::rename(away_pass_rate = pass_rate) %>%
  dplyr::left_join(special_teams_fp, by = c("home_team" = "team")) %>%
    dplyr::rename(home_fp_adv = fp_advantage, home_drive_start = avg_drive_start) %>%
  dplyr::left_join(special_teams_fp, by = c("away_team" = "team")) %>%
    dplyr::rename(away_fp_adv = fp_advantage, away_drive_start = avg_drive_start) %>%
  dplyr::left_join(home_away_splits, by = c("home_team" = "team")) %>%
    dplyr::rename(home_epa_home_adv = epa_home_adv, home_sr_adv = sr_home_adv) %>%
  dplyr::left_join(home_away_splits, by = c("away_team" = "team")) %>%
    dplyr::rename(away_epa_home_adv = epa_home_adv, away_sr_adv = sr_home_adv) %>%
  # Add all new situational metrics
  dplyr::left_join(third_down_metrics, by = c("home_team" = "team")) %>%
    dplyr::rename(home_3rd_conv = third_down_conv_rate) %>%
  dplyr::left_join(third_down_def, by = c("away_team" = "team")) %>%
    dplyr::rename(away_3rd_def = third_down_def_rate) %>%
  dplyr::left_join(third_down_metrics, by = c("away_team" = "team")) %>%
    dplyr::rename(away_3rd_conv = third_down_conv_rate) %>%
  dplyr::left_join(third_down_def, by = c("home_team" = "team")) %>%
    dplyr::rename(home_3rd_def = third_down_def_rate) %>%
  dplyr::left_join(turnover_metrics, by = c("home_team" = "team")) %>%
    dplyr::rename(home_to_rate = turnover_rate) %>%
  dplyr::left_join(turnover_def, by = c("away_team" = "team")) %>%
    dplyr::rename(away_forced_to = forced_turnover_rate) %>%
  dplyr::left_join(turnover_metrics, by = c("away_team" = "team")) %>%
    dplyr::rename(away_to_rate = turnover_rate) %>%
  dplyr::left_join(turnover_def, by = c("home_team" = "team")) %>%
    dplyr::rename(home_forced_to = forced_turnover_rate) %>%
  dplyr::left_join(penalty_metrics, by = c("home_team" = "team")) %>%
    dplyr::rename(home_pen_rate = penalty_rate) %>%
  dplyr::left_join(penalty_def, by = c("away_team" = "team")) %>%
    dplyr::rename(away_def_pen = def_penalty_rate) %>%
  dplyr::left_join(penalty_metrics, by = c("away_team" = "team")) %>%
    dplyr::rename(away_pen_rate = penalty_rate) %>%
  dplyr::left_join(penalty_def, by = c("home_team" = "team")) %>%
    dplyr::rename(home_def_pen = def_penalty_rate) %>%
  dplyr::left_join(two_min_metrics, by = c("home_team" = "team")) %>%
    dplyr::rename(home_2min_epa = two_min_epa) %>%
  dplyr::left_join(two_min_metrics, by = c("away_team" = "team")) %>%
    dplyr::rename(away_2min_epa = two_min_epa) %>%
  dplyr::left_join(scripted_plays, by = c("home_team" = "team")) %>%
    dplyr::rename(home_script_epa = scripted_epa) %>%
  dplyr::left_join(scripted_plays, by = c("away_team" = "team")) %>%
    dplyr::rename(away_script_epa = scripted_epa) %>%
  dplyr::left_join(momentum_metrics, by = c("home_team" = "team")) %>%
    dplyr::rename(home_momentum = recent_momentum, home_hot = hot_streak) %>%
  dplyr::left_join(momentum_metrics, by = c("away_team" = "team")) %>%
    dplyr::rename(away_momentum = recent_momentum, away_hot = hot_streak) %>%
  dplyr::left_join(division_performance, by = c("home_team" = "team")) %>%
    dplyr::rename(home_div_margin = div_game_margin_avg) %>%
  dplyr::left_join(division_performance, by = c("away_team" = "team")) %>%
    dplyr::rename(away_div_margin = div_game_margin_avg) %>%
  dplyr::left_join(post_bye_performance, by = c("home_team" = "team")) %>%
    dplyr::rename(home_post_bye_margin = post_bye_margin) %>%
  dplyr::left_join(post_bye_performance, by = c("away_team" = "team")) %>%
    dplyr::rename(away_post_bye_margin = post_bye_margin) %>%
  dplyr::mutate(
    # Red zone impact
    rz_edge_home = (home_rz_off - away_rz_def),
    rz_edge_away = (away_rz_off - home_rz_def),
    rz_impact_home = 2.0 * rz_edge_home * coalesce(home_rz_trip_rate, 0.25),
    rz_impact_away = 2.0 * rz_edge_away * coalesce(away_rz_trip_rate, 0.25),

    # Special teams impact
    st_impact_home = 1.5 * (coalesce(home_fp_adv, 0) - coalesce(away_fp_adv, 0)),
    st_impact_away = 1.5 * (coalesce(away_fp_adv, 0) - coalesce(home_fp_adv, 0)),

    # Home/Away location advantage
    home_location_boost = coalesce(home_epa_home_adv, 0) * 60 * 9,
    away_location_penalty = coalesce(away_epa_home_adv, 0) * 60 * 9 * (-1),

    # Third down efficiency impact (~1 pt per 10% advantage)
    third_down_edge_home = 10 * (coalesce(home_3rd_conv, 0.40) - coalesce(away_3rd_def, 0.40)),
    third_down_edge_away = 10 * (coalesce(away_3rd_conv, 0.40) - coalesce(home_3rd_def, 0.40)),

    # Turnover impact (p = 0.051, marginally significant - reduced weight to avoid overfitting)
    # Reduced from 40 to 20 (50% reduction) due to weak statistical significance
    to_edge_home = 20 * (coalesce(away_to_rate, 0.03) - coalesce(home_to_rate, 0.03) +
                         coalesce(home_forced_to, 0.03) - coalesce(away_forced_to, 0.03)),
    to_edge_away = 20 * (coalesce(home_to_rate, 0.03) - coalesce(away_to_rate, 0.03) +
                         coalesce(away_forced_to, 0.03) - coalesce(home_forced_to, 0.03)),

    # Penalty impact (conservative - penalties kill ~0.3 drives per 1% rate increase)
    penalty_edge_home = -3 * (coalesce(home_pen_rate, 0.05) - 0.05) +
                         3 * (coalesce(away_def_pen, 0.05) - 0.05),
    penalty_edge_away = -3 * (coalesce(away_pen_rate, 0.05) - 0.05) +
                         3 * (coalesce(home_def_pen, 0.05) - 0.05),

    # Situational efficiency (two-minute drill worth ~0.5 pts, scripted plays ~0.3 pts)
    situational_home = 5 * coalesce(home_2min_epa, 0) + 3 * coalesce(home_script_epa, 0),
    situational_away = 5 * coalesce(away_2min_epa, 0) + 3 * coalesce(away_script_epa, 0),

    # Momentum (hot teams get ~0.5 pt boost, use margin differential)
    momentum_home = 0.15 * coalesce(home_momentum, 0) + 0.5 * coalesce(home_hot, 0),
    momentum_away = 0.15 * coalesce(away_momentum, 0) + 0.5 * coalesce(away_hot, 0),

    # Division game adjustment (if division_game, use historical performance)
    div_adjustment_home = ifelse(division_game, 0.25 * coalesce(home_div_margin, 0), 0),
    div_adjustment_away = ifelse(division_game, 0.25 * coalesce(away_div_margin, 0), 0),

    # Post-bye adjustment (if team had bye last week)
    # This will be applied in the rest calculation section that already exists

    # Apply all adjustments with Bayesian shrinkage (0.7 weight to avoid overfitting)
    # Increased from 0.6 to 0.7 to reduce overfitting risk with many features
    mu_home = pmax(mu_home + 0.7 * (rz_impact_home + st_impact_home + home_location_boost +
                                     third_down_edge_home + to_edge_home + penalty_edge_home +
                                     situational_home + momentum_home + div_adjustment_home), 0),
    mu_away = pmax(mu_away + 0.7 * (rz_impact_away + st_impact_away + away_location_penalty +
                                     third_down_edge_away + to_edge_away + penalty_edge_away +
                                     situational_away + momentum_away + div_adjustment_away), 0)
  )



# --- Helper functions for negative binomial calculations ---------------------
# Convert (mu, sd) -> NB size k. If v<=mu, fall back to Poisson (k = Inf)
nb_size_from_musd <- function(mu, sd) {
  v <- sd^2
  if (!is.finite(mu) || !is.finite(sd) || mu <= 0 || v <= mu) return(Inf)
  k <- mu^2 / (v - mu)
  # FIX: NFL scores typically have k ∈ [5, 15], not k >= 2 (too much overdispersion)
  k <- pmax(k, NB_SIZE_MIN)  # Realistic floor (was 2, now 5)
  k <- pmin(k, NB_SIZE_MAX)  # Tighter ceiling (was 1e4, now 50)
  k
}

# --- Total-sensitive SD guardrail (light touch) -------------------------------
sd_total_curve <- function(total_mu){
  # ~12 at total 38 to ~16 at total 55; clamp to [10,18]
  val <- 12 + 0.195 * (total_mu - 38)
  pmin(pmax(val, 9.5), 17)
}

games_ready <- games_ready |>
  dplyr::mutate(
    total_mu = mu_home + mu_away,
    sd_goal  = sd_total_curve(total_mu),
    sd_home  = 0.6 * sd_home + 0.4 * (sd_goal / sqrt(2)),
    sd_away  = 0.6 * sd_away + 0.4 * (sd_goal / sqrt(2)),
    sd_home  = pmax(sd_home, 5.0),
    sd_away  = pmax(sd_away, 5.0)
  ) |>
  # Calculate negative binomial size parameters for prediction intervals
  dplyr::mutate(
    k_home = purrr::map2_dbl(mu_home, sd_home, nb_size_from_musd),
    k_away = purrr::map2_dbl(mu_away, sd_away, nb_size_from_musd)
  )

# Game-specific score correlation (rho): higher totals -> more positive correlation; larger mismatch -> less correlation
rho_from_game <- function(total_mu, spread_abs, rho_global = RHO_SCORE) {
  base <- 0.10 + 0.20 * plogis((total_mu - 44)/4)   # totals around 44 are neutral
  anti <- -0.15 * plogis((spread_abs - 10)/3)       # big spreads dampen correlation

  # Game script effect: larger spreads = more script-dependent (winning team runs clock)
  # This strengthens negative correlation in blowouts
  script_factor <- tanh(spread_abs / 10)            # 0 for close games, ~1 for blowouts
  rho  <- base - (anti * (1 + script_factor))       # Amplify anti-correlation in blowouts

  # shrink toward global estimate to avoid overfit
  rho  <- 0.5 * rho + 0.5 * rho_global
  pmin(pmax(rho, -0.20), 0.60)
}

games_ready <- games_ready %>%
  mutate(
    spread_est = abs(mu_home - mu_away),
    rho_game   = rho_from_game(total_mu, spread_est)
  )


games_ready <- games_ready %>%
  dplyr::left_join(to_team %>% dplyr::select(team, to_adj_pts), by = c("home_team" = "team")) %>%
  dplyr::rename(home_to_adj = to_adj_pts) %>%
  dplyr::left_join(to_team %>% dplyr::select(team, to_adj_pts), by = c("away_team" = "team")) %>%
  dplyr::rename(away_to_adj = to_adj_pts) %>%
  mutate(
    mu_home = pmax(mu_home + home_to_adj, 0),
    mu_away = pmax(mu_away + away_to_adj, 0)
  )


# ====================================================================================================
# Simulator-based isotonic calibration (out-of-sample by week)
# Trains isotonic on TWO-WAY probabilities produced by a LIGHT sim at each cut.
# ====================================================================================================

# Tunables (keep same knobs you already use)
N_CALIB_YEARS   <- 5
CALIB_HALFLIFE  <- RECENCY_HALFLIFE
CALIB_USE_SOS   <- USE_SOS
CALIB_SOS_POW   <- SOS_STRENGTH
CALIB_TRIALS    <- 20000        # light sim per game-week for calibration
CALIB_SEED_BASE <- SEED + 12345 # stable seed offset for backtest sims

# Build "recent form" at a (cut_season, cut_week) same as earlier but local
recent_form_at_sim <- function(cut_season, cut_week, teams,
                               use_sos = TRUE, sos_pow = 0.6, halflife = 3) {
  rec_wts <- function(n, hl = halflife) { if (!USE_RECENCY_DECAY || n <= 1) rep(1, n) else (1/2)^((0:(n-1))/hl) }
  get_recent_cut <- function(team_abbr) {
    team_games |>
      dplyr::filter(team == team_abbr) |>
      dplyr::filter((.data$season < cut_season) | (.data$season == cut_season & .data$week < cut_week)) |>
      dplyr::arrange(dplyr::desc(game_date)) |>
      dplyr::slice_head(n = N_RECENT)
  }
  opp_tab_cut <- opp_strength(cut_season, cut_week)

  rf <- purrr::map_dfr(
    teams,
    ~{
      g <- get_recent_cut(.x)
      if (nrow(g) == 0) {
        tibble::tibble(team = .x, off_mean_raw = 21, def_mean_raw = 21,
                       off_sd_raw = 7, def_sd_raw = 7, n_games = 0)
      } else {
        w <- rec_wts(nrow(g)); w <- w / sum(w)
        if (use_sos) {
          g2 <- g |>
            dplyr::left_join(
              opp_tab_cut |>
                dplyr::select(team, def_ppg, off_ppg) |>
                dplyr::rename(opp_def_ppg = def_ppg, opp_off_ppg = off_ppg),
              by = c("opponent" = "team")
            ) |>
            dplyr::mutate(
              opp_def_ppg = dplyr::coalesce(opp_def_ppg, league_def_ppg),
              opp_off_ppg = dplyr::coalesce(opp_off_ppg, league_off_ppg),
              def_factor  = (opp_def_ppg / league_def_ppg),
              off_factor  = (opp_off_ppg / league_off_ppg)
            )
          pf_adj <- g2$points_for     * (g2$def_factor ^ sos_pow)
          pa_adj <- g2$points_against * (g2$off_factor ^ sos_pow)

          off_mean_raw <- sum(w * pf_adj)
          def_mean_raw <- sum(w * pa_adj)
          off_sd_raw   <- sqrt(stats::weighted.mean((pf_adj - off_mean_raw)^2, w))
          def_sd_raw   <- sqrt(stats::weighted.mean((pa_adj - def_mean_raw)^2, w))
        } else {
          off_mean_raw <- sum(w * g$points_for)
          def_mean_raw <- sum(w * g$points_against)
          off_sd_raw   <- sqrt(stats::weighted.mean((g$points_for     - off_mean_raw)^2, w))
          def_sd_raw   <- sqrt(stats::weighted.mean((g$points_against - def_mean_raw)^2, w))
        }
        tibble::tibble(
          team = .x,
          off_mean_raw = off_mean_raw,
          def_mean_raw = def_mean_raw,
          off_sd_raw   = ifelse(is.finite(off_sd_raw), off_sd_raw, 7),
          def_sd_raw   = ifelse(is.finite(def_sd_raw), def_sd_raw, 7),
          n_games      = nrow(g)
        )
      }
    }
  ) |>
    dplyr::mutate(
      # FIX: Adaptive regression to mean (was hardcoded 6, now uses REGRESSION_BASE × exp(-n/REGRESSION_GAMES))
      regression_weight = BETA_PRIOR_STRENGTH * (1 + REGRESSION_BASE * exp(-n_games / REGRESSION_GAMES)),
      off_mean_raw = (n_games * off_mean_raw + regression_weight * league_off_ppg) / (n_games + regression_weight),
      def_mean_raw = (n_games * def_mean_raw + regression_weight * league_def_ppg) / (n_games + regression_weight),
      off_sd_raw   = pmax(off_sd_raw, 5.5),
      def_sd_raw   = pmax(def_sd_raw, 5.5)
    ) |>
    dplyr::left_join(hfa_tbl, by = "team")

  # rest effects at the cutpoint
  last_game_cut <- team_games |>
    dplyr::group_by(team) |>
    dplyr::filter((.data$season < cut_season) | (.data$season == cut_season & .data$week < cut_week)) |>
    dplyr::arrange(dplyr::desc(game_date)) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(team, last_date = game_date, last_season = season, last_week = week)

  fake_slate_date <- {
    d <- sched |>
      dplyr::filter(.data$season == cut_season, .data$week == cut_week, game_type == "REG") |>
      dplyr::summarise(day = min(as.Date(game_date), na.rm = TRUE)) |>
      dplyr::pull(day)
    if (!is.finite(as.numeric(d))) as.Date(sprintf("%s-09-10", cut_season)) else d
  }

  rest_tbl_cut <- tibble::tibble(team = teams) |>
    dplyr::left_join(last_game_cut, by = "team") |>
    dplyr::mutate(
      days_rest = dplyr::coalesce(as.numeric(difftime(fake_slate_date, as.Date(last_date), units = "days")), 10),
      short_rest = days_rest <= 6,
      long_rest  = days_rest >= 9,
      bye_prev   = !is.na(last_season) & (last_season == cut_season) &
        (ifelse(is.na(last_week), FALSE, (cut_week - last_week) >= 2)),
      rest_points = 0 +
        ifelse(short_rest, REST_SHORT_PENALTY, 0) +
        ifelse(long_rest & !bye_prev, REST_LONG_BONUS, 0) +
        ifelse(bye_prev, BYE_BONUS, 0)
    ) |>
    dplyr::select(team, rest_points)

  rf |>
    dplyr::left_join(rest_tbl_cut, by = "team") |>
    dplyr::mutate(rest_points = dplyr::coalesce(rest_points, 0))
}

# --- MONTE CARLO (Negative Binomial + Gaussian Copula) -----------------------
# Correlated NB via Gaussian copula; respects your mu/sd targets
simulate_game_nb <- function(mu_home, sd_home, mu_away, sd_away,
                             n_trials = N_TRIALS, rho = RHO_SCORE, cap = PTS_CAP_HI, seed = SEED) {
  # 1) Sobol QMC + antithetic
  n_half <- ceiling(n_trials/2)
  U <- randtoolbox::sobol(n = n_half, dim = 2, scrambling = 0, seed = seed, normal = FALSE)
  U <- rbind(U, 1 - U)   # antithetic pairs
  Z1 <- qnorm(U[,1]); Z2 <- qnorm(U[,2])
  Z2c <- rho * Z1 + sqrt(pmax(1 - rho^2, 0)) * Z2
  u1 <- pnorm(Z1); u2 <- pnorm(Z2c)

  # 2) NB params (Poisson fallback if var<=mean)
  k_h <- nb_size_from_musd(mu_home, sd_home)
  k_a <- nb_size_from_musd(mu_away, sd_away)

  home <- if (is.infinite(k_h)) qpois(u1, lambda = pmax(mu_home, 0)) else
    qnbinom(u1, size = k_h, mu = pmax(mu_home, 0))
  away <- if (is.infinite(k_a)) qpois(u2, lambda = pmax(mu_away, 0)) else
    qnbinom(u2, size = k_a, mu = pmax(mu_away, 0))

  home <- pmin(home, cap); away <- pmin(away, cap)
  tibble::tibble(
    home   = home,
    away   = away,
    total  = home + away,
    margin = home - away,
    home_win = as.integer(home > away),
    away_win = as.integer(away > home),
    tie      = as.integer(home == away)
  )
}


week_inputs_and_sim_2w <- function(cut_season, cut_week, n_trials = CALIB_TRIALS) {
  slate <- sched |>
    dplyr::filter(season == cut_season, week == cut_week, game_type == "REG") |>
    dplyr::select(game_id, game_date, home_team, away_team, home_score, away_score) |>
    dplyr::distinct()
  if (!nrow(slate)) return(tibble::tibble())

  fake_date <- min(as.Date(slate$game_date), na.rm = TRUE)
  teams <- sort(unique(c(slate$home_team, slate$away_team)))

  # recent form (SoS + decay + rest) at the cut
  rf <- recent_form_at_sim(
    cut_season, cut_week, teams,
    use_sos = CALIB_USE_SOS, sos_pow = CALIB_SOS_POW, halflife = CALIB_HALFLIFE
  )

  # --------- Build PACE and PPD "as of" the cut date ----------
  gid_date <- sched_dates


  off_drives_game_cut <- pbp_for_pace %>%
    filter(!is.na(.data[[posteam_col]]), !is.na(.data[[drive_col]])) %>%
    distinct(game_id, team = .data[[posteam_col]], drive = .data[[drive_col]]) %>%
    inner_join(sched_dates, by = "game_id") %>%
    filter(sched_date < fake_date) %>%
    count(team, game_id, name = "off_drives")

  def_drives_game_cut <- pbp_for_pace %>%
    filter(!is.na(.data[[defteam_col]]), !is.na(.data[[drive_col]])) %>%
    distinct(game_id, team = .data[[defteam_col]], drive = .data[[drive_col]]) %>%
    inner_join(sched_dates, by = "game_id") %>%
    filter(sched_date < fake_date) %>%
    count(team, game_id, name = "def_drives")

  off_drives_cut <- off_drives_game_cut |>
    dplyr::group_by(team) |>
    dplyr::summarise(off_drives_pg = mean(off_drives, na.rm = TRUE), .groups = "drop")

  def_drives_cut <- def_drives_game_cut |>
    dplyr::group_by(team) |>
    dplyr::summarise(def_drives_pg = mean(def_drives, na.rm = TRUE), .groups = "drop")

  # points per game (pre-cut) for PPD
  team_points_game_cut <- sched |>
    dplyr::filter(game_completed, game_type %in% c("REG","POST"), as.Date(game_date) < fake_date) |>
    dplyr::transmute(
      game_id,
      home_team = .data[[home_team_col]],
      away_team = .data[[away_team_col]],
      home_pts  = .data[[home_pts_col]],
      away_pts  = .data[[away_pts_col]]
    )
  tp_home_cut <- team_points_game_cut |>
    dplyr::transmute(game_id, team = home_team, points_for = home_pts, points_against = away_pts)
  tp_away_cut <- team_points_game_cut |>
    dplyr::transmute(game_id, team = away_team, points_for = away_pts, points_against = home_pts)
  team_points_game_cut <- dplyr::bind_rows(tp_home_cut, tp_away_cut)

  off_ppd_game_cut <- off_drives_game_cut |>
    dplyr::left_join(team_points_game_cut |> dplyr::select(game_id, team, points_for),
                     by = c("game_id","team")) |>
    dplyr::mutate(off_ppd = points_for / off_drives)

  def_ppd_game_cut <- def_drives_game_cut |>
    dplyr::left_join(team_points_game_cut |> dplyr::select(game_id, team, points_against),
                     by = c("game_id","team")) |>
    dplyr::mutate(def_ppd = points_against / def_drives)

  off_ppd_tbl_cut <- off_ppd_game_cut |>
    dplyr::group_by(team) |>
    dplyr::summarise(off_ppd = mean(off_ppd, na.rm = TRUE), .groups = "drop")

  def_ppd_tbl_cut <- def_ppd_game_cut |>
    dplyr::group_by(team) |>
    dplyr::summarise(def_ppd = mean(def_ppd, na.rm = TRUE), .groups = "drop")

  league_off_ppd_cut <- mean(off_ppd_tbl_cut$off_ppd, na.rm = TRUE)
  league_def_ppd_cut <- mean(def_ppd_tbl_cut$def_ppd, na.rm = TRUE)

  off_ppd_tbl_cut <- off_ppd_tbl_cut |> dplyr::mutate(off_ppd = dplyr::coalesce(off_ppd, league_off_ppd_cut))
  def_ppd_tbl_cut <- def_ppd_tbl_cut |> dplyr::mutate(def_ppd = dplyr::coalesce(def_ppd, league_def_ppd_cut))

  pace_tbl_cut <- dplyr::full_join(off_drives_cut, def_drives_cut, by = "team")
  league_drives_avg_cut <- mean(pace_tbl_cut$off_drives_pg, na.rm = TRUE)
  if (!is.finite(league_drives_avg_cut) || league_drives_avg_cut <= 0) league_drives_avg_cut <- 11.5

  clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

  # --------- Build game mu/sd like your main slate ---------
  g <- slate |>
    # join recency/SD (rf) just to get sd_* and hfa/rest
    dplyr::left_join(rf |> dplyr::rename_with(~paste0("home_", .x), -team),
                     by = c("home_team" = "team")) |>
    dplyr::left_join(rf |> dplyr::rename_with(~paste0("away_", .x), -team),
                     by = c("away_team" = "team")) |>
    # join pace (as-of)
    dplyr::left_join(off_drives_cut |> dplyr::rename(home_off_drives_pg = off_drives_pg),
                     by = c("home_team" = "team")) |>
    dplyr::left_join(def_drives_cut |> dplyr::rename(home_def_drives_pg = def_drives_pg),
                     by = c("home_team" = "team")) |>
    dplyr::left_join(off_drives_cut |> dplyr::rename(away_off_drives_pg = off_drives_pg),
                     by = c("away_team" = "team")) |>
    dplyr::left_join(def_drives_cut |> dplyr::rename(away_def_drives_pg = def_drives_pg),
                     by = c("away_team" = "team")) |>
    # join PPD (as-of)
    dplyr::left_join(off_ppd_tbl_cut |> dplyr::rename(home_off_ppd = off_ppd),
                     by = c("home_team" = "team")) |>
    dplyr::left_join(def_ppd_tbl_cut |> dplyr::rename(home_def_ppd = def_ppd),
                     by = c("home_team" = "team")) |>
    dplyr::left_join(off_ppd_tbl_cut |> dplyr::rename(away_off_ppd = off_ppd),
                     by = c("away_team" = "team")) |>
    dplyr::left_join(def_ppd_tbl_cut |> dplyr::rename(away_def_ppd = def_ppd),
                     by = c("away_team" = "team")) |>
    dplyr::mutate(
      exp_drives_home = (home_off_drives_pg + away_def_drives_pg)/2,
      exp_drives_away = (away_off_drives_pg + home_def_drives_pg)/2,

      exp_ppd_home = ppd_blend(home_off_ppd, away_def_ppd, w_off = 0.65),
      exp_ppd_away = ppd_blend(away_off_ppd, home_def_ppd, w_off = 0.65),

      mu_home_base = exp_drives_home * exp_ppd_home,
      mu_away_base = exp_drives_away * exp_ppd_away,

      margin_shift = (home_hfa - away_hfa)/2,

      mu_home = pmax(mu_home_base + home_rest_points + margin_shift/2, 0),
      mu_away = pmax(mu_away_base + away_rest_points - margin_shift/2, 0),

      sd_home = pmax(home_off_sd_raw, 5.5),
      sd_away = pmax(away_off_sd_raw, 5.5),

      total_mu = mu_home + mu_away,
      sd_goal  = sd_total_curve(total_mu),
      sd_home  = 0.7 * sd_home + 0.3 * (sd_goal / sqrt(2)),
      sd_away  = 0.7 * sd_away + 0.3 * (sd_goal / sqrt(2)),
      sd_home  = pmax(sd_home, 4.8),
      sd_away  = pmax(sd_away, 4.8),

      y_home = dplyr::case_when(
        is.finite(home_score) & is.finite(away_score) & home_score > away_score ~ 1L,
        is.finite(home_score) & is.finite(away_score) & home_score < away_score ~ 0L,
        TRUE ~ NA_integer_
      )
    ) |>
    dplyr::filter(!is.na(y_home))

  if (!nrow(g)) return(tibble::tibble())

  set.seed(CALIB_SEED_BASE + cut_season * 100 + cut_week)

  sim_list <- lapply(seq_len(nrow(g)), function(i) {
    sims <- simulate_game_nb(
      g$mu_home[i], g$sd_home[i],
      g$mu_away[i], g$sd_away[i],
      n_trials, rho = RHO_SCORE, cap = PTS_CAP_HI
    )

    prob_info <- margin_probs_from_summary(
      mean(sims$margin),
      stats::sd(sims$margin),
      mean(sims$tie)
    )

    tibble::tibble(
      game_id   = g$game_id[i],
      home_team = g$home_team[i],
      away_team = g$away_team[i],
      p_home_2w_sim = prob_info$home_p_2w,
      p_home_3w_sim = prob_info$home_win_prob,
      p_away_3w_sim = prob_info$away_win_prob,
      p_tie_3w_sim  = prob_info$tie_prob,
      margin_mean_sim = mean(sims$margin),
      margin_sd_sim   = prob_info$sd_effective,
      # outcome as 3-way label
      y3 = dplyr::case_when(
        is.finite(g$home_score[i]) & is.finite(g$away_score[i]) & g$home_score[i] > g$away_score[i] ~ "H",
        is.finite(g$home_score[i]) & is.finite(g$away_score[i]) & g$home_score[i] < g$away_score[i] ~ "A",
        TRUE ~ "T"
      ),
      y_home   = g$y_home[i]
    )
  })

  dplyr::bind_rows(sim_list)
}

# Collect calibration dataset across last N_CALIB_YEARS (completed seasons)
calib_seasons <- (SEASON - N_CALIB_YEARS):(SEASON - 1)
# --- disk cache for calibration dataset ---
.calib_key <- calib_cache_key(
  SEASON, N_CALIB_YEARS, CALIB_HALFLIFE, CALIB_USE_SOS, CALIB_SOS_POW,
  CALIB_TRIALS, RHO_SCORE, CALIB_SEED_BASE
)
.calib_path <- file.path(.calib_cache_dir, paste0(.calib_key, ".rds"))

if (file.exists(.calib_path)) {
  calib_sim_df <- readRDS(.calib_path)
} else {
  # (optional) parallel on Unix/macOS; harmless single-core on Windows
  weeks_by_season <- lapply(calib_seasons, function(s){
    sort(unique(sched$week[sched$season == s & sched$game_type == "REG"]))
  })
  names(weeks_by_season) <- calib_seasons

  # sequential fallback
  build_one <- function(s, w){
    result <- week_inputs_and_sim_2w(s, w, CALIB_TRIALS)
    # Add season and week columns for fold identification
    if (is.data.frame(result) && nrow(result) > 0) {
      result$season <- s
      result$week <- w
    }
    result
  }

  # simple sequential loop (robust and fast enough with skip+cache)
  out <- list(); idx <- 0L
  for (s0 in calib_seasons) {
    wks <- weeks_by_season[[as.character(s0)]]
    for (wk in wks) {
      idx <- idx + 1L
      message(sprintf("Calibrating: season %d week %d ...", s0, wk))
      out[[idx]] <- build_one(s0, wk)
      flush.console()
    }
  }

  calib_sim_df <- dplyr::bind_rows(out) %>%
    dplyr::filter(!is.na(y_home), is.finite(p_home_2w_sim)) %>%
    dplyr::mutate(
      # Create fold ID for leave-one-week-out CV (season_week identifier)
      fold_id = paste0(season, "_", week)
    )

  saveRDS(calib_sim_df, .calib_path)
}

# ═══════════════════════════════════════════════════════════════════════════════════
# NESTED CROSS-VALIDATION: Leave-One-Week-Out Isotonic Calibration
# FIX: Prevents data leakage by fitting isotonic on all weeks EXCEPT the test week
# ═══════════════════════════════════════════════════════════════════════════════════

if (!nrow(calib_sim_df)) {
  map_iso <- function(p) p
  map_iso_nested <- function(p, fold_id = NULL) p  # Fallback for no calibration data
  message("Simulator-based isotonic: no calibration data found; using identity.")
} else {
  # light binning to avoid heavy duplicates, then isotonic on simulator probs
  K <- 120                 # fewer bins = more samples per bin
  EPS_ISO <- ISOTONIC_EPSILON  # Use named constant (0.01)
  ALPHA <- 1              # Beta(2,2) prior ~ gentle Laplace smoothing

  # ═══ GLOBAL isotonic (for backward compatibility and non-nested predictions) ═══
  binned_global <- calib_sim_df %>%
    mutate(bin = pmin(pmax(floor(p_home_2w_sim * K), 0), K)) %>%
    group_by(bin) %>%
    summarise(
      x = mean(p_home_2w_sim),
      n = n(),
      # smoothed rate: (wins + ALPHA) / (n + 2*ALPHA)
      y = (sum(y_home) + ALPHA) / (n + 2*ALPHA),
      .groups = "drop"
    ) %>%
    arrange(x)

  # anchor the extremes so the fit can't hit 0/1
  iso_global <- stats::isoreg(
    c(0, binned_global$x, 1),
    c(EPS_ISO, binned_global$y, 1 - EPS_ISO)
  )

  x_support_global <- iso_global$x[!duplicated(iso_global$x)]
  y_support_global <- iso_global$yf[!duplicated(iso_global$x)]

  # Global mapping (includes test week - has leakage but kept for backward compatibility)
  map_iso <- function(p) {
    p <- pmin(pmax(p, 0), 1)
    y <- stats::approx(x_support_global, y_support_global, xout = p, rule = 2)$y
    pmin(pmax(y, EPS_ISO), 1 - EPS_ISO)
  }

  mae_global <- mean(abs(binned_global$x - binned_global$y), na.rm = TRUE)
  message(sprintf("Global isotonic fit - bins=%d | MAE=%.4f (includes test weeks - has leakage)",
                  nrow(binned_global), mae_global))

  # ═══ NESTED CV: Leave-One-Week-Out Isotonic Mappings ═══
  unique_folds <- unique(calib_sim_df$fold_id)

  # Pre-compute isotonic mappings for each fold (leave-one-out)
  isotonic_mappings <- list()

  message(sprintf("Computing %d leave-one-week-out isotonic calibrations (nested CV)...",
                  length(unique_folds)))

  for (test_fold in unique_folds) {
    # Train on all weeks EXCEPT test_fold (true out-of-sample)
    train_data <- calib_sim_df %>% filter(fold_id != test_fold)

    if (nrow(train_data) < MIN_SAMPLE_SIZE) {
      # Not enough training data - use global mapping
      isotonic_mappings[[test_fold]] <- list(
        x_support = x_support_global,
        y_support = y_support_global
      )
      next
    }

    # Bin and smooth training data
    binned_fold <- train_data %>%
      mutate(bin = pmin(pmax(floor(p_home_2w_sim * K), 0), K)) %>%
      group_by(bin) %>%
      summarise(
        x = mean(p_home_2w_sim),
        n = n(),
        y = (sum(y_home) + ALPHA) / (n + 2*ALPHA),
        .groups = "drop"
      ) %>%
      arrange(x)

    # Fit isotonic regression
    iso_fold <- stats::isoreg(
      c(0, binned_fold$x, 1),
      c(EPS_ISO, binned_fold$y, 1 - EPS_ISO)
    )

    # Store mapping for this fold
    isotonic_mappings[[test_fold]] <- list(
      x_support = iso_fold$x[!duplicated(iso_fold$x)],
      y_support = iso_fold$yf[!duplicated(iso_fold$x)]
    )
  }

  # Nested mapping function: applies fold-specific isotonic OR falls back to global
  map_iso_nested <- function(p, fold_id = NULL) {
    p <- pmin(pmax(p, 0), 1)

    # If fold_id provided and mapping exists, use nested (OOS) calibration
    if (!is.null(fold_id) && fold_id %in% names(isotonic_mappings)) {
      mapping <- isotonic_mappings[[fold_id]]
      y <- stats::approx(mapping$x_support, mapping$y_support, xout = p, rule = 2)$y
    } else {
      # Fallback to global mapping (for new/unseen folds)
      y <- stats::approx(x_support_global, y_support_global, xout = p, rule = 2)$y
    }

    pmin(pmax(y, EPS_ISO), 1 - EPS_ISO)
  }

  # Compute nested CV performance (true out-of-sample)
  calib_sim_df <- calib_sim_df %>%
    rowwise() %>%
    mutate(
      p_home_2w_cal_nested = map_iso_nested(p_home_2w_sim, fold_id),
      p_home_2w_cal_global = map_iso(p_home_2w_sim)
    ) %>%
    ungroup()

  # Calculate Brier scores for comparison
  brier_uncalibrated <- mean((calib_sim_df$p_home_2w_sim - calib_sim_df$y_home)^2, na.rm = TRUE)
  brier_global <- mean((calib_sim_df$p_home_2w_cal_global - calib_sim_df$y_home)^2, na.rm = TRUE)
  brier_nested <- mean((calib_sim_df$p_home_2w_cal_nested - calib_sim_df$y_home)^2, na.rm = TRUE)

  message(sprintf("Brier Scores | Uncalibrated: %.4f | Global: %.4f | Nested CV: %.4f",
                  brier_uncalibrated, brier_global, brier_nested))
  message(sprintf("Nested CV improvement over global: %.1f%% reduction in Brier score",
                  100 * (brier_global - brier_nested) / brier_global))

  # Store calibration diagnostics for later analysis
  calibration_diagnostics <- list(
    n_folds = length(unique_folds),
    n_games = nrow(calib_sim_df),
    brier_uncalibrated = brier_uncalibrated,
    brier_global = brier_global,
    brier_nested = brier_nested,
    nested_improvement_pct = 100 * (brier_global - brier_nested) / brier_global
  )

  # USE NESTED CV MAPPING BY DEFAULT (no data leakage)
  message("✅ Using nested CV isotonic calibration (leave-one-week-out, no leakage)")

  # Note: For LIVE PREDICTIONS (current week slate), we use global mapping
  # since current week is truly future data (not in any fold).
  # Nested CV is for backtesting only to measure true OOS performance.
}

# ═══════════════════════════════════════════════════════════════════════════════════
# PRACTICAL IMPACT: High-Confidence Betting Filters
# Identifies games with strong edge and high model confidence
# ═══════════════════════════════════════════════════════════════════════════════════

# Define confidence thresholds for practical betting
HIGH_CONFIDENCE_EDGE <- 0.05      # 5%+ EV edge required
MEDIUM_CONFIDENCE_EDGE <- 0.03    # 3%+ EV edge
LOW_UNCERTAINTY_THRESHOLD <- 0.15 # Model uncertainty < 15%
MIN_EDGE_FOR_BET <- 0.02          # Minimum 2% edge to consider

# Store thresholds in named list for easy tuning
betting_thresholds <- list(
  high_confidence_edge = HIGH_CONFIDENCE_EDGE,
  medium_confidence_edge = MEDIUM_CONFIDENCE_EDGE,
  low_uncertainty = LOW_UNCERTAINTY_THRESHOLD,
  min_edge = MIN_EDGE_FOR_BET
)

# ----- 3-way multinomial calibration (H/A/T) -----
cal3 <- NULL
if (nrow(calib_sim_df) && all(c("p_home_3w_sim","p_away_3w_sim","p_tie_3w_sim","y3") %in% names(calib_sim_df))) {
  eps <- 1e-6
  train3 <- calib_sim_df %>%
    transmute(
      y3 = factor(y3, levels = c("T","H","A")),
      xH = log((p_home_3w_sim + eps) / (p_tie_3w_sim + eps)),
      xA = log((p_away_3w_sim + eps) / (p_tie_3w_sim + eps))
    ) %>% filter(is.finite(xH), is.finite(xA))

  # If no ties in training, skip 3-way multinomial (avoid the warning)
  if (sum(train3$y3 == "T", na.rm = TRUE) == 0) {
    cal3 <- NULL
  } else if (nrow(train3) > 100) {
    cal3 <- nnet::multinom(y3 ~ xH + xA, data = train3, trace = FALSE, MaxNWts = 10000, maxit = 500)
  }
}


calibrate_3way <- function(pH, pA, pT) {
  if (is.null(cal3)) return(c(H = pH, A = pA, T = pT))
  eps <- 1e-6
  x <- data.frame(
    xH = log((pH + eps) / (pT + eps)),
    xA = log((pA + eps) / (pT + eps))
  )
  pr <- try(predict(cal3, newdata = x, type = "probs"), silent = TRUE)
  if (inherits(pr, "try-error")) return(c(H = pH, A = pA, T = pT))

  # normalize predict() output to a single named vector with T/H/A
  if (is.null(dim(pr))) {
    # single-row came back as named vector
    out <- c(T = NA_real_, H = NA_real_, A = NA_real_)
    out[names(pr)] <- pr
  } else {
    cols <- colnames(pr)
    out <- c(
      T = pr[1, match("T", cols, nomatch = NA)],
      H = pr[1, match("H", cols, nomatch = NA)],
      A = pr[1, match("A", cols, nomatch = NA)]
    )
  }

  # fall back to raw where missing, clamp, renormalize
  raw <- c(T = pT, H = pH, A = pA)
  out[is.na(out)] <- raw[names(out)[is.na(out)]]
  out <- pmin(pmax(out, eps), 1 - eps)
  out <- out / sum(out)

  c(H = out["H"], A = out["A"], T = out["T"])
}

# ---- Safe fallbacks if calibrators weren't created above ----
if (!exists("map_iso")) {
  map_iso <- function(p) p  # identity map
}
# NOTE: your calibrate_3way() already falls back to raw if cal3 is NULL

# --- Safety: ensure all env columns exist on game_modifiers ---
req_cols <- list(
  game_id     = character(),
  mu_home_adj = numeric(),
  mu_away_adj = numeric(),
  sd_home_adj = numeric(),
  sd_away_adj = numeric(),
  dome        = logical(),
  windy       = logical(),
  cold        = logical(),
  precip      = logical(),
  wind_mph    = numeric(),
  temp_f      = numeric(),
  precip_prob = numeric()
)
for (nm in names(req_cols)) {
  if (!nm %in% names(game_modifiers)) game_modifiers[[nm]] <- req_cols[[nm]]
}



# --- Environment / pace hooks from game_modifiers (robust when empty) ---
env_mod <- game_modifiers %>%
  transmute(
    game_id,
    mu_home_adj = coalesce(mu_home_adj, 0),
    mu_away_adj = coalesce(mu_away_adj, 0),
    sd_home_adj = coalesce(sd_home_adj, 0),
    sd_away_adj = coalesce(sd_away_adj, 0),
    env_dome   = coalesce(dome,   FALSE),
    env_windy  = coalesce(windy,  FALSE),
    env_cold   = coalesce(cold,   FALSE),
    env_precip = coalesce(precip, FALSE),
    wind_mph,
    temp_f,
    precip_prob
  )

# If the overrides table is empty, fabricate a per-game NA/0 row set
env_mod_full <- if (nrow(env_mod) > 0) {
  env_mod
} else {
  week_slate %>%
    transmute(
      game_id,
      mu_home_adj = 0, mu_away_adj = 0,
      sd_home_adj = 0, sd_away_adj = 0,
      env_dome = FALSE, env_windy = FALSE, env_cold = FALSE, env_precip = FALSE,
      wind_mph = NA_real_, temp_f = NA_real_, precip_prob = NA_real_
    )
}

# make sure week_slate has a dome flag to fall back to
week_slate_domes <- week_slate %>% dplyr::select(game_id, week_dome = dome)

if (!"week_dome" %in% names(games_ready)) {
  games_ready <- games_ready %>% left_join(week_slate_domes, by = "game_id")
}

# (HARDENING GUARD A) If weather join failed earlier, create the columns now
if (!all(c("wind_mph","temp_f","precip_prob") %in% names(games_ready))) {
  games_ready <- games_ready %>%
    mutate(wind_mph = NA_real_, temp_f = NA_real_, precip_prob = NA_real_)
}

# Now join the guaranteed-full env overrides and apply adjustments
games_ready <- games_ready %>%
  left_join(env_mod_full, by = "game_id", suffix = c("", ".env")) %>%
  mutate(
    wind_mph    = coalesce(wind_mph,    wind_mph.env),
    temp_f      = coalesce(temp_f,      temp_f.env),
    precip_prob = coalesce(precip_prob, precip_prob.env)
  ) %>%
  dplyr::select(-ends_with(".env")) %>%
  mutate(
    mu_home_adj = coalesce(mu_home_adj, 0),
    mu_away_adj = coalesce(mu_away_adj, 0),
    sd_home_adj = coalesce(sd_home_adj, 0),
    sd_away_adj = coalesce(sd_away_adj, 0),

    dome   = coalesce(env_dome, week_dome, FALSE),
    windy  = coalesce(env_windy, FALSE),
    cold   = coalesce(env_cold,  FALSE),
    precip = coalesce(env_precip, FALSE),

    # Base weather effects (same as before)
    env_total_auto =
      ifelse(dome, DOME_BONUS_TOTAL, 0) +
      pmax(coalesce(wind_mph, 0) - 12, 0) * (-0.04) +
      ifelse(!dome & !is.na(temp_f) & temp_f <= 35, -0.25, 0) +
      ifelse(!dome & !is.na(precip_prob) & precip_prob >= 0.5, -0.50, 0),

    env_total_flags =
      ifelse(is.na(wind_mph)    & windy,  OUTDOOR_WIND_PEN * 0.5, 0) +
      ifelse(is.na(temp_f)      & cold,   COLD_TEMP_PEN    * 0.5, 0) +
      ifelse(is.na(precip_prob) & precip, RAIN_SNOW_PEN    * 0.5, 0),

    # Multiplicative weather interaction effects (data-driven per user analysis)
    # Wind hurts pass-heavy teams more: effect = wind × (pass_rate - league_avg)
    home_pass_dev = coalesce(home_pass_rate, league_avg_pass_rate) - league_avg_pass_rate,
    away_pass_dev = coalesce(away_pass_rate, league_avg_pass_rate) - league_avg_pass_rate,
    wind_interaction_home = ifelse(!dome & !is.na(wind_mph) & wind_mph > 12,
                                    -0.10 * (wind_mph - 12) * home_pass_dev, 0),
    wind_interaction_away = ifelse(!dome & !is.na(wind_mph) & wind_mph > 12,
                                    -0.10 * (wind_mph - 12) * away_pass_dev, 0),

    # Cold effect: dome teams struggle more in cold (body not acclimated)
    # Identify dome teams by checking if their stadium is a dome
    home_is_dome_team = dome,  # Playing at home dome
    away_is_dome_team = FALSE, # Away team dome status would need roster lookup
    cold_interaction_home = ifelse(!dome & !is.na(temp_f) & temp_f <= 35 & away_is_dome_team,
                                    -0.40, 0),  # Away dome team penalty
    cold_interaction_away = ifelse(!dome & !is.na(temp_f) & temp_f <= 35 & home_is_dome_team,
                                    -0.40, 0),  # Would be home dome team if traveling

    env_total_adj = env_total_auto + env_total_flags,

    mu_home = pmax(mu_home + env_total_adj/2 + mu_home_adj + wind_interaction_home + cold_interaction_home, 0),
    mu_away = pmax(mu_away + env_total_adj/2 + mu_away_adj + wind_interaction_away + cold_interaction_away, 0),
    sd_home = pmax(sd_home + sd_home_adj, 5.0),
    sd_away = pmax(sd_away + sd_away_adj, 5.0)
  )
# ------------------------ OVERTIME STATS (data-driven) ------------------------
# We'll compute:
#   - ot_rate_hist       = P(game goes to OT)
#   - p_tie_given_ot     = P(tie | OT)
#   - league_ot_home_prob= P(home wins | OT & non-tie)
#   - team-level OT home/away tendencies with shrinkage
#
# Robustly detect "went OT" from schedules if available, else from PBP.

# Helper: pull a "went_ot" table (REG season) for a set of seasons
get_ot_flags <- function(seasons_vec) {
  nm <- names(sched)
  if ("overtime" %in% nm || "ot" %in% nm) {
    flag_col <- if ("overtime" %in% nm) "overtime" else "ot"
    return(
      sched |>
        dplyr::filter(game_type == "REG", season %in% seasons_vec, game_completed) |>
        dplyr::transmute(game_id, went_ot = as.logical(.data[[flag_col]]))
    )
  }
  # Reuse already-loaded pbp_hist if present
  if (exists("pbp_hist")) {
    nm2 <- names(pbp_hist)
    if ("qtr" %in% nm2) {
      flags <- pbp_hist %>% dplyr::filter(season %in% seasons_vec) %>%
        dplyr::group_by(game_id) %>% dplyr::summarise(went_ot = any(qtr >= 5), .groups="drop")
    } else if ("quarter" %in% nm2) {
      flags <- pbp_hist %>% dplyr::filter(season %in% seasons_vec) %>%
        dplyr::group_by(game_id) %>% dplyr::summarise(went_ot = any(quarter >= 5), .groups="drop")
    } else if ("period" %in% nm2) {
      flags <- pbp_hist %>% dplyr::filter(season %in% seasons_vec) %>%
        dplyr::group_by(game_id) %>% dplyr::summarise(went_ot = any(period >= 5), .groups="drop")
    } else if ("game_half" %in% nm2) {
      flags <- pbp_hist %>% dplyr::filter(season %in% seasons_vec) %>%
        dplyr::group_by(game_id) %>% dplyr::summarise(
          went_ot = any(grepl("OT|Overtime", game_half, ignore.case = TRUE)), .groups="drop")
    } else {
      stop("Could not detect OT from schedules or PBP schema.")
    }
    return(flags)
  }
  stop("No OT flag in schedules and pbp_hist not available.")
}


# Use last 8 *completed* seasons for stable priors
seasons_hist <- pmax(2017, SEASON - 7):(SEASON - 1)
ot_flags_hist <- get_ot_flags(seasons_hist)

# Join to final scores & teams
ot_joined <- sched |>
  dplyr::filter(game_type == "REG", season %in% seasons_hist, game_completed) |>
  dplyr::transmute(
    game_id,
    season,
    home_team = .data[[home_team_col]],
    away_team = .data[[away_team_col]],
    home_pts  = .data[[home_pts_col]],
    away_pts  = .data[[away_pts_col]]
  ) |>
  dplyr::inner_join(ot_flags_hist, by = "game_id") |>
  dplyr::mutate(
    tie = home_pts == away_pts,
    home_win = (home_pts > away_pts)
  )

# 1) Historical OT trigger rate (fraction of games that went OT)
ot_rate_hist <- mean(ot_joined$went_ot, na.rm = TRUE)
if (!is.finite(ot_rate_hist)) ot_rate_hist <- 0.12  # safe fallback

# 2) Tie given OT (among OT games, fraction that ended tied)
p_tie_given_ot <- {
  sub <- ot_joined |> dplyr::filter(went_ot)
  if (nrow(sub)) mean(sub$tie, na.rm = TRUE) else 0.03
}

# 3) League baseline: P(home wins | OT & non-tie)
league_ot_home_prob <- {
  sub <- ot_joined |> dplyr::filter(went_ot, !tie)
  if (nrow(sub)) mean(sub$home_win, na.rm = TRUE) else 0.55
}

# 4) Team-level OT tendencies with shrinkage toward league baseline
#    Home side: P(home wins | OT & non-tie) when team is home
#    Away side: P(away wins | OT & non-tie) when team is away

# choose prior strength ~ median sample size per team
prior_n <- ot_joined |>
  dplyr::filter(went_ot, !tie) |>
  dplyr::count(home_team, name = "n") |>
  dplyr::summarise(med = stats::median(n, na.rm = TRUE)) |>
  dplyr::pull(med)
if (!is.finite(prior_n) || prior_n <= 0) prior_n <- 6

a0 <- league_ot_home_prob * prior_n
b0 <- (1 - league_ot_home_prob) * prior_n

# Home OT table
home_ot_tbl <- ot_joined |>
  dplyr::filter(went_ot, !tie) |>
  dplyr::group_by(home_team) |>
  dplyr::summarise(
    n = dplyr::n(),
    w = sum(home_win, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(home_ot_win = (w + a0) / (n + a0 + b0)) |>
  dplyr::select(team = home_team, home_ot_win)

# Away OT table (prob away wins when team is away)
away_ot_tbl <- ot_joined |>
  dplyr::filter(went_ot, !tie) |>
  dplyr::group_by(away_team) |>
  dplyr::summarise(
    n = dplyr::n(),
    w_away = sum(!home_win, na.rm = TRUE),  # away wins when home loses
    .groups = "drop"
  ) |>
  dplyr::mutate(away_ot_win = (w_away + a0) / (n + a0 + b0)) |>
  dplyr::select(team = away_team, away_ot_win)

# Join into games_ready and blend:  ot_home_prob = average of
#   (home team's home-OT win rate) and (1 - away team's away-OT win rate)
games_ready <- games_ready |>
  dplyr::left_join(home_ot_tbl, by = c("home_team" = "team")) |>
  dplyr::left_join(away_ot_tbl, by = c("away_team" = "team")) |>
  dplyr::mutate(
    home_ot_win = dplyr::coalesce(home_ot_win, league_ot_home_prob),
    away_ot_win = dplyr::coalesce(away_ot_win, 1 - league_ot_home_prob),
    ot_home_prob = 0.5 * (home_ot_win + (1 - away_ot_win)),
    # keep it realistic
    ot_home_prob = pmin(pmax(ot_home_prob, 0.35), 0.65)
  ) |>
  dplyr::select(-home_ot_win, -away_ot_win)

# --- Rebuild sims to match the current games_ready exactly ---
stopifnot(nrow(games_ready) > 0)

# Safety: coerce any NA mu/sd to sane values so sim never drops a game
safe_mu  <- function(x) ifelse(is.finite(x) & x >= 0, x, 21)
safe_sd  <- function(x) ifelse(is.finite(x) & x >= 5, x, 7)

# If any mu is NA after all adjustments, rebuild it from safe inputs
games_ready <- games_ready %>%
  mutate(
    mu_home = dplyr::if_else(
      is.finite(mu_home),
      mu_home,
      pmax(exp_drives_home * exp_ppd_home + dplyr::coalesce(HFA_pts, 0), 0)
    ),
    mu_away = dplyr::if_else(
      is.finite(mu_away),
      mu_away,
      pmax(exp_drives_away * exp_ppd_away, 0)
    )
  )


results_list <- lapply(seq_len(nrow(games_ready)), function(i) {
  set.seed(SEED + i)
  simulate_game_nb(
    mu_home = safe_mu(games_ready$mu_home[i]),
    sd_home = safe_sd(games_ready$sd_home[i]),
    mu_away = safe_mu(games_ready$mu_away[i]),
    sd_away = safe_sd(games_ready$sd_away[i]),
    n_trials = N_TRIALS,
    rho = dplyr::coalesce(games_ready$rho_game[i], RHO_SCORE),
    cap = PTS_CAP_HI
  )
})

# Sanity check: must align 1:1 with games_ready
if (length(results_list) != nrow(games_ready)) {
  stop(sprintf("Sim alignment failed: results_list=%d, games_ready=%d",
               length(results_list), nrow(games_ready)))
}


# ------------------------ OVERTIME (trigger + resolution) ---------------------
# 1) Compute historical OT rate (already computed earlier as ot_rate_hist)
# 2) Historical ties among OT games: ot_tie_rate (already computed via ot_stats)

# Trigger OT if regulation margin == 0 (certain) or |margin| == 1 (with probability alpha_1pt)
trigger_ot_vec <- function(margin, alpha_1pt) {
  (margin == 0) | ((abs(margin) == 1) & (runif(length(margin)) < alpha_1pt))
}

# Resolve OT: with small prob keep a tie; else +1 to winner with ot_home_prob
resolve_ot_vec <- function(home, away, went_ot, p_tie_given_ot, ot_home_prob, cap = PTS_CAP_HI) {
  n <- length(home)
  keep_tie <- went_ot & (runif(n) < p_tie_given_ot)
  need_res <- went_ot & !keep_tie
  home_wins <- need_res & (runif(n) < ot_home_prob)
  if (any(home_wins)) {
    i <- which(home_wins); home[i] <- pmin(home[i] + 1, cap)
  }
  if (any(need_res & !home_wins)) {
    j <- which(need_res & !home_wins); away[j] <- pmin(away[j] + 1, cap)
  }
  tibble(
    home   = home,
    away   = away,
    total  = home + away,
    margin = home - away,
    home_win = as.integer(home > away),
    away_win = as.integer(away > home),
    tie      = as.integer(keep_tie & (home == away))
  )
}

# Calibrate alpha_1pt so overall OT rate matches history:
estimate_alpha <- function(results_list, target_ot_rate) {
  # pilot margins across all games
  margins <- unlist(lapply(results_list, function(tb) tb$home - tb$away), use.names = FALSE)
  f <- function(alpha) {
    went_ot <- trigger_ot_vec(margins, alpha)
    mean(went_ot, na.rm = TRUE) - target_ot_rate
  }
  # bracket [0,1]; fallbacks if uniroot fails
  out <- tryCatch(uniroot(f, c(0, 1))$root, error = function(e) 0.35)
  pmin(pmax(out, 0), 1)
}

alpha_1pt <- estimate_alpha(results_list, ot_rate_hist)
# p_tie_given_ot already computed in the data-driven OT block above

# Apply OT logic per game using per-game ot_home_prob (built earlier)
resolved_list <- Map(function(sim_df, ot_hp) {
  went_ot <- trigger_ot_vec(sim_df$home - sim_df$away, alpha_1pt)
  resolve_ot_vec(sim_df$home, sim_df$away, went_ot, p_tie_given_ot, ot_hp, cap = PTS_CAP_HI)
}, results_list, games_ready$ot_home_prob)

overall_tie_rate <- sum(vapply(resolved_list, function(tb) sum(tb$tie), integer(1))) /
  (N_TRIALS * length(resolved_list))
message(sprintf("Final tie rate: %.3f%% | alpha_1pt=%.3f", 100*overall_tie_rate, alpha_1pt))

# Per-game resolver: convert a portion of regulation ties into OT results


# --- Build/repair `final` from resolved_list + games_ready ---------------------
build_final_safe <- function(resolved_list, games_ready) {
  stopifnot(length(resolved_list) == nrow(games_ready))

  qfun <- function(v, p) as.numeric(quantile(v, probs = p, na.rm = TRUE, names = FALSE, type = 7))

  sum_tbl <- purrr::map2_dfr(
    resolved_list,
    seq_len(nrow(games_ready)),
    ~{
      sims <- .x
      g <- games_ready[.y,]

      margin_mean <- mean(sims$margin)
      margin_sd_raw <- stats::sd(sims$margin)
      tie_prob      <- mean(sims$tie)
      prob_info     <- margin_probs_from_summary(margin_mean, margin_sd_raw, tie_prob)

      tibble::tibble(
        season  = g$season,
        week    = g$week,
        date    = as.Date(g$game_date),
        matchup = paste0(g$away_team, " @ ", g$home_team),
        proj_away_score = round(mean(sims$away)),
        proj_home_score = round(mean(sims$home)),
        away_mean_pts   = mean(sims$away),
        home_mean_pts   = mean(sims$home),
        total_mean      = mean(sims$total),
        margin_mean     = margin_mean,
        away_sd_pts     = stats::sd(sims$away),
        home_sd_pts     = stats::sd(sims$home),
        total_sd        = stats::sd(sims$total),
        margin_sd       = margin_sd_raw,
        margin_sd_eff   = prob_info$sd_effective,
        away_median_pts = median(sims$away),
        home_median_pts = median(sims$home),
        total_median    = median(sims$total),
        margin_median   = median(sims$margin),
        away_ci_lo      = qfun(sims$away, 0.025),
        away_ci_hi      = qfun(sims$away, 0.975),
        home_ci_lo      = qfun(sims$home, 0.025),
        home_ci_hi      = qfun(sims$home, 0.975),
        total_ci_lo     = qfun(sims$total, 0.025),
        total_ci_hi     = qfun(sims$total, 0.975),
        margin_ci_lo    = qfun(sims$margin, 0.025),
        margin_ci_hi    = qfun(sims$margin, 0.975),
        home_win_prob   = prob_info$home_win_prob,
        away_win_prob   = prob_info$away_win_prob,
        tie_prob        = prob_info$tie_prob,
        mu_home_used    = g$mu_home,
        mu_away_used    = g$mu_away,
        sd_home_used    = g$sd_home,
        sd_away_used    = g$sd_away
      )
    }
  ) |>
    dplyr::arrange(date, matchup)

  out <- sum_tbl |>
    dplyr::select(
      season, week, date, matchup,
      proj_away_score, proj_home_score,
      away_mean_pts, home_mean_pts,
      away_sd_pts, home_sd_pts,
      away_median_pts, home_median_pts,
      away_ci_lo, away_ci_hi, home_ci_lo, home_ci_hi,
      total_mean, total_sd, total_median, total_ci_lo, total_ci_hi,
      margin_mean, margin_sd, margin_sd_eff, margin_median, margin_ci_lo, margin_ci_hi,
      home_win_prob, away_win_prob, tie_prob
    )

  # Guarantee calibrated columns exist even if isotonic hasn't run yet
  if (!("home_win_prob_cal" %in% names(out)) ||
      !("away_win_prob_cal" %in% names(out))) {
    out <- out |>
      dplyr::mutate(
        home_win_prob_cal = home_win_prob,
        away_win_prob_cal = away_win_prob
      )
  }
  out
}
# Rebuild `final` if missing OR just to be sure
final <- build_final_safe(resolved_list, games_ready)
# ---- Apply DIRECT 3-way calibration, then derive 2-way from it ----
final <- final %>%
  rowwise() %>%
  mutate(
    .cal = list(calibrate_3way(home_win_prob, away_win_prob, tie_prob)),
    h_cal = suppressWarnings(as.numeric(.cal[[1]]["H"])),
    a_cal = suppressWarnings(as.numeric(.cal[[1]]["A"])),
    t_cal = suppressWarnings(as.numeric(.cal[[1]]["T"])),

    # if calibration returns invalid values, use raw
    bad = any(!is.finite(c(h_cal, a_cal, t_cal))) ||
      any(c(h_cal, a_cal, t_cal) < 0) ||
      (h_cal + a_cal + t_cal) == 0,

    home_win_prob_cal = ifelse(bad, home_win_prob, h_cal),
    away_win_prob_cal = ifelse(bad, away_win_prob, a_cal),
    tie_prob          = ifelse(bad, tie_prob, t_cal)
  ) %>%
  ungroup() %>%
  mutate(
    # NA-safe renormalization
    sum3 = pmax(rowSums(cbind(home_win_prob_cal, away_win_prob_cal, tie_prob), na.rm = TRUE), 1e-9),
    home_win_prob_cal = home_win_prob_cal / sum3,
    away_win_prob_cal = away_win_prob_cal / sum3,
    tie_prob          = tie_prob / sum3
  ) %>%
  dplyr::select(-.cal, -h_cal, -a_cal, -t_cal, -bad, -sum3)

# (Use home/away + date to find the correct row in games_ready)
final <- final %>%
  tidyr::separate(matchup, into = c("away_team","home_team"),
                  sep = " @ ", remove = FALSE) %>%
  dplyr::left_join(
    games_ready %>%
      dplyr::transmute(game_id,
                       home_team,
                       away_team,
                       date = as.Date(game_date)),
    by = c("home_team","away_team","date")
  ) %>%
  dplyr::distinct(game_id, .keep_all = TRUE)


print(
  final %>%
    dplyr::select(matchup, home_win_prob, away_win_prob, tie_prob,
                  home_win_prob_cal, away_win_prob_cal, tie_prob) %>%
    slice_head(n = 8)
)

# helper
.clamp01 <- function(x, lo = 1e-3, hi = 1 - 1e-3) pmin(pmax(x, lo), hi)

# --- Derive THREE-WAY (calibrated already) and TWO-WAY from calibrated --------
# 3-way calibrated already in: home_win_prob_cal, away_win_prob_cal, tie_prob

# TWO-WAY derived from calibrated 3-way (for market/EV usage)
final <- final |>
  dplyr::mutate(
    two_way_mass_cal = pmax(1 - tie_prob, 1e-9),
    home_p_2w_cal    = .clamp01(home_win_prob_cal / two_way_mass_cal),
    away_p_2w_cal    = .clamp01(away_win_prob_cal / two_way_mass_cal)
  )

# (Optional) keep raw two-way for QA (not used in tables)
final <- final |>
  dplyr::mutate(
    two_way_mass_raw = pmax(home_win_prob + away_win_prob, 1e-9),
    home_p_2w_raw    = .clamp01(home_win_prob / two_way_mass_raw),
    away_p_2w_raw    = 1 - home_p_2w_raw
  )

# ═══════════════════════════════════════════════════════════════════════════════════
# MARKET DATA: Load market probabilities (needed for uncertainty and betting analysis)
# Must be loaded BEFORE uncertainty calculations below
# ═══════════════════════════════════════════════════════════════════════════════════

# Helper function for probability clamping
.clp <- function(x, lo = 1e-3, hi = 1 - 1e-3) pmin(pmax(x, lo), hi)

# ═══════════════════════════════════════════════════════════════════════════════════
# MARKET PROBABILITY HELPER FUNCTIONS
# ═══════════════════════════════════════════════════════════════════════════════════

# Convert spread to implied probability (using historical logistic fit)
map_spread_prob <- function(sp) {
  # sp = home spread (negative favors home)
  # Logistic regression fit from historical data
  plogis(-sp / 3.5)  # roughly ~14% per point near pk, flatter at extremes
}

# ═══════════════════════════════════════════════════════════════════════════════════
# MARKET PROBABILITY EXTRACTION FUNCTION
# Converts market data (moneylines, spreads) to implied probabilities
# ═══════════════════════════════════════════════════════════════════════════════════

market_probs_from_sched <- function(sched_df) {
  .clp <- function(x, eps = 1e-12) pmin(pmax(x, eps), 1 - eps)
  .pick <- function(df, cands) {
    nm <- intersect(cands, names(df))
    if (length(nm)) nm[1] else NA_character_
  }
  american_to_probability <- function(odds) {
    ifelse(
      is.na(odds) | odds == 0, NA_real_,
      ifelse(odds < 0, (-odds)/((-odds) + 100), 100/(odds + 100))
    )
  }

  base <- sched_df %>%
    dplyr::filter(!is.na(game_id)) %>%
    dplyr::transmute(game_id, season, week) %>%
    dplyr::distinct()

  if (!nrow(base)) {
    return(tibble::tibble(game_id = character(), season = integer(), week = integer(), p_home_mkt_2w = numeric()))
  }

  ml_home <- .pick(sched_df, c(
    "home_ml_close", "ml_home_close", "moneyline_home_close", "home_moneyline_close",
    "home_ml", "ml_home", "moneyline_home", "home_moneyline"
  ))
  ml_away <- .pick(sched_df, c(
    "away_ml_close", "ml_away_close", "moneyline_away_close", "away_moneyline_close",
    "away_ml", "ml_away", "moneyline_away", "away_moneyline"
  ))
  sp_col <- .pick(sched_df, c(
    "close_spread", "spread_close", "home_spread_close",
    "spread_line", "spread", "home_spread", "spread_favorite"
  ))

  ml_tbl <- tibble::tibble(
    game_id = character(), season = integer(), week = integer(), p_home_mkt_2w_ml = numeric()
  )
  if (!is.na(ml_home) && !is.na(ml_away)) {
    ml_tbl <- sched_df %>%
      dplyr::transmute(
        game_id, season, week,
        p_home_raw = american_to_probability(suppressWarnings(as.numeric(.data[[ml_home]]))),
        p_away_raw = american_to_probability(suppressWarnings(as.numeric(.data[[ml_away]])))
      ) %>%
      dplyr::mutate(
        den = p_home_raw + p_away_raw,
        p_home_mkt_2w_ml = .clp(ifelse(is.finite(den) & den > 0, p_home_raw/den, NA_real_))
      ) %>%
      dplyr::filter(is.finite(p_home_mkt_2w_ml)) %>%
      dplyr::select(game_id, season, week, p_home_mkt_2w_ml) %>%
      dplyr::distinct()
  }

  spread_tbl <- tibble::tibble(
    game_id = character(), season = integer(), week = integer(), p_home_mkt_2w_spread = numeric()
  )
  if (!is.na(sp_col)) {
    spread_tbl <- sched_df %>%
      dplyr::transmute(
        game_id, season, week,
        home_spread = suppressWarnings(as.numeric(.data[[sp_col]]))
      ) %>%
      dplyr::filter(is.finite(home_spread)) %>%
      dplyr::mutate(p_home_mkt_2w_spread = .clp(map_spread_prob(home_spread))) %>%
      dplyr::filter(is.finite(p_home_mkt_2w_spread)) %>%
      dplyr::select(game_id, season, week, p_home_mkt_2w_spread) %>%
      dplyr::distinct()
  }

  out <- base %>%
    dplyr::left_join(ml_tbl, by = c("game_id", "season", "week")) %>%
    dplyr::left_join(spread_tbl, by = c("game_id", "season", "week")) %>%
    dplyr::mutate(
      p_home_mkt_2w = dplyr::coalesce(p_home_mkt_2w_ml, p_home_mkt_2w_spread)
    ) %>%
    dplyr::select(game_id, season, week, p_home_mkt_2w)

  if (!any(is.finite(out$p_home_mkt_2w))) {
    message(
      "market_probs_from_sched(): no usable closing moneyline or spread columns found; returning NA probabilities."
    )
  }

  out
}

# Load market data and join to final predictions
mkt_now <- tryCatch(
  market_probs_from_sched(sched) %>%
    dplyr::transmute(game_id, home_p_2w_mkt = p_home_mkt_2w),
  error = function(e) tibble::tibble(game_id = character(), home_p_2w_mkt = numeric())
)

# Join market data and calculate away market probability
final <- final %>%
  dplyr::left_join(mkt_now, by = "game_id") %>%
  dplyr::mutate(
    home_p_2w_mkt = .clp(home_p_2w_mkt),
    away_p_2w_mkt = 1 - home_p_2w_mkt,  # Away market prob is complement of home
    # Fill missing market data with model predictions (for games without lines)
    home_p_2w_mkt = ifelse(is.na(home_p_2w_mkt), home_p_2w_cal, home_p_2w_mkt),
    away_p_2w_mkt = ifelse(is.na(away_p_2w_mkt), away_p_2w_cal, away_p_2w_mkt)
  )

# Add prediction intervals and model uncertainty metrics (CRITICAL for uncertainty quantification)
final <- final |>
  dplyr::left_join(
    games_ready %>%
      dplyr::select(game_id, mu_home, mu_away, sd_home, sd_away, k_home, k_away) %>%
      dplyr::mutate(
        k_home = pmin(pmax(k_home, NB_SIZE_MIN), NB_SIZE_MAX),
        k_away = pmin(pmax(k_away, NB_SIZE_MIN), NB_SIZE_MAX)
      ),
    by = "game_id"
  ) %>%
  dplyr::mutate(
    # Prediction intervals using negative binomial quantiles (95% CI)
    home_score_pi_lo = ifelse(is.finite(k_home) & k_home < Inf,
                               qnbinom((1 - CONFIDENCE_LEVEL) / 2, size = k_home, mu = mu_home),
                               qpois((1 - CONFIDENCE_LEVEL) / 2, lambda = mu_home)),
    home_score_pi_hi = ifelse(is.finite(k_home) & k_home < Inf,
                               qnbinom(1 - (1 - CONFIDENCE_LEVEL) / 2, size = k_home, mu = mu_home),
                               qpois(1 - (1 - CONFIDENCE_LEVEL) / 2, lambda = mu_home)),
    away_score_pi_lo = ifelse(is.finite(k_away) & k_away < Inf,
                               qnbinom((1 - CONFIDENCE_LEVEL) / 2, size = k_away, mu = mu_away),
                               qpois((1 - CONFIDENCE_LEVEL) / 2, lambda = mu_away)),
    away_score_pi_hi = ifelse(is.finite(k_away) & k_away < Inf,
                               qnbinom(1 - (1 - CONFIDENCE_LEVEL) / 2, size = k_away, mu = mu_away),
                               qpois(1 - (1 - CONFIDENCE_LEVEL) / 2, lambda = mu_away)),

    # Epistemic uncertainty from model disagreement (blend vs market)
    # Higher values = more model disagreement = less certain prediction
    model_uncertainty = abs(home_p_2w_cal - home_p_2w_mkt) / pmax(home_p_2w_cal + home_p_2w_mkt, 0.01),

    # Total uncertainty combines aleatoric (score variance) + epistemic (model disagreement)
    total_uncertainty = sqrt((sd_home^2 + sd_away^2) / 2) + 10 * model_uncertainty
  )

# ═══════════════════════════════════════════════════════════════════════════════════
# BETTING CONFIDENCE ANALYSIS: Identify High-Confidence Opportunities
# Uses thresholds defined earlier to flag games with strong edge and low uncertainty
# ═══════════════════════════════════════════════════════════════════════════════════

# Calculate EV edge for home and away (requires market data)
# Note: Full betting recommendations are in NFLmarket.R; this provides quick filters
final <- final %>%
  dplyr::mutate(
    # Calculate approximate EV edge (home team perspective)
    # This is a simplified version; full calculations are in moneyline report
    prob_edge_home = home_p_2w_cal - home_p_2w_mkt,
    prob_edge_away = away_p_2w_cal - away_p_2w_mkt,

    # Betting confidence flags (for filtering high-quality opportunities)
    high_confidence_home = (prob_edge_home > HIGH_CONFIDENCE_EDGE) &
                           (model_uncertainty < LOW_UNCERTAINTY_THRESHOLD),
    high_confidence_away = (prob_edge_away > HIGH_CONFIDENCE_EDGE) &
                           (model_uncertainty < LOW_UNCERTAINTY_THRESHOLD),
    medium_confidence_home = (prob_edge_home > MEDIUM_CONFIDENCE_EDGE) &
                             (model_uncertainty < LOW_UNCERTAINTY_THRESHOLD) &
                             !high_confidence_home,
    medium_confidence_away = (prob_edge_away > MEDIUM_CONFIDENCE_EDGE) &
                             (model_uncertainty < LOW_UNCERTAINTY_THRESHOLD) &
                             !high_confidence_away,

    # Overall betting signal strength (0 = pass, 1 = medium, 2 = high)
    betting_signal_home = dplyr::case_when(
      high_confidence_home ~ 2L,
      medium_confidence_home ~ 1L,
      TRUE ~ 0L
    ),
    betting_signal_away = dplyr::case_when(
      high_confidence_away ~ 2L,
      medium_confidence_away ~ 1L,
      TRUE ~ 0L
    )
  )

# Print betting opportunity summary
betting_summary <- final %>%
  dplyr::summarise(
    n_games = dplyr::n(),
    high_conf_home = sum(high_confidence_home, na.rm = TRUE),
    high_conf_away = sum(high_confidence_away, na.rm = TRUE),
    med_conf_home = sum(medium_confidence_home, na.rm = TRUE),
    med_conf_away = sum(medium_confidence_away, na.rm = TRUE),
    total_high_conf = high_conf_home + high_conf_away,
    total_med_conf = med_conf_home + med_conf_away,
    avg_model_uncertainty = mean(model_uncertainty, na.rm = TRUE),
    avg_prob_edge = mean(abs(prob_edge_home), na.rm = TRUE)
  )

message("\n═══════════════════════════════════════════════════════════════════")
message("BETTING CONFIDENCE SUMMARY (Practical Impact Analysis)")
message("═══════════════════════════════════════════════════════════════════")
message(sprintf("Total games analyzed: %d", betting_summary$n_games))
message(sprintf("High confidence opportunities: %d (%.1f%% of games)",
                betting_summary$total_high_conf,
                100 * betting_summary$total_high_conf / betting_summary$n_games))
message(sprintf("  - Home team: %d | Away team: %d",
                betting_summary$high_conf_home, betting_summary$high_conf_away))
message(sprintf("Medium confidence opportunities: %d (%.1f%% of games)",
                betting_summary$total_med_conf,
                100 * betting_summary$total_med_conf / betting_summary$n_games))
message(sprintf("  - Home team: %d | Away team: %d",
                betting_summary$med_conf_home, betting_summary$med_conf_away))
message(sprintf("Average model uncertainty: %.3f", betting_summary$avg_model_uncertainty))
message(sprintf("Average probability edge: %.3f (%.1f pp)",
                betting_summary$avg_prob_edge, 100 * betting_summary$avg_prob_edge))
message("═══════════════════════════════════════════════════════════════════\n")

# If there are high-confidence opportunities, display them
if (betting_summary$total_high_conf > 0) {
  high_conf_games <- final %>%
    dplyr::filter(high_confidence_home | high_confidence_away) %>%
    dplyr::mutate(
      bet_side = dplyr::case_when(
        high_confidence_home ~ "Home",
        high_confidence_away ~ "Away",
        TRUE ~ NA_character_
      ),
      prob_edge = dplyr::case_when(
        high_confidence_home ~ prob_edge_home,
        high_confidence_away ~ prob_edge_away,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::select(matchup, bet_side, prob_edge, model_uncertainty,
                  home_p_2w_cal, away_p_2w_cal) %>%
    dplyr::arrange(desc(prob_edge))

  message("🎯 HIGH CONFIDENCE BETTING OPPORTUNITIES:")
  print(high_conf_games, n = Inf)
  message("")
}

# Canonical calibrated probs (3-way) + canonical two-way derived from them
final_canon <- final |>
  tidyr::separate(matchup, into = c("away_team","home_team"), sep = " @ ", remove = FALSE) |>
  dplyr::select(
    date, matchup, home_team, away_team,
    home_win_prob_cal, away_win_prob_cal, tie_prob,
    p_home_2w_cal = home_p_2w_cal,
    p_away_2w_cal = away_p_2w_cal
  )

# 1) How much are we falling back to league?
games_ready %>%
  summarise(
    pct_off_drives_na = mean(is.na(home_off_drives_pg) | is.na(away_off_drives_pg), na.rm = TRUE),
    pct_ppd_na        = mean(is.na(home_off_ppd) | is.na(away_off_ppd) |
                               is.na(home_def_ppd) | is.na(away_def_ppd), na.rm = TRUE)
  )

# 2) Distributions feeding mu
games_ready %>%
  dplyr::select(exp_drives_home, exp_drives_away, exp_ppd_home, exp_ppd_away, mu_home, mu_away, total_mu) %>%
  summary()

# 3) Are pace & PPD nearly constant?
games_ready %>%
  transmute(
    drives_spread = abs(exp_drives_home - exp_drives_away),
    ppd_spread    = abs(exp_ppd_home   - exp_ppd_away)
  ) %>%
  summary()

# ====================================================================================================
# Multi-week scoring of your simulation vs. actuals (market-free)
# Uses week_inputs_and_sim_2w() -> p_home_2w_sim, p_home_3w_sim, p_away_3w_sim, p_tie_3w_sim, y_home, y3
# Calibrates 2-way with map_iso and 3-way with calibrate_3way().
# ====================================================================================================

.logloss2 <- function(p, y) {
  p <- .clamp01(p)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}
.logloss3 <- function(pH, pA, pT, yH, yA, yT) {
  pH <- .clamp01(pH); pA <- .clamp01(pA); pT <- .clamp01(pT)
  -mean(yH * log(pH) + yA * log(pA) + yT * log(pT))
}

# ---- FAST backtest using cached calibration sims (no re-sim) ----
score_weeks_fast <- function(start_season, end_season, weeks = NULL) {
  stopifnot(exists("calib_sim_df"))
  stopifnot(exists("map_iso"))
  stopifnot(exists("calibrate_3way"))

  sims <- calib_sim_df %>%
    dplyr::left_join(sched %>% dplyr::select(game_id, season, week), by = "game_id") %>%
    dplyr::filter(season >= start_season, season <= end_season) %>%
    { if (is.null(weeks)) . else dplyr::filter(., week %in% weeks) } %>%
    dplyr::filter(!is.na(y_home)) %>%
    dplyr::mutate(
      p2_raw = p_home_2w_sim,
      p2_cal = .clamp01(map_iso(p_home_2w_sim))
    )

  cald <- purrr::pmap_dfr(
    list(sims$p_home_3w_sim, sims$p_away_3w_sim, sims$p_tie_3w_sim),
    ~{
      v <- calibrate_3way(..1, ..2, ..3)
      tibble::tibble(H_cal = as.numeric(v["H"]), A_cal = as.numeric(v["A"]), T_cal = as.numeric(v["T"]))
    }
  )
  sims <- dplyr::bind_cols(sims, cald)

  sims <- sims %>%
    dplyr::mutate(
      y2 = y_home,
      yH = as.integer(y3 == "H"),
      yA = as.integer(y3 == "A"),
      yT = as.integer(y3 == "T")
    )

  by_week <- sims %>%
    dplyr::group_by(season, week) %>%
    dplyr::summarise(
      n_games      = dplyr::n(),
      Brier2_raw   = mean((p2_raw - y2)^2),
      Brier2_cal   = mean((p2_cal - y2)^2),
      LogLoss2_raw = .logloss2(p2_raw, y2),
      LogLoss2_cal = .logloss2(p2_cal, y2),
      Brier3_raw   = mean((p_home_3w_sim - yH)^2 + (p_away_3w_sim - yA)^2 + (p_tie_3w_sim - yT)^2),
      Brier3_cal   = mean((H_cal - yH)^2 + (A_cal - yA)^2 + (T_cal - yT)^2),
      LogLoss3_raw = .logloss3(p_home_3w_sim, p_away_3w_sim, p_tie_3w_sim, yH, yA, yT),
      LogLoss3_cal = .logloss3(H_cal, A_cal, T_cal, yH, yA, yT),
      .groups = "drop"
    )


  overall <- by_week %>%
    dplyr::mutate(.w = n_games) %>%
    dplyr::summarise(
      Brier2_raw   = stats::weighted.mean(Brier2_raw,   .w, na.rm = TRUE),
      Brier2_cal   = stats::weighted.mean(Brier2_cal,   .w, na.rm = TRUE),
      LogLoss2_raw = stats::weighted.mean(LogLoss2_raw, .w, na.rm = TRUE),
      LogLoss2_cal = stats::weighted.mean(LogLoss2_cal, .w, na.rm = TRUE),
      Brier3_raw   = stats::weighted.mean(Brier3_raw,   .w, na.rm = TRUE),
      Brier3_cal   = stats::weighted.mean(Brier3_cal,   .w, na.rm = TRUE),
      LogLoss3_raw = stats::weighted.mean(LogLoss3_raw, .w, na.rm = TRUE),
      LogLoss3_cal = stats::weighted.mean(LogLoss3_cal, .w, na.rm = TRUE),
      n_games      = sum(n_games),
      n_weeks      = dplyr::n()
    )

  per_game <- sims %>%
    dplyr::transmute(
      season, week, game_id,
      p2_raw, p2_cal,
      pH_raw = p_home_3w_sim, pA_raw = p_away_3w_sim, pT_raw = p_tie_3w_sim,
      pH_cal = H_cal,          pA_cal = A_cal,          pT_cal = T_cal,
      p_blend = NA_real_,
      y2 = y_home, y3
    )

  list(overall = overall, by_week = by_week, per_game = per_game)
}

# Score a single completed (season, week)
score_one_week <- function(season, week, trials = 40000L) {
  sim <- week_inputs_and_sim_2w(season, week, n_trials = trials)
  if (!nrow(sim)) return(NULL)

  # 2-way: raw + isotonic
  sim <- sim %>%
    dplyr::mutate(
      p2_raw = p_home_2w_sim,
      p2_cal = .clamp01(map_iso(p_home_2w_sim))
    )

  # 3-way: raw + multinomial cal (falls back to raw if cal3 missing)
  cal3_vec <- function(ph, pa, pt) {
    out <- calibrate_3way(ph, pa, pt)  # returns named H/A/T
    c(H = as.numeric(out["H"]), A = as.numeric(out["A"]), T = as.numeric(out["T"]))
  }
  cald <- purrr::pmap_dfr(
    list(sim$p_home_3w_sim, sim$p_away_3w_sim, sim$p_tie_3w_sim),
    ~{ v <- cal3_vec(..1, ..2, ..3); tibble::tibble(H_cal = v["H"], A_cal = v["A"], T_cal = v["T"]) }
  )
  sim <- dplyr::bind_cols(sim, cald)

  # outcomes
  y2 <- sim$y_home
  yH <- as.integer(sim$y3 == "H")
  yA <- as.integer(sim$y3 == "A")
  yT <- as.integer(sim$y3 == "T")

  # scores
  brier2_raw <- mean((sim$p2_raw - y2)^2, na.rm = TRUE)
  brier2_cal <- mean((sim$p2_cal - y2)^2, na.rm = TRUE)
  ll2_raw    <- .logloss2(sim$p2_raw, y2)
  ll2_cal    <- .logloss2(sim$p2_cal, y2)

  brier3_raw <- mean((sim$p_home_3w_sim - yH)^2 +
                       (sim$p_away_3w_sim - yA)^2 +
                       (sim$p_tie_3w_sim  - yT)^2, na.rm = TRUE)
  brier3_cal <- mean((sim$H_cal - yH)^2 +
                       (sim$A_cal - yA)^2 +
                       (sim$T_cal - yT)^2, na.rm = TRUE)
  ll3_raw <- .logloss3(sim$p_home_3w_sim, sim$p_away_3w_sim, sim$p_tie_3w_sim, yH, yA, yT)
  ll3_cal    <- .logloss3(sim$H_cal, sim$A_cal, sim$T_cal, yH, yA, yT)


  list(
    week_scores = tibble::tibble(
      season = season, week = week, n_games = nrow(sim),
      Brier2_raw = brier2_raw, Brier2_cal = brier2_cal,
      LogLoss2_raw = ll2_raw, LogLoss2_cal = ll2_cal,
      Brier3_raw = brier3_raw, Brier3_cal = brier3_cal,
      LogLoss3_raw = ll3_raw, LogLoss3_cal = ll3_cal
    ),
    per_game = sim %>%
      dplyr::transmute(
        season = !!season, week = !!week, game_id,
        p2_raw, p2_cal,
        pH_raw = p_home_3w_sim, pA_raw = p_away_3w_sim, pT_raw = p_tie_3w_sim,
        pH_cal = H_cal, pA_cal = A_cal, pT_cal = T_cal,
        p_blend = NA_real_,
        y2 = y2, y3 = y3
      )
  )
}

# Score multiple weeks
# examples:
#   res <- score_weeks(start_season = SEASON, end_season = SEASON)     # this season, completed weeks
#   res <- score_weeks(start_season = SEASON-8, end_season = SEASON-1)  # last 8 seasons
#   res <- score_weeks(start_season = 2017, end_season = SEASON-1, weeks = 1:18)
score_weeks <- function(start_season, end_season, weeks = NULL, trials = 40000L) {
  # disk cache (skip heavy backtest if same knobs)
  .key  <- score_cache_key(start_season, end_season, weeks, trials, SEED, RHO_SCORE)
  .path <- file.path(.score_cache_dir, paste0(.key, ".rds"))
  if (file.exists(.path)) return(readRDS(.path))

  out_w   <- list()
  out_pg  <- list()
  idx     <- 0L

  seasons <- seq.int(start_season, end_season)
  for (s in seasons) {
    wk_avail <- sched %>%
      dplyr::filter(.data$season == s, .data$game_type == "REG") %>%
      dplyr::distinct(week) %>% dplyr::pull(week) %>% sort()
    wk_list <- if (is.null(weeks)) wk_avail else intersect(wk_avail, weeks)

    for (w in wk_list) {
      ans <- score_one_week(s, w, trials = trials)
      if (is.null(ans)) next
      idx <- idx + 1L
      out_w[[idx]]  <- ans$week_scores
      out_pg[[idx]] <- ans$per_game
    }
  }

  by_week  <- dplyr::bind_rows(out_w)
  per_game <- dplyr::bind_rows(out_pg)

  overall <- by_week %>%
    dplyr::mutate(.w = n_games) %>%  # preserve vector weights
    dplyr::summarise(
      Brier2_raw   = stats::weighted.mean(Brier2_raw,   .w, na.rm = TRUE),
      Brier2_cal   = stats::weighted.mean(Brier2_cal,   .w, na.rm = TRUE),
      LogLoss2_raw = stats::weighted.mean(LogLoss2_raw, .w, na.rm = TRUE),
      LogLoss2_cal = stats::weighted.mean(LogLoss2_cal, .w, na.rm = TRUE),
      Brier3_raw   = stats::weighted.mean(Brier3_raw,   .w, na.rm = TRUE),
      Brier3_cal   = stats::weighted.mean(Brier3_cal,   .w, na.rm = TRUE),
      LogLoss3_raw = stats::weighted.mean(LogLoss3_raw, .w, na.rm = TRUE),
      LogLoss3_cal = stats::weighted.mean(LogLoss3_cal, .w, na.rm = TRUE),
      n_games      = sum(n_games),
      n_weeks      = dplyr::n()
    )


  ans <- list(overall = overall, by_week = by_week, per_game = per_game)
  saveRDS(ans, .path)
  return(ans)
}

# ===================== BLEND MODEL (market + model) ===========================
# Trains out-of-sample by season on the last ~8 completed seasons, then applies
# the blend to your CURRENT `final` slate. Self-contained; no dependency on your
# earlier market/Brier code.

# ---- helpers (local, safe) ----
.clp <- function(x, eps=1e-12) pmin(pmax(x, eps), 1-eps)
.lgt <- function(p) log(p/(1-p))
.inv <- function(z) 1/(1+exp(-z))
# Note: american_to_probability() is defined in NFLmarket.R (sourced above)

align_blend_with_margin <- function(p_blend,
                                    margin_mean,
                                    margin_median,
                                    margin_sd_eff,
                                    home_mean_pts,
                                    away_mean_pts,
                                    home_median_pts,
                                    away_median_pts,
                                    influence_scale = 1.65,
                                    coinflip_tol = 0.15) {
  p_blend <- .clp(p_blend)

  sd_eff <- ifelse(is.finite(margin_sd_eff) & margin_sd_eff > 0, margin_sd_eff, NA_real_)
  score_mean <- home_mean_pts - away_mean_pts
  score_median <- home_median_pts - away_median_pts

  score_center <- 0.5 * (score_mean + score_median)
  margin_center <- 0.5 * (margin_mean + margin_median)

  use_margin <- !is.finite(score_center) & is.finite(margin_center)
  score_center[use_margin] <- margin_center[use_margin]

  ok <- is.finite(score_center) & is.finite(sd_eff)
  if (!any(ok)) {
    return(p_blend)
  }

  agreement <- ifelse(
    is.finite(score_mean) & is.finite(score_median) &
      (abs(score_mean) + abs(score_median)) > 0,
    1 - pmin(1, abs(score_mean - score_median) /
               (abs(score_mean) + abs(score_median))),
    1
  )

  z_score <- score_center[ok] / sd_eff[ok]
  p_target <- stats::pnorm(z_score)
  p_target <- .clp(p_target)

  influence <- abs(z_score) / influence_scale
  influence <- pmin(pmax(influence, 0), 1)
  influence <- pmax(influence, 0.25 * agreement[ok])
  influence <- pmin(influence, agreement[ok])

  same_scores <- is.finite(score_center) & abs(score_center) <= coinflip_tol &
    is.finite(agreement) & agreement > 0.75
  if (any(same_scores & ok, na.rm = TRUE)) {
    idx <- which(ok)[which(same_scores[ok])]
    influence[idx] <- 1
    p_target[idx] <- 0.5
  }

  adj_logit <- (1 - influence) * .lgt(p_blend[ok]) + influence * .lgt(p_target)
  p_adj <- stats::plogis(adj_logit)

  out <- p_blend
  out[ok] <- .clp(p_adj)
  out
}

fit_spread_to_prob <- function(df) {
  sp <- .pick_col2(df, c("close_spread","spread_close","home_spread_close",
                         "spread_line","spread","home_spread","spread_favorite"))
  hp <- .pick_col2(df, c("home_score","home_points","score_home","home_pts"))
  ap <- .pick_col2(df, c("away_score","away_points","score_away","away_pts"))
  if (is.na(sp) || is.na(hp) || is.na(ap)) return(NULL)
  d <- df %>%
    dplyr::filter(is.finite(suppressWarnings(as.numeric(.data[[sp]]))),
                  is.finite(.data[[hp]]), is.finite(.data[[ap]])) %>%
    dplyr::transmute(
      spread = suppressWarnings(as.numeric(.data[[sp]])),
      y      = as.integer(.data[[hp]] > .data[[ap]])
    )
  if (nrow(d) < 200) return(NULL)
  suppressWarnings(
    stats::glm(
      y ~ poly(spread, 2, raw = TRUE),
      family  = binomial(),
      data    = d,
      control = list(maxit = 50)
    )
  )
}

fit_spread_total_to_prob <- function(df) {
  sp <- .pick_col2(df, c("close_spread","spread_close","home_spread_close",
                         "spread_line","spread","home_spread","spread_favorite"))
  tot <- .pick_col2(df, c("total_close","close_total","total","total_points"))
  hp  <- .pick_col2(df, c("home_score","home_points","score_home","home_pts"))
  ap  <- .pick_col2(df, c("away_score","away_points","score_away","away_pts"))
  if (is.na(sp) || is.na(tot) || is.na(hp) || is.na(ap)) return(NULL)

  d <- df %>%
    dplyr::transmute(
      spread = suppressWarnings(as.numeric(.data[[sp]])),
      total  = suppressWarnings(as.numeric(.data[[tot]])),
      y      = as.integer(.data[[hp]] > .data[[ap]])
    ) %>%
    dplyr::filter(is.finite(spread), is.finite(total))

  if (nrow(d) < 500) return(NULL)

  suppressWarnings(
    stats::glm(
      y ~ spread + total + I(spread*total) + I(spread^2) + I(total^2),
      family  = binomial(),
      data    = d,
      control = list(maxit = 50)
    )
  )
}

spread_total_map <- fit_spread_total_to_prob(sched %>% dplyr::filter(game_type %in% c("REG","Regular")))
map_spread_total_prob <- function(sp, tot) {
  sp <- suppressWarnings(as.numeric(sp)); tot <- suppressWarnings(as.numeric(tot))
  if (is.null(spread_total_map)) {
    # fallback to your simple map or normal-approx default (13.86 = Historical NFL margin standard deviation)
    if (is.null(spread_map)) pnorm(-sp/13.86) else
      .clp(as.numeric(predict(spread_map, newdata = data.frame(spread = sp), type = "response")))
  } else {
    .clp(as.numeric(predict(spread_total_map,
                            newdata = data.frame(spread = sp, total = tot),
                            type = "response")))
  }
}


# Note: map_spread_prob defined earlier at line 5634 (simple fallback version)
# Note: market_probs_from_sched defined earlier at line 5645 (must be before first use)

spread_map <- fit_spread_to_prob(sched %>% dplyr::filter(game_type %in% c("REG","Regular")))

# DUPLICATE REMOVED - map_spread_prob now defined at line 5634

# DUPLICATE REMOVED - market_probs_from_sched now defined at line 5645
# Keeping function definition commented for reference:
market_probs_from_sched_DUPLICATE <- function(sched_df) {
  .clp <- function(x, eps = 1e-12) pmin(pmax(x, eps), 1 - eps)
  .pick <- function(df, cands) {
    nm <- intersect(cands, names(df))
    if (length(nm)) nm[1] else NA_character_
  }
  american_to_probability <- function(odds) {
    ifelse(
      is.na(odds) | odds == 0, NA_real_,
      ifelse(odds < 0, (-odds)/((-odds) + 100), 100/(odds + 100))
    )
  }

  base <- sched_df %>%
    dplyr::filter(!is.na(game_id)) %>%
    dplyr::transmute(game_id, season, week) %>%
    dplyr::distinct()

  if (!nrow(base)) {
    return(tibble::tibble(game_id = character(), season = integer(), week = integer(), p_home_mkt_2w = numeric()))
  }

  ml_home <- .pick(sched_df, c(
    "home_ml_close", "ml_home_close", "moneyline_home_close", "home_moneyline_close",
    "home_ml", "ml_home", "moneyline_home", "home_moneyline"
  ))
  ml_away <- .pick(sched_df, c(
    "away_ml_close", "ml_away_close", "moneyline_away_close", "away_moneyline_close",
    "away_ml", "ml_away", "moneyline_away", "away_moneyline"
  ))
  sp_col <- .pick(sched_df, c(
    "close_spread", "spread_close", "home_spread_close",
    "spread_line", "spread", "home_spread", "spread_favorite"
  ))

  ml_tbl <- tibble::tibble(
    game_id = character(), season = integer(), week = integer(), p_home_mkt_2w_ml = numeric()
  )
  if (!is.na(ml_home) && !is.na(ml_away)) {
    ml_tbl <- sched_df %>%
      dplyr::transmute(
        game_id, season, week,
        p_home_raw = american_to_probability(suppressWarnings(as.numeric(.data[[ml_home]]))),
        p_away_raw = american_to_probability(suppressWarnings(as.numeric(.data[[ml_away]])))
      ) %>%
      dplyr::mutate(
        den = p_home_raw + p_away_raw,
        p_home_mkt_2w_ml = .clp(ifelse(is.finite(den) & den > 0, p_home_raw/den, NA_real_))
      ) %>%
      dplyr::filter(is.finite(p_home_mkt_2w_ml)) %>%
      dplyr::select(game_id, season, week, p_home_mkt_2w_ml) %>%
      dplyr::distinct()
  }

  spread_tbl <- tibble::tibble(
    game_id = character(), season = integer(), week = integer(), p_home_mkt_2w_spread = numeric()
  )
  if (!is.na(sp_col)) {
    spread_tbl <- sched_df %>%
      dplyr::transmute(
        game_id, season, week,
        home_spread = suppressWarnings(as.numeric(.data[[sp_col]]))
      ) %>%
      dplyr::filter(is.finite(home_spread)) %>%
      dplyr::mutate(p_home_mkt_2w_spread = .clp(map_spread_prob(home_spread))) %>%
      dplyr::filter(is.finite(p_home_mkt_2w_spread)) %>%
      dplyr::select(game_id, season, week, p_home_mkt_2w_spread) %>%
      dplyr::distinct()
  }

  out <- base %>%
    dplyr::left_join(ml_tbl, by = c("game_id", "season", "week")) %>%
    dplyr::left_join(spread_tbl, by = c("game_id", "season", "week")) %>%
    dplyr::mutate(
      p_home_mkt_2w = dplyr::coalesce(p_home_mkt_2w_ml, p_home_mkt_2w_spread)
    ) %>%
    dplyr::select(game_id, season, week, p_home_mkt_2w)

  if (!any(is.finite(out$p_home_mkt_2w))) {
    message(
      "market_probs_from_sched(): no usable closing moneyline or spread columns found; returning NA probabilities."
    )
  }

  out
}

# ---- assemble historical training set (OOS by season) ----
# Use the last 8 completed seasons (change 8 to a different window if you like)
seasons_all <- sort(unique(sched$season[sched$game_type %in% c("REG","Regular")]))
seasons_hist <- seasons_all[seasons_all < max(seasons_all, na.rm = TRUE)]
seasons_hist <- tail(seasons_hist, 8)

# outcomes from schedules (use the score columns you already detected earlier)
home_pts_col <- .pick_col2(sched, c("home_score","home_points","score_home","home_pts"))
away_pts_col <- .pick_col2(sched, c("away_score","away_points","score_away","away_pts"))
if (is.na(home_pts_col) || is.na(away_pts_col)) {
  stop("Couldn't find score columns in `sched`.")
}

outcomes_hist <- sched %>%
  dplyr::filter(season %in% seasons_hist, game_type %in% c("REG","Regular")) %>%
  dplyr::transmute(game_id, season, week,
                   y2 = as.integer(.data[[home_pts_col]] > .data[[away_pts_col]])) %>%
  tibble::as_tibble() %>%
  dplyr::filter(stats::complete.cases(dplyr::across(dplyr::all_of(c("game_id", "season", "week"))))) %>%
  dplyr::filter(!is.na(y2))

outcomes_hist <- collapse_by_keys_strict(
  outcomes_hist,
  c("game_id", "season", "week"),
  label = "Historical outcomes"
)

# your calibrated model 2-way prob history for those seasons
preds_hist <- if (exists("res") && "per_game" %in% names(res)) {
  res$per_game %>%
    dplyr::filter(season %in% seasons_hist) %>%
    dplyr::transmute(game_id, season, week, p_model = .clp(p2_cal))
} else if (exists("calib_sim_df")) {
  # CRITICAL FIX: Ensure game_id, season, week exist in calib_sim_df before filtering
  # If calib_sim_df doesn't have season/week, join from sched first
  temp_df <- if ("season" %in% names(calib_sim_df) && "week" %in% names(calib_sim_df)) {
    calib_sim_df
  } else {
    calib_sim_df %>%
      dplyr::left_join(
        sched %>% dplyr::select(game_id, season, week),
        by = "game_id"
      )
  }

  temp_df %>%
    dplyr::filter(season %in% seasons_hist) %>%
    dplyr::transmute(
      game_id, season, week,
      p_model = .clp(map_iso(p_home_2w_sim))   # use your cached sim+isotonic
    )
} else {
  stop("Need either `res$per_game` or `calib_sim_df` in scope for preds_hist.")
}

preds_hist <- preds_hist %>%
  tibble::as_tibble() %>%
  dplyr::filter(stats::complete.cases(dplyr::across(dplyr::all_of(c("game_id", "season", "week"))))) %>%
  dplyr::filter(is.finite(p_model))

preds_hist <- collapse_by_keys_strict(
  preds_hist,
  c("game_id", "season", "week"),
  label = "Historical model probabilities"
)

# market probs for those same games
mkt_hist <- market_probs_from_sched(
  sched %>% dplyr::filter(season %in% seasons_hist, game_type %in% c("REG","Regular"))
)

mkt_hist <- mkt_hist %>%
  tibble::as_tibble() %>%
  dplyr::filter(stats::complete.cases(dplyr::across(dplyr::all_of(c("game_id", "season", "week"))))) %>%
  dplyr::filter(is.finite(p_home_mkt_2w))

mkt_hist <- collapse_by_keys_strict(
  mkt_hist,
  c("game_id", "season", "week"),
  label = "Historical market probabilities"
)

# Pick the right columns from sched
date_col <- .pick_col2(sched, c("gameday","game_date","game_datetime","game_time","kickoff","start_time","date"))
home_col <- .pick_col2(sched, c("home_team","home","team_home"))
away_col <- .pick_col2(sched, c("away_team","away","team_away"))
stopifnot(!is.na(date_col), !is.na(home_col), !is.na(away_col))


#Add rest features to schedule
rest_features <- sched %>%
  dplyr::filter(game_type %in% c("REG","Regular")) %>%
  dplyr::transmute(
    game_id,
    date = as.Date(.data[[date_col]]),
    home_team = .data[[home_col]],
    away_team = .data[[away_col]]
  ) %>%
  tidyr::pivot_longer(
    c(home_team, away_team),
    names_to = "ha",
    values_to = "team"
  ) %>%
  dplyr::mutate(ha = ifelse(ha == "home_team","home","away")) %>%
  dplyr::arrange(team, date) %>%
  dplyr::group_by(team) %>%
  dplyr::mutate(rest_days = as.integer(difftime(date, dplyr::lag(date), units = "days"))) %>%
  dplyr::ungroup() %>%
  tidyr::pivot_wider(
    id_cols = game_id,
    names_from = ha,
    values_from = c(team, rest_days),
    names_vary = "slowest"
  ) %>%
  dplyr::transmute(
    game_id,
    home_rest = rest_days_home,
    away_rest = rest_days_away,
    short_week_home = as.integer(home_rest <= 6),
    short_week_away = as.integer(away_rest <= 6),
    bye_home        = as.integer(home_rest >= 13),
    bye_away        = as.integer(away_rest >= 13)
  )

roof_col <- .pick_col2(sched, c("roof","stadium_roof","venue_roof"))
wind_col <- .pick_col2(sched, c("wind","wind_mph","weather_wind"))
outdoor_flag <- function(x) as.integer(tolower(x) %in% c("outdoors","open","retractable"))
wind_num <- function(x) { v <- suppressWarnings(as.numeric(x)); ifelse(is.finite(v), v, NA_real_) }

ctx <- sched %>%
  dplyr::transmute(
    game_id, season, week,
    home_team = .data[[home_col]],
    away_team = .data[[away_col]],
    is_outdoors = if (!is.na(roof_col)) outdoor_flag(.data[[roof_col]]) else NA_integer_,
    wind_mph    = if (!is.na(wind_col)) wind_num(.data[[wind_col]]) else NA_real_,
    high_wind   = as.integer(is.finite(wind_mph) & wind_mph >= 12)
  ) %>%
  dplyr::left_join(rest_features, by = "game_id")

# Add division/conference indicators to historical context
if (nrow(team_info) > 0) {
  ctx <- ctx %>%
    dplyr::left_join(team_info %>% dplyr::rename(home_division = team_division, home_conf = team_conf),
                     by = c("home_team" = "team")) %>%
    dplyr::left_join(team_info %>% dplyr::rename(away_division = team_division, away_conf = team_conf),
                     by = c("away_team" = "team")) %>%
    dplyr::mutate(
      division_game = as.integer(!is.na(home_division) & !is.na(away_division) & home_division == away_division),
      conference_game = as.integer(!is.na(home_conf) & !is.na(away_conf) & home_conf == away_conf)
    ) %>%
    dplyr::select(-home_division, -away_division, -home_conf, -away_conf)
} else {
  ctx <- ctx %>%
    dplyr::mutate(division_game = 0L, conference_game = 0L)
}

if (nrow(inj_hist_features)) {
  inj_home <- inj_hist_features %>%
    dplyr::rename(home_team = team) %>%
    dplyr::rename_with(~ paste0("home_", .x),
                       dplyr::all_of(setdiff(names(.), c("season", "week", "home_team"))))
  inj_away <- inj_hist_features %>%
    dplyr::rename(away_team = team) %>%
    dplyr::rename_with(~ paste0("away_", .x),
                       dplyr::all_of(setdiff(names(.), c("season", "week", "away_team"))))

  ctx <- ctx %>%
    dplyr::left_join(inj_home, by = c("season", "week", "home_team")) %>%
    dplyr::left_join(inj_away, by = c("season", "week", "away_team"))
}

# ---- Team strength features from nflreadr::load_team_stats -------------------
safe_team_stats <- function(stat_type, seasons) {
  tryCatch({
    nflreadr::load_team_stats(stat_type = stat_type, seasons = seasons)
  }, error = function(e) NULL)
}

extract_team_metric <- function(df, metric_candidates, label) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)

  season_col <- .pick_col2(df, c("season", "season_year", "year"))
  team_col   <- .pick_col2(df, c("team", "team_abbr", "club_code", "team_code"))
  metric_col <- .pick_col2(df, metric_candidates)
  stype_col  <- .pick_col2(df, c("season_type", "season_type_name", "type"))
  week_col   <- .pick_col2(df, c("week", "game_week"))

  if (is.na(season_col) || is.na(team_col) || is.na(metric_col)) return(NULL)

  out <- df %>%
    dplyr::mutate(
      season = suppressWarnings(as.integer(.data[[season_col]])),
      team   = toupper(as.character(.data[[team_col]])),
      metric = suppressWarnings(as.numeric(.data[[metric_col]]))
    ) %>%
    dplyr::filter(!is.na(season), nzchar(team), is.finite(metric))

  if (!is.na(stype_col)) {
    out <- out %>%
      dplyr::filter(.data[[stype_col]] %in% c("REG", "Regular", "REGULAR SEASON"))
  }

  if (!is.na(week_col) && any(is.finite(out[[week_col]]))) {
    out <- out %>%
      dplyr::group_by(season, team) %>%
      dplyr::filter(.data[[week_col]] == max(.data[[week_col]], na.rm = TRUE)) %>%
      dplyr::ungroup()
  } else {
    out <- out %>% dplyr::distinct(season, team, .keep_all = TRUE)
  }

  out %>%
    dplyr::transmute(season, team, !!label := metric)
}

derive_rate_metric <- function(df, numer_candidates, denom_candidates, label, multiplier = 1) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)

  numer_col <- .pick_col2(df, numer_candidates)
  denom_col <- .pick_col2(df, denom_candidates)
  if (is.na(numer_col) || is.na(denom_col)) return(df)

  numer <- suppressWarnings(as.numeric(df[[numer_col]]))
  denom <- suppressWarnings(as.numeric(df[[denom_col]]))

  valid <- is.finite(numer) & is.finite(denom) & abs(denom) > 0
  out <- rep(NA_real_, length(numer))
  out[valid] <- (numer[valid] / denom[valid]) * multiplier

  df[[label]] <- out
  df
}

team_stats_off <- safe_team_stats("offense", unique(sched$season))
team_stats_def <- safe_team_stats("defense", unique(sched$season))

team_stats_off <- team_stats_off %>%
  derive_rate_metric(c("points", "points_for", "points_scored"),
                     c("drives", "offensive_drives", "drive"),
                     "points_per_drive_calc") %>%
  derive_rate_metric(c("turnovers", "turnover", "turnovers_lost"),
                     c("drives", "offensive_drives", "drive"),
                     "turnovers_per_drive_calc")

team_stats_def <- team_stats_def %>%
  derive_rate_metric(c("points_allowed", "points_against", "points"),
                     c("drives", "defensive_drives", "drive"),
                     "points_allowed_per_drive_calc") %>%
  derive_rate_metric(c("takeaways", "turnovers_forced", "turnovers"),
                     c("drives", "defensive_drives", "drive"),
                     "takeaways_per_drive_calc")

team_strength_tbl <- list(
  extract_team_metric(team_stats_off, c("epa_per_play", "epa", "epa_per_pass"), "off_epa"),
  extract_team_metric(team_stats_off, c("success_rate", "series_success_rate", "sr"), "off_sr"),
  extract_team_metric(team_stats_def, c("epa_per_play", "epa", "epa_per_pass"), "def_epa"),
  extract_team_metric(team_stats_def, c("success_rate", "series_success_rate", "sr"), "def_sr"),
  extract_team_metric(team_stats_off, c("epa_per_pass", "pass_epa", "passing_epa", "epa_per_pass_play"), "off_pass_epa"),
  extract_team_metric(team_stats_def, c("epa_per_pass", "pass_epa", "passing_epa", "epa_per_pass_play"), "def_pass_epa"),
  extract_team_metric(team_stats_off, c("epa_per_rush", "rush_epa", "rushing_epa", "epa_per_rush_play"), "off_rush_epa"),
  extract_team_metric(team_stats_def, c("epa_per_rush", "rush_epa", "rushing_epa", "epa_per_rush_play"), "def_rush_epa"),
  extract_team_metric(team_stats_off, c("success_rate_pass", "pass_success_rate", "passing_success_rate"), "off_pass_sr"),
  extract_team_metric(team_stats_def, c("success_rate_pass", "pass_success_rate", "passing_success_rate"), "def_pass_sr"),
  extract_team_metric(team_stats_off, c("success_rate_rush", "rush_success_rate", "rushing_success_rate"), "off_rush_sr"),
  extract_team_metric(team_stats_def, c("success_rate_rush", "rush_success_rate", "rushing_success_rate"), "def_rush_sr"),
  extract_team_metric(team_stats_def, c("pressure_rate", "pressures_per_dropback", "qb_hit_rate"), "def_pressure_rate"),
  extract_team_metric(team_stats_off, c("pressure_rate_allowed", "pressure_rate", "pressures_per_dropback"), "off_pressure_rate"),
  extract_team_metric(team_stats_off, c("points_per_drive_calc", "points_per_drive", "points_per_off_drive"), "off_ppd"),
  extract_team_metric(team_stats_def, c("points_allowed_per_drive_calc", "points_per_drive_allowed", "points_allowed_per_drive"), "def_ppd"),
  extract_team_metric(team_stats_off, c("explosive_rate", "explosive_play_rate", "explosive_pct"), "off_explosive_rate"),
  extract_team_metric(team_stats_def, c("explosive_rate", "explosive_play_rate", "explosive_pct"), "def_explosive_rate"),
  extract_team_metric(team_stats_off, c("turnovers_per_drive_calc", "turnovers_per_drive", "turnover_rate"), "off_turnover_rate"),
  extract_team_metric(team_stats_def, c("takeaways_per_drive_calc", "takeaways_per_drive", "takeaway_rate"), "def_takeaway_rate"),
  extract_team_metric(team_stats_def, c("success_rate", "series_success_rate", "sr"), "def_sr")
) %>%
  purrr::compact()

if (length(team_strength_tbl)) {
  team_strength_tbl <- purrr::reduce(team_strength_tbl, dplyr::full_join, by = c("season", "team"))

  rename_strength_cols <- function(df, prefix) {
    cols <- setdiff(names(df), c("season", "team", "home_team", "away_team"))
    cols <- cols[!startsWith(cols, "home_") & !startsWith(cols, "away_")]
    if (!length(cols)) return(df)
    dplyr::rename_with(df, ~ paste0(prefix, .x), dplyr::all_of(cols))
  }

  ctx_strength <- sched %>%
    dplyr::filter(game_type %in% c("REG", "Regular")) %>%
    dplyr::transmute(
      game_id, season, week,
      home_team = .data[[home_col]],
      away_team = .data[[away_col]]
    ) %>%
    dplyr::left_join(team_strength_tbl, by = c("season", "home_team" = "team")) %>%
    rename_strength_cols("home_") %>%
    dplyr::left_join(team_strength_tbl, by = c("season", "away_team" = "team")) %>%
    rename_strength_cols("away_")

  if (all(c("home_off_epa", "away_off_epa") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(off_epa_edge = home_off_epa - away_off_epa)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(off_epa_edge = NA_real_)
  }

  if (all(c("home_def_epa", "away_def_epa") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(def_epa_edge = away_def_epa - home_def_epa,
                    net_epa_edge = (home_off_epa - home_def_epa) - (away_off_epa - away_def_epa))
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(def_epa_edge = NA_real_, net_epa_edge = NA_real_)
  }

  if (all(c("home_off_sr", "away_off_sr") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(sr_edge = home_off_sr - away_off_sr)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(sr_edge = NA_real_)
  }

  if (all(c("home_off_pass_epa", "away_def_pass_epa") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(pass_epa_edge = home_off_pass_epa - away_def_pass_epa)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(pass_epa_edge = NA_real_)
  }

  if (all(c("home_off_rush_epa", "away_def_rush_epa") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(rush_epa_edge = home_off_rush_epa - away_def_rush_epa)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(rush_epa_edge = NA_real_)
  }

  if (all(c("home_off_pass_sr", "away_def_pass_sr") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(pass_sr_edge = home_off_pass_sr - away_def_pass_sr)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(pass_sr_edge = NA_real_)
  }

  if (all(c("home_off_rush_sr", "away_def_rush_sr") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(rush_sr_edge = home_off_rush_sr - away_def_rush_sr)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(rush_sr_edge = NA_real_)
  }

  if (all(c("home_off_pressure_rate", "away_def_pressure_rate") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(pressure_rate_diff = away_def_pressure_rate - home_off_pressure_rate)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(pressure_rate_diff = NA_real_)
  }

  if (all(c("home_off_ppd", "away_def_ppd") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(drive_finish_edge = home_off_ppd - away_def_ppd)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(drive_finish_edge = NA_real_)
  }

  if (all(c("home_def_ppd", "away_off_ppd") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(drive_prevention_edge = away_off_ppd - home_def_ppd)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(drive_prevention_edge = NA_real_)
  }

  if (all(c("home_off_explosive_rate", "away_def_explosive_rate") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(explosive_edge = home_off_explosive_rate - away_def_explosive_rate)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(explosive_edge = NA_real_)
  }

  if (all(c("home_def_explosive_rate", "away_off_explosive_rate") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(explosive_prevention_edge = away_off_explosive_rate - home_def_explosive_rate)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(explosive_prevention_edge = NA_real_)
  }

  if (all(c("home_off_turnover_rate", "away_def_takeaway_rate") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(turnover_pressure_edge = away_def_takeaway_rate - home_off_turnover_rate)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(turnover_pressure_edge = NA_real_)
  }

  if (all(c("home_def_takeaway_rate", "away_off_turnover_rate") %in% names(ctx_strength))) {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(ball_security_edge = home_def_takeaway_rate - away_off_turnover_rate)
  } else {
    ctx_strength <- ctx_strength %>%
      dplyr::mutate(ball_security_edge = NA_real_)
  }

  ctx <- ctx %>%
    dplyr::left_join(ctx_strength, by = c("game_id", "season", "week"))
}


# align
comp0 <- preds_hist %>%
  dplyr::inner_join(mkt_hist,    by = c("game_id","season","week")) %>%
  dplyr::inner_join(outcomes_hist, by = c("game_id","season","week")) %>%
  dplyr::left_join(ctx, by = c("game_id","season","week")) %>%
  dplyr::mutate(p_model = .clp(p_model), p_mkt = .clp(p_home_mkt_2w))

# ---- fit OOS blend by week (ridge + meta-features + recency weights) ----
suppressWarnings(suppressMessages(require(glmnet)))

.clp <- function(x, eps=1e-12) pmin(pmax(x, eps), 1-eps)
.lgt <- function(p) log(p/(1-p))
.inv <- function(z) 1/(1+exp(-z))

.pick_open <- function(df, cands) { nm <- intersect(cands, names(df)); if (length(nm)) nm[1] else NA_character_ }

# Try to add open/close deltas when present
blend_design <- function(df) {
  zm <- .lgt(df$p_model); zk <- .lgt(df$p_mkt)
  diff <- zm - zk

  # optional: open lines
  sp_open  <- .pick_open(df, c("spread_open","open_spread","home_spread_open"))
  sp_close <- .pick_open(df, c("close_spread","spread_close","home_spread_close","spread_line","spread"))
  mlh_open <- .pick_open(df, c("home_ml_open","ml_home_open","home_moneyline_open"))
  mlh_close<- .pick_open(df, c("home_ml_close","ml_home_close","home_moneyline_close","home_ml"))

  d_sp <- if (!is.na(sp_open) && !is.na(sp_close)) suppressWarnings(as.numeric(df[[sp_close]]) - as.numeric(df[[sp_open]])) else NA_real_
  d_ml <- if (!is.na(mlh_open) && !is.na(mlh_close)) {
    ph_o <- american_to_probability(suppressWarnings(as.numeric(df[[mlh_open]])))
    ph_c <- american_to_probability(suppressWarnings(as.numeric(df[[mlh_close]])))
    (ph_c - ph_o)
  } else NA_real_

  design <- cbind(
    lgt_model = zm,
    lgt_mkt   = zk,
    diff      = diff,
    adiff     = abs(diff),
    diff2     = diff^2,
    d_spread  = ifelse(is.finite(d_sp), d_sp, 0),
    d_mlprob  = ifelse(is.finite(d_ml), d_ml, 0)
  )

  # optional contextual features if present on the data frame
  add_feat <- function(mat, col, values) {
    mat <- cbind(mat, as.numeric(values))
    colnames(mat)[ncol(mat)] <- col
    mat
  }

  optional_cols <- intersect(c(
    "short_week_home", "short_week_away", "bye_home", "bye_away",
    "is_outdoors", "high_wind", "wind_mph",
    "home_off_epa", "away_off_epa", "home_def_epa", "away_def_epa",
    "home_off_sr", "away_off_sr", "home_def_sr", "away_def_sr",
    "home_off_pass_epa", "away_off_pass_epa", "home_def_pass_epa", "away_def_pass_epa",
    "home_off_rush_epa", "away_off_rush_epa", "home_def_rush_epa", "away_def_rush_epa",
    "home_off_pass_sr", "away_off_pass_sr", "home_def_pass_sr", "away_def_pass_sr",
    "home_off_rush_sr", "away_off_rush_sr", "home_def_rush_sr", "away_def_rush_sr",
    "home_off_pressure_rate", "away_off_pressure_rate", "home_def_pressure_rate", "away_def_pressure_rate",
    "home_off_ppd", "away_off_ppd", "home_def_ppd", "away_def_ppd",
    "home_off_explosive_rate", "away_off_explosive_rate", "home_def_explosive_rate", "away_def_explosive_rate",
    "home_off_turnover_rate", "away_off_turnover_rate", "home_def_takeaway_rate", "away_def_takeaway_rate",
    "drive_finish_edge", "drive_prevention_edge", "explosive_edge", "explosive_prevention_edge",
    "turnover_pressure_edge", "ball_security_edge",
    "home_inj_off_pts", "away_inj_off_pts", "home_inj_def_pts", "away_inj_def_pts",
    "home_skill_avail_pen", "away_skill_avail_pen", "home_trench_avail_pen", "away_trench_avail_pen",
    "home_secondary_avail_pen", "away_secondary_avail_pen", "home_front7_avail_pen", "away_front7_avail_pen",
    "off_epa_edge", "def_epa_edge", "sr_edge", "net_epa_edge",
    "pass_epa_edge", "rush_epa_edge", "pass_sr_edge", "rush_sr_edge", "pressure_rate_diff",
    "off_epa_edge", "def_epa_edge", "sr_edge", "net_epa_edge",
    "division_game", "conference_game"
  ), names(df))

  for (col in optional_cols) {
    vals <- suppressWarnings(as.numeric(df[[col]]))
    vals[!is.finite(vals)] <- 0
    design <- add_feat(design, col, vals)
  }

  design
}

# weeks 'ago' from (S,W) for recency weights (approx 18 weeks/season)
weeks_ago <- function(season, week, S, W) (S - season) * 18 + (W - week)

make_calibrator <- function(method, p, y, weights = NULL) {
  method <- tolower(ifelse(is.null(method) || !nzchar(method), "isotonic", method))
  p <- .clp(p)
  y <- as.numeric(y)

  if (method == "beta") {
    df <- tibble::tibble(
      y = y,
      lp = log(p),
      l1mp = log1p(-p)
    )
    fit <- try(stats::glm(y ~ lp + l1mp, data = df, family = binomial(), weights = weights), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      return(function(newp) {
        newp <- .clp(newp)
        newdata <- tibble::tibble(lp = log(newp), l1mp = log1p(-newp))
        stats::predict(fit, newdata = newdata, type = "response") %>% .clp()
      })
    }
    method <- "isotonic"  # fallback if beta calibration failed
  }

  if (method == "logistic") {
    df <- tibble::tibble(y = y, z = stats::qlogis(p))
    fit <- try(stats::glm(y ~ poly(z, 2, raw = TRUE), family = binomial(), data = df, weights = weights), silent = TRUE)
    if (!inherits(fit, "try-error")) {
      return(function(newp) {
        newp <- .clp(newp)
        stats::predict(fit, newdata = tibble::tibble(z = stats::qlogis(newp)), type = "response") %>% .clp()
      })
    }
    method <- "isotonic"
  }

  # default isotonic regression calibrator with adaptive boundaries
  tb <- tibble::tibble(p = .clp(p), y = y) %>%
    dplyr::mutate(bin = pmin(pmax(floor(p * 100), 0), 100)) %>%
    dplyr::group_by(bin) %>%
    dplyr::summarise(x = mean(p), y = mean(y), .groups = "drop") %>%
    dplyr::arrange(x)
  # Adaptive boundaries: cushion 5% beyond observed range for better extrapolation
  y_min <- max(0.01, min(tb$y, na.rm = TRUE) * 0.95)
  y_max <- min(0.99, max(tb$y, na.rm = TRUE) * 1.05)
  iso <- stats::isoreg(c(0, tb$x, 1), c(y_min, tb$y, y_max))
  xs  <- iso$x[!duplicated(iso$x)]
  ys  <- iso$yf[!duplicated(iso$x)]
  function(newp) {
    newp <- .clp(newp)
    stats::approx(xs, ys, xout = newp, rule = 2)$y %>% .clp()
  }
}

# Note: weeks_ago function already defined above at line 6698

cw <- sched %>% dplyr::filter(game_type %in% c("REG","Regular")) %>%
  dplyr::arrange(season, week) %>% dplyr::distinct(season, week)

blend_oos <- purrr::map_dfr(seq_len(nrow(cw)), function(i){
  S <- cw$season[i]; W <- cw$week[i]
  test  <- comp0 %>% dplyr::filter(season == S, week == W)
  train <- comp0 %>% dplyr::filter((season < S) | (season == S & week < W))

  if (!nrow(test)) return(tibble::tibble())
  if (nrow(train) < 500) {
    test$p_blend <- test$p_mkt
    return(test)
  }

  Xtr <- blend_design(train)
  ytr <- as.numeric(train$y2)
  wtr <- 0.98 ^ pmax(0, weeks_ago(train$season, train$week, S, W))  # recency

  Xte <- blend_design(test)

  model_name <- tolower(ifelse(is.null(BLEND_META_MODEL) || !nzchar(BLEND_META_MODEL), "glmnet", BLEND_META_MODEL))
  alpha_val <- suppressWarnings(as.numeric(BLEND_ALPHA))
  if (!is.finite(alpha_val)) alpha_val <- 0.25
  alpha_val <- pmin(pmax(alpha_val, 0), 1)

  fit_glmnet <- function() {
    glmnet::cv.glmnet(Xtr, ytr, family = "binomial", alpha = alpha_val, weights = wtr, nfolds = 10)
  }

  p_raw_tr <- p_raw_te <- NULL

  if (model_name == "glm") {
    df_tr <- as.data.frame(Xtr)
    df_tr$y <- ytr
    glm_fit <- try(stats::glm(y ~ ., family = binomial(), data = df_tr, weights = wtr), silent = TRUE)
    if (!inherits(glm_fit, "try-error")) {
      p_raw_tr <- stats::predict(glm_fit, type = "response")
      p_raw_te <- stats::predict(glm_fit, newdata = as.data.frame(Xte), type = "response")
    } else {
      model_name <- "glmnet"  # fallback
    }
  }

  if (model_name != "glm") {
    cv <- fit_glmnet()
    p_raw_tr <- as.numeric(predict(cv, newx = Xtr, s = "lambda.min", type = "response"))
    p_raw_te <- as.numeric(predict(cv, newx = Xte, s = "lambda.min", type = "response"))
  }

  calibrator <- make_calibrator(CALIBRATION_METHOD, p_raw_tr, ytr, weights = wtr)
  test$p_blend <- calibrator(p_raw_te)
  test
})

blend_oos <- standardize_join_keys(blend_oos)

boot_week_ci <- function(df, B = 2000, seed = SEED) {
  if (!nrow(df)) {
    return(rep(NA_real_, 2L))
  }

  set.seed(seed)

  groups <- split(seq_len(nrow(df)), interaction(df$season, df$week, drop = TRUE, lex.order = TRUE))
  n <- length(groups)
  dif <- numeric(B)

  for (b in seq_len(B)) {
    idx <- sample.int(n, n, replace = TRUE)
    rows <- unlist(groups[idx], use.names = FALSE)
    dd <- df[rows, , drop = FALSE]
    dif[b] <- mean((dd$p_blend - dd$y2)^2) - mean((dd$p_mkt - dd$y2)^2)
  }

  stats::quantile(dif, c(0.025, 0.975))
}
ci_brier_diff <- boot_week_ci(blend_oos)
print(ci_brier_diff)



# quick sanity print (optional)
if (nrow(blend_oos)) {
  brier <- function(p,y) mean((p-y)^2)
  logl  <- function(p,y){p <- .clp(p); -mean(y*log(p)+(1-y)*log(1-p))}
  msg <- tibble::tibble(
    model_Brier2 = brier(blend_oos$p_model, blend_oos$y2),
    mkt_Brier2   = brier(blend_oos$p_mkt,   blend_oos$y2),
    blend_Brier2 = brier(blend_oos$p_blend, blend_oos$y2),
    model_LogL2  = logl(blend_oos$p_model, blend_oos$y2),
    mkt_LogL2    = logl(blend_oos$p_mkt,   blend_oos$y2),
    blend_LogL2  = logl(blend_oos$p_blend, blend_oos$y2),
    n_games      = nrow(blend_oos)
  )
  print(msg)
}
# --- attach blended probabilities to comp0 ---
comp_blend <- comp0 %>%
  dplyr::left_join(
    dplyr::select(blend_oos, game_id, season, week, p_blend),
    by = c("game_id","season","week")
  )

# Economic backtest (uses blended prob when available)
edge_thresh <- 0.02
kelly_frac  <- 0.25

econ_df <- comp_blend %>%
  dplyr::mutate(
    p_take = dplyr::if_else(is.finite(p_blend), p_blend, p_model),
    edge   = p_take - p_mkt,
    take_home = edge >= edge_thresh,
    price_home = p_mkt,               # devigged baseline
    # Kelly against a fair price; simple approx since we compare prob directly
    kelly = dplyr::if_else(take_home,
                           (p_take - (1 - p_take) * price_home/(1 - price_home)),
                           0
    ),
    stake = pmax(kelly, 0) * kelly_frac,
    ret   = dplyr::case_when(
      take_home & y2 == 1 ~ stake * ((1/price_home) - 1),
      take_home & y2 == 0 ~ -stake,
      TRUE ~ 0
    ),
    clv_logit = (.lgt(p_take) - .lgt(p_mkt))
  )


econ_summ <- econ_df %>%
  summarise(
    n_bets   = sum(take_home, na.rm = TRUE),
    hit      = mean(y2[take_home], na.rm = TRUE),
    mean_edge= mean(edge[take_home], na.rm = TRUE),
    clv      = mean(clv_logit[take_home], na.rm = TRUE),
    total_ret= sum(ret, na.rm = TRUE),
    avg_ret  = mean(ret, na.rm = TRUE)
  )
print(econ_summ)

# ---- train a *deployment* blend on latest K seasons (ridge) + isotonic; apply to `final` ----
suppressWarnings(suppressMessages(require(glmnet)))

make_mat <- function(df){
  zm <- .lgt(df$p_model); zk <- .lgt(df$p_mkt); diff <- zm - zk
  x <- cbind(
    lgt_model = zm,
    lgt_mkt   = zk,
    diff      = diff,
    adiff     = abs(diff),
    diff2     = diff^2
  )
  list(x = as.matrix(x), y = as.numeric(df$y2))
}

K <- 8
lastK <- tail(seasons_hist, K)
train_deploy <- comp0 %>% dplyr::filter(season %in% lastK)

fit_deploy <- NULL
map_blend  <- NULL

if (nrow(train_deploy) >= 500) {
  mm <- make_mat(train_deploy)

  # recency weights over the training window (weeks ago from end of window)
  endS <- max(train_deploy$season); endW <- max(train_deploy$week[train_deploy$season==endS])
  w    <- 0.98 ^ pmax(0, weeks_ago(train_deploy$season, train_deploy$week, endS, endW))

  cv <- cv.glmnet(mm$x, mm$y, family = "binomial", alpha = 0.25, weights = w, nfolds = 10)
  fit_deploy <- list(cv = cv, feat = colnames(mm$x))

  # isotonic on the blend scores (train side) with adaptive boundaries
  p_tr <- as.numeric(predict(cv, newx = mm$x, s = "lambda.min", type = "response"))
  tb <- tibble::tibble(p = .clp(p_tr), y = mm$y) %>%
    dplyr::mutate(bin = pmin(pmax(floor(p*100),0),100)) %>%
    dplyr::group_by(bin) %>% dplyr::summarise(x = mean(p), y = mean(y), .groups="drop") %>%
    dplyr::arrange(x)
  # Adaptive boundaries based on actual outcome range (more robust than hardcoded values)
  y_min <- max(0.01, min(tb$y, na.rm = TRUE) * 0.95)  # 5% cushion below min
  y_max <- min(0.99, max(tb$y, na.rm = TRUE) * 1.05)  # 5% cushion above max
  iso <- stats::isoreg(c(0, tb$x, 1), c(y_min, tb$y, y_max))
  xs  <- iso$x[!duplicated(iso$x)]
  ys  <- iso$yf[!duplicated(iso$x)]
  map_blend <- function(p){ p <- .clp(p); .clp(stats::approx(xs, ys, xout = p, rule = 2)$y) }
}

# ---- Apply to CURRENT slate (`final`) ----
# Note: Market data already loaded and joined above (lines 5630-5645)
# This ensures home_p_2w_mkt and away_p_2w_mkt are available for uncertainty calculations

stopifnot("home_p_2w_cal" %in% names(final))
stopifnot("home_p_2w_mkt" %in% names(final))

# Create model probability alias for blend prediction
final <- final %>%
  dplyr::mutate(
    home_p_2w_model = .clp(home_p_2w_cal)
  )


# 2) Predict blend only where market is available; else fall back to model
p_raw <- final$home_p_2w_model
if (!is.null(fit_deploy)) {
  mask <- is.finite(final$home_p_2w_mkt)
  if (any(mask)) {
    dfX <- tibble::tibble(
      p_model = .clp(final$home_p_2w_model[mask]),
      p_mkt   = .clp(final$home_p_2w_mkt[mask])
    )
    X <- blend_design(dfX)

    # force same columns/order as training
    need <- fit_deploy$feat
    miss <- setdiff(need, colnames(X))
    if (length(miss)) {
      for (m in miss) X[[m]] <- 0
    }
    X <- as.matrix(X[, need, drop = FALSE])

    p_raw[mask] <- as.numeric(
      predict(fit_deploy$cv, newx = X, s = "lambda.min", type = "response")
    )
  }
}


# 3) Isotonic-correct the blended prob (if we trained the map)
final$home_p_2w_blend_raw <- if (!is.null(map_blend)) map_blend(p_raw) else p_raw

final <- final %>%
  dplyr::mutate(
    home_p_2w_blend = align_blend_with_margin(
      home_p_2w_blend_raw,
      margin_mean,
      margin_median,
      margin_sd_eff,
      home_mean_pts,
      away_mean_pts,
      home_median_pts,
      away_median_pts
    ),
    home_win_prob_blend = .clp(home_p_2w_blend * (1 - tie_prob)),
    away_win_prob_blend = .clp((1 - tie_prob) - home_win_prob_blend),
    two_way_mass_blend  = pmax(1 - tie_prob, 1e-9),
    home_p_2w_blend     = .clp(home_win_prob_blend / two_way_mass_blend),
    away_p_2w_blend     = 1 - home_p_2w_blend,
    margin_blend        = dplyr::if_else(
      is.finite(margin_sd_eff) & margin_sd_eff > 0,
      stats::qnorm(.clp(home_p_2w_blend)) * margin_sd_eff,
      NA_real_
    ),
    total_median_blend  = dplyr::if_else(
      is.finite(total_median),
      total_median,
      away_median_pts + home_median_pts
    ),
    home_median_blend   = dplyr::if_else(
      is.finite(total_median_blend) & is.finite(margin_blend),
      0.5 * (total_median_blend + margin_blend),
      home_median_pts
    ),
    away_median_blend   = dplyr::if_else(
      is.finite(total_median_blend) & is.finite(margin_blend),
      0.5 * (total_median_blend - margin_blend),
      away_median_pts
    )
  )


message("Blend (ridge+iso) added: home_p_2w_blend/home_win_prob_blend/away_win_prob_blend")
# ---- END deployment ridge+isotonic block ----

# Prepare snapshot for the current slate's moneyline report before kicking off
# any backtesting so the HTML export can rely on the same sequentially derived
# probabilities the simulation just produced.
moneyline_report_inputs <- NULL
primary_report_tbl <- NULL

join_keys_html <- if (exists("PREDICTION_JOIN_KEYS", inherits = TRUE)) {
  PREDICTION_JOIN_KEYS
} else {
  c("game_id", "season", "week")
}

upcoming_snapshot <- build_snapshot_moneyline_result(
  final_df = final,
  season = SEASON,
  week = WEEK_TO_SIM,
  verbose = TRUE
)

if (nrow(upcoming_snapshot)) {
  upcoming_result <- list(comp = upcoming_snapshot)
  preview_tbl <- build_moneyline_comparison_table(
    market_comparison_result = upcoming_result,
    enriched_schedule = sched,
    join_keys = join_keys_html,
    vig = 0.10,
    verbose = TRUE
  )

  if (nrow(preview_tbl)) {
    moneyline_report_inputs <- list(
      comparison = upcoming_result,
      join_keys = join_keys_html,
      preview = preview_tbl
    )
    primary_report_tbl <- preview_tbl
    message(sprintf(
      "Prepared current-week moneyline snapshot for %d games.",
      nrow(preview_tbl)
    ))
  } else {
    message("Moneyline snapshot preview empty after schedule join; will fall back to backtest comparison if available.")
  }
} else {
  message("Moneyline snapshot skipped because no upcoming slate probabilities were available.")
}

# === BACKTEST to create `res` for market comparison (required by compare_to_market) ===
# Pick the window you want (last 8 completed seasons works well)
BACKTEST_TRIALS <- 8000L
res <- if (exists("calib_sim_df")) {
  score_weeks_fast(
    start_season = SEASON - 8,
    end_season   = SEASON - 1
  )
} else {
  score_weeks(
    start_season = SEASON - 8,
    end_season   = SEASON - 1,
    trials       = BACKTEST_TRIALS
  )
}

# === Evaluate *blended* probabilities vs market with paired week-block CIs ===
# We reuse your existing `compare_to_market()` which expects res$per_game$p2_cal.
# We create res_blend by overwriting p2_cal with the blended p for the same games.

if (exists("res") && exists("blend_oos") && nrow(blend_oos)) {
  join_keys <- if (exists("PREDICTION_JOIN_KEYS", inherits = TRUE)) PREDICTION_JOIN_KEYS else c("game_id", "season", "week")
  prob_col_candidates <- c("p2_cal", "home_p_2w_cal", "p2_home_cal", "home_p2w_cal")

  res_blend <- build_res_blend(
    res = res,
    blend_oos = blend_oos,
    join_keys = join_keys,
    prob_candidates = prob_col_candidates,
    verbose = TRUE
  )

  if (!is.null(res_blend)) {
    cat("\n=== Blended vs market (paired, week-block bootstrap) ===\n")
    cmp_blend <- compare_to_market(res_blend, sched)
    # cmp_blend$overall$... has Brier/LogLoss and deltas; 95% CIs printed by the function
  } else {
    message("Blended backtest comparison skipped because blended results could not be constructed.")
  }
} else {
  message("Blended backtest comparison skipped: missing res or blend_oos inputs.")
}

# Optional quick peeks:
if (exists("cmp_blend") && !is.null(cmp_blend)) {
  print(cmp_blend$overall)
  head(cmp_blend$by_season)
}


moneyline_report_inputs <- NULL

if (exists("cmp_blend") && !is.null(cmp_blend)) {
  join_keys_html <- if (exists("PREDICTION_JOIN_KEYS", inherits = TRUE)) PREDICTION_JOIN_KEYS else c("game_id", "season", "week")
  report_tbl <- build_moneyline_comparison_table(
    market_comparison_result = cmp_blend,
    enriched_schedule = sched,
    join_keys = join_keys_html,
    vig = 0.10,
    verbose = TRUE
  )

  if (is.null(moneyline_report_inputs)) {
    moneyline_report_inputs <- list(
      comparison = cmp_blend,
      join_keys = join_keys_html
    )
  } else {
    moneyline_report_inputs$backtest <- cmp_blend
    if (is.null(moneyline_report_inputs$join_keys)) {
      moneyline_report_inputs$join_keys <- join_keys_html
    }
  }

  if (nrow(report_tbl)) {
    if (is.null(primary_report_tbl) || !nrow(primary_report_tbl)) {
      primary_report_tbl <- report_tbl
    }
  } else {
    message("Market comparison HTML snapshot from backtest was empty.")
  }
}

if (!is.null(primary_report_tbl) && nrow(primary_report_tbl)) {
  export_tbl <- primary_report_tbl
  skip_export <- FALSE

  if (exists("SEASON", inherits = TRUE) && exists("WEEK_TO_SIM", inherits = TRUE) &&
      all(c("season", "week") %in% names(export_tbl))) {
    filtered_export <- dplyr::filter(export_tbl, .data$season == SEASON, .data$week == WEEK_TO_SIM)
    if (nrow(filtered_export)) {
      export_tbl <- filtered_export
    } else {
      message("Moneyline comparison HTML skipped because no rows matched the current season/week filter.")
      skip_export <- TRUE
    }
  }

  if (!skip_export && nrow(export_tbl)) {
    report_title <- sprintf("Blend vs Market Moneylines - Week %s, %s", WEEK_TO_SIM, SEASON)
    report_path <- export_moneyline_comparison_html(
      comparison_tbl = export_tbl,
      title = report_title,
      verbose = TRUE,
      auto_open = TRUE,
      season = SEASON,
      week = WEEK_TO_SIM
    )
  } else if (!skip_export) {
    message("Moneyline comparison HTML skipped because the filtered table was empty.")
  }
} else {
  message("Moneyline comparison HTML skipped because no report table was available.")
}



# ============================== INTERACTIVE SLATE TABLE (reactable) =============================
pretty_df <- final |>
  tidyr::separate(matchup, into = c("away_team","home_team"), sep = " @ ", remove = FALSE) |>
  dplyr::left_join(
    games_ready |>
      dplyr::select(game_id, game_date, home_team, away_team, venue, dome, windy, cold, precip),
    by = c("home_team","away_team", "date" = "game_date")
  ) |>
  dplyr::mutate(
    dow      = lubridate::wday(date, label = TRUE, abbr = TRUE),

    # Model-only favorite and probabilities (calibrated 3-way)
    fav_team_model = dplyr::if_else(home_win_prob_cal >= away_win_prob_cal, home_team, away_team),
    fav_prob_model = pmax(home_win_prob_cal, away_win_prob_cal),
    win_tier_model = dplyr::case_when(
      fav_prob_model < 0.55 ~ "Coin flip (<55%)",
      fav_prob_model < 0.65 ~ "Lean (55-65%)",
      fav_prob_model < 0.75 ~ "Strong (65-75%)",
      TRUE                  ~ "Heavy (75%+)"
    ),

    # Blended (model vs market) favorite/probabilities for comparison
    fav_team_blend = dplyr::if_else(home_win_prob_blend >= away_win_prob_blend, home_team, away_team),
    fav_prob_blend = pmax(home_win_prob_blend, away_win_prob_blend),
    win_tier_blend = dplyr::case_when(
      fav_prob_blend < 0.55 ~ "Coin flip (<55%)",
      fav_prob_blend < 0.65 ~ "Lean (55-65%)",
      fav_prob_blend < 0.75 ~ "Strong (65-75%)",
      TRUE                  ~ "Heavy (75%+)"
    ),

    # Preserve legacy names for downstream usage (now explicitly model-based)
    fav_team = fav_team_model,
    fav_prob = fav_prob_model,
    win_tier = win_tier_model,
    total_bucket = dplyr::case_when(
      total_mean >= 50 ~ "Shootout (50+)",
      total_mean <= 41 ~ "Grinder (<=41)",
      total_mean >= 46 ~ "High (46-49.5)",
      TRUE             ~ "Average (41.5-45.5)"
    ),
    env = paste0(
      dplyr::if_else(dplyr::coalesce(dome, FALSE), "Dome", "Outdoor"),
      dplyr::if_else(dplyr::coalesce(windy, FALSE),  " + Wind",  ""),
      dplyr::if_else(dplyr::coalesce(cold,  FALSE),  " + Cold",  ""),
      dplyr::if_else(dplyr::coalesce(precip,FALSE),  " + Precip","")
    ),
    Category = paste(win_tier, "-", total_bucket, "-", env)
  ) |>
  dplyr::arrange(date, dplyr::desc(fav_prob_blend))
rt_df <- pretty_df %>%
  transmute(
    Day = as.character(dow),
    Date = format(date, "%b %d"),
    Matchup = matchup,
    `Home% (Blend 3-way)` = home_win_prob_blend,
    `Away% (Blend 3-way)` = away_win_prob_blend,
    `Home% (Blend 2-way)` = home_p_2w_blend,
    `Away% (Blend 2-way)` = 1 - home_p_2w_blend,
    `Home Median (Blend)` = home_median_blend,
    `Away Median (Blend)` = away_median_blend,
    `Total Median (Blend)` = total_median_blend
  )

# spot-check a few probabilities
print(
  final %>%
    dplyr::select(matchup, home_win_prob_cal, away_win_prob_cal, tie_prob,
                  home_p_2w_cal, away_p_2w_cal) %>%
    slice_head(n = 5)
)


if (reactable_available) {
  # Render reactable
  reactable::reactable(
    rt_df,
    searchable = TRUE,
    pagination = TRUE,
    defaultPageSize = 25,
    striped = TRUE,
    highlight = TRUE,
    defaultSorted = list("Date" = "asc", "Home% (Blend 3-way)" = "desc"),
    groupBy = "Day",
    columns = list(
      `Home% (Blend 3-way)` = reactable::colDef(
        format = reactable::colFormat(percent = TRUE, digits = 1),
        align = "center",
        style = function(value) list(
          background = pal_fav(value),
          color = if (is.na(value) || value < 0.72) "black" else "white"
        )
      ),
      `Away% (Blend 3-way)` = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1), align = "center"),
      `Home% (Blend 2-way)` = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1), align = "center"),
      `Away% (Blend 2-way)` = reactable::colDef(format = reactable::colFormat(percent = TRUE, digits = 1), align = "center"),
      `Home Median (Blend)` = reactable::colDef(
        format = reactable::colFormat(digits = 1),
        align = "center"
      ),
      `Away Median (Blend)` = reactable::colDef(
        format = reactable::colFormat(digits = 1),
        align = "center"
      ),
      `Total Median (Blend)` = reactable::colDef(
        format = reactable::colFormat(digits = 1),
        align = "center",
        style = function(value) list(
          background = pal_total(value),
          color = if (is.na(value) || value < 46) "black" else "white"
        )
      ),
      Date        = reactable::colDef(align = "center"),
      Day         = reactable::colDef(align = "center")
    ),
    theme = reactable::reactableTheme(
      borderColor = "#e5e7eb",
      stripedColor = "#f9fafb",
      highlightColor = "#eef2ff",
      cellPadding = "8px 10px",
      tableStyle = list(fontFamily = "system-ui, -apple-system, Segoe UI, Roboto, Helvetica, Arial, sans-serif")
    )
  )
} else {
  message("Interactive slate table skipped because 'reactable' is not available.")
}

log_dir <- file.path(getwd(), "run_logs")
if (!dir.exists(log_dir)) dir.create(log_dir, recursive = TRUE)

run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
cfg <- list(
  SEASON = SEASON, WEEK_TO_SIM = WEEK_TO_SIM, N_TRIALS = N_TRIALS, SEED = SEED,
  USE_SOS = USE_SOS, SOS_STRENGTH = SOS_STRENGTH,
  USE_RECENCY_DECAY = USE_RECENCY_DECAY, RECENCY_HALFLIFE = RECENCY_HALFLIFE,
  GLMM_BLEND_W_fixed = if (exists("GLMM_BLEND_W")) GLMM_BLEND_W else NA_real_,
  RHO_SCORE = RHO_SCORE, PTS_CAP_HI = PTS_CAP_HI
)
saveRDS(cfg, file.path(log_dir, paste0("config_", run_id, ".rds")))
saveRDS(final, file.path(log_dir, paste0("final_", run_id, ".rds")))
saveRDS(games_ready, file.path(log_dir, paste0("games_ready_", run_id, ".rds")))



if (!is.null(moneyline_report_inputs) && exists("moneyline_report", inherits = TRUE)) {
  report_schedule <- tryCatch({
    base_sched <- sched
    if (exists("final")) {
      medians_tbl <- final %>%
        dplyr::select(
          game_id,
          season,
          week,
          blend_home_median = home_median_blend,
          blend_away_median = away_median_blend,
          blend_total_median = total_median_blend
        )
      base_sched <- base_sched %>%
        dplyr::left_join(medians_tbl, by = c("game_id", "season", "week"))
    }

    base_sched %>%
      dplyr::filter(season == SEASON, week == WEEK_TO_SIM)
  }, error = function(e) {
    message(sprintf("Unable to prepare schedule context for moneyline report: %s", conditionMessage(e)))
    NULL
  })

  if (!is.null(report_schedule)) {
    report_title <- sprintf("Blend vs Market Moneylines - Week %s, %s", WEEK_TO_SIM, SEASON)
    comparison_payload <- moneyline_report_inputs$comparison
    if (is.null(comparison_payload) && !is.null(moneyline_report_inputs$backtest)) {
      comparison_payload <- moneyline_report_inputs$backtest
    }
    report_path <- tryCatch(
      moneyline_report(
        market_comparison_result = comparison_payload,
        schedule = report_schedule,
        join_keys = moneyline_report_inputs$join_keys,
        vig = 0.10,
        title = report_title,
        verbose = TRUE,
        auto_open = TRUE
      ),
      error = function(e) {
        message(sprintf("Moneyline report generation failed: %s", conditionMessage(e)))
        NULL
      }
    )

    if (!is.null(report_path)) {
      message(sprintf("Moneyline report written to %s", report_path))
    }
  }
} else if (!exists("moneyline_report", inherits = TRUE)) {
  message("moneyline_report() is unavailable; HTML export skipped.")
}
