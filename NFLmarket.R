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

  combined <- scores %>%
    ensure_score_defaults() %>%
    dplyr::mutate(
      blend_home_prob = dplyr::coalesce(blend_home_prob, model_prob),
      market_home_prob = dplyr::coalesce(market_home_prob, market_prob)
    ) %>%
    dplyr::inner_join(schedule_context, by = join_cols) %>%
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
      market_home_prob = clamp_probability(market_home_prob),
      market_away_prob = clamp_probability(1 - market_home_prob),
      market_home_spread = coerce_numeric_safely(market_home_spread),
      market_total_line = coerce_numeric_safely(market_total_line),
      blend_median_margin = dplyr::if_else(
        is.na(blend_home_median) | is.na(blend_away_median),
        NA_real_,
        blend_home_median - blend_away_median
      ),
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
      market_moneyline = dplyr::if_else(
        blend_favorite_side == "home",
        market_home_ml,
        market_away_ml
      ),
      market_moneyline = dplyr::if_else(
        is.na(market_moneyline),
        probability_to_american(
          dplyr::if_else(blend_favorite_side == "home", market_home_prob, market_away_prob)
        ),
        market_moneyline
      ),
      blend_moneyline = dplyr::if_else(
        blend_favorite_side == "home",
        blend_home_ml,
        blend_away_ml
      ),
      blend_moneyline_vig = dplyr::if_else(
        blend_favorite_side == "home",
        blend_home_ml_vig,
        blend_away_ml_vig
      ),
      blend_prob_fav = dplyr::if_else(
        blend_favorite_side == "home",
        blend_home_prob,
        blend_away_prob
      ),
      market_prob_fav = dplyr::if_else(
        blend_favorite_side == "home",
        market_home_prob,
        market_away_prob
      ),
      blend_edge_prob = blend_prob_fav - market_prob_fav,
      blend_ev_units = dplyr::if_else(
        blend_favorite_side == "home",
        blend_ev_units_home,
        blend_ev_units_away
      ),
      blend_beats_market = dplyr::case_when(
        is.na(blend_ev_units) ~ NA,
        TRUE ~ blend_ev_units > 0
      ),
      market_ev_units = dplyr::if_else(
        is.na(blend_ev_units),
        NA_real_,
        -blend_ev_units
      ),
      market_winning = dplyr::case_when(
        is.na(market_ev_units) ~ NA,
        market_ev_units > 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      blend_recommendation = dplyr::case_when(
        is.na(blend_ev_units) ~ "No play",
        blend_ev_units > 0 ~ paste("Bet", blend_favorite, "moneyline"),
        TRUE ~ "Pass"
      ),
      blend_confidence = dplyr::case_when(
        is.na(blend_ev_units) ~ NA_real_,
        blend_ev_units < 0 ~ 0,
        TRUE ~ blend_ev_units
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
    "Blend Favorite Moneyline (vig)"
  )

  format_moneyline_strings <- function(x) {
    num <- suppressWarnings(as.numeric(x))
    out <- rep("", length(x))
    mask <- !is.na(num) & is.finite(num)
    if (any(mask)) {
      out[mask] <- sprintf("%+d", as.integer(round(num[mask])))
    }
    out
  }

  display_tbl <- comparison_tbl %>%
    dplyr::transmute(
      Season = season,
      Week = week,
      Date = game_date,
      Matchup = matchup,
      `Blend Favorite` = blend_favorite,
      `Blend Recommendation` = blend_recommendation,
      `Blend Stake (Units)` = blend_confidence,
      `Blend Home Moneyline (vig)` = blend_home_ml_vig,
      `Blend Away Moneyline (vig)` = blend_away_ml_vig,
      `Blend Favorite Moneyline (vig)` = blend_moneyline_vig,
      `Blend Median Home Score` = blend_home_median,
      `Blend Median Away Score` = blend_away_median,
      `Blend Median Total` = blend_total_median,
      `Blend Median Margin` = blend_median_margin,
      `Market Home Spread` = market_home_spread,
      `Market Implied Margin` = market_implied_margin,
      `Market Total` = market_total,
      `Blend Edge` = blend_edge_prob,
      `Blend EV Units` = blend_ev_units,
      `Market EV Units` = market_ev_units,
      `Blend Beat Market?` = dplyr::case_when(
        is.na(blend_beats_market) ~ "N/A",
        blend_beats_market ~ "Yes",
        TRUE ~ "No"
      ),
      `Market Winning?` = dplyr::case_when(
        is.na(market_winning) ~ "N/A",
        market_winning ~ "Yes",
        TRUE ~ "No"
      ),
      `Market Home Prob` = market_home_prob,
      `Blend Home Prob` = blend_home_prob,
      `Market Away Prob` = market_away_prob,
      `Blend Away Prob` = blend_away_prob,
      Winner = actual_winner
    )

  saved <- FALSE

  if (requireNamespace("gt", quietly = TRUE)) {
    gt_tbl <- display_tbl %>%
      gt::gt() %>%
      gt::fmt_percent(
        columns = c(
          "Blend Edge",
          "Market Home Prob", "Blend Home Prob",
          "Market Away Prob", "Blend Away Prob"
        ),
        decimals = 1
      ) %>%
        gt::fmt_number(
          columns = c(
            "Blend Median Home Score", "Blend Median Away Score", "Blend Median Total",
            "Blend Median Margin", "Market Home Spread", "Market Implied Margin", "Market Total"
          ),
          decimals = 1,
          drop_trailing_zeros = TRUE
        ) %>%
      gt::fmt_number(
        columns = c("Blend EV Units", "Market EV Units", "Blend Stake (Units)"),
        decimals = 3,
        drop_trailing_zeros = FALSE
      ) %>%
      gt::fmt(
        columns = moneyline_cols,
        fns = function(x) format_moneyline_strings(x)
      ) %>%
      gt::cols_align(
        align = "center",
        columns = c(
          "Season", "Week", "Blend Favorite", "Blend Recommendation",
          "Blend Beat Market?", "Market Winning?"
        )
      ) %>%
        gt::cols_label(
          `Blend Edge` = "Blend Edge (pp)",
          `Blend EV Units` = "Blend EV (units)",
          `Market EV Units` = "Market EV (units)",
          `Blend Stake (Units)` = "Blend Stake",
          `Blend Favorite Moneyline (vig)` = "Blend ML (vig)",
          `Blend Median Margin` = "Blend Margin",
          `Market Home Spread` = "Market Home Spread",
          `Market Implied Margin` = "Market Margin",
          `Market Total` = "Market Total"
        ) %>%
      gt::tab_header(title = title) %>%
      gt::tab_options(
        table.font.names = c("Source Sans Pro", "Helvetica Neue", "Arial", "sans-serif"),
        table.background.color = "#020617",
        table.font.color = "#e2e8f0",
        heading.background.color = "#0f172a",
        column_labels.background.color = "#1e293b",
        column_labels.font.weight = "bold",
        column_labels.text_transform = "uppercase",
        row.striping.background_color = "#111827"
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#f8fafc"),
        locations = gt::cells_title(groups = "title")
      ) %>%
      gt::tab_style(
        style = gt::cell_text(color = "#cbd5f5"),
        locations = gt::cells_title(groups = "subtitle")
      ) %>%
      gt::opt_row_striping() %>%
      gt::data_color(
        columns = "Blend Beat Market?",
        colors = scales::col_factor(
          palette = c("No" = "#1f2937", "Yes" = "#166534", "N/A" = "#374151"),
          domain = c("No", "Yes", "N/A")
        )
      ) %>%
      gt::tab_style(
        style = list(
          gt::cell_fill(color = "#1d4ed8"),
          gt::cell_text(color = "#f8fafc", weight = "bold")
        ),
        locations = gt::cells_body(
          columns = "Blend Recommendation",
          rows = !is.na(`Blend Recommendation`) & `Blend Recommendation` != "Pass"
        )
      ) %>%
      gt::tab_style(
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
    css_block <- "body {font-family: 'Source Sans Pro', Arial, sans-serif; background-color: #020617; color: #e2e8f0;}\n"
    css_block <- paste0(
      css_block,
      "table {width: 100%; border-collapse: collapse; background-color: #0f172a; color: #e2e8f0;}\n",
      "th {background-color: #1e293b; color: #f8fafc; text-transform: uppercase; letter-spacing: 0.08em;}\n",
      "td, th {padding: 10px 12px; border-bottom: 1px solid #1f2937; text-align: center;}\n",
      "tr:nth-child(even) {background-color: #111c2f;}\n",
      "tr.blend-win {background-color: #14532d;}\n",
      "tr.blend-win td {color: #ecfdf5;}\n",
      "td.blend-reco {background-color: #1d4ed8 !important; color: #f8fafc !important; font-weight: 600;}\n",
      "caption {caption-side: top; font-size: 1.25rem; font-weight: 600; margin-bottom: 0.75rem; color: #f8fafc;}\n"
    )

    formatted_tbl <- display_tbl %>%
      dplyr::mutate(
        `Blend Edge` = scales::percent(`Blend Edge`, accuracy = 0.1),
        `Market Home Prob` = scales::percent(`Market Home Prob`, accuracy = 0.1),
        `Blend Home Prob` = scales::percent(`Blend Home Prob`, accuracy = 0.1),
        `Market Away Prob` = scales::percent(`Market Away Prob`, accuracy = 0.1),
        `Blend Away Prob` = scales::percent(`Blend Away Prob`, accuracy = 0.1),
        `Blend Median Home Score` = format(round(`Blend Median Home Score`, 1), nsmall = 1),
        `Blend Median Away Score` = format(round(`Blend Median Away Score`, 1), nsmall = 1),
        `Blend Median Total` = format(round(`Blend Median Total`, 1), nsmall = 1),
        `Blend Median Margin` = format(round(`Blend Median Margin`, 1), nsmall = 1),
        `Market Home Spread` = format(round(`Market Home Spread`, 1), nsmall = 1),
        `Market Implied Margin` = format(round(`Market Implied Margin`, 1), nsmall = 1),
        `Market Total` = format(round(`Market Total`, 1), nsmall = 1),
        `Blend EV Units` = format(round(`Blend EV Units`, 3), nsmall = 3),
        `Market EV Units` = format(round(`Market EV Units`, 3), nsmall = 3),
        `Blend Stake (Units)` = dplyr::if_else(
          is.na(`Blend Stake (Units)`),
          "",
          format(round(`Blend Stake (Units)`, 3), nsmall = 3)
        ),
        dplyr::across(
          dplyr::all_of(moneyline_cols),
          format_moneyline_strings
        )
      )

    if (requireNamespace("htmltools", quietly = TRUE)) {
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
              cell_class <- NULL
              if (identical(col_name, "Blend Recommendation") && !is.null(recommendation_val) &&
                  !identical(recommendation_val, "Pass") && !is.na(recommendation_val)) {
                cell_class <- "blend-reco"
              }
              cell_value <- ifelse(is.na(value), "", value)
              if (length(cell_class)) {
                htmltools::tags$td(class = cell_class, cell_value)
              } else {
                htmltools::tags$td(cell_value)
              }
            })
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
            cell_class <- NULL
            if (identical(col_name, "Blend Recommendation") && !is.null(recommendation_val) &&
                !identical(recommendation_val, "Pass") && !is.na(recommendation_val)) {
              cell_class <- "blend-reco"
            }
            cell_value <- ifelse(is.na(value), "", value)
            if (length(cell_class)) {
              sprintf("<td class=\"%s\">%s</td>", cell_class, cell_value)
            } else {
              sprintf("<td>%s</td>", cell_value)
            }
          })
          sprintf("<tr%s>%s</tr>", row_class_attr, paste(cells, collapse = ""))
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
