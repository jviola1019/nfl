# ------------------------------------------------------------------------------
# NFLmarket.R
# Utility helpers for attaching blended probabilities to historic results and
# comparing the simulation output against the betting market.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  source("NFLbrier_logloss.R")
  library(tidyverse)
})

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

if (!exists("first_non_missing_typed", inherits = FALSE)) {
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
}

if (!exists("collapse_by_keys_relaxed", inherits = FALSE)) {
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
}

if (!exists("standardize_join_keys", inherits = FALSE)) {
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
}

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

evaluate_market_vs_blend <- function(res,
                                     blend_oos,
                                     sched,
                                     join_keys = PREDICTION_JOIN_KEYS,
                                     prob_candidates = c("p2_cal", "home_p_2w_cal", "p2_home_cal", "home_p2w_cal"),
                                     verbose = TRUE) {
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

  compare_to_market(res_blend, sched)
}

if (interactive()) {
  message("NFLmarket.R loaded. Use load_latest_market_inputs() and evaluate_market_vs_blend() as needed.")
}
