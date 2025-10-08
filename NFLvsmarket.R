# ──────────────────────────────────────────────────────────────────────────────
# NFL_verify_vs_market.R
# Standalone verification & best-bet helper for your simulation.
# - Learns a spread→probability map when MLs are missing
# - Compares Model (and Blend, if present) vs Market with week-block bootstrap CIs
# - Builds a readable Best Bets table for the current slate
#
# EXPECTED INPUTS (preferred, but script can auto-source fallbacks):
#   sched  : schedules/odds data.frame (from nflreadr::load_schedules)
#   res    : list with $per_game tibble (must include p_blend)
#   final  : tibble of current-slate outputs (must include home_p_2w_blend, tie_prob, matchup, date)
#
# Optional convenience: if 'res' is not in memory, will try to load newest
# RDS from ~/.cache/nfl_sim_scores/. If 'sched' missing, will load schedules.
# If 'final' missing, you can point FINAL_RDS to a saved object.
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(nflreadr)
  library(scales)
  library(glue)
})

xfun_meets_min <- tryCatch({
  if (!requireNamespace("xfun", quietly = TRUE)) {
    FALSE
  } else {
    utils::packageVersion("xfun") >= "0.52"
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
htmlwidgets_available <- has_namespace("htmlwidgets", needs_new_xfun = TRUE)
htmltools_available <- has_namespace("htmltools", needs_new_xfun = TRUE)

if (!gt_available) {
  if (!xfun_meets_min) {
    message("Package 'gt' skipped because 'xfun' >= 0.52 is unavailable; using plain tables instead.")
  } else {
    message("Package 'gt' is not available; falling back to data-frame output for tables.")
  }
}
if (!reactable_available) {
  message("Package 'reactable' is not available; skipping interactive odds table.")
}
if (!htmlwidgets_available) {
  if (!xfun_meets_min) {
    message("Package 'htmlwidgets' skipped because 'xfun' >= 0.52 is unavailable; report will omit widgets.")
  } else {
    message("Package 'htmlwidgets' is not available; HTML report will be saved without widget dependencies.")
  }
}
if (!htmltools_available) {
  if (!xfun_meets_min) {
    message("Package 'htmltools' skipped because 'xfun' >= 0.52 is unavailable; writing simplified HTML report.")
  } else {
    message("Package 'htmltools' is not available; will write a simplified HTML report.")
  }
}

# -------------------------- Config knobs --------------------------------------
# Set this if you saved 'final' to disk; leave NULL to skip:
FINAL_RDS <- NULL        # e.g., FINAL_RDS <- "final_latest.rds"
N_BOOT    <- 2000        # bootstrap resamples for week-block CIs
FOCUS_MATCHUP <- NULL    # e.g., "SF @ KC" to print only one game in Best Bets
MARKET_PROVIDER <- "ESPN BET"

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

american_to_decimal <- function(odds) {
  odds <- suppressWarnings(as.numeric(odds))
  ifelse(odds < 0, 1 + 100/abs(odds), 1 + odds/100)
}

expected_value_units <- function(prob, odds) {
  dec <- american_to_decimal(odds)
  prob <- .clp(prob)
  ifelse(is.finite(dec), prob * (dec - 1) - (1 - prob), NA_real_)
}

apply_moneyline_vig <- function(odds, vig = 0.10) {
  ifelse(
    is.finite(odds),
    ifelse(
      odds < 0,
      -as.numeric(round(abs(odds) * (1 + vig))),
      as.numeric(round(odds / (1 + vig)))
    ),
    NA_real_
  )
}

vig_moneyline_from_prob <- function(prob, side_key = c("home", "away"), vig = 0.10) {
  side_key <- match.arg(side_key)
  if (!is.finite(prob)) return(NA_real_)
  prob <- .clp(prob)
  base <- prob_to_american(prob)
  if (!is.finite(base)) return(NA_real_)

  favorite <- prob > 0.5
  if (abs(prob - 0.5) < 1e-8) {
    favorite <- (side_key == "home")
  }

  adj <- apply_moneyline_vig(base, vig = vig)

  if (!favorite && adj < 0) {
    adj <- abs(adj)
  }

  if (!favorite && adj < 100) {
    adj <- 100
  }

  if (favorite && adj > -100) {
    adj <- -100
  }

  adj
}

safe_weighted_mean <- function(x, w) {
  keep <- is.finite(x) & is.finite(w)
  if (!any(keep)) {
    return(NA_real_)
  }
  stats::weighted.mean(x[keep], w[keep], na.rm = TRUE)
}

first_valid_column <- function(df, candidates) {
  for (nm in candidates) {
    if (nm %in% names(df)) {
      col <- df[[nm]]
      if (is.numeric(col) && any(is.finite(col))) {
        return(nm)
      }
    }
  }
  NA_character_
}

derive_blend_probability <- function(df, extra_blend_sources = list()) {
  if (!is.list(extra_blend_sources)) {
    extra_blend_sources <- list(extra_blend_sources)
  }

  augmented <- df
  join_keys <- intersect(c("game_id", "season", "week"), names(df))

  if (length(join_keys)) {
    for (src in extra_blend_sources) {
      if (is.null(src) || !inherits(src, "data.frame")) next

      blend_cols <- names(src)[grepl("_blend$", names(src))]
      if (!length(blend_cols)) next

      src_keys <- intersect(join_keys, names(src))
      if (!length(src_keys)) next

      extra <- src %>%
        dplyr::select(dplyr::any_of(c(src_keys, blend_cols))) %>%
        distinct()

      if (!nrow(extra)) next

      joined <- augmented %>%
        left_join(extra, by = src_keys, relationship = "many-to-one", suffix = c("", ".src"))

      for (col in blend_cols) {
        src_nm <- paste0(col, ".src")
        if (!src_nm %in% names(joined)) next

        if (col %in% names(joined)) {
          joined[[col]] <- dplyr::coalesce(joined[[col]], joined[[src_nm]])
        } else {
          joined[[col]] <- joined[[src_nm]]
        }

        joined[[src_nm]] <- NULL
      }

      augmented <- joined
    }
  }

  orig_has_p <- "p_blend" %in% names(augmented)
  orig_p <- if (orig_has_p) augmented$p_blend else rep(NA_real_, nrow(augmented))

  home_priority <- c("home_p_2w_blend", "home_p_blend")
  home_candidates <- unique(c(
    intersect(home_priority, names(augmented)),
    grep("^home_.*_blend$", names(augmented), value = TRUE)
  ))
  home_col <- first_valid_column(augmented, home_candidates)

  away_priority <- c("away_p_2w_blend", "away_p_blend")
  away_candidates <- unique(c(
    intersect(away_priority, names(augmented)),
    grep("^away_.*_blend$", names(augmented), value = TRUE)
  ))
  away_col <- first_valid_column(augmented, away_candidates)

  other_candidates <- setdiff(
    grep("_blend$", names(augmented), value = TRUE),
    c("p_blend", home_candidates, away_candidates)
  )
  other_col <- first_valid_column(augmented, other_candidates)
  if (orig_has_p && any(is.finite(orig_p))) {
    p <- orig_p

    if (!is.na(home_col)) {
      home_vals <- augmented[[home_col]]
      fill_idx <- !is.finite(p) & is.finite(home_vals)
      if (any(fill_idx)) {
        p[fill_idx] <- home_vals[fill_idx]
      }
    }

    if (!is.na(away_col)) {
      away_vals <- augmented[[away_col]]
      fill_idx <- !is.finite(p) & is.finite(away_vals)
      if (any(fill_idx)) {
        p[fill_idx] <- 1 - away_vals[fill_idx]
      }
    }

    if (!is.na(other_col)) {
      other_vals <- augmented[[other_col]]
      fill_idx <- !is.finite(p) & is.finite(other_vals)
      if (any(fill_idx)) {
        p[fill_idx] <- other_vals[fill_idx]
      }
    }

    return(augmented %>% mutate(p_blend = .clp(p)))
  }

  if (!is.na(home_col)) {
    message(sprintf(
      "res$per_game missing p_blend; using '%s' as the blended home win probability.",
      home_col
    ))
    return(augmented %>% mutate(p_blend = .clp(.data[[home_col]])))
  }

  if (!is.na(away_col)) {
    message(sprintf(
      "res$per_game missing p_blend; using 1 - '%s' as the blended home win probability.",
      away_col
    ))
    return(augmented %>% mutate(p_blend = .clp(1 - .data[[away_col]])))
  }
  if (!is.na(other_col)) {
    message(sprintf(
      "res$per_game missing p_blend; using '%s' as the blended probability source.",
      other_col
    ))
    return(augmented %>% mutate(p_blend = .clp(.data[[other_col]])))
  }

  stop("res$per_game must include a blend probability column (one ending in '_blend').")
}

format_line <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    ifelse(abs(x - round(x)) < 1e-6,
           sprintf("%+d", as.integer(round(x))),
           sprintf("%+.1f", x))
  )
}

cover_probability_norm <- function(mean_margin, sd_margin, spread, side = c("home","away")) {
  side <- match.arg(side)
  if (!is.finite(mean_margin) || !is.finite(sd_margin) || sd_margin <= 0 || !is.finite(spread)) {
    return(NA_real_)
  }
  if (side == "home") {
    stats::pnorm(-spread, mean = mean_margin, sd = sd_margin, lower.tail = FALSE)
  } else {
    stats::pnorm(spread, mean = mean_margin, sd = sd_margin, lower.tail = TRUE)
  }
}

pick_col <- function(df, cands) {
  nm <- intersect(cands, names(df))
  if (length(nm)) nm[1] else NA_character_
}

safe_load_lines <- function(seasons) {
  tryCatch(nflreadr::load_lines(seasons = seasons), error = function(e) NULL)
}

normalize_provider <- function(x) {
  if (is.null(x)) return(character())
  gsub("[^a-z]", "", tolower(trimws(as.character(x))))
}

market_probs_from_lines <- function(lines_df, sched_df, provider = "ESPN BET") {
  if (is.null(lines_df) || !nrow(lines_df)) return(tibble())

  provider_col <- pick_col(lines_df, c("provider", "book", "bookmaker", "sportsbook"))
  bet_col      <- pick_col(lines_df, c("bet_type", "market_type", "type", "wager_type"))
  side_col     <- pick_col(lines_df, c("side", "team", "participant", "selection"))
  odds_col     <- pick_col(lines_df, c("american_odds", "odds_american", "price", "line_price", "odds"))
  ts_col       <- pick_col(lines_df, c("timestamp", "last_update", "line_timestamp", "updated_at", "lastUpdated"))
  alt_col      <- pick_col(lines_df, c("is_alternate", "alternate", "is_alt", "alt_line"))

  if (is.na(provider_col) || is.na(bet_col) || is.na(side_col) ||
      is.na(odds_col) || !"game_id" %in% names(lines_df)) {
    return(tibble())
  }

  base <- sched_df %>%
    filter(game_type %in% c("REG", "Regular")) %>%
    transmute(game_id, season, week, home_team, away_team)

  target_provider <- normalize_provider(provider)
  if (!nzchar(target_provider)) return(tibble())

  lines_df %>%
    mutate(provider_norm = normalize_provider(.data[[provider_col]])) %>%
    filter(provider_norm == target_provider, game_id %in% base$game_id) %>%
    transmute(
      game_id,
      bet_type = tolower(as.character(.data[[bet_col]])),
      side_raw = .data[[side_col]],
      odds = suppressWarnings(as.numeric(.data[[odds_col]])),
      is_alt = if (!is.na(alt_col)) as.logical(.data[[alt_col]]) else FALSE,
      ts_raw = if (!is.na(ts_col)) .data[[ts_col]] else NA,
      provider_norm
    ) %>%
    left_join(base, by = "game_id") %>%
    mutate(
      side = standardize_side(side_raw, home_team, away_team),
      ts_num = suppressWarnings(as.numeric(lubridate::ymd_hms(ts_raw, quiet = TRUE)))
    ) %>%
    mutate(ts_num = ifelse(is.na(ts_num), suppressWarnings(as.numeric(as.POSIXct(ts_raw, tz = "UTC"))), ts_num)) %>%
    mutate(ts_num = ifelse(is.na(ts_num), as.numeric(dplyr::row_number()), ts_num)) %>%
    filter(grepl("moneyline", bet_type), !is.na(side), side %in% c("home", "away"),
           is.finite(odds), !isTRUE(is_alt)) %>%
    group_by(game_id, side) %>%
    arrange(dplyr::desc(ts_num)) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(game_id, season, week, side, odds) %>%
    tidyr::pivot_wider(names_from = side, values_from = odds, names_prefix = "odds_") %>%
    mutate(
      ph = american_to_prob(odds_home),
      pa = american_to_prob(odds_away),
      den = ph + pa,
      p_home_mkt_2w = .clp(ifelse(is.finite(den) & den > 0, ph/den, NA_real_))
    ) %>%
    select(game_id, season, week, p_home_mkt_2w) %>%
    filter(is.finite(p_home_mkt_2w))
}

closing_spreads_tbl <- function(sched_df) {
  sp_col <- pick_col(sched_df, c("close_spread","spread_close","home_spread_close","spread_line","spread","home_spread"))
  if (is.na(sp_col)) return(tibble(game_id = character(), home_main_spread = numeric()))
  sched_df %>%
    filter(game_type %in% c("REG","Regular")) %>%
    transmute(game_id, home_main_spread = suppressWarnings(as.numeric(.data[[sp_col]])))
}

standardize_side <- function(side_raw, home_team, away_team) {
  if (is.na(side_raw)) return(NA_character_)
  sr <- toupper(trimws(as.character(side_raw)))
  home <- toupper(home_team)
  away <- toupper(away_team)

  if (sr %in% c("HOME","H","HOST")) return("home")
  if (sr %in% c("AWAY","VISITOR","ROAD","A")) return("away")
  if (nzchar(home) && grepl(home, sr, fixed = TRUE)) return("home")
  if (nzchar(away) && grepl(away, sr, fixed = TRUE)) return("away")
  if (nzchar(home) && startsWith(sr, substr(home, 1, 3))) return("home")
  if (nzchar(away) && startsWith(sr, substr(away, 1, 3))) return("away")
  NA_character_
}

build_line_catalog <- function(final_df, lines_df, sched_df, provider = "ESPN BET") {
  if (is.null(lines_df) || !nrow(lines_df)) return(tibble())

  bt_col  <- pick_col(lines_df, c("bet_type","market_type","type","wager_type"))
  side_col<- pick_col(lines_df, c("side","team","participant","selection"))
  odds_col<- pick_col(lines_df, c("american_odds","odds_american","price","line_price","odds"))
  line_col<- pick_col(lines_df, c("line","spread_line","handicap","points","spread"))
  book_col<- pick_col(lines_df, c("provider","book","bookmaker","sportsbook"))
  alt_col <- pick_col(lines_df, c("is_alternate","alternate","is_alt","alt_line"))

  if (is.na(bt_col) || is.na(side_col) || is.na(odds_col)) return(tibble())

  if (!"home_p_2w_blend" %in% names(final_df)) {
    stop("final_df must include 'home_p_2w_blend' when building the line catalog.")
  }

  base <- final_df %>%
    transmute(game_id, date = as.Date(date), matchup, home_team, away_team,
              home_prob = .clp(home_p_2w_blend),
              away_prob = 1 - home_prob,
              margin_mean, margin_sd) %>%
    distinct()

  if (!"game_id" %in% names(lines_df)) return(tibble())

  provider_key <- normalize_provider(provider)

  lines_df %>%
    filter(game_id %in% base$game_id) %>%
    transmute(
      game_id,
      bet_type = tolower(as.character(.data[[bt_col]])),
      side_raw = .data[[side_col]],
      odds = suppressWarnings(as.numeric(.data[[odds_col]])),
      line = if (!is.na(line_col)) suppressWarnings(as.numeric(.data[[line_col]])) else NA_real_,
      book = if (!is.na(book_col)) as.character(.data[[book_col]]) else NA_character_,
      is_alt = if (!is.na(alt_col)) as.logical(.data[[alt_col]]) else NA
    ) %>%
    mutate(book_norm = normalize_provider(book)) %>%
    filter(is.na(provider_key) | book_norm == provider_key) %>%
    left_join(base, by = "game_id") %>%
    mutate(
      side = standardize_side(side_raw, home_team, away_team),
      is_alt = ifelse(is.na(is_alt), FALSE, is_alt)
    ) %>%
    filter(is.finite(odds), !is.na(side)) %>%
    left_join(closing_spreads_tbl(sched_df), by = "game_id") %>%
    mutate(
      main_spread_side = case_when(
        !is.na(home_main_spread) & side == "home" ~ home_main_spread,
        !is.na(home_main_spread) & side == "away" ~ -home_main_spread,
        TRUE ~ NA_real_
      ),
      is_alt = ifelse(!is_alt & !is.na(main_spread_side) & !is.na(line), abs(line - main_spread_side) > 0.05, is_alt),
      bet_bucket = case_when(
        grepl("moneyline", bet_type) ~ "Moneyline",
        grepl("spread", bet_type) & is_alt ~ "Alt Spread",
        grepl("spread", bet_type) ~ "Spread",
        TRUE ~ stringr::str_to_title(bet_type)
      ),
      team_label = ifelse(side == "home", home_team, away_team),
      model_prob = case_when(
        bet_bucket == "Moneyline" & side == "home" ~ home_prob,
        bet_bucket == "Moneyline" & side == "away" ~ away_prob,
        bet_bucket %in% c("Spread","Alt Spread") & side == "home" ~ cover_probability_norm(margin_mean, margin_sd, line, "home"),
        bet_bucket %in% c("Spread","Alt Spread") & side == "away" ~ cover_probability_norm(margin_mean, margin_sd, line, "away"),
        TRUE ~ NA_real_
      ),
      market_prob = .clp(american_to_prob(odds)),
      edge = model_prob - market_prob,
      ev_units = expected_value_units(model_prob, odds),
      line_display = dplyr::case_when(
        bet_bucket == "Moneyline" ~ "ML",
        bet_bucket %in% c("Spread","Alt Spread") ~ format_line(line),
        TRUE ~ as.character(line)
      ),
      selection = as.character(glue("{team_label} {line_display}")),
      odds_fmt = ifelse(is.finite(odds), sprintf("%+d", as.integer(round(odds))), NA_character_)
    ) %>%
    filter(is.finite(model_prob)) %>%
    select(-book_norm)
}

best_offer_rows <- function(catalog) {
  if (!nrow(catalog)) return(catalog)

  catalog %>%
    group_by(game_id, bet_bucket, side, line_display) %>%
    slice_max(order_by = ev_units, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    group_by(game_id, bet_bucket) %>%
    slice_max(order_by = ev_units, n = 6, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(date, matchup, bet_bucket, desc(ev_units))
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

if (!exists("final")) {
  candidate_files <- character()
  if (!is.null(FINAL_RDS)) {
    candidate_files <- c(candidate_files, FINAL_RDS)
  }

  search_dirs <- c(
    getOption("nfl.final_dir", default = character()),
    Sys.getenv("NFL_FINAL_DIR", unset = ""),
    file.path(getwd(), "run_logs"),
    "run_logs",
    path.expand(file.path("~", "run_logs"))
  )
  if (!is.null(FINAL_RDS)) {
    search_dirs <- c(search_dirs, dirname(FINAL_RDS))
  }
  search_dirs <- unique(search_dirs)

  search_dirs <- search_dirs[nzchar(search_dirs)]
  for (dir in search_dirs) {
    candidate_files <- c(candidate_files, list_rds_with_prefix(dir, "final_"))
  }

  candidate_files <- unique(candidate_files)
  latest_final <- latest_existing_file(candidate_files)
  if (!is.null(latest_final)) {
    message(sprintf("Loading final from: %s", latest_final))
    final <- readRDS(latest_final)
  }
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
spread_map_notice_emitted <- FALSE

learn_spread_map <- function(sched_df) {
  sp_col <- pick_col(sched_df, c("close_spread","spread_close","home_spread_close",
                                 "spread_line","spread","home_spread"))
  ml_h   <- pick_col(sched_df, c("home_ml_close","ml_home_close","moneyline_home_close",
                                 "home_moneyline_close","home_ml","ml_home","moneyline_home",
                                 "home_moneyline"))
  ml_a   <- pick_col(sched_df, c("away_ml_close","ml_away_close","moneyline_away_close",
                                 "away_moneyline_close","away_ml","ml_away","moneyline_away",
                                 "away_moneyline"))
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
    mutate(
      p_home = ifelse(is.finite(ph + pa) & (ph + pa) > 0, ph/(ph + pa), NA_real_)
    ) %>%
    filter(is.finite(spread), is.finite(p_home)) %>%
    mutate(
      weight = pmax(1L, round(1000 * abs(p_home - 0.5))),
      success = pmin(pmax(round(p_home * weight), 0L), weight)
    )
  
  if (nrow(df) < 400) {
    if (!spread_map_notice_emitted) {
      message("Not enough ML+spread history to learn map; falling back to Normal SD heuristic.")
      spread_map_notice_emitted <<- TRUE
    }
    return(NULL)
  }
  
  # Use flexible logistic polynomial (3rd degree) to map spread -> p_home_2w
  fit <- glm(cbind(success, weight - success) ~ poly(spread, 3, raw = TRUE),
             data = df, family = binomial())
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
market_probs_from_sched <- function(sched_df, spread_mapper = NULL, lines_df = NULL, provider = "ESPN BET") {
  sp_col <- pick_col(sched_df, c("close_spread","spread_close","home_spread_close",
                                 "spread_line","spread","home_spread"))
  ml_h   <- pick_col(sched_df, c("home_ml_close","ml_home_close","moneyline_home_close",
                                 "home_moneyline_close","home_ml","ml_home","moneyline_home",
                                 "home_moneyline"))
  ml_a   <- pick_col(sched_df, c("away_ml_close","ml_away_close","moneyline_away_close",
                                 "away_moneyline_close","away_ml","ml_away","moneyline_away",
                                 "away_moneyline"))

  base <- sched_df %>%
    filter(game_type %in% c("REG","Regular")) %>%
    transmute(game_id, season, week)

  ml_tbl <- tibble::tibble(game_id = character(), season = integer(), week = integer(), p_home_mkt_2w_ml = numeric())
  if (!is.na(ml_h) && !is.na(ml_a)) {
    ml_tbl <- sched_df %>%
      transmute(
        game_id, season, week,
        ph = american_to_prob(.data[[ml_h]]),
        pa = american_to_prob(.data[[ml_a]])
      ) %>%
      filter(is.finite(ph), is.finite(pa)) %>%
      mutate(
        den = ph + pa,
        p_home_mkt_2w_ml = .clp(ifelse(is.finite(den) & den > 0, ph/den, NA_real_))
      ) %>%
      select(game_id, season, week, p_home_mkt_2w_ml)
  }

  spread_tbl <- tibble::tibble(game_id = character(), season = integer(), week = integer(), p_home_mkt_2w_spread = numeric())
  if (!is.na(sp_col)) {
    spreads_df <- sched_df %>%
      transmute(
        game_id, season, week,
        home_spread = suppressWarnings(as.numeric(.data[[sp_col]]))
      )

    if (is.null(spread_mapper)) {
      spread_mapper <- learn_spread_map(sched_df)
    }

    if (!is.null(spread_mapper)) {
      spread_tbl <- spreads_df %>%
        mutate(p_home_mkt_2w_spread = spread_mapper$predict(home_spread)) %>%
        select(game_id, season, week, p_home_mkt_2w_spread)
    } else {
      SD_MARGIN <- 13.86
      spread_tbl <- spreads_df %>%
        filter(is.finite(home_spread)) %>%
        mutate(p_home_mkt_2w_spread = .clp(pnorm(-home_spread / SD_MARGIN))) %>%
        select(game_id, season, week, p_home_mkt_2w_spread)
    }
  }

  fallback_tbl <- base %>%
    left_join(spread_tbl, by = c("game_id", "season", "week")) %>%
    left_join(ml_tbl, by = c("game_id", "season", "week")) %>%
    mutate(p_home_mkt_2w = dplyr::coalesce(p_home_mkt_2w_ml, p_home_mkt_2w_spread)) %>%
    select(game_id, season, week, p_home_mkt_2w)

  if (is.null(lines_df)) {
    seasons_lines <- sort(unique(base$season))
    lines_df <- safe_load_lines(seasons_lines)
  }

  provider_tbl <- market_probs_from_lines(lines_df, sched_df, provider = provider)

  out <- fallback_tbl
  if (!nrow(out)) {
    out <- base %>% mutate(p_home_mkt_2w = NA_real_)
  }

  if (nrow(provider_tbl)) {
    out <- out %>%
      left_join(provider_tbl %>% rename(p_home_mkt_2w_provider = p_home_mkt_2w),
                by = c("game_id", "season", "week")) %>%
      mutate(p_home_mkt_2w = dplyr::coalesce(p_home_mkt_2w_provider, p_home_mkt_2w)) %>%
      select(-p_home_mkt_2w_provider)
  }

  if (!"p_home_mkt_2w" %in% names(out)) {
    out$p_home_mkt_2w <- NA_real_
  }

  if (!any(is.finite(out$p_home_mkt_2w))) {
    if (!nrow(fallback_tbl) && !nrow(provider_tbl)) {
      stop("No usable moneyline or spread columns found to derive market probabilities.")
    }
  }

  out
}

# ------------------ Metrics + week-block bootstrap ----------------------------
brier <- function(p,y) mean((.clp(p)-y)^2, na.rm = TRUE)
logloss <- function(p,y) { p <- .clp(p); -mean(y*log(p) + (1-y)*log(1-p), na.rm = TRUE) }

score_metrics <- function(p, y) {
  keep <- is.finite(p) & is.finite(y)
  if (!any(keep)) {
    return(list(brier = NA_real_, logloss = NA_real_))
  }
  p_use <- .clp(p[keep])
  y_use <- y[keep]
  list(
    brier = mean((p_use - y_use)^2),
    logloss = -mean(y_use * log(p_use) + (1 - y_use) * log(1 - p_use))
  )
}

bootstrap_week_ci <- function(df, p_col_model, p_col_mkt, y_col = "y2",
                              n_boot = 2000, seed = 42, model_label = "Model") {
  n_boot <- suppressWarnings(as.integer(n_boot[1]))
  if (!is.finite(n_boot) || n_boot < 1L) {
    stop("n_boot must be at least 1")
  }

  required_cols <- c("season", "week", p_col_model, p_col_mkt, y_col)
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols)) {
    stop(sprintf(
      "bootstrap_week_ci is missing required columns: %s",
      paste(missing_cols, collapse = ", ")
    ))
  }

  p_model <- suppressWarnings(as.numeric(df[[p_col_model]]))
  p_mkt   <- suppressWarnings(as.numeric(df[[p_col_mkt]]))
  y_val   <- suppressWarnings(as.numeric(df[[y_col]]))

  valid <- is.finite(p_model) & is.finite(p_mkt) & is.finite(y_val)
  if (!any(valid)) {
    return(tibble(
      metric = c(sprintf("Brier (%s - Market)", model_label),
                 sprintf("LogLoss (%s - Market)", model_label)),
      delta = NA_real_,
      lo = NA_real_,
      hi = NA_real_,
      verdict = "TIE"
    ))
  }

  idx_valid <- which(valid)
  week_groups <- split(
    idx_valid,
    interaction(df$season[idx_valid], df$week[idx_valid], drop = TRUE, lex.order = TRUE)
  )

  n_groups <- length(week_groups)
  if (n_groups == 0L) {
    return(tibble(
      metric = c(sprintf("Brier (%s - Market)", model_label),
                 sprintf("LogLoss (%s - Market)", model_label)),
      delta = NA_real_,
      lo = NA_real_,
      hi = NA_real_,
      verdict = "TIE"
    ))
  }

  seed_num <- suppressWarnings(as.numeric(seed[1]))
  if (is.finite(seed_num)) {
    set.seed(seed_num)
  }

  Brier_d <- numeric(n_boot)
  LogL_d  <- numeric(n_boot)

  for (b in seq_len(n_boot)) {
    draw <- sample.int(n_groups, n_groups, replace = TRUE)
    rows <- unlist(week_groups[draw], use.names = FALSE)
    if (!length(rows)) {
      Brier_d[b] <- NA_real_
      LogL_d[b] <- NA_real_
      next
    }
    Brier_d[b] <- brier(p_model[rows], y_val[rows]) - brier(p_mkt[rows], y_val[rows])
    LogL_d[b]  <- logloss(p_model[rows], y_val[rows]) - logloss(p_mkt[rows], y_val[rows])
  }

  ci <- function(v) {
    if (!any(is.finite(v))) {
      return(c(NA_real_, NA_real_))
    }
    stats::quantile(v, c(0.025, 0.975), na.rm = TRUE)
  }

  mean_safe <- function(v) {
    if (!any(is.finite(v))) {
      return(NA_real_)
    }
    mean(v[is.finite(v)])
  }

  brier_ci <- ci(Brier_d)
  logl_ci  <- ci(LogL_d)

  lo_vals <- c(brier_ci[1], logl_ci[1])
  hi_vals <- c(brier_ci[2], logl_ci[2])

  verdicts <- ifelse(
    is.finite(hi_vals) & hi_vals < 0,
    "WIN",
    ifelse(is.finite(lo_vals) & lo_vals > 0, "LOSE", "TIE")
  )

  tibble(
    metric = c(sprintf("Brier (%s - Market)", model_label),
               sprintf("LogLoss (%s - Market)", model_label)),
    delta  = c(mean_safe(Brier_d), mean_safe(LogL_d)),
    lo     = lo_vals,
    hi     = hi_vals,
    verdict = verdicts
  )
}

# ------------------ Assemble evaluation dataset -------------------------------
stopifnot("per_game" %in% names(res))

extra_blend_sources <- list()
if (exists("final") && inherits(final, "data.frame")) {
  extra_blend_sources <- c(extra_blend_sources, list(final))
}
if (exists("blend_oos") && inherits(blend_oos, "data.frame")) {
  extra_blend_sources <- c(extra_blend_sources, list(blend_oos))
}

res$per_game <- derive_blend_probability(res$per_game, extra_blend_sources = extra_blend_sources)

if (!"p_blend" %in% names(res$per_game)) {
  stop("res$per_game must include a 'p_blend' column containing blended probabilities.")
}

market_prob_col <- pick_col(res$per_game, c("p_home_mkt_2w","p_mkt","market_prob_home","p_mkt_2w","home_p_mkt","p2_market","market_p_home"))

eval_df <- res$per_game %>%
  # keep just what we need
  transmute(
    game_id, season, week,
    p_blend = .clp(p_blend),
    p_mkt_res = if (!is.na(market_prob_col)) .clp(.data[[market_prob_col]]) else NA_real_
  ) %>%
  inner_join(outcomes, by = c("game_id","season","week"))

# Market probs (closing ML preferred; else spread mapping)
spread_mapper <- learn_spread_map(sched)  # may return NULL -> script falls back gracefully
seasons_eval <- sort(unique(eval_df$season))
lines_eval <- safe_load_lines(seasons_eval)
mkt_df <- market_probs_from_sched(sched, spread_mapper = spread_mapper, lines_df = lines_eval, provider = MARKET_PROVIDER)

comp <- eval_df %>%
  left_join(mkt_df, by = c("game_id","season","week")) %>%
  mutate(
    p_mkt = dplyr::coalesce(
      if ("p_home_mkt_2w" %in% names(.)) .clp(p_home_mkt_2w) else NA_real_,
      .clp(p_mkt_res)
    ),
    prob_model = .clp(p_blend),
    prob_blend = .clp(p_blend)
  ) %>%
  filter(is.finite(p_mkt))

prob_has_blend <- any(is.finite(comp$prob_blend))
prob_source <- "p_blend"
model_label <- "Blend"
comp <- comp %>%
  mutate(
    prob_eval = prob_model
  )

metrics_eval <- score_metrics(comp$prob_eval, comp$y2)
fmt_metric <- function(x) ifelse(is.finite(x), sprintf("%.6f", x), "NA")
message(sprintf(
  "Best-bet probabilities sourced from %s column (Brier=%s, LogLoss=%s).",
  model_label, fmt_metric(metrics_eval$brier), fmt_metric(metrics_eval$logloss)
))

preferred_final_prob_col <- "home_p_2w_blend"

# ------------------ Print headline table (preferred vs Market) ---------------
overall_weekly <- comp %>%
  group_by(season, week) %>%
  summarise(
    n_games = n(),
    Brier_model = brier(prob_model, y2),
    LogL_model  = logloss(prob_model, y2),
    Brier_blend = if (prob_has_blend) brier(prob_blend, y2) else NA_real_,
    LogL_blend  = if (prob_has_blend) logloss(prob_blend, y2) else NA_real_,
    Brier_mkt   = brier(p_mkt,   y2),
    LogL_mkt    = logloss(p_mkt,   y2),
    .groups = "drop"
  )

overall_tbl <- overall_weekly %>%
  summarise(
    n_weeks = n(),
    Brier_model = stats::weighted.mean(Brier_model, n_games, na.rm = TRUE),
    LogL_model  = stats::weighted.mean(LogL_model,  n_games, na.rm = TRUE),
    Brier_blend = if (prob_has_blend) stats::weighted.mean(Brier_blend, n_games, na.rm = TRUE) else NA_real_,
    LogL_blend  = if (prob_has_blend) stats::weighted.mean(LogL_blend,  n_games, na.rm = TRUE) else NA_real_,
    Brier_mkt   = stats::weighted.mean(Brier_mkt,   n_games, na.rm = TRUE),
    LogL_mkt    = stats::weighted.mean(LogL_mkt,    n_games, na.rm = TRUE),
    Brier_model_median = stats::median(Brier_model, na.rm = TRUE),
    LogL_model_median  = stats::median(LogL_model, na.rm = TRUE),
    Brier_blend_median = if (prob_has_blend) stats::median(Brier_blend, na.rm = TRUE) else NA_real_,
    LogL_blend_median  = if (prob_has_blend) stats::median(LogL_blend, na.rm = TRUE) else NA_real_,
    Brier_mkt_median   = stats::median(Brier_mkt, na.rm = TRUE),
    LogL_mkt_median    = stats::median(LogL_mkt, na.rm = TRUE),
    n_games = sum(n_games),
    .groups = "drop"
  ) %>%
  mutate(
    Brier_model_delta = Brier_model - Brier_mkt,
    LogL_model_delta  = LogL_model  - LogL_mkt,
    Brier_blend_delta = if (prob_has_blend) Brier_blend - Brier_mkt else NA_real_,
    LogL_blend_delta  = if (prob_has_blend) LogL_blend  - LogL_mkt else NA_real_,
    Brier_model_delta_median = Brier_model_median - Brier_mkt_median,
    LogL_model_delta_median  = LogL_model_median  - LogL_mkt_median,
    Brier_blend_delta_median = if (prob_has_blend) Brier_blend_median - Brier_mkt_median else NA_real_,
    LogL_blend_delta_median  = if (prob_has_blend) LogL_blend_median  - LogL_mkt_median else NA_real_
  )

if (prob_has_blend) {
  overall_tbl <- overall_tbl %>%
    mutate(
      Brier_model = Brier_blend,
      LogL_model = LogL_blend,
      Brier_model_delta = Brier_blend_delta,
      LogL_model_delta = LogL_blend_delta,
      Brier_model_median = Brier_blend_median,
      LogL_model_median = LogL_blend_median,
      Brier_model_delta_median = Brier_blend_delta_median,
      LogL_model_delta_median = LogL_blend_delta_median
    )
}

overall_display_label <- model_label

overall_tbl <- overall_tbl %>%
  select(any_of(c(
    "n_weeks", "n_games",
    "Brier_model", "Brier_mkt",
    "Brier_model_delta",
    "Brier_model_median", "Brier_mkt_median",
    "Brier_model_delta_median",
    "LogL_model", "LogL_mkt",
    "LogL_model_delta",
    "LogL_model_median", "LogL_mkt_median",
    "LogL_model_delta_median"
  )))

message(sprintf("\n=== Overall (%s vs Market) ===", overall_display_label))
print(overall_tbl)

ci_tbl_model <- bootstrap_week_ci(comp, p_col_model = "prob_model", p_col_mkt = "p_mkt", n_boot = N_BOOT, model_label = "Model")
ci_tbl_blend <- if (prob_has_blend) {
  bootstrap_week_ci(comp, p_col_model = "prob_blend", p_col_mkt = "p_mkt", n_boot = N_BOOT, model_label = "Blend")
} else {
  tibble::tibble()
}

ci_tbl <- dplyr::bind_rows(ci_tbl_model, ci_tbl_blend)
ci_display_label <- if (prob_has_blend) "Model & Blend" else model_label
message(sprintf("\n=== Week-block bootstrap CI (%s – Market) ===", ci_display_label))
print(ci_tbl)

overall_gt <- NULL
ci_gt <- NULL

if (gt_available) {
  num_cols <- names(overall_tbl)[vapply(overall_tbl, is.numeric, logical(1))]
  overall_gt <- overall_tbl %>%
    gt::gt() %>%
    gt::fmt_number(columns = setdiff(num_cols, c("n_weeks", "n_games")), decimals = 6) %>%
    gt::fmt_number(columns = c("n_weeks", "n_games"), decimals = 0) %>%
    gt::tab_spanner(
      label = "Brier (Mean)",
      columns = intersect(c("Brier_model", "Brier_blend", "Brier_mkt"), names(overall_tbl))
    ) %>%
    gt::tab_spanner(
      label = "Brier Δ (Mean)",
      columns = intersect(c("Brier_model_delta", "Brier_blend_delta"), names(overall_tbl))
    ) %>%
    gt::tab_spanner(
      label = "Brier (Median)",
      columns = intersect(c("Brier_model_median", "Brier_blend_median", "Brier_mkt_median"), names(overall_tbl))
    ) %>%
    gt::tab_spanner(
      label = "Brier Δ (Median)",
      columns = intersect(c("Brier_model_delta_median", "Brier_blend_delta_median"), names(overall_tbl))
    ) %>%
    gt::tab_spanner(
      label = "LogLoss (Mean)",
      columns = intersect(c("LogL_model", "LogL_blend", "LogL_mkt"), names(overall_tbl))
    ) %>%
    gt::tab_spanner(
      label = "LogLoss Δ (Mean)",
      columns = intersect(c("LogL_model_delta", "LogL_blend_delta"), names(overall_tbl))
    ) %>%
    gt::tab_spanner(
      label = "LogLoss (Median)",
      columns = intersect(c("LogL_model_median", "LogL_blend_median", "LogL_mkt_median"), names(overall_tbl))
    ) %>%
    gt::tab_spanner(
      label = "LogLoss Δ (Median)",
      columns = intersect(c("LogL_model_delta_median", "LogL_blend_delta_median"), names(overall_tbl))
    )

  ci_gt <- ci_tbl %>%
    gt::gt() %>%
    gt::fmt_number(columns = c("delta", "lo", "hi"), decimals = 6)
}

# ------------------ Best Bets table for current slate -------------------------
build_best_bets <- function(final_df, sched_df, spread_mapper = NULL, focus_matchup = NULL, line_catalog = NULL,
                            preferred_prob_col = NULL, lines_df = NULL, provider = "ESPN BET") {
  stopifnot(all(c("matchup", "date") %in% names(final_df)))

  if (!"home_p_2w_blend" %in% names(final_df)) {
    stop("final_df must include 'home_p_2w_blend' for best-bet calculations.")
  }

  if (!("home_p_2w_mkt" %in% names(final_df))) {
    final_df$home_p_2w_mkt <- NA_real_
  }

  missing_market <- !is.finite(final_df$home_p_2w_mkt)
  if (any(missing_market)) {
    mkt_now <- tryCatch(
      market_probs_from_sched(sched_df, spread_mapper = spread_mapper, lines_df = lines_df, provider = provider) %>%
        dplyr::select(game_id, p_home_mkt_2w),
      error = function(e) tibble::tibble(game_id = character(), p_home_mkt_2w = numeric())
    )
    if (nrow(mkt_now)) {
      final_df <- final_df %>%
        left_join(mkt_now, by = "game_id") %>%
        mutate(home_p_2w_mkt = dplyr::coalesce(home_p_2w_mkt, p_home_mkt_2w)) %>%
        select(-p_home_mkt_2w)
    }
  }

  spread_col <- pick_col(final_df, c("home_main_spread", "spread_close", "spread", "home_spread"))
  if (!is.na(spread_col)) {
    missing_market <- !is.finite(final_df$home_p_2w_mkt)
    if (any(missing_market)) {
      spreads <- suppressWarnings(as.numeric(final_df[[spread_col]]))
      if (!is.null(spread_mapper)) {
        final_df$home_p_2w_mkt[missing_market] <- spread_mapper$predict(spreads[missing_market])
      } else {
        SD_MARGIN <- 13.86
        final_df$home_p_2w_mkt[missing_market] <- .clp(stats::pnorm(-spreads[missing_market] / SD_MARGIN))
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
  source_map <- c(
    home_p_2w_blend = "Blend"
  )
  source_labels <- source_map[picked$source]
  source_labels[is.na(source_labels)] <- picked$source[is.na(source_labels)]

  final_df <- final_df %>%
    separate(matchup, into = c("away", "home"), sep = " @ ", remove = FALSE) %>%
    mutate(
      date = as.Date(date),
      home_prob_model = .clp(picked$values),
      home_prob_source = source_labels,
      home_prob_mkt   = dplyr::coalesce(.clp(home_p_2w_mkt), NA_real_),
      away_prob_model = 1 - home_prob_model,
      away_prob_mkt   = ifelse(is.finite(home_prob_mkt), 1 - home_prob_mkt, NA_real_),
      home_team = home,
      away_team = away
    )

  bets <- bind_rows(
    final_df %>%
      transmute(game_id, date, matchup, opponent = away_team, team = home_team,
                side_key = "home", model_prob = home_prob_model, market_prob = home_prob_mkt,
                model_source = home_prob_source),
    final_df %>%
      transmute(game_id, date, matchup, opponent = home_team, team = away_team,
                side_key = "away", model_prob = away_prob_model, market_prob = away_prob_mkt,
                model_source = home_prob_source)
  ) %>%
    mutate(
      model_prob = .clp(model_prob),
      market_prob = ifelse(is.finite(market_prob), .clp(market_prob), NA_real_),
      side = paste0(team, " ML"),
      fair_ml_model = prob_to_american(model_prob),
      fair_ml_mkt   = ifelse(is.finite(market_prob), vig_moneyline_from_prob(market_prob, side_key), NA_real_)
    )

  if (!is.null(focus_matchup)) {
    bets <- bets %>% filter(matchup == focus_matchup)
  }

  if (!is.null(line_catalog) && nrow(line_catalog)) {
    ml_best <- line_catalog %>%
      filter(bet_bucket == "Moneyline") %>%
      group_by(game_id, side) %>%
      slice_max(order_by = ev_units, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      transmute(
        game_id,
        side_key = side,
        best_book = book,
        best_odds = ifelse(is.finite(odds), sprintf("%+d", as.integer(round(odds))), NA_character_),
        best_odds_num = odds,
        best_market_prob = market_prob
      )

    bets <- bets %>%
      left_join(ml_best, by = c("game_id", "side_key")) %>%
      mutate(
        market_prob = dplyr::coalesce(best_market_prob, market_prob),
        best_odds_num = dplyr::coalesce(
          best_odds_num,
          ifelse(is.finite(market_prob), vig_moneyline_from_prob(market_prob, side_key), NA_real_)
        ),
        best_odds = ifelse(is.na(best_odds) & is.finite(best_odds_num),
                           sprintf("%+d", as.integer(round(best_odds_num))), best_odds)
      )
  } else {
    bets <- bets %>%
      mutate(
        best_book = provider,
        best_odds_num = ifelse(is.finite(market_prob), vig_moneyline_from_prob(market_prob, side_key), NA_real_),
        best_odds = ifelse(is.finite(best_odds_num), sprintf("%+d", as.integer(round(best_odds_num))), NA_character_)
      )
  }

  bets <- bets %>%
    mutate(
      best_book = ifelse(is.na(best_book) & nzchar(provider), provider, best_book),
      market_prob = ifelse(is.finite(market_prob), .clp(market_prob), NA_real_),
      edge = model_prob - market_prob,
      fair_ml_mkt = ifelse(is.finite(market_prob), vig_moneyline_from_prob(market_prob, side_key), NA_real_),
      ev_units = ifelse(is.finite(best_odds_num), expected_value_units(model_prob, best_odds_num), NA_real_)
    ) %>%
    filter(is.finite(market_prob)) %>%
    arrange(desc(edge))

  bets %>%
    select(game_id, date, matchup, side_key, side, team, opponent, model_prob, market_prob,
           edge, ev_units, best_book, best_odds, best_odds_num, fair_ml_model, fair_ml_mkt,
           model_source)
}

bets_gt <- NULL
bets_table <- tibble::tibble()
best_offers_widget <- NULL

if (exists("final")) {
  seasons_needed <- sort(unique(final$season))
  lines_raw <- safe_load_lines(seasons_needed)
  line_catalog <- build_line_catalog(final, lines_raw, sched, provider = MARKET_PROVIDER)
  best_offers <- best_offer_rows(line_catalog)

  bets <- build_best_bets(final, sched, spread_mapper, FOCUS_MATCHUP, line_catalog,
                          preferred_prob_col = preferred_final_prob_col, lines_df = lines_raw,
                          provider = MARKET_PROVIDER)
  message("\n=== Best Bets (current slate) ===")
  if (nrow(bets)) {
    bets_table <- bets %>%
      mutate(
        `Side` = side,
        `Edge` = edge,
        `Model` = model_prob,
        `Market` = market_prob,
        `Best Odds` = best_odds_num,
        `Best Book` = best_book,
        `EV (1u)` = ev_units,
        `Fair ML (Model)` = fair_ml_model,
        `Fair ML (Market)` = fair_ml_mkt,
        `Model Source` = model_source,
        `Value Tier` = dplyr::case_when(
          `EV (1u)` >= 0.05 ~ "High Value",
          `EV (1u)` >= 0.02 ~ "Worth Watching",
          `EV (1u)` >= 0     ~ "Lean",
          TRUE ~ "Pass"
        )
      ) %>%
      select(date, matchup, `Side`, `Edge`, `Model`, `Market`, `Best Odds`, `Best Book`, `EV (1u)`,
             `Value Tier`, `Fair ML (Model)`, `Fair ML (Market)`, `Model Source`)

    edge_domain <- range(bets_table$`Edge`, na.rm = TRUE)
    if (!all(is.finite(edge_domain))) edge_domain <- c(-0.1, 0.1)
    edge_domain <- c(min(edge_domain[1], -0.1), max(edge_domain[2], 0.1))
    palette_edge <- scales::col_numeric(c("#0b1d3a", "#123a63", "#1f4f85", "#2f6ca6", "#3f82c9"), domain = edge_domain)
    tier_colors <- c(
      "High Value" = "#1e3a8a",
      "Worth Watching" = "#1d4ed8",
      "Lean" = "#2563eb",
      "Pass" = "#3b82f6"
    )

    if (gt_available) {
      bets_gt <- bets_table %>%
        gt::gt() %>%
        gt::tab_header(
          title = gt::md("**Best Bets vs Market**"),
          subtitle = "Moneyline focus"
        ) %>%
        gt::fmt_date(columns = date) %>%
        gt::fmt_percent(columns = c(`Edge`, `Model`, `Market`), decimals = 1) %>%
        gt::fmt_number(columns = `EV (1u)`, decimals = 3, drop_trailing_zeros = TRUE) %>%
        gt::fmt(
          columns = c(`Best Odds`, `Fair ML (Model)`, `Fair ML (Market)`),
          fns = function(x) ifelse(is.na(x), "—", sprintf("%+d", as.integer(round(x))))
        ) %>%
        gt::data_color(
          columns = `Edge`,
          colors = function(x) palette_edge(scales::squish(x, edge_domain))
        ) %>%
        gt::text_transform(
          locations = gt::cells_body(columns = `Value Tier`),
          fn = function(x) {
            purrr::map_chr(x, function(val) {
              col <- tier_colors[[val]]
              if (is.null(col)) col <- "#6c757d"
              glue::glue("<span style=\"display:inline-block;padding:4px 8px;border-radius:12px;background:{col};color:white;font-weight:600;\">{val}</span>")
            })
          }
        )

      print(bets_gt)
    } else {
      print(bets_table)
    }
  } else {
    message("No moneyline opportunities surfaced for the current slate.")
  }

  if (reactable_available && nrow(best_offers)) {
    message("\n=== Best Available Odds (spread & alt) ===")
    ev_domain <- range(best_offers$ev_units, na.rm = TRUE)
    if (!all(is.finite(ev_domain))) ev_domain <- c(-0.05, 0.05)
    ev_domain <- c(min(ev_domain[1], -0.05), max(ev_domain[2], 0.05))
    palette_ev <- scales::col_numeric("RdYlGn", domain = ev_domain)

    best_offers_widget <- reactable::reactable(
      best_offers %>%
        mutate(date = as.Date(date)),
      searchable = TRUE,
      defaultPageSize = 20,
      pagination = TRUE,
      defaultSorted = list("date" = "asc"),
      defaultColDef = reactable::colDef(align = "center"),
      columns = list(
        date = reactable::colDef(name = "Date", sticky = "left", format = reactable::colFormat(date = TRUE)),
        matchup = reactable::colDef(name = "Matchup", sticky = "left", align = "left"),
        bet_bucket = reactable::colDef(name = "Bet Type"),
        selection = reactable::colDef(name = "Selection", align = "left", sticky = "left"),
        odds_fmt = reactable::colDef(name = "Odds"),
        book = reactable::colDef(name = "Book"),
        model_prob = reactable::colDef(name = "Model %", format = reactable::colFormat(percent = TRUE, digits = 1)),
        market_prob = reactable::colDef(name = "Market %", format = reactable::colFormat(percent = TRUE, digits = 1)),
        edge = reactable::colDef(name = "Edge %", format = reactable::colFormat(percent = TRUE, digits = 1),
                      cell = function(value) {
                        col <- palette_ev(value)
                        list(style = list(background = col, color = ifelse(value > 0.04, "white", "black")))
                      }),
        ev_units = reactable::colDef(name = "EV (1u)", format = reactable::colFormat(digits = 3),
                          cell = function(value) {
                            col <- palette_ev(value)
                            list(style = list(background = col, color = ifelse(value > 0.02, "white", "black")))
                          })
      ),
      highlight = TRUE
    )
    print(best_offers_widget)
  } else if (nrow(best_offers)) {
    message("Best available odds table skipped because 'reactable' is unavailable in this environment.")
  } else {
    message("No sportsbook lines available via nflreadr::load_lines for the requested slate.")
  }
} else {
  message("\nNo `final` object found. To see Best Bets, run your simulation first (or save `final` to RDS and set FINAL_RDS).")
}

# ------------------ Plain “win/lose” verdict line (nice & loud) ---------------
verdict_line <- function(metric, delta, lo, hi, verdict, digits = 4, ...) {
  fmt <- function(x) ifelse(is.finite(x), formatC(x, format = "f", digits = digits), "NA")
  sprintf("%s: Δ=%s  (95%% CI: [%s, %s])  → %s",
          metric, fmt(delta), fmt(lo), fmt(hi), verdict)
}
message(sprintf("\n=== Verdicts (%s – Market) ===", model_label))
if (nrow(ci_tbl)) {
  purrr::pwalk(ci_tbl, function(metric, delta, lo, hi, verdict, ...) {
    message(verdict_line(metric, delta, lo, hi, verdict))
  })
}

if (htmltools_available) {
  preformatted_df <- function(df) {
    htmltools::tags$pre(paste0(utils::capture.output(print(df)), collapse = "\n"))
  }

  report_sections <- htmltools::tagList(htmltools::tags$h1("NFL Model vs Market"))

  report_sections <- htmltools::tagAppendChildren(
    report_sections,
    htmltools::tags$h2(sprintf("Overall (%s vs Market)", model_label)),
    if (!is.null(overall_gt)) htmltools::HTML(gt::as_raw_html(overall_gt)) else preformatted_df(overall_tbl)
  )

  report_sections <- htmltools::tagAppendChildren(
    report_sections,
    htmltools::tags$h2(sprintf("Week-block bootstrap CI (%s – Market)", model_label)),
    if (!is.null(ci_gt)) htmltools::HTML(gt::as_raw_html(ci_gt)) else preformatted_df(ci_tbl)
  )

  if (!is.null(bets_gt)) {
    report_sections <- htmltools::tagAppendChildren(
      report_sections,
      htmltools::tags$h2("Best Bets vs Market"),
      htmltools::HTML(gt::as_raw_html(bets_gt))
    )
  } else if (exists("bets_table") && nrow(bets_table)) {
    report_sections <- htmltools::tagAppendChildren(
      report_sections,
      htmltools::tags$h2("Best Bets vs Market"),
      preformatted_df(bets_table)
    )
  }

  if (!is.null(best_offers_widget)) {
    report_sections <- htmltools::tagAppendChildren(
      report_sections,
      htmltools::tags$h2("Best Available Odds"),
      best_offers_widget
    )
  } else if (exists("best_offers") && nrow(best_offers)) {
    report_sections <- htmltools::tagAppendChildren(
      report_sections,
      htmltools::tags$h2("Best Available Odds"),
      preformatted_df(best_offers)
    )
  }

  report_file <- file.path(getwd(), "NFLvsmarket_report.html")
  if (htmlwidgets_available) {
    htmlwidgets::saveWidget(htmlwidgets::browsable(report_sections), report_file, selfcontained = TRUE)
  } else {
    htmltools::save_html(report_sections, file = report_file)
  }
  message(sprintf("Saved HTML report to: %s", report_file))
  try(utils::browseURL(report_file), silent = TRUE)
} else {
  html_escape <- function(text) {
    text <- as.character(text)
    text <- gsub("&", "&amp;", text, fixed = TRUE)
    text <- gsub("<", "&lt;", text, fixed = TRUE)
    text <- gsub(">", "&gt;", text, fixed = TRUE)
    text <- gsub('"', "&quot;", text, fixed = TRUE)
    text
  }

  df_to_html_table <- function(df, classes = "data-table", row_class = NULL, escape_cols = names(df)) {
    if (!nrow(df)) return(character())
    headers <- paste0("<tr>", paste0(sprintf("<th>%s</th>", html_escape(names(df))), collapse = ""), "</tr>")
    rows <- purrr::map_chr(seq_len(nrow(df)), function(i) {
      cells <- purrr::map_chr(seq_along(df), function(j) {
        col_name <- names(df)[j]
        raw_value <- df[[j]][i]
        if (is.na(raw_value)) {
          value <- "—"
        } else {
          value <- as.character(raw_value)
          if (!nzchar(value)) value <- "—"
        }
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
    c("<div class=\"table-container\">", sprintf("<table class=\"%s\">", classes), headers, rows, "</table>", "</div>")
  }

  format_overall_for_html <- function(df) {
    if (!nrow(df)) return(df)
    df_fmt <- df
    num_cols <- setdiff(names(df_fmt)[vapply(df_fmt, is.numeric, logical(1))], c("n_weeks", "n_games"))
    if (length(num_cols)) {
      df_fmt <- df_fmt %>%
        mutate(across(all_of(num_cols), ~ ifelse(is.na(.), NA_character_, formatC(., format = "f", digits = 6))))
    }
    for (col in intersect(c("n_weeks", "n_games"), names(df_fmt))) {
      df_fmt[[col]] <- ifelse(is.na(df[[col]]), "—", formatC(df[[col]], format = "d"))
    }
    df_fmt %>% mutate(across(everything(), as.character))
  }

  format_ci_for_html <- function(df) {
    if (!nrow(df)) return(df)
    df %>% mutate(
      across(where(is.numeric), ~ ifelse(is.na(.), NA_character_, formatC(., format = "f", digits = 6)))
    ) %>% mutate(across(everything(), as.character))
  }

  format_best_bets_for_html <- function(df) {
    if (!nrow(df)) return(df)
    df %>%
      mutate(
        date = format(as.Date(date), "%a %b %d"),
        `Edge` = ifelse(is.na(`Edge`), NA_character_, scales::percent(`Edge`, accuracy = 0.1)),
        `Model` = ifelse(is.na(`Model`), NA_character_, scales::percent(`Model`, accuracy = 0.1)),
        `Market` = ifelse(is.na(`Market`), NA_character_, scales::percent(`Market`, accuracy = 0.1)),
        `Best Odds` = ifelse(is.na(`Best Odds`), "—", sprintf("%+d", as.integer(round(`Best Odds`)))),
        `EV (1u)` = ifelse(is.na(`EV (1u)`), NA_character_, formatC(`EV (1u)`, format = "f", digits = 3)),
        `Fair ML (Model)` = ifelse(is.na(`Fair ML (Model)`), "—", sprintf("%+d", as.integer(round(`Fair ML (Model)`)))),
        `Fair ML (Market)` = ifelse(is.na(`Fair ML (Market)`), "—", sprintf("%+d", as.integer(round(`Fair ML (Market)`)))),
        `Model Source` = ifelse(is.na(`Model Source`), "—", `Model Source`),
        `Value Tier` = dplyr::case_when(
          `Value Tier` == "High Value" ~ "<span class=\"tag tier-high\">High Value</span>",
          `Value Tier` == "Worth Watching" ~ "<span class=\"tag tier-medium\">Worth Watching</span>",
          `Value Tier` == "Lean" ~ "<span class=\"tag tier-lean\">Lean</span>",
          `Value Tier` == "Pass" ~ "<span class=\"tag tier-pass\">Pass</span>",
          TRUE ~ "<span class=\"tag tier-unknown\">N/A</span>"
        ),
        tier_class = dplyr::case_when(
          grepl("High Value", `Value Tier`) ~ "tier-row-high",
          grepl("Worth Watching", `Value Tier`) ~ "tier-row-medium",
          grepl("Lean", `Value Tier`) ~ "tier-row-lean",
          grepl("Pass", `Value Tier`) ~ "tier-row-pass",
          TRUE ~ ""
        )
      ) %>%
      mutate(across(everything(), as.character))
  }

  format_best_offers_for_html <- function(df) {
    if (!nrow(df)) return(df)
    df %>%
      mutate(
        date = format(as.Date(date), "%a %b %d"),
        model_prob = ifelse(is.na(model_prob), NA_character_, scales::percent(model_prob, accuracy = 0.1)),
        market_prob = ifelse(is.na(market_prob), NA_character_, scales::percent(market_prob, accuracy = 0.1)),
        odds = ifelse(is.na(odds_fmt), "—", odds_fmt),
        edge = ifelse(is.na(edge), NA_character_, scales::percent(edge, accuracy = 0.1)),
        ev_units = ifelse(is.na(ev_units), NA_character_, formatC(ev_units, format = "f", digits = 3))
      ) %>%
      select(date, matchup, bet_bucket, selection, odds, book, model_prob, market_prob, edge, ev_units, everything(), -odds_fmt) %>%
      mutate(across(everything(), as.character))
  }

  bets_table_fmt <- NULL
  bets_row_classes <- NULL
  bets_grouped_html <- character()

  if (exists("bets_table") && nrow(bets_table)) {
    bets_table_fmt <- format_best_bets_for_html(bets_table)
    bets_row_classes <- bets_table_fmt$tier_class
    bets_table_fmt <- bets_table_fmt %>% select(-tier_class)

    bet_split <- split(bets_table_fmt, paste0(bets_table_fmt$date, " · ", bets_table_fmt$matchup))
    bets_row_split <- split(bets_row_classes, paste0(bets_table_fmt$date, " · ", bets_table_fmt$matchup))
    bets_grouped_html <- purrr::imap(bet_split, function(df_sub, key) {
      df_render <- df_sub %>% select(-matchup)
      rows <- df_to_html_table(df_render, classes = "data-table bets-table", row_class = bets_row_split[[key]], escape_cols = setdiff(names(df_render), "Value Tier"))
      c(sprintf("<div class=\"game-section\"><h3>%s</h3>", html_escape(key)), rows, "</div>")
    }) %>% purrr::flatten_chr()
  }

  best_offers_fmt <- NULL
  if (exists("best_offers") && nrow(best_offers)) {
    best_offers_fmt <- format_best_offers_for_html(best_offers)
  }

  html_lines <- c(
    "<!DOCTYPE html>",
    "<html>",
    "<head>",
    "<meta charset=\"UTF-8\">",
    "<title>NFL Model vs Market</title>",
    "<style>",
    "body { font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif; margin: 0; padding: 0; background: #05070f; color: #eef2f8; line-height: 1.6; }",
    "header { background: linear-gradient(135deg, #05070f, #0f1c2e); padding: 32px 5vw 24px; box-shadow: 0 4px 16px rgba(0,0,0,0.5); }",
    "header h1 { margin: 0; font-size: 2.4rem; letter-spacing: 0.05em; text-transform: uppercase; color: #e7edf7; }",
    "main { padding: 24px 5vw 60px; max-width: 1200px; margin: 0 auto; box-sizing: border-box; }",
    "section { margin-bottom: 32px; background: rgba(9,16,30,0.92); border-radius: 18px; padding: 24px; box-shadow: 0 10px 24px rgba(0,0,0,0.28); border: 1px solid rgba(120,150,210,0.28); }",
    "section h2 { margin-top: 0; font-size: 1.6rem; letter-spacing: 0.04em; text-transform: uppercase; color: #8ab4ff; }",
    "section h3 { margin: 0 0 12px; font-size: 1.25rem; color: #a7c7ff; }",
    ".game-section { margin-bottom: 18px; padding: 16px 18px; background: rgba(6,12,24,0.88); border-radius: 14px; border: 1px solid rgba(120,150,210,0.28); }",
    ".game-section:last-child { margin-bottom: 0; }",
    ".table-container { width: 100%; overflow-x: auto; background: rgba(5,10,20,0.95); border-radius: 14px; border: 1px solid rgba(120,150,210,0.32); padding: 12px; box-sizing: border-box; }",
    "table.data-table { width: 100%; min-width: 580px; border-collapse: collapse; }",
    "table.data-table th { text-align: left; padding: 12px 14px; background: rgba(118,156,220,0.18); text-transform: uppercase; font-size: 0.8rem; letter-spacing: 0.08em; color: #dce6f7; }",
    "table.data-table td { padding: 12px 14px; border-bottom: 1px solid rgba(118,156,220,0.25); font-size: 0.95rem; }",
    "table.data-table tr:nth-child(even) { background: rgba(255,255,255,0.03); }",
    "table.data-table tr:hover { background: rgba(76,120,188,0.2); transition: background 0.2s ease-in-out; }",
    "table.data-table tr.tier-row-high { border-left: 6px solid #1e3a8a; }",
    "table.data-table tr.tier-row-medium { border-left: 6px solid #1d4ed8; }",
    "table.data-table tr.tier-row-lean { border-left: 6px solid #2563eb; }",
    "table.data-table tr.tier-row-pass { border-left: 6px solid #3b82f6; opacity: 0.9; }",
    "span.tag { display: inline-block; padding: 4px 10px; border-radius: 999px; font-size: 0.8rem; font-weight: 600; color: #e7edf7; background: rgba(76,120,188,0.18); text-transform: uppercase; letter-spacing: 0.05em; border: 1px solid rgba(118,156,220,0.35); }",
    ".tier-high { background: rgba(40,83,170,0.9); color: #e7edf7; }",
    ".tier-medium { background: rgba(59,102,191,0.9); color: #e7edf7; }",
    ".tier-lean { background: rgba(76,129,216,0.9); color: #e7edf7; }",
    ".tier-pass { background: rgba(100,149,237,0.9); color: #0b162a; }",
    ".tier-unknown { background: rgba(88,110,150,0.9); color: #f5f7fa; }",
    "@media (max-width: 900px) { table.data-table th, table.data-table td { padding: 10px; font-size: 0.85rem; } section { padding: 18px; } .table-container { padding: 10px; } }",
    "</style>",
    "</head>",
    "<body>",
    "<header><h1>NFL Model vs Market</h1></header>",
    "<main>"
  )

  html_lines <- c(html_lines, "<section>", sprintf("<h2>Overall (%s vs Market)</h2>", html_escape(overall_display_label)), df_to_html_table(format_overall_for_html(overall_tbl)), "</section>")

  html_lines <- c(html_lines, "<section>", sprintf("<h2>Week-block bootstrap CI (%s – Market)</h2>", html_escape(ci_display_label)), df_to_html_table(format_ci_for_html(ci_tbl)), "</section>")

  if (length(bets_grouped_html)) {
    html_lines <- c(html_lines, "<section>", "<h2>Best Bets vs Market</h2>", bets_grouped_html, "</section>")
  }

  if (!is.null(best_offers_fmt) && nrow(best_offers_fmt)) {
    html_lines <- c(html_lines, "<section>", "<h2>Best Available Odds</h2>", df_to_html_table(best_offers_fmt, classes = "data-table offers-table"), "</section>")
  }

  html_lines <- c(html_lines, "</main>", "</body>", "</html>")

  report_file <- file.path(getwd(), "NFLvsmarket_report.html")
  writeLines(html_lines, con = report_file)
  message(sprintf("Saved simplified HTML report to: %s", report_file))
  try(utils::browseURL(report_file), silent = TRUE)
}

