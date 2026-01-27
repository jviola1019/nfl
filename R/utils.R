# =============================================================================
# NFL Prediction Model - Core Utilities Module
# =============================================================================
# Canonical implementations of shared utility functions.
# All helper functions should be defined here and sourced by other modules.
# =============================================================================

#' @title Standard Probability Epsilon
#' @description Prevents log(0) and division by zero issues
PROB_EPSILON <- 1e-9

#' @title Join Key Aliases
#' @description Canonical names and their common aliases for join operations
JOIN_KEY_ALIASES <- list(
  game_id   = c("game_id", "gameid", "gameId", "gid"),
  season    = c("season", "season_std", "Season", "season_year", "seasonYear", "year"),
  week      = c("week", "week_std", "Week", "game_week", "gameWeek", "gameday_week", "wk"),
  game_type = c("game_type", "gameType", "type", "game_phase")
)

#' @title Prediction Join Keys
#' @description Standard keys used for joining prediction tables
PREDICTION_JOIN_KEYS <- names(JOIN_KEY_ALIASES)

# =============================================================================
# TYPE-SAFE NUMERIC COERCION
# =============================================================================

#' Safely coerce to numeric
#' @param x Vector to coerce
#' @return Numeric vector with NA for non-coercible values
coerce_numeric_safely <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(x))
}

#' Clamp probability to valid range
#' @param p Probability value(s) to clamp
#' @param eps Epsilon for numerical stability
#' @return Clamped probability in [eps, 1-eps]
clamp_probability <- function(p, eps = PROB_EPSILON) {
  p <- suppressWarnings(as.numeric(p))
  pmin(pmax(p, eps), 1 - eps)
}

# =============================================================================
# ODDS CONVERSION FUNCTIONS
# =============================================================================

#' Convert American odds to implied probability
#' @param odds American odds (e.g., -110, +150)
#' @return Implied probability (not de-vigged)
american_to_probability <- function(odds) {
  odds <- suppressWarnings(as.numeric(odds))
  dplyr::case_when(
    is.na(odds) ~ NA_real_,
    !is.finite(odds) ~ NA_real_,
    odds == 0 ~ NA_real_,
    odds < 0 ~ (-odds) / ((-odds) + 100),
    TRUE ~ 100 / (odds + 100)
  )
}

#' Convert American odds to decimal odds
#' @param odds American odds (e.g., -110, +150)
#' @return Decimal odds (e.g., 1.91, 2.50)
american_to_decimal <- function(odds) {
  odds <- suppressWarnings(as.numeric(odds))
  dec <- rep(NA_real_, length(odds))
  valid <- is.finite(odds) & odds != 0
  neg_mask <- valid & odds < 0
  pos_mask <- valid & odds > 0
  dec[neg_mask] <- 1 + 100 / abs(odds[neg_mask])
  dec[pos_mask] <- 1 + odds[pos_mask] / 100
  dec
}

#' Convert decimal odds to American odds
#' @param dec Decimal odds
#' @return American odds
decimal_to_american <- function(dec) {
  dec <- coerce_numeric_safely(dec)
  dplyr::case_when(
    is.na(dec) | dec <= 1 ~ NA_real_,
    dec >= 2 ~ round((dec - 1) * 100),
    TRUE ~ round(-100 / (dec - 1))
  )
}

#' Convert probability to American odds
#' @param prob Probability (0-1)
#' @return American odds
probability_to_american <- function(prob) {
  prob <- clamp_probability(prob)
  dplyr::case_when(
    is.na(prob) ~ NA_real_,
    prob >= 0.5 ~ -round(100 * prob / (1 - prob)),
    TRUE ~ round(100 * (1 - prob) / prob)
  )
}

# =============================================================================
# BETTING & STAKING FUNCTIONS
# =============================================================================

#' Calculate expected value in units
#' @param prob Win probability (will be clamped)
#' @param odds American odds
#' @return EV in units (positive = +EV bet)
expected_value_units <- function(prob, odds) {
  prob <- clamp_probability(prob)
  dec <- american_to_decimal(odds)
  b <- dec - 1
  out <- prob * b - (1 - prob)
  invalid <- is.na(prob) | is.na(dec) | !is.finite(dec) | b <= 0 | abs(b) < 1e-6
  out[invalid] <- NA_real_
  out
}

#' Shrink model probability toward market consensus
#' @param model_prob Model's estimated probability
#' @param market_prob Market-implied probability
#' @param shrinkage Shrinkage factor (0-1). Default 0.6 = 60% market weight
#' @return Blended probability
shrink_probability_toward_market <- function(model_prob, market_prob, shrinkage = 0.6) {
  model_prob <- clamp_probability(model_prob)
  market_prob <- clamp_probability(market_prob)
  shrinkage <- pmax(0, pmin(1, shrinkage))
  shrunk <- (1 - shrinkage) * model_prob + shrinkage * market_prob
  clamp_probability(shrunk)
}

#' Classify edge magnitude for warnings
#' @param edge EV edge as decimal (e.g., 0.15 = 15%)
#' @return Classification string
classify_edge_magnitude <- function(edge) {
  dplyr::case_when(
    is.na(edge) ~ NA_character_,
    edge <= 0 ~ "negative",
    edge <= 0.05 ~ "realistic",
    edge <= 0.10 ~ "optimistic",
    edge <= 0.15 ~ "suspicious",
    TRUE ~ "implausible"
  )
}

#' Calculate conservative Kelly stake with edge skepticism
#' @param prob Estimated win probability
#' @param odds American odds for the bet
#' @param kelly_fraction Fraction of Kelly to use (default 0.125 = 1/8 Kelly)
#' @param max_edge Maximum believable edge (default 0.10 = 10%)
#' @param max_stake Maximum stake as fraction of bankroll (default 0.02 = 2%)
#' @return Conservative stake size
conservative_kelly_stake <- function(prob, odds,
                                     kelly_fraction = 0.125,
                                     max_edge = 0.10,
                                     max_stake = 0.02) {
  prob <- clamp_probability(prob)
  dec <- american_to_decimal(odds)
  b <- dec - 1

  valid_b <- !is.na(b) & is.finite(b) & b > 1e-6
  kelly <- rep(NA_real_, length(prob))
  if (any(valid_b)) {
    kelly[valid_b] <- (prob[valid_b] * b[valid_b] - (1 - prob[valid_b])) / b[valid_b]
  }

  edge_penalty <- dplyr::case_when(
    is.na(kelly) ~ NA_real_,
    kelly <= max_edge ~ 1.0,
    kelly <= max_edge * 2 ~ 0.5,
    kelly <= max_edge * 3 ~ 0.25,
    TRUE ~ 0.1
  )

  stake <- kelly * kelly_fraction * edge_penalty
  stake <- pmax(0, pmin(stake, max_stake))
  stake[is.na(stake) | !is.finite(stake)] <- NA_real_
  stake
}

# =============================================================================
# VALIDATION METRICS
# =============================================================================

#' Calculate Brier score
#' @param pred_prob Predicted probabilities
#' @param actual_outcome Actual outcomes (0 or 1)
#' @return Brier score (lower is better)
brier_score <- function(pred_prob, actual_outcome) {
  valid <- !is.na(pred_prob) & !is.na(actual_outcome)
  if (sum(valid) == 0) return(NA_real_)
  mean((pred_prob[valid] - actual_outcome[valid])^2)
}

#' Calculate log loss
#' @param pred_prob Predicted probabilities
#' @param actual_outcome Actual outcomes (0 or 1)
#' @param eps Epsilon to prevent log(0) (default: PROB_EPSILON = 1e-9)
#' @return Log loss (lower is better)
log_loss <- function(pred_prob, actual_outcome, eps = PROB_EPSILON) {
  pred_prob <- pmax(pmin(pred_prob, 1 - eps), eps)
  valid <- !is.na(pred_prob) & !is.na(actual_outcome)
  if (sum(valid) == 0) return(NA_real_)
  pred <- pred_prob[valid]
  actual <- actual_outcome[valid]
  -mean(actual * log(pred) + (1 - actual) * log(1 - pred))
}

#' Calculate accuracy
#' @param pred_prob Predicted probabilities
#' @param actual_outcome Actual outcomes (0 or 1)
#' @param threshold Decision threshold (default 0.5)
#' @return Accuracy (higher is better)
accuracy <- function(pred_prob, actual_outcome, threshold = 0.5) {
  valid <- !is.na(pred_prob) & !is.na(actual_outcome)
  if (sum(valid) == 0) return(NA_real_)
  mean((pred_prob[valid] > threshold) == actual_outcome[valid])
}

# =============================================================================
# TYPE-SAFE JOIN UTILITIES
# =============================================================================

#' Standardize join keys with type coercion
#' @description Renames columns to canonical names AND coerces to standard types:
#'   - game_id: character
#'   - season: integer
#'   - week: integer
#'   - game_type: character (REG, POST, WC, DIV, CON, SB)
#' @param df Data frame to standardize
#' @param key_alias List mapping canonical names to aliases
#' @return Data frame with standardized, typed join keys
standardize_join_keys <- function(df, key_alias = JOIN_KEY_ALIASES) {
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

#' Validate join key overlap between two data frames
#' @param x First data frame
#' @param y Second data frame
#' @param keys Join key columns
#' @param label Label for error messages
#' @return TRUE if valid, stops with error otherwise
validate_join_overlap <- function(x, y, keys, label = "join") {
  if (!all(keys %in% names(x))) {
    missing <- setdiff(keys, names(x))
    stop(sprintf("%s: left table missing keys: %s", label, paste(missing, collapse = ", ")))
  }
  if (!all(keys %in% names(y))) {
    missing <- setdiff(keys, names(y))
    stop(sprintf("%s: right table missing keys: %s", label, paste(missing, collapse = ", ")))
  }

  # Check for overlapping values
  for (key in keys) {
    v1 <- unique(x[[key]])
    v2 <- unique(y[[key]])
    overlap <- length(intersect(as.character(v1), as.character(v2)))
    if (overlap == 0) {
      warning(sprintf("%s: no overlapping values in key '%s' (left: %d unique, right: %d unique)",
                      label, key, length(v1), length(v2)))
    }
  }

  TRUE
}

#' Safe left join with type coercion and diagnostics
#' @param x Left data frame
#' @param y Right data frame
#' @param by Join columns (will be type-coerced)
#' @param label Label for diagnostic messages
#' @return Joined data frame
safe_typed_join <- function(x, y, by, label = "safe_typed_join") {
  # Standardize both sides
  x_std <- standardize_join_keys(x)
  y_std <- standardize_join_keys(y)

  # Validate overlap
  validate_join_overlap(x_std, y_std, by, label)

  # Perform join
  result <- dplyr::left_join(x_std, y_std, by = by)

  if (nrow(result) == 0 && nrow(x_std) > 0) {
    warning(sprintf("%s: join produced zero rows despite %d input rows",
                    label, nrow(x_std)))
  }

  result
}

# =============================================================================
# DATA FRAME UTILITIES
# =============================================================================

#' Get first non-missing value (type-preserving)
#' @param x Vector
#' @return First non-missing value or NA of same type
first_non_missing_typed <- function(x) {
  if (!length(x)) return(x)
  valid_idx <- if (is.numeric(x)) which(is.finite(x)) else which(!is.na(x))
  if (!length(valid_idx)) return(x[NA_integer_])
  x[[valid_idx[1L]]]
}

#' Select first matching column from candidates
#' @param df Data frame
#' @param candidates Vector of candidate column names
#' @return First matching column name or NA
select_first_column <- function(df, candidates) {
  intersect(candidates, names(df))[1]
}

#' Ensure columns exist with defaults
#' @param df Data frame
#' @param defaults Named list of column -> default value
#' @return Data frame with all columns present
ensure_columns_with_defaults <- function(df, defaults) {
  for (col in names(defaults)) {
    if (!col %in% names(df)) {
      df[[col]] <- defaults[[col]]
    }
  }
  df
}

#' Collapse duplicate rows by keys (relaxed)
#' @param df Data frame
#' @param keys Key columns
#' @param label Label for messages
#' @return Data frame with duplicates collapsed
collapse_by_keys_relaxed <- function(df, keys, label = "data frame") {
  if (is.null(df) || !nrow(df) || !length(keys)) return(df)

  missing_keys <- setdiff(keys, names(df))
  if (length(missing_keys)) {
    warning(sprintf("%s: missing key columns %s", label, paste(missing_keys, collapse = ", ")))
    return(df)
  }

  complete_mask <- stats::complete.cases(df[keys])
  df_complete <- df[complete_mask, , drop = FALSE]
  df_incomplete <- df[!complete_mask, , drop = FALSE]

  if (!nrow(df_complete)) return(df)

  dup <- df_complete %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::filter(.data$n > 1L)

  if (!nrow(dup)) return(df)

  message(sprintf("%s: collapsing %d duplicate key combinations", label, nrow(dup)))

  non_key_cols <- setdiff(names(df_complete), keys)

  collapsed <- df_complete %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(non_key_cols), ~ first_non_missing_typed(.x)),
      .groups = "drop"
    )

  dplyr::bind_rows(collapsed, df_incomplete) %>%
    dplyr::select(dplyr::all_of(names(df)))
}

#' Ensure unique join keys
#' @param df Data frame
#' @param keys Key columns
#' @param label Label for messages
#' @return Data frame with unique key combinations
ensure_unique_join_keys <- function(df, keys, label = "data frame") {
  if (is.null(df) || !nrow(df) || !length(keys)) return(df)

  missing_keys <- setdiff(keys, names(df))
  if (length(missing_keys)) return(df)

  dup_keys <- df %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(keys), ~ !is.na(.))) %>%
    dplyr::count(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::filter(.data$n > 1L)

  if (!nrow(dup_keys)) return(df)

  message(sprintf("%s: removing %d duplicate key combinations", label, nrow(dup_keys)))

  df %>%
    dplyr::arrange(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(keys)), .keep_all = TRUE)
}
