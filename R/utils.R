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

#' Apply vig (juice) to probability for realistic moneyline
#'
#' Converts a true probability to American odds with vig applied.
#' This makes model moneylines comparable to market odds which include juice.
#'
#' @param prob True probability (0-1)
#' @param vig_pct Total vig percentage (e.g., 0.10 for 10% juice)
#' @return American odds with vig applied
#'
#' @details
#' For a 50/50 game with 10% vig, each side shows -110 (52.4% implied).
#' The vig is split between both sides, inflating implied probabilities.
#'
#' Examples:
#' - 50% true prob with 10% vig → -110 (52.4% implied)
#' - 55% true prob with 10% vig → -133 (57.0% implied)
#' - 60% true prob with 10% vig → -162 (61.8% implied)
#'
#' @examples
#' apply_model_vig(0.50, 0.10)  # Returns -110
#' apply_model_vig(0.55, 0.10)  # Returns approximately -133
#' apply_model_vig(0.60, 0.10)  # Returns approximately -162
#'
#' @export
apply_model_vig <- function(prob, vig_pct = 0.10) {
  prob <- clamp_probability(prob)

  # Vig inflates both sides proportionally (10% vig = 110% total implied)
  vig_factor <- 1 + vig_pct
  vigged_prob <- prob * vig_factor

  # Cap at 99% to avoid extreme odds

  vigged_prob <- pmin(vigged_prob, 0.99)

  probability_to_american(vigged_prob)
}

#' Remove vig from American odds to get true probability
#'
#' De-vigs American odds by calculating the overround and adjusting.
#'
#' @param home_odds American odds for home team
#' @param away_odds American odds for away team
#' @return Named list with true home and away probabilities
#'
#' @export
devig_american_odds <- function(home_odds, away_odds) {
  home_implied <- american_to_probability(home_odds)
  away_implied <- american_to_probability(away_odds)

  # Total implied (should be > 1 if vigged)
  total_implied <- home_implied + away_implied

  # De-vig by normalizing
  home_true <- home_implied / total_implied
  away_true <- away_implied / total_implied

  list(
    home_prob = home_true,
    away_prob = away_true,
    overround = total_implied - 1
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

#' Apply ordered betting governance rules
#' @param ev Expected value in units for the displayed side
#' @param prob Win probability for the displayed side
#' @param odds American odds for the displayed side
#' @param min_stake Minimum allowed stake (bankroll fraction)
#' @param kelly_fraction Fractional Kelly multiplier
#' @param max_stake Maximum stake cap (bankroll fraction)
#' @param is_placeholder_odds Optional logical flag for placeholder odds rows
#' @return Tibble with governance decision, reasons, and stake audit columns
apply_bet_governance <- function(ev,
                                 prob,
                                 odds,
                                 min_stake = 0.01,
                                 kelly_fraction = 0.125,
                                 max_stake = 0.02,
                                 is_placeholder_odds = FALSE) {
  prob <- clamp_probability(prob)
  odds <- suppressWarnings(as.numeric(odds))
  ev <- suppressWarnings(as.numeric(ev))

  dec <- american_to_decimal(odds)
  b <- dec - 1
  raw_kelly <- (prob * b - (1 - prob)) / b
  invalid_raw <- is.na(prob) | is.na(dec) | !is.finite(dec) | b <= 0 | abs(b) < 1e-6
  raw_kelly[invalid_raw] <- NA_real_

  capped_stake <- raw_kelly * kelly_fraction
  capped_stake <- pmax(0, pmin(capped_stake, max_stake))
  capped_stake[is.na(capped_stake) | !is.finite(capped_stake)] <- NA_real_

  missing_or_placeholder <- is.na(odds) | !is.finite(odds) | odds == 0 | isTRUE(is_placeholder_odds)
  if (length(missing_or_placeholder) == 1L && length(ev) > 1L) {
    missing_or_placeholder <- rep(missing_or_placeholder, length(ev))
  }

  pass_reason <- dplyr::case_when(
    missing_or_placeholder ~ "Market odds missing/placeholder",
    !is.na(ev) & ev <= 0 ~ "Negative EV",
    is.na(capped_stake) | capped_stake < min_stake ~ "Stake below minimum",
    TRUE ~ ""
  )

  final_stake <- dplyr::if_else(pass_reason == "", capped_stake, 0)
  recommendation <- dplyr::if_else(pass_reason == "", "Bet", "Pass")

  tibble::tibble(
    recommendation = recommendation,
    raw_kelly_pct = raw_kelly,
    capped_stake_pct = capped_stake,
    final_stake_pct = final_stake,
    pass_reason = pass_reason
  )
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
#' @description Adds missing columns to a data frame with specified default values.
#'   Handles NULL and non-dataframe inputs gracefully.
#' @param df Data frame (or NULL, or convertible to tibble)
#' @param defaults Named list of column -> default value
#' @return Data frame with all columns present
#' @examples
#' \dontrun{
#'   df <- ensure_columns_with_defaults(df, list(score = 0, team = NA_character_))
#' }
ensure_columns_with_defaults <- function(df, defaults) {
  # Handle NULL input

  if (is.null(df)) {
    df <- tibble::tibble()
  }
  # Handle non-dataframe input
  if (!inherits(df, "data.frame")) {
    df <- tibble::as_tibble(df)
  }
  # Find missing columns
  missing <- setdiff(names(defaults), names(df))
  if (!length(missing)) {
    return(df)
  }
  # Add missing columns with defaults
  for (col in missing) {
    df[[col]] <- defaults[[col]]
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


# =============================================================================
# Numeric Utility Functions (Canonical Definitions)
# =============================================================================

#' Clamp values to a range
#'
#' @param x Numeric vector
#' @param lo Lower bound (default 0)
#' @param hi Upper bound (default 1)
#' @return Clamped numeric vector
#' @export
clamp <- function(x, lo = 0, hi = 1) {

pmin(pmax(x, lo), hi)
}

#' Safe mean value extraction (handles NA/Inf)
#'
#' Used for score distribution parameters. Returns default if value is
#' non-finite or negative.
#'
#' @param x Numeric value
#' @param default Default value if x is invalid (default 21, league avg points)
#' @return Safe numeric value
#' @export
safe_mu <- function(x, default = 21) {
  ifelse(is.finite(x) & x >= 0, x, default)
}

#' Safe standard deviation extraction (handles NA/Inf)
#'
#' Used for score distribution parameters. Returns default if value is
#' non-finite or too small.
#'
#' @param x Numeric value
#' @param default Default value if x is invalid (default 7, typical score SD)
#' @param min_val Minimum acceptable value (default 5)
#' @return Safe numeric value
#' @export
safe_sd <- function(x, default = 7, min_val = 5) {
  ifelse(is.finite(x) & x >= min_val, x, default)
}
