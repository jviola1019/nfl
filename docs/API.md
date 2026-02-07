# NFL Prediction Model - API Reference

**Version**: 2.7.0
**Last Updated**: 2026-02-06

---

## Table of Contents

1. [Simulation Engine](#simulation-engine)
2. [Market Analysis](#market-analysis)
3. [Data Validation](#data-validation)
4. [Utility Functions](#utility-functions)
5. [Configuration Parameters](#configuration-parameters)

---

## Simulation Engine

### `simulate_game_nb()`

**File**: [NFLsimulation.R](../NFLsimulation.R#L5192)

Core Monte Carlo simulation engine using negative binomial distributions with Gaussian copula correlation.

```r
simulate_game_nb(
  mu_home,              # Expected home score (required, > 0)
  sd_home,              # Home score SD (required, > 0)
  mu_away,              # Expected away score (required, > 0)
  sd_away,              # Away score SD (required, > 0)
  n_trials = N_TRIALS,  # Monte Carlo iterations (default: 100000)
  rho = RHO_SCORE,      # Score correlation (default: -0.15)
  cap = PTS_CAP_HI,     # Max score cap (default: 70)
  seed = SEED           # Random seed for reproducibility
)
```

**Returns**: List with `home_scores`, `away_scores`, `p_home_win`, `p_away_win`, `p_tie`, `spread_mean`, `spread_sd`, `total_mean`, `total_sd`

**Example**:
```r
result <- simulate_game_nb(24.5, 10.5, 21.0, 10.0, n_trials = 50000)
sprintf("Home win: %.1f%%", result$p_home_win * 100)  # "Home win: 58.3%"
```

---

### `score_one_week()`

**File**: [NFLsimulation.R](../NFLsimulation.R#L6852)

Score predictions for a single completed NFL week.

```r
score_one_week(
  season,        # NFL season year (e.g., 2024)
  week,          # Week number (1-18 regular, 19-22 playoffs)
  trials = 40000 # Monte Carlo trials per game
)
```

**Returns**: List with Brier scores, log loss, and accuracy metrics.

---

### `score_weeks()`

**File**: [NFLsimulation.R](../NFLsimulation.R#L6842)

Score predictions across multiple seasons/weeks with caching.

```r
score_weeks(
  start_season,   # First season (e.g., 2019)
  end_season,     # Last season (e.g., 2024)
  weeks = NULL,   # Specific weeks or NULL for all
  trials = 40000  # Monte Carlo trials per game
)
```

---

### `calc_injury_impacts()`

**File**: [NFLsimulation.R](../NFLsimulation.R#L3320)

Calculate team-level injury impacts using position-weighted factors.

```r
calc_injury_impacts(
  df,                      # Injury data frame
  group_vars = c("team"),  # Grouping columns
  season = NULL            # Optional season filter
)
```

**Position Weights** (validated p < 0.001):
- Skill (QB, RB, WR, TE): Highest impact
- Offensive line (T, G, C): Medium impact
- Defensive front (DE, DT): Medium impact
- Secondary (CB, S, LB): Lower impact

---

## Market Analysis

### `build_moneyline_comparison_table()`

**File**: [NFLmarket.R](../NFLmarket.R#L1766)

Main market comparison engine combining predictions with betting lines.

```r
build_moneyline_comparison_table(
  market_comparison_result,  # Output from compare_to_market()
  enriched_schedule,         # Schedule with market odds
  join_keys = c("game_id", "season", "week"),
  vig = 0.10,               # Market vig assumption (10%)
  verbose = TRUE
)
```

**Returns**: Tibble with shrunk probabilities, EV calculations, and Kelly stakes.

---

### `shrink_probability_toward_market()`

**File**: [R/utils.R](../R/utils.R#L125)

Apply professional shrinkage to model probabilities.

```r
shrink_probability_toward_market(
  model_prob,      # Raw model probability
  market_prob,     # Market-implied probability
  shrinkage = 0.6  # Market weight (default: 60%)
)
```

**Formula**: `shrunk = (1 - shrinkage) * model + shrinkage * market`

**Example**:
```r
# Model: 70% home win, Market: 55% home win
shrunk <- shrink_probability_toward_market(0.70, 0.55, 0.6)
# shrunk = 0.4 * 0.70 + 0.6 * 0.55 = 0.61 (61%)
```

---

### `expected_value_units()`

**File**: [R/utils.R](../R/utils.R#L110)

Calculate expected value in units for a bet.

```r
expected_value_units(
  prob,  # Estimated win probability
  odds   # American odds (e.g., -150, +200)
)
```

**Formula**: `EV = prob * (decimal - 1) - (1 - prob)`

**Example**:
```r
# 60% win probability at +150 odds
ev <- expected_value_units(0.60, 150)
# EV = 0.60 * 2.50 - 1 = 0.50 (50% edge)
```

---

### `conservative_kelly_stake()`

**File**: [R/utils.R](../R/utils.R#L154)

Calculate conservative Kelly stake with edge skepticism.

```r
conservative_kelly_stake(
  prob,                  # Estimated win probability
  odds,                  # American odds
  kelly_fraction = 0.125, # 1/8 Kelly (default)
  max_edge = 0.10,       # Maximum believable edge (10%)
  max_stake = 0.02       # Maximum stake (2% of bankroll)
)
```

---

### `apply_bet_governance()`

**File**: [R/utils.R](../R/utils.R#L248)

Apply ordered betting governance rules to EV, probability, and odds.

```r
apply_bet_governance(
  ev,
  prob,
  odds,
  min_stake = 0.01,
  kelly_fraction = 0.125,
  max_stake = 0.02,
  max_edge = 0.10,
  is_placeholder_odds = FALSE
)
```

**Returns**: Tibble with `recommendation`, `raw_kelly_pct`, `capped_stake_pct`, `final_stake_pct`, and `pass_reason`.

---

### `render_moneyline_comparison_html()`

**File**: [NFLmarket.R](../NFLmarket.R#L2800)

Generate HTML report from comparison table.

```r
render_moneyline_comparison_html(
  moneyline_tbl,    # Output from build_moneyline_comparison_table()
  output_file = "NFLvsmarket_report.html"
)
```

---

## Data Validation

### `update_injury_quality()`

**File**: [R/data_validation.R](../R/data_validation.R)

Track injury data quality for the current run.

```r
update_injury_quality(
  status,              # "full", "partial", or "unavailable"
  missing_seasons = c() # Which seasons had issues
)
```

---

### `update_weather_quality()`

**File**: [R/data_validation.R](../R/data_validation.R)

Track weather data quality.

```r
update_weather_quality(
  status,             # "full", "partial_fallback", or "all_fallback"
  fallback_games = c() # Games using fallback values
)
```

---

### `get_data_quality()`

**File**: [R/data_validation.R](../R/data_validation.R)

Retrieve current data quality status.

```r
quality <- get_data_quality()
quality$injury$status      # "full", "partial", "unavailable"
quality$weather$status     # "full", "partial_fallback", "all_fallback"
quality$market$status      # "full", "partial", "unavailable"
quality$calibration$method # Calibration method used
```

---

### `compute_overall_quality()`

**File**: [R/data_validation.R](../R/data_validation.R)

Compute overall quality badge.

```r
badge <- compute_overall_quality()
# Returns: "HIGH", "MEDIUM", "LOW", or "CRITICAL"
```

---

## Utility Functions

### `standardize_join_keys()`

**File**: [R/utils.R](../R/utils.R)

Standardize column names and types for reliable joins.

```r
standardize_join_keys(
  df,                  # Data frame to standardize
  aliases = JOIN_KEY_ALIASES
)
```

Coerces:
- `game_id` to character
- `season` to integer
- `week` to integer

---

### `brier_score()`

**File**: [R/utils.R](../R/utils.R)

Calculate Brier score for probability predictions.

```r
brier_score(prob, outcome)
# prob: predicted probabilities
# outcome: binary outcomes (0/1)
```

**Formula**: `mean((prob - outcome)^2)`

---

### `log_loss()`

**File**: [R/utils.R](../R/utils.R)

Calculate log loss (cross-entropy) for predictions.

```r
log_loss(prob, outcome, eps = 1e-15)
```

**Formula**: `-mean(outcome * log(prob) + (1 - outcome) * log(1 - prob))`

---

## Configuration Parameters

### Core Parameters (config.R)

| Parameter | Default | Description |
|-----------|---------|-------------|
| `SEASON` | 2024 | Current NFL season |
| `WEEK_TO_SIM` | 1 | Week to simulate |
| `N_TRIALS` | 100000 | Monte Carlo iterations |
| `RHO_SCORE` | -0.15 | Score correlation |
| `SHRINKAGE` | 0.60 | Market shrinkage weight |
| `KELLY_FRACTION` | 0.125 | Kelly criterion fraction |
| `MAX_EDGE` | 0.10 | Maximum believable edge |
| `USE_SNAP_WEIGHTED_INJURIES` | FALSE | Snap weighting (disabled) |

### Validated Ranges

| Parameter | Valid Range | Validation |
|-----------|-------------|------------|
| `SHRINKAGE` | [0.4, 0.8] | K-fold CV optimization |
| `KELLY_FRACTION` | [0.05, 0.25] | Risk-adjusted returns |
| `RHO_SCORE` | [-0.25, 0] | Historical correlation |

---

## Edge Quality Classification

The model flags edges by plausibility:

| Edge Range | Classification | Display |
|------------|---------------|---------|
| ≤ 0% | Negative | Pass |
| ≤ 5% | Realistic | ✓ OK |
| ≤ 10% | Optimistic | ⚠ High |
| ≤ 15% | Suspicious | ⚠⚠ Suspicious |
| > 15% | Implausible | 🚫 Implausible |

**Note**: 15%+ edges are mathematically valid for underdogs due to EV amplification, but are flagged for user awareness.

---

*Generated: 2026-02-02*
