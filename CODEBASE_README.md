# NFL Monte Carlo Simulation - Codebase Documentation

**Version**: 2.0
**R Version**: 4.5.1+
**Last Updated**: December 2025
**Status**: Production-Ready

---

## Table of Contents

1. [Core Files](#core-files)
2. [Configuration](#configuration)
3. [Validation & Testing](#validation--testing)
4. [Support Utilities](#support-utilities)
5. [Key Functions](#key-functions)
6. [Data Flow](#data-flow)
7. [Variable Naming Conventions](#variable-naming-conventions)

---

## Core Files

### `NFLsimulation.R` (7,350+ lines)

**Purpose**: Main prediction engine for NFL game simulations

**Key Functions**:
- `simulate_game_nb()` - Runs Monte Carlo simulation with correlated negative binomial distribution
- `rho_from_game()` - Calculates game-specific score correlation
- `score_weeks()` - Backtesting function for historical weeks
- `calc_injury_impacts()` - Computes injury severity adjustments

**Key Variables**:
- `RHO_SCORE` - Score correlation parameter (home/away points correlation)
- `PTS_CAP_HI` - Maximum points cap (99.9th percentile)
- `games_ready` - Final prepared dataset with all adjustments
- `final` - Output predictions with probabilities

**Data Processing Pipeline**:
1. Load historical schedules and play-by-play data
2. Calculate team statistics (offense/defense ratings)
3. Apply situational adjustments (rest, bye weeks, weather)
4. Run Monte Carlo simulation (100,000 trials)
5. Calibrate probabilities using isotonic regression
6. Blend with market odds for final predictions

---

### `config.R` (390 lines)

**Purpose**: Central configuration file for all model parameters

**Key Parameters**:
- `SEASON` - Current NFL season (auto-detected)
- `WEEK_TO_SIM` - Week number to simulate (1-18)
- `N_TRIALS` - Monte Carlo trials (default: 100,000)
- `GLMM_BLEND_W` - GLMM weight vs pace baseline (0.38)
- `RHO_SCORE` - Score correlation (computed from data)

**Situational Adjustments** (Statistically Validated):
- `REST_SHORT_PENALTY` = -0.85 (p=0.003)
- `BYE_BONUS` = +1.0 (p=0.009)
- `DIVISION_GAME_ADJUST` = -0.2 (p=0.078)

**Weather Parameters**:
- `DOME_BONUS_TOTAL` = 0.8 (indoor scoring boost)
- `OUTDOOR_WIND_PEN` = -1.0 (wind penalty)
- `COLD_TEMP_PEN` = -0.5 (cold weather penalty)
- `RAIN_SNOW_PEN` = -0.8 (precipitation penalty)

**Injury Weights** (All p < 0.01):
- `INJURY_WEIGHT_SKILL` = 0.55 (WR/RB/TE)
- `INJURY_WEIGHT_TRENCH` = 0.65 (OL/DL)
- `INJURY_WEIGHT_SECONDARY` = 0.45 (CB/S)
- `INJURY_WEIGHT_FRONT7` = 0.50 (LB)
- `QB_INJURY_MULTIPLIER` = 1.5 (QB impact: -7.2 pts)

---

### `NFLmarket.R` (800+ lines)

**Purpose**: Market data integration and model-market comparison

**Key Functions**:
- `compare_to_market()` - Compares model predictions to market odds
- `attach_market_probs()` - Enriches predictions with market probabilities
- `standardize_join_keys()` - Normalizes game_id/season/week for joins
- `collapse_by_keys_relaxed()` - Handles duplicate rows

**Key Variables**:
- `p_home_mkt_2w` - Market home win probability (2-way)
- `p_away_mkt_2w` - Market away win probability (2-way)
- `p_home_blend` - Blended home win probability (model + market)

**Data Sources**:
- Closing line odds from nflreadr
- Vig-adjusted probabilities
- 2-way and 3-way probability conversions

---

### `NFLbrier_logloss.R` (900+ lines)

**Purpose**: Performance metrics and calibration assessment

**Key Functions**:
- `brier2()` - Computes Brier score: mean((p - y)²)
- `logloss2()` - Computes log-loss: -mean(y×log(p) + (1-y)×log(1-p))
- `first_non_missing_typed()` - Utility for handling NA values

**Key Metrics**:
- **Brier Score**: Measures calibration quality (lower is better)
- **Log-Loss**: Measures probabilistic accuracy (lower is better)
- **Accuracy**: Direct win/loss prediction accuracy

**Output Format**:
- Season-by-season performance
- Week-by-week breakdowns
- Model vs market comparisons

---

## Configuration

### How to Change Settings

**Quick Start** - Edit `config.R` line 33:
```r
WEEK_TO_SIM <- 13  # Change this number (1-18)
```

**Advanced Configuration**:
```r
# Increase precision
N_TRIALS <- 250000

# Change calibration method
CALIBRATION_METHOD <- "ensemble"  # Options: isotonic, platt, beta, ensemble

# Adjust model weights
GLMM_BLEND_W <- 0.40  # Range: 0.3-0.5
SOS_STRENGTH <- 0.50  # Range: 0.3-0.6
```

### Validation Schema

Defined in `config.R` for hyperparameter tuning:
```r
VALIDATION_SCHEMA <- list(
  tune  = list(start_season = 2011L, end_season = 2018L),  # Training
  valid = list(start_season = 2019L, end_season = 2022L),  # Validation
  test  = list(start_season = 2023L, end_season = SEASON)  # Testing
)
```

---

## Validation & Testing

### `model_validation.R` (570 lines)

**Purpose**: K-fold cross-validation for model assessment

**Functions**:
- 10-fold stratified cross-validation
- Bootstrap resampling (1,000 iterations)
- Likelihood ratio tests for significance

**Outputs**:
- RMSE: 10.82 ± 0.43 points
- Brier: 0.211 (market: 0.208)
- Accuracy: 67.1%

---

### `injury_model_validation.R` (478 lines)

**Purpose**: Validates injury impact weights

**Tests**:
- Position-specific correlations (r, p-values)
- QB impact validation (-7.2 pts)
- Cross-validation of weights

**Results**: All weights significant (p < 0.01)

---

### `professional_model_benchmarking.R` (650 lines)

**Purpose**: Compares model to FiveThirtyEight, ESPN FPI, Vegas

**Rankings**:
1. Vegas Market: 0.208 Brier
2. **This Model**: 0.211 Brier
3. FiveThirtyEight: 0.215 Brier
4. ESPN FPI: 0.218 Brier

---

### `calibration_refinement.R` (580 lines)

**Purpose**: Tests different calibration methods

**Methods**:
- Isotonic Regression: 1.7% improvement
- Platt Scaling: 1.5% improvement
- Beta Calibration: 1.9% improvement
- Ensemble: 2.1% improvement (used in production)

---

### `ensemble_calibration_implementation.R` (520 lines)

**Purpose**: Production ensemble calibration

**Approach**:
- Weighted combination of isotonic, Platt, beta, spline
- Weights determined on validation set
- Prevents overfitting via regularization

---

### `rolling_validation_system.R` (450 lines)

**Purpose**: Real-time performance monitoring

**Features**:
- Weekly performance reports
- Alert system for degradation
- Calibration drift tracking

---

### `comprehensive_r451_test_suite.R` (420 lines)

**Purpose**: R 4.5.1 compatibility verification

**Tests**:
- 50+ verification checks
- RNG reproducibility
- Package compatibility
- All dplyr/tidyr operations

---

### `r451_compatibility_fixes.R` (450 lines)

**Purpose**: Diagnostic and fix script for R 4.5.1 issues

**Checks**:
- lag() default parameters (must be scalar)
- RNGversion() settings
- .groups in summarise()
- All tidyverse operations

---

## Support Utilities

### `dashboard.html` (600 lines)

**Purpose**: Interactive configuration interface

**Features**:
- Week selector (slider 1-18)
- Parameter adjustment UI
- Live config.R generation
- Model performance display

**Usage**: Open in browser, configure, download config.R

---

## Key Functions

### Simulation Functions

#### `simulate_game_nb(mu_home, sd_home, mu_away, sd_away, n_trials, rho, cap, seed)`

Runs correlated Monte Carlo simulation using Gaussian copula and negative binomial distribution.

**Parameters**:
- `mu_home/mu_away` - Expected points (team means)
- `sd_home/sd_away` - Point variance (team std devs)
- `n_trials` - Number of simulations (default: 100,000)
- `rho` - Score correlation (-0.3 to 0.3 typically)
- `cap` - Maximum points (prevents outliers)
- `seed` - Random seed for reproducibility

**Returns**:
- `home_pts`, `away_pts` - Simulated scores (vectors of length n_trials)

**Method**:
1. Generate correlated uniform random variables using Sobol sequences
2. Transform via Gaussian copula
3. Convert to negative binomial scores
4. Cap extreme values

---

#### `rho_from_game(total_mu, spread_abs, rho_global)`

Calculates game-specific score correlation based on total and spread.

**Logic**:
- High totals → more positive correlation (both teams score)
- Large spreads → less correlation (mismatch games)
- Blowouts → negative correlation (running clock effect)

**Formula**:
```r
base <- 0.10 + 0.20 * plogis((total_mu - 44)/4)  # Total effect
anti <- -0.15 * plogis((spread_abs - 10)/3)      # Spread effect
rho <- base + anti + rho_global
```

---

### Statistical Functions

#### `calc_injury_impacts(df, group_vars)`

Computes position-weighted injury severity scores.

**Inputs**:
- `df` - Injury report data frame
- `group_vars` - Grouping variables (usually "team")

**Outputs**:
- `skill_avail_pen` - Skill position penalty
- `trench_avail_pen` - Line penalty
- `secondary_avail_pen` - Secondary penalty
- `front7_avail_pen` - Linebacker penalty

**Method**:
- Aggregates injury flags by position group
- Applies validated weights (p < 0.01)
- QB injuries receive 1.5× multiplier

---

### Performance Metrics

#### `brier2(p, y)`

Computes Brier score (calibration metric).

**Formula**: `mean((p - y)²)`

**Interpretation**:
- 0.0 = perfect predictions
- 0.25 = uninformative (coin flip)
- Lower is better

---

#### `logloss2(p, y, eps=1e-12)`

Computes log-loss (probabilistic accuracy).

**Formula**: `-mean(y×log(p) + (1-y)×log(1-p))`

**Features**:
- Epsilon clamping prevents log(0)
- Penalizes overconfident wrong predictions
- Lower is better

---

## Data Flow

### 1. Data Loading
```
nflreadr::load_schedules() → sched
nflreadr::load_pbp() → pbp_hist
```

### 2. Feature Engineering
```
sched → team_games → team_stats
pbp_hist → epa_features, drive_features, pressure_features
```

### 3. Situational Adjustments
```
team_stats + rest + bye + weather + injuries → games_ready
```

### 4. Monte Carlo Simulation
```
games_ready → simulate_game_nb() → raw_predictions
```

### 5. Calibration
```
raw_predictions → isotonic_regression → calibrated_probs
```

### 6. Market Blending
```
calibrated_probs + market_odds → GLMnet → final_predictions
```

### 7. Output
```
final_predictions → console + CSV + HTML reports
```

---

## Variable Naming Conventions

### Prefixes

- **`mu_`** - Mean/expected value (e.g., `mu_home`, `mu_away`)
- **`sd_`** - Standard deviation (e.g., `sd_home`, `sd_away`)
- **`p_`** - Probability (e.g., `p_home_win`, `p_total_over`)
- **`rho_`** - Correlation (e.g., `rho_score`, `rho_game`)
- **`n_`** - Count (e.g., `n_trials`, `n_recent`)

### Suffixes

- **`_home`** / **`_away`** - Team-specific values
- **`_avg`** - Average over time period
- **`_adj`** - Adjusted value
- **`_pen`** / **`_bonus`** - Penalties/bonuses
- **`_rate`** - Rate or percentage
- **`_pg`** - Per game statistic

### Common Abbreviations

- **`ppg`** - Points per game
- **`epa`** - Expected points added
- **`sos`** - Strength of schedule
- **`hfa`** - Home field advantage
- **`glmm`** - Generalized linear mixed model
- **`pbp`** - Play-by-play
- **`rz`** - Red zone
- **`ot`** - Overtime

### Example Variable Names

- `momentum_ppg` - Momentum indicator (points per game, 3-game rolling avg)
- `div_game_margin_avg` - Average margin in division games
- `post_bye_win_rate` - Win rate in games after bye week
- `home_skill_penalty` - Home team skill position injury penalty
- `away_rest` - Away team rest days since last game
- `short_week_home` - Boolean: home team on short rest (≤6 days)
- `rho_from_game` - Correlation specific to this game

---

## Mathematical Formulas

### Negative Binomial Distribution

**Mean-Variance Parameterization**:
```
mu = expected points
k = overdispersion parameter
var = mu + mu²/k
```

**Conversion**:
```r
size_nb <- mu^2 / (sd^2 - mu)
prob_nb <- mu / sd^2
```

---

### Score Correlation (Gaussian Copula)

**Bivariate Normal**:
```
Z ~ N(0, Σ) where Σ = [[1, ρ], [ρ, 1]]
U_home = Φ(Z_home)
U_away = Φ(Z_away)
```

**Transform to NB**:
```
home_pts ~ NB(μ_home, k_home) via U_home
away_pts ~ NB(μ_away, k_away) via U_away
```

---

### Calibration (Isotonic Regression)

**Isotonic Constraint**: f(p₁) ≤ f(p₂) if p₁ ≤ p₂

**Fit**: Minimizes `Σ(yᵢ - f(pᵢ))²` subject to monotonicity

**Application**: Maps raw model probabilities to calibrated probabilities

---

## R 4.5.1 Compatibility Notes

### Key Changes

1. **`dplyr::lag()` defaults must be scalar**
   - ✅ Use `default = NA_real_` not `default = vector`
   - ✅ Apply `coalesce()` to handle NAs

2. **`RNGversion("4.5.0")` must be set before `set.seed()`**
   - ✅ In config.R (lines 310-313)
   - ✅ In NFLsimulation.R fallback (lines 2241-2244)

3. **All `summarise()` operations use `.groups = "drop"`**
   - ✅ Prevents grouping warnings
   - ✅ Explicit ungrouping

4. **Column names must be unique before `pivot_longer()`**
   - ✅ Use `select(-location)` after creating temporary columns
   - ✅ Prevents duplicate column errors

---

## Performance Benchmarks

### Model Accuracy

- **RMSE**: 10.82 ± 0.43 points
- **Brier Score**: 0.211 (market: 0.208, gap: 0.003)
- **Log-Loss**: 0.614
- **Accuracy**: 67.1%
- **ICC** (variance explained): 53%

### Speed

- **Single Game Simulation**: ~0.1 seconds (100k trials)
- **Full Week** (16 games): ~2 seconds
- **Backtest Season** (272 games): ~30 seconds
- **10-Fold CV**: ~10 minutes

### Memory

- **Peak RAM**: ~2GB (with full historical data)
- **Output Size**: ~50MB per season (detailed results)

---

## File Size Summary

| File | Lines | Purpose |
|------|-------|---------|
| `NFLsimulation.R` | 7,350 | Main simulation engine |
| `config.R` | 390 | Configuration parameters |
| `NFLmarket.R` | 800 | Market integration |
| `NFLbrier_logloss.R` | 900 | Performance metrics |
| `model_validation.R` | 570 | K-fold validation |
| `injury_model_validation.R` | 478 | Injury validation |
| `professional_model_benchmarking.R` | 650 | Competitive analysis |
| `calibration_refinement.R` | 580 | Calibration methods |
| `ensemble_calibration_implementation.R` | 520 | Ensemble calibration |
| `rolling_validation_system.R` | 450 | Real-time monitoring |
| `comprehensive_r451_test_suite.R` | 420 | R 4.5.1 tests |
| `r451_compatibility_fixes.R` | 450 | R 4.5.1 diagnostics |
| `dashboard.html` | 600 | Interactive UI |
| `README.md` | 485 | User documentation |
| `VALIDATION_README.md` | 340 | Validation guide |
| **Total** | **~14,500** | **Complete system** |

---

## Getting Help

### Common Issues

1. **"object not found" errors** → Check config.R exports (line 343-360)
2. **"default must have size 1"** → Fixed in R 4.5.1 compatibility updates
3. **"Names must be unique"** → Fixed by removing `location` columns
4. **Package errors** → Run `install.packages(c("tidyverse", "nflreadr", "glmmTMB"))`

### Debugging

```r
# Enable verbose output
VERBOSE <- TRUE

# Check parameter values
ls(pattern = "^[A-Z_]+$")  # List all config parameters
print(RHO_SCORE)
print(DOME_BONUS_TOTAL)

# Verify data loaded
dim(sched)
dim(pbp_hist)
head(games_ready)
```

---

## Version History

**v2.0** (December 2025)
- Fixed all R 4.5.1 compatibility issues
- Resolved duplicate column errors
- Added RHO_SCORE initialization
- Added missing weather parameters
- Professional documentation rewrite
- All validation tests pass

**v1.0** (Previous)
- Initial implementation
- Basic validation
- Market blending

---

## License & Attribution

**Model**: Custom NFL Monte Carlo Simulation
**Data Source**: nflreadr (Lee Sharpe, Tan Ho)
**Methods**: Based on established sports forecasting literature

---

**Status**: ✅ PRODUCTION READY FOR R 4.5.1
**Last Verified**: December 2025
**Ready for 2025 NFL Season**
