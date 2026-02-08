# NFL Prediction Model - Complete Documentation

Comprehensive technical reference for the NFL game prediction system.

**Version**: 2.6.4
**R Version**: 4.3.0+ (tested on 4.5.1)
**Last Updated**: January 2026

---

## Table of Contents

1. [Architecture Overview](#architecture-overview)
2. [Core Files](#core-files)
3. [Validation Files](#validation-files)
4. [Statistical Methodology](#statistical-methodology)
5. [Key Functions](#key-functions)
6. [Data Pipeline](#data-pipeline)
7. [Variable Naming Conventions](#variable-naming-conventions)
8. [Mathematical Formulas](#mathematical-formulas)
9. [R 4.5.1 Compatibility](#r-451-compatibility)
10. [Performance Benchmarks](#performance-benchmarks)

---

## Architecture Overview

### System Design

```
config.R → NFLsimulation.R → Output Files
              ↓
         NFLmarket.R (market comparison)
              ↓
         NFLbrier_logloss.R (evaluation)
```

**Data Flow**:
1. Load configuration parameters (`config.R`)
2. Fetch NFL data from nflverse (`nflreadr` package)
3. Calculate team statistics and strength metrics
4. Load injury reports and weather data
5. Run Monte Carlo simulations (100,000 trials per game)
6. Apply probability calibration (isotonic regression)
7. Generate predictions and performance metrics

**Model Type**: Hierarchical Bayesian with Monte Carlo simulation
- Base model: Negative binomial GLMM
- Team effects: Random intercepts for offense/defense
- Adjustments: Rest, injuries, weather, schedule strength
- Correlation: Gaussian copula for score dependence

---

## Core Files

### config.R (986 lines)

**Purpose**: Central configuration for all model parameters

**Key Sections**:
1. **Package Management** (lines 1-30)
   - Auto-installs missing packages
   - R version compatibility check
   - Loads tidyverse, nflreadr, lubridate, etc.

2. **Basic Settings** (lines 31-60)
   - `SEASON`: Current NFL season (default: current year)
   - `WEEK_TO_SIM`: Week to predict (1-18 for regular season)
   - `N_TRIALS`: Monte Carlo trial count (default: 100,000)
   - `SEED`: Random seed for reproducibility (471)

3. **Model Hyperparameters** (lines 61-130)
   - `GLMM_BLEND_W = 0.38`: GLMM vs baseline weight (p < 0.001)
   - `SOS_STRENGTH = 0.45`: Strength of schedule factor (p < 0.001)
   - `RECENCY_HALFLIFE = 3.0`: Recent form decay rate (p < 0.001)
   - All values determined by cross-validation

4. **Situational Adjustments** (lines 131-250)
   - Rest: `REST_SHORT_PENALTY = -0.85` (p = 0.003)
   - Bye: `BYE_BONUS = +1.0` (p = 0.009)
   - Division: `DIVISION_GAME_ADJUST = -0.2` (p = 0.078)
   - Injuries: Position-specific weights (all p < 0.01)
   - Weather: Dome, wind, cold, precipitation (all p < 0.05)

5. **Validation Schema** (lines 251-290)
   - Train set: 2011-2018
   - Validation set: 2019-2022
   - Test set: 2023-current

6. **Output Settings** (lines 291-320)
   - Output directory structure
   - Report generation flags
   - Monitoring thresholds

**Exports**: All parameters to global environment via `list2env()`

### NFLsimulation.R (8,226 lines)

**Purpose**: Main prediction engine with Monte Carlo simulation

**Architecture**:
```
Data Loading → Team Stats → Adjustments → Simulation → Calibration → Output
```

**Major Sections**:

1. **Data Loading** (lines 1-600)
   - `load_market_helpers()`: Sources NFLmarket.R utilities
   - `load_schedules()`: Gets schedule from nflreadr
   - `load_injuries()`: Injury reports with fallbacks
   - Date/time parsing and normalization

2. **Team Statistics** (lines 601-2000)
   - Play-by-play EPA calculations
   - Offensive/defensive efficiency metrics
   - Red zone performance
   - Third down conversion rates
   - Turnover metrics
   - Special teams (field position, punting)

3. **Strength of Schedule** (lines 2001-2500)
   - Opponent-adjusted EPA
   - Iterative SOS calculation
   - Home field advantage by team
   - Recent form with exponential decay

4. **Hierarchical Model** (lines 2501-3000)
   - GLMM fitting: `points ~ is_home + (1|team) + (1|opp)`
   - Negative binomial family (overdispersion)
   - Team random effects (offense/defense strength)
   - Model validation and diagnostics

5. **Situational Adjustments** (lines 3001-4500)
   - **QB Status**: Starter/backup identification
   - **Injuries**: `calc_injury_impacts()` with position weighting
   - **Rest/Travel**: Days since last game, time zone changes
   - **Weather**: Temperature, wind, precipitation, roof type
   - **Matchup Factors**: Division games, pass rush vs protection

6. **Monte Carlo Simulation** (lines 4501-5500)
   - `simulate_game_nb()`: Negative binomial simulation
   - Score correlation via Gaussian copula
   - Parameter: `RHO_SCORE` (typically -0.3 to 0.3)
   - 100,000 trials per game for stable estimates

7. **Probability Calibration** (lines 5501-6500)
   - **Spline calibration** (default): GAM with smoothing penalty
   - Corrects for model overconfidence/underconfidence
   - Applied to win probabilities
   - **6.9% Brier score improvement** (spline, best performer)
   - Isotonic regression available as fallback (1.7% improvement)

8. **Output Generation** (lines 6501-7400)
   - Prediction summaries (win prob, spread, total)
   - Confidence intervals (90%)
   - Market comparisons (if available)
   - Performance metrics (Brier, log-loss)

**Key Functions**:
- `simulate_game_nb()`: Monte Carlo game simulation
- `calc_injury_impacts()`: Position-weighted injury effects
- `make_calibrator()`: Isotonic regression calibration
- `rho_from_game()`: Score correlation estimation

### NFLmarket.R (3,942 lines)

**Purpose**: Market data integration and model-market comparison

**Features**:
1. **Data Standardization**
   - Joins schedule, market odds, model predictions
   - Handles missing data gracefully
   - Column name normalization

2. **Probability Conversion**
   - `american_to_probability()`: Moneyline → probability
   - `american_to_decimal()`: Moneyline → decimal odds
   - `spread_to_win_probability()`: Spread → win %
   - `devig_two_way_probabilities()`: Remove vig from market odds

3. **Model-Market Blending**
   - `build_res_blend()`: Combines model + market probabilities
   - Convex combination with configurable weights
   - Alignment with spread/total markets

4. **Expected Value**
   - `expected_value_units()`: EV calculation for bets
   - Kelly criterion position sizing
   - ROI analysis

5. **Reporting**
   - `build_moneyline_comparison_table()`: Model vs market table
   - `export_moneyline_comparison_html()`: HTML report generation
   - Interactive visualizations

### NFLbrier_logloss.R (1,200+ lines)

**Purpose**: Model evaluation metrics and performance tracking

**Metrics Implemented**:

1. **Brier Score**
   ```r
   brier = mean((p - y)^2)
   # p = predicted probability, y = actual outcome (0 or 1)
   # Range: 0 (perfect) to 1 (worst)
   # Model: 0.211, Vegas: 0.208
   ```

2. **Log-Loss** (cross-entropy)
   ```r
   logloss = -mean(y * log(p) + (1-y) * log(1-p))
   # Penalizes confident wrong predictions more heavily
   # Model: 0.614
   ```

3. **Calibration Plots**
   - Predicted probability vs observed frequency
   - Ideal: points on diagonal line
   - Shows over/underconfidence regions

4. **ROC Curves**
   - True positive rate vs false positive rate
   - AUC (Area Under Curve) metric
   - Model AUC: ~0.71

**Functions**:
- `compare_to_market()`: Model vs market performance
- `dedupe_by_keys()`: Ensures unique game records
- `first_non_missing_typed()`: Safe column selection

---

## Validation Files

### validation_pipeline.R (740 lines)

**Purpose**: Hyperparameter tuning with train/validation/test splits

**Methodology**:
- **Training Set**: 2011-2018 (tune hyperparameters)
- **Validation Set**: 2019-2022 (evaluate performance)
- **Test Set**: 2023-current (final assessment)

**Grid Search**:
```r
Parameters tuned:
- GLMM_BLEND_W: 0.3, 0.38, 0.5
- RECENCY_HALFLIFE: 2.0, 3.0, 4.0
- N_RECENT: 4, 6, 8
- SOS_STRENGTH: 0.3, 0.45, 0.6

Grid size: 3 × 3 × 3 × 3 = 81 combinations
Selection metric: Minimize 2-way Brier score
Tiebreaker: Minimize log-loss
```

**Functions**:
- `tune_hyperparams()`: Grid search with cross-validation
- `apply_best_hyperparams()`: Updates config.R with optimal values
- `build_calibration()`: Trains isotonic regression
- `evaluate_phase()`: Computes metrics on held-out data

### model_validation.R (935 lines)

**Purpose**: K-fold cross-validation and statistical significance testing

**K-Fold Cross-Validation** (k=10):
```r
Method: Stratified by season
Folds: 10 folds × ~230 games each
Metrics per fold:
- RMSE (point margin error)
- Brier score (probability accuracy)
- Log-loss (probability calibration)
- Coverage (confidence interval validity)
```

**Statistical Tests**:

1. **Permutation Tests** (n=1000 iterations)
   - Null hypothesis: Adjustment has no effect
   - Permute adjustment variable randomly
   - Compare observed effect to null distribution
   - p-value: proportion of permutations ≥ observed effect

2. **Bootstrap Confidence Intervals** (n=1000 samples)
   - Resample games with replacement
   - Calculate metric on each bootstrap sample
   - 95% CI: 2.5th and 97.5th percentiles

3. **ANOVA for Model Components**
   - F-tests for variance explained
   - Effect sizes (eta-squared)
   - Contribution of each model term

**Functions**:
- `source_data()`: Loads NFL data for validation
- `perform_kfold_cv()`: K-fold cross-validation
- `test_model_significance()`: Permutation tests
- `calculate_effect_sizes()`: Effect size estimates
- `generate_final_report()`: Validation summary

### injury_model_validation.R (668 lines)

**Purpose**: Validates injury impact weights against actual outcomes

**Methodology**:

1. **Data Collection**:
   - Injury reports from nflreadr::load_injuries()
   - Matched to game outcomes (2022-2024)
   - Position groupings: QB, Skill, Trench, Secondary, Front7

2. **Severity Scoring**:
   ```r
   Status severity weights:
   - OUT/IR: 1.0
   - Doubtful: 0.7
   - Questionable: 0.4
   - Limited practice: 0.2
   ```

3. **Statistical Tests**:
   - Linear regression: `points ~ injury_severity + team + opp`
   - Permutation test: shuffle injury assignments
   - p-values: All position groups p < 0.01

4. **Position-Specific Weights** (validated):
   - QB: 1.5× multiplier (average -7.2 pts when out)
   - Skill (WR/RB/TE): 0.55 pts per injury flag
   - Trench (OL/DL): 0.65 pts per injury flag
   - Secondary (CB/S): 0.45 pts per injury flag
   - Front7 (LB/EDGE): 0.50 pts per injury flag

**Functions**:
- `load_injury_data()`: Fetches injury reports
- `calculate_injury_severity()`: Scores injury impact
- `test_injury_impact()`: Correlates with game outcomes

**A/B Comparison** (validation/injury_ab_comparison.R):
```r
# Run A/B test comparing WITH vs WITHOUT injuries
results <- run_injury_ab_test(seasons = 2022:2024, n_bootstrap = 1000)
# Compares Brier score, log-loss, accuracy
# Reports statistical significance (p-values)
```

### professional_model_benchmarking.R (750 lines)

**Purpose**: Compares model to FiveThirtyEight ELO and ESPN FPI

**Data Sources**:
- FiveThirtyEight: Historical ELO ratings
- ESPN FPI: Football Power Index ratings
- Vegas: Closing lines (baseline)

**Comparison Metrics**:
```
Model vs Benchmarks (2022-2024):

Brier Score:
- Vegas Market: 0.208 (best)
- This Model: 0.211
- FiveThirtyEight: 0.215
- ESPN FPI: 0.218

Accuracy:
- Vegas Market: 68.2%
- This Model: 67.1%
- FiveThirtyEight: 65.8%
- ESPN FPI: 64.5%

Log-Loss:
- This Model: 0.614
- FiveThirtyEight: 0.649
- ESPN FPI: 0.672
```

**Statistical Tests**:
- McNemar's test: Pairwise accuracy comparison
- DeLong test: ROC curve comparison
- Bootstrap: Metric confidence intervals

### calibration_refinement.R (680 lines)

**Purpose**: Probability calibration analysis and refinement

**Calibration Methods Tested**:

1. **Spline Calibration** (selected, default)
   - GAM with smoothing penalty
   - Flexible, handles non-linear patterns
   - **Brier: 0.215 → 0.200 (-6.9%)** (best performer)

2. **Isotonic Regression** (fallback)
   - Monotonic transformation
   - Fits observed frequencies
   - Brier: 0.215 → 0.211 (-1.7%)
   - Note: Found to have bugs in v2.6.3; spline is preferred

3. **Platt Scaling**
   - Logistic regression calibration
   - Less flexible than isotonic
   - Brier: 0.215 → 0.213 (-0.9%)

4. **Beta Calibration**
   - Beta distribution fitting
   - Good for extreme probabilities
   - Brier: 0.215 → 0.212 (-1.4%)

**Selection**: Spline calibration (best Brier improvement, robust)

### ensemble_calibration_implementation.R (680 lines)

**Purpose**: Ensemble calibration combining multiple methods

**Ensemble Strategy**:
```r
Final probability = weighted average of calibration methods
Weights determined by validation set performance:
- Isotonic: 50%
- Beta: 25%
- Spline: 15%
- Platt: 10%
```

**Cross-Validation**:
- Temporal splits (train on older seasons)
- Prevents data leakage
- Out-of-sample performance: Brier 0.209

### rolling_validation_system.R (599 lines)

**Purpose**: Real-time monitoring during NFL season

**Monitoring Windows**:
- 4-week rolling: Recent performance
- 8-week rolling: Medium-term trends
- Full season: Overall assessment

**Alert Thresholds**:
```r
Alerts trigger when:
- Brier > 0.23 (model deteriorating)
- Accuracy < 48% (worse than random)
- Trend > 0.03 (rapid decline)
```

**Tracked Metrics**:
- Weekly Brier score and log-loss
- Calibration drift (predicted vs observed)
- Accuracy by game type (division, primetime, etc.)
- Performance vs market (if available)

### run_validation_example.R (259 lines)

**Purpose**: Example script showing how to run validation

**Quick Validation** (10 minutes):
```r
Small grid: 4 combinations
Trials: 10,000 per configuration
Use for testing changes
```

**Full Validation** (2-3 hours):
```r
Full grid: 81 combinations
Trials: 40,000 per configuration
Use for final hyperparameter selection
```

### final_verification_checklist.R (670 lines)

**Purpose**: Pre-deployment validation checklist

**Checks Performed**:
1. **File Integrity**: All required files present
2. **Package Versions**: R 4.5.1 compatibility
3. **Statistical Validation**: All p-values documented
4. **Code Quality**: No duplicates, proper formatting
5. **Output Validation**: Predictions in valid ranges
6. **Performance Benchmarks**: Meets target metrics

**Categories**:
- Core Model Validation (RMSE, Brier, log-loss)
- Injury Model Validation (position weights, p-values)
- Adjustment Validation (rest, weather, SOS)
- Code Quality (duplicates, style, documentation)
- Data Integrity (missing values, outliers)

---

## Statistical Methodology

### Hierarchical Bayesian Model

**Base Model**:
```r
points ~ NegBinom(μ, φ)
log(μ) = β₀ + β₁·is_home + u_team + v_opp

Where:
- μ: Expected points
- φ: Dispersion parameter (overdispersion)
- β₀: League average intercept
- β₁: Home field advantage
- u_team: Team random effect (offense)
- v_opp: Opponent random effect (defense)
```

**Random Effects**:
```r
u_team ~ N(0, σ²_team)    # Team offense strength
v_opp ~ N(0, σ²_opp)      # Team defense strength

Estimates:
- σ_team ≈ 0.18 (offense varies by ±1.8 pts)
- σ_opp ≈ 0.15 (defense varies by ±1.5 pts)
- β₁ ≈ 0.19 (home advantage ≈ 2 pts)
```

### Monte Carlo Simulation

**Negative Binomial Distribution**:
```r
Y ~ NegBinom(μ, φ)

Mean: E[Y] = μ
Variance: Var[Y] = μ + μ²/φ

Allows overdispersion (variance > mean)
NFL games: φ ≈ 8 (high variability)
```

**Score Correlation** (Gaussian Copula):
```r
1. Generate correlated uniforms (U₁, U₂)
2. Transform to negative binomial quantiles
3. Correlation ρ ≈ -0.1 to -0.2 (mild negative)

Interpretation: Higher home score slightly associated
with lower away score (defensive efforts, possession time)
```

**Simulation Process**:
```r
For each game:
  1. Calculate μ_home, μ_away (expected points)
  2. Calculate σ_home, σ_away (variability)
  3. Generate N=100,000 simulated scores
  4. Count outcomes: P(home win), P(total > O/U), etc.
```

### Statistical Significance Testing

**Inclusion Criteria**:
```r
Variable included if:
1. p-value < 0.05 (95% confidence effect is real)
2. Brier improvement > 0.001 (practical significance)
3. Effect sign matches theory (e.g., rest helps)
```

**Permutation Test**:
```r
1. Observe actual effect (e.g., Brier with rest variable)
2. Shuffle rest variable randomly (n=1000 times)
3. Calculate effect for each shuffle
4. p-value = (# shuffles ≥ observed) / 1000

Example - REST_SHORT_PENALTY:
- Observed Brier improvement: 0.0027
- Permutation p-value: 0.003
- Conclusion: Significant at p < 0.01
```

**Bootstrap Confidence Intervals**:
```r
1. Resample games with replacement (n=1000 times)
2. Calculate metric for each resample
3. 95% CI: (2.5th percentile, 97.5th percentile)

Example - RMSE:
- Point estimate: 10.82
- 95% CI: (10.51, 11.13)
- Interpretation: True RMSE likely in this range
```

### Cross-Validation

**K-Fold** (k=10):
```r
1. Split data into 10 folds (~230 games each)
2. For each fold:
   - Train on other 9 folds
   - Predict on held-out fold
   - Calculate metrics (RMSE, Brier, log-loss)
3. Average metrics across folds
4. Report mean ± standard deviation

Prevents overfitting: Each game predicted using
model trained on data that doesn't include that game
```

**Temporal Validation**:
```r
Train: 2011-2018 (8 seasons)
Validate: 2019-2022 (4 seasons)
Test: 2023-current (2+ seasons)

Realistic: Simulates using past to predict future
No data leakage: Future never used to train past
```

---

## Key Functions

### simulate_game_nb(mu_home, mu_away, sd_home, sd_away, rho, n_trials)

**Purpose**: Monte Carlo simulation of single game

**Parameters**:
- `mu_home`: Expected home team points
- `mu_away`: Expected away team points
- `sd_home`: Home team score variability
- `sd_away`: Away team score variability
- `rho`: Score correlation (-1 to 1)
- `n_trials`: Number of simulations (default: 100,000)

**Returns**:
```r
list(
  home_win_prob = P(home wins),
  tie_prob = P(tie),
  away_win_prob = P(away wins),
  home_score_median = Median home score,
  away_score_median = Median away score,
  total_median = Median total points,
  home_score_mean = Mean home score,
  away_score_mean = Mean away score
)
```

**Implementation**:
```r
1. Generate correlated random uniforms via Gaussian copula
2. Transform to negative binomial quantiles
3. Simulate n_trials games
4. Calculate probabilities and summary statistics
```

### calc_injury_impacts(df, group_vars)

**Purpose**: Compute position-weighted injury severity

**Parameters**:
- `df`: Injury report data frame
- `group_vars`: Grouping variables (e.g., c("team", "week"))

**Returns**:
```r
data.frame(
  team, week,
  inj_off_pts = Offensive impact (negative),
  inj_def_pts = Defensive impact (positive = opponent scores more),
  skill_avail_pen = Skill position severity,
  trench_avail_pen = Line severity,
  secondary_avail_pen = DB severity,
  front7_avail_pen = LB/EDGE severity
)
```

**Position Weights** (from config.R):
```r
QB: 0.0 base penalty (handled separately with QB_INJURY_MULTIPLIER)
Trenches (OL/DL): INJURY_POS_MULT_TRENCH = 1.3
Skill (WR/RB/TE): INJURY_POS_MULT_SKILL = 1.05
Secondary (CB/S): INJURY_POS_MULT_SECONDARY = 0.95
Front7 (LB/EDGE/DL): INJURY_POS_MULT_FRONT7 = 0.85
Other: INJURY_POS_MULT_OTHER = 0.6

Point-per-flag scalars (validated p < 0.01):
Skill: INJURY_WEIGHT_SKILL = 0.55 (r = 0.28)
Trench: INJURY_WEIGHT_TRENCH = 0.65 (r = 0.24)
Secondary: INJURY_WEIGHT_SECONDARY = 0.45 (r = 0.19)
Front7: INJURY_WEIGHT_FRONT7 = 0.50 (r = 0.21)
```

**Severity Levels**:
```r
OUT/IR: 1.0 (full impact)
Doubtful: 0.7
Questionable: 0.4
Limited: 0.2
```

### make_calibrator(method, p, y, weights)

**Purpose**: Create probability calibration function

**Parameters**:
- `method`: "isotonic", "platt", "beta", "spline"
- `p`: Predicted probabilities
- `y`: Actual outcomes (0 or 1)
- `weights`: Optional case weights

**Returns**: Function that transforms probabilities

**Isotonic Regression** (recommended):
```r
# Spline calibration (default, recommended)
calibrator <- make_calibrator("spline", p_pred, y_actual)
p_calibrated <- calibrator(p_pred)

Properties:
- Smooth (handles non-linear patterns)
- Flexible via GAM smoothing penalty
- Reduces Brier score by 6.9% (best performer)

# Isotonic regression (fallback)
calibrator <- make_calibrator("isotonic", p_pred, y_actual)
p_calibrated <- calibrator(p_pred)

Properties:
- Monotonic (preserves ordering)
- Non-parametric (flexible)
- Reduces Brier score by 1.7%
```

### rho_from_game(home_pts, away_pts)

**Purpose**: Estimate score correlation from historical data

**Parameters**:
- `home_pts`: Vector of home scores
- `away_pts`: Vector of away scores

**Returns**: Correlation coefficient ρ

**Method**:
```r
1. Convert scores to negative binomial quantiles
2. Transform to standard normal (inverse CDF)
3. Calculate Pearson correlation
4. Typical range: -0.1 to -0.2 (mild negative correlation)
```

---

## Data Pipeline

### Complete Flow

```
1. DATA SOURCES
   ├─ nflreadr::load_schedules() → Game schedule
   ├─ nflreadr::load_pbp() → Play-by-play data
   ├─ nflreadr::load_injuries() → Injury reports
   └─ Weather API → Forecasts (optional)

2. FEATURE ENGINEERING
   ├─ Team EPA (offense/defense/special teams)
   ├─ Red zone efficiency (TD %)
   ├─ Third down conversion rates
   ├─ Turnover rates (forced and committed)
   ├─ Pass rush pressure rates
   ├─ Pass protection allowed rates
   └─ Explosive play rates (20+ yard gains)

3. STRENGTH METRICS
   ├─ Raw EPA per play
   ├─ Opponent-adjusted EPA (strength of schedule)
   ├─ Recent form (exponential decay, halflife=3 games)
   └─ Home field advantage (team-specific)

4. HIERARCHICAL MODEL
   ├─ Fit GLMM: points ~ is_home + (1|team) + (1|opp)
   ├─ Extract random effects (team strength)
   └─ Estimate dispersion (variability)

5. ADJUSTMENTS
   ├─ QB status (starter vs backup)
   ├─ Injuries (position-weighted severity)
   ├─ Rest (days since last game)
   ├─ Bye week recovery
   ├─ Weather (temperature, wind, precipitation)
   ├─ Division/conference game effects
   └─ Matchup-specific (pass rush vs OL, etc.)

6. EXPECTED POINTS
   ├─ Blend GLMM + EPA-based predictions
   ├─ Apply all adjustments
   ├─ Calculate variability (σ)
   └─ Ensure non-negativity

7. MONTE CARLO
   ├─ Simulate 100,000 games per matchup
   ├─ Use negative binomial distribution
   ├─ Apply score correlation (Gaussian copula)
   └─ Calculate outcome probabilities

8. CALIBRATION
   ├─ Apply isotonic regression
   ├─ Correct for over/underconfidence
   └─ Improve probability accuracy

9. OUTPUT
   ├─ Win probabilities (calibrated)
   ├─ Predicted scores (median, mean)
   ├─ Confidence intervals (90%)
   ├─ Spreads and totals
   └─ Performance metrics (Brier, log-loss)
```

---

## Variable Naming Conventions

### Prefixes

- `mu_`: Expected value (mean)
- `sd_`: Standard deviation (variability)
- `p_`: Probability
- `rho_`: Correlation
- `n_`: Count
- `w_`: Weight
- `adj_`: Adjusted value
- `raw_`: Unadjusted value
- `home_`: Home team metric
- `away_`: Away team metric
- `off_`: Offensive metric
- `def_`: Defensive metric

### Suffixes

- `_home`: Home team value
- `_away`: Away team value
- `_avg`: Average
- `_med`: Median
- `_sd`: Standard deviation
- `_wt`: Weight
- `_adj`: Adjusted
- `_raw`: Raw (unadjusted)
- `_pen`: Penalty
- `_bonus`: Bonus

### Common Abbreviations

- `ppg`: Points per game
- `epa`: Expected Points Added
- `sos`: Strength of Schedule
- `hfa`: Home Field Advantage
- `glmm`: Generalized Linear Mixed Model
- `pbp`: Play-by-play
- `rz`: Red zone
- `to`: Turnover
- `3rd`: Third down
- `conv`: Conversion
- `def`: Defense/defensive

### Examples

```r
mu_home_base       # Base expected home points
mu_home_qb_rest    # After QB and rest adjustments
mu_home            # Final expected home points

sd_home_raw        # Raw variability (historical)
sd_home            # Adjusted variability

p_home_win         # Probability home wins
p_home_win_cal     # Calibrated win probability

home_off_epa       # Home offensive EPA
away_def_epa       # Away defensive EPA

skill_avail_pen    # Skill position injury penalty
trench_avail_pen   # Trench position injury penalty
```

### Moneyline Naming Rules (Fair vs Vigged)

- `blend_home_prob_shrunk`: canonical shrunk home win probability used for betting math.
- `blend_home_ml`: fair model home moneyline computed only from shrunk probability.
- `blend_home_ml_vig`: vigged model home moneyline computed only from `blend_home_ml`.
- Display column `Blend Home ML (Fair, from Shrunk Prob)` maps to `blend_home_ml`.
- Display column `Blend Home ML (Vigged, +X%)` maps to `blend_home_ml_vig`.
- Display column `Blend Away Moneyline (Vigged)` maps to `blend_away_ml_vig`.

Hard rule: probability coherence checks must use fair quantities only (`blend_home_prob_shrunk` ↔ `blend_home_ml`) and must never use vigged moneylines.


### Moneyline Report Columns (HTML)

Key columns and definitions for the game table:

- `EV Edge (Raw)`: Expected return per unit for the displayed pick: `(Shrunk Prob ? Decimal Odds) - 1` (uncapped).
- `EV Edge (Displayed, Capped)`: Display-only cap of raw EV at `MAX_EDGE` (10% default); governance uses raw EV.
- `Min Stake (%)`: Minimum stake threshold required to place a bet (default 1%).
- `Final Stake (%)`: Kelly-based stake after caps; zero when `PASS`.
- `Total EV (Units)`: `Final Stake ? EV Edge (Raw)` (0 for `PASS`).
- `Market Home Win % (Fair, Devig=proportional)`: Vig-free home win probability using proportional devig on market moneylines.
- `ML Implied Home % (Raw)`: Raw implied probability from the home moneyline before devig.
- `Blend Home ML (Fair, from Shrunk Prob)`: `probability_to_american(blend_home_prob_shrunk)`.
- `Blend Home ML (Vigged, +X%)`: `apply_moneyline_vig(blend_home_ml, vig)` (display only).
- `Blend Away Moneyline (Vigged)`: `apply_moneyline_vig(blend_away_ml, vig)` (display only).

### Player Props Report Columns (HTML)

Key columns and definitions for the player props table:

- `Market DK`: DraftKings line + odds (Over/Under) when available. Anytime TD rows append implied percent to the "Yes" odds.
- `Market FD`: FanDuel line + odds (Over/Under) when available. Anytime TD rows append implied percent to the "Yes" odds.
- `Market Used`: The line/odds selected by the odds resolver (book priority + best price). Anytime TD rows append implied percent to the "Yes" odds.
- `Book Used`: Book key used for `Market Used` (e.g., `draftkings`, `fanduel`).
- `Model`: Model projection (yards/receptions) or anytime TD probability (percent).
- `P(Over/Yes)`: Model probability of the Over/Yes outcome.
- `P(Under/No)`: Model probability of the Under/No outcome.
- `EV Over`: Expected value for Over/Yes using market odds.
- `EV Under`: Expected value for Under/No using market odds (blank if no "No" odds).
- `Edge Quality`: EV tier (OK/High/Review/Pass) using best positive EV.
- `Recommendation`: `OVER`, `UNDER`, `BET`, `REVIEW`, or `PASS`.
- `Source`: `line_source/odds_source` (e.g., `market/market`, `model/model`, or `model/missing`). When `odds_source = missing`, EV and recommendations are suppressed.

**Anytime TD props:** Only the "Yes" side is priced in most books. The table displays `P(Over/Yes)` and `EV Over` only, with `EV Under` left blank unless "No" odds exist. If `PROP_ALLOW_MODEL_ODDS = FALSE` and no market odds are found, the "Yes" odds stay blank and the recommendation is `PASS`.


---

## Mathematical Formulas

### Moneyline Conversion (Exact Forms)

Probability to American moneyline:

```
ML_fair(p) =
  -round(100 * p / (1 - p))      if p >= 0.5
   round(100 * (1 - p) / p)      if p < 0.5
```

In this codebase, fair home model moneyline is:

```
blend_home_ml = probability_to_american(blend_home_prob_shrunk)
```

American moneyline to implied probability:

```
P_implied(ML) =
  (-ML) / ((-ML) + 100)          if ML < 0
   100 / (ML + 100)              if ML > 0
```

Roundtrip consistency target:

```
american_to_probability(probability_to_american(p)) ≈ p
```

### Vigged Model Moneyline (Derived from Fair ML)

Vig is applied to moneyline price, not to probability coherence checks:

```
ML_vigged(ML_fair, v) =
  -round(abs(ML_fair) * (1 + v))   if ML_fair < 0
   round(ML_fair / (1 + v))        if ML_fair > 0
```

In this codebase:

```
blend_home_ml_vig = apply_moneyline_vig(blend_home_ml, vig)
```

`blend_home_ml_vig` is for display/comparison only and is intentionally excluded from fair-probability coherence validation.

### Brier Score

**Definition**:
```
Brier = (1/N) × Σ(p_i - y_i)²

Where:
- N = number of predictions
- p_i = predicted probability for game i
- y_i = actual outcome (1 if predicted team won, 0 otherwise)
- Range: 0 (perfect) to 1 (worst)
- Lower is better
```

**Interpretation**:
- 0.25 = random guessing (always predict 50%)
- 0.20 = professional models (Vegas, FiveThirtyEight)
- 0.211 = this model
- 0.208 = Vegas market (baseline)

### Log-Loss (Cross-Entropy)

**Definition**:
```
LogLoss = -(1/N) × Σ[y_i × log(p_i) + (1-y_i) × log(1-p_i)]

With ε-clamping to prevent log(0):
p_i_clamped = max(ε, min(1-ε, p_i))
where ε = 10⁻⁹ (PROB_EPSILON from R/utils.R)

- Penalizes confident wrong predictions heavily
- Lower is better
```

**Properties**:
- Proper scoring rule (encourages honest probabilities)
- Unbounded (can be arbitrarily bad)
- More sensitive to extreme probabilities than Brier

### Negative Binomial Distribution

**Probability Mass Function**:
```
P(Y = k) = Γ(k + φ) / (Γ(k+1) × Γ(φ)) × (φ/(φ+μ))^φ × (μ/(φ+μ))^k

Where:
- μ = mean
- φ = dispersion parameter
- k = observed count (points scored)
```

**Mean-Variance Relationship**:
```
E[Y] = μ
Var[Y] = μ + μ²/φ

For NFL games:
- μ ≈ 20-30 points
- φ ≈ 8
- Var[Y] ≈ μ + μ²/8 (higher variance than Poisson)
```

**Why Negative Binomial?**
- Poisson: Var = Mean (too restrictive for NFL)
- Negative Binomial: Var > Mean (captures high variability)
- Better fit for scoring patterns

### Gaussian Copula for Correlation

**Transformation**:
```
1. Generate bivariate normal (Z₁, Z₂) with correlation ρ
   [Z₁]     [  0  ]   [  1    ρ  ]
   [Z₂] ~ N([  0  ], [  ρ    1  ])

2. Transform to uniform: U₁ = Φ(Z₁), U₂ = Φ(Z₂)
   where Φ is standard normal CDF

3. Transform to scores:
   Y₁ = F⁻¹_NB(U₁; μ₁, φ₁)  # Home score
   Y₂ = F⁻¹_NB(U₂; μ₂, φ₂)  # Away score
```

**Correlation Parameter** (ρ):
```
Typical values: -0.1 to -0.2

Interpretation:
- ρ = 0: Independent scores
- ρ < 0: Negative correlation (defensive games)
- ρ > 0: Positive correlation (shootouts)

Empirical: Mild negative correlation
(when one team scores more, other scores slightly less)
```

### Isotonic Regression Calibration

**Objective**:
```
Find monotonic function f such that:
  minimize Σ w_i × (f(p_i) - y_i)²
  subject to: f is non-decreasing

Solution via Pool Adjacent Violators Algorithm (PAVA)
```

**Algorithm**:
```
1. Sort predictions p_i in ascending order
2. Initialize f(p_i) = observed frequency in bin
3. While adjacent bins violate monotonicity:
   - Pool violating bins
   - Set both to weighted average
   - Repeat until all bins satisfy f(p_i) ≤ f(p_j) for i < j
```

**Example**:
```
Predicted: 0.3, 0.5, 0.7, 0.9
Observed:  0.2, 0.6, 0.5, 0.95

Before:  [0.2, 0.6, 0.5, 0.95]  # Violates at 0.6 > 0.5
After:   [0.2, 0.55, 0.55, 0.95]  # Monotonic
```

---

## R 4.5.1 Compatibility

### Critical Changes in R 4.5.1

**1. dplyr::lag() Requires Scalar Defaults**

**Problem** (R < 4.5.1):
```r
df %>%
  group_by(team) %>%
  mutate(
    lag_points = lag(points, default = points)  # WORKS in R 4.4
  )
```

**Solution** (R 4.5.1+):
```r
df %>%
  group_by(team) %>%
  mutate(
    lag_points = lag(points, default = NA_real_),  # Scalar default
    lag_points = coalesce(lag_points, points)      # Handle NAs
  )
```

**All Fixes Applied**:
- NFLsimulation.R: Lines 4157-4166 (momentum calculations)
- All lag() calls use `default = NA_real_` or `default = 0`
- Use `coalesce()` to replace NAs with appropriate values

**2. tidyr::pivot_longer() Column Conflicts**

**Problem**:
```r
df %>%
  pivot_longer(cols = c(home_team, away_team),
               names_to = "location",  # Creates "location" column
               values_to = "team")     # Conflicts if "location" already exists
```

**Solution**:
```r
df %>%
  select(-any_of("location")) %>%  # Remove before pivot
  pivot_longer(cols = c(home_team, away_team),
               names_to = "location",
               values_to = "team") %>%
  select(-location)  # Remove after if not needed
```

**All Fixes Applied**:
- NFLsimulation.R: Lines 4141, 4184, 4216
- Each pivot_longer has `select(-any_of("location"))` before
- Prevents "Names must be unique" errors

**3. RNGversion() Before set.seed()**

**Recommended**:
```r
if (getRversion() >= "4.5.0") {
  suppressWarnings(RNGversion("4.5.0"))
}
set.seed(471)
```

**Applied in**:
- config.R
- All validation scripts
- Any script using random number generation

### Compatibility Test Suite

Run `comprehensive_r451_test_suite.R` to verify:
```r
source("comprehensive_r451_test_suite.R")

Tests:
✓ dplyr::lag() with scalar defaults
✓ tidyr::pivot_longer() without column conflicts
✓ Package versions compatible with R 4.5.1
✓ RNG consistency
✓ No deprecated functions
✓ All core functions execute without errors
```

---

## Performance Benchmarks

### Computational Performance

**Typical Runtime** (2024 MacBook Pro M3, 16GB RAM):

| Task | Time | Notes |
|------|------|-------|
| Load data (3 seasons) | 5-10 sec | Downloads from nflreadr |
| Calculate team stats | 10-15 sec | EPA, SOS, recent form |
| Load injuries | 2-5 sec | nflreadr API call |
| Fit GLMM | 3-5 sec | glmmTMB optimization |
| Run simulations (1 game) | 0.5-1 sec | 100,000 trials |
| **Full week (10-15 games)** | **60-90 sec** | **End-to-end** |
| Hyperparameter tuning | 1.5-2 hours | 81 grid points |
| Full validation pipeline | 2-3 hours | Train/val/test splits |

**Optimization Tips**:
```r
# Faster testing (reduce trials)
N_TRIALS <- 10000  # vs 100000 (10× faster)

# Parallel processing (if available)
library(parallel)
cl <- makeCluster(detectCores() - 1)
# Use with validation pipeline
```

### Statistical Performance

**Out-of-Sample Metrics** (2023-2024 Test Set):

| Metric | Value | 95% CI | Target | Status |
|--------|-------|--------|--------|--------|
| Brier Score | 0.211 | (0.205, 0.217) | <0.215 | ✓ |
| Log-Loss | 0.614 | (0.602, 0.626) | <0.630 | ✓ |
| Accuracy | 67.1% | (65.3%, 68.9%) | >65% | ✓ |
| RMSE | 10.82 | (10.51, 11.13) | <11.0 | ✓ |
| Coverage (90% CI) | 89.2% | (87.5%, 90.9%) | 88-92% | ✓ |

**Calibration Quality**:
```
Predicted %   Observed %   N_games
20-30%        24.1%        156
30-40%        33.8%        203
40-50%        44.2%        287
50-60%        54.7%        312
60-70%        63.1%        264
70-80%        72.4%        189
80-90%        81.9%        134

Mean Absolute Error: 2.1% (excellent calibration)
```

**By Game Type**:

| Type | Accuracy | Brier | Notes |
|------|----------|-------|-------|
| Division | 64.2% | 0.225 | Closer games |
| Conference | 66.8% | 0.213 | Similar to avg |
| Non-conference | 68.9% | 0.204 | Easier to predict |
| Thursday | 62.1% | 0.238 | Short rest affects both |
| Sunday | 67.4% | 0.210 | Standard |
| Monday | 66.2% | 0.218 | Slightly harder |
| Primetime | 65.3% | 0.221 | More variance |

---

## File Reference Table

| File | Lines | Purpose | Status |
|------|-------|---------|--------|
| **Core** | | | |
| config.R | 750+ | Configuration (all model parameters) | Active |
| NFLsimulation.R | 7,800+ | Main simulation engine | Active |
| NFLmarket.R | 3,900+ | Market data and comparisons | Active |
| NFLbrier_logloss.R | 1,200 | Evaluation metrics | Active |
| run_week.R | 50 | Entry point for weekly predictions | Active |
| **R/ Library** | | | |
| R/utils.R | 350+ | Core utilities (PROB_EPSILON, clamp_probability) | Active |
| R/data_validation.R | 500+ | Data quality tracking API | Active |
| R/logging.R | 200+ | Structured logging (log_info, log_warn, log_error) | Active |
| R/playoffs.R | 300+ | Playoff-specific logic | Active |
| R/date_resolver.R | 250+ | Week/date resolution | Active |
| R/sleeper_api.R | 350+ | Sleeper API injury data integration | Active |
| **Validation** | | | |
| validation_pipeline.R | 740 | Hyperparameter tuning | Active |
| model_validation.R | 935 | K-fold CV | Active |
| injury_model_validation.R | 668 | Injury validation | Active |
| professional_model_benchmarking.R | 750 | vs FTE/ESPN | Active |
| calibration_refinement.R | 680 | Calibration | Active |
| ensemble_calibration_implementation.R | 680 | Ensemble | Active |
| final_verification_checklist.R | 670 | Pre-deploy checks | Active |
| rolling_validation_system.R | 599 | Real-time monitor | Active |
| validation_reports.R | 388 | Report generation | Active |
| run_validation_example.R | 259 | Usage example | Active |
| **Testing** | | | |
| comprehensive_r451_test_suite.R | 463 | R 4.5.1 tests | Active |
| r451_compatibility_fixes.R | 482 | Compatibility | Reference |
| tests/testthat/test-utils.R | 300+ | Utils unit tests | Active |
| tests/testthat/test-data-validation.R | 200+ | Data validation tests | Active |
| tests/testthat/test-playoffs.R | 200+ | Playoff logic tests | Active |
| tests/testthat/test-date-resolver.R | 150+ | Date resolver tests | Active |
| tests/testthat/test-injury-model.R | 200+ | Injury model tests | Active |
| tests/testthat/test-weather.R | 200+ | Weather impact tests | Active |
| tests/testthat/test-logging.R | 150+ | Logging tests | Active |
| tests/testthat/test-sleeper-api.R | 200+ | Sleeper API tests | Active |
| **Validation** | | | |
| validation/injury_ab_comparison.R | 350+ | A/B test WITH/WITHOUT injuries | Active |
| **Documentation** | | | |
| README.md | 8 KB | Project overview | Active |
| GETTING_STARTED.md | 8 KB | Beginner guide | Active |
| DOCUMENTATION.md | This file | Technical reference | Active |
| CLAUDE.md | 2 KB | Claude Code context | Active |

**Total**: 20+ R files, 4 documentation files

---

**For beginner guide, see**: `GETTING_STARTED.md`
**For project overview, see**: `README.md`
