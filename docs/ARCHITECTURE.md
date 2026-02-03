# NFL Prediction Model - Architecture

**Version**: 2.7.0
**Last Updated**: 2026-02-02

---

## Overview

This document describes the architecture of the NFL prediction model, designed as a blueprint for multi-sport prediction systems.

---

## System Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              USER INTERFACE                                 │
├─────────────────────────────────────────────────────────────────────────────┤
│  run_week.R (Entry Point)  │  config.R (Parameters)  │  HTML Report        │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            SIMULATION ENGINE                                │
├─────────────────────────────────────────────────────────────────────────────┤
│  NFLsimulation.R                                                            │
│  ├── simulate_game_nb()      Monte Carlo + Gaussian copula                  │
│  ├── calc_injury_impacts()   Position-weighted adjustments                  │
│  ├── safe_hourly()           Weather integration                            │
│  └── score_weeks()           Multi-week scoring with caching                │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            MARKET ANALYSIS                                  │
├─────────────────────────────────────────────────────────────────────────────┤
│  NFLmarket.R                                                                │
│  ├── build_moneyline_comparison_table()   Market vs model comparison        │
│  ├── shrink_probability_toward_market()   60% market shrinkage              │
│  ├── conservative_kelly_stake()           1/8 Kelly with edge caps          │
│  └── render_moneyline_comparison_html()   Report generation                 │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            VALIDATION & METRICS                             │
├─────────────────────────────────────────────────────────────────────────────┤
│  NFLbrier_logloss.R                                                         │
│  ├── brier_score()           Brier scoring                                  │
│  ├── log_loss()              Cross-entropy loss                             │
│  └── compare_to_market()     Model vs Vegas comparison                      │
└─────────────────────────────────────────────────────────────────────────────┘
                                      │
                                      ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                            CORE UTILITIES                                   │
├─────────────────────────────────────────────────────────────────────────────┤
│  R/utils.R              Core functions (SINGLE SOURCE)                      │
│  R/data_validation.R    Quality tracking                                    │
│  R/logging.R            Structured logging                                  │
│  R/playoffs.R           Playoff logic                                       │
│  R/date_resolver.R      Date handling                                       │
└─────────────────────────────────────────────────────────────────────────────┘
```

---

## Data Flow

```
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│   nflreadr   │    │  ESPN API    │    │ Sleeper API  │
│  (schedules, │    │   (market    │    │  (injuries,  │
│   outcomes)  │    │    odds)     │    │   real-time) │
└──────┬───────┘    └──────┬───────┘    └──────┬───────┘
       │                   │                   │
       ▼                   ▼                   ▼
┌─────────────────────────────────────────────────────┐
│              DATA INTEGRATION LAYER                 │
│  standardize_join_keys() → type coercion            │
│  safe_load_injuries() → robust loading              │
│  update_*_quality() → quality tracking              │
└─────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────┐
│              FEATURE ENGINEERING                    │
│  • Strength of Schedule (SoS)                       │
│  • Injury impacts (position-weighted)              │
│  • Weather effects (wind, precipitation)            │
│  • Rest/travel adjustments                          │
│  • Coaching change penalties                        │
└─────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────┐
│              MONTE CARLO SIMULATION                 │
│  • Negative binomial score distributions            │
│  • Gaussian copula correlation (rho ≈ -0.15)        │
│  • Sobol QMC sequences                              │
│  • Antithetic variance reduction                    │
│  • 100,000 trials per game                          │
└─────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────┐
│              CALIBRATION                            │
│  • GAM spline with smoothing penalty                │
│  • -6.9% Brier improvement vs raw                   │
│  • Trained on 2019-2022, tested on 2023+            │
└─────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────┐
│              MARKET COMPARISON                      │
│  • 60% shrinkage toward market consensus            │
│  • Expected value calculation                       │
│  • 1/8 Kelly staking with edge caps                 │
│  • Edge quality flags (>15% = implausible)          │
└─────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────┐
│              OUTPUT                                 │
│  • NFLvsmarket_report.html                          │
│  • run_logs/*.rds                                   │
│  • Console: quality badge, progress                 │
└─────────────────────────────────────────────────────┘
```

---

## Statistical Methodology

### Score Distribution: Negative Binomial

NFL scores exhibit overdispersion (variance > mean), making negative binomial superior to Poisson:

```
Score ~ NegBin(mu, size)
where size = mu^2 / (var - mu)
```

### Score Correlation: Gaussian Copula

Game dynamics create correlated scores (defensive battles, shootouts):

```
1. Generate correlated uniform (U_home, U_away) via Gaussian copula
2. Transform: Score_home = qnbinom(U_home, size, mu)
3. Transform: Score_away = qnbinom(U_away, size, mu)
```

Typical NFL rho ≈ -0.15 (slight negative correlation)

### Calibration: GAM Spline

```
cal_prob = gam(outcome ~ s(raw_prob, k=10))
```

Smoothing penalty prevents overfitting. Validated improvement: -6.9% Brier.

### Probability Shrinkage

```
shrunk = 0.4 * model_prob + 0.6 * market_prob
```

Rationale: NFL markets are extremely efficient. Divergence > 10% from market is likely model overconfidence.

### Kelly Criterion

```
kelly = (p * b - q) / b
where p = win prob, q = 1-p, b = decimal odds - 1

conservative_stake = kelly * 0.125  # 1/8 Kelly
capped_stake = min(conservative_stake, 0.02)  # 2% max
```

---

## Directory Structure

```
nfl/
├── run_week.R              # Entry point
├── config.R                # Configuration parameters
├── NFLsimulation.R         # Core simulation engine (8,300+ lines)
├── NFLmarket.R             # Market analysis (3,900+ lines)
├── NFLbrier_logloss.R      # Scoring metrics (1,200+ lines)
├── injury_scalp.R          # Injury data processing
├── validation_reports.R    # Report generation
│
├── R/                      # Core utility modules
│   ├── utils.R             # Shared utilities (CANONICAL)
│   ├── data_validation.R   # Quality tracking
│   ├── logging.R           # Structured logging
│   ├── playoffs.R          # Playoff logic
│   ├── date_resolver.R     # Date handling
│   ├── sleeper_api.R       # Sleeper fantasy integration
│   ├── coaching_adjustments.R
│   ├── red_zone_data.R
│   ├── simulation_helpers.R
│   └── model_diagnostics.R
│
├── docs/                   # Documentation
│   ├── API.md              # Function reference
│   ├── ARCHITECTURE.md     # This file
│   └── CONTRIBUTING.md     # Contributor guide
│
├── tests/testthat/         # Unit tests (~575 tests)
│   ├── test-utils.R
│   ├── test-data-validation.R
│   ├── test-playoffs.R
│   ├── test-injury-model.R
│   ├── test-weather.R
│   └── test-calibration.R
│
├── validation/             # Validation scripts
│   ├── injury_model_validation.R
│   ├── weather_validation.R
│   └── calibration_validation.R
│
├── scripts/                # Utility scripts
│   ├── verify_repo_integrity.R  # 35-check verification
│   └── run_matrix.R        # Run all artifacts
│
├── archive/                # Deprecated files
│
└── run_logs/               # Generated artifacts
```

---

## Future: Multi-Sport Blueprint

### Target Structure

```
sports-prediction/
├── core/                   # Sport-agnostic core
│   ├── simulation_engine.R
│   ├── calibration.R
│   ├── market_analysis.R
│   ├── metrics.R
│   └── utils.R
│
├── sports/                 # Sport-specific implementations
│   ├── nfl/
│   │   ├── config.R
│   │   ├── simulation.R    # NFL-specific adjustments
│   │   ├── injuries.R
│   │   └── props/          # Player props
│   │       ├── passing_yards.R
│   │       ├── rushing_yards.R
│   │       └── receiving_yards.R
│   │
│   ├── nba/                # (Future)
│   └── mlb/                # (Future)
│
└── run.R                   # Universal entry point
```

### Key Abstractions

**Generic Simulation Interface**:
```r
simulate_game <- function(
  home_expected,
  away_expected,
  home_variance,
  away_variance,
  score_correlation,
  distribution = "negbin"  # or "poisson", "normal"
)
```

**Sport-Specific Config**:
```r
# NFL
SCORE_DISTRIBUTION <- "negbin"
HOME_ADVANTAGE <- 2.5
SCORE_CORRELATION <- -0.15

# NBA (future)
SCORE_DISTRIBUTION <- "normal"
HOME_ADVANTAGE <- 3.0
SCORE_CORRELATION <- -0.10
```

---

## Validation Status

| Component | Status | Method |
|-----------|--------|--------|
| Brier Score | 0.211 | Test set 2023-2024 |
| 95% CI | (0.205, 0.217) | Bootstrap n=1000 |
| K-fold CV | Valid | 10-fold, stratified by season |
| Train/Test Splits | Valid | No leakage |
| Calibration | Valid | Separate train/test |
| Position Weights | p < 0.001 | A/B comparison |
| Shrinkage | Optimized | Grid search on validation |

---

## Design Decisions

### Why Negative Binomial over Poisson?

NFL scores show overdispersion (variance ≈ 100, mean ≈ 23). Negative binomial captures this naturally.

### Why 60% Market Shrinkage?

Empirical optimization on validation set. Lower shrinkage = overconfident. Higher = too conservative.

### Why 1/8 Kelly?

Full Kelly is too volatile. 1/8 Kelly balances growth with drawdown risk. Empirically optimal for sports betting.

### Why Disable Snap Weighting?

No validated improvement in Brier/log-loss. Position-level weights (validated p < 0.001) remain active.

---

*Generated: 2026-02-02*
