# NFL Prediction Model - Claude Code Context

This file provides context for Claude Code sessions working on this repository.

## Project Overview

This is an NFL game prediction model using Monte Carlo simulation with:
- Negative Binomial score distributions with Gaussian copula correlation
- Isotonic regression calibration
- 60% market shrinkage for probability estimates
- 1/8 Kelly staking with edge skepticism
- Strength-of-schedule and injury adjustments

## Repository Structure

```
nfl/
├── R/                          # Modular R package source
│   ├── utils.R                 # Core utility functions (CANONICAL)
│   ├── logging.R               # Structured logging utilities
│   └── data_validation.R       # Data validation & fallbacks
├── tests/
│   └── testthat/
│       └── test-utils.R        # Unit tests for utilities
├── NFLsimulation.R             # Main simulation engine (~7600 lines)
├── NFLmarket.R                 # Market analysis & reporting (~3900 lines)
├── NFLbrier_logloss.R          # Evaluation metrics (~1150 lines)
├── config.R                    # Configuration parameters
├── run_week.R                  # Entry point for weekly predictions
└── DESCRIPTION                 # R package metadata
```

## Key Design Decisions

### Canonical Function Locations

**R/utils.R** is the SINGLE SOURCE OF TRUTH for:
- `clamp_probability()` - epsilon = 1e-9
- `american_to_probability()`, `american_to_decimal()`
- `expected_value_units()`, `conservative_kelly_stake()`
- `shrink_probability_toward_market()` - default 60% market weight
- `brier_score()`, `log_loss()`, `accuracy()`
- `standardize_join_keys()` - **CRITICAL: coerces types for safe joins**

### Type-Safe Joins

The `standardize_join_keys()` function:
1. Renames column aliases to canonical names (game_id, season, week)
2. **Coerces to standard types**: game_id=character, season=integer, week=integer

This prevents the "JOIN PRODUCED ZERO ROWS" errors from type mismatches.

### Configuration

Use `config.R` for all tunable parameters. Do NOT hardcode:
- Shrinkage factors
- Kelly fractions
- Simulation counts
- Validation splits

## Common Tasks

### Run Weekly Simulation
```r
source("run_week.R")
# Edit WEEK_TO_SIM in config.R first
```

### Run Tests
```r
testthat::test_dir("tests/testthat")
```

### Validate Data
```r
source("R/data_validation.R")
report <- create_validation_report(schedule, predictions, injuries)
print_validation_report(report)
```

## Known Issues & Workarounds

1. **Injury Data for 2025**: May fail to load. The model will proceed with zero injury impact and log a warning.

2. **Join Failures**: If `build_moneyline_comparison_table()` returns empty:
   - Check that both sides have been passed through `standardize_join_keys()`
   - Verify overlapping values in game_id, season, week

3. **Calibration Leakage**: The nested CV isotonic calibration has minimal impact. Consider Platt scaling for simpler models.

## Code Style

- Use `dplyr` for data manipulation
- Prefer explicit `pkg::fun()` calls for non-standard packages
- Use `log_info()`, `log_warn()`, `log_error()` from R/logging.R instead of `cat()`
- No runtime `install.packages()` calls - use renv
- Document functions with roxygen2 style comments

## Testing Requirements

Before committing:
1. Run `testthat::test_dir("tests/testthat")`
2. Verify Week 16 simulation produces valid HTML output
3. Check that join operations don't produce empty results

## Performance Notes

- NFLsimulation.R uses Sobol QMC sequences with antithetic variates
- Default 100,000 trials; reduce to 40,000 for backtesting
- Memory usage scales with N_TRIALS * n_games

## Contact

For questions about model methodology, see DOCUMENTATION.md and RESULTS.md.
