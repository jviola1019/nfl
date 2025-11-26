# NFL Model - Train/Validation/Test Pipeline

> **Prerequisites**: See [README.md](README.md) for installation, basic setup, and model architecture. This document covers the hyperparameter tuning and validation pipeline specifically.

## Overview

This validation system implements proper out-of-sample hyperparameter tuning with explicit train/validation/test splits to prevent data leakage and ensure honest model evaluation.

### Key Principles

- **No data leakage**: Calibration and hyperparameters trained only on TRAIN data
- **Honest evaluation**: VALIDATION and TEST sets never used for model selection
- **Reproducible results**: Clear separation of tuning, validation, and testing phases

## Data Splits

The validation schema (defined in `config.R`) divides historical data into three temporal windows:

```r
VALIDATION_SCHEMA <- list(
  tune  = list(start_season = 2011L, end_season = 2018L),  # TRAIN: 2011-2018
  valid = list(start_season = 2019L, end_season = 2022L),  # VALIDATION: 2019-2022
  test  = list(start_season = 2023L, end_season = SEASON)  # TEST: 2023-current
)
```

### Purpose of Each Phase

| Phase | Years | Purpose | What Gets Fixed? |
|-------|-------|---------|------------------|
| **TRAIN** | 2011-2018 | Hyperparameter grid search | Best hyperparameters selected |
| **VALIDATION** | 2019-2022 | Model selection and evaluation | Nothing (frozen from TRAIN) |
| **TEST** | 2023-current | Final performance assessment | Nothing (frozen from TRAIN) |

## Key Files

1. **`config.R`** - Defines `VALIDATION_SCHEMA`, `RECENCY_HALFLIFE`, and `BACKTEST_TRIALS`

2. **`validation_pipeline.R`** (if available)
   - `tune_hyperparams()`: Grid search on TRAIN set
   - `build_calibration()`: Train calibration on TRAIN set only
   - `evaluate_phase()`: Evaluate on specific window
   - `run_full_validation()`: Complete pipeline

3. **`validation_reports.R`** (if available)
   - `generate_validation_report()`: Phase-labeled tables
   - `save_validation_html()`: HTML output with phase markers

4. **`run_validation_example.R`** (if available)
   - `run_quick_validation()`: Fast test with small grid
   - `run_full_validation_default()`: Full grid search

## Quick Start

### Option 1: Quick Validation (Recommended First Run)

```r
# Load configuration
source("config.R")

# Load validation functions (if available)
source("validation_pipeline.R")
source("run_validation_example.R")

# Load NFL data
library(nflreadr)
sched <- load_schedules()

# Run quick validation (~10 minutes)
results <- run_quick_validation()

# View summary
print_validation_summary(results, detailed = TRUE)
```

### Option 2: Full Validation Pipeline

```r
source("config.R")
source("validation_pipeline.R")
library(nflreadr)

# Load schedule for market comparisons
sched <- load_schedules()

# Run complete validation with default grid
results <- run_full_validation(
  schema       = VALIDATION_SCHEMA,
  tune_grid    = NULL,  # Use default 3x3x3x3 grid
  trials       = BACKTEST_TRIALS,
  save_results = TRUE,
  sched        = sched
)

# Print summary
print_validation_summary(results, detailed = TRUE)

# Generate HTML report
source("validation_reports.R")
report <- generate_validation_report(
  results,
  output_file = "validation_report.html"
)
```

## Hyperparameters Being Tuned

The default grid searches over four key hyperparameters:

### 1. GLMM_BLEND_W (0.3, 0.38, 0.5)

Weight on GLMM priors versus pace-based baseline.

- Lower values (0.3): Rely more on pace/total-based estimates
- Higher values (0.5): Rely more on GLMM team strength model
- Default range: 0.3 to 0.5

### 2. RECENCY_HALFLIFE (2.0, 3.0, 4.0)

Exponential decay halflife for recent game weighting (in games).

- Lower values (2.0): Stronger recency bias, recent games matter more
- Higher values (4.0): Weaker recency bias, longer history considered
- Default range: 2.0 to 4.0 games

### 3. N_RECENT (4, 6, 8)

Number of recent games to include in form calculation.

- Lower values (4): Only very recent games
- Higher values (8): Longer recent history window
- Default range: 4 to 8 games

### 4. SOS_STRENGTH (0.3, 0.45, 0.6)

Magnitude of strength-of-schedule adjustment.

- Lower values (0.3): Weaker SoS effect
- Higher values (0.6): Stronger SoS effect
- Default range: 0.3 to 0.6

## Validation Workflow

### Phase 1: TUNING (on TRAIN data)

1. **Grid Search**: Test all hyperparameter combinations on 2011-2018 data
2. **Score Each Config**: Run simulations for each configuration
3. **Select Best**: Choose configuration minimizing calibrated 2-way Brier score
4. **Tiebreaker**: If Brier scores tied, choose lower log-loss

**Output**: Best hyperparameters frozen for validation/test

### Phase 2: CALIBRATION (on TRAIN data)

1. **Build Calibration**: Train isotonic calibration on 2011-2018 using best hyperparameters
2. **Save Calibration**: Store calibration object in `run_logs/calibration_train_2011_2018.rds`
3. **Freeze**: Calibration is NOT retrained on validation or test data

**Output**: Calibration object frozen for validation/test

### Phase 3: VALIDATION (on held-out 2019-2022)

1. **Apply Frozen Config**: Use best hyperparameters from Phase 1
2. **Apply Frozen Calibration**: Use calibration from Phase 2
3. **Score**: Run simulations with frozen settings
4. **Compare to Market**: Compute Brier/log-loss versus market

**Output**: Out-of-sample performance on held-out data

### Phase 4: TEST (on forward 2023-current)

1. **Apply Frozen Config**: Same hyperparameters from Phase 1
2. **Apply Frozen Calibration**: Same calibration from Phase 2
3. **Score**: Run simulations with frozen settings
4. **Compare to Market**: Final test performance versus market

**Output**: Final test set performance (true out-of-sample)

## Interpreting Results

### Good Validation Example

```
Performance Summary (Brier scores):
  TRAIN      (2011-2018): 0.2105
  VALIDATION (2019-2022): 0.2118  ← Small increase is expected
  TEST       (2023-2024): 0.2112  ← Similar to validation

Market Comparison (Validation):
  Model vs Market delta: +0.0028 Brier  ← Close to market

Market Comparison (Test):
  Model vs Market delta: +0.0031 Brier  ← Consistent with validation
```

**Interpretation**: Model generalizes well. The small increase from TRAIN to VALIDATION/TEST is expected and acceptable. Consistent performance across validation and test indicates stable out-of-sample behavior.

### Overfitting Warning

```
Performance Summary (Brier scores):
  TRAIN      (2011-2018): 0.2000  ← Very low
  VALIDATION (2019-2022): 0.2200  ← Much higher ⚠
  TEST       (2023-2024): 0.2250  ← Even higher ⚠
```

**Interpretation**: Model is overfit to training data. Consider reducing model complexity, trying simpler hyperparameter values, or adding regularization.

## Common Tasks

### Apply Best Hyperparameters to Live Simulation

```r
# Load validation results
results <- readRDS("run_logs/validation_results_YYYYMMDD_HHMMSS.rds")

# Apply to global environment
apply_best_hyperparams(results$tuning$best_params)

# Update config.R with these values
cat("\nUpdate config.R with:\n")
cat(sprintf("GLMM_BLEND_W <- %.3f\n", results$tuning$best_params$GLMM_BLEND_W))
cat(sprintf("RECENCY_HALFLIFE <- %.2f\n", results$tuning$best_params$RECENCY_HALFLIFE))
cat(sprintf("N_RECENT <- %d\n", results$tuning$best_params$N_RECENT))
cat(sprintf("SOS_STRENGTH <- %.3f\n", results$tuning$best_params$SOS_STRENGTH))
```

### Generate HTML Report

```r
source("validation_reports.R")

# Load results
results <- readRDS("run_logs/validation_results_YYYYMMDD_HHMMSS.rds")

# Generate HTML
report <- generate_validation_report(
  results,
  output_file = "validation_report.html"
)

# Open in browser
browseURL("validation_report.html")
```

## Important Notes

### Data Leakage Prevention

**NEVER**:

- Train calibration on validation or test data
- Select hyperparameters based on test performance
- Modify model based on test results and re-evaluate

**ALWAYS**:

- Use TRAIN data only for hyperparameter selection and calibration
- Keep validation and test sets completely frozen
- Report all three phases (train/valid/test) in results

### Calibration Behavior

The existing calibration system in `NFLsimulation.R` uses:

1. **Global isotonic mapping** (`map_iso`): Trained on all calibration data
2. **Nested CV isotonic mapping** (`isotonic_mappings`): Leave-one-week-out for backtesting
3. **3-way multinomial** (`cal3`): For tie probabilities

The `build_calibration()` function captures these objects after running simulations on the training window. These objects are then frozen and reused for validation/test without refitting.

### Caching

The `score_weeks()` function uses disk caching. This means:

- First run of a configuration: Full simulation (slow)
- Subsequent runs: Cache hit (fast)
- Cache key includes: season range, weeks, trials, seed, and key parameters

If you change hyperparameters that affect simulation, the cache will miss and re-simulate.

## File Outputs

All validation runs save results to the `run_logs/` directory:

```
run_logs/
├── calibration_train_2011_2018.rds       # Frozen calibration object
├── validation_results_YYYYMMDD_HHMMSS.rds  # Full validation results
└── validation_report.html                # HTML report with phase labels
```

## Troubleshooting

### "All hyperparameter configurations failed"

**Cause**: Data not available for training window (2011-2018)

**Solution**: Check that `nflreadr::load_schedules()` includes 2011+ data. If not, adjust `VALIDATION_SCHEMA$tune` to available years.

### Validation takes too long

**Cause**: Large grid with many combinations

**Solutions**:

1. Use `run_quick_validation()` with small grid
2. Reduce grid size by fixing some parameters
3. Reduce `BACKTEST_TRIALS` (but increases variance)

### Market comparison returns empty

**Cause**: Missing market probability data in schedule

**Solution**: Ensure `nflreadr::load_schedules()` includes betting lines. Market data may not be available for all historical seasons.

## Next Steps

1. **Run Initial Validation**:
   ```r
   results <- run_quick_validation()
   ```

2. **Review Results**:
   ```r
   print_validation_summary(results, detailed = TRUE)
   ```

3. **If Results Look Good**: Run full grid
   ```r
   results_full <- run_full_validation_default()
   ```

4. **Apply Best Parameters**: Update `config.R` with best hyperparameters

5. **Use for Live Predictions**: Run `NFLsimulation.R` with tuned hyperparameters

---

For more information on model architecture, validation methodology, and performance benchmarks, see [README.md](README.md).
