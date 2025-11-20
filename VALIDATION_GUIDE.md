# NFL Model Validation Guide

## Overview

This guide explains how to run comprehensive k-fold cross-validation and statistical significance tests on the NFL prediction model.

## Requirements

- **R Version**: 4.5.1 or later
- **Required Packages**: See installation section below

## Quick Start

### 1. Install R 4.5.1+

```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install r-base r-base-dev

# macOS
brew install r

# Verify version
R --version  # Should show 4.5.1 or later
```

### 2. Run Validation Script

```bash
# From the project directory
Rscript model_validation.R
```

This will:
- Perform 10-fold cross-validation
- Test statistical significance of all model components
- Calculate effect sizes and ICC
- Check R 4.5.1 compatibility
- Generate comprehensive report
- Save results to `model_validation_full_results.rds`

### 3. View Results

```r
# In R console
results <- readRDS("model_validation_full_results.rds")

# View cross-validation performance
results$cv_results$summary_stats

# View significance tests
results$significance_results$significance_results

# View recommendations
results$recommendations
```

## What Gets Tested

### Core Model Components

The base GLMM model:
```r
points ~ is_home + (1|team) + (1|opp)
family = nbinom2
```

**Tests performed**:
1. **Cross-Validation**: 10-fold CV with stratification by points
   - RMSE (Root Mean Squared Error)
   - MAE (Mean Absolute Error)
   - Log Loss
   - Brier Score

2. **Significance Tests**: Likelihood Ratio Tests (LRT)
   - `is_home` fixed effect (home field advantage)
   - `team` random effect (team offensive quality)
   - `opp` random effect (opponent defensive quality)

3. **Effect Sizes**:
   - Home field advantage in points
   - Team quality variation (SD)
   - Opponent strength variation (SD)
   - ICC (Intraclass Correlation Coefficient)

4. **Model Diagnostics**:
   - Convergence checks
   - Outlier detection
   - Residual normality (Shapiro-Wilk test)
   - Heteroscedasticity tests

### Adjustment Variables (Priority Testing)

These adjustments are applied on top of the base model and should be tested individually:

#### HIGH PRIORITY
1. **Rest Adjustments**
   - Short rest penalty (≤6 days): -0.85 pts
   - Long rest bonus (≥9 days): +0.5 pts
   - Bye week bonus: +1.0 pts

2. **Injury Adjustments**
   - Skill positions: 0.55 pts/flag
   - Trench (OL/DL): 0.65 pts/flag
   - Secondary: 0.45 pts/flag
   - Front 7: 0.50 pts/flag

3. **Weather Adjustments**
   - Dome bonus: +0.8 pts to total
   - Wind penalty: -1.2 pts
   - Cold penalty: -0.6 pts
   - Rain/snow: -0.8 pts

4. **QB Impact**
   - Availability adjustments
   - On/off EPA differential

5. **Recent Form** (EPA-based)
   - Recency halflife: 3 games
   - Exponential decay weighting

6. **Red Zone Efficiency**
   - Trip rate
   - TD conversion rate
   - Goal-to-go efficiency

#### MEDIUM PRIORITY
7. **Travel Adjustments**
   - West→East early games: -0.4 pts
   - Timezone effects

8. **Pass Protection vs Rush**
   - Pressure mismatch: ~0.6 pts per 10% edge
   - Correlation with scoring

9. **Explosive Play Rates**
   - 20+ yard plays
   - ~0.8 pts per 5% edge

10. **Third Down Conversion**
    - Conversion rate vs win probability
    - Short/medium/long situations

11. **Turnover Metrics**
    - Interception rates
    - Fumble rates
    - Impact on points

12. **Schedule Strength (SoS)**
    - SOS_STRENGTH = 0.45
    - Opponent-adjusted performance

#### LOW PRIORITY
13. **Division Game Adjustments**: -0.4 pts
14. **Conference Game Adjustments**: -0.2 pts

## Testing Methodology

### For Each Adjustment Variable:

```r
# 1. Fit model WITH the adjustment
model_with <- fit_model_with_adjustment(data, adjustment = TRUE)
preds_with <- predict_and_evaluate(model_with, test_data)

# 2. Fit model WITHOUT the adjustment
model_without <- fit_model_with_adjustment(data, adjustment = FALSE)
preds_without <- predict_and_evaluate(model_without, test_data)

# 3. Compare performance
improvement <- compare_models(preds_with, preds_without)

# 4. Bootstrap for statistical significance
boot_results <- boot::boot(
  data = test_data,
  statistic = brier_difference,
  R = 1000,
  model_with = model_with,
  model_without = model_without
)

# 5. Decision rule
if (improvement$p_value < 0.10 & improvement$brier_reduction > 0.001) {
  decision <- "RETAIN - Statistically and practically significant"
} else if (improvement$p_value >= 0.10) {
  decision <- "CONSIDER REMOVING - Not statistically significant"
} else {
  decision <- "REVIEW - Significant but small effect"
}
```

### Decision Criteria

**RETAIN** if:
- p-value < 0.05 (significant)
- Brier score improvement > 0.001 (practical benefit)
- OR strong domain knowledge justification

**REMOVE** if:
- p-value > 0.10 (not significant)
- No practical improvement in predictions
- AND no strong theoretical justification

**REVIEW** if:
- Marginally significant (0.05 < p < 0.10)
- Small effect size
- May depend on sample size or specific scenarios

## Expected Results

### Baseline Performance (from RESULTS.md)
- Model Brier Score: ~0.210
- Market Brier Score: ~0.208
- Model Log Loss: ~0.608
- Market Log Loss: ~0.604

### Core Model Expected Significance
Based on NFL literature and prior analyses:

| Component | Expected p-value | Expected Effect |
|-----------|-----------------|----------------|
| is_home | < 0.001 | ~2.0-2.5 points |
| team RE | < 0.001 | SD ~3-5 points |
| opp RE | < 0.001 | SD ~3-5 points |

**ICC Expected**: 40-60% of variance explained by team/opponent structure

### Cross-Validation Expected Performance
- **RMSE**: 10-12 points (typical NFL game prediction)
- **MAE**: 8-10 points
- **Log Loss**: ~0.60-0.65
- **Brier Score**: ~0.20-0.22

## Interpreting Results

### Cross-Validation Performance

**RMSE Interpretation**:
- < 11 points: **EXCELLENT** - Model has good precision
- 11-13 points: **GOOD** - Acceptable for NFL prediction
- 13-15 points: **FAIR** - Room for improvement
- > 15 points: **POOR** - Major issues with model

**Brier Score Interpretation**:
- < 0.200: **EXCELLENT** - Better than typical sportsbooks
- 0.200-0.215: **GOOD** - Competitive with market
- 0.215-0.230: **FAIR** - Decent but needs work
- > 0.230: **POOR** - Significant calibration issues

### Statistical Significance

**p-value < 0.001**: Highly significant - Definitely keep
**p-value < 0.01**: Very significant - Strong evidence to keep
**p-value < 0.05**: Significant - Keep
**p-value < 0.10**: Marginally significant - Review
**p-value ≥ 0.10**: Not significant - Consider removing

### Effect Sizes

**Home Field Advantage**:
- Expected: 2.0-2.5 points
- If < 1.5 or > 3.5: Review data or model specification

**Team Quality Variation (SD)**:
- Expected: 3-5 points
- Measures spread of team offensive strength

**Opponent Strength Variation (SD)**:
- Expected: 3-5 points
- Measures spread of defensive quality

## Common Issues & Solutions

### Issue 1: Model Fails to Converge

**Symptoms**: Warning messages about convergence failure

**Solutions**:
1. Increase max iterations: `optCtrl = list(iter.max = 2000)`
2. Try different optimizer: `optimizer = optim`
3. Scale predictors to similar ranges
4. Check for perfect separation or multicollinearity

### Issue 2: High RMSE (> 15 points)

**Possible causes**:
- Missing important predictors
- Outliers not handled properly
- Model mis-specification (wrong distribution)
- Data quality issues

**Solutions**:
1. Add more predictive features (injuries, weather, etc.)
2. Use robust methods or downweight outliers
3. Try different error distributions
4. Check data cleaning procedures

### Issue 3: Non-Significant Components

**If `is_home` is not significant** (unlikely):
- Check if home/away coded correctly
- May vary by season/era
- Consider team-specific HFA

**If random effects not significant** (very unlikely):
- Check sufficient data per team
- May indicate all teams equal (unrealistic)
- Review model specification

### Issue 4: Poor Calibration (High Brier)

**Causes**:
- Probabilities not well-calibrated
- Over/under-confident predictions
- Not accounting for uncertainty properly

**Solutions**:
1. Apply isotonic regression calibration (already implemented)
2. Use nested cross-validation (already implemented)
3. Add more context-specific adjustments
4. Review probability generation from simulations

## R 4.5.1 Specific Compatibility

### Key Changes in R 4.5.1

1. **Matrix/Array Handling**: Stricter dimension handling
   - Use explicit `drop = FALSE` when subsetting
   - Check `matrix()` calls for compatibility

2. **Package Updates**: Ensure latest versions
   ```r
   update.packages(ask = FALSE, checkBuilt = TRUE)
   ```

3. **RNG Changes**: May affect reproducibility
   - Set `RNGkind()` explicitly if needed
   - Document RNG version in results

4. **Formula Interface**: Minor changes in model.frame()
   - Mostly backward compatible
   - Test all formula specifications

### Compatibility Checks in Script

The validation script automatically:
1. Checks R version ≥ 4.5.1
2. Tests critical packages (glmmTMB, nflreadr, tidyverse)
3. Verifies core functionality works
4. Reports any compatibility issues

## Advanced Usage

### Custom Adjustments Testing

To test a specific adjustment:

```r
# Load the validation functions
source("model_validation.R")

# Define custom test
test_custom_adjustment <- function(data, adjustment_name) {
  # Your testing code here
}

# Run test
results <- test_custom_adjustment(stacked, "my_adjustment")
```

### Parallel Processing

For faster cross-validation:

```r
library(doParallel)
registerDoParallel(cores = 4)

# CV will automatically use parallel backend
cv_results <- perform_kfold_cv(stacked, k = 10)
```

### Bootstrap Confidence Intervals

For more robust uncertainty estimates:

```r
library(boot)

# Define bootstrap statistic
brier_stat <- function(data, indices) {
  d <- data[indices, ]
  model <- fit_model(d)
  preds <- predict(model, newdata = d)
  return(mean((d$actual - preds)^2))
}

# Run bootstrap
boot_results <- boot(data, brier_stat, R = 1000)
boot.ci(boot_results, type = "bca")  # BCa confidence interval
```

## Output Files

After running validation:

| File | Contents |
|------|----------|
| `model_validation_full_results.rds` | All results in single object |
| `model_validation_cv_results.rds` | Cross-validation results |
| `model_validation_significance_results.rds` | Significance tests |
| `model_validation_effect_sizes.rds` | Effect size calculations |
| `model_validation_diagnostics.rds` | Model diagnostics |
| `model_validation_compatibility.rds` | R 4.5.1 compatibility check |
| `model_validation_output.txt` | Console output log |

## Next Steps After Validation

1. **Review Report**: Check `model_validation_full_results.rds`
2. **Implement Changes**: Remove non-significant variables
3. **Re-validate**: Run validation again after changes
4. **Test Adjustments**: Work through HIGH → MEDIUM → LOW priority
5. **Document Changes**: Update RESULTS.md with findings
6. **Monitor Performance**: Track on held-out 2024 season data

## References

- glmmTMB documentation: https://cran.r-project.org/package=glmmTMB
- Cross-validation best practices: Hastie et al. (2009) - Elements of Statistical Learning
- NFL prediction literature: Baio & Blangiardo (2010) - Bayesian hierarchical model for Premier League
- Calibration methods: Niculescu-Mizil & Caruana (2005) - Predicting good probabilities

## Support

For issues or questions:
1. Check this guide thoroughly
2. Review the validation script comments
3. Consult R package documentation
4. Review NFL prediction literature

---

**Author**: NFL Prediction Model Validation System
**Date**: 2025-11-20
**Version**: 1.0
**R Version Tested**: 4.5.1
