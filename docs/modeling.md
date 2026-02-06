# NFL Prediction Model -- Mathematical Reference

This document is the comprehensive mathematical reference for the NFL prediction model. It covers every formula, distribution, and governance rule used in the pipeline.

---

## Report Rendering Authority

HTML/report rendering is intentionally centralized in `NFLmarket.R` (`moneyline_report()` and `export_moneyline_comparison_html()`). `NFLsimulation.R` is limited to simulation/backtest generation and preparing report inputs; it does not own a separate HTML export path.

---

## 1. Shrinkage

Market shrinkage blends the raw model probability toward the market-implied probability to account for NFL market efficiency.

**Formula:**

```
shrunk_prob = (1 - shrinkage) * model_prob + shrinkage * market_prob
```

**Shrinkage values by context:**

| Context | Shrinkage | Config Key |
|---------|-----------|------------|
| Regular season | 0.70 | `SHRINKAGE` |
| Wild Card / Divisional / Conference | 0.70 | (same) |
| Super Bowl | 0.75 | `SUPER_BOWL_SHRINKAGE` |

**Rationale:** NFL markets are extremely efficient. Raw model disagreements with the market are more likely to reflect overfit or noise than genuine edge. A 70% weight toward the market keeps the model's contribution meaningful while respecting the information already priced in.

---

## 2. Devig Method

The model uses **proportional normalization** (also called the "basic" or "multiplicative" method) to remove the bookmaker's vig from American moneyline odds.

**Step 1 -- Convert American odds to implied probability:**

```
For negative ML:  implied_prob = -ML / (-ML + 100)
For positive ML:  implied_prob = 100 / (ML + 100)
```

**Step 2 -- Normalize to remove vig:**

```
true_prob = implied_prob / sum(all_implied_probs)
```

For a two-way market with implied probabilities `p_home` and `p_away`:

```
true_home = p_home / (p_home + p_away)
true_away = p_away / (p_home + p_away)
```

**Why proportional over Shin or logit methods:** Simpler, well-understood, and the differences between devig methods are less than 1 percentage point in practice for NFL moneylines. The added complexity of Shin or logit devig does not produce meaningful accuracy gains in this context.

**Source:** `R/utils.R:devig_american_odds()`

---

## 3. EV Calculation

Expected Value (EV) measures the average profit per unit staked at the offered odds, given the model's estimated probability.

**Formula:**

```
EV = prob * (decimal_odds - 1) - (1 - prob)
```

**Converting American odds to decimal:**

```
For negative ML:  decimal_odds = 1 + 100 / |ML|
For positive ML:  decimal_odds = 1 + ML / 100
```

**Example:** Model probability = 0.55, offered odds = -110 (decimal 1.909):

```
EV = 0.55 * (1.909 - 1) - (1 - 0.55)
   = 0.55 * 0.909 - 0.45
   = 0.500 - 0.450
   = +0.050  (i.e., +5.0% EV)
```

**Important:** The model always uses the **shrunk probability** (not the raw simulation probability) for all betting EV calculations.

**Source:** `R/utils.R:expected_value_units()`

---

## 4. Kelly Criterion

The Kelly criterion determines optimal bet sizing to maximize long-term bankroll growth.

**Full Kelly formula:**

```
kelly = (p * b - q) / b
```

Where:
- `p` = estimated probability of winning (shrunk probability)
- `q` = 1 - p (probability of losing)
- `b` = decimal_odds - 1 (net payout per unit)

**Fractional Kelly:**

The model applies **1/8 Kelly** to reduce volatility:

```
stake = kelly * KELLY_FRACTION
```

Where `KELLY_FRACTION = 0.125`.

**Edge skepticism penalty:**

Large perceived edges are penalized because they are more likely to reflect model error than genuine mispricing:

| Perceived Edge | Penalty Multiplier |
|---------------|--------------------|
| Edge <= 10% | 1.0 (no penalty) |
| 10% < Edge <= 20% | 0.5 |
| 20% < Edge <= 30% | 0.25 |
| Edge > 30% | 0.1 |

The final stake is:

```
final_stake = kelly * KELLY_FRACTION * edge_penalty
```

**Stake bounds:**

| Parameter | Value | Meaning |
|-----------|-------|---------|
| `MAX_STAKE` | 0.02 | Maximum 2% of bankroll on any single bet |
| `MIN_STAKE_THRESHOLD` | 0.01 | Minimum 1% of bankroll; below this the bet is a PASS |

**Source:** `R/utils.R:conservative_kelly_stake()`

---

## 5. Calibration

Calibration maps raw simulation probabilities to better-calibrated probabilities using historical data.

**Primary method:** Spline calibration via a GAM (Generalized Additive Model) with a smoothing penalty.

**Fallback method:** Isotonic regression via `isoreg()`, used when the spline model is unavailable or fails to fit.

**Performance:** Spline calibration achieves a **-6.9% improvement** in Brier score compared to uncalibrated probabilities.

**Application rules:**
- Calibration is applied **exactly once** to raw simulation probabilities.
- Output probabilities are bounded to **[0.01, 0.99]** to prevent degenerate log-loss values.
- When calibration is unavailable, the model compensates by adjusting shrinkage upward.

**Source:** `NFLsimulation.R:5596-5670`

---

## 6. Player Props Model

### 6.1 Gaussian Copula

Player prop simulations are correlated with game-level simulation outcomes using a Gaussian copula. This ensures that, for example, a QB's passing yards are higher in simulations where the game total is high.

**Correlation targets** (empirically validated against nflreadr 2019-2024 data):

| Relationship | Correlation (r) | 95% CI |
|-------------|-----------------|--------|
| QB passing <-> game total | 0.75 | [0.72, 0.78] |
| RB rushing <-> game total | 0.60 | [0.55, 0.65] |
| WR receiving <-> team passing | 0.50 | [0.45, 0.55] |
| TD probability <-> game total | 0.40 | [0.35, 0.45] |
| Same-team cannibalization | -0.15 | [-0.20, -0.10] |

**Mechanism:** For each simulation trial, a reference z-score from the game simulation is used to generate a correlated z-score for the player prop via:

```
z_player = rho * z_game + sqrt(1 - rho^2) * z_independent
```

Where `z_independent ~ N(0, 1)` is an independent standard normal draw.

**Source:** `R/correlated_props.R:generate_correlated_variates()`

### 6.2 Yard Props (Truncated Normal)

Yard props (passing, rushing, receiving) are modeled with a **truncated normal distribution**, truncated at zero to prevent negative yardage.

**Distribution:**

```
yards ~ Normal(baseline, sd), truncated at 0
```

Where:
- `baseline` = player's projected yards for the game (adjusted for opponent defense and game context)
- `sd = baseline * CV`

**Coefficient of Variation (CV) values:**

| Position / Stat | CV |
|----------------|----|
| QB passing yards | 0.29 |
| RB rushing yards | 0.40 |
| WR/TE receiving yards | 0.35 |

**Over/Under probability:**

```
P(Over line) = mean(simulated_yards > market_line)
```

Computed as the proportion of simulation trials exceeding the market line.

### 6.3 Anytime TD Props (Negative Binomial)

Anytime touchdown props model the count of scoring touchdowns a player records in a game.

**Distribution:**

```
scoring_tds ~ NegBin(size, mu = baseline)
```

Where:
- `baseline` = player's average scoring TDs per game
- `scoring_tds` = rushing TDs + receiving TDs (passing TDs are **excluded**)
- `size` = overdispersion parameter

**Overdispersion:** `TD_OVERDISPERSION = 1.5`

**Anytime TD probability:**

```
P(anytime TD) = mean(simulated_count >= 1)
              = 1 - P(count = 0 | NegBin)
```

**Display note:** The props table shows P(anytime TD), **not** the expected TD count.

### 6.4 EV for Props

Player prop EV uses the same formula as game-level EV:

```
EV = P(side) * (decimal_odds - 1) - (1 - P(side))
```

Where `P(side)` is the model probability for the relevant side (over or under for yards; anytime or no-TD for touchdowns).

**Minimum edge for recommendation:** 2%. Below this threshold, no BET/OVER/UNDER recommendation is made.

**Review threshold:** If `|EV| > 20%`, the prop is flagged for manual review to check for data issues.

---

## 7. Governance Rules

### Game Table

| Condition | Action | Reason |
|-----------|--------|--------|
| EV > 15% | Auto-PASS ("Edge too large") | Likely reflects stale or incorrect odds |
| Kelly stake < 1% bankroll | Auto-PASS ("Stake below minimum") | Not worth the transaction cost |

**Edge bins for display:**

| Edge Range | Label |
|-----------|-------|
| 0-5% | OK |
| 5-10% | High |
| >10% | Capped |

### Player Props

**Yard props EV tiers:**

| |EV| Range | Label |
|-----------|-------|
| <= 5% | OK |
| <= 10% | Caution |
| <= 20% | High |
| > 20% | Review |

**TD props EV tiers** (wider bands due to long odds and higher variance):

| |EV| Range | Label |
|-----------|-------|
| <= 10% | OK |
| <= 25% | Caution |
| <= 50% | High |
| > 50% | Review |

**Recommendation thresholds:**
- Minimum 2% edge required for any BET/OVER/UNDER recommendation.
- Props below the 2% threshold receive no directional recommendation.

---

## 8. Known Limitations

1. **Shrinkage hides the raw model view.** The displayed probability is the shrunk (blended) probability, not the direct simulation output. Users cannot see the model's "pure" opinion without consulting the raw simulation logs.

2. **Default -110 prop odds used when `ODDS_API_KEY` is not set.** This is the industry convention for standard juice but does not reflect actual lines at any specific sportsbook.

3. **TD model uses average scoring TDs as the Negative Binomial mean.** For low-volume players (tight ends, QB rushers), this may underestimate variance because the sample size for their TD rates is small and unstable.

4. **Calibration is trained on 2022-2024 data.** The calibration function may not generalize well to seasons with significant rule changes, scoring environment shifts, or other structural breaks in NFL gameplay.

5. **Margin coherence is enforced post-hoc.** The function `harmonize_home_margin` reconciles cases where the raw simulation margin and the shrunk win probability disagree. In rare edge cases, these two quantities legitimately diverge, and the post-hoc adjustment may mask that signal.


## 10. Ambiguity Handling and Column Contracts

To prevent report ambiguity, the game table now distinguishes between raw implied and vig-free market probabilities:

- `ML Implied Home % (Raw)` = implied probability directly from market home moneyline.
- `Market Home Win % (Fair, Devig=proportional)` = vig-free probability from proportional devig.

Recommendation governance is deterministic and ordered:

1. Missing market odds -> `PASS` with reason `Market odds missing/placeholder`
2. Non-positive EV -> `PASS` with reason `Negative EV`
3. Positive EV but stake below threshold -> `PASS` with reason `Stake below minimum`
4. Otherwise -> bet recommendation

For props, entries above governance bounds are labeled `MODEL ERROR / REVIEW` (review gate), not a runtime/model crash indicator by itself.

