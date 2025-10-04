# Market Comparison Results

This summary explains the diagnostic tables emitted by `compare_to_market()` in
`NFLbrier_logloss.R` when it is passed the latest backtest output.  The
reported deltas compare the model probabilities against the vig-adjusted market
probabilities for the same historical games.

## Overall scoring rules

The overall table lists the average Brier score and log loss for both the model
and the market along with their differences and the number of games scored.
Positive deltas (`model - market`) indicate the model is *worse* than the
market on that metric, while negative deltas indicate the model is beating the
market.  In the captured run, the model Brier score was 0.210 versus the
market's 0.208, and the log loss was 0.608 versus 0.604, so the market edged the
model on both scoring rules.

The paired week-block bootstrap that precedes the tables provides uncertainty
bands around those deltas.  Because the 95% confidence intervals for both Brier
and log-loss deltas cross zero, the run does not show a statistically significant
edge for the model relative to the market.  `compare_to_market()` now returns the
raw per-game paired deltas, their t-based confidence intervals, p-values, effect
sizes, and the approximate number of games that would be required to exclude
zero at the chosen confidence level.  When an interval still spans zero, inspect
`cmp$paired_stats` to see the magnitude of the mean edge relative to its
variance and whether acquiring more data or reducing predictive noise would help.

## Season-by-season detail

The per-season table recomputes the same scoring rules within each season to
highlight when the model approached or beat the market.  Only the 2020 season
shows a small Brier-score advantage, while later years—including the partial
2024 dataset—remain above or roughly tied with the market.  This indicates the
current blend has not consistently outperformed closing prices across seasons,
so it is unlikely to be the most effective configuration for projecting 2025
without further refinement.

## Market reliability bins

The reliability table bins the market's two-way probabilities into 10% buckets
and compares the mean implied probability (`p_hat`) to the actual win rate
(`y_bar`).  Deviations highlight market calibration issues the blend could
exploit.  The current run shows mild underestimation in the 10–20% bucket and
overestimation in the 80–100% buckets, suggesting the market slightly overrated
large favorites and underrated some deeper underdogs during the sample.

## Coverage of game factors

`NFLsimulation.R` currently models a targeted set of contextual effects when
building its blended probabilities: strength of schedule and recency weights,
rest and bye adjustments, altitude, and weather/venue toggles for dome, wind,
cold, and precipitation impacts.【F:NFLsimulation.R†L84-L113】  It also ingests
season-level offensive and defensive EPA and success-rate metrics from
`nflreadr` and feeds those edges into the blend design matrix alongside the
market and model logits.【F:NFLsimulation.R†L2889-L3040】  These variables are the
only statistically driven features currently available for comparison against
peer models; adding structured injury reports, officiating crews, or advanced
player participation data would create additional significant signals once they
are engineered and verified.

`compare_to_market()` can accept a `peers` data frame with alternative model
probabilities to benchmark the primary blend.  The returned `cmp$peers` table
lists the paired improvements in Brier and log-loss versus both the market and
the base model, along with p-values and standardized effect sizes so you can
identify configurations that are both more accurate and statistically
significant.

Important factors such as real-time injuries, depth chart changes, officiating
crews, detailed weather forecasts, travel logistics, or player-level matchup
metrics are not ingested anywhere in the current codebase, so they cannot be
considered covered.  Incorporating additional nflreadr endpoints (e.g., weekly
player participation, injuries) or external feeds would be necessary to capture
those influences before the 2025 season.

## Recommendations

1. Continue to expand the contextual feature set—especially player availability
   and opponent matchup details—and re-run the bootstrap comparison to see if
   the blend meaningfully beats the market with the new information.
2. Investigate recalibration strategies (e.g., more flexible isotonic or beta
   calibration) and alternative meta-models to close the observed scoring gap.
3. Track the bootstrap deltas over rolling windows so you can detect when new
   features consistently deliver a statistically significant advantage.

## Where to tune the current model

`NFLsimulation.R` now ships with a `show_tuning_help()` helper that lists the
highest-impact levers, the recommended ranges to test, and the metrics each knob
typically influences.  Call the function without arguments to review the full
table or supply a metric name—`show_tuning_help("ret_total")`, for example—to
filter the guidance to the profit-focused adjustments.【F:NFLsimulation.R†L108-L154】

When iterating toward a 55–57% win rate with a positive `ret_total`, start by
exploring combinations of the GLMM blend weight, strength-of-schedule factor,
and recency halflife: these determine how much the projection leans on priors
versus recent form and will show up quickly in both the Brier/log-loss deltas
and the realized hit rate.  Rebalance the rest/bye bonuses and injury scalars
after the core blend is stable; they primarily move the tail games that can
shift `ret_total` without derailing overall calibration.  Each sweep should be
validated by re-running the backtest trio (`NFLsimulation.R`, `NFLvsmarket.R`,
and `NFLbrier_logloss.R`) so the scoring and bankroll diagnostics stay aligned.
