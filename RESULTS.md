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
edge for the model relative to the market.

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
market and model logits.【F:NFLsimulation.R†L2889-L3040】

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
