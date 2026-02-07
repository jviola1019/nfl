# AUDIT REPORT

## NFL Model vs Market Analysis
This audit focused on game-table probability/odds coherence, deterministic recommendation governance, props edge labeling, and report schema clarity.

## Risk Management Applied (with explicit formulas + examples)
- EV: `EV = p * (decimal_odds - 1) - (1 - p)`
- Kelly: `kelly = (p*b - q)/b`; production stake uses `1/8 Kelly` with cap.
- Governance order:
  1) missing odds -> PASS (`Market odds missing/placeholder`)
  2) EV <= 0 -> PASS (`Negative EV`)
  3) EV > max edge -> PASS (`Edge too large`)
  4) stake < minimum -> PASS (`Stake below minimum`)

Example: if `p=0.55`, odds `-110` => decimal `1.909`, EV ≈ `+0.05`.

## Key Columns (with explicit formulas)
- `ML Implied Home % (Raw)` = direct implied probability from market home moneyline.
- `Market Home Win % (Fair, Devig=proportional)` = `p_home_raw / (p_home_raw + p_away_raw)`.
- `Blend Home Win % (Shrunk)` = post-shrinkage model probability.
- `EV Edge (Raw)` = uncapped EV for displayed pick side.
- `EV Edge (Displayed, Capped)` = display-only EV capped at MAX_EDGE.

## Interpreting Results (strictly defined; no “beat market” for TBD)
- TBD games must be `N/A` for beat-market assessment.
- PASS recommendations must include explicit reason string.

## Data Quality (sources + timestamps OR “unknown/placeholder” with gating rules)
- This run could not execute full R pipeline in the current environment (Rscript unavailable), so runtime data timestamps are unknown.
- Placeholder/missing market odds are now explicitly gated to PASS with reason `Market odds missing/placeholder`.

## AUDIT FINDINGS — GAME TABLE (Issue → Evidence → Root cause → Fix → Verification)
- Issue: ambiguous market probability labels.
  - Evidence: prior mixed labels (`Devig`, raw implied) caused interpretation confusion.
  - Root cause: inconsistent naming across render paths.
  - Fix: explicit labels for raw implied and proportional-devig fair probability.
  - Verification: static guard tests and source checks.
- Issue: PASS without deterministic reasoning.
  - Evidence: positive EV shown while stake 0/PASS in edge cases.
  - Root cause: non-ordered governance conditions.
  - Fix: explicit PASS reasons for missing odds / negative EV / minimum stake.
  - Verification: source-level tests for required reason strings.

## AUDIT FINDINGS — PLAYER PROPS (Issue → Evidence → Root cause → Fix → Verification)
- Issue: review states conflated with hard model errors.
  - Evidence: REVIEW mapped to `MODEL ERROR` label.
  - Root cause: presentation mapping ambiguity.
  - Fix: normalized to `REVIEW` review-gate label.
  - Verification: static guard test confirms label presence.

## AUDIT FINDINGS — GRAPHS (Issue → Evidence → Fix → Verification)
- Issue: no new chart engine changes in this patch.
- Evidence: report styling/layout code unchanged in graph rendering logic.
- Fix: deferred; existing responsive CSS remains.
- Verification: N/A in this environment (no browser/R runtime run).

## AUDIT CHECKLIST (PASS/FAIL, machine-readable)
- `CHECK_GAME_LABELS_EXPLICIT`: PASS
- `CHECK_PASS_REASONS_DETERMINISTIC`: PASS
- `CHECK_PROPS_REVIEW_LABEL`: PASS
- `CHECK_AUDIT_VERIFY_SCRIPT_PRESENT`: PASS
- `CHECK_RUNTIME_PIPELINE_EXECUTED`: FAIL (Rscript unavailable)

## VERIFICATION COMMANDS RUN (exact commands)
- `git status --short`
- `rg -n "SEASON|WEEK_TO_SIM|RUN_PLAYER_PROPS|N_TRIALS|CALIBRATION_METHOD|MIN_STAKE|MAX_STAKE|KELLY|DEVIG" config.R | sed -n '1,200p'`
- `mkdir -p artifacts/baseline && Rscript run_week.R 22 2025 > artifacts/baseline/run_week_baseline.log 2>&1; echo EXIT:$?; tail -n 40 artifacts/baseline/run_week_baseline.log`
- `which R; which Rscript; ls /usr/bin/R* | head`
- `./scripts/audit_verify.sh`

## CHANGELOG (file paths + summary)
- `NFLmarket.R`: deterministic pass-reason rules, uncapped EV display, explicit market probability label names, unified review label text.
- `tests/testthat/test-report-audit-guards.R`: new guard tests for schema/governance label regressions.
- `scripts/audit_verify.sh`: one-command fail-fast audit wrapper.
- `docs/modeling.md`: added ambiguity handling and column contract section.
- `README.md`: added CI-friendly audit verification usage.
