#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

echo "AUDIT_VERIFY_START"

# Static invariants that should always hold in source
rg -n "Market Home Win % \(Fair, Devig=proportional\)" NFLmarket.R >/dev/null
rg -n "ML Implied Home % \(Raw\)" NFLmarket.R >/dev/null
rg -n "EV Edge \(Raw\)" NFLmarket.R >/dev/null
rg -n "EV Edge \(Displayed, Capped\)" NFLmarket.R >/dev/null
rg -n "Min Stake \(%\)" NFLmarket.R >/dev/null
rg -n "Market odds missing/placeholder|Negative EV|Stake below minimum" NFLmarket.R >/dev/null
rg -n "MODEL ERROR / REVIEW" NFLmarket.R >/dev/null

if command -v Rscript >/dev/null 2>&1; then
  Rscript -e "testthat::test_dir('tests/testthat', reporter='summary')"
  Rscript scripts/audit_verify.R
  echo "AUDIT_VERIFY_RESULT=PASS"
else
  echo "AUDIT_VERIFY_RESULT=WARN"
  echo "WARN: Rscript not available; skipped executable R tests"
fi
