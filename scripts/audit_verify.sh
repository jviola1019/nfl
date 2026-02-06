#!/usr/bin/env bash
set -euo pipefail

SCHEMA_INVARIANT_STATUS="FAIL"
ANALYTICAL_CALIBRATION_STATUS="FAIL"
REPORT_STRUCTURE_STATUS="FAIL"

print_summary() {
  local overall="FAIL"
  if [[ "$SCHEMA_INVARIANT_STATUS" == "PASS" && "$ANALYTICAL_CALIBRATION_STATUS" == "PASS" && "$REPORT_STRUCTURE_STATUS" == "PASS" ]]; then
    overall="PASS"
  fi

  printf 'AUDIT_VERIFY_SUMMARY={"schema_invariant":"%s","analytical_calibration":"%s","report_structural":"%s","overall":"%s"}\n' \
    "$SCHEMA_INVARIANT_STATUS" \
    "$ANALYTICAL_CALIBRATION_STATUS" \
    "$REPORT_STRUCTURE_STATUS" \
    "$overall"
}

run_check() {
  local key="$1"
  local description="$2"
  shift 2

  echo "[RUN] $description"
  if "$@"; then
    echo "[PASS] $description"
    case "$key" in
      schema_invariant) SCHEMA_INVARIANT_STATUS="PASS" ;;
      analytical_calibration) ANALYTICAL_CALIBRATION_STATUS="PASS" ;;
      report_structural) REPORT_STRUCTURE_STATUS="PASS" ;;
    esac
  else
    echo "[FAIL] $description"
    print_summary
    exit 1
  fi
}

run_check schema_invariant \
  "Schema + invariant verification" \
  Rscript scripts/verify_repo_integrity.R

run_check analytical_calibration \
  "Analytical calibration tests" \
  Rscript -e "testthat::test_file('tests/testthat/test-calibration.R', stop_on_failure = TRUE, stop_on_warning = FALSE)"

run_check report_structural \
  "Report structural checks" \
  Rscript scripts/audit_verify.R

print_summary
