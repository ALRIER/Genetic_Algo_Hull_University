#!/usr/bin/env bash
set -euo pipefail

: "${PROJECT_ROOT:?Set PROJECT_ROOT to the folder containing the original R modules.}"
: "${DISCOVERY_ROOT:?Set DISCOVERY_ROOT to the folder containing the final GA_REGIME_FIRST_* outputs.}"
: "${VALIDATION_OUTPUT_ROOT:=${Q1_OUTPUT_ROOT:-$PWD/FIXED_WEIGHT_VALIDATION_OUTPUT}}"

export PROJECT_ROOT
export DISCOVERY_ROOT
export VALIDATION_OUTPUT_ROOT

Rscript "$(dirname "$0")/run_fixed_weight_validation.R"
