# Post-Discovery Fixed-Weight Validation (`N_EST = 10`)

## Purpose

This folder contains the confirmatory validation layer associated with the original 60-80 discovery experiment. It evaluates fixed GA weight vectors discovered by `GA_Discovery_60_80_CODE` under additional independent simulation seeds and an expanded external benchmark set.

This stage is not a new GA search. It does not mutate, retrain, or re-optimize candidate weights. Its role is to test whether candidates discovered in the original 10-estimator regime-first experiment remain competitive under stricter validation conditions.

## Naming note

This code was originally developed as the `Q1` validation layer. Some internal function names and output CSV filenames retain the `q1_` prefix so older result files can still be matched to the validation code.

## Inputs

- `PROJECT_ROOT`: path to the original 60-80 discovery code containing modules `00` through `10`.
- `DISCOVERY_ROOT`: path to completed discovery results containing `GA_REGIME_FIRST_*` run folders and `REGIME_FIRST_WINNER_SUMMARY__seed*.csv` files.
- `VALIDATION_OUTPUT_ROOT`: destination for validation tables, figures, task summaries, and bootstrap outputs.

## Estimator Basis

- This validation layer belongs to the original discovery experiment.
- It expects the historical estimator registry with `N_EST = 10`.
- Fixed GA candidates are reconstructed from 10-dimensional discovery weight vectors.
- The expanded benchmark set used here is external to the GA candidate and does not alter candidate weights.

## Validation Logic

1. Load the original discovery modules from `PROJECT_ROOT`.
2. Read winner summaries from `DISCOVERY_ROOT`.
3. Select accepted GA wins, borderline cases, seed-sensitive cases, and benchmark-dominant negative controls.
4. Freeze each GA candidate weight vector exactly as discovered.
5. Evaluate each fixed candidate under additional independent validation seeds.
6. Compare each fixed GA candidate against the expanded benchmark set.
7. Evaluate original-regime and locked unseen similar-regime modes when enabled.
8. Compute paired bootstrap confidence intervals for benchmark-relative gains.
9. Export seed-level, regime-level, and stability summaries.

## Validation Modes

- `original_regime`: evaluates the fixed candidate in the same regime structure selected from discovery outputs.
- `locked_unseen_similar`: evaluates the same fixed candidate in a neighboring but previously unseen regime built from adjacent contamination rate, scale, and mechanism values.

No GA search is rerun in either mode.

## Module Map

- `config/fixed_weight_validation_config.R`: validation paths, seeds, Monte Carlo budget, benchmark grid, and validation-mode switches.
- `R/00_validation_helpers.R`: path handling, CSV I/O, weight-vector parsing, module loading, and PSOCK worker helpers.
- `R/01_select_regimes.R`: discovery-output ingestion, candidate labeling, negative-control selection, and validation-set construction.
- `R/02_expanded_benchmarks.R`: additional robust benchmark estimators used only for validation comparisons.
- `R/03_fixed_weight_validation_run.R`: Monte Carlo evaluation loop, task construction, fixed-weight scoring, and seed-level summaries.
- `R/04_bootstrap_ci.R`: paired bootstrap confidence intervals and regime-stability aggregation.
- `R/05_tables_figures.R`: table and figure export helpers.
- `R/06_locked_unseen_regimes.R`: construction of locked unseen similar-regime validation conditions.
- `run_fixed_weight_validation.R`: main R entrypoint.
- `run_fixed_weight_validation.sh`: shell launcher for terminal execution.

## How to Run

```bash
PROJECT_ROOT=/path/to/Genetic_Algo_Hull_University/code/1_discovery_nest10 \
DISCOVERY_ROOT=/path/to/discovery_results \
VALIDATION_OUTPUT_ROOT=/path/to/FIXED_WEIGHT_VALIDATION_OUTPUT \
Rscript run_fixed_weight_validation.R
```

The older `Q1_OUTPUT_ROOT` environment variable is still accepted as a fallback for earlier reproduction scripts.

## Interpretation

A candidate that survives this validation stage provides stronger evidence for local, regime-specific robustness. Failure to survive does not imply that the original discovery stage was invalid; it means the candidate did not retain its advantage under stricter fixed-weight validation. The appropriate claim remains local specialization, not universal dominance.
