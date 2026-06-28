# Post-Discovery Fixed-Weight Validation (`N_EST = 26`)

## Purpose

This folder contains the confirmatory validation layer for the validation-stage GA experiment. It evaluates fixed 26-dimensional GA weight vectors discovered by the expanded validation-stage search under additional independent seeds, expanded benchmarks, and locked unseen similar-regime checks.

This stage does not rerun GA search, mutate weights, or retrain candidates. It validates frozen candidates from the completed validation-stage discovery outputs.

## Estimator Basis

- Uses the expanded estimator registry from the validation-stage GA code.
- Expects `N_EST = 26` and reconstructs fixed GA candidates from 26-dimensional weight vectors.
- Benchmarks are external comparators and do not change candidate weights.

## Validation Logic

1. Load the validation-stage GA modules from `PROJECT_ROOT`.
2. Read completed discovery winner summaries from `DISCOVERY_ROOT`.
3. Select accepted GA wins, borderline cases, seed-sensitive cases, and negative controls.
4. Freeze each candidate weight vector exactly as discovered.
5. Evaluate fixed candidates over independent validation seeds.
6. Compare fixed GA candidates against the expanded benchmark set.
7. Evaluate original-regime and locked unseen similar-regime modes when enabled.
8. Compute paired bootstrap confidence intervals and stability summaries.

## Module Map

- `config/fixed_weight_validation_config.R`: paths, validation seeds, Monte Carlo budget, benchmark grid, and validation switches.
- `R/00_validation_helpers.R`: path handling, I/O, module loading, dynamic `N_EST` weight parsing, and worker helpers.
- `R/01_select_regimes.R`: discovery-output ingestion and validation-set construction.
- `R/02_expanded_benchmarks.R`: robust benchmark estimators used for validation comparison.
- `R/03_fixed_weight_validation_run.R`: Monte Carlo validation loop and seed-level summaries.
- `R/04_bootstrap_ci.R`: paired bootstrap confidence intervals and stability summaries.
- `R/05_tables_figures.R`: table, audit, manifest, and figure exports.
- `R/06_locked_unseen_regimes.R`: locked unseen similar-regime construction.
- `run_fixed_weight_validation.R`: main R entrypoint.

## Interpretation

This layer validates local specialist candidates from the expanded 26-estimator experiment. It supports regime-specific evidence under a No Free Lunch framing, not a universal-dominance claim.
