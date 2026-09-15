# Post-Discovery Fixed-Weight Validation (`N_EST = 10`)

## Purpose

This folder contains the fixed-weight validation stage for the original Phase 1 discovery experiment. It evaluates previously discovered GA weight vectors under independent simulation seeds, expanded benchmarks, and locked-unseen related regimes.

Candidate weights remain frozen throughout this stage: no GA search, mutation, retraining, or re-optimization is performed.

## Inputs

- `PROJECT_ROOT`: `code/1_discovery_nest10/`, containing the Phase 1 simulation and estimator modules.
- `DISCOVERY_ROOT`: completed Phase 1 discovery results.
- `VALIDATION_OUTPUT_ROOT`: destination for validation tables, figures, audit files, and run metadata.

## Validation logic

1. Load the Phase 1 discovery modules.
2. Read the completed discovery results.
3. Select accepted candidates, borderline cases, seed-sensitive cases, and negative controls.
4. Freeze each selected weight vector.
5. Evaluate candidates over independent validation seeds.
6. Compare each candidate with the expanded benchmark set.
7. Evaluate original-regime and locked-unseen-similar conditions.
8. Compute paired bootstrap confidence intervals and stability summaries.

## Modules

- `config/fixed_weight_validation_config.R`: paths, seeds, Monte Carlo budget, benchmarks, and validation settings.
- `R/00_validation_helpers.R`: path handling, I/O, weight parsing, module loading, and worker helpers.
- `R/01_select_regimes.R`: candidate selection from discovery outputs.
- `R/02_expanded_benchmarks.R`: additional benchmark estimators used during validation.
- `R/03_fixed_weight_validation_run.R`: Monte Carlo validation and seed-level summaries.
- `R/04_bootstrap_ci.R`: paired bootstrap confidence intervals and stability aggregation.
- `R/05_tables_figures.R`: result-table and figure exports.
- `R/06_locked_unseen_regimes.R`: construction of related regimes not used during discovery.
- `run_fixed_weight_validation.R`: main R entry point.
- `run_fixed_weight_validation.sh`: shell launcher.

## Run

```bash
PROJECT_ROOT=/path/to/Genetic_Algo_Hull_University/code/1_discovery_nest10 \
DISCOVERY_ROOT=/path/to/Genetic_Algo_Hull_University/results/1_discovery_nest10 \
VALIDATION_OUTPUT_ROOT=/path/to/validation_output \
Rscript run_fixed_weight_validation.R
```

A candidate that survives this stage provides evidence of local robustness under fixed-weight validation. Candidates that do not survive remain discovery results and are not treated as validated specialists.
