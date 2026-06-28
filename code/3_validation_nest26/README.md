# Validation-Stage GA Experiment (`N_EST = 26`)

## Purpose

This folder contains the second-stage GA experiment used to stress-test and extend the original 60-80 discovery design.

The stage keeps the regime-first specialist logic but expands the estimator basis to 26 estimators, increases HPF2 scenario exposure to 90%, and allows prior winners/regimes from the earlier tournament to enter as protected or priority candidates. Inherited candidates do not receive automatic wins; they must compete under the same specialist, held-out, and fixed-weight validation gates as newly discovered candidates.

## Experimental Scope

- Families: normal, lognormal, Weibull, inverse Gaussian, ex-Gaussian, and ex-Wald.
- Estimator basis: 26 estimators defined by `ESTIMATOR_NAMES` in `04_estimators_registry.R`.
- HPF1 scenario exposure: 60%.
- HPF2 scenario exposure: 90%.
- Specialist final budget: full final-stage budget after screening/halving.
- Inheritance: internal stage elites and configured external prior winners/regimes can enter later stages as competitors.

## High-Level Pipeline

1. Load shared utilities, scenario grids, distribution generators, estimator registry, GA core, data preparation, and fitness objectives.
2. Build full and light scenario universes.
3. Run HPF1 screening with internal elite archiving.
4. Inject eligible prior winners/regimes and internal stage elites into the appropriate candidate pools.
5. Run HPF2 with 90% scenario exposure and the expanded 26-estimator basis.
6. Select promising regimes for specialist training.
7. Run specialist halving and final fixed-weight held-out evaluation.
8. Apply the benchmark gate to select local winners.
9. Export inheritance, audit, winner, and multi-seed summaries.

## Module Map

- `00_utils_debug_io.R`: logging, defensive checks, safe writing, and runtime diagnostics.
- `01_paths_repro.R`: reproducible paths, run directory setup, and seed helpers.
- `02_scenarios_sampling.R`: contamination/sample-size scenario construction.
- `03_distributions_params.R`: distribution generators, analytic means, and parameter grids.
- `04_estimators_registry.R`: canonical 26-estimator registry and fixed-length weight helpers.
- `05_ga_core.R`: GA population, mutation, crossover, parallelism, and optimizer internals.
- `06_data_prep.R`: contamination injection and estimator component preparation.
- `07_fitness_objectives.R`: benchmark-relative MSE/q95 objectives and diagnostics.
- `08_LocationEstimators.R`: main regime-first engine with inheritance, HPF1/HPF2, specialist training, held-out gates, and exports.
- `10_distributional_diagnostics.R`: pre-GA distributional diagnostics.
- `run_experiment.R`: single-entry launcher.

## How to Run

```r
setwd("/path/to/GA_Validation_Extended_90HPF2_Inheritance_NEST26_CODE")
Sys.setenv(PROJECT_ROOT = getwd())
Sys.setenv(RUN_MODE = "full")
source("run_experiment.R")
```

For a structural preflight without running the GA:

```r
Sys.setenv(RUN_MODE = "micro")
source("run_experiment.R")
```

## Interpretation

This stage complements the original discovery experiment. It tests whether local specialist logic remains productive under a broader estimator basis, stronger HPF2 exposure, inherited priors, and stricter benchmark pressure. The scientific claim remains local and regime-specific rather than universal dominance.
