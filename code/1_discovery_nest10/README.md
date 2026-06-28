# GA Discovery Stage 60-80

## Purpose

This folder contains the original regime-first genetic algorithm discovery code used for the 60-80 discovery experiment.

The goal of this stage is to identify local, regime-specific composite estimators. It is a discovery and candidate-generation pipeline, not a universal-estimator proof. The code searches for GA weight vectors that can outperform the best available benchmark within specific distributional and contamination regimes.

## Experimental Scope

- Families: normal, lognormal, Weibull, inverse Gaussian, ex-Gaussian, and ex-Wald.
- Estimator basis: 10 estimators.
- `N_EST = 10` is defined in `04_estimators_registry.R` from `ESTIMATOR_NAMES`.
- Estimators: mean, median, 20% trimmed mean, harmonic mean, geometric mean, half-sample mode, Parzen mode, trimean, Huber M-estimator, and Tukey biweight M-estimator.
- Primary objective: benchmark-relative improvement in mean-squared error, with strong emphasis on robust q95 performance.
- Main design principle: local specialization under a No Free Lunch framing.

## High-Level Pipeline

1. Load utility, path, scenario, distribution, estimator, GA, data-preparation, and fitness modules.
2. Build distribution-specific parameter grids.
3. Generate contamination and sample-size scenarios.
4. Run HPF1 configuration screening using 60% scenario exposure.
5. Run HPF2 regime and candidate screening using 80% scenario exposure.
6. Select promising regimes for specialist training.
7. Train regime-specialist GA candidates on training scenarios.
8. Evaluate fixed finalists on held-out scenarios without retraining.
9. Apply the benchmark gate against the strongest benchmark comparator.
10. Export per-family and multi-seed result tables.

## Module Map

- `00_utils_debug_io.R`: logging, defensive checks, safe CSV writing, diagnostics, and helper utilities used across the pipeline.
- `01_paths_repro.R`: output paths, run directories, reproducibility settings, and environment-aware project-root handling.
- `02_scenarios_sampling.R`: scenario construction for sample sizes, contamination rates, contamination scales, and contamination mechanisms.
- `03_distributions_params.R`: distribution generators, analytic means, and parameter grids for the six families.
- `04_estimators_registry.R`: canonical 10-estimator registry, estimator metadata, custom composite estimator, and weight-vector helpers.
- `05_ga_core.R`: genetic algorithm primitives, population handling, mutation/crossover logic, early stopping, and optimizer internals.
- `06_data_prep.R`: scenario preparation and shared data objects used by GA evaluation.
- `07_fitness_objectives.R`: MSE-based fitness objectives, benchmark-relative scoring, and robust performance summaries.
- `08_LocationEstimators.R`: main experiment engine; implements regime-first HPF screening, specialist training, held-out gates, audits, and exports.
- `10_distributional_diagnostics.R`: pre-GA diagnostics summarizing distributional shape and simulation behavior.
- `auxiliary_validation/`: optional Monte Carlo simulation-engine validation suite; not part of the active GA discovery launcher.
- `run_experiment.R`: single-entry launcher that loads modules in dependency order and starts the selected run mode.

## Regime-First Architecture

The active architecture is `regime_first_mode = TRUE`. In this mode the code does not search for one global estimator per family. Instead, it searches for candidate estimators tied to specific distributional and contamination regimes.

The regime-first workflow has four main stages:

- HPF1: screens hyperparameter configurations on a 60% scenario subset and archives promising stage elites.
- HPF2: screens candidate configurations and regimes on an 80% scenario subset, using protected elites from HPF1 where applicable.
- Specialist GA: trains local candidates inside selected regimes, with train/held-out splitting and successive-halving style budget allocation.
- Held-out benchmark gate: evaluates fixed finalist weights on held-out scenarios and selects GA winners only when they beat the relevant benchmark gate.

## Scenario and Regime Logic

Regimes are defined by distributional family, shape/tail characteristics, contamination direction, contamination structure, contamination rate, outlier scale, severity, and sample-size support.

Small regimes can be expanded through neighboring conditions when needed for training stability. This is a local support mechanism, not a global generalization claim.

## Specialist Training and Held-Out Gate

The specialist stage receives selected regimes from HPF2 and trains GA candidates only within those local scenario profiles. Finalist candidates are then frozen and evaluated on held-out scenarios. The held-out gate compares the GA candidate against the strongest available benchmark under both mean MSE and q95 MSE criteria.

The final winner is therefore a fixed local composite estimator, not a retrained validation artifact.

## Multi-Seed Design

The code supports multi-seed execution through `run_all_one_shot()`. When multiple seeds are supplied, the launcher creates child run folders, binds key CSV outputs, and writes seed-stability summaries. The publication run used independent discovery seeds to assess whether local wins were stable or seed-sensitive.

## Main Outputs

Per family, the main outputs include:

- `REGIME_FIRST_HPF1_DISCOVERY_SUMMARY__seed*.csv`
- `REGIME_FIRST_HPF1_STAGE_ELITES__seed*.csv`
- `REGIME_FIRST_HPF2_DISCOVERY_SUMMARY__seed*.csv`
- `REGIME_FIRST_HPF2_STAGE_ELITES__seed*.csv`
- `REGIME_FIRST_SELECTED_FOR_SPECIALIST_TRAINING__seed*.csv`
- `REGIME_FIRST_WINNER_SUMMARY__seed*.csv`
- `REGIME_FINALISTS_RANK__seed*__REG*.csv`
- `REGIME_WINNER_SCENARIOS__seed*__REG*.csv`

Across seeds, the main outputs include:

- `MULTISEED_RUN_INDEX.csv`
- `REGIME_FIRST_THESIS_RESULTS_COMPACT__ALL_SEEDS.csv`
- `REGIME_FIRST__ALL_FAMILIES_SUMMARY__ALL_SEEDS.csv`
- `REGIME_FIRST_SEED_STABILITY_SUMMARY.csv`
- `runtime_log__ALL_SEEDS.csv`

## How to Run

From a clean R session or terminal, set the project root to this folder and run the launcher.

```r
setwd("/path/to/GA_Discovery_60_80_CODE")
Sys.setenv(PROJECT_ROOT = getwd())
Sys.setenv(RUN_MODE = "full")
source("run_experiment.R")
```

For a structural preflight that loads the modules without running the full GA:

```r
Sys.setenv(RUN_MODE = "micro")
source("run_experiment.R")
```

## Interpretation

This code should be cited as the original discovery-stage implementation. Its role is to generate and screen local candidate estimators under the original 10-estimator basis. Later validation-stage code should be treated as a separate phase because it expands the estimator basis and validation design.
