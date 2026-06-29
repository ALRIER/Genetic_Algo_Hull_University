# Code Map

This file summarizes which modules are shared across stages and which modules are stage-specific. The purpose is to make the folder structure easier to read without requiring a manual diff of every repeated filename.

## GA Stages

The discovery stage (`code/1_discovery_nest10`) and the expanded validation-stage GA (`code/3_validation_nest26`) use the same module order but differ in several important places.

| Module | Relationship | Reason |
| --- | --- | --- |
| `00_utils_debug_io.R` | stage-specific | output helpers and metadata evolved between stages |
| `01_paths_repro.R` | stage-specific | run folders and reproducibility settings differ |
| `02_scenarios_sampling.R` | shared | same scenario-grid construction |
| `03_distributions_params.R` | shared | same distribution generators and parameter grids |
| `04_estimators_registry.R` | stage-specific | `N_EST = 10` in Phase 1, `N_EST = 26` in Phase 2 |
| `05_ga_core.R` | stage-specific | expanded basis, inheritance, and validation-stage controls |
| `06_data_prep.R` | stage-specific | estimator component preparation depends on the active basis |
| `07_fitness_objectives.R` | stage-specific | benchmark and reporting logic differ by stage |
| `08_LocationEstimators.R` | stage-specific | main launcher differs by discovery/validation design |
| `10_distributional_diagnostics.R` | shared | same pre-GA diagnostics |
| `run_experiment.R` | stage-specific | each stage loads its own module set |

## Fixed-Weight Validation Stages

The fixed-weight validation folders also keep separate copies because they reconstruct candidates from different estimator bases:

- `code/2_fixed_weight_validation_nest10`: validates 10-dimensional candidates from Phase 1.
- `code/4_fixed_weight_validation_nest26`: validates 26-dimensional candidates from Phase 2.

Both stages freeze discovered weights and evaluate them under additional seeds, expanded benchmarks, and locked unseen similar-regime checks. They should not be merged because the expected weight-vector length and upstream discovery outputs differ.

## External and Dirichlet Audits

- `code/5_realworld_external_battery` evaluates frozen specialists on public datasets using the evidence taxonomy.
- `code/6_random_dirichlet_abstain_audit` tests benchmark-retained regimes by sampling random Dirichlet composites over the NEST26 estimator basis.

These stages depend on the earlier validation/taxonomy outputs but answer distinct audit questions, so they remain separate folders.
