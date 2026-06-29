# Manifest

Research code and summarized results for the genetic-algorithm location-estimator experiments (University of Hull).

## Root

- `code/`: experiment code, organized by stage.
- `results/`: summary result files, figures, evidence-taxonomy tables, and audit metadata.

## Estimator dimensions

- `code/1_discovery_nest10/04_estimators_registry.R`: `N_EST = 10`
- `code/3_validation_nest26/04_estimators_registry.R`: `N_EST = 26`

## Code stages

- `code/1_discovery_nest10/`: Phase 1 discovery-stage GA (10-estimator basis, 60–80 exposure).
- `code/2_fixed_weight_validation_nest10/`: fixed-weight validation of Phase 1 candidates.
- `code/3_validation_nest26/`: Phase 2 expanded GA with inheritance (26-estimator basis, HPF2 90%).
- `code/4_fixed_weight_validation_nest26/`: fixed-weight validation of Phase 2 candidates.
- `code/5_realworld_external_battery/`: external public-data battery for frozen specialist estimators.
- `code/6_random_dirichlet_abstain_audit/`: random-Dirichlet stress test of the regime map.

## Results

- `results/1_discovery_nest10/`
- `results/2_fixed_weight_validation_nest10/`
- `results/3_validation_nest26/`
- `results/4_fixed_weight_validation_nest26/`
- `results/5_evidence_taxonomy_nest26/`
- `results/6_realworld_external_battery/`
- `results/7_random_dirichlet_abstain_audit/`

Large checkpoints, per-task outputs, logs, raw downloads, and RDS files are not committed here; see *Data Availability* in `README.md`.
