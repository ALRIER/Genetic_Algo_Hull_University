# Manifest

Research code and summarized results for the genetic-algorithm location-estimator experiments (University of Hull).

## Root

- `code/`: experiment code organized by stage.
- `results/`: summarized results, figures, evidence tables, and audit metadata.
- `results/RESULTS_SELECTION_INDEX.csv`: concise index of key committed result artifacts.
- `Publication_Appendices/`: extended publication material.

## Estimator dimensions

- `code/1_discovery_nest10/04_estimators_registry.R`: `N_EST = 10`
- `code/3_validation_nest26/04_estimators_registry.R`: `N_EST = 26`

## Code stages

- `code/1_discovery_nest10/`: Phase 1 discovery-stage GA using the 10-estimator basis.
- `code/2_fixed_weight_validation_nest10/`: fixed-weight validation of Phase 1 candidates.
- `code/3_validation_nest26/`: Phase 2 expanded GA using the 26-estimator basis and inherited candidates.
- `code/4_fixed_weight_validation_nest26/`: fixed-weight validation of Phase 2 candidates.
- `code/5_realworld_external_battery/`: external public-data validation of frozen candidates.
- `code/6_random_dirichlet_abstain_audit/`: random-Dirichlet audit of benchmark-retained regimes.

## Result stages

- `results/1_discovery_nest10/`
- `results/2_fixed_weight_validation_nest10/`
- `results/3_validation_nest26/`
- `results/4_fixed_weight_validation_nest26/`
- `results/5_evidence_taxonomy_nest26/`
- `results/6_realworld_external_battery/`
- `results/7_random_dirichlet_abstain_audit/`

Large checkpoints, per-task outputs, recovery logs, and raw downloaded datasets are not committed. The repository keeps the code and the result layer needed to inspect the main findings and their audit trail.
