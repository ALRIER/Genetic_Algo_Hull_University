# Experiment 1 Final Results Folder

This directory is organized as a procedural record of Experiment 1 and its later validation extensions.

## How to Read This Folder

Start with the numbered folders in order. The numbering follows the experiment workflow:

1. `01_DISCOVERY_MONTE_CARLO_60_80`
   Initial Monte Carlo discovery stage and distribution diagnostics.
2. `02_ORIGINAL_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST10`
   Original post-discovery fixed-weight validation layer.
3. `03_EXPANDED_DISCOVERY_NEST26`
   Expanded discovery stage with the larger estimator set.
4. `04_EXPANDED_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST26`
   Expanded fixed-weight validation run with raw tasks and checkpoints.
5. `05_EVIDENCE_TAXONOMY_NEST26`
   Evidence taxonomy summarizing which candidates passed, failed, or remained near-gate.
6. `06_REALWORLD_EXTERNAL_BATTERY_40DATASETS`
   Final real-world external battery over 40 public/market datasets.
7. `07_CODE_ARCHIVE`
   Code snapshots and scripts used to produce the stages above.
8. `08_RANDOM_DIRICHLET_ABSTAIN_AUDIT`
   Post-taxonomy random Dirichlet audit of the NEST26 benchmark-retained regimes.

Supporting material:

- `00_README_AND_GUIDES`: briefs, method guides, and high-level documentation.
- `90_ARCHIVED_ZIPS_AND_LEGACY`: compressed copies and older naming artifacts retained for traceability.

## Practical Use

For reporting, start with:

- `01_DISCOVERY_MONTE_CARLO_60_80/2026_May_25_Finals`
- `02_ORIGINAL_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST10/curated_results`
- `05_EVIDENCE_TAXONOMY_NEST26/evidence_results_20260611`
- `06_REALWORLD_EXTERNAL_BATTERY_40DATASETS/realworld_battery_40datasets_full_run/audit_report_package`
- `08_RANDOM_DIRICHLET_ABSTAIN_AUDIT/tables`

For full audit reconstruction, use the raw folders:

- `02_ORIGINAL_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST10/raw_full_run_output_with_tasks_checkpoints`
- `04_EXPANDED_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST26/raw_full_run_output_with_tasks_checkpoints_20260611`
- `06_REALWORLD_EXTERNAL_BATTERY_40DATASETS/estimator_results_raw`
- `08_RANDOM_DIRICHLET_ABSTAIN_AUDIT/logs`

## Stage 08 Summary

The random Dirichlet abstention audit re-evaluates the 17 NEST26 `benchmark_retained` regimes from the final evidence taxonomy. It samples 4000 random Dirichlet composites over the same 26-estimator basis and tests them against the original dual gate in both `original_regime` and `locked_unseen_similar` validation modes.

Headline result: 23 of 34 regime-mode rows confirmed the abstention, while 11 showed at least one seed-stable random Dirichlet pass. At the unique-regime level, 9 of 17 abstentions were fully confirmed and 8 showed some Dirichlet signal. The transfer-specialist sanity check passed for Q1R011 and Q1R012 at 8 of 8 seeds.

## Size Notes

Most of the storage is raw task/checkpoint output, not final reporting tables. The curated reporting package for the real-world battery is much smaller and is isolated under `audit_report_package`.

## Naming Notes

Some internal code and archived outputs retain legacy function/file prefixes for reproducibility. Folder names at the top level use methodological names so the workflow is readable without knowing the older internal labels.
