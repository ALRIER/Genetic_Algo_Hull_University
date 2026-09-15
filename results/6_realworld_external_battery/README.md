# Real-World External Battery Results

This folder contains the summarized results and audit tables from the external public-data validation of the frozen candidate estimators.

## Layout

- `01_results_basic/`: compact result tables used to inspect the main findings.
- `02_reports/`: concise human-readable results report.
- `03_audit_ready/`: provenance, dataset-level decisions, gate decisions, oracle checks, and condition-level metrics.

## Main result files

`01_results_basic/` includes:

- `external_primary_results_compact.csv`
- `external_fdr_survivors_dataset_level.csv`
- `external_dataset_saturation_flags.csv`
- `external_evidence_grid.csv`
- `external_methods_audit_flags.csv`
- `external_survival_map.csv`
- `external_taxonomy_summary.csv`

The main report is `02_reports/REALWORLD_EXTERNAL_BATTERY_RESULTS_REPORT.md`.

Large audit tables in `03_audit_ready/` are split into numbered CSV parts where necessary. The dataset registry and run manifest preserve source and execution provenance.

## Primary result

Dataset-level Benjamini-Hochberg FDR is the primary multiplicity control for eligible, profile-matched comparisons. The completed battery contains 255 rows passing the 5% FDR threshold and 257 rows passing the 10% sensitivity threshold. Depth probes remain within-dataset checks and are not counted as independent public datasets.
