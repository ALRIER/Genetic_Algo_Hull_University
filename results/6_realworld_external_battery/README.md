# Real-World External Battery Results

This folder contains the curated GitHub result package for the taxonomy-guided
real-world external battery V5.

The full local run produced recovery logs, raw downloads, and checkpoints. Those
files are intentionally excluded from the GitHub package and should remain in the
full archival deposit. The files kept here include the compact result tables, the
dataset registry, the generated reports, and the split condition-level audit
table needed to inspect the external validation stage.

## Included Layout

```text
01_results_basic/
02_reports/
03_audit_ready/
```

## Included Files

`01_results_basic/` contains the compact final result tables:

- `external_primary_results_compact.csv`
- `external_fdr_survivors_dataset_level.csv`
- `external_dataset_saturation_flags.csv`
- `external_evidence_grid.csv`
- `external_methods_audit_flags.csv`
- `external_survival_map.csv`
- `external_taxonomy_summary.csv`

`02_reports/` contains the generated report files and the curated interpretation
summary:

- `SUMMARY_EXTERNAL_BATTERY.md`
- `external_realworld_results_report_V5.md`
- `external_realworld_results_report_V5.pdf`
- `REALWORLD_EXTERNAL_BATTERY_RESULTS_REPORT.md`

`03_audit_ready/` contains provenance and decision-level audit tables:

- `RUN_MANIFEST.csv`
- `external_dataset_registry.csv`
- `external_dataset_level_decisions.csv`
- `external_gate_decisions.csv` and split part files
- `external_oracle_audit.csv` and split part files
- `external_condition_metrics.csv` and split part files

## Excluded From GitHub

The following outputs are deliberately not committed here:

- `99_recovery_logs/`
- checkpoint files
- raw downloaded data files
- recovery copies
- logs

Those files are suitable for the full data archive, not the compact GitHub
repository.

## Primary Result

The strict primary interpretation uses dataset-level Benjamini-Hochberg FDR on
eligible, in-regime, pre-registered benchmark comparisons. The final compact
report records 255 rows passing the 5% FDR threshold and 257 rows passing the
10% sensitivity threshold. Depth probes are secondary within-dataset mechanism
checks and should not be counted as independent public datasets.
