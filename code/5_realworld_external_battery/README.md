# Real-World External Battery

## Purpose

This module evaluates frozen candidate estimators on public empirical datasets after the expanded fixed-weight validation and evidence-taxonomy stages. Candidates are loaded from the N_EST = 26 evidence taxonomy and are evaluated without retraining or changing their weights.

## Candidate source

```text
results/5_evidence_taxonomy_nest26/tables/evidence_taxonomy_all_candidates.csv
```

The complete taxonomy is retained during evaluation so that validated candidates, near-gate candidates, and controls can be compared under the same external-data procedure.

## Output layout

```text
01_results_basic/       compact result tables
02_reports/             generated result summaries
03_audit_ready/         provenance, decisions, oracle checks, and split CSVs
98_discarded_nonfinal/  temporary or superseded run artifacts
99_recovery_logs/       checkpoints, downloads, and recovery logs
```

The committed result layer contains the compact final outputs and selected audit files needed to inspect the completed run. Large checkpoints and downloaded datasets are stored outside the repository.

## Main files

- `13_realworld_external_battery.R`: data catalog, evaluation loop, evidence labels, checkpointing, and exports.
- `realworld_external_battery_utils.R`: candidate loading and empirical evaluation helpers.
- `generate_external_report.R`: concise base-R PDF report generator.
- `run_realworld_external_battery.R`: R entry point.
- `run_realworld_external_battery.sh`: shell entry point with reproducible defaults.

The folder also contains the validation-stage modules required by the external battery so the stage can run independently.

## Run

From the repository root:

```bash
code/5_realworld_external_battery/run_realworld_external_battery.sh
```

The launcher resolves the repository root automatically and writes the completed run under `results/6_realworld_external_battery/` unless the output variables are overridden.

## Interpretation

Primary inference is based on eligible, profile-matched dataset-level decisions after paired confidence intervals and Benjamini-Hochberg FDR correction. Gate-row decisions, depth probes, and oracle comparisons are retained as secondary audit layers.

The external battery evaluates regime-conditional transfer. It identifies where fixed specialist estimators retain benchmark-relative advantages on public data and where the benchmark remains preferred.
