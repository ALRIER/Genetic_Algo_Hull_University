# Real-World External Battery

## Purpose

This module runs the external public-data validation battery after the expanded
post-discovery fixed-weight validation and evidence-taxonomy stages. It evaluates
frozen candidate estimators on empirical public datasets, using the taxonomy
table from the N_EST = 26 validation stage as the candidate source.

The battery does not rerun discovery, mutate GA weights, or retrain candidates.
Its role is to test whether fixed candidate estimators retain benchmark-relative
advantages outside the synthetic regime grid.

## Candidate Source

Candidates are loaded from:

```text
results/5_evidence_taxonomy_nest26/tables/evidence_taxonomy_all_candidates.csv
```

The runner keeps the taxonomy labels in the output, including:

- `transfer_specialist`
- `discovery_supported_local_candidate`
- `near_gate_candidate`
- `negative_control_success`

By default the module evaluates the complete taxonomy table, including negative
controls. This is intentional: the controls provide a check on whether external
signals are concentrated in the candidate classes where they are expected.

## Output Layout

The run writes into a structured result folder:

```text
01_results_basic/       compact tables for interpretation and manuscript work
02_reports/             PDF and markdown reports
03_audit_ready/         full provenance, decisions, oracle audit, and split CSVs
98_discarded_nonfinal/  superseded byproducts, kept out of evidence
99_recovery_logs/       downloads, checkpoints, logs, and recovery files
```

The committed result layer should keep final outputs from `01_results_basic`,
`02_reports`, and selected audit files from `03_audit_ready`. Large checkpoints
and raw downloads belong in the full data archive.

## Main Files

- `13_realworld_external_battery.R`: public-data catalog, evaluation loop,
  checkpointing, evidence labels, and final export.
- `realworld_external_battery_utils.R`: fixed-weight specialist loading,
  validation utilities, and empirical evaluation helpers.
- `generate_external_report.R`: base-R PDF and markdown report generator.
- `run_realworld_external_battery.sh`: shell entrypoint with reproducible
  defaults.

Supporting validation-stage modules are included in this folder because the
battery reuses the N_EST = 26 estimator registry and fixed-weight evaluation
logic.

## Recommended Run

From the repository root:

```bash
code/5_realworld_external_battery/run_realworld_external_battery.sh
```

The launcher sets:

- `PROJECT_ROOT` to this code folder.
- `VALIDATION_RESULTS_ROOT` to the repository root.
- `REALWORLD_OUT_ROOT` to `results`.
- `EXTERNAL_RUN_LABEL` to `6_realworld_external_battery`.

## Interpretation

The primary claim should use dataset-level decisions after paired CI and
Benjamini-Hochberg FDR correction. Gate-row wins and within-dataset depth probes
remain important, but they answer different questions.

- Gate-row wins show condition-level performance.
- Dataset-level rows define the independent external evidence layer.
- Depth probes test whether a signal persists within resampled or tail-enriched
  subscenarios of a dataset.
- Oracle rows are secondary audit rows and should not be promoted to the primary
  benchmark claim.

This battery is a regime-conditional transfer audit. It is designed to identify
where specialist candidates retain value, not to claim universal estimator
dominance.
