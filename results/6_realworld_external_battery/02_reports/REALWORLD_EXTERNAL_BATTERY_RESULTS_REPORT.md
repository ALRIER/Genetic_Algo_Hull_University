# Real-World External Battery V5 Results Report

## Run Summary

- Run directory: `realworld_battery_40datasets_full_run`
- Requested public sources: 264
- Loaded sources: 228
- Selected/evaluated sources: 128
- Primary dataset-level rows: 809
- Unique evaluated dataset identifiers in compact output: 120
- Strong-win rows before dataset-level FDR: 256
- Dataset-level BH-FDR survivors at 5%: 255
- Dataset-level BH-FDR survivors at 10% sensitivity: 257
- Saturated datasets under the cap: 21

## Evidence Distribution

- `equal_weight_gain`: 458
- `strong_win`: 256
- `no_win`: 77
- `strong_point_signal`: 15
- `pareto_signal`: 3

## CI Layer Distribution

- `descriptive`: 532
- `ci_confirmed`: 256
- `near_ci`: 21

## Survivor Profile

- Survivor rows at 10% sensitivity: 257
- Unique survivor datasets: 82
- Strong-win survivor rows: 256
- Non-strong sensitivity rows: 1

Survivors by specialist family:

- `lognormal`: 132
- `normal`: 99
- `weibull`: 26

Survivors by regime mode:

- `original_regime`: 163
- `locked_unseen_similar`: 94

Top survivor specialists:

- `TAX-Q1R008`: 77
- `TAX-Q1R009`: 55
- `TAX-Q1R010`: 40
- `TAX-Q1R025`: 19
- `TAX-Q1R023`: 18
- `TAX-Q1R018`: 11
- `TAX-Q1R019`: 11
- `TAX-Q1R024`: 9
- `TAX-Q1R017`: 8
- `TAX-Q1R016`: 5
- `TAX-Q1R011`: 3
- `TAX-Q1R012`: 1

Top survivor domains:

- `positive_skew_tabular`: 38
- `mobility_demand`: 34
- `rdatasets_gt`: 25
- `web_popularity`: 22
- `rdatasets_bayesrules`: 14
- `rdatasets_boot`: 11
- `rdatasets_ISLR`: 11
- `rdatasets_admiral`: 10
- `rdatasets_fpp2`: 10
- `rdatasets_psych`: 10
- `biological_positive`: 10
- `rdatasets_archdata`: 9
- `rdatasets_dragracer`: 9
- `rdatasets_HistData`: 9
- `rdatasets_asaur`: 8

## Interpretation

The external battery produced a strong positive result after the dataset-level multiplicity layer. The strict primary claim should use the 5% BH-FDR column, not raw gate-row counts or depth-probe totals. On that strict definition, 255 dataset-level rows survive. At the 10% sensitivity threshold, 257 rows survive, adding only two marginal rows, so the result is not driven by relaxing the FDR threshold.

The signal is conditional rather than universal. Most surviving rows are concentrated in lognormal, normal, and Weibull-like profiles where the specialist family matches the empirical profile gate. This supports the intended No Free Lunch interpretation: the GA weights do not dominate everywhere, but they retain externally validated value in profile-matched regimes.

The depth probes should be treated as within-dataset mechanism checks. They strengthen the interpretation of repeated signal inside a source, but they should not be counted as independent public datasets. The saturation cap is therefore important: it prevents a single dataset from inflating the primary count while preserving the evidence pattern for audit.

This repository excludes recovery logs, checkpoints, raw downloads, and the large split condition-metric table. Those files belong in the full data archive. The committed result layer keeps the compact decision tables, dataset registry, run manifest, generated report, and gate/oracle audit tables needed to inspect the result.
