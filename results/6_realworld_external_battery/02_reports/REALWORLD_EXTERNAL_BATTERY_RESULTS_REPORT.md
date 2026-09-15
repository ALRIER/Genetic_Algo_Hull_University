# Real-World External Battery Results

## Run summary

- Requested public sources: 264
- Loaded sources: 228
- Selected and evaluated sources: 128
- Primary dataset-level rows: 809
- Unique evaluated dataset identifiers: 120
- Strong-win rows before dataset-level FDR: 256
- Dataset-level BH-FDR survivors at 5%: 255
- Dataset-level BH-FDR survivors at 10%: 257
- Saturated datasets under the cap: 21

## Evidence distribution

- `equal_weight_gain`: 458
- `strong_win`: 256
- `no_win`: 77
- `strong_point_signal`: 15
- `pareto_signal`: 3

## Confidence-interval layer

- `descriptive`: 532
- `ci_confirmed`: 256
- `near_ci`: 21

## Survivor profile

At the 10% sensitivity threshold, 257 rows remain across 82 unique datasets. The surviving rows are concentrated in lognormal-, normal-, and Weibull-like profiles and appear in both original-regime and locked-unseen-similar evaluations.

The largest survivor counts occur for `TAX-Q1R008`, `TAX-Q1R009`, and `TAX-Q1R010`. Detailed candidate, dataset, and domain counts are available in the compact result tables under `../01_results_basic/`.

## Interpretation

Primary inference is based on dataset-level Benjamini-Hochberg FDR for eligible, profile-matched comparisons. At the 5% threshold, 255 dataset-level rows remain. The 10% sensitivity threshold adds two rows, indicating that the overall evidence pattern is not driven by relaxing the multiplicity threshold.

The results are regime-conditional. Surviving rows are concentrated in empirical profiles that match the specialist families, while other profiles remain benchmark-favored or unsupported. Depth probes provide secondary within-dataset checks and are not treated as independent datasets.

Audit-ready dataset, gate, oracle, and condition-level tables are stored under `../03_audit_ready/`. Large tables are split into numbered CSV parts so that the full audit layer remains inspectable within the repository.
