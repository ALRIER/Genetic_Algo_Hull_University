# Phase 1 Fixed-Weight Validation Results (N_EST = 10)

This folder contains the post-discovery validation of frozen Phase 1 candidates. Candidate weights are evaluated without retraining.

## Folder contents

- `tables/`: final decision tables and estimator-level validation metrics.
- `figures/`: validation figures.
- `root_summaries/`: bootstrap, stability, selected-regime, and seed-level summaries.
- `audit/`: benchmark registry, configuration snapshot, code hashes, and regime-construction records.
- `run_metadata/`: task and runtime metadata for the completed validation run.

Together, these files provide the result and audit layers needed to inspect the fixed-weight validation stage.
