# External Public Dataset Battery

Requested public sources: 264; loaded sources: 228.
Gate rows: 5900. Base R=500; targeted extra R=300; base bootstrap B=500; max adaptive B=1500.
Profile gate: TRUE; primary benchmark policy: profile_preregistered; primary metric: q95.
Secondary oracle audit written: TRUE; oracle bootstrap B: 0.
Dataset-level BH FDR: alpha 0.05 primary, 0.10 secondary sensitivity.
CSV split threshold: 5000 rows per part.
Output layout: basic=01_results_basic; reports=02_reports; audit=03_audit_ready; discarded=98_discarded_nonfinal; recovery=99_recovery_logs.
Dataset breadth target: 120; source cap: 0.25; saturation cap: 10 countable strong wins per dataset.
Futility stop: TRUE after at least 12 gate rows without positive or near-CI signal.
Secondary depth probes: TRUE; trigger=3 countable strong wins; added=78 probes.
Contamination mode: endogenous; natural z threshold: 3.00.

Primary evidence uses the pre-registered profile benchmark and profile gate. Oracle comparisons are secondary audit rows and should not be promoted to the primary claim.
All source URLs, access timestamps, downloaded-file MD5 hashes, variables, transformations, and dataset diagnostics are recorded in `external_dataset_registry.csv`.
Large tables are split into part files when they exceed EXTERNAL_CSV_MAX_ROWS. The original CSV name becomes a small index listing each part.
V5 breadth/depth and audit-ready policy: primary N is based on selected primary datasets. Within-dataset depth probes are secondary and share parent_dataset_id; countable_primary_win caps each dataset at EXTERNAL_WINS_SATURATION_CAP.
Dataset-level BH FDR is computed after aggregation and should be treated as the primary multiplicity control. Gate-row FDR columns are included only for transparency.
