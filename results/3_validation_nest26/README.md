# Phase 2 Expanded Discovery Results (N_EST = 26)

This folder contains summarized outputs from the expanded regime-first genetic-algorithm search using the 26-estimator basis.

## Main files

- `combined_final_regime_results_all_rows.csv`: final regime-level results across seeds and families.
- `summary_by_family_across_seeds.csv`: family-level summary across discovery seeds.
- `summary_by_seed_family.csv`: results summarized by seed and family.
- `ga_winner_candidates_for_q1.csv`: selected GA candidates passed to the subsequent fixed-weight validation stage; the historical filename is retained for traceability.
- `per_family_candidate_summaries/`: detailed candidate outputs by family and seed.
- `per_seed_all_family_summaries/`: all-family summaries for each seed.

The completed run evaluated 36 final regimes, with 12 GA wins and 24 benchmark wins. The selected candidates feed the Phase 2 fixed-weight validation stage.
