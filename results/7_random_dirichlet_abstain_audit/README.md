# Random Dirichlet Abstention Audit (N_EST = 26)

This folder contains the random-search audit of the NEST26 regimes classified as `benchmark_retained` after fixed-weight validation.

## Purpose

The audit tests whether the same 26-estimator convex space contains random Dirichlet composites that pass the original dual gate when the selected GA candidate does not. It does not rerun or retrain the genetic algorithm.

Random weight vectors are evaluated with the same simulator, contamination rules, benchmark definitions, q95 metric, admissibility rules, and validation seeds used by the fixed-weight validation stage.

## Inputs

- `../5_evidence_taxonomy_nest26/tables/evidence_taxonomy_all_candidates.csv`
- `../4_fixed_weight_validation_nest26/root_summaries/post_discovery_fixed_weight_validation_selected_regimes.csv`
- `../../code/3_validation_nest26/`
- `../../code/4_fixed_weight_validation_nest26/`

## Reproduction

From the repository root:

```bash
Rscript code/6_random_dirichlet_abstain_audit/random_dirichlet_gate_audit.R --draws 4000 --R 500
```

The recorded run used 17 benchmark-retained regimes, two validation modes, eight validation seeds, 4000 Dirichlet draws per regime-mode, Monte Carlo budget `R = 500`, and Dirichlet alpha `0.30`.

## Results

- 34 regime-mode rows were audited.
- 23 of 34 rows contained no random Dirichlet vector that passed all seeds.
- 11 of 34 rows contained at least one seed-stable random Dirichlet pass.
- 9 of 17 unique regimes were confirmed across both validation modes.
- 8 of 17 unique regimes showed some Dirichlet signal.
- The transfer-specialist sanity check passed for `Q1R011` and `Q1R012` across all eight seeds in `locked_unseen_similar` mode.

## Result tables

- `tables/abstain_audit_results.csv`: one row per audited regime-mode.
- `tables/abstain_audit_summary_by_regime.csv`: summary by regime.
- `tables/dirichlet_signal_regime_modes.csv`: regime-modes with at least one all-seed pass.
- `tables/gate_sanity_transfer_specialists.csv`: transfer-specialist sanity check.
- `tables/abstain_audit_counts.csv`: headline audit counts.

A confirmed abstention means that neither the selected fixed GA candidate nor the independent random search produced a seed-stable winner under the same gate. A random-search pass indicates that the estimator space contains at least one stable alternative in that regime-mode.
