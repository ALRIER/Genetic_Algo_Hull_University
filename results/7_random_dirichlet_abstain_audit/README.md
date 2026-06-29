# Random Dirichlet Abstention Audit, NEST26

This folder contains the post-taxonomy random-search audit for the NEST26 benchmark-retained regimes.

## Purpose

The evidence taxonomy in stage 05 labels 17 NEST26 candidates as `benchmark_retained`. This means the fixed GA candidate was not retained as a validated win. The audit asks a narrower follow-up question:

> If the selected GA candidate did not pass the gate, does the same 26-estimator convex space still contain random Dirichlet composites that pass the original dual gate?

The audit does not rerun the GA. It samples random Dirichlet weight vectors over the same 26 estimator components, evaluates them with the original simulator, contamination logic, benchmarks, q95 definition, admissibility rules, and validation seeds, then checks whether any draw beats the best admissible benchmark on both mean MSE and q95 MSE across all seeds.

## Inputs

The script uses the cleaned repository structure by default:

- `results/5_evidence_taxonomy_nest26/tables/evidence_taxonomy_all_candidates.csv`
- `results/4_fixed_weight_validation_nest26/root_summaries/post_discovery_fixed_weight_validation_selected_regimes.csv`
- `code/3_validation_nest26`
- `code/4_fixed_weight_validation_nest26`

It also keeps fallback support for the original full-results bundle paths:

- `05_EVIDENCE_TAXONOMY_NEST26/evidence_results_20260611/tables/evidence_taxonomy_all_candidates.csv`
- `04_EXPANDED_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST26/raw_full_run_output_with_tasks_checkpoints_20260611/q1_selected_regimes.csv`
- `07_CODE_ARCHIVE/expanded_and_realworld_experiment_code`
- `07_CODE_ARCHIVE/original_fixed_weight_validation_code`

The script checks that the sourced estimator basis has `N_EST = 26`.

## Run

From the repository root:

```bash
Rscript code/6_random_dirichlet_abstain_audit/random_dirichlet_gate_audit.R --draws 4000 --R 500
```

The completed production run used:

- 17 `benchmark_retained` regimes
- 2 validation modes: `original_regime` and `locked_unseen_similar`
- 8 validation seeds: `303,404,505,606,707,808,909,1010`
- 4000 Dirichlet draws per regime-mode
- Monte Carlo budget `R = 500`
- Dirichlet alpha `0.30`

## Results

Main result files:

- `tables/abstain_audit_results.csv`: one row per audited regime-mode.
- `tables/abstain_audit_summary_by_regime.csv`: compact summary collapsed to one row per regime.
- `tables/dirichlet_signal_regime_modes.csv`: only regime-modes where at least one draw passed all seeds.
- `tables/gate_sanity_transfer_specialists.csv`: sanity check on the two known transfer specialists.
- `tables/abstain_audit_counts.csv`: headline counts.
- `logs/run_full_20260627_115103.log`: terminal log from the production run.

Headline results:

- 34 regime-mode rows audited.
- 23 of 34 regime-mode rows confirmed the abstention: no random Dirichlet draw passed all seeds.
- 11 of 34 regime-mode rows showed at least one seed-stable random Dirichlet pass.
- 17 unique regimes audited.
- 9 of 17 unique regimes were fully confirmed across both validation modes.
- 8 of 17 unique regimes showed some Dirichlet signal.
- The transfer-specialist sanity check passed: Q1R011 and Q1R012 each passed 8 of 8 seeds in `locked_unseen_similar`.

Regimes with Dirichlet signal:

- Q1R003, exwald, discovery-supported local candidate: 76 total seed-stable passes.
- Q1R005, invgauss, discovery-supported local candidate: 22 total seed-stable passes.
- Q1R018, weibull, near-gate candidate: 14 total seed-stable passes.
- Q1R006, invgauss, discovery-supported local candidate: 6 total seed-stable passes.
- Q1R002, exwald, discovery-supported local candidate: 4 total seed-stable passes.
- Q1R004, invgauss, discovery-supported local candidate: 3 total seed-stable passes.
- Q1R008, lognormal, discovery-supported local candidate: 1 total seed-stable pass.
- Q1R009, lognormal, discovery-supported local candidate: 1 total seed-stable pass.

Fully confirmed abstentions:

- Q1R001
- Q1R007
- Q1R010
- Q1R013
- Q1R014
- Q1R015
- Q1R016
- Q1R017
- Q1R019

## Interpretation

This audit strengthens the taxonomy rather than replacing it. A confirmed abstention means that the selected GA candidate failed and an independent random search over the same estimator simplex also failed to find a seed-stable winner. A random-search pass means the abstention should be revisited: the convex estimator space contains at least one stable candidate even though the selected fixed GA candidate was not retained.

The strongest follow-up target is Q1R003 in `locked_unseen_similar`, where 72 of 4000 Dirichlet draws passed all eight seeds. The next methodological step is to persist the winning Dirichlet weight vectors and re-evaluate them as candidate specialists.
