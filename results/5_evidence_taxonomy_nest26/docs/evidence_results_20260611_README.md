# Methodological Guide: Post-Discovery Fixed-Weight Validation (N_EST = 26)

Generated on: 2026-06-11 18:27:46 UTC

## Purpose

This document summarizes the post hoc evidence taxonomy applied to the completed expanded validation-stage experiment. It does not rerun the genetic algorithm, does not alter the original validation outputs, and does not change any fixed-weight validation result. It only recatalogs the existing result tables into a more informative hierarchy of evidence.

The public methodological name for the former internal Q1 stage is:

**Post-Discovery Fixed-Weight Validation (N_EST = 26)**

Internal files may still contain the historical `q1_` prefix for compatibility. In manuscripts, repositories, and reviewer-facing descriptions, the public name above should be used.

## Why the Taxonomy Was Added

The original final decision table used a narrow pass/fail style classification. That is useful for strict validation, but it collapses several scientifically distinct cases. A candidate discovered by the regime-first GA can be meaningful even when it does not survive the strictest fixed-weight validation gate. The taxonomy therefore separates discovery evidence from fixed-weight validation evidence.

This is especially important under a No Free Lunch interpretation. The experiment is not designed to find a universally dominant estimator. It is designed to identify local/specialist candidates and then grade how far their advantage generalizes.

## Evidence Grades

- `confirmed_specialist`: the candidate wins in both the original regime and locked unseen similar validation, with CI confirmation.
- `transfer_specialist`: the candidate wins in locked unseen similar validation with CI confirmation, even if it does not win in the original regime.
- `original_regime_specialist`: the candidate wins in the original regime with CI confirmation, but not in locked unseen similar validation.
- `discovery_supported_local_candidate`: the candidate was an accepted GA winner in the expanded discovery run, but was not confirmed by fixed-weight validation.
- `near_gate_candidate`: the candidate was retained as a borderline/near-gate candidate for audit, but was not confirmed by fixed-weight validation.
- `benchmark_retained`: the expanded benchmark set retained superiority under fixed-weight validation.
- `negative_control_success`: benchmark/control candidates correctly failed to beat the expanded fixed-weight validation gate.

## Evidence Grade Counts

| evidence_grade | n |
| --- | --- |
| transfer_specialist |  2 |
| discovery_supported_local_candidate | 10 |
| near_gate_candidate |  7 |
| negative_control_success |  6 |

## Legacy Strict Decision Counts

The original strict final decision is preserved as a support table. The taxonomy does not erase the strict outcome; it adds an evidence-grade interpretation on top of it.

| legacy_final_decision | n |
| --- | --- |
| benchmark_retained | 17 |
| marginal_ga_win |  2 |
| negative_control_success |  6 |

## Validated Specialist Candidates

| validation_id | family | regime | seed | evidence_grade | locked_mean_gain | locked_q95_gain | original_mean_gain | original_q95_gain | condition |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Q1R011 | weibull | REG01 | 101 | transfer_specialist | +2.79% | +2.72% | -15.77% | -45.06% | shape=right_skewed_flexible_shape | tail=flexible_tail | contamination=contaminated | direction=upper_tail | structure=tail_outlier | severity=extreme_severity | rates={0.05;0.1} | scales={30} | types={upper_tail} |
| Q1R012 | weibull | REG02 | 202 | transfer_specialist | +3.18% | +1.25% | -15.20% | -34.56% | shape=right_skewed_flexible_shape | tail=flexible_tail | contamination=contaminated | direction=point_mass | structure=point_mass | severity=extreme_severity | rates={0.05;0.1} | scales={30} | types={point_mass} |

## Main Result Summary

The expanded validation-stage discovery run identified 12 accepted GA winners before fixed-weight validation. After the Post-Discovery Fixed-Weight Validation (N_EST = 26), two candidates were upgraded to `transfer_specialist` status. Both are Weibull candidates and both won under locked unseen similar validation with bootstrap CI confirmation across all validation seeds. No candidate reached `confirmed_specialist` status because none won both original-regime and locked unseen similar validation.

The two validated transfer specialists are:

- `Q1R011`: Weibull REG01, source seed 101, upper-tail/tail-outlier contamination, moderate rate, extreme scale, extreme severity.
- `Q1R012`: Weibull REG02, source seed 202, point-mass contamination, moderate rate, extreme scale, extreme severity.

These candidates should be interpreted as local transfer specialists, not as universal estimators. They generalize to similar locked unseen Weibull regimes but do not dominate the original-regime fixed-weight validation gate.

## Naming Changes

The internal stage formerly called Q1 should be described publicly as `Post-Discovery Fixed-Weight Validation (N_EST = 26)`. This name is more accurate because the stage evaluates fixed candidate weights after discovery, uses the expanded 26-estimator basis, and does not continue GA optimization.

Recommended manuscript language:

> Twelve regime-first GA candidates were identified during the expanded validation-stage discovery run. Under the stricter Post-Discovery Fixed-Weight Validation (N_EST = 26), two Weibull candidates were upgraded to transfer-specialist status, while the remaining discovery-supported candidates did not generalize under the expanded fixed-weight benchmark gate.

## Output Tables Produced

- `tables/evidence_taxonomy_all_candidates.csv`: all 25 validation entries with public evidence grades.
- `tables/validated_specialists.csv`: candidates promoted to confirmed/transfer/original-regime specialist status.
- `tables/discovery_supported_local_candidates_not_fixed_weight_confirmed.csv`: accepted discovery winners not confirmed by fixed-weight validation.
- `tables/near_gate_candidates_not_fixed_weight_confirmed.csv`: borderline candidates not confirmed by fixed-weight validation.
- `tables/evidence_grade_counts.csv`: count by evidence grade.
- `tables/evidence_grade_by_family.csv`: evidence-grade distribution by family.
- `support/post_discovery_fixed_weight_validation_regime_stability.csv`: renamed support copy of the regime-stability table.
- `support/post_discovery_fixed_weight_validation_bootstrap_ci.csv`: renamed support copy of bootstrap CI evidence.
- `support/post_discovery_fixed_weight_validation_seed_level_gate.csv`: renamed support copy of seed-level gate evidence.

## Interpretation for Manuscript Updating

The strongest result is not that many candidates universally win. The strongest result is that a severe fixed-weight validation stage prunes the discovery set while preserving two coherent Weibull specialists under extreme contamination. This supports a regime-first and No Free Lunch framing: candidate value is local, evidence is graded, and benchmark superiority remains common outside the validated local regimes.

The remaining accepted GA winners should not be described as fixed-weight validation winners. They should be described as `discovery_supported_local_candidate` cases: candidates that passed the adaptive regime-first discovery stage but were not upgraded by the stricter fixed-weight validation stage.

## Source Tables Used

- `Q1_VALIDATION_OUTPUT_NEW_EXPERIMENT_20260611/tables/q1_final_decision_table.csv`
- `Q1_VALIDATION_OUTPUT_NEW_EXPERIMENT_20260611/tables/q1_table_regime_stability.csv`
- `Q1_VALIDATION_OUTPUT_NEW_EXPERIMENT_20260611/tables/q1_table_bootstrap_ci.csv`
- `Q1_VALIDATION_OUTPUT_NEW_EXPERIMENT_20260611/tables/q1_table_seed_level_expanded_gate.csv`
- `Q1_VALIDATION_OUTPUT_NEW_EXPERIMENT_20260611/q1_selected_regimes.csv`
