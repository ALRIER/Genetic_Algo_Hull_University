# Random Dirichlet Abstention Audit

This folder contains the code annex used to audit NEST26 `benchmark_retained` regimes after the final evidence taxonomy was built.

The script samples random Dirichlet weight vectors over the same 26-estimator basis used by the expanded validation stage. It then evaluates those random composites with the original validation simulator, contamination rules, benchmark set, admissibility logic, q95 definition, and validation seeds.

The audit does not rerun the GA and does not modify discovered candidates. It answers a post-taxonomy question: whether a benchmark-retained regime remains unsupported after an independent random search of the same convex estimator space.

Run from this repository root:

```bash
Rscript code/6_random_dirichlet_abstain_audit/random_dirichlet_gate_audit.R --draws 4000 --R 500
```

By default the script reads the cleaned repository layout:

- `code/3_validation_nest26`
- `code/4_fixed_weight_validation_nest26`
- `results/4_fixed_weight_validation_nest26`
- `results/5_evidence_taxonomy_nest26`

It also keeps fallback support for the original full-results bundle layout.

Curated results from the completed run are stored in:

```text
results/7_random_dirichlet_abstain_audit/
```
