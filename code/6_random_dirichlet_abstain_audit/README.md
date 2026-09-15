# Random Dirichlet Abstention Audit

This folder contains the code used to audit NEST26 regimes classified as `benchmark_retained` after the fixed-weight validation and evidence-taxonomy stages.

The script samples random Dirichlet weight vectors over the same 26-estimator basis used by the expanded validation stage. Each random composite is evaluated with the same simulator, contamination rules, benchmark set, admissibility logic, q95 definition, and validation seeds.

The audit does not rerun the GA or modify discovered candidates. It tests whether a benchmark-retained regime remains unsupported under an independent random search of the same convex estimator space.

Run from the repository root:

```bash
Rscript code/6_random_dirichlet_abstain_audit/random_dirichlet_gate_audit.R --draws 4000 --R 500
```

The script uses:

- `code/3_validation_nest26/`
- `code/4_fixed_weight_validation_nest26/`
- `results/4_fixed_weight_validation_nest26/`
- `results/5_evidence_taxonomy_nest26/`

Curated outputs from the completed run are stored in `results/7_random_dirichlet_abstain_audit/`.
