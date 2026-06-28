# Random Dirichlet Abstention Audit

This folder contains the code annex used to audit NEST26 `benchmark_retained` regimes after the final evidence taxonomy was built.

The script samples random Dirichlet weight vectors over the same 26-estimator basis used by the expanded validation stage. It then evaluates those random composites with the original validation simulator, contamination rules, benchmark set, admissibility logic, q95 definition, and validation seeds.

The audit does not rerun the GA and does not modify discovered candidates. It answers a post-taxonomy question: whether a benchmark-retained regime remains unsupported after an independent random search of the same convex estimator space.

Run from a final-results bundle that contains stages 04, 05, and 07:

```bash
Rscript 08_RANDOM_DIRICHLET_ABSTAIN_AUDIT/code/random_dirichlet_gate_audit.R --draws 4000 --R 500
```

Curated results from the completed run are stored in:

```text
GA_Results/07_random_dirichlet_abstain_audit_NEST26/
```
