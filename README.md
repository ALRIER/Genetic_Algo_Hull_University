# Genetic Algorithms for Regime-Specific Location Estimators — University of Hull

This repository is the publication-oriented code and curated-result compendium for a two-phase research program on genetic algorithms (GA) for robust, regime-specific location estimation. It documents the full experimental pipeline: how Monte Carlo samples were generated, how distributional regimes were defined, how a genetic algorithm searched a space of weighted estimator combinations, how candidates were filtered and validated, how winners from the first phase were inherited into a stricter second phase, and how the resulting fixed-weight estimators were stress-tested against external public data.

The repository is **not** a general-purpose R package. It is a reproducibility and audit archive: a clean, self-contained record that allows an independent researcher to follow the methodology end to end and to verify the reported findings.

## Scientific Purpose

The central question is whether a genetic algorithm can construct weighted location estimators that outperform strong benchmark estimators within specific data-generating regimes. The emphasis is **local, regime-specific** performance rather than universal dominance. This is consistent with the No-Free-Lunch perspective: a method is not expected to dominate everywhere, but it may be systematically superior under particular distributional, contamination, and sample-size conditions.

The GA does not invent an estimator from raw observations. It learns convex combinations (weight vectors) over a predefined basis of location estimators, and each candidate is evaluated against benchmark estimators across a structured family-by-regime scenario grid. A candidate is interesting only when it improves over benchmarks in a defined family/regime combination **and** remains credible under post-discovery, fixed-weight validation.

## Repository Layout

```
Genetic_Algo_Hull_University/
├── README.md
├── MANIFEST.md
├── CODE_MAP.md                 # map of shared vs stage-specific modules
├── .gitignore
├── code/
│   ├── 1_discovery_nest10/                  # Phase 1 GA discovery (N_EST = 10)
│   ├── 2_fixed_weight_validation_nest10/    # fixed-weight validation of Phase 1 candidates
│   ├── 3_validation_nest26/                 # Phase 2 expanded GA with inheritance (N_EST = 26)
│   ├── 4_fixed_weight_validation_nest26/    # fixed-weight validation of Phase 2 candidates
│   ├── 5_realworld_external_battery/        # external public-data battery for frozen specialists
│   └── 6_random_dirichlet_abstain_audit/    # random-Dirichlet stress test of the regime map
└── results/
    ├── 1_discovery_nest10/
    ├── 2_fixed_weight_validation_nest10/
    ├── 3_validation_nest26/
    ├── 4_fixed_weight_validation_nest26/
    ├── 5_evidence_taxonomy_nest26/
    ├── 6_realworld_external_battery/
    └── 7_random_dirichlet_abstain_audit/
```

`code/` holds the cleaned source for each stage. `results/` holds a compact, GitHub-friendly subset of summaries, diagnostic figures, evidence-taxonomy tables, and small audit-metadata files that let the main findings be reconstructed quickly without downloading the full raw experiment output. Large raw outputs (RDS checkpoints, per-task dumps, logs, downloaded datasets) are intentionally excluded and belong in a research-data repository (see *Data Availability*).

## Monte Carlo Sample Generator

The experiments run on simulated Monte Carlo samples drawn from six distribution families:

- Normal
- Lognormal
- Weibull
- Inverse Gaussian
- Ex-Gaussian
- Ex-Wald

These expose the estimators to symmetric and asymmetric settings, light-tailed and heavy/skewed settings, and reaction-time–relevant shapes. The ex-Gaussian, inverse-Gaussian, Weibull, and ex-Wald families are especially relevant for response-time and latency-style data, where skewness and tail behavior dominate.

Sample generation and family-specific parameterization are defined in the `02_scenarios_sampling.R` and `03_distributions_params.R` modules of each GA stage.

## Scenario Design

Each family is evaluated across a grid of Monte Carlo scenarios that vary along several methodological axes:

- **Sample-size regime** — small, medium, and larger samples test behavior under different information budgets.
- **Contamination rate** — clean and contaminated samples, with contamination intensity varied across regimes.
- **Contamination structure** — different contamination mechanisms and placements, rather than a single generic outlier process.
- **Outlier scale/severity** — contamination magnitude is varied so estimators face both mild and severe perturbations.
- **Distributional shape** — skewness, tail weight, and family-specific parameters change across the grid.
- **Regime labels** — each family/scenario combination is assigned a regime identity used by the GA and by all downstream validation summaries.

The GA is therefore evaluated across a structured family-by-regime space, allowing the experiment to identify where a candidate is useful and where it fails.

## Estimator Basis

The GA learns weighted combinations of a predefined basis of location estimators. Two basis sizes are central to the design:

- **Phase 1 (discovery):** `N_EST = 10`
- **Phase 2 (validation):** `N_EST = 26`

The first phase used a smaller basis for discovery. The second phase expanded the basis to 26 estimators, allowing the GA to search a richer combination space. The registry for each phase is defined in its `04_estimators_registry.R`.

## Genetic Algorithm Logic

The GA core (`05_ga_core.R`) and the benchmark-relative objective functions (`07_fitness_objectives.R`) implement a regime-first search over weight vectors. Each candidate weight vector defines a composite location estimator, evaluated against benchmark estimators under Monte Carlo scenarios and scored by benchmark-relative loss/gain. The GA includes the standard evolutionary components: initialization of weight vectors, fitness evaluation, selection, crossover/recombination, mutation, constraint handling for valid (simplex) weights, and summarization of best candidates per regime.

## Phase 1 — Discovery (`code/1_discovery_nest10/`)

The cleaned original discovery experiment: a regime-first search using the 10-estimator basis and the original 60–80 exposure logic. Its role is to find promising specialist candidates rather than universal winners. The accompanying fixed-weight validation layer (`code/2_fixed_weight_validation_nest10/`) re-evaluates those candidates with **frozen** weights, on additional seeds and held-out regimes, without allowing further optimization. This separates candidate *generation* from candidate *evaluation*: discovery can find a promising estimator, but fixed-weight validation asks whether it remains credible once it is no longer being optimized.

## Phase 2 — Validation with Inheritance (`code/3_validation_nest26/`)

The second phase is not a rerun. It expands the estimator basis to 26, increases benchmark exposure, raises the high-performance filter to 90% (HPF2), and **inherits** winners from Phase 1 as candidates that must compete again under the stricter design. Inheritance is a stress test, not a shortcut: an inherited candidate that survives provides evidence of cross-phase stability; one that fails is also informative. The matching fixed-weight validation layer (`code/4_fixed_weight_validation_nest26/`) freezes the surviving candidates and re-evaluates them under post-discovery seeds and regimes in the expanded setting.

## External Real-World Battery (`code/5_realworld_external_battery/`)

The frozen specialist estimators are confronted with external public datasets organized under a pre-registered family/regime taxonomy. This evaluates whether estimators selected on simulated regimes retain their advantage on independent real-world data, outside the exact conditions in which they were discovered.

## Random-Dirichlet Abstain Audit (`code/6_random_dirichlet_abstain_audit/`)

A targeted stress test of the regime map. For each benchmark-retained regime where the pipeline abstains from naming a winner, a large set of random Dirichlet weight vectors is drawn over the same estimator simplex and pushed through the identical dual gate (mean and tail criteria, stable across all seeds). A regime's abstention is *confirmed* when no random vector passes. The audit refines, rather than overturns, the regime map: it confirms the abstentions in the great majority of cells, isolates a small number of low-rate candidate regimes for future targeted search, and serves as a negative-control check that the gate is not trivially permissive.

## Phase 1 vs Phase 2

| Feature | Phase 1 — Discovery | Phase 2 — Validation |
| --- | --- | --- |
| Estimator basis | 10 estimators | 26 estimators |
| Main role | Initial candidate discovery | Expanded second-phase search |
| Exposure logic | Original 60–80 | Expanded HPF2 (90%) |
| Inheritance | None | Phase 1 winners reintroduced |
| Benchmark exposure | Original set | Expanded set |
| Fixed-weight validation | `code/2_...nest10/` | `code/4_...nest26/` |

The phases are kept separate **on purpose**, because the estimator basis and validation logic differ. Code should not be mixed across phases: use the `N_EST = 10` modules for Phase 1 and the `N_EST = 26` modules for Phase 2.

## Shared vs Stage-Specific Code

Several modules share the same filename across phases (for example `00_utils_debug_io.R`, `01_paths_repro.R`). Some are byte-identical between phases; others differ because they encode the phase differences (estimator basis, GA operators tuned to the expanded basis, fitness/benchmark exposure, inheritance). To keep every stage independently runnable and to preserve an exact record of what produced each result, the stages are kept self-contained rather than collapsed into a single copy. `CODE_MAP.md` documents, file by file, which shared-named modules are identical across phases and which differ — so the structure can be understood at a glance without diffing the folders manually.

## Reproducibility Notes

- Each stage folder is self-contained and runnable on its own; `run_*.sh` / `run_*.R` launchers are included where applicable.
- Do not mix the `N_EST = 10` and `N_EST = 26` modules.
- Random seeds and path/reproducibility settings are handled in each stage's `01_paths_repro.R`.
- The compact `results/` layer is sufficient to reconstruct the headline findings; full raw output is hosted separately.

## Data Availability

This repository holds cleaned source code and a lightweight curated-result subset. Full Monte Carlo output (large CSV/RDS archives, per-task checkpoints, downloaded external datasets, and complete evidence packs) can exceed practical GitHub limits and should be deposited in a research-data repository such as OSF, Zenodo, or institutional storage. Add the deposit DOI/link here once available.

## Citation

If you use this code, please cite the associated research output and this repository. A `CITATION.cff` can be added at the project root to standardize attribution.
