# Code Map - modulos compartidos vs especificos por etapa

Comparacion por MD5 de modulos con el mismo nombre entre fases.

## GA core: discovery (N_EST=10) vs validation (N_EST=26)

| file | md5 A | md5 B | status |
| --- | --- | --- | --- |
| 00_utils_debug_io.R | E14CCADF | BEDA36F5 | DIFFERS |
| 01_paths_repro.R | 110DE91D | C0B71354 | DIFFERS |
| 02_scenarios_sampling.R | DDC158E5 | DDC158E5 | identical |
| 03_distributions_params.R | AAE1BAA7 | AAE1BAA7 | identical |
| 04_estimators_registry.R | EA720A85 | 64D07193 | DIFFERS |
| 05_ga_core.R | 7EF975AF | 559BCF5A | DIFFERS |
| 06_data_prep.R | 554B6E2B | 4E2A22D2 | DIFFERS |
| 07_fitness_objectives.R | D25C878A | C85D492C | DIFFERS |
| 08_LocationEstimators.R | 140E7D19 | 16BBFDAC | DIFFERS |
| 10_distributional_diagnostics.R | E0DA2006 | E0DA2006 | identical |
| run_experiment.R | E2F5D838 | 0FD8ABB4 | DIFFERS |

## Fixed-weight: N_EST=10 vs N_EST=26

| file | md5 A | md5 B | status |
| --- | --- | --- | --- |
| run_fixed_weight_validation.R | B446E41E | 491839FB | DIFFERS |

