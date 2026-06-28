# V5 preregistered real-world validation taxonomy

## Objective
Validate frozen GA specialist estimators on a broader, more diverse external
public-data battery without rerunning discovery or changing the learned weights.
The purpose is not to inflate winners. The purpose is to separate strict
confirmatory wins from secondary signals and audit controls.

## Primary dataset breadth
The primary external battery targets 100-120 datasets. Dataset inclusion is
balanced across empirical profiles and source groups. No source group should
exceed the configured cap, default 25 percent of selected primary datasets.

## Output layout
The run writes four folders:

1. `01_results_basic`: compact final results for reading, writing, and review.
2. `02_reports`: generated PDF and markdown report.
3. `03_audit_ready`: full provenance, decisions, oracle audit, and split CSVs.
4. `99_recovery_logs`: downloads, checkpoints, temporary recovery files, and logs.

## Empirical profiles
A dataset is assigned to one empirical profile before any winner is counted:
normal_like, lognormal_like, weibull_like, heavy_tail_symmetric,
bounded_or_proportion, zero_inflated_count, or empirical_unknown.

## Eligibility
A specialist competes in the primary test only if the specialist family matches
the dataset empirical profile. Non-matching specialist rows are negative-control
or oracle-audit rows, not primary evidence.

## Benchmark policy
The primary benchmark is pre-registered by profile. The oracle benchmark, based
on the best joint benchmark available in the cell, is reported only as a
secondary audit.

## Evidence layers
Strong win: profile eligible, improves both mean MSE and q95 MSE, and the paired
bootstrap interval for the reported primary gain is fully above zero.

Strong point signal: profile eligible and improves both mean MSE and q95 MSE,
but the CI is not fully confirmed.

Pareto signal: profile eligible and improves one metric without materially
worsening the other, using the predeclared tolerance.

Equal weight gain: GA specialist improves over the equal-weight composite but
not over the primary benchmark.

No win: no positive support by the above taxonomy.

## CI policy
The confidence interval must be a paired bootstrap over the same gain that is
reported. If q95 gain is reported, the bootstrap statistic is q95 benchmark
error minus q95 GA error. Adaptive bootstrap may increase B near the boundary,
but it cannot create or amplify the point gain.

## FDR policy
Dataset-level Benjamini-Hochberg FDR is computed on
`external_dataset_level_decisions.csv` after aggregation. The strict claim uses
5 percent FDR. A 10 percent FDR column is retained as sensitivity analysis.
Gate-row FDR columns are retained for transparency only because condition rows
inside a dataset are correlated.

## Saturation cap
Each primary dataset can contribute at most EXTERNAL_WINS_SATURATION_CAP
countable strong wins, default 10. Additional strong signals are retained in the
audit trail as saturation information, but they do not increase the primary win
count.

## Futility stop
After EXTERNAL_FUTILITY_MIN_GATE_ROWS gate rows, default 12, a primary dataset
may stop early if it has no countable strong wins, no positive point signal, and
no near-CI signal. The evaluated rows remain in the audit trail.

## Secondary depth probes
If a primary dataset reaches the configured signal trigger, default 3 countable
strong wins, the runner may queue within-dataset depth probes: body sample,
upper-tail enriched sample, and bootstrap full sample. These probes share
parent_dataset_id and analysis_role == within_dataset_depth_probe. They are not
new independent datasets.

## Reporting rule
Primary headline counts must be based on selected primary datasets after
profile gate, paired CI, saturation cap, and dataset-level BH FDR. Secondary
depth and oracle audit results should be reported in separate tables.
