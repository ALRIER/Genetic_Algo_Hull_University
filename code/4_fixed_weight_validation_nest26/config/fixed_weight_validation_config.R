# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION CONFIGURATION
# =============================================================================
# This file is intentionally separate from the original discovery pipeline.  The
# original GA code remains untouched.  This layer reads the existing discovery
# outputs, selects a focused set of regimes, and re-evaluates the fixed GA winners
# against an expanded benchmark set under additional independent validation seeds.
# =============================================================================

FIXED_WEIGHT_VALIDATION_CONFIG <- list(
  # Paths. Set PROJECT_ROOT to the folder containing 00_utils_debug_io.R ...
  # 11_validation_suite.R. Set DISCOVERY_ROOT to the folder containing the final
  # GA_REGIME_FIRST_* result folders.
  project_root = Sys.getenv("PROJECT_ROOT", unset = normalizePath(".", winslash = "/", mustWork = FALSE)),
  discovery_root = Sys.getenv("DISCOVERY_ROOT", unset = normalizePath(".", winslash = "/", mustWork = FALSE)),
  output_root = Sys.getenv("VALIDATION_OUTPUT_ROOT", unset = Sys.getenv("Q1_OUTPUT_ROOT", unset = file.path(getwd(), "FIXED_WEIGHT_VALIDATION_OUTPUT"))),

  # Thermal/computation safety. The layer never uses more than 5 workers.
  workers = 5L,
  reserve_cores = 0L,

  # Eight additional seeds. Existing discovery seeds 101 and 202 are not rerun by
  # this layer; the document can report 2 discovery seeds + 8 validation seeds.
  validation_seeds = c(303L, 404L, 505L, 606L, 707L, 808L, 909L, 1010L),

  # Focused validation set. The code selects accepted GA wins, borderline or
  # seed-sensitive cases, and benchmark-dominant negative controls. It then caps
  # the total at target_regimes, preferring accepted wins and borderline cases.
  target_regimes = 25L,
  negative_controls_per_family = 3L,
  negative_control_families = c("normal", "exgaussian"),

  # Final validation budget. You can lower these for a smoke test.
  monte_carlo_R = 500L,
  bootstrap_B = 500L,
  sample_sizes_default = c(300L, 500L, 1000L),

  # Additional benchmark set. These are evaluated as external benchmarks; they do
  # not change the GA weight vectors discovered in the original run.
  include_catoni = TRUE,
  include_median_of_means = TRUE,
  include_winsorized = TRUE,
  include_huber_tuned = TRUE,
  include_equal_weight_composite = TRUE,

  # Benchmark tuning grid used only inside the validation layer.
  winsor_probs = c(0.05, 0.10, 0.20),
  mom_blocks = c(5L, 10L, 20L),
  catoni_alpha_grid = c(0.05, 0.10, 0.20, 0.35, 0.50),
  huber_k_grid = c(0.75, 1.00, 1.345, 1.75, 2.00),

  # Criterion for confirmed fixed-weight wins. A fixed GA winner must beat the expanded
  # benchmark gate on both mean MSE and q95 MSE, and its paired bootstrap CI for
  # relative gain should not cross zero if require_positive_ci is TRUE.
  q = 0.95,
  ci_level = 0.95,
  require_positive_ci = FALSE,

  # Locked unseen-regime test. The original discovery run found fixed GA weights.
  # This layer can evaluate each fixed winner in two modes: its selected validation
  # regime and a similar but never-seen regime built from neighboring contamination
  # rate/scale/mechanism values. No GA search is rerun in either mode.
  run_original_regime_validation = TRUE,
  run_locked_unseen_validation = TRUE,
  unseen_conditions_per_regime = 1L,
  full_contamination_rates = c(0.00, 0.01, 0.02, 0.05, 0.10, 0.15, 0.20, 0.30),
  full_outlier_scales = c(1.5, 3, 4.5, 6, 9, 12, 15, 20, 30),
  full_contamination_types = c("upper_tail", "lower_tail", "symmetric_t", "point_mass",
                               "clustered_upper", "clustered_symmetric",
                               "mixture_bimodal_near", "mixture_bimodal_far"),

  # CRN control: all estimators see the same samples within a regime/seed.
  # This reduces Monte Carlo noise in paired comparisons.
  use_common_random_numbers = TRUE,

  # Long-run observability and recovery. Each validation task is checkpointed
  # independently so a crash can resume from the last completed task. Heartbeat
  # messages are written during Monte Carlo loops to show that workers are alive.
  enable_recovery = TRUE,
  overwrite_completed_tasks = FALSE,
  heartbeat_every_replicates = 50L,
  task_checkpoint_format = "rds",
  export_long_squared_errors = TRUE,
  export_task_outputs = TRUE,
  output_formats = c("csv", "parquet"),

  # Safety: this validation layer does not rerun HPF1/HPF2 or mutate discovered
  # GA candidates. It evaluates fixed weights from the discovery output.
  rerun_ga_search = FALSE
)

# Backward-compatible alias for archived scripts that used the original Q1 name.
Q1_CONFIG <- FIXED_WEIGHT_VALIDATION_CONFIG
