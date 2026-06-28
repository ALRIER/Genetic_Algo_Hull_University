# =============================================================================
# RUN POST-DISCOVERY FIXED-WEIGHT VALIDATION
# =============================================================================
# Usage from terminal:
#   PROJECT_ROOT=/path/to/original/code \
#   DISCOVERY_ROOT=/path/to/2026_May_25_Finals \
#   Rscript run_fixed_weight_validation.R
#
# Ultra-fast functionality check:
#   FIXED_WEIGHT_VALIDATION_SMOKE=1 ... Rscript run_fixed_weight_validation.R
# =============================================================================

args <- commandArgs(trailingOnly = FALSE)
this_file <- sub("^--file=", "", args[grep("^--file=", args)])
this_dir <- if (length(this_file)) dirname(normalizePath(this_file, winslash = "/")) else getwd()

source(file.path(this_dir, "config", "fixed_weight_validation_config.R"))
source(file.path(this_dir, "R", "00_q1_helpers.R"))
source(file.path(this_dir, "R", "01_select_regimes.R"))
source(file.path(this_dir, "R", "02_extra_benchmarks.R"))
source(file.path(this_dir, "R", "03_q1_validation_run.R"))
source(file.path(this_dir, "R", "04_bootstrap_ci.R"))
source(file.path(this_dir, "R", "05_q1_tables_figures.R"))
source(file.path(this_dir, "R", "06_locked_unseen_regimes.R"))

q1_apply_smoke_overrides <- function(config) {
  smoke <- identical(Sys.getenv("FIXED_WEIGHT_VALIDATION_SMOKE", unset = Sys.getenv("Q1_SMOKE", unset = "0")), "1")
  if (!smoke) return(config)
  config$output_root <- paste0(config$output_root, "_SMOKE")
  config$workers <- 1L
  config$validation_seeds <- config$validation_seeds[1]
  config$target_regimes <- 1L
  config$negative_controls_per_family <- 1L
  config$monte_carlo_R <- 2L
  config$bootstrap_B <- 2L
  config$heartbeat_every_replicates <- 1L
  config$export_long_squared_errors <- TRUE
  config$export_task_outputs <- TRUE
  config$overwrite_completed_tasks <- TRUE
  config
}

config <- FIXED_WEIGHT_VALIDATION_CONFIG
config$project_root <- q1_norm_path(config$project_root)
config$discovery_root <- q1_norm_path(config$discovery_root)
config$output_root <- q1_make_dir(config$output_root)
config <- q1_apply_smoke_overrides(config)
config$output_root <- q1_make_dir(config$output_root)
options(q1.output_formats = if ("output_formats" %in% names(config)) config$output_formats else "csv")

q1_msg("PROJECT_ROOT   = ", config$project_root)
q1_msg("DISCOVERY_ROOT = ", config$discovery_root)
q1_msg("OUTPUT_ROOT    = ", config$output_root)
q1_msg("Workers        = ", q1_worker_count(config), " (cap = 5)")
q1_msg("Validation seeds = ", paste(config$validation_seeds, collapse = ", "))
if (identical(Sys.getenv("FIXED_WEIGHT_VALIDATION_SMOKE", unset = Sys.getenv("Q1_SMOKE", unset = "0")), "1")) q1_msg("SMOKE MODE     = enabled")

q1_msg("Loading original project modules.")
q1_source_original_modules(config$project_root)
q1_msg("Selecting fixed-weight validation regimes.")
selected <- q1_select_regimes(config$discovery_root, config)
q1_msg("Selected ", nrow(selected), " regimes for fixed-weight validation.")

q1_msg("Starting/resuming validation stage.")
res <- q1_run_validation(selected, config)
q1_msg("Starting/resuming bootstrap CI stage.")
ci <- if (is.null(res$long)) q1_bootstrap_all_from_tasks(res$tasks, config) else q1_bootstrap_all(res$long, config)
q1_msg("Computing stability summary.")
stability <- q1_stability_summary(res$summary, ci, config)
q1_msg("Exporting final tables, audit files, and figures.")
q1_export_tables_figures(res$summary, ci, stability, config, res = res, selected = selected)

q1_msg("Post-discovery fixed-weight validation completed.")
q1_msg("Main outputs:")
q1_msg("  - ", file.path(config$output_root, "q1_selected_regimes.csv"))
q1_msg("  - ", file.path(config$output_root, "q1_validation_summary_by_seed.csv"))
q1_msg("  - ", file.path(config$output_root, "q1_bootstrap_ci_by_seed.csv"))
q1_msg("  - ", file.path(config$output_root, "q1_stability_summary.csv"))
q1_msg("  - ", file.path(config$output_root, "logs", "q1_task_status.csv"))
q1_msg("  - ", file.path(config$output_root, "checkpoints"))
