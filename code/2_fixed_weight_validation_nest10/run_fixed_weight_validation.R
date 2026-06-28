# =============================================================================
# RUN POST-DISCOVERY FIXED-WEIGHT VALIDATION
# =============================================================================
# Usage from terminal:
#   PROJECT_ROOT=/path/to/original/code \
#   DISCOVERY_ROOT=/path/to/2026_May_25_Finals \
#   VALIDATION_OUTPUT_ROOT=/path/to/FIXED_WEIGHT_VALIDATION_OUTPUT \
#   Rscript run_fixed_weight_validation.R
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

config <- FIXED_WEIGHT_VALIDATION_CONFIG
config$project_root <- q1_norm_path(config$project_root)
config$discovery_root <- q1_norm_path(config$discovery_root)
config$output_root <- q1_make_dir(config$output_root)

q1_msg("PROJECT_ROOT   = ", config$project_root)
q1_msg("DISCOVERY_ROOT = ", config$discovery_root)
q1_msg("OUTPUT_ROOT    = ", config$output_root)
q1_msg("Workers        = ", q1_worker_count(config), " (cap = 5)")
q1_msg("Validation seeds = ", paste(config$validation_seeds, collapse = ", "))

q1_source_original_modules(config$project_root)
selected <- q1_select_regimes(config$discovery_root, config)
q1_msg("Selected ", nrow(selected), " regimes for fixed-weight validation.")

res <- q1_run_validation(selected, config)
ci <- q1_bootstrap_all(res$long, config)
stability <- q1_stability_summary(res$summary, ci, config)
q1_export_tables_figures(res$summary, ci, stability, config)

q1_msg("Post-discovery fixed-weight validation completed.")
q1_msg("Main outputs:")
q1_msg("  - ", file.path(config$output_root, "q1_selected_regimes.csv"))
q1_msg("  - ", file.path(config$output_root, "q1_validation_summary_by_seed.csv"))
q1_msg("  - ", file.path(config$output_root, "q1_bootstrap_ci_by_seed.csv"))
q1_msg("  - ", file.path(config$output_root, "q1_stability_summary.csv"))
