# =============================================================================
# run_experiment.R
# =============================================================================
# Entry point for the Phase 1 discovery pipeline.
# Run from a clean R session or terminal with this folder as PROJECT_ROOT.
#
# Examples:
#   source("run_experiment.R")
#
#   Sys.setenv(RUN_MODE = "full")
#   source("run_experiment.R")
#
# Available modes:
#   micro         - load modules and run structural checks without the GA
#   quick         - run a small one-family pipeline check
#   smoke_results - run a two-family viability check with a reduced budget
#   full          - run the complete six-family experiment
#   none          - load definitions without starting an experiment
#
# Modules 00-07 define shared functions and objects. Module 10 runs
# distributional diagnostics, and module 08 runs the main experiment.
# =============================================================================

# Use the structural preflight unless the caller selects another mode.
if (!nzchar(Sys.getenv("RUN_MODE"))) {
  Sys.setenv(RUN_MODE = "micro")
}

cat(sprintf("\n[LAUNCHER] RUN_MODE = %s\n", Sys.getenv("RUN_MODE")))
cat(sprintf("[LAUNCHER] Working dir = %s\n\n", getwd()))

# Resolve the experiment folder from PROJECT_ROOT when provided.
this_dir <- normalizePath(
  Sys.getenv("PROJECT_ROOT", unset = getwd()),
  winslash = "/", mustWork = FALSE
)

.src <- function(filename) {
  path <- file.path(this_dir, filename)
  if (!file.exists(path))
    stop(sprintf("[LAUNCHER] File not found: %s\n  Set PROJECT_ROOT to the project folder.", path),
         call. = FALSE)
  cat(sprintf("[LAUNCHER] Loading %s ...\n", filename))
  source(path, local = FALSE)
  invisible(NULL)
}

# Load definition modules in dependency order.
definition_modules <- c(
  "00_utils_debug_io.R",
  "01_paths_repro.R",
  "02_scenarios_sampling.R",
  "03_distributions_params.R",
  "04_estimators_registry.R",
  "05_ga_core.R",
  "06_data_prep.R",
  "07_fitness_objectives.R"
)

for (mod in definition_modules) {
  .src(mod)
}

cat("\n[LAUNCHER] All definition modules loaded.\n\n")

# Micro mode checks the pipeline structure without diagnostics or GA execution.
if (tolower(Sys.getenv("RUN_MODE")) == "micro") {
  cat("[LAUNCHER] Micro mode: skipping diagnostics and GA. Loading module 08 only.\n\n")
  .src("08_LocationEstimators.R")
} else {

  # Run distributional checks before the GA experiment.
  cat("[LAUNCHER] Running distributional diagnostics (module 10)...\n")
  .src("10_distributional_diagnostics.R")

  diag_out <- tryCatch(
    run_distributional_diagnostics(
      param_grids = param_grids,
      families    = names(param_grids),
      run_dir     = OUT_ROOT,
      N_per_row   = 5000L
    ),
    error = function(e) {
      cat(sprintf("[LAUNCHER] WARNING: distributional diagnostics failed: %s\n",
                  conditionMessage(e)))
      NULL
    }
  )
  cat("[LAUNCHER] Distributional diagnostics complete.\n\n")

  # smoke_results uses a reduced two-family budget before the full experiment.
  if (Sys.getenv("RUN_MODE") == "smoke_results") {
    cat("[LAUNCHER] smoke_results mode: setting 2-family viability budget...\n")
    Sys.setenv(SMOKE_FAMILIES  = "normal,exgaussian",
               SMOKE_R         = "25",
               SMOKE_G         = "20",
               SMOKE_B         = "60",
               SMOKE_HP2_K     = "4",
               SMOKE_SCEN_FRAC = "0.5")
    Sys.setenv(RUN_MODE = "full")
    cat("[LAUNCHER] Smoke families: normal + exgaussian\n")
  }

  # Load module 08 to start the selected experiment mode.
  cat("[LAUNCHER] Starting main experiment (module 08)...\n\n")
  .src("08_LocationEstimators.R")

}

cat(sprintf("\n[LAUNCHER] Pipeline complete. RUN_MODE=%s\n", Sys.getenv("RUN_MODE")))
cat(sprintf("[LAUNCHER] Outputs in: %s\n\n", OUT_ROOT))
