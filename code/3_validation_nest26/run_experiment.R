# =============================================================================
# run_experiment.R
# =============================================================================
# Single-entry launcher for the full pipeline.
# Run this file from a clean R session or terminal.
#
# Usage:
#   # Default structural preflight:
#   source("run_experiment.R")
#
#   # Full single-machine run:
#   Sys.setenv(RUN_MODE = "full")
#   source("run_experiment.R")
#
#   # Full cluster run (Linux only):
#   Sys.setenv(RUN_MODE      = "full",
#              CLUSTER_HOSTS = "192.168.1.2,192.168.1.3",
#              SCRIPT_PATH   = "/nfs/project/08_LocationEstimators.R")
#   source("run_experiment.R")
#
# RUN_MODE values:
#   "micro"        → preflight only: loads modules and validates core helpers, no GA
#   "two_min_smoke" → ultra-fast structural pipeline smoke, skips diagnostics
#   "quick"         → pipeline smoke test: 1 family, small budget
#   "smoke_results" → viability check: normal + exgaussian, medium budget
#                     validates GA convergence and result
#                     quality BEFORE committing to the full run
#   "full"          → full experiment: all 6 families, production budget
#                     ~48-60h single machine | ~18-20h on 3-node cluster
#   "none"          → load definitions only, do not run
#
# Module load order:
#   00–07  : definitions (utils, paths, scenarios, distributions,
#             estimators, GA core, data prep, fitness)
#   10     : distributional diagnostics (pre-GA, needs only param_grids)
#   08     : main experiment (GA, HP search, outputs)
# =============================================================================

# ── 0. Set default RUN_MODE if not already set ────────────────────────────────
if (!nzchar(Sys.getenv("RUN_MODE"))) {
  Sys.setenv(RUN_MODE = "micro")
}

cat(sprintf("\n[LAUNCHER] RUN_MODE = %s\n", Sys.getenv("RUN_MODE")))
cat(sprintf("[LAUNCHER] Working dir = %s\n\n", getwd()))

# ── 1. Resolve project root ───────────────────────────────────────────────────
# Honours PROJECT_ROOT env var if set, otherwise uses working directory.
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

# ── 2. Load definition modules (00–07) in dependency order ───────────────────
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

# ── 3. Micro preflight short-circuit ─────────────────────────────────────────
# Micro mode is a structural preflight only. It loads module 08, runs the micro
# checks defined there, and skips diagnostics, GA, and transfer analysis.
if (tolower(Sys.getenv("RUN_MODE")) == "micro") {
  cat("[LAUNCHER] Micro mode: skipping diagnostics and GA. Loading module 08 only.\n\n")
  .src("08_LocationEstimators.R")
} else if (tolower(Sys.getenv("RUN_MODE")) == "two_min_smoke") {
  cat("[LAUNCHER] two_min_smoke: skipping distributional diagnostics. Loading module 08 only.\n\n")
  .src("08_LocationEstimators.R")
} else {

  # ── 4. Run distributional diagnostics (pre-GA) ─────────────────────────────
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

  # ── 5. Configure smoke_results mode if selected ────────────────────────────
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

  # ── 6. Run main experiment (module 08) ─────────────────────────────────────
  cat("[LAUNCHER] Starting main experiment (module 08)...\n\n")
  .src("08_LocationEstimators.R")

  # ── 7. Legacy transfer disabled ────────────────────────────────────────────
  # cross-family transfer is not part of the active pipeline.

}

# ── 6. Done ───────────────────────────────────────────────────────────────────
cat(sprintf("\n[LAUNCHER] Pipeline complete. RUN_MODE=%s\n", Sys.getenv("RUN_MODE")))
cat(sprintf("[LAUNCHER] Outputs in: %s\n\n", OUT_ROOT))
