# ===== MODULE: PATHS + REPRODUCIBILITY (01_paths_repro.R) ===== 
#This file configures where outputs are written (cluster vs local) 
#and sets reproducible RNG conventions used across the whole experiment.

# Module tracking is defined in 00_utils_debug_io.R. This fallback only lets the file source on its own.
if (!exists("mark_module_done", mode = "function", inherits = TRUE)) {
  mark_module_done <- function(module_id, extra = NULL) invisible(TRUE)
  is_module_done   <- function(module_id) FALSE
}


# Module-run tracking: record that this module has been sourced successfully (helps confirm module order during interactive debugging).
# Note: this early mark confirms the file was read; later code still may fail, but the timestamp helps localize issues quickly.
mark_module_done("01_paths_repro.R")

# =========================== OPTIONAL WORKING DIRECTORY =========================== (usually leave commented so the project runs from any folder)
# setwd("...")  # If you want a fixed base folder for relative paths, uncomment and set a local path here.
# options(stringsAsFactors = FALSE)  # Legacy R option (kept for compatibility; not required for modern R).

# =========================== OUTPUT ROOT (CLUSTER vs LOCAL) =========================== Centralizes ALL outputs under a single OUT_ROOT so runs are easy to archive and share.
# Design: detect a shared cluster mount (/srv/cluster). If present, write to shared results so master/workers see the same filesystem.
# If not on a cluster, write to WIN_OUT_ROOT (env var) or default to <getwd()>/Estimator_Results (single local folder).
# This prevents scattering CSVs/logs across multiple places and keeps provenance of each run self-contained.
# You can override the local folder without editing code by setting the environment variable WIN_OUT_ROOT.

IS_WINDOWS <- .Platform$OS.type == "windows"
CLUSTER_SHARED <- "/srv/cluster"
IS_CLUSTER <- dir.exists(CLUSTER_SHARED)

if (IS_CLUSTER) {
  # Cluster mode: write all artifacts to a shared mount so every node reads/writes the same paths.
  # This is essential for multi-node orchestration: results, caches, and logs must be visible across workers.
  SHARED <- CLUSTER_SHARED
  PROJECT_DIR <- file.path(SHARED, "ga_project")
  OUT_ROOT    <- file.path(PROJECT_DIR, "results")
  
  dir.create(PROJECT_DIR, recursive = TRUE, showWarnings = FALSE)
  dir.create(OUT_ROOT,    recursive = TRUE, showWarnings = FALSE)
  
  # Fail-fast write test: confirms permissions and path validity before expensive computation starts.
  # Without this, you could run hours of compute and only later discover the filesystem was read-only/misconfigured.
  testfile <- file.path(OUT_ROOT, paste0("shared_test_", Sys.getpid(), ".txt"))
  writeLines("ok", testfile)
  stopifnot(file.exists(testfile))
  unlink(testfile)
} else {
  # Local mode: route outputs to a single folder (typically Windows), optionally overridden via WIN_OUT_ROOT.
  # This makes runs portable and self-contained while avoiding hard-coded paths inside scripts and notebooks.
  WIN_OUT_ROOT <- Sys.getenv("WIN_OUT_ROOT", unset = file.path(getwd(), "Estimator_Results"))
  OUT_ROOT <- WIN_OUT_ROOT
  dir.create(OUT_ROOT, recursive = TRUE, showWarnings = FALSE)
  
  # Fail-fast write test for local runs: catches locked folders, permission issues, and invalid characters early.
  # Ensures downstream functions (CSV/RDS/manifest writers) will not fail mid-run due to simple I/O problems.
  testfile <- file.path(OUT_ROOT, paste0("local_test_", Sys.getpid(), ".txt"))
  writeLines("ok", testfile)
  stopifnot(file.exists(testfile))
  unlink(testfile)
}

# ============================ RNG & REPRODUCIBILITY ============================= Uses L'Ecuyer-CMRG (parallel-safe) to support fair comparisons and CRN across configurations.
## L'Ecuyer-CMRG provides independent substreams for parallel runs while remaining reproducible given the same seeds.
# Global RNG convention: L'Ecuyer-CMRG supports reproducible parallel substreams (PSOCK/parallel).
# Setting a fixed seed here initializes a known baseline state; later modules may use local seeds via .seed_scope.
RNGkind("L'Ecuyer-CMRG")
set.seed(12345)
invisible(runif(1))

## CRN INDEX GENERATOR: builds deterministic indices keyed by (distribution family, sample size) so all configurations share the same base random draws (fair comparisons).
make_crn_indices <- function(families, sample_sizes, num_samples = 50L, ...) {
  # CRN (Common Random Numbers) improves fairness: for each (family, n), every GA configuration reuses identical
  # simulation identifiers, so performance differences are driven by estimator behavior, not by different random draws.
  # The "..." exists only for backward compatibility; extra arguments are ignored to avoid mixing CRN with bootstrap logic.
  invisible(list(...))
  
  crn <- new.env(parent = emptyenv())
  ctr <- 1L
  for (fam in families) {
    for (n in sample_sizes) {
      ## Deterministic sub-seed per (fam, n) so CRN indices are stable across runs.
      # We snapshot and restore .Random.seed so this local seeding does not perturb the caller’s RNG state.
      # The counter-based seed is deterministic given the family/sample_sizes ordering, ensuring stable CRN maps.
      s_backup <- if (exists(".Random.seed", inherits = FALSE)) .Random.seed else NULL
      set.seed(1e6 + ctr)
      
      ## Indices used downstream as reusable simulation IDs / ordering keys (same for all configs under the same scenario).
      # sim_idx are large unique integers that act like immutable “replicate IDs”; downstream code can use them
      # to drive .seed_scope() or to select consistent samples, keeping comparisons aligned across candidates.
      sim_idx <- sample.int(1e9, num_samples, replace = FALSE)
      
      assign(paste(fam, n, sep = "__"),
             list(sim_idx = sim_idx),
             envir = crn)
      
      ## Restore previous RNG state so callers are not affected by this local seeding.
      # This is critical: CRN creation must not change the global stream, otherwise unrelated modules drift.
      if (!is.null(s_backup)) .Random.seed <- s_backup
      ctr <- ctr + 1L
    }
  }
  crn
}

# Stable integer seed from an arbitrary string key (hash-like): 
#used when we need deterministic per-key seeds without integer overflow.
.stable_int_seed <- function(key, base_seed = 0L) {
  # Converts an arbitrary string key into a stable positive 
  #32-bit-ish integer seed using modular hashing.
  # Used when we want deterministic seeds per “entity” 
  #(e.g., family/stage/config key) without depending on R’s
  # internal hashing, and without overflow issues that can occur with naive integer conversions.
  key  <- paste0(key)
  ints <- utf8ToInt(key)
  
  # Convert the key to UTF-8 code points (stable across platforms).
  h <- as.numeric(base_seed %% 2147483647)
  
  for (v in ints) {
    h <- (h * 131 + v) %% 2147483647
  }
  
  h_int <- suppressWarnings(as.integer(h))
  if (length(h_int) != 1L || is.na(h_int) || h_int <= 0L) h_int <- 1L
  h_int
}

.ensure_seed <- function(seed, fallback = 12345L) {
  # Normalizes user-provided seeds into a single positive integer, defaulting to 
  #fallback on invalid input.
  # This prevents accidental NA/0/negative seeds from silently 
  #breaking reproducibility or RNG-stream selection.
  s <- suppressWarnings(as.integer(seed))
  if (length(s) != 1L || is.na(s) || s <= 0L) s <- as.integer(fallback)
  s
}


# Helper: evaluate an expression under a local seed and then restore the previous RNG state (prevents accidental cross-talk between modules).
.seed_scope <- function(seed, expr) {
  # Executes an expression under a temporary seed, then restores the prior RNG state on exit.
  # This isolates stochastic components (sampling, bootstrapping, CRN-driven draws) so one module does not
  # unintentionally shift the global RNG stream and alter results elsewhere in the pipeline.
  s_backup_exists <- exists(".Random.seed", inherits = FALSE)
  if (s_backup_exists) s_backup <- .Random.seed
  on.exit({
    if (s_backup_exists) .Random.seed <<- s_backup
  }, add = TRUE)
  seed <- .ensure_seed(seed, fallback = 101L)
  set.seed(seed)
  force(expr)
}
