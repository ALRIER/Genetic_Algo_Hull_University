#!/usr/bin/env Rscript
# =============================================================================
# random_dirichlet_gate_audit.R
# -----------------------------------------------------------------------------
# Methodological annex: random-search audit for NEST26 abstention decisions.
#
# This script is meant to be run from the final results bundle, not from a toy
# example. By default it reads:
#   - the final NEST26 evidence taxonomy (stage 05), which says which regimes
#     ended as benchmark_retained, marginal_ga_win, or negative controls;
#   - the expanded fixed-weight validation table (stage 04), which contains the
#     exact contamination cells, sample sizes, and GA weight vectors; and
#   - the archived NEST26 code, so the estimator basis, benchmarks, simulator,
#     contamination mechanism, and validation gate remain the same as the
#     experiment that produced the reported tables.
#
# Scientific question:
#   For each benchmark-retained regime, is the abstention robust to an
#   independent search over the same 26-component convex estimator space? If many
#   random Dirichlet composites are tried and none passes the original dual gate
#   across the validation seeds and validation modes, the abstention is stronger
#   evidence than a single GA non-win. If a random composite does pass, that
#   regime/mode deserves follow-up because the composite space may contain signal
#   missed by the selected GA candidate.
#
# The audit does not rerun the GA and does not alter discovered weights. It
# precomputes the 26 estimator components once per simulated sample, then scores
# Dirichlet weight draws by matrix multiplication. A draw must beat the best
# admissible benchmark on both mean MSE and q95 MSE for every validation seed.
#
# Typical use from Final_Results_15June2026:
#   Rscript random_dirichlet_gate_audit.R --draws 4000 --R 500
#
# Useful smoke test before a long run:
#   Rscript random_dirichlet_gate_audit.R --draws 25 --R 5 --seeds 303
#
# CLI flags:
#   --root PATH                  final results bundle root
#   --project-root PATH          folder with archived 00_*..07_* NEST26 modules
#   --validation-code-root PATH  folder with archived Q1 validation code
#   --taxonomy-csv PATH          stage-05 evidence_taxonomy_all_candidates.csv
#   --selected-regimes-csv PATH  stage-04 q1_selected_regimes.csv
#   --output-root PATH           output directory
#   --modes "original_regime,locked_unseen_similar"
#   --draws N, --R N, --alpha A, --q Q, --seeds "303,404,..."
#   --include-neg-controls       include the six negative-control successes
#   --grades "benchmark_retained" override audited taxonomy decisions
# =============================================================================

invisible(NULL)

script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(f)) normalizePath(f[1], winslash = "/", mustWork = FALSE) else NA_character_
}

default_bundle_root <- function() {
  env <- Sys.getenv("AUDIT_ROOT", unset = "")
  if (nzchar(env)) return(normalizePath(env, winslash = "/", mustWork = FALSE))
  sp <- script_path()
  start <- if (!is.na(sp)) dirname(sp) else getwd()
  here <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    has_bundle_markers <- dir.exists(file.path(here, "05_EVIDENCE_TAXONOMY_NEST26")) &&
      dir.exists(file.path(here, "07_CODE_ARCHIVE"))
    if (has_bundle_markers) return(here)
    parent <- dirname(here)
    if (identical(parent, here)) break
    here <- parent
  }
  normalizePath(start, winslash = "/", mustWork = FALSE)
}

ROOT0 <- default_bundle_root()

# ----------------------------- configuration ---------------------------------
CFG <- list(
  bundle_root          = ROOT0,
  project_root         = Sys.getenv("PROJECT_ROOT", unset = file.path(ROOT0, "07_CODE_ARCHIVE", "expanded_and_realworld_experiment_code")),
  validation_code_root = Sys.getenv("VALIDATION_CODE_ROOT", unset = Sys.getenv("VALIDATION_ROOT", unset = file.path(ROOT0, "07_CODE_ARCHIVE", "original_fixed_weight_validation_code"))),
  taxonomy_csv         = Sys.getenv("TAXONOMY_CSV", unset = file.path(ROOT0, "05_EVIDENCE_TAXONOMY_NEST26", "evidence_results_20260611", "tables", "evidence_taxonomy_all_candidates.csv")),
  selected_regimes_csv = Sys.getenv("SELECTED_REGIMES_CSV", unset = file.path(ROOT0, "04_EXPANDED_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST26", "raw_full_run_output_with_tasks_checkpoints_20260611", "q1_selected_regimes.csv")),
  output_root          = Sys.getenv("OUTPUT_ROOT", unset = file.path(ROOT0, "08_RANDOM_DIRICHLET_ABSTAIN_AUDIT")),
  n_draws              = 4000L,
  R                    = 500L,
  q                    = 0.95,
  dirichlet_alpha      = 0.3,
  validation_seeds     = c(303L,404L,505L,606L,707L,808L,909L,1010L),
  audit_modes          = c("original_regime", "locked_unseen_similar"),
  draw_seed            = 20260626L,
  abstain_decisions    = "benchmark_retained",
  include_neg_controls = FALSE,
  draw_block           = 500L,
  out_csv              = NA_character_
)

parse_args <- function(cfg) {
  a <- commandArgs(trailingOnly = TRUE); i <- 1L
  getv <- function(i) if (i + 1L <= length(a)) a[i + 1L] else NA
  while (i <= length(a)) {
    key <- sub("^--", "", a[i])
    switch(key,
      root    = {
        cfg$bundle_root <- normalizePath(getv(i), winslash = "/", mustWork = FALSE)
        cfg$project_root <- file.path(cfg$bundle_root, "07_CODE_ARCHIVE", "expanded_and_realworld_experiment_code")
        cfg$validation_code_root <- file.path(cfg$bundle_root, "07_CODE_ARCHIVE", "original_fixed_weight_validation_code")
        cfg$taxonomy_csv <- file.path(cfg$bundle_root, "05_EVIDENCE_TAXONOMY_NEST26", "evidence_results_20260611", "tables", "evidence_taxonomy_all_candidates.csv")
        cfg$selected_regimes_csv <- file.path(cfg$bundle_root, "04_EXPANDED_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST26", "raw_full_run_output_with_tasks_checkpoints_20260611", "q1_selected_regimes.csv")
        cfg$output_root <- file.path(cfg$bundle_root, "08_RANDOM_DIRICHLET_ABSTAIN_AUDIT")
        i <- i + 2L
      },
      "project-root" = { cfg$project_root <- getv(i); i <- i + 2L },
      "validation-code-root" = { cfg$validation_code_root <- getv(i); i <- i + 2L },
      "taxonomy-csv" = { cfg$taxonomy_csv <- getv(i); i <- i + 2L },
      "selected-regimes-csv" = { cfg$selected_regimes_csv <- getv(i); i <- i + 2L },
      "output-root" = { cfg$output_root <- getv(i); i <- i + 2L },
      draws   = { cfg$n_draws <- as.integer(getv(i)); i <- i + 2L },
      R       = { cfg$R <- as.integer(getv(i)); i <- i + 2L },
      alpha   = { cfg$dirichlet_alpha <- as.numeric(getv(i)); i <- i + 2L },
      q       = { cfg$q <- as.numeric(getv(i)); i <- i + 2L },
      seeds   = { cfg$validation_seeds <- as.integer(strsplit(getv(i), ",")[[1]]); i <- i + 2L },
      modes   = { cfg$audit_modes <- strsplit(getv(i), ",")[[1]]; i <- i + 2L },
      "draw-seed" = { cfg$draw_seed <- as.integer(getv(i)); i <- i + 2L },
      "draw-block" = { cfg$draw_block <- as.integer(getv(i)); i <- i + 2L },
      out     = { cfg$out_csv <- getv(i); i <- i + 2L },
      grades  = { cfg$abstain_decisions <- strsplit(getv(i), ",")[[1]]; i <- i + 2L },
      "include-neg-controls" = { cfg$include_neg_controls <- TRUE; i <- i + 1L },
      { i <- i + 1L }
    )
  }
  cfg
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || (length(a) == 1 && is.na(a))) b else a

die <- function(...) { message("[FATAL] ", ...); quit(status = 1L, save = "no") }

# =============================================================================
# 1. Load the experiment code used by the reported NEST26 results
# =============================================================================
load_project <- function(cfg) {
  if (!nzchar(cfg$validation_code_root)) die("VALIDATION_CODE_ROOT not set.")
  if (!nzchar(cfg$project_root))         die("PROJECT_ROOT not set.")
  if (!dir.exists(cfg$validation_code_root))
    die("Validation code folder not found: ", cfg$validation_code_root)
  if (!dir.exists(cfg$project_root))
    die("Project code folder not found: ", cfg$project_root)

  config_file <- file.path(cfg$validation_code_root, "config", "q1_validation_config.R")
  rdir <- file.path(cfg$validation_code_root, "R")
  vfiles <- file.path(rdir, c(
    "00_q1_helpers.R",
    "02_extra_benchmarks.R",
    "03_q1_validation_run.R",
    "06_locked_unseen_regimes.R"
  ))
  missing <- c(config_file, vfiles)[!file.exists(c(config_file, vfiles))]
  if (length(missing))
    die("Validation code is incomplete. Missing: ", paste(missing, collapse = ", "))

  old <- getwd(); on.exit(setwd(old), add = TRUE)
  setwd(cfg$validation_code_root)
  source(config_file, chdir = TRUE)
  for (f in vfiles) source(f, chdir = TRUE)

  if (!exists("q1_source_original_modules", mode = "function"))
    die("q1_source_original_modules() was not loaded from validation helpers.")
  q1_source_original_modules(cfg$project_root)

  Q1_CONFIG$project_root <- normalizePath(cfg$project_root, winslash = "/", mustWork = FALSE)
  Q1_CONFIG$output_root <- normalizePath(cfg$output_root, winslash = "/", mustWork = FALSE)
  Q1_CONFIG$monte_carlo_R <- cfg$R
  Q1_CONFIG$q <- cfg$q
  Q1_CONFIG$validation_seeds <- cfg$validation_seeds

  # These symbols are the contract between this annex and the original pipeline.
  # If any are missing, the audit would be silently drifting away from the
  # experiment and should stop before producing interpretable numbers.
  need <- c("generate_population", "analytic_mean_from_params", "param_grids",
            "q1_inject_from_summary", "ESTIMATOR_REGISTRY", "ESTIMATOR_NAMES",
            "N_EST", "custom_estimator", "admissible_estimator_mask",
            "q1_expanded_benchmarks", "q1_conditions_for_mode",
            "q1_parse_semicolon_values")
  miss <- need[!vapply(need, function(s) exists(s, inherits = TRUE), logical(1))]
  if (length(miss)) die("Missing project symbols after sourcing: ", paste(miss, collapse = ", "))
  if (!identical(as.integer(N_EST), 26L))
    die("This final-results audit expects the NEST26 estimator basis, but N_EST = ", N_EST,
        ". Check PROJECT_ROOT.")
  invisible(TRUE)
}

# =============================================================================
# 2. Select the final-stage regimes to audit
# =============================================================================
read_csv0 <- function(p) {
  if (!nzchar(p) || !file.exists(p)) die("CSV not found: ", p)
  utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
}

select_abstain_regimes <- function(cfg) {
  tax <- read_csv0(cfg$taxonomy_csv)
  sel <- read_csv0(cfg$selected_regimes_csv)

  if (!"legacy_final_decision" %in% names(tax))
    die("TAXONOMY_CSV has no 'legacy_final_decision' column; is this the stage-05 taxonomy?")
  key <- if ("validation_id" %in% names(tax) && "validation_id" %in% names(sel)) "validation_id"
         else die("Cannot join taxonomy<->selected_regimes: both need 'validation_id'.")

  keep_dec <- cfg$abstain_decisions
  if (isTRUE(cfg$include_neg_controls)) keep_dec <- unique(c(keep_dec, "negative_control_success"))
  abstain_ids <- tax[[key]][tax$legacy_final_decision %in% keep_dec]
  if (!length(abstain_ids)) die("No regimes matched abstain decisions: ", paste(keep_dec, collapse = ", "))

  rows <- sel[sel[[key]] %in% abstain_ids, , drop = FALSE]
  g <- tax[match(rows[[key]], tax[[key]]),
           c("legacy_final_decision", "evidence_grade"), drop = FALSE]
  rows$legacy_final_decision <- g$legacy_final_decision
  rows$evidence_grade        <- g$evidence_grade
  for (cc in c("exact_contamination_rates","exact_outlier_scales",
               "exact_contamination_types","exact_sample_sizes"))
    if (!cc %in% names(rows)) die("selected_regimes missing required column: ", cc)
  rows <- do.call(rbind, lapply(cfg$audit_modes, function(mode) {
    z <- rows
    z$validation_mode <- trimws(mode)
    z
  }))
  message(sprintf("[select] %d selected regimes (decisions: %s)%s",
                  length(unique(rows[[key]])), paste(keep_dec, collapse = "+"),
                  if (isTRUE(cfg$include_neg_controls)) " [incl. negative controls]" else ""))
  message(sprintf("[select] audit modes: %s | total regime-mode tasks: %d",
                  paste(unique(rows$validation_mode), collapse = ", "), nrow(rows)))
  rows
}

# =============================================================================
# 3. Precompute one regime/seed under the original validation design
# =============================================================================
# Each simulated sample is shared by all candidate estimators and benchmarks.
# We store the 26 estimator components in E, then later score thousands of
# Dirichlet composites as E %*% w. This preserves common random numbers and
# avoids re-evaluating robust estimators for every random draw.
precompute_regime_seed <- function(row, seed, cfg) {
  set.seed(as.integer(seed))
  distribution <- tolower(as.character(row$distribution[1]))
  param_grid <- param_grids[[distribution]]
  if (is.null(param_grid)) die("No param_grid for distribution: ", distribution)

  true_mu <- q1_true_mean_for_family(distribution, param_grid)

  mode <- if ("validation_mode" %in% names(row)) as.character(row$validation_mode[1]) else "original_regime"
  conditions <- q1_conditions_for_mode(row, Q1_CONFIG, validation_mode = mode)
  if (!nrow(conditions)) die("No conditions expanded for regime ", row$validation_id)

  reg_names <- ESTIMATOR_NAMES
  Nrep <- nrow(conditions) * cfg$R
  E <- matrix(NA_real_, nrow = Nrep, ncol = N_EST, dimnames = list(NULL, reg_names))
  bench_list <- vector("list", Nrep)
  rr <- 0L
  for (cc in seq_len(nrow(conditions))) {
    cond  <- conditions[cc, , drop = FALSE]
    n     <- as.integer(cond$sample_size)
    rate  <- as.numeric(cond$contamination_rate)
    scale <- as.numeric(cond$outlier_scale)
    type  <- as.character(cond$contamination_type)
    for (r in seq_len(cfg$R)) {
      i <- sample.int(nrow(param_grid), 1L)
      x <- generate_population(distribution, n, as.list(param_grid[i, , drop = FALSE]))
      if (rate > 0 && !identical(type, "none"))
        x <- q1_inject_from_summary(x, rate = rate, scale = scale, type = type)
      comps <- vapply(ESTIMATOR_REGISTRY, function(f) {
        v <- tryCatch(f(x), error = function(e) NA_real_)
        if (is.finite(v)) v else stats::median(x, na.rm = TRUE)
      }, numeric(1))
      bench <- q1_expanded_benchmarks(x, Q1_CONFIG, distribution = distribution)
      rr <- rr + 1L
      E[rr, ] <- comps
      bench_list[[rr]] <- bench
    }
  }
  # Some benchmarks are support-dependent, so the matrix is aligned on the union
  # of benchmark names and missing entries are ignored estimator-wise.
  bn <- unique(unlist(lapply(bench_list, names)))
  B  <- matrix(NA_real_, nrow = Nrep, ncol = length(bn), dimnames = list(NULL, bn))
  for (k in seq_len(Nrep)) { v <- bench_list[[k]]; B[k, names(v)] <- as.numeric(v) }

  be_mean <- apply(B, 2, function(col) mean((col - true_mu)^2, na.rm = TRUE))
  be_q95  <- apply(B, 2, function(col)
    as.numeric(stats::quantile((col - true_mu)^2, cfg$q, na.rm = TRUE, names = FALSE, type = 8)))
  mask <- admissible_estimator_mask(distribution = distribution, target = "arithmetic_mean")

  list(E = E, mu = true_mu, mask = mask,
       best_bench_mean = min(be_mean, na.rm = TRUE),
       best_bench_q95  = min(be_q95,  na.rm = TRUE))
}

# Apply the same support/target admissibility rule used by custom_estimator().
# Dirichlet draws already sum to one; after masking unsupported components, the
# remaining allowed weights are renormalized.
adjust_weights <- function(W, mask) {
  Wm <- W
  if (any(!mask)) Wm[, !mask] <- 0
  rs <- rowSums(Wm)
  bad <- !is.finite(rs) | rs <= 0
  if (any(bad)) {
    allowed <- which(mask); if (!length(allowed)) allowed <- seq_len(ncol(W))
    u <- numeric(ncol(W)); u[allowed] <- 1 / length(allowed)
    Wm[bad, ] <- matrix(u, nrow = sum(bad), ncol = ncol(W), byrow = TRUE)
    rs[bad] <- 1
  }
  Wm / rs
}

# Recover the fixed GA vector reported by the experiment. The selected-regimes
# CSV stores both semicolon vectors and w_<estimator> columns; this helper accepts
# either form but always returns a length-26 normalized vector.
ga_weight_for_row <- function(row) {
  w <- q1_parse_semicolon_values(row$ga_weight_vector %||% "", numeric = TRUE)
  if (length(w) != N_EST) {
    wcols <- paste0("w_", ESTIMATOR_NAMES)
    if (all(wcols %in% names(row))) {
      w <- suppressWarnings(as.numeric(row[wcols]))
    } else {
      return(NULL)
    }
  }
  w[!is.finite(w)] <- 0
  if (sum(w) <= 0) return(NULL)
  w / sum(w)
}

# The sanity oracle scores the experiment's own fixed GA vector through this
# audit path. Transfer specialists should pass most seeds; abstained regimes
# should not. If that pattern fails, the audit is probably pointed at the wrong
# code/results pair and the random-search verdicts should not be interpreted.
oracle_seeds_passed <- function(row, w, cfg) {
  if (is.null(w)) return(NA_integer_)
  Wm <- matrix(w, nrow = 1)
  passed <- 0L
  for (seed in cfg$validation_seeds) {
    pc <- precompute_regime_seed(row, seed, cfg)
    if (score_draws(pc, Wm, cfg)[1]) passed <- passed + 1L
  }
  passed
}

# Score all Dirichlet draws against one precomputed seed. The block loop keeps
# memory bounded when draws or Monte Carlo replicates are large.
score_draws <- function(pc, W, cfg) {
  Wadj <- adjust_weights(W, pc$mask)
  nD <- nrow(Wadj); pass <- logical(nD)
  blk <- cfg$draw_block
  s <- 1L
  while (s <= nD) {
    e <- min(s + blk - 1L, nD)
    C <- pc$E %*% t(Wadj[s:e, , drop = FALSE])         # Nrep x block
    err2 <- (C - pc$mu)^2
    mean_mse <- colMeans(err2, na.rm = TRUE)
    q95_mse  <- apply(err2, 2, function(col)
      as.numeric(stats::quantile(col, cfg$q, na.rm = TRUE, names = FALSE, type = 8)))
    pass[s:e] <- is.finite(mean_mse) & is.finite(q95_mse) &
                 (mean_mse < pc$best_bench_mean) & (q95_mse < pc$best_bench_q95)
    s <- e + 1L
  }
  pass
}

# =============================================================================
# 4. Audit one regime with the seed-stable dual gate
# =============================================================================
audit_regime <- function(row, cfg) {
  vid <- as.character(row$validation_id)
  mode <- if ("validation_mode" %in% names(row)) as.character(row$validation_mode[1]) else "original_regime"
  message(sprintf("\n[regime %s | %s] dist=%s grade=%s  rates={%s} scales={%s} types={%s} n={%s}",
                  vid, mode, row$distribution, row$evidence_grade %||% "?",
                  row$exact_contamination_rates, row$exact_outlier_scales,
                  row$exact_contamination_types, row$exact_sample_sizes))

  set.seed(cfg$draw_seed)
  g <- matrix(stats::rgamma(cfg$n_draws * N_EST, shape = cfg$dirichlet_alpha),
              nrow = cfg$n_draws, ncol = N_EST)
  W <- g / rowSums(g)
  ga_w <- ga_weight_for_row(row)

  per_seed <- integer(length(cfg$validation_seeds))
  stable   <- rep(TRUE, cfg$n_draws)
  ga_seeds_passed <- if (is.null(ga_w)) NA_integer_ else 0L
  for (si in seq_along(cfg$validation_seeds)) {
    seed <- cfg$validation_seeds[si]
    pc <- precompute_regime_seed(row, seed, cfg)
    p  <- score_draws(pc, W, cfg)
    per_seed[si] <- sum(p, na.rm = TRUE)
    stable <- stable & p
    if (!is.null(ga_w))
      ga_seeds_passed <- ga_seeds_passed + as.integer(score_draws(pc, matrix(ga_w, nrow = 1), cfg)[1])
    message(sprintf("   seed %4d : %5d / %d random draws win both axes",
                    seed, per_seed[si], cfg$n_draws))
  }
  n_stable <- sum(stable, na.rm = TRUE)
  verdict <- if (n_stable == 0L)
    "ABSTAIN CONFIRMED (random search also fails the gate at all seeds)"
  else
    "RANDOM SEARCH PASSES (composite signal exists; revisit this abstain)"
  message(sprintf("   GA-winner oracle: passes %s/%d seeds | random seed-stable: %d/%d | %s",
                  ifelse(is.na(ga_seeds_passed), "NA", ga_seeds_passed),
                  length(cfg$validation_seeds), n_stable, cfg$n_draws, verdict))

  data.frame(
    validation_id = vid, distribution = row$distribution,
    validation_mode = row$validation_mode %||% NA_character_,
    evidence_grade = row$evidence_grade %||% NA_character_,
    legacy_final_decision = row$legacy_final_decision %||% NA_character_,
    n_draws = cfg$n_draws, n_seeds = length(cfg$validation_seeds),
    ga_winner_seeds_passed = ga_seeds_passed,
    median_per_seed_passes = stats::median(per_seed),
    max_per_seed_passes = max(per_seed),
    n_seed_stable_passes = n_stable,
    seed_stable_pass_rate = n_stable / cfg$n_draws,
    verdict = verdict, stringsAsFactors = FALSE
  )
}

# =============================================================================
# 5. Main program
# =============================================================================
main <- function() {
  cfg <- parse_args(CFG)
  message("=============================================================")
  message(" Random-Dirichlet audit for NEST26 benchmark-retained regimes")
  message("   final taxonomy + fixed-weight validation + archived NEST26 code")
  message("=============================================================")
  message(" bundle_root          = ", normalizePath(cfg$bundle_root, winslash = "/", mustWork = FALSE))
  message(" project_root         = ", normalizePath(cfg$project_root, winslash = "/", mustWork = FALSE))
  message(" validation_code_root = ", normalizePath(cfg$validation_code_root, winslash = "/", mustWork = FALSE))
  message(" taxonomy_csv         = ", normalizePath(cfg$taxonomy_csv, winslash = "/", mustWork = FALSE))
  message(" selected_regimes_csv = ", normalizePath(cfg$selected_regimes_csv, winslash = "/", mustWork = FALSE))
  message(" output_root          = ", normalizePath(cfg$output_root, winslash = "/", mustWork = FALSE))
  load_project(cfg)
  message(sprintf(" basis N_EST = %d (from sourced registry: %s)",
                  N_EST, paste(ESTIMATOR_NAMES, collapse = ",")))
  message(sprintf(" draws/regime=%d  R=%d  q=%.2f  alpha=%.2f  seeds={%s}",
                  cfg$n_draws, cfg$R, cfg$q, cfg$dirichlet_alpha,
                  paste(cfg$validation_seeds, collapse = ",")))
  message(sprintf(" audit modes={%s}", paste(cfg$audit_modes, collapse = ",")))

  regimes <- select_abstain_regimes(cfg)
  res <- do.call(rbind, lapply(seq_len(nrow(regimes)),
                               function(i) audit_regime(regimes[i, , drop = FALSE], cfg)))

  # Positive oracle: the two transfer specialists are known wins in the final
  # taxonomy. Scoring their own fixed GA weights through this annex is a practical
  # guard against path drift, estimator-basis mismatch, or gate miswiring.
  message("\n--- GATE SANITY CHECK: transfer specialists in locked-unseen mode ---")
  tax <- read_csv0(cfg$taxonomy_csv); sel <- read_csv0(cfg$selected_regimes_csv)
  win_ids <- tax$validation_id[tax$evidence_grade %in% "transfer_specialist"]
  oracle <- NULL
  for (wid in win_ids) {
    wr <- sel[sel$validation_id == wid, , drop = FALSE]
    if (!nrow(wr)) next
    wr$validation_mode <- "locked_unseen_similar"
    gw <- ga_weight_for_row(wr[1, , drop = FALSE])
    sp <- oracle_seeds_passed(wr[1, , drop = FALSE], gw, cfg)
    message(sprintf("   %s (%s, locked_unseen_similar): GA winner passes %s/%d seeds (expected: high)",
                    wid, wr$distribution[1], ifelse(is.na(sp), "NA", sp),
                    length(cfg$validation_seeds)))
    oracle <- rbind(oracle, data.frame(validation_id = wid, distribution = wr$distribution[1],
                                       validation_mode = wr$validation_mode[1],
                                       ga_winner_seeds_passed = sp, stringsAsFactors = FALSE))
  }

  dir.create(cfg$output_root, showWarnings = FALSE, recursive = TRUE)
  out <- if (is.null(cfg$out_csv) || length(cfg$out_csv) == 0L ||
             (length(cfg$out_csv) == 1L && is.na(cfg$out_csv))) {
    file.path(cfg$output_root, "abstain_audit_results.csv")
  } else {
    as.character(cfg$out_csv[1])
  }
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(res, out, row.names = FALSE)
  if (!is.null(oracle)) {
    oracle_out <- file.path(cfg$output_root, "gate_sanity_transfer_specialists.csv")
    utils::write.csv(oracle, oracle_out, row.names = FALSE)
  }

  message("\n================================ SUMMARY ================================")
  print(res[, c("validation_id","distribution","validation_mode","evidence_grade","ga_winner_seeds_passed",
                "max_per_seed_passes","n_seed_stable_passes","verdict")], row.names = FALSE)
  n_conf <- sum(res$n_seed_stable_passes == 0L)
  message(sprintf("\n%d / %d audited regime-mode rows CONFIRMED (random search also abstains).",
                  n_conf, nrow(res)))
  message("Read the 'ga_winner_seeds_passed' column first: for a genuine abstain regime the")
  message("pipeline's OWN winner should pass few/no seeds; for the sanity-check wins it should")
  message("pass most. If those don't hold, the gate is mis-pointed (e.g. NEST10 vs NEST26 basis).")
  flagged <- res[res$n_seed_stable_passes > 0L, c("validation_id", "validation_mode"), drop = FALSE]
  if (nrow(flagged)) {
    flagged_txt <- paste(paste0(flagged$validation_id, "@", flagged$validation_mode), collapse = ", ")
    message("Regime-modes where random search DID pass (review): ", flagged_txt)
  }
  message("Written: ", normalizePath(out, mustWork = FALSE))
  invisible(res)
}

if (sys.nframe() == 0L) main()
