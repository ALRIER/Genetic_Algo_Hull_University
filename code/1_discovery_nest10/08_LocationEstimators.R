# =============================================================================
# 08_LOCATIONESTIMATORS
# =============================================================================
# Main launcher/orchestration module for the robust-location simulation pipeline.
# The file keeps the stage self-contained: it checks that the expected modules
# are available, then runs the regime-first discovery workflow.
# =============================================================================

# Module tracking is defined in 00_utils_debug_io.R. This fallback only lets the file source on its own.
if (!exists("mark_module_done", mode = "function", inherits = TRUE)) {
  mark_module_done <- function(module_id, extra = NULL) invisible(TRUE)
  is_module_done   <- function(module_id) FALSE
}

# Resolve the project directory without forcing the working directory.
# Priority:
#   1) PROJECT_ROOT env var, when the user wants to pin a run folder explicitly.
#   2) Current working directory, for normal modular project runs.
# This keeps the script portable across machines, folders, and cluster/shared runs.
this_dir <- normalizePath(Sys.getenv("PROJECT_ROOT", unset = getwd()), winslash = "/", mustWork = FALSE)

# Respect opt-in debug flags instead of auto-enabling them.
if (!nzchar(Sys.getenv("DEBUG_MAIN", ""))) {
  Sys.setenv(DEBUG_MAIN = "0")
}

# Keep RUN_MODE available both as an environment variable and as an R object.
# This avoids failures when this module is sourced directly after setting
# Sys.setenv(RUN_MODE = "none") in interactive/manual runs.
if (!exists("RUN_MODE", inherits = FALSE)) {
  RUN_MODE <- tolower(trimws(Sys.getenv("RUN_MODE", unset = "none")))
}

# Fallback console formatter in case 00_utils_debug_io.R has not been sourced yet.
if (!exists("catf", mode = "function", inherits = TRUE)) {
  catf <- function(fmt, ...) cat(sprintf(fmt, ...))
}

.first_chr <- function(x, nm) if (nm %in% names(x) && length(x[[nm]]) > 0L) as.character(x[[nm]][1]) else NA_character_
.first_int <- function(x, nm) suppressWarnings(as.integer(if (nm %in% names(x) && length(x[[nm]]) > 0L) x[[nm]][1] else NA_integer_))

catf("\n[CHECK] this_dir = %s\n", this_dir)
catf("[CHECK] getwd()  = %s\n\n", getwd())

# 1) Confirm that the expected module files are present.
module_files <- c(
  "00_utils_debug_io.R",
  "01_paths_repro.R",
  "02_scenarios_sampling.R",
  "03_distributions_params.R",
  "04_estimators_registry.R",
  "05_ga_core.R",
  "06_data_prep.R",
  "07_fitness_objectives.R"
)

missing_files <- module_files[!file.exists(file.path(this_dir, module_files))]
if (length(missing_files) > 0L) {
  msg <- paste0(
    "[CHECK] Missing module files under this_dir:\n  - ",
    paste(missing_files, collapse = "\n  - "),
    "\n\nCheck PROJECT_ROOT, the working directory, or the module_files list."
  )
  stop(msg, call. = FALSE)
} else {
  catf("[CHECK] All module files exist under this_dir.\n")
}

# 2) Confirm that the symbols needed by the launcher are available.
required <- list(
  "00_utils" = list(
    fun = c("safe_write_csv", "safe_save_rds", "safe_write_lines"),
    obj = c()
  ),
  "01_paths_repro" = list(
    fun = c("make_crn_indices", ".ensure_seed", ".seed_scope"),
    obj = c("IS_WINDOWS", "OUT_ROOT")
  ),
  "02_scenarios_sampling" = list(
    fun = c("build_scenarios_full", "build_scenarios_light", "pick_scenario_subset"),
    obj = c(".scenario_subset_cache")
  ),
  "03_distributions_params" = list(
    fun = c("generate_population", "analytic_mean_from_params"),
    obj = c("param_grids")
  ),
  "04_estimators_registry" = list(
    fun = c("custom_estimator", "weights_to_formula",
            "admissible_estimator_mask", "apply_estimator_admissibility",
            "estimator_admissibility_report"),
    obj = c("ESTIMATOR_REGISTRY", "ESTIMATOR_NAMES", "N_EST", "ESTIMATOR_METADATA")
  ),
  "05_ga_core" = list(
    fun = c("init_population", "crossover", "mutate_weights", "tournament_select", "make_folds"),
    obj = c(".ga_default_ctrl")
  ),
  "06_data_prep" = list(
    fun = c("build_scenarios", "prep_scenarios", "inject_outliers_realistic"),
    obj = c(".prepped_cache")
  ),
  "07_fitness_objectives" = list(
    fun = c("fitness_universal", "random_search_baseline"),
    obj = c()
  )
)

missing_syms <- character(0)

for (mod in names(required)) {
  for (fn in required[[mod]]$fun) {
    if (!exists(fn, mode = "function", inherits = TRUE)) {
      missing_syms <- c(missing_syms, paste0(mod, "::", fn, " (function)"))
    }
  }
  for (ob in required[[mod]]$obj) {
    if (!exists(ob, inherits = TRUE)) {
      missing_syms <- c(missing_syms, paste0(mod, "::", ob, " (object)"))
    }
  }
}

if (length(missing_syms) > 0L) {
  msg <- paste0(
    "[CHECK] Module load check failed. Missing symbols:\n  - ",
    paste(missing_syms, collapse = "\n  - "),
    "\n\nMost common cause: source() paths are wrong (PROJECT_ROOT/getwd mismatch).\n",
    "Ensure the modules are sourced first, or point PROJECT_ROOT to the module folder."
  )
  stop(msg, call. = FALSE)
} else {
  catf("[CHECK] Module load check OK: all required symbols are present.\n\n")
}
# ===============================================================================


# ====================== DEBUG SWITCH (MAIN) ======================
# Set environment variable DEBUG_MAIN=1 to enable verbose console logs from MAIN pipeline.
.dbg_main <- function(msg) {
  if (identical(Sys.getenv("DEBUG_MAIN","0"), "1")) {
    message(sprintf("[DEBUG_MAIN %s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg))
  }
}
# Convenience: print short object summary (names, dim, class)
.dbg_obj <- function(name, x) {
  if (!identical(Sys.getenv("DEBUG_MAIN","0"), "1")) return(invisible(NULL))
  cls <- paste(class(x), collapse=",")
  dn  <- tryCatch(paste(dim(x), collapse="x"), error=function(e) NA_character_)
  nms <- tryCatch(paste(utils::head(names(x), 20), collapse=","), error=function(e) "<no-names>")
  message(sprintf("[DEBUG_MAIN %s] %s | class=%s | dim=%s | names(head)=%s",
                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"), name, cls, dn, nms))
  invisible(NULL)
}


# =============================================================================
# REGIME DISCOVERY REPORTING HELPERS
# =============================================================================
# These helpers add a reporting-only layer for regime-aware discovery.
# They do not change the GA training loop, PSOCK logic, K-folds, halving,
# final retrain policy, or benchmark gate. They summarize where the final
# GA finalist appears promising relative to admissible classical benchmarks.

.regime_shape_bin <- function(distribution) {
  fam <- tolower(as.character(distribution)[1])
  switch(fam,
         "normal"     = "symmetric",
         "lognormal"  = "right_skewed",
         "weibull"    = "right_skewed_flexible_shape",
         "invgauss"   = "right_skewed_duration",
         "exgaussian" = "gaussian_core_right_skewed_tail",
         "exwald"     = "first_passage_right_skewed",
         "unknown_shape")
}

.regime_tail_bin <- function(distribution) {
  fam <- tolower(as.character(distribution)[1])
  switch(fam,
         "normal"     = "light_tail",
         "lognormal"  = "heavy_right_tail",
         "weibull"    = "flexible_tail",
         "invgauss"   = "heavy_right_tail",
         "exgaussian" = "exponential_right_tail",
         "exwald"     = "heavy_first_passage_tail",
         "unknown_tail")
}

.regime_rate_bin <- function(r) {
  if (is.null(r) || length(r) == 0L || !is.finite(r)) return("r=NA")
  if (r <= 0)    return("clean")
  if (r <= 0.02) return("low")
  if (r <= 0.10) return("moderate")
  if (r <= 0.20) return("high")
  "extreme"
}

.regime_rate_order <- function(rate_regime) {
  x <- tolower(as.character(rate_regime)[1])
  switch(x, "clean" = 0L, "low" = 1L, "moderate" = 2L,
         "high" = 3L, "extreme" = 4L, NA_integer_)
}

.regime_scale_bin <- function(s) {
  if (is.null(s) || length(s) == 0L || !is.finite(s)) return("s=NA")
  if (s <= 3)  return("low_scale")
  if (s <= 9)  return("moderate_scale")
  if (s <= 20) return("high_scale")
  "extreme_scale"
}

.regime_scale_order <- function(scale_regime) {
  x <- tolower(as.character(scale_regime)[1])
  switch(x, "low_scale" = 1L, "moderate_scale" = 2L,
         "high_scale" = 3L, "extreme_scale" = 4L, NA_integer_)
}

.regime_scale_group_bin <- function(scale_regime) {
  x <- tolower(as.character(scale_regime)[1])
  switch(x,
         "low_scale" = "low_scale",
         "moderate_scale" = "mid_high_scale",
         "high_scale" = "mid_high_scale",
         "extreme_scale" = "extreme_scale",
         "scale_unknown")
}

.regime_scale_group_order <- function(scale_group_regime) {
  x <- tolower(as.character(scale_group_regime)[1])
  switch(x,
         "low_scale" = 1L,
         "mid_high_scale" = 2L,
         "extreme_scale" = 3L,
         NA_integer_)
}

.regime_sample_size_bin <- function(n) {
  if (is.null(n) || length(n) == 0L || !is.finite(n)) return("n=NA")
  if (n <= 500)  return("small_n")
  if (n <= 2000) return("medium_n")
  "large_n"
}

.regime_contamination_presence <- function(r) {
  if (is.null(r) || length(r) == 0L || !is.finite(r)) return("contamination_unknown")
  if (r <= 0) "clean" else "contaminated"
}

.regime_contamination_direction <- function(type, rate = NA_real_) {
  if (!is.null(rate) && length(rate) > 0L && is.finite(rate) && rate <= 0) return("none")
  typ <- tolower(as.character(type)[1])
  if (!nzchar(typ) || is.na(typ)) return("unknown_direction")
  if (grepl("upper", typ)) return("upper_tail")
  if (grepl("lower", typ)) return("lower_tail")
  if (grepl("symmetric", typ)) return("symmetric")
  if (grepl("bimodal", typ)) return("bimodal")
  if (grepl("point", typ)) return("point_mass")
  if (grepl("t", typ)) return("symmetric")
  "other_direction"
}

.regime_contamination_structure <- function(type, rate = NA_real_) {
  if (!is.null(rate) && length(rate) > 0L && is.finite(rate) && rate <= 0) return("none")
  typ <- tolower(as.character(type)[1])
  if (!nzchar(typ) || is.na(typ)) return("unknown_structure")
  if (grepl("clustered", typ)) return("clustered")
  if (grepl("bimodal", typ)) return("bimodal_mixture")
  if (grepl("point", typ)) return("point_mass")
  if (grepl("symmetric_t", typ)) return("heavy_tailed_t")
  if (grepl("tail", typ)) return("tail_outlier")
  "other_structure"
}

.regime_severity_bin <- function(rate, scale) {
  if (is.null(rate) || length(rate) == 0L || !is.finite(rate)) return("severity_unknown")
  if (rate <= 0) return("clean")
  if (is.null(scale) || length(scale) == 0L || !is.finite(scale)) return("severity_unknown")
  if (rate <= 0.02 && scale <= 3) return("low_severity")
  if (rate <= 0.10 && scale <= 9) return("moderate_severity")
  if (rate <= 0.20 && scale <= 20) return("high_severity")
  "extreme_severity"
}

.add_regime_columns <- function(scen_tbl, distribution = NULL, include_sample_size_bin = FALSE) {
  if (is.null(scen_tbl) || !is.data.frame(scen_tbl) || !nrow(scen_tbl)) return(scen_tbl)
  out <- as.data.frame(scen_tbl)
  if (!"distribution" %in% names(out)) {
    out$distribution <- if (!is.null(distribution)) as.character(distribution)[1] else NA_character_
  }
  if (!"contamination_rate" %in% names(out) ||
      !"outlier_scale_mad" %in% names(out) ||
      !"contamination_type" %in% names(out)) {
    stop(".add_regime_columns requires contamination_rate, outlier_scale_mad, and contamination_type.", call. = FALSE)
  }
  out$shape_regime <- vapply(out$distribution, .regime_shape_bin, character(1))
  out$tail_regime <- vapply(out$distribution, .regime_tail_bin, character(1))
  out$rate_regime <- vapply(out$contamination_rate, .regime_rate_bin, character(1))
  out$scale_regime <- vapply(out$outlier_scale_mad, .regime_scale_bin, character(1))
  out$rate_regime_order <- vapply(out$rate_regime, .regime_rate_order, integer(1))
  out$scale_regime_order <- vapply(out$scale_regime, .regime_scale_order, integer(1))
  out$scale_group_regime <- vapply(out$scale_regime, .regime_scale_group_bin, character(1))
  out$scale_group_regime_order <- vapply(out$scale_group_regime, .regime_scale_group_order, integer(1))
  out$mechanism_regime <- as.character(out$contamination_type)
  out$contamination_regime <- vapply(out$contamination_rate, .regime_contamination_presence, character(1))
  out$contamination_direction <- mapply(.regime_contamination_direction, out$contamination_type, out$contamination_rate, USE.NAMES = FALSE)
  out$contamination_structure <- mapply(.regime_contamination_structure, out$contamination_type, out$contamination_rate, USE.NAMES = FALSE)
  out$severity_regime <- mapply(.regime_severity_bin, out$contamination_rate, out$outlier_scale_mad, USE.NAMES = FALSE)
  if (isTRUE(include_sample_size_bin) && "sample_size" %in% names(out)) {
    out$sample_size_regime <- vapply(out$sample_size, .regime_sample_size_bin, character(1))
    out$regime_key <- paste(out$distribution, out$shape_regime, out$tail_regime,
                            out$contamination_direction, out$contamination_structure,
                            out$rate_regime, out$scale_group_regime,
                            out$sample_size_regime, sep = " | ")
  } else {
    out$sample_size_regime <- NA_character_
    out$regime_key <- paste(out$distribution, out$shape_regime, out$tail_regime,
                            out$contamination_direction, out$contamination_structure,
                            out$rate_regime, out$scale_group_regime, sep = " | ")
  }
  out$experimental_condition_key <- paste(out$distribution, out$shape_regime,
                                          out$tail_regime, out$contamination_regime,
                                          out$contamination_direction,
                                          out$contamination_structure,
                                          out$rate_regime, out$scale_regime,
                                          out$severity_regime, sep = " | ")
  out$exact_condition_key <- paste0(out$distribution,
                                    " | type=", out$contamination_type,
                                    " | rate=", out$contamination_rate,
                                    " | scale=", out$outlier_scale_mad)
  out
}

.allowed_benchmarks_for_distribution <- function(distribution = NULL) {
  allowed <- if (exists("ESTIMATOR_NAMES", inherits = TRUE)) ESTIMATOR_NAMES else character(0)
  if (exists("estimator_admissibility_report", mode = "function", inherits = TRUE)) {
    rep <- tryCatch(estimator_admissibility_report(distribution, target = "arithmetic_mean"), error = function(e) NULL)
    if (!is.null(rep) && all(c("estimator", "allowed") %in% names(rep))) {
      allowed <- as.character(rep$estimator[rep$allowed %in% TRUE])
    }
  }
  allowed
}

.collapse_unique <- function(df, col) {
  if (!col %in% names(df)) return(NA_character_)
  vals <- unique(df[[col]])
  vals <- vals[!is.na(vals)]
  if (!length(vals)) return(NA_character_)
  vals <- if (is.numeric(vals)) sort(vals) else sort(as.character(vals))
  paste(vals, collapse = ";")
}

.first_or_na <- function(df, col) {
  if (!col %in% names(df) || !length(df[[col]])) return(NA_character_)
  as.character(df[[col]][1])
}

.summarize_one_regime <- function(df, distribution = NULL,
                                  ga_estimator_name = "robust",
                                  min_rel_improvement = 0.00) {
  allowed <- .allowed_benchmarks_for_distribution(distribution)
  ga_rows <- df[df$estimator == ga_estimator_name, , drop = FALSE]
  bench_rows <- df[df$estimator != ga_estimator_name & df$estimator %in% allowed, , drop = FALSE]
  id_cols <- c("contamination_rate", "outlier_scale_mad", "contamination_type")
  id_cols <- id_cols[id_cols %in% names(df)]
  n_unique_scenarios <- if (length(id_cols)) nrow(unique(df[, id_cols, drop = FALSE])) else NA_integer_
  sample_cols <- c("sample_size", id_cols)
  sample_cols <- sample_cols[sample_cols %in% names(df)]
  n_unique_sample_scenarios <- if (length(sample_cols)) nrow(unique(df[, sample_cols, drop = FALSE])) else NA_integer_
  condition_summary <- paste0(
    "shape=", .first_or_na(df, "shape_regime"),
    " | tail=", .first_or_na(df, "tail_regime"),
    " | contamination=", .first_or_na(df, "contamination_regime"),
    " | direction=", .first_or_na(df, "contamination_direction"),
    " | structure=", .first_or_na(df, "contamination_structure"),
    " | severity=", .first_or_na(df, "severity_regime"),
    " | rates={", .collapse_unique(df, "contamination_rate"), "}",
    " | scales={", .collapse_unique(df, "outlier_scale_mad"), "}",
    " | types={", .collapse_unique(df, "contamination_type"), "}"
  )
  base <- data.frame(
    distribution = if ("distribution" %in% names(df)) as.character(df$distribution[1]) else as.character(distribution)[1],
    regime_key = .first_or_na(df, "regime_key"),
    experimental_condition_key = .first_or_na(df, "experimental_condition_key"),
    condition_summary = condition_summary,
    shape_regime = .first_or_na(df, "shape_regime"),
    tail_regime = .first_or_na(df, "tail_regime"),
    mechanism_regime = .first_or_na(df, "mechanism_regime"),
    contamination_regime = .first_or_na(df, "contamination_regime"),
    contamination_direction = .first_or_na(df, "contamination_direction"),
    contamination_structure = .first_or_na(df, "contamination_structure"),
    rate_regime = .first_or_na(df, "rate_regime"),
    scale_regime = .first_or_na(df, "scale_regime"),
    scale_group_regime = .first_or_na(df, "scale_group_regime"),
    rate_regime_order = suppressWarnings(as.integer(.first_or_na(df, "rate_regime_order"))),
    scale_regime_order = suppressWarnings(as.integer(.first_or_na(df, "scale_regime_order"))),
    scale_group_regime_order = suppressWarnings(as.integer(.first_or_na(df, "scale_group_regime_order"))),
    severity_regime = .first_or_na(df, "severity_regime"),
    sample_size_regime = .first_or_na(df, "sample_size_regime"),
    exact_contamination_rates = .collapse_unique(df, "contamination_rate"),
    exact_outlier_scales = .collapse_unique(df, "outlier_scale_mad"),
    exact_contamination_types = .collapse_unique(df, "contamination_type"),
    exact_sample_sizes = .collapse_unique(df, "sample_size"),
    n_rows = nrow(df),
    n_unique_scenarios = as.integer(n_unique_scenarios),
    n_unique_sample_scenarios = as.integer(n_unique_sample_scenarios),
    n_ga_rows = nrow(ga_rows),
    n_benchmark_rows = nrow(bench_rows),
    stringsAsFactors = FALSE
  )
  if (!nrow(ga_rows) || !nrow(bench_rows)) {
    return(cbind(base, data.frame(
      ga_mean_mse = NA_real_, ga_q95_mse = NA_real_, ga_abs_bias = NA_real_,
      best_benchmark_q95_estimator = NA_character_, best_benchmark_q95_mse = NA_real_,
      best_benchmark_mean_estimator = NA_character_, best_benchmark_mean_mse = NA_real_,
      ga_rel_improvement_q95 = NA_real_, ga_rel_improvement_mean = NA_real_,
      ga_win_rate_vs_best_q95 = NA_real_, ga_q95_win_rate_vs_best_q95 = NA_real_,
      benchmark_q95_disagreement = NA_real_, benchmark_mean_disagreement = NA_real_,
      benchmark_instability_score = NA_real_, contamination_difficulty_score = NA_real_,
      profile_diversity_score = NA_real_, ga_closeness_to_benchmark = NA_real_,
      opportunity_score = NA_real_,
      gate_pass_regime = FALSE, regime_class = "insufficient_data",
      promise_score = NA_real_, stringsAsFactors = FALSE
    )))
  }
  ga_mean <- mean(ga_rows$mse, na.rm = TRUE)
  ga_q95  <- mean(ga_rows$mse_q95, na.rm = TRUE)
  ga_bias <- mean(abs(ga_rows$bias), na.rm = TRUE)
  bench_sum <- bench_rows |>
    dplyr::group_by(estimator) |>
    dplyr::summarise(mean_mse = mean(mse, na.rm = TRUE),
                     q95_mse = mean(mse_q95, na.rm = TRUE),
                     abs_bias = mean(abs(bias), na.rm = TRUE), .groups = "drop")
  best_q95 <- bench_sum |> dplyr::arrange(q95_mse) |> dplyr::slice(1)
  best_mean <- bench_sum |> dplyr::arrange(mean_mse) |> dplyr::slice(1)
  rel_imp_q95 <- (as.numeric(best_q95$q95_mse[1]) - ga_q95) / (abs(as.numeric(best_q95$q95_mse[1])) + 1e-12)
  rel_imp_mean <- (as.numeric(best_mean$mean_mse[1]) - ga_mean) / (abs(as.numeric(best_mean$mean_mse[1])) + 1e-12)
  scenario_cols <- c("sample_size", "contamination_rate", "outlier_scale_mad", "contamination_type", "true_mean", "scenario_mode")
  scenario_cols <- scenario_cols[scenario_cols %in% names(df)]
  win_mse <- NA_real_; win_q95 <- NA_real_
  bq_est <- as.character(best_q95$estimator[1])
  if (length(scenario_cols) > 0L && isTRUE(nzchar(bq_est))) {
    ga_small <- ga_rows[, c(scenario_cols, "mse", "mse_q95"), drop = FALSE]
    names(ga_small)[names(ga_small) == "mse"] <- "ga_mse"
    names(ga_small)[names(ga_small) == "mse_q95"] <- "ga_mse_q95"
    b_small <- bench_rows[bench_rows$estimator == bq_est, c(scenario_cols, "mse", "mse_q95"), drop = FALSE]
    names(b_small)[names(b_small) == "mse"] <- "benchmark_mse"
    names(b_small)[names(b_small) == "mse_q95"] <- "benchmark_mse_q95"
    joined <- tryCatch(merge(ga_small, b_small, by = scenario_cols), error = function(e) NULL)
    if (!is.null(joined) && nrow(joined)) {
      ok_mse <- is.finite(joined$ga_mse) & is.finite(joined$benchmark_mse)
      ok_q95 <- is.finite(joined$ga_mse_q95) & is.finite(joined$benchmark_mse_q95)
      if (any(ok_mse)) win_mse <- mean(joined$ga_mse[ok_mse] < joined$benchmark_mse[ok_mse])
      if (any(ok_q95)) win_q95 <- mean(joined$ga_mse_q95[ok_q95] < joined$benchmark_mse_q95[ok_q95])
    }
  }
  gate_pass <- is.finite(rel_imp_q95) && is.finite(rel_imp_mean) &&
    rel_imp_q95 >= min_rel_improvement && rel_imp_mean >= min_rel_improvement
  regime_class <- if (isTRUE(gate_pass)) "ga_promising" else if (is.finite(rel_imp_q95) && is.finite(rel_imp_mean) && rel_imp_q95 > -0.05 && rel_imp_mean > -0.05) "ambiguous_near_benchmark" else "benchmark_dominant"

  # Opportunity score: HPF1/HPF2 should not only pass early GA winners.
  # They should also pass regimes where the GA is close to the best
  # benchmark, benchmarks disagree/are unstable, and the contamination profile
  # is difficult enough for composite estimators to have a real opportunity.
  bench_q95_vals <- suppressWarnings(as.numeric(bench_sum$q95_mse))
  bench_mean_vals <- suppressWarnings(as.numeric(bench_sum$mean_mse))
  benchmark_q95_disagreement <- if (sum(is.finite(bench_q95_vals)) >= 2L) {
    stats::sd(bench_q95_vals, na.rm = TRUE) / (abs(mean(bench_q95_vals, na.rm = TRUE)) + 1e-12)
  } else 0
  benchmark_mean_disagreement <- if (sum(is.finite(bench_mean_vals)) >= 2L) {
    stats::sd(bench_mean_vals, na.rm = TRUE) / (abs(mean(bench_mean_vals, na.rm = TRUE)) + 1e-12)
  } else 0
  benchmark_instability_score <- mean(c(benchmark_q95_disagreement, benchmark_mean_disagreement), na.rm = TRUE)
  contamination_difficulty_score <- mean(
    suppressWarnings(as.numeric(df$contamination_rate)) *
      suppressWarnings(as.numeric(df$outlier_scale_mad)),
    na.rm = TRUE
  )
  if (!is.finite(contamination_difficulty_score)) contamination_difficulty_score <- 0
  contamination_difficulty_score <- log1p(max(0, contamination_difficulty_score))
  profile_diversity_score <- length(unique(paste(df$contamination_type, df$contamination_rate, df$outlier_scale_mad, sep = "|"))) /
    max(1, nrow(df))
  ga_closeness_q95 <- if (is.finite(rel_imp_q95)) max(0, 1 + rel_imp_q95) else 0
  ga_closeness_mean <- if (is.finite(rel_imp_mean)) max(0, 1 + rel_imp_mean) else 0
  ga_closeness_to_benchmark <- mean(c(ga_closeness_q95, ga_closeness_mean), na.rm = TRUE)

  opportunity_score <-
    0.40 * ga_closeness_to_benchmark +
    0.25 * benchmark_instability_score +
    0.15 * contamination_difficulty_score +
    0.10 * profile_diversity_score +
    0.10 * mean(c(win_mse, win_q95), na.rm = TRUE)

  promise_score <- mean(c(rel_imp_q95, rel_imp_mean, win_mse, win_q95), na.rm = TRUE) -
    ifelse(is.finite(ga_bias), 0.05 * ga_bias, 0) +
    ifelse(is.finite(opportunity_score), 0.25 * opportunity_score, 0)

  cbind(base, data.frame(
    ga_mean_mse = ga_mean, ga_q95_mse = ga_q95, ga_abs_bias = ga_bias,
    best_benchmark_q95_estimator = as.character(best_q95$estimator[1]),
    best_benchmark_q95_mse = as.numeric(best_q95$q95_mse[1]),
    best_benchmark_mean_estimator = as.character(best_mean$estimator[1]),
    best_benchmark_mean_mse = as.numeric(best_mean$mean_mse[1]),
    ga_rel_improvement_q95 = as.numeric(rel_imp_q95),
    ga_rel_improvement_mean = as.numeric(rel_imp_mean),
    ga_win_rate_vs_best_q95 = as.numeric(win_mse),
    ga_q95_win_rate_vs_best_q95 = as.numeric(win_q95),
    benchmark_q95_disagreement = as.numeric(benchmark_q95_disagreement),
    benchmark_mean_disagreement = as.numeric(benchmark_mean_disagreement),
    benchmark_instability_score = as.numeric(benchmark_instability_score),
    contamination_difficulty_score = as.numeric(contamination_difficulty_score),
    profile_diversity_score = as.numeric(profile_diversity_score),
    ga_closeness_to_benchmark = as.numeric(ga_closeness_to_benchmark),
    opportunity_score = as.numeric(opportunity_score),
    gate_pass_regime = isTRUE(gate_pass), regime_class = regime_class,
    promise_score = as.numeric(promise_score), stringsAsFactors = FALSE
  ))
}

build_regime_discovery_summary <- function(scen_tbl, distribution = NULL,
                                           ga_estimator_name = "robust",
                                           include_sample_size_bin = FALSE,
                                           min_rel_improvement = 0.00) {
  if (is.null(scen_tbl) || !is.data.frame(scen_tbl) || !nrow(scen_tbl)) return(data.frame())
  required_cols <- c("estimator", "mse", "mse_q95", "bias", "contamination_rate", "outlier_scale_mad", "contamination_type")
  missing <- setdiff(required_cols, names(scen_tbl))
  if (length(missing)) {
    warning(sprintf("REGIME_DISCOVERY skipped: missing columns: %s", paste(missing, collapse = ", ")))
    return(data.frame())
  }
  x <- .add_regime_columns(scen_tbl, distribution = distribution, include_sample_size_bin = include_sample_size_bin)
  pieces <- split(x, x$regime_key, drop = TRUE)
  out <- dplyr::bind_rows(lapply(pieces, .summarize_one_regime,
                                 distribution = distribution,
                                 ga_estimator_name = ga_estimator_name,
                                 min_rel_improvement = min_rel_improvement))
  if (nrow(out)) {
    out <- out |> dplyr::arrange(dplyr::desc(gate_pass_regime), dplyr::desc(promise_score), regime_key) |>
      dplyr::mutate(regime_rank = dplyr::row_number(), .before = 1)
  }
  out
}

select_regimes_for_specialist_training <- function(regime_summary, distribution = NULL,
                                                   top_k = 5L, min_rows = 6L,
                                                   near_margin = 0.10,
                                                   holdout_frac = 0.30,
                                                   seed = 101L,
                                                   min_train_scenarios = 5L,
                                                   min_heldout_scenarios = 1L,
                                                   diversity_quota_per_group = 1L,
                                                   diversity_max_extra = 3L) {
  if (is.null(regime_summary) || !is.data.frame(regime_summary) || !nrow(regime_summary)) return(data.frame())
  top_k <- max(1L, as.integer(top_k)[1])
  min_rows <- max(1L, as.integer(min_rows)[1])
  min_train_scenarios <- max(1L, as.integer(min_train_scenarios)[1])
  min_heldout_scenarios <- max(1L, as.integer(min_heldout_scenarios)[1])
  diversity_quota_per_group <- max(0L, as.integer(diversity_quota_per_group)[1])
  diversity_max_extra <- max(0L, as.integer(diversity_max_extra)[1])
  near_margin <- as.numeric(near_margin)[1]
  if (!is.finite(near_margin) || near_margin < 0) near_margin <- 0.10
  holdout_frac <- as.numeric(holdout_frac)[1]
  if (!is.finite(holdout_frac) || holdout_frac <= 0 || holdout_frac >= 1) holdout_frac <- 0.30
  seed <- .ensure_seed(seed, fallback = 101L)

  x <- regime_summary
  need <- c("distribution", "regime_key", "n_rows", "n_unique_scenarios", "regime_class", "promise_score",
            "ga_rel_improvement_q95", "ga_rel_improvement_mean", "ga_win_rate_vs_best_q95",
            "ga_q95_win_rate_vs_best_q95", "rate_regime", "scale_regime", "rate_regime_order", "scale_regime_order",
            "contamination_direction", "contamination_structure", "scale_group_regime",
            "source_stage", "source_config_index", "source_config_tag")
  for (nm in need) if (!nm %in% names(x)) x[[nm]] <- NA
  if (!"opportunity_score" %in% names(x)) x$opportunity_score <- suppressWarnings(as.numeric(x$promise_score))
  if (!is.null(distribution) && all(is.na(x$distribution))) x$distribution <- as.character(distribution)[1]
  if (all(!is.finite(suppressWarnings(as.integer(x$rate_regime_order)))) && "rate_regime" %in% names(x)) {
    x$rate_regime_order <- vapply(x$rate_regime, .regime_rate_order, integer(1))
  }
  if (all(!is.finite(suppressWarnings(as.integer(x$scale_regime_order)))) && "scale_regime" %in% names(x)) {
    x$scale_regime_order <- vapply(x$scale_regime, .regime_scale_order, integer(1))
  }

  # Selection is deliberately pre-neighborhood. Exact regime size is
  # kept for audit, but it must not kill promising regimes before the elite
  # neighborhood has a chance to rescue sparse cells. The hard eligibility
  # check is applied later inside run_regime_specialist_ga() after exact+neighbor
  # expansion and train/heldout splitting.
  x <- x |>
    dplyr::mutate(
      n_unique_scenarios = suppressWarnings(as.integer(n_unique_scenarios)),
      n_holdout_expected_exact = ifelse(is.finite(n_unique_scenarios),
                                        pmax(min_heldout_scenarios, floor(n_unique_scenarios * holdout_frac)),
                                        NA_integer_),
      n_holdout_expected_exact = ifelse(is.finite(n_unique_scenarios),
                                        pmin(n_holdout_expected_exact,
                                             pmax(min_heldout_scenarios, n_unique_scenarios - min_train_scenarios)),
                                        n_holdout_expected_exact),
      n_train_expected_exact = ifelse(is.finite(n_unique_scenarios), n_unique_scenarios - n_holdout_expected_exact, NA_integer_),
      enough_unique_scenarios_exact = is.finite(n_unique_scenarios) & n_unique_scenarios >= min_rows,
      enough_train_scenarios_exact = is.finite(n_train_expected_exact) & n_train_expected_exact >= min_train_scenarios,
      enough_heldout_scenarios_exact = is.finite(n_holdout_expected_exact) & n_holdout_expected_exact >= min_heldout_scenarios,
      exact_regime_trainable_before_neighborhood = enough_unique_scenarios_exact & enough_train_scenarios_exact & enough_heldout_scenarios_exact,
      needs_neighborhood_for_eligibility = !exact_regime_trainable_before_neighborhood,
      near_q95 = is.finite(ga_rel_improvement_q95) & ga_rel_improvement_q95 >= -near_margin,
      near_mean = is.finite(ga_rel_improvement_mean) & ga_rel_improvement_mean >= -near_margin,
      has_promising_signal = dplyr::case_when(
        regime_class == "ga_promising" ~ TRUE,
        regime_class == "ambiguous_near_benchmark" & near_q95 & near_mean ~ TRUE,
        is.finite(promise_score) ~ TRUE,
        TRUE ~ FALSE
      ),
      # Pre-neighborhood eligibility: signal first; size later. This avoids the
      # sparse but promising regimes are retained for neighborhood expansion
      # before elite neighbors were added.
      eligible_for_specialist = has_promising_signal,
      selection_priority = dplyr::case_when(
        regime_class == "ga_promising" ~ 1L,
        regime_class == "ambiguous_near_benchmark" & near_q95 & near_mean ~ 2L,
        is.finite(promise_score) ~ 3L,
        TRUE ~ 9L
      ),
      skip_reason = dplyr::case_when(
        selection_priority == 9L ~ "no_promising_signal",
        needs_neighborhood_for_eligibility ~ "pending_elite_neighborhood_expansion",
        TRUE ~ NA_character_
      ),
      selection_reason = dplyr::case_when(
        regime_class == "ga_promising" & !needs_neighborhood_for_eligibility ~ "passed_regime_gate_exact_trainable",
        regime_class == "ga_promising" & needs_neighborhood_for_eligibility ~ "passed_regime_gate_pending_neighborhood",
        regime_class == "ambiguous_near_benchmark" & near_q95 & near_mean & !needs_neighborhood_for_eligibility ~ "near_benchmark_candidate_exact_trainable",
        regime_class == "ambiguous_near_benchmark" & near_q95 & near_mean & needs_neighborhood_for_eligibility ~ "near_benchmark_candidate_pending_neighborhood",
        is.finite(promise_score) & !needs_neighborhood_for_eligibility ~ "top_promise_score_candidate_exact_trainable",
        is.finite(promise_score) & needs_neighborhood_for_eligibility ~ "top_promise_score_candidate_pending_neighborhood",
        TRUE ~ skip_reason
      )
    ) |>
    dplyr::mutate(
      diversity_group = paste(distribution, contamination_direction, contamination_structure,
                              ifelse(is.na(scale_group_regime), scale_regime, scale_group_regime), sep = " | ")
    ) |>
    dplyr::arrange(selection_priority, dplyr::desc(promise_score),
                   dplyr::desc(ga_rel_improvement_q95), dplyr::desc(ga_rel_improvement_mean), regime_key) |>
    dplyr::group_by(distribution) |>
    dplyr::mutate(specialist_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(distribution, diversity_group) |>
    dplyr::mutate(diversity_rank = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::group_by(distribution) |>
    dplyr::mutate(
      diversity_selected_prelim = eligible_for_specialist & diversity_quota_per_group > 0L & diversity_rank <= diversity_quota_per_group,
      diversity_extra_rank = dplyr::if_else(diversity_selected_prelim & specialist_rank > top_k,
                                            dplyr::row_number(), NA_integer_),
      selected_by_top_rank = eligible_for_specialist & specialist_rank <= top_k,
      selected_by_diversity = diversity_selected_prelim & specialist_rank > top_k &
        (is.na(diversity_extra_rank) | diversity_extra_rank <= diversity_max_extra),
      selected_for_specialist_training = selected_by_top_rank | selected_by_diversity,
      selection_reason = dplyr::case_when(
        selected_by_top_rank ~ selection_reason,
        selected_by_diversity ~ "diversity_quota_candidate",
        TRUE ~ selection_reason
      ),
      specialist_holdout_frac = holdout_frac,
      specialist_train_frac = 1 - holdout_frac,
      regime_selection_seed = seed,
      regime_selection_stage = "pre_neighborhood_screening_with_diversity_quota"
    ) |>
    dplyr::ungroup() |>
    dplyr::arrange(dplyr::desc(selected_for_specialist_training), selection_priority, specialist_rank, diversity_rank, regime_key)
  x
}

.split_regime_train_heldout <- function(regime_scenarios, holdout_frac = 0.30,
                                        k_folds = 3L, seed = 101L,
                                        min_train_scenarios = 5L,
                                        min_heldout_scenarios = 1L) {
  empty <- list(train = data.frame(), heldout = data.frame(), status = "skipped_empty_regime", n_total = 0L, n_train = 0L, n_heldout = 0L)
  if (is.null(regime_scenarios) || !is.data.frame(regime_scenarios) || !nrow(regime_scenarios)) return(empty)
  holdout_frac <- as.numeric(holdout_frac)[1]
  if (!is.finite(holdout_frac) || holdout_frac <= 0 || holdout_frac >= 1) holdout_frac <- 0.30
  seed <- .ensure_seed(seed, fallback = 101L)
  min_train_scenarios <- max(1L, as.integer(min_train_scenarios)[1])
  min_heldout_scenarios <- max(1L, as.integer(min_heldout_scenarios)[1])
  k_folds <- max(2L, as.integer(k_folds)[1])
  n <- nrow(regime_scenarios)
  if (n < (min_train_scenarios + min_heldout_scenarios)) {
    empty$status <- "skipped_insufficient_unique_scenarios"; empty$n_total <- n; return(empty)
  }
  n_hold <- max(min_heldout_scenarios, floor(n * holdout_frac))
  n_hold <- min(n_hold, n - min_train_scenarios)
  n_train <- n - n_hold
  if (n_train < max(min_train_scenarios, k_folds) || n_hold < min_heldout_scenarios) {
    empty$status <- "skipped_bad_train_heldout_split"; empty$n_total <- n; empty$n_train <- n_train; empty$n_heldout <- n_hold; return(empty)
  }
  set.seed(seed)
  ids <- sample(seq_len(n))
  hold_ix <- ids[seq_len(n_hold)]
  train_ix <- setdiff(seq_len(n), hold_ix)
  keep_cols <- c("scenario_id", "distribution", "contamination_rate", "outlier_scale_mad", "contamination_type",
                 "shape_regime", "tail_regime", "mechanism_regime", "contamination_regime", "contamination_direction",
                 "contamination_structure", "rate_regime", "scale_regime", "rate_regime_order", "scale_regime_order",
                 "severity_regime", "sample_size_regime", "regime_key", "experimental_condition_key", "exact_condition_key",
                 "neighborhood_role", "source_regime_key", "neighbor_rank", "neighbor_promise_score", "neighborhood_skip_reason")
  keep_cols <- keep_cols[keep_cols %in% names(regime_scenarios)]
  list(train = regime_scenarios[train_ix, keep_cols, drop = FALSE],
       heldout = regime_scenarios[hold_ix, keep_cols, drop = FALSE],
       status = "ok", n_total = n, n_train = length(train_ix), n_heldout = length(hold_ix))
}

.expand_regime_with_elite_neighbors <- function(scen_tbl, selected_regime_row,
                                                discovery_or_selection_tbl = NULL,
                                                distribution = NULL,
                                                max_neighbors = 2L,
                                                max_rate_distance = 1L,
                                                max_scale_distance = 1L,
                                                max_total_scenarios = 18L,
                                                min_promise_quantile = 0.50,
                                                include_sample_size_bin = FALSE,
                                                seed = 101L) {
  exact_only <- function(exact, target_key, promise = NA_real_, reason = "exact_only") {
    if (is.null(exact) || !is.data.frame(exact) || !nrow(exact)) return(data.frame())
    exact$neighborhood_role <- "exact_regime"
    exact$source_regime_key <- target_key
    exact$neighbor_rank <- 0L
    exact$neighbor_promise_score <- suppressWarnings(as.numeric(promise)[1])
    exact$neighborhood_skip_reason <- reason
    exact[seq_len(min(nrow(exact), max_total_scenarios)), , drop = FALSE]
  }
  safe_int <- function(x, default = NA_integer_) {
    if (is.null(x) || length(x) < 1L) return(default)
    z <- suppressWarnings(as.integer(x[1])); if (!is.finite(z)) return(default); z
  }
  safe_num <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) < 1L) return(default)
    z <- suppressWarnings(as.numeric(x[1])); if (!is.finite(z)) return(default); z
  }
  safe_chr <- function(x, default = NA_character_) {
    if (is.null(x) || length(x) < 1L || is.na(x[1])) return(default)
    as.character(x[1])
  }
  if (is.null(scen_tbl) || !is.data.frame(scen_tbl) || !nrow(scen_tbl)) return(data.frame())
  if (is.null(selected_regime_row) || !is.data.frame(selected_regime_row) || !nrow(selected_regime_row)) return(data.frame())
  max_neighbors <- max(0L, safe_int(max_neighbors, 0L))
  max_rate_distance <- max(0L, safe_int(max_rate_distance, 0L))
  max_scale_distance <- max(0L, safe_int(max_scale_distance, 0L))
  max_total_scenarios <- max(1L, safe_int(max_total_scenarios, 18L))
  min_promise_quantile <- safe_num(min_promise_quantile, 0.50)
  if (!is.finite(min_promise_quantile) || min_promise_quantile < 0 || min_promise_quantile > 1) min_promise_quantile <- 0.50
  seed <- .ensure_seed(seed, fallback = 101L)
  sc <- .add_regime_columns(scen_tbl, distribution = distribution, include_sample_size_bin = include_sample_size_bin)
  target <- selected_regime_row[1, , drop = FALSE]
  target_key <- safe_chr(target$regime_key, NA_character_)
  if (is.na(target_key) || !nzchar(target_key)) return(data.frame())
  exact <- sc[sc$regime_key == target_key, , drop = FALSE]
  target_promise <- safe_num(target$promise_score, NA_real_)
  if (!nrow(exact)) return(data.frame())
  if (max_neighbors <= 0L) return(exact_only(exact, target_key, target_promise, "neighborhood_disabled"))
  target_rate <- safe_int(target$rate_regime_order, NA_integer_)
  target_scale <- safe_int(target$scale_regime_order, NA_integer_)
  if (!is.finite(target_rate) && "rate_regime" %in% names(target)) target_rate <- safe_int(.regime_rate_order(target$rate_regime[1]), NA_integer_)
  target_scale_group <- safe_int(target$scale_group_regime_order, NA_integer_)
  if (!is.finite(target_scale) && "scale_regime" %in% names(target)) target_scale <- safe_int(.regime_scale_order(target$scale_regime[1]), NA_integer_)
  if (!is.finite(target_scale_group) && "scale_group_regime" %in% names(target)) target_scale_group <- safe_int(.regime_scale_group_order(target$scale_group_regime[1]), NA_integer_)
  if (!is.finite(target_scale_group) && "scale_regime" %in% names(target)) target_scale_group <- safe_int(.regime_scale_group_order(.regime_scale_group_bin(target$scale_regime[1])), NA_integer_)
  if (!is.finite(target_rate) || (!is.finite(target_scale) && !is.finite(target_scale_group))) {
    return(exact_only(exact, target_key, target_promise, "missing_rate_or_scale_order_exact_only"))
  }
  reg_tbl <- if (!is.null(discovery_or_selection_tbl) && is.data.frame(discovery_or_selection_tbl) && nrow(discovery_or_selection_tbl)) {
    as.data.frame(discovery_or_selection_tbl)
  } else {
    unique(sc[, intersect(c("distribution", "regime_key", "mechanism_regime", "contamination_direction", "contamination_structure",
                           "rate_regime", "scale_regime", "rate_regime_order", "scale_regime_order"), names(sc)), drop = FALSE])
  }
  for (nm in c("distribution", "regime_key", "mechanism_regime", "contamination_direction", "contamination_structure",
               "rate_regime", "scale_regime", "scale_group_regime", "rate_regime_order", "scale_regime_order", "scale_group_regime_order", "promise_score")) {
    if (!nm %in% names(reg_tbl)) reg_tbl[[nm]] <- NA
  }
  if (all(!is.finite(suppressWarnings(as.integer(reg_tbl$rate_regime_order))))) {
    reg_tbl$rate_regime_order <- vapply(reg_tbl$rate_regime, .regime_rate_order, integer(1))
  }
  if (all(!is.finite(suppressWarnings(as.integer(reg_tbl$scale_regime_order))))) {
    reg_tbl$scale_regime_order <- vapply(reg_tbl$scale_regime, .regime_scale_order, integer(1))
  }
  if (all(!is.finite(suppressWarnings(as.integer(reg_tbl$scale_group_regime_order))))) {
    if (all(is.na(reg_tbl$scale_group_regime)) && "scale_regime" %in% names(reg_tbl)) {
      reg_tbl$scale_group_regime <- vapply(reg_tbl$scale_regime, .regime_scale_group_bin, character(1))
    }
    reg_tbl$scale_group_regime_order <- vapply(reg_tbl$scale_group_regime, .regime_scale_group_order, integer(1))
  }
  cand <- reg_tbl[as.character(reg_tbl$regime_key) != target_key, , drop = FALSE]
  if (!nrow(cand)) return(exact_only(exact, target_key, target_promise, "no_candidate_neighbors"))
  cand_rate <- suppressWarnings(as.integer(cand$rate_regime_order))
  cand_scale <- suppressWarnings(as.integer(cand$scale_regime_order))
  cand_scale_group <- suppressWarnings(as.integer(cand$scale_group_regime_order))
  scale_ok <- if (is.finite(target_scale_group) && any(is.finite(cand_scale_group))) {
    is.finite(cand_scale_group) & abs(cand_scale_group - target_scale_group) <= max_scale_distance
  } else {
    is.finite(cand_scale) & abs(cand_scale - target_scale) <= max_scale_distance
  }
  keep <- !is.na(cand$distribution) & as.character(cand$distribution) == safe_chr(target$distribution) &
    !is.na(cand$contamination_direction) & as.character(cand$contamination_direction) == safe_chr(target$contamination_direction) &
    !is.na(cand$contamination_structure) & as.character(cand$contamination_structure) == safe_chr(target$contamination_structure) &
    is.finite(cand_rate) & abs(cand_rate - target_rate) <= max_rate_distance &
    scale_ok
  keep[is.na(keep)] <- FALSE
  candidates <- cand[keep, , drop = FALSE]
  if (!nrow(candidates)) return(exact_only(exact, target_key, target_promise, "no_elite_neighbors_after_filters"))
  candidates$promise_score <- suppressWarnings(as.numeric(candidates$promise_score))
  finite_prom <- candidates$promise_score[is.finite(candidates$promise_score)]
  if (length(finite_prom)) {
    cutoff <- as.numeric(stats::quantile(finite_prom, probs = min_promise_quantile, na.rm = TRUE, names = FALSE))
    keep_promise <- is.finite(candidates$promise_score) & candidates$promise_score >= cutoff
    keep_promise[is.na(keep_promise)] <- FALSE
    candidates <- candidates[keep_promise, , drop = FALSE]
  }
  if (!nrow(candidates)) return(exact_only(exact, target_key, target_promise, "no_elite_neighbors_after_promise_filter"))
  candidates <- candidates[order(-candidates$promise_score, candidates$regime_key), , drop = FALSE]
  candidates <- candidates[seq_len(min(max_neighbors, nrow(candidates))), , drop = FALSE]
  pieces <- list(exact_only(exact, target_key, target_promise, "exact_plus_elite_neighbors"))
  for (ii in seq_len(nrow(candidates))) {
    nb_key <- as.character(candidates$regime_key[ii])
    nb <- sc[sc$regime_key == nb_key, , drop = FALSE]
    if (!nrow(nb)) next
    nb$neighborhood_role <- "elite_neighbor"
    nb$source_regime_key <- target_key
    nb$neighbor_rank <- ii
    nb$neighbor_promise_score <- safe_num(candidates$promise_score[ii], NA_real_)
    nb$neighborhood_skip_reason <- NA_character_
    pieces[[length(pieces) + 1L]] <- nb
  }
  out <- do.call(rbind, pieces)
  if (!"scenario_id" %in% names(out)) out <- add_scenario_ids(out)
  out <- out[!duplicated(out$scenario_id), , drop = FALSE]
  if (nrow(out) > max_total_scenarios) {
    exact_ids <- which(out$neighborhood_role == "exact_regime")
    nb_ids <- which(out$neighborhood_role != "exact_regime")
    keep_exact <- exact_ids[seq_len(min(length(exact_ids), max_total_scenarios))]
    remaining <- max_total_scenarios - length(keep_exact)
    keep_nb <- if (remaining > 0 && length(nb_ids)) nb_ids[seq_len(min(remaining, length(nb_ids)))] else integer(0)
    out <- out[c(keep_exact, keep_nb), , drop = FALSE]
  }
  out
}


# Extract protected elite weights from a previous-stage summary for one regime.
# Protection is one-stage only: these candidates are injected into the next
# heldout finalist set, but they must re-earn survival there. They are not
# automatically protected in any later stage.
.extract_protected_elites_for_regime <- function(protected_elites,
                                                 regime_key,
                                                 max_per_regime = 3L,
                                                 stage_origin = NA_character_) {
  empty <- list(weights = NULL, meta = data.frame())
  if (is.null(protected_elites) || !is.data.frame(protected_elites) || !nrow(protected_elites)) return(empty)
  if (!"regime_key" %in% names(protected_elites)) return(empty)
  wcols <- grep("^w_", names(protected_elites), value = TRUE)
  if (!length(wcols)) return(empty)
  rows <- protected_elites[as.character(protected_elites$regime_key) == as.character(regime_key), , drop = FALSE]
  if (!nrow(rows)) return(empty)
  rows <- rows[seq_len(min(nrow(rows), max(1L, as.integer(max_per_regime)[1]))), , drop = FALSE]
  W <- as.matrix(rows[, wcols, drop = FALSE])
  storage.mode(W) <- "numeric"
  ok <- apply(W, 1, function(z) length(z) && all(is.finite(z)) && sum(abs(z)) > 0)
  if (!any(ok)) return(empty)
  W <- W[ok, , drop = FALSE]
  rows <- rows[ok, , drop = FALSE]
  W <- t(apply(W, 1, .normalize_simplex))
  origin <- if (is.character(stage_origin) && length(stage_origin) && nzchar(stage_origin[1])) stage_origin[1] else
    if ("specialist_halving_rung" %in% names(rows)) as.character(rows$specialist_halving_rung) else "previous_stage_elite"
  if (length(origin) == 1L) origin <- rep(origin, nrow(W))
  meta <- data.frame(
    protected_elite = TRUE,
    protected_elite_origin = origin[seq_len(nrow(W))],
    protected_elite_one_stage_only = TRUE,
    protected_elite_source_gate_pass = if ("gate_pass" %in% names(rows)) rows$gate_pass else NA,
    protected_elite_source_q95_improvement = if ("ga_rel_improvement_q95" %in% names(rows)) suppressWarnings(as.numeric(rows$ga_rel_improvement_q95)) else NA_real_,
    protected_elite_source_mean_improvement = if ("ga_rel_improvement_mean" %in% names(rows)) suppressWarnings(as.numeric(rows$ga_rel_improvement_mean)) else NA_real_,
    stringsAsFactors = FALSE
  )
  list(weights = W, meta = meta)
}

.combine_current_and_protected_weights <- function(current_weights,
                                                   protected_pack = NULL,
                                                   current_origin = "current_stage_finalist") {
  Wcur <- as.matrix(current_weights)
  storage.mode(Wcur) <- "numeric"
  if (nrow(Wcur)) Wcur <- t(apply(Wcur, 1, .normalize_simplex))
  cur_meta <- data.frame(
    candidate_source = current_origin,
    protected_elite = FALSE,
    protected_elite_origin = NA_character_,
    protected_elite_one_stage_only = FALSE,
    protected_elite_source_gate_pass = NA,
    protected_elite_source_q95_improvement = NA_real_,
    protected_elite_source_mean_improvement = NA_real_,
    stringsAsFactors = FALSE
  )
  if (nrow(Wcur) > 1L) cur_meta <- cur_meta[rep(1L, nrow(Wcur)), , drop = FALSE]
  Wout <- Wcur
  meta <- cur_meta
  if (!is.null(protected_pack) && !is.null(protected_pack$weights) && nrow(as.matrix(protected_pack$weights))) {
    Wp <- as.matrix(protected_pack$weights)
    storage.mode(Wp) <- "numeric"
    if (nrow(Wp)) Wp <- t(apply(Wp, 1, .normalize_simplex))
    pm <- protected_pack$meta
    if (is.null(pm) || !is.data.frame(pm) || nrow(pm) != nrow(Wp)) {
      pm <- data.frame(
        protected_elite = TRUE,
        protected_elite_origin = "previous_stage_elite",
        protected_elite_one_stage_only = TRUE,
        protected_elite_source_gate_pass = NA,
        protected_elite_source_q95_improvement = NA_real_,
        protected_elite_source_mean_improvement = NA_real_,
        stringsAsFactors = FALSE
      )
      pm <- pm[rep(1L, nrow(Wp)), , drop = FALSE]
    }
    pm$candidate_source <- "protected_previous_stage_elite"
    Wcat <- rbind(Wout, Wp)
    meta_cat <- dplyr::bind_rows(meta, pm[, names(meta), drop = FALSE])
    sig <- apply(round(Wcat, 12), 1, paste, collapse = "|")
    keep <- !duplicated(sig)
    Wout <- Wcat[keep, , drop = FALSE]
    meta <- meta_cat[keep, , drop = FALSE]
  }
  meta$candidate_k <- seq_len(nrow(Wout))
  list(weights = Wout, meta = meta)
}

.evaluate_topk_fixed_weights_subset <- function(dist_name, dist_param_grid, weights_mat,
                                                sample_sizes, num_samples, scenario_subset,
                                                scenario_mode = "full", seed = 1,
                                                q95_B = 100L, crn_env = NULL, fam_key = NULL,
                                                rank_metric = c("robust_q95_mse", "robust_mean_mse", "robust_max_mse"),
                                                subset_tag = NULL, do_perturb = FALSE) {
  rank_metric <- match.arg(rank_metric)
  if (is.null(weights_mat) || length(weights_mat) == 0L) stop("weights_mat is empty.")
  if (is.null(scenario_subset) || !is.data.frame(scenario_subset) || !nrow(scenario_subset)) stop("scenario_subset must be non-empty.")
  weights_mat <- as.matrix(weights_mat)
  .scalar1 <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) < 1L) return(default)
    v <- suppressWarnings(as.numeric(x[[1]])); if (!is.finite(v)) return(default); v
  }
  eval_list <- vector("list", nrow(weights_mat))
  for (k in seq_len(nrow(weights_mat))) {
    eval_list[[k]] <- .evaluate_fixed_weights(
      dist_name = dist_name, dist_param_grid = dist_param_grid, weights = weights_mat[k, ],
      sample_sizes = sample_sizes, num_samples = num_samples,
      scenario_mode = scenario_mode, seed = seed + k, q95_B = q95_B,
      crn_env = crn_env, fam_key = if (is.null(fam_key)) dist_name else fam_key,
      scenario_subset = scenario_subset,
      subset_tag = subset_tag %||% paste0("REGIME_HELDOUT__", dist_name),
      use_cache = FALSE, do_perturb = do_perturb
    )
  }
  rank_df <- dplyr::bind_rows(lapply(seq_along(eval_list), function(k) {
    ov <- eval_list[[k]]$overall; pt <- eval_list[[k]]$perturbation
    tibble::tibble(candidate_k = k, estimator_str = eval_list[[k]]$estimator_str,
                   robust_mean_mse = .scalar1(ov$robust_mean_mse),
                   robust_q95_mse = .scalar1(ov$robust_q95_mse),
                   robust_max_mse = .scalar1(ov$robust_max_mse),
                   perturb_fitness_mean = .scalar1(pt$fitness_mean),
                   perturb_fitness_sd = .scalar1(pt$fitness_sd),
                   perturb_fitness_q95 = .scalar1(pt$fitness_q95))
  }))
  rank_df <- rank_df |> dplyr::arrange(.data[[rank_metric]]) |>
    dplyr::mutate(rank_pos = dplyr::row_number(), is_top1 = rank_pos == 1L)
  list(rank = rank_df, candidates = eval_list)
}


# Build a compact one-stage elite archive from a GA/CV result object.
# These candidates are protected only in the immediately following stage.
# If they do not rank well in that next stage, they are naturally removed.
.make_stage_elite_archive <- function(res_cv, family, source_stage, source_config_index, source_config_tag,
                                      source_score = NA_real_, K = 3L) {
  W <- tryCatch(.extract_topk_from_res(res_cv, K = K), error = function(e) NULL)
  if (is.null(W) || length(W) == 0L) return(data.frame())
  W <- as.matrix(W)
  if (!nrow(W)) return(data.frame())
  W <- W[seq_len(min(nrow(W), max(1L, as.integer(K)[1]))), , drop = FALSE]
  wdf <- as.data.frame(W, stringsAsFactors = FALSE)
  names(wdf) <- paste0("w_", ESTIMATOR_NAMES[seq_len(ncol(W))])
  cbind(data.frame(
    family = as.character(family)[1],
    candidate_source = paste0(source_stage, "_elite"),
    protected_elite = TRUE,
    protected_elite_origin = as.character(source_stage)[1],
    protected_elite_one_stage_only = TRUE,
    source_stage = as.character(source_stage)[1],
    source_config_index = as.integer(source_config_index)[1],
    source_config_tag = as.character(source_config_tag)[1],
    source_score = suppressWarnings(as.numeric(source_score)[1]),
    elite_rank_within_source = seq_len(nrow(W)),
    stringsAsFactors = FALSE
  ), wdf)
}

.stage_elite_matrix <- function(elite_df, max_total = 12L) {
  if (is.null(elite_df) || !is.data.frame(elite_df) || !nrow(elite_df)) return(NULL)
  wcols <- grep("^w_", names(elite_df), value = TRUE)
  if (!length(wcols)) return(NULL)
  x <- elite_df
  if ("source_score" %in% names(x)) {
    x$source_score <- suppressWarnings(as.numeric(x$source_score))
    x <- x[order(x$source_score, x$elite_rank_within_source), , drop = FALSE]
  }
  W <- as.matrix(x[, wcols, drop = FALSE])
  storage.mode(W) <- "numeric"
  W <- W[stats::complete.cases(W), , drop = FALSE]
  if (!nrow(W)) return(NULL)
  W <- unique(W)
  W <- W[seq_len(min(nrow(W), max(1L, as.integer(max_total)[1]))), , drop = FALSE]
  W
}

.make_regime_protected_elites_from_selected <- function(selected_regimes, stage_elites, max_per_regime = 3L) {
  if (is.null(selected_regimes) || !is.data.frame(selected_regimes) || !nrow(selected_regimes)) return(data.frame())
  if (is.null(stage_elites) || !is.data.frame(stage_elites) || !nrow(stage_elites)) return(data.frame())
  wcols <- grep("^w_", names(stage_elites), value = TRUE)
  if (!length(wcols)) return(data.frame())
  max_per_regime <- max(1L, as.integer(max_per_regime)[1])
  rows <- list()
  for (ii in seq_len(nrow(selected_regimes))) {
    reg <- selected_regimes[ii, , drop = FALSE]
    tag <- if ("source_config_tag" %in% names(reg)) as.character(reg$source_config_tag[1]) else NA_character_
    source_stage <- if ("source_stage" %in% names(reg)) as.character(reg$source_stage[1]) else NA_character_
    cand <- stage_elites
    if (is.finite(match("source_config_tag", names(cand))) && isTRUE(nzchar(tag))) {
      cand <- cand[as.character(cand$source_config_tag) == tag, , drop = FALSE]
    }
    if (!nrow(cand) && isTRUE(nzchar(source_stage))) {
      cand <- stage_elites[as.character(stage_elites$source_stage) == source_stage, , drop = FALSE]
    }
    if (!nrow(cand)) cand <- stage_elites
    if ("source_score" %in% names(cand)) cand <- cand[order(suppressWarnings(as.numeric(cand$source_score)), cand$elite_rank_within_source), , drop = FALSE]
    cand <- cand[seq_len(min(nrow(cand), max_per_regime)), , drop = FALSE]
    cand$distribution <- if ("distribution" %in% names(reg)) as.character(reg$distribution[1]) else as.character(reg$family[1])
    cand$regime_key <- as.character(reg$regime_key[1])
    cand$regime_class <- if ("regime_class" %in% names(reg)) as.character(reg$regime_class[1]) else NA_character_
    cand$ga_rel_improvement_q95 <- if ("ga_rel_improvement_q95" %in% names(reg)) suppressWarnings(as.numeric(reg$ga_rel_improvement_q95[1])) else NA_real_
    cand$ga_rel_improvement_mean <- if ("ga_rel_improvement_mean" %in% names(reg)) suppressWarnings(as.numeric(reg$ga_rel_improvement_mean[1])) else NA_real_
    cand$gate_pass <- if ("gate_pass_regime" %in% names(reg)) reg$gate_pass_regime[1] else NA
    cand$protected_elite_origin <- paste0(as.character(cand$source_stage), "_to_next_stage")
    cand$protected_elite_one_stage_only <- TRUE
    rows[[length(rows)+1L]] <- cand
  }
  dplyr::bind_rows(rows)
}

run_regime_specialist_ga <- function(selected_regimes, dist_name, dist_param_grid, cfg,
                                     scenario_universe_full, sample_sizes, num_samples,
                                     generations_per_fold, k_folds, objective, use_parallel,
                                     lambda_instab, bootstrap_B, t_size, elitism,
                                     immigrant_rate, mutation_rate_init, init_alpha,
                                     alpha_mut, check_every, patience, min_delta,
                                     mix_w_q95, mix_w_max, crn_env, fam_key, run_dir,
                                     finalists_per_regime = 3L, holdout_frac = 0.30,
                                     specialist_gens_frac = 0.50,
                                     specialist_num_samples_frac = 0.60,
                                     specialist_bootstrap_frac = 0.60,
                                     specialist_pop_size = NA_integer_,
                                     regime_neighborhood = TRUE,
                                     regime_neighborhood_trigger_scenarios = 10L,
                                     regime_neighborhood_max_neighbors = 3L,
                                     regime_neighborhood_max_scenarios = 24L,
                                     regime_neighborhood_max_rate_distance = 1L,
                                     regime_neighborhood_max_scale_distance = 1L,
                                     regime_neighborhood_min_promise_quantile = 0.40,
                                     regime_global_audit = TRUE,
                                     regime_global_audit_scenario_frac = 0.10,
                                     regime_global_audit_min_scenarios = 24L,
                                     regime_global_audit_num_samples = 20L,
                                     regime_global_audit_bootstrap_B = 40L,
                                     min_unique_scenarios = 6L,
                                     min_train_scenarios = 5L,
                                     min_heldout_scenarios = 1L,
                                     protected_elites = NULL,
                                     protected_elite_max_per_regime = 3L,
                                     seed_offset = 70000L) {
  if (is.null(selected_regimes) || !is.data.frame(selected_regimes) || !nrow(selected_regimes)) return(data.frame())
  selected <- selected_regimes[selected_regimes$selected_for_specialist_training %in% TRUE, , drop = FALSE]
  if (!nrow(selected)) return(data.frame())
  sc_all <- as.data.frame(scenario_universe_full)
  if (!"scenario_id" %in% names(sc_all)) sc_all <- add_scenario_ids(sc_all)
  sc_all <- .add_regime_columns(sc_all, distribution = dist_name, include_sample_size_bin = FALSE)
  finalists_per_regime <- max(1L, as.integer(finalists_per_regime)[1])
  holdout_frac <- as.numeric(holdout_frac)[1]
  if (!is.finite(holdout_frac) || holdout_frac <= 0 || holdout_frac >= 1) holdout_frac <- 0.30
  min_unique_scenarios <- max(1L, as.integer(min_unique_scenarios)[1])
  min_train_scenarios <- max(1L, as.integer(min_train_scenarios)[1])
  min_heldout_scenarios <- max(1L, as.integer(min_heldout_scenarios)[1])
  base_seed <- .ensure_seed(as.integer(cfg$seed)[1], fallback = 101L)
  pop_s <- suppressWarnings(as.integer(specialist_pop_size)[1])
  if (!is.finite(pop_s) || pop_s < 2L) pop_s <- max(20L, as.integer(cfg$pop_size)[1])
  gens_s <- max(5L, as.integer(ceiling(generations_per_fold * specialist_gens_frac)))
  ns_s <- max(5L, as.integer(ceiling(num_samples * specialist_num_samples_frac)))
  B_s <- max(20L, as.integer(ceiling(bootstrap_B * specialist_bootstrap_frac)))
  regime_neighborhood <- isTRUE(regime_neighborhood)
  regime_neighborhood_trigger_scenarios <- max(min_unique_scenarios, as.integer(regime_neighborhood_trigger_scenarios)[1])
  regime_neighborhood_max_neighbors <- max(0L, as.integer(regime_neighborhood_max_neighbors)[1])
  regime_neighborhood_max_scenarios <- max(min_unique_scenarios, as.integer(regime_neighborhood_max_scenarios)[1])
  regime_neighborhood_max_rate_distance <- max(0L, as.integer(regime_neighborhood_max_rate_distance)[1])
  regime_neighborhood_max_scale_distance <- max(0L, as.integer(regime_neighborhood_max_scale_distance)[1])
  regime_neighborhood_min_promise_quantile <- as.numeric(regime_neighborhood_min_promise_quantile)[1]
  if (!is.finite(regime_neighborhood_min_promise_quantile) || regime_neighborhood_min_promise_quantile < 0 || regime_neighborhood_min_promise_quantile > 1) {
    regime_neighborhood_min_promise_quantile <- 0.50
  }
  regime_global_audit <- isTRUE(regime_global_audit)
  regime_global_audit_scenario_frac <- suppressWarnings(as.numeric(regime_global_audit_scenario_frac)[1])
  if (!is.finite(regime_global_audit_scenario_frac) || regime_global_audit_scenario_frac <= 0 || regime_global_audit_scenario_frac > 1) {
    regime_global_audit_scenario_frac <- 0.10
  }
  regime_global_audit_min_scenarios <- max(1L, suppressWarnings(as.integer(regime_global_audit_min_scenarios)[1]))
  regime_global_audit_num_samples <- max(5L, suppressWarnings(as.integer(regime_global_audit_num_samples)[1]))
  regime_global_audit_bootstrap_B <- max(20L, suppressWarnings(as.integer(regime_global_audit_bootstrap_B)[1]))
  audit_cols <- c("experimental_condition_key", "condition_summary", "shape_regime", "tail_regime", "mechanism_regime",
                  "contamination_regime", "contamination_direction", "contamination_structure", "rate_regime", "scale_regime", "scale_group_regime",
                  "rate_regime_order", "scale_regime_order", "scale_group_regime_order", "severity_regime", "sample_size_regime",
                  "exact_contamination_rates", "exact_outlier_scales", "exact_contamination_types", "exact_sample_sizes")
  audit_value <- function(reg, col) if (col %in% names(reg) && length(reg[[col]]) > 0L) as.character(reg[[col]][1]) else NA_character_
  audit_frame <- function(reg) as.data.frame(setNames(lapply(audit_cols, function(cc) audit_value(reg, cc)), audit_cols), stringsAsFactors = FALSE)
  add_audit_columns <- function(df, reg) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
    for (cc in audit_cols) if (!cc %in% names(df)) df[[cc]] <- audit_value(reg, cc)
    df
  }
  summary_row <- function(reg, ...) cbind(data.frame(..., stringsAsFactors = FALSE), audit_frame(reg))
  weight_frame <- function(w) {
    nm <- if (exists("ESTIMATOR_NAMES", inherits = TRUE)) ESTIMATOR_NAMES else paste0("est", seq_along(w))
    nm <- make.names(nm, unique = TRUE)
    w <- as.numeric(w)
    if (!length(w)) w <- rep(NA_real_, length(nm))
    if (length(w) < length(nm)) w <- c(w, rep(NA_real_, length(nm) - length(w)))
    if (length(w) > length(nm)) w <- w[seq_along(nm)]
    as.data.frame(setNames(as.list(w), paste0("w_", nm)), stringsAsFactors = FALSE)
  }
  add_weight_columns <- function(df, w) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
    wf <- weight_frame(w)
    for (cc in names(wf)) df[[cc]] <- wf[[cc]][1]
    df$winner_weight_vector <- paste(round(as.numeric(w), 8), collapse = ";")
    df
  }
  run_global_audit <- function(w, reg, reg_id, heldout_gate, heldout_rank, source_subset = NULL) {
    # Profile-matched audit for fixed-weight local generalization.
    # This replaces the old idea of cross-family transfer/global transfer matrices.
    # The winner's fixed weights are evaluated without retraining on new scenarios
    # inside the same family and near the selected regime profile. This tests
    # intra-regime / intra-family / profile-matched generalization rather than
    # asking a specialist estimator to be universal across unrelated families.
    empty <- list(ok = FALSE, summary = data.frame(), scenarios = data.frame())
    if (!isTRUE(regime_global_audit)) return(empty)
    w <- as.numeric(w)
    if (!length(w) || any(!is.finite(w))) return(empty)

    audit_seed <- .stable_int_seed(paste0(dist_name, "::", reg_id, "::profile_matched_audit"),
                                   base_seed = base_seed + seed_offset + 30000L)

    profile_pool <- sc_all
    if (!"scenario_id" %in% names(profile_pool)) profile_pool <- add_scenario_ids(profile_pool)
    if (!"regime_key" %in% names(profile_pool)) {
      profile_pool <- .add_regime_columns(profile_pool, distribution = dist_name, include_sample_size_bin = FALSE)
    }

    exclude_ids <- character(0)
    if (!is.null(source_subset) && is.data.frame(source_subset) && "scenario_id" %in% names(source_subset)) {
      exclude_ids <- unique(as.character(source_subset$scenario_id))
    }

    target_direction <- .first_chr(reg, "contamination_direction")
    target_structure <- .first_chr(reg, "contamination_structure")
    target_rate_order <- .first_int(reg, "rate_regime_order")
    target_scale_order <- .first_int(reg, "scale_group_regime_order")
    scale_order_col <- "scale_group_regime_order"
    if (!is.finite(target_scale_order) || !scale_order_col %in% names(profile_pool)) {
      target_scale_order <- .first_int(reg, "scale_regime_order")
      scale_order_col <- "scale_regime_order"
    }

    # If ordinals are unavailable, keep the audit conservative: profile match by
    # family + direction/structure only, still excluding source scenarios.
    same_family <- as.character(profile_pool$distribution) == as.character(dist_name)
    not_source <- if ("scenario_id" %in% names(profile_pool) && length(exclude_ids)) {
      !(as.character(profile_pool$scenario_id) %in% exclude_ids)
    } else rep(TRUE, nrow(profile_pool))

    rate_close <- if ("rate_regime_order" %in% names(profile_pool) && is.finite(target_rate_order)) {
      abs(suppressWarnings(as.integer(profile_pool$rate_regime_order)) - target_rate_order) <= regime_neighborhood_max_rate_distance
    } else rep(TRUE, nrow(profile_pool))
    scale_close <- if (scale_order_col %in% names(profile_pool) && is.finite(target_scale_order)) {
      abs(suppressWarnings(as.integer(profile_pool[[scale_order_col]])) - target_scale_order) <= regime_neighborhood_max_scale_distance
    } else rep(TRUE, nrow(profile_pool))
    same_direction <- if ("contamination_direction" %in% names(profile_pool) && nzchar(target_direction) && !is.na(target_direction)) {
      as.character(profile_pool$contamination_direction) == target_direction
    } else rep(TRUE, nrow(profile_pool))
    same_structure <- if ("contamination_structure" %in% names(profile_pool) && nzchar(target_structure) && !is.na(target_structure)) {
      as.character(profile_pool$contamination_structure) == target_structure
    } else rep(TRUE, nrow(profile_pool))

    candidates <- profile_pool[same_family & not_source & same_direction & same_structure & rate_close & scale_close, , drop = FALSE]
    profile_match_level <- "family_direction_structure_adjacent_rate_scale"

    if (nrow(candidates) < regime_global_audit_min_scenarios) {
      candidates <- profile_pool[same_family & not_source & same_direction & rate_close & scale_close, , drop = FALSE]
      profile_match_level <- "family_direction_adjacent_rate_scale"
    }
    if (nrow(candidates) < regime_global_audit_min_scenarios) {
      candidates <- profile_pool[same_family & not_source & rate_close & scale_close, , drop = FALSE]
      profile_match_level <- "family_adjacent_rate_scale"
    }
    if (nrow(candidates) < max(3L, min_heldout_scenarios)) {
      candidates <- profile_pool[same_family & not_source, , drop = FALSE]
      profile_match_level <- "family_only_fallback"
    }
    if (is.null(candidates) || !is.data.frame(candidates) || !nrow(candidates)) return(empty)

    profile_available <- nrow(candidates)
    target_n <- min(
      profile_available,
      max(1L, regime_global_audit_min_scenarios,
          as.integer(ceiling(profile_available * regime_global_audit_scenario_frac)))
    )
    set.seed(audit_seed)
    keep_ix <- if (profile_available > target_n) sample(seq_len(profile_available), target_n) else seq_len(profile_available)
    audit_subset <- candidates[keep_ix, , drop = FALSE]
    audit_subset$profile_match_level <- profile_match_level
    audit_subset$profile_audit_source_regime_id <- reg_id
    audit_subset$profile_audit_source_regime_key <- as.character(reg$regime_key[1])

    audit_eval <- tryCatch(
      .evaluate_topk_fixed_weights_subset(dist_name, dist_param_grid, matrix(w, nrow = 1),
        sample_sizes = sample_sizes, num_samples = regime_global_audit_num_samples,
        scenario_subset = audit_subset, scenario_mode = "full", seed = audit_seed,
        q95_B = regime_global_audit_bootstrap_B, crn_env = crn_env, fam_key = fam_key,
        rank_metric = "robust_q95_mse",
        subset_tag = paste0("REGIME_PROFILE_MATCHED_AUDIT__", dist_name, "__", reg_id),
        do_perturb = FALSE),
      error = function(e) NULL
    )
    if (is.null(audit_eval) || is.null(audit_eval$rank) || !nrow(audit_eval$rank)) return(empty)
    gate_a <- tryCatch(
      benchmark_gate_from_scenario_table(audit_eval$candidates[[1]]$scenario_table,
                                         distribution = dist_name,
                                         min_rel_improvement = 0.00,
                                         ga_estimator_name = "robust"),
      error = function(e) NULL
    )
    if (is.null(gate_a)) return(empty)

    held_q95 <- suppressWarnings(as.numeric(heldout_rank$robust_q95_mse[1]))
    held_mean <- suppressWarnings(as.numeric(heldout_rank$robust_mean_mse[1]))
    aud_q95 <- suppressWarnings(as.numeric(audit_eval$rank$robust_q95_mse[1]))
    aud_mean <- suppressWarnings(as.numeric(audit_eval$rank$robust_mean_mse[1]))
    local_pass <- isTRUE(heldout_gate$gate_pass)
    profile_pass <- isTRUE(gate_a$gate_pass)
    interp <- if (local_pass && !profile_pass) {
      "regime_specific_profile_no_free_lunch"
    } else if (local_pass && profile_pass) {
      "profile_matched_generalization_bonus"
    } else if (!local_pass && profile_pass) {
      "profile_only_unexpected"
    } else {
      "benchmark_dominant_local_and_profile"
    }

    summ <- data.frame(
      # New preferred terminology
      profile_audit_run = TRUE,
      profile_audit_scope = "intra_family_profile_matched_fixed_weight_no_retraining",
      profile_match_level = profile_match_level,
      profile_audit_n_available = profile_available,
      profile_audit_n_scenarios = nrow(audit_subset),
      profile_audit_excluded_source_scenarios = length(exclude_ids),
      profile_audit_scenario_frac = regime_global_audit_scenario_frac,
      profile_audit_min_scenarios = regime_global_audit_min_scenarios,
      profile_audit_num_samples = regime_global_audit_num_samples,
      profile_audit_bootstrap_B = regime_global_audit_bootstrap_B,
      profile_audit_seed = audit_seed,
      profile_audit_ga_q95_mse = aud_q95,
      profile_audit_ga_mean_mse = aud_mean,
      heldout_ga_q95_mse = held_q95,
      heldout_ga_mean_mse = held_mean,
      profile_to_heldout_q95_ratio = aud_q95 / (abs(held_q95) + 1e-12),
      profile_to_heldout_mean_ratio = aud_mean / (abs(held_mean) + 1e-12),
      profile_audit_gate_pass = profile_pass,
      profile_audit_final_selected_type = gate_a$final_selected_type,
      profile_audit_final_selected_estimator = gate_a$final_selected_estimator,
      profile_audit_best_benchmark_q95_estimator = gate_a$best_benchmark_q95_estimator,
      profile_audit_best_benchmark_mean_estimator = gate_a$best_benchmark_mean_estimator,
      profile_audit_best_benchmark_q95_mse = gate_a$best_benchmark_q95_mse,
      profile_audit_best_benchmark_mean_mse = gate_a$best_benchmark_mean_mse,
      profile_audit_rel_improvement_q95 = gate_a$ga_rel_improvement_q95,
      profile_audit_rel_improvement_mean = gate_a$ga_rel_improvement_mean,
      profile_audit_interpretation = interp,
      # Backward-compatible aliases retained so downstream summaries do not break.
      global_audit_run = TRUE,
      global_audit_scenario_frac = regime_global_audit_scenario_frac,
      global_audit_min_scenarios = regime_global_audit_min_scenarios,
      global_audit_n_scenarios = nrow(audit_subset),
      global_audit_num_samples = regime_global_audit_num_samples,
      global_audit_bootstrap_B = regime_global_audit_bootstrap_B,
      global_audit_seed = audit_seed,
      global_audit_ga_q95_mse = aud_q95,
      global_audit_ga_mean_mse = aud_mean,
      global_to_heldout_q95_ratio = aud_q95 / (abs(held_q95) + 1e-12),
      global_to_heldout_mean_ratio = aud_mean / (abs(held_mean) + 1e-12),
      global_audit_gate_pass = profile_pass,
      global_audit_final_selected_type = gate_a$final_selected_type,
      global_audit_final_selected_estimator = gate_a$final_selected_estimator,
      global_audit_best_benchmark_q95_estimator = gate_a$best_benchmark_q95_estimator,
      global_audit_best_benchmark_mean_estimator = gate_a$best_benchmark_mean_estimator,
      global_audit_best_benchmark_q95_mse = gate_a$best_benchmark_q95_mse,
      global_audit_best_benchmark_mean_mse = gate_a$best_benchmark_mean_mse,
      global_audit_rel_improvement_q95 = gate_a$ga_rel_improvement_q95,
      global_audit_rel_improvement_mean = gate_a$ga_rel_improvement_mean,
      global_audit_interpretation = interp,
      stringsAsFactors = FALSE
    )
    scen <- audit_eval$candidates[[1]]$scenario_table
    scen$specialist_regime_id <- reg_id
    scen$regime_key <- as.character(reg$regime_key[1])
    scen$profile_audit_gate_pass <- profile_pass
    scen$profile_audit_interpretation <- interp
    scen$profile_match_level <- profile_match_level
    scen$global_audit_gate_pass <- profile_pass
    scen$global_audit_interpretation <- interp
    scen <- add_audit_columns(scen, reg)
    scen <- add_weight_columns(scen, w)
    list(ok = TRUE, summary = summ, scenarios = scen)
  }

  summary_rows <- list()
  for (rr in seq_len(nrow(selected))) {
    reg <- selected[rr, , drop = FALSE]
    # Regime-first compatibility: when the selected regime carries the HPF2
    # source configuration, specialize with that config instead of one global
    # fallback config. This lets HPF1/HPF2 select both regimes and candidates.
    cfg_eff <- cfg
    .ov_num <- function(col, target) {
      if (col %in% names(reg) && length(reg[[col]]) > 0L) {
        v <- suppressWarnings(as.numeric(reg[[col]][1])); if (is.finite(v)) cfg_eff[[target]] <<- v
      }
    }
    .ov_int <- function(col, target) {
      if (col %in% names(reg) && length(reg[[col]]) > 0L) {
        v <- suppressWarnings(as.integer(reg[[col]][1])); if (is.finite(v)) cfg_eff[[target]] <<- v
      }
    }
    .ov_int("source_pop_size", "pop_size")
    .ov_num("source_mutation_rate_init", "mutation_rate_init")
    .ov_num("source_init_alpha", "init_alpha")
    .ov_num("source_alpha_mut", "alpha_mut")
    .ov_num("source_immigrant_rate", "immigrant_rate")
    .ov_int("source_t_size", "t_size")
    .ov_int("source_elitism", "elitism")
    .ov_int("source_seed", "seed")
    .ov_num("source_lambda_instab", "lambda_instab")
    reg_key <- as.character(reg$regime_key[1])
    reg_id <- sprintf("REG%02d", rr)
    sc_exact <- sc_all[sc_all$regime_key == reg_key, , drop = FALSE]
    neighborhood_seed <- .stable_int_seed(paste0(dist_name, "::", reg_key, "::neighborhood"), base_seed = base_seed + seed_offset)
    should_use_neighborhood <- isTRUE(regime_neighborhood) && is.data.frame(sc_exact) && nrow(sc_exact) > 0L && nrow(sc_exact) < regime_neighborhood_trigger_scenarios
    sc_reg <- if (isTRUE(should_use_neighborhood)) {
      .expand_regime_with_elite_neighbors(sc_all, reg, selected_regimes, dist_name,
                                          max_neighbors = regime_neighborhood_max_neighbors,
                                          max_rate_distance = regime_neighborhood_max_rate_distance,
                                          max_scale_distance = regime_neighborhood_max_scale_distance,
                                          max_total_scenarios = regime_neighborhood_max_scenarios,
                                          min_promise_quantile = regime_neighborhood_min_promise_quantile,
                                          seed = neighborhood_seed)
    } else sc_exact
    n_exact_scenarios <- nrow(sc_exact)
    if (is.null(sc_reg) || !is.data.frame(sc_reg) || !nrow(sc_reg)) {
      sc_reg <- sc_exact
      if (nrow(sc_reg) && !"neighborhood_skip_reason" %in% names(sc_reg)) sc_reg$neighborhood_skip_reason <- "neighborhood_empty_fallback_exact"
    }
    n_regime_scenarios <- nrow(sc_reg)
    n_neighbor_scenarios <- max(0L, n_regime_scenarios - n_exact_scenarios)
    n_neighbor_regimes <- if ("regime_key" %in% names(sc_reg)) length(setdiff(unique(sc_reg$regime_key), reg_key)) else 0L
    base_seed_rr <- .ensure_seed(as.integer(cfg_eff$seed)[1], fallback = base_seed)
    pop_s_rr <- suppressWarnings(as.integer(specialist_pop_size)[1])
    if (!is.finite(pop_s_rr) || pop_s_rr < 2L) pop_s_rr <- max(20L, as.integer(cfg_eff$pop_size)[1])
    if (n_regime_scenarios < min_unique_scenarios) {
      summary_rows[[length(summary_rows)+1L]] <- summary_row(reg, distribution=dist_name, specialist_regime_id=reg_id, regime_key=reg_key,
        status="skipped_insufficient_unique_scenarios", skip_reason="insufficient_unique_scenarios",
        n_scenarios=n_regime_scenarios, n_exact_scenarios=n_exact_scenarios, n_neighbor_scenarios=n_neighbor_scenarios,
        n_neighbor_regimes=n_neighbor_regimes, neighborhood_used=isTRUE(should_use_neighborhood),
        neighborhood_trigger_scenarios=regime_neighborhood_trigger_scenarios,
        n_train_scenarios=NA_integer_, n_heldout_scenarios=NA_integer_)
      next
    }
    split_seed <- .stable_int_seed(paste0(dist_name, "::", reg_key, "::specialist"), base_seed = base_seed + seed_offset)
    split <- .split_regime_train_heldout(sc_reg, holdout_frac, k_folds, split_seed, min_train_scenarios, min_heldout_scenarios)
    train_subset <- split$train; holdout_subset <- split$heldout
    if (!identical(split$status, "ok") || nrow(train_subset) < min_train_scenarios || nrow(holdout_subset) < min_heldout_scenarios) {
      summary_rows[[length(summary_rows)+1L]] <- summary_row(reg, distribution=dist_name, specialist_regime_id=reg_id, regime_key=reg_key,
        status=split$status, skip_reason=split$status, n_scenarios=n_regime_scenarios,
        n_exact_scenarios=n_exact_scenarios, n_neighbor_scenarios=n_neighbor_scenarios, n_neighbor_regimes=n_neighbor_regimes,
        neighborhood_used=isTRUE(should_use_neighborhood), neighborhood_trigger_scenarios=regime_neighborhood_trigger_scenarios,
        n_train_scenarios=nrow(train_subset), n_heldout_scenarios=nrow(holdout_subset))
      next
    }
    safe_write_csv(train_subset, file.path(run_dir, sprintf("REGIME_SPECIALIST_TRAIN_SCENARIOS__seed%d__%s.csv", base_seed, reg_id)))
    safe_write_csv(holdout_subset, file.path(run_dir, sprintf("REGIME_SPECIALIST_HELDOUT_SCENARIOS__seed%d__%s.csv", base_seed, reg_id)))
    protected_pack <- .extract_protected_elites_for_regime(
      protected_elites = protected_elites,
      regime_key = reg_key,
      max_per_regime = protected_elite_max_per_regime,
      stage_origin = "previous_stage_elite"
    )
    res_sp <- tryCatch(evolve_universal_estimator_per_family_cv(
      dist_name=dist_name, dist_param_grid=dist_param_grid, sample_sizes=sample_sizes, num_samples=ns_s,
      pop_size=pop_s_rr, generations_per_fold=gens_s, seed=base_seed_rr+seed_offset+rr, objective=objective,
      use_parallel=use_parallel, k_folds=k_folds, lambda_instab=cfg_eff$lambda_instab, bootstrap_B=B_s,
      t_size=cfg_eff$t_size, elitism=cfg_eff$elitism, immigrant_rate=cfg_eff$immigrant_rate, mutation_rate_init=cfg_eff$mutation_rate_init,
      init_alpha=cfg_eff$init_alpha, alpha_mut=cfg_eff$alpha_mut, check_every=check_every, patience=patience,
      min_delta=min_delta, final_retrain=FALSE, mix_w_q95=mix_w_q95, mix_w_max=mix_w_max,
      crn_env=crn_env, fam_key=fam_key, force_scenario_mode="full", scenario_frac=1.0,
      scenario_seed=split_seed, subset_tag=paste0("REGIME_SPECIALIST_TRAIN__", dist_name, "__", reg_id),
      scenario_universe=scenario_universe_full, scenario_subset_override=train_subset,
      run_dir=run_dir, stage_name=paste0("REGIME_SPECIALIST_", reg_id), checkpoint_dir=run_dir,
      protected_elite_matrix = if (!is.null(protected_pack) && !is.null(protected_pack$weights)) protected_pack$weights else NULL),
      error=function(e){cat(sprintf("[%s] WARNING: specialist GA failed for %s: %s\n", dist_name, reg_id, conditionMessage(e))); NULL})
    topk_w <- .extract_topk_from_res(res_sp, K=finalists_per_regime)
    if (is.null(topk_w) || length(topk_w)==0L) {
      summary_rows[[length(summary_rows)+1L]] <- summary_row(reg, distribution=dist_name, specialist_regime_id=reg_id, regime_key=reg_key,
        status="skipped_no_finalists", skip_reason="no_finalists_extracted", n_scenarios=nrow(sc_reg), n_exact_scenarios=n_exact_scenarios,
        n_neighbor_scenarios=n_neighbor_scenarios, n_neighbor_regimes=n_neighbor_regimes, neighborhood_used=isTRUE(should_use_neighborhood),
        neighborhood_trigger_scenarios=regime_neighborhood_trigger_scenarios, n_train_scenarios=nrow(train_subset), n_heldout_scenarios=nrow(holdout_subset))
      next
    }
    topk_w <- as.matrix(topk_w)
    if (nrow(topk_w)>finalists_per_regime) topk_w <- topk_w[seq_len(finalists_per_regime), , drop=FALSE]
    combined_pack <- .combine_current_and_protected_weights(
      current_weights = topk_w,
      protected_pack = protected_pack,
      current_origin = "current_specialist_finalist"
    )
    topk_w <- combined_pack$weights
    candidate_meta <- combined_pack$meta
    eval_sp <- tryCatch(.evaluate_topk_fixed_weights_subset(dist_name, dist_param_grid, topk_w, sample_sizes, num_samples, holdout_subset,
      scenario_mode="full", seed=base_seed_rr+seed_offset+1000L+rr, q95_B=bootstrap_B, crn_env=crn_env, fam_key=fam_key,
      rank_metric="robust_q95_mse", subset_tag=paste0("REGIME_SPECIALIST_HELDOUT__", dist_name, "__", reg_id), do_perturb=FALSE),
      error=function(e){cat(sprintf("[%s] WARNING: specialist heldout evaluation failed for %s: %s\n", dist_name, reg_id, conditionMessage(e))); NULL})
    if (is.null(eval_sp) || is.null(eval_sp$rank) || !nrow(eval_sp$rank)) {
      summary_rows[[length(summary_rows)+1L]] <- summary_row(reg, distribution=dist_name, specialist_regime_id=reg_id, regime_key=reg_key,
        status="skipped_no_heldout_eval", skip_reason="heldout_eval_failed", n_scenarios=nrow(sc_reg), n_exact_scenarios=n_exact_scenarios,
        n_neighbor_scenarios=n_neighbor_scenarios, n_neighbor_regimes=n_neighbor_regimes, neighborhood_used=isTRUE(should_use_neighborhood),
        neighborhood_trigger_scenarios=regime_neighborhood_trigger_scenarios, n_train_scenarios=nrow(train_subset), n_heldout_scenarios=nrow(holdout_subset))
      next
    }
    rank_sp <- eval_sp$rank |> dplyr::mutate(specialist_regime_id=reg_id, regime_key=reg_key,
      selected_from_regime_class=as.character(reg$regime_class[1]), selected_from_promise_score=suppressWarnings(as.numeric(reg$promise_score[1])))
    rank_sp <- add_audit_columns(rank_sp, reg)
    if (exists("candidate_meta", inherits = FALSE) && is.data.frame(candidate_meta) && nrow(candidate_meta)) {
      rank_sp <- dplyr::left_join(rank_sp, candidate_meta, by = "candidate_k")
    }
    if (!"candidate_source" %in% names(rank_sp)) rank_sp$candidate_source <- "current_specialist_finalist"
    if (!"protected_elite" %in% names(rank_sp)) rank_sp$protected_elite <- FALSE
    rank_sp$protected_elite[is.na(rank_sp$protected_elite)] <- FALSE

    # Heldout gate over Top-K finalists: evaluate each fixed finalist on the
    # heldout scenarios, then select the first candidate that passes the same
    # benchmark gate. If none pass, keep the q95-ranked best candidate. This
    # preserves a predeclared finalist set without retraining or cherry-picking.
    candidate_gate_rows <- list()
    if (!is.null(eval_sp$candidates) && length(eval_sp$candidates)) {
      for (kk in seq_along(eval_sp$candidates)) {
        scen_kk <- eval_sp$candidates[[kk]]$scenario_table
        if (is.null(scen_kk) || !is.data.frame(scen_kk) || !nrow(scen_kk)) next
        gate_kk <- tryCatch(benchmark_gate_from_scenario_table(scen_kk, distribution=dist_name,
                                                               min_rel_improvement=0.00,
                                                               ga_estimator_name="robust"),
                             error=function(e) NULL)
        if (is.null(gate_kk)) next
        candidate_gate_rows[[length(candidate_gate_rows)+1L]] <- data.frame(
          candidate_k = kk,
          candidate_gate_pass = isTRUE(gate_kk$gate_pass),
          candidate_rel_improvement_q95 = suppressWarnings(as.numeric(gate_kk$ga_rel_improvement_q95)),
          candidate_rel_improvement_mean = suppressWarnings(as.numeric(gate_kk$ga_rel_improvement_mean)),
          candidate_final_selected_type = as.character(gate_kk$final_selected_type),
          candidate_final_selected_estimator = as.character(gate_kk$final_selected_estimator),
          stringsAsFactors = FALSE
        )
      }
    }
    candidate_gates <- if (length(candidate_gate_rows)) dplyr::bind_rows(candidate_gate_rows) else data.frame()
    if (nrow(candidate_gates)) {
      rank_sp <- dplyr::left_join(rank_sp, candidate_gates, by = "candidate_k")
    } else {
      rank_sp$candidate_gate_pass <- FALSE
      rank_sp$candidate_rel_improvement_q95 <- NA_real_
      rank_sp$candidate_rel_improvement_mean <- NA_real_
    }
    rank_sp$candidate_gate_pass[is.na(rank_sp$candidate_gate_pass)] <- FALSE
    rank_sp$.candidate_q95 <- suppressWarnings(as.numeric(rank_sp$candidate_rel_improvement_q95))
    rank_sp$.candidate_mean <- suppressWarnings(as.numeric(rank_sp$candidate_rel_improvement_mean))
    rank_sp$.candidate_q95[!is.finite(rank_sp$.candidate_q95)] <- -Inf
    rank_sp$.candidate_mean[!is.finite(rank_sp$.candidate_mean)] <- -Inf
    rank_sp$.heldout_selection_score <- ifelse(rank_sp$candidate_gate_pass, 1e6, 0) + 0.75*rank_sp$.candidate_q95 + 0.25*rank_sp$.candidate_mean
    rank_sp <- rank_sp[order(-rank_sp$candidate_gate_pass, -rank_sp$.heldout_selection_score, rank_sp$robust_q95_mse, rank_sp$candidate_k), , drop=FALSE]
    rank_sp$heldout_topk_selection_rank <- seq_len(nrow(rank_sp))
    rank_sp$heldout_topk_selection_reason <- ifelse(rank_sp$candidate_gate_pass, "first_topk_candidate_passing_benchmark_gate", "best_topk_candidate_by_q95_weighted_score")
    ga_k <- suppressWarnings(as.integer(rank_sp$candidate_k[1]))
    if (!is.finite(ga_k) || ga_k < 1L || ga_k > nrow(topk_w)) {
      summary_rows[[length(summary_rows)+1L]] <- summary_row(reg, distribution=dist_name, specialist_regime_id=reg_id, regime_key=reg_key,
        status="skipped_invalid_candidate_index", skip_reason="invalid_candidate_index", n_scenarios=nrow(sc_reg), n_exact_scenarios=n_exact_scenarios,
        n_neighbor_scenarios=n_neighbor_scenarios, n_neighbor_regimes=n_neighbor_regimes, neighborhood_used=isTRUE(should_use_neighborhood),
        neighborhood_trigger_scenarios=regime_neighborhood_trigger_scenarios, n_train_scenarios=nrow(train_subset), n_heldout_scenarios=nrow(holdout_subset))
      next
    }
    winner_w <- as.numeric(topk_w[ga_k, ])
    if (!length(winner_w) || all(!is.finite(winner_w))) {
      summary_rows[[length(summary_rows)+1L]] <- summary_row(reg, distribution=dist_name, specialist_regime_id=reg_id, regime_key=reg_key,
        status="skipped_invalid_weight_vector", skip_reason="invalid_weight_vector", n_scenarios=nrow(sc_reg), n_exact_scenarios=n_exact_scenarios,
        n_neighbor_scenarios=n_neighbor_scenarios, n_neighbor_regimes=n_neighbor_regimes, neighborhood_used=isTRUE(should_use_neighborhood),
        neighborhood_trigger_scenarios=regime_neighborhood_trigger_scenarios, n_train_scenarios=nrow(train_subset), n_heldout_scenarios=nrow(holdout_subset))
      next
    }
    rank_sp <- add_weight_columns(rank_sp, winner_w)
    gate_sp <- benchmark_gate_from_scenario_table(eval_sp$candidates[[ga_k]]$scenario_table, distribution=dist_name,
                                                  min_rel_improvement=0.00, ga_estimator_name="robust")
    global_audit <- run_global_audit(winner_w, reg, reg_id, gate_sp, rank_sp[1, , drop = FALSE], source_subset = sc_reg)
    global_audit_summary <- if (isTRUE(global_audit$ok) && nrow(global_audit$summary)) global_audit$summary else data.frame(
      profile_audit_run = FALSE,
      profile_audit_scope = "intra_family_profile_matched_fixed_weight_no_retraining",
      profile_match_level = NA_character_,
      profile_audit_n_available = NA_integer_,
      profile_audit_n_scenarios = NA_integer_,
      profile_audit_excluded_source_scenarios = NA_integer_,
      profile_audit_scenario_frac = regime_global_audit_scenario_frac,
      profile_audit_min_scenarios = regime_global_audit_min_scenarios,
      profile_audit_num_samples = regime_global_audit_num_samples,
      profile_audit_bootstrap_B = regime_global_audit_bootstrap_B,
      profile_audit_seed = NA_integer_,
      profile_audit_ga_q95_mse = NA_real_,
      profile_audit_ga_mean_mse = NA_real_,
      profile_to_heldout_q95_ratio = NA_real_,
      profile_to_heldout_mean_ratio = NA_real_,
      profile_audit_gate_pass = FALSE,
      profile_audit_final_selected_type = NA_character_,
      profile_audit_final_selected_estimator = NA_character_,
      profile_audit_best_benchmark_q95_estimator = NA_character_,
      profile_audit_best_benchmark_mean_estimator = NA_character_,
      profile_audit_best_benchmark_q95_mse = NA_real_,
      profile_audit_best_benchmark_mean_mse = NA_real_,
      profile_audit_rel_improvement_q95 = NA_real_,
      profile_audit_rel_improvement_mean = NA_real_,
      profile_audit_interpretation = "profile_audit_not_run",
      global_audit_run = FALSE,
      global_audit_scenario_frac = regime_global_audit_scenario_frac,
      global_audit_min_scenarios = regime_global_audit_min_scenarios,
      global_audit_n_scenarios = NA_integer_,
      global_audit_num_samples = regime_global_audit_num_samples,
      global_audit_bootstrap_B = regime_global_audit_bootstrap_B,
      global_audit_seed = NA_integer_,
      global_audit_ga_q95_mse = NA_real_,
      global_audit_ga_mean_mse = NA_real_,
      heldout_ga_q95_mse = suppressWarnings(as.numeric(rank_sp$robust_q95_mse[1])),
      heldout_ga_mean_mse = suppressWarnings(as.numeric(rank_sp$robust_mean_mse[1])),
      global_to_heldout_q95_ratio = NA_real_,
      global_to_heldout_mean_ratio = NA_real_,
      global_audit_gate_pass = FALSE,
      global_audit_final_selected_type = NA_character_,
      global_audit_final_selected_estimator = NA_character_,
      global_audit_best_benchmark_q95_estimator = NA_character_,
      global_audit_best_benchmark_mean_estimator = NA_character_,
      global_audit_best_benchmark_q95_mse = NA_real_,
      global_audit_best_benchmark_mean_mse = NA_real_,
      global_audit_rel_improvement_q95 = NA_real_,
      global_audit_rel_improvement_mean = NA_real_,
      global_audit_interpretation = "global_audit_not_run",
      stringsAsFactors = FALSE
    )
    audit_col <- function(nm, default = NA) {
      if (nm %in% names(global_audit_summary) && length(global_audit_summary[[nm]]) > 0L) {
        global_audit_summary[[nm]][1]
      } else default
    }
    rank_sp <- rank_sp |> dplyr::mutate(gate_pass=gate_sp$gate_pass, final_selected_type=gate_sp$final_selected_type,
      final_selected_estimator=gate_sp$final_selected_estimator, best_benchmark_q95_estimator=gate_sp$best_benchmark_q95_estimator,
      best_benchmark_mean_estimator=gate_sp$best_benchmark_mean_estimator, ga_rel_improvement_q95=gate_sp$ga_rel_improvement_q95,
      ga_rel_improvement_mean=gate_sp$ga_rel_improvement_mean, neighborhood_used=isTRUE(should_use_neighborhood),
      n_exact_scenarios=n_exact_scenarios, n_neighbor_scenarios=n_neighbor_scenarios, n_neighbor_regimes=n_neighbor_regimes,
      profile_audit_run=audit_col("profile_audit_run", FALSE),
      profile_match_level=audit_col("profile_match_level", NA_character_),
      profile_audit_gate_pass=audit_col("profile_audit_gate_pass", FALSE),
      profile_audit_interpretation=audit_col("profile_audit_interpretation", NA_character_),
      profile_audit_rel_improvement_q95=audit_col("profile_audit_rel_improvement_q95", NA_real_),
      profile_audit_rel_improvement_mean=audit_col("profile_audit_rel_improvement_mean", NA_real_),
      global_audit_run=global_audit_summary$global_audit_run[1],
      global_audit_gate_pass=global_audit_summary$global_audit_gate_pass[1],
      global_audit_interpretation=global_audit_summary$global_audit_interpretation[1],
      global_audit_rel_improvement_q95=global_audit_summary$global_audit_rel_improvement_q95[1],
      global_audit_rel_improvement_mean=global_audit_summary$global_audit_rel_improvement_mean[1])
    scen_sp <- eval_sp$candidates[[ga_k]]$scenario_table |> dplyr::mutate(specialist_regime_id=reg_id, regime_key=reg_key,
      gate_pass=gate_sp$gate_pass, final_selected_type=gate_sp$final_selected_type, final_selected_estimator=gate_sp$final_selected_estimator,
      best_benchmark_q95_estimator=gate_sp$best_benchmark_q95_estimator, best_benchmark_mean_estimator=gate_sp$best_benchmark_mean_estimator,
      ga_rel_improvement_q95=gate_sp$ga_rel_improvement_q95, ga_rel_improvement_mean=gate_sp$ga_rel_improvement_mean,
      neighborhood_used=isTRUE(should_use_neighborhood), n_exact_scenarios=n_exact_scenarios,
      n_neighbor_scenarios=n_neighbor_scenarios, n_neighbor_regimes=n_neighbor_regimes)
    scen_sp <- add_audit_columns(scen_sp, reg)
    scen_sp <- add_weight_columns(scen_sp, winner_w)
    if (isTRUE(global_audit$ok) && is.data.frame(global_audit$scenarios) && nrow(global_audit$scenarios)) {
      safe_write_csv(global_audit$scenarios, file.path(run_dir, sprintf("REGIME_PROFILE_MATCHED_AUDIT_SCENARIOS__seed%d__%s.csv", base_seed, reg_id)))
    }
    safe_write_csv(global_audit_summary, file.path(run_dir, sprintf("REGIME_PROFILE_MATCHED_AUDIT_SUMMARY__seed%d__%s.csv", base_seed, reg_id)))
    safe_write_csv(rank_sp, file.path(run_dir, sprintf("REGIME_FINALISTS_RANK__seed%d__%s.csv", base_seed, reg_id)))
    safe_write_csv(scen_sp, file.path(run_dir, sprintf("REGIME_WINNER_SCENARIOS__seed%d__%s.csv", base_seed, reg_id)))
    summary_rows[[length(summary_rows)+1L]] <- summary_row(reg, distribution=dist_name, specialist_regime_id=reg_id, regime_key=reg_key,
      status="evaluated", skip_reason=NA_character_, n_scenarios=nrow(sc_reg), n_exact_scenarios=n_exact_scenarios,
      n_neighbor_scenarios=n_neighbor_scenarios, n_neighbor_regimes=n_neighbor_regimes, neighborhood_used=isTRUE(should_use_neighborhood),
      neighborhood_trigger_scenarios=regime_neighborhood_trigger_scenarios, n_train_scenarios=nrow(train_subset),
      n_heldout_scenarios=nrow(holdout_subset), specialist_seed=base_seed_rr+seed_offset+rr, ga_candidate_k=ga_k,
      ga_candidate_source=if ("candidate_source" %in% names(rank_sp)) as.character(rank_sp$candidate_source[1]) else NA_character_,
      ga_candidate_protected_elite=if ("protected_elite" %in% names(rank_sp)) isTRUE(rank_sp$protected_elite[1]) else FALSE,
      protected_elite_origin=if ("protected_elite_origin" %in% names(rank_sp)) as.character(rank_sp$protected_elite_origin[1]) else NA_character_,
      protected_elite_one_stage_only=if ("protected_elite_one_stage_only" %in% names(rank_sp)) isTRUE(rank_sp$protected_elite_one_stage_only[1]) else FALSE,
      n_protected_elites_competing=if ("protected_elite" %in% names(rank_sp)) sum(rank_sp$protected_elite %in% TRUE, na.rm=TRUE) else 0L,
      ga_robust_q95_mse=as.numeric(rank_sp$robust_q95_mse[1]), ga_robust_mean_mse=as.numeric(rank_sp$robust_mean_mse[1]),
      gate_pass=gate_sp$gate_pass, final_selected_type=gate_sp$final_selected_type, final_selected_estimator=gate_sp$final_selected_estimator,
      final_selected_q95_mse=if (isTRUE(gate_sp$gate_pass)) gate_sp$ga_q95_mse else gate_sp$best_benchmark_q95_mse,
      final_selected_mean_mse=if (isTRUE(gate_sp$gate_pass)) gate_sp$ga_mean_mse else gate_sp$best_benchmark_mean_mse,
      best_benchmark_q95_estimator=gate_sp$best_benchmark_q95_estimator, best_benchmark_mean_estimator=gate_sp$best_benchmark_mean_estimator,
      ga_rel_improvement_q95=gate_sp$ga_rel_improvement_q95, ga_rel_improvement_mean=gate_sp$ga_rel_improvement_mean,
      profile_audit_run=audit_col("profile_audit_run", FALSE),
      profile_audit_scope=audit_col("profile_audit_scope", NA_character_),
      profile_match_level=audit_col("profile_match_level", NA_character_),
      profile_audit_n_available=audit_col("profile_audit_n_available", NA_integer_),
      profile_audit_n_scenarios=audit_col("profile_audit_n_scenarios", NA_integer_),
      profile_audit_excluded_source_scenarios=audit_col("profile_audit_excluded_source_scenarios", NA_integer_),
      profile_audit_num_samples=audit_col("profile_audit_num_samples", NA_integer_),
      profile_audit_bootstrap_B=audit_col("profile_audit_bootstrap_B", NA_integer_),
      profile_audit_seed=audit_col("profile_audit_seed", NA_integer_),
      profile_audit_ga_q95_mse=audit_col("profile_audit_ga_q95_mse", NA_real_),
      profile_audit_ga_mean_mse=audit_col("profile_audit_ga_mean_mse", NA_real_),
      profile_to_heldout_q95_ratio=audit_col("profile_to_heldout_q95_ratio", NA_real_),
      profile_to_heldout_mean_ratio=audit_col("profile_to_heldout_mean_ratio", NA_real_),
      profile_audit_gate_pass=audit_col("profile_audit_gate_pass", FALSE),
      profile_audit_final_selected_type=audit_col("profile_audit_final_selected_type", NA_character_),
      profile_audit_final_selected_estimator=audit_col("profile_audit_final_selected_estimator", NA_character_),
      profile_audit_best_benchmark_q95_estimator=audit_col("profile_audit_best_benchmark_q95_estimator", NA_character_),
      profile_audit_best_benchmark_mean_estimator=audit_col("profile_audit_best_benchmark_mean_estimator", NA_character_),
      profile_audit_best_benchmark_q95_mse=audit_col("profile_audit_best_benchmark_q95_mse", NA_real_),
      profile_audit_best_benchmark_mean_mse=audit_col("profile_audit_best_benchmark_mean_mse", NA_real_),
      profile_audit_rel_improvement_q95=audit_col("profile_audit_rel_improvement_q95", NA_real_),
      profile_audit_rel_improvement_mean=audit_col("profile_audit_rel_improvement_mean", NA_real_),
      profile_audit_interpretation=audit_col("profile_audit_interpretation", NA_character_),
      global_audit_run=global_audit_summary$global_audit_run[1],
      global_audit_scenario_frac=global_audit_summary$global_audit_scenario_frac[1],
      global_audit_min_scenarios=global_audit_summary$global_audit_min_scenarios[1],
      global_audit_n_scenarios=global_audit_summary$global_audit_n_scenarios[1],
      global_audit_num_samples=global_audit_summary$global_audit_num_samples[1],
      global_audit_bootstrap_B=global_audit_summary$global_audit_bootstrap_B[1],
      global_audit_seed=global_audit_summary$global_audit_seed[1],
      global_audit_ga_q95_mse=global_audit_summary$global_audit_ga_q95_mse[1],
      global_audit_ga_mean_mse=global_audit_summary$global_audit_ga_mean_mse[1],
      heldout_ga_q95_mse=global_audit_summary$heldout_ga_q95_mse[1],
      heldout_ga_mean_mse=global_audit_summary$heldout_ga_mean_mse[1],
      global_to_heldout_q95_ratio=global_audit_summary$global_to_heldout_q95_ratio[1],
      global_to_heldout_mean_ratio=global_audit_summary$global_to_heldout_mean_ratio[1],
      global_audit_gate_pass=global_audit_summary$global_audit_gate_pass[1],
      global_audit_final_selected_type=global_audit_summary$global_audit_final_selected_type[1],
      global_audit_final_selected_estimator=global_audit_summary$global_audit_final_selected_estimator[1],
      global_audit_best_benchmark_q95_estimator=global_audit_summary$global_audit_best_benchmark_q95_estimator[1],
      global_audit_best_benchmark_mean_estimator=global_audit_summary$global_audit_best_benchmark_mean_estimator[1],
      global_audit_best_benchmark_q95_mse=global_audit_summary$global_audit_best_benchmark_q95_mse[1],
      global_audit_best_benchmark_mean_mse=global_audit_summary$global_audit_best_benchmark_mean_mse[1],
      global_audit_rel_improvement_q95=global_audit_summary$global_audit_rel_improvement_q95[1],
      global_audit_rel_improvement_mean=global_audit_summary$global_audit_rel_improvement_mean[1],
      global_audit_interpretation=global_audit_summary$global_audit_interpretation[1]) |> add_weight_columns(winner_w)
  }
  dplyr::bind_rows(summary_rows)
}
# =============================================================================


# =============================================================================
# INTER-FAMILY PROFILE-MATCHED GENERALIZATION AUDIT
# =============================================================================
# This is a lightweight, no-retraining audit that is run only after specialist
# winners are selected. It is not the old global family transfer matrix. Instead,
# it takes GA-winning specialist weight vectors and evaluates them on structurally
# similar profile-matched scenarios in other compatible distribution families.
# This tests whether a regime-specialist estimator transfers across families with
# comparable contamination profiles, while preserving the No-Free-Lunch claim.
# =============================================================================
run_interfamily_profile_matched_audit <- function(specialist_summary,
                                                  target_families,
                                                  param_grids,
                                                  sample_sizes,
                                                  crn_env,
                                                  out_dir,
                                                  source_families = NULL,
                                                  num_samples = 20L,
                                                  bootstrap_B = 40L,
                                                  scenario_frac = 0.10,
                                                  min_scenarios = 24L,
                                                  max_targets_per_winner = 3L,
                                                  only_ga_winners = TRUE,
                                                  max_rate_distance = 1L,
                                                  max_scale_distance = 1L,
                                                  seed = 101L) {
  empty <- list(summary = data.frame(), scenarios = data.frame())
  if (is.null(specialist_summary) || !is.data.frame(specialist_summary) || !nrow(specialist_summary)) return(empty)
  if (is.null(param_grids) || !length(param_grids)) return(empty)
  target_families <- unique(as.character(target_families))
  target_families <- target_families[nzchar(target_families) & target_families %in% names(param_grids)]
  if (!length(target_families)) return(empty)
  if (is.null(source_families)) source_families <- unique(as.character(specialist_summary$distribution))
  source_families <- unique(as.character(source_families))

  num_samples <- max(5L, suppressWarnings(as.integer(num_samples)[1]))
  bootstrap_B <- max(20L, suppressWarnings(as.integer(bootstrap_B)[1]))
  scenario_frac <- suppressWarnings(as.numeric(scenario_frac)[1])
  if (!is.finite(scenario_frac) || scenario_frac <= 0 || scenario_frac > 1) scenario_frac <- 0.10
  min_scenarios <- max(1L, suppressWarnings(as.integer(min_scenarios)[1]))
  max_targets_per_winner <- max(1L, suppressWarnings(as.integer(max_targets_per_winner)[1]))
  max_rate_distance <- max(0L, suppressWarnings(as.integer(max_rate_distance)[1]))
  max_scale_distance <- max(0L, suppressWarnings(as.integer(max_scale_distance)[1]))
  seed <- .ensure_seed(seed, fallback = 101L)
  mkdirp(out_dir)

  w_names <- if (exists("ESTIMATOR_NAMES", inherits = TRUE)) paste0("w_", make.names(ESTIMATOR_NAMES, unique = TRUE)) else character(0)
  if (!length(w_names) || !all(w_names %in% names(specialist_summary))) return(empty)

  rows <- specialist_summary[specialist_summary$status == "evaluated", , drop = FALSE]
  if (isTRUE(only_ga_winners)) rows <- rows[rows$gate_pass %in% TRUE, , drop = FALSE]
  if (!nrow(rows)) return(empty)

  compatible_targets <- function(source_family, target_families) {
    source_family <- as.character(source_family)[1]
    targets <- setdiff(target_families, source_family)
    positive_skew_family <- c("lognormal", "weibull", "invgauss", "exgaussian", "exwald")
    if (source_family %in% positive_skew_family) {
      targets <- intersect(targets, positive_skew_family)
    } else if (identical(source_family, "normal")) {
      # Normal is retained as a control family; cross-family transfer from normal
      # is not forced because its regime logic is not the core specialist claim.
      targets <- intersect(targets, setdiff(target_families, source_family))
    }
    unique(targets)
  }
  all_summary <- list(); all_scenarios <- list()
  for (ii in seq_len(nrow(rows))) {
    src <- rows[ii, , drop = FALSE]
    src_family <- .first_chr(src, "distribution")
    if (!nzchar(src_family) || is.na(src_family) || !(src_family %in% source_families)) next
    w <- suppressWarnings(as.numeric(src[1, w_names, drop = TRUE]))
    if (!length(w) || any(!is.finite(w))) next

    candidate_targets <- compatible_targets(src_family, target_families)
    if (!length(candidate_targets)) next
    candidate_targets <- candidate_targets[seq_len(min(max_targets_per_winner, length(candidate_targets)))]

    target_direction <- .first_chr(src, "contamination_direction")
    target_structure <- .first_chr(src, "contamination_structure")
    target_rate_order <- .first_int(src, "rate_regime_order")
    target_scale_order <- .first_int(src, "scale_group_regime_order")
    src_regime_id <- .first_chr(src, "specialist_regime_id")
    src_regime_key <- .first_chr(src, "regime_key")

    for (target_family in candidate_targets) {
      pool <- add_scenario_ids(build_scenarios("full"))
      pool <- .add_regime_columns(pool, distribution = target_family, include_sample_size_bin = FALSE)
      scale_col <- "scale_group_regime_order"
      if (!is.finite(target_scale_order) || !scale_col %in% names(pool)) {
        target_scale_order <- .first_int(src, "scale_regime_order")
        scale_col <- "scale_regime_order"
      }
      rate_close <- if ("rate_regime_order" %in% names(pool) && is.finite(target_rate_order)) {
        abs(suppressWarnings(as.integer(pool$rate_regime_order)) - target_rate_order) <= max_rate_distance
      } else rep(TRUE, nrow(pool))
      scale_close <- if (scale_col %in% names(pool) && is.finite(target_scale_order)) {
        abs(suppressWarnings(as.integer(pool[[scale_col]])) - target_scale_order) <= max_scale_distance
      } else rep(TRUE, nrow(pool))
      same_direction <- if ("contamination_direction" %in% names(pool) && nzchar(target_direction) && !is.na(target_direction)) {
        as.character(pool$contamination_direction) == target_direction
      } else rep(TRUE, nrow(pool))
      same_structure <- if ("contamination_structure" %in% names(pool) && nzchar(target_structure) && !is.na(target_structure)) {
        as.character(pool$contamination_structure) == target_structure
      } else rep(TRUE, nrow(pool))

      candidates <- pool[same_direction & same_structure & rate_close & scale_close, , drop = FALSE]
      profile_match_level <- "interfamily_direction_structure_adjacent_rate_scale"
      if (nrow(candidates) < min_scenarios) {
        candidates <- pool[same_direction & rate_close & scale_close, , drop = FALSE]
        profile_match_level <- "interfamily_direction_adjacent_rate_scale"
      }
      if (nrow(candidates) < min_scenarios) {
        candidates <- pool[rate_close & scale_close, , drop = FALSE]
        profile_match_level <- "interfamily_adjacent_rate_scale"
      }
      if (nrow(candidates) < 3L) {
        candidates <- pool
        profile_match_level <- "interfamily_family_only_fallback"
      }
      if (!nrow(candidates)) next

      available <- nrow(candidates)
      target_n <- min(available, max(1L, min_scenarios, as.integer(ceiling(available * scenario_frac))))
      audit_seed <- .stable_int_seed(paste0(src_family, "::", src_regime_id, "::", target_family, "::interfamily_profile"),
                                     base_seed = seed + 91000L)
      set.seed(audit_seed)
      keep_ix <- if (available > target_n) sample(seq_len(available), target_n) else seq_len(available)
      audit_subset <- candidates[keep_ix, , drop = FALSE]
      audit_subset$interfamily_profile_match_level <- profile_match_level
      audit_subset$source_family <- src_family
      audit_subset$source_specialist_regime_id <- src_regime_id
      audit_subset$source_regime_key <- src_regime_key
      audit_subset$target_family <- target_family

      audit_eval <- tryCatch(
        .evaluate_topk_fixed_weights_subset(target_family, param_grids[[target_family]], matrix(w, nrow = 1),
          sample_sizes = sample_sizes, num_samples = num_samples,
          scenario_subset = audit_subset, scenario_mode = "full", seed = audit_seed,
          q95_B = bootstrap_B, crn_env = crn_env, fam_key = target_family,
          rank_metric = "robust_q95_mse",
          subset_tag = paste0("REGIME_INTERFAMILY_PROFILE_MATCHED_AUDIT__", src_family, "__", src_regime_id, "__TO__", target_family),
          do_perturb = FALSE),
        error = function(e) NULL
      )
      if (is.null(audit_eval) || is.null(audit_eval$rank) || !nrow(audit_eval$rank)) next
      gate_t <- tryCatch(
        benchmark_gate_from_scenario_table(audit_eval$candidates[[1]]$scenario_table,
                                           distribution = target_family,
                                           min_rel_improvement = 0.00,
                                           ga_estimator_name = "robust"),
        error = function(e) NULL
      )
      if (is.null(gate_t)) next

      src_pass <- isTRUE(src$gate_pass[1])
      tgt_pass <- isTRUE(gate_t$gate_pass)
      interp <- if (src_pass && tgt_pass) {
        "interfamily_profile_generalization_bonus"
      } else if (src_pass && !tgt_pass) {
        "source_regime_specific_cross_family_no_free_lunch"
      } else if (!src_pass && tgt_pass) {
        "interfamily_profile_only_unexpected"
      } else {
        "benchmark_dominant_source_and_target"
      }

      target_q95 <- suppressWarnings(as.numeric(audit_eval$rank$robust_q95_mse[1]))
      target_mean <- suppressWarnings(as.numeric(audit_eval$rank$robust_mean_mse[1]))
      source_q95 <- suppressWarnings(as.numeric(src$ga_robust_q95_mse[1]))
      source_mean <- suppressWarnings(as.numeric(src$ga_robust_mean_mse[1]))
      summ <- data.frame(
        interfamily_profile_audit_run = TRUE,
        interfamily_profile_audit_scope = "cross_family_profile_matched_fixed_weight_no_retraining",
        source_family = src_family,
        source_specialist_regime_id = src_regime_id,
        source_regime_key = src_regime_key,
        target_family = target_family,
        interfamily_profile_match_level = profile_match_level,
        interfamily_profile_n_available = available,
        interfamily_profile_n_scenarios = nrow(audit_subset),
        interfamily_profile_scenario_frac = scenario_frac,
        interfamily_profile_min_scenarios = min_scenarios,
        interfamily_profile_num_samples = num_samples,
        interfamily_profile_bootstrap_B = bootstrap_B,
        interfamily_profile_seed = audit_seed,
        source_heldout_gate_pass = src_pass,
        source_heldout_ga_q95_mse = source_q95,
        source_heldout_ga_mean_mse = source_mean,
        target_profile_gate_pass = tgt_pass,
        target_profile_ga_q95_mse = target_q95,
        target_profile_ga_mean_mse = target_mean,
        target_profile_best_benchmark_q95_estimator = gate_t$best_benchmark_q95_estimator,
        target_profile_best_benchmark_mean_estimator = gate_t$best_benchmark_mean_estimator,
        target_profile_best_benchmark_q95_mse = gate_t$best_benchmark_q95_mse,
        target_profile_best_benchmark_mean_mse = gate_t$best_benchmark_mean_mse,
        target_profile_rel_improvement_q95 = gate_t$ga_rel_improvement_q95,
        target_profile_rel_improvement_mean = gate_t$ga_rel_improvement_mean,
        cross_family_to_source_q95_ratio = target_q95 / (abs(source_q95) + 1e-12),
        cross_family_to_source_mean_ratio = target_mean / (abs(source_mean) + 1e-12),
        interfamily_profile_interpretation = interp,
        stringsAsFactors = FALSE
      )
      # Keep compact regime/profile descriptors from the source row for auditability.
      for (cc in c("shape_regime", "tail_regime", "contamination_direction", "contamination_structure",
                   "rate_regime", "scale_regime", "scale_group_regime", "severity_regime")) {
        if (cc %in% names(src)) summ[[paste0("source_", cc)]] <- as.character(src[[cc]][1])
      }
      for (ww in seq_along(w_names)) summ[[w_names[ww]]] <- w[[ww]]
      summ$winner_weight_vector <- paste(round(w, 8), collapse = ";")

      scen <- audit_eval$candidates[[1]]$scenario_table
      scen$source_family <- src_family
      scen$source_specialist_regime_id <- src_regime_id
      scen$source_regime_key <- src_regime_key
      scen$target_family <- target_family
      scen$interfamily_profile_match_level <- profile_match_level
      scen$interfamily_profile_gate_pass <- tgt_pass
      scen$interfamily_profile_interpretation <- interp
      for (ww in seq_along(w_names)) scen[[w_names[ww]]] <- w[[ww]]
      scen$winner_weight_vector <- paste(round(w, 8), collapse = ";")

      all_summary[[length(all_summary) + 1L]] <- summ
      all_scenarios[[length(all_scenarios) + 1L]] <- scen
    }
  }

  summary_df <- if (length(all_summary)) dplyr::bind_rows(all_summary) else data.frame()
  scenarios_df <- if (length(all_scenarios)) dplyr::bind_rows(all_scenarios) else data.frame()
  if (nrow(summary_df)) safe_write_csv(summary_df, file.path(out_dir, "REGIME_INTERFAMILY_PROFILE_MATCHED_AUDIT__ALL_FAMILIES.csv"))
  if (nrow(scenarios_df)) safe_write_csv(scenarios_df, file.path(out_dir, "REGIME_INTERFAMILY_PROFILE_MATCHED_AUDIT_SCENARIOS__ALL_FAMILIES.csv"))
  list(summary = summary_df, scenarios = scenarios_df)
}



# =============================================================================
# THESIS COMPACT RESULTS SUMMARY
# =============================================================================
# One-row-per-family/regime table for thesis writing and audit summary.
# It does not alter the experiment. It only consolidates heldout, benchmark-gate,
# profile-matched audit, inter-family profile audit, neighborhood, and weights.
# =============================================================================
.build_regime_first_thesis_results_compact <- function(local_summary, interfamily_summary = NULL) {
  if (is.null(local_summary) || !is.data.frame(local_summary) || !nrow(local_summary)) return(data.frame())
  x <- as.data.frame(local_summary)
  n <- nrow(x)
  col <- function(nm, default = NA) {
    if (nm %in% names(x)) x[[nm]] else rep(default, n)
  }
  chr <- function(nm, default = NA_character_) as.character(col(nm, default))
  num <- function(nm, default = NA_real_) suppressWarnings(as.numeric(col(nm, default)))
  int <- function(nm, default = NA_integer_) suppressWarnings(as.integer(col(nm, default)))
  logi <- function(nm, default = FALSE) {
    z <- col(nm, default)
    if (is.logical(z)) z else as.character(z) %in% c("TRUE", "true", "1", "yes")
  }
  first_nonmissing_chr <- function(...) {
    vals <- list(...)
    out <- rep(NA_character_, n)
    for (v in vals) {
      v <- as.character(v)
      take <- is.na(out) | !nzchar(out)
      out[take] <- v[take]
    }
    out
  }

  gate <- logi("gate_pass", FALSE)
  final_type <- chr("final_selected_type")
  winner_type <- ifelse(gate, "ga_composite", ifelse(nzchar(final_type) & !is.na(final_type), final_type, "benchmark"))

  out <- data.frame(
    family = chr("distribution"),
    specialist_regime_id = chr("specialist_regime_id"),
    regime_key = chr("regime_key"),
    experimental_condition_key = chr("experimental_condition_key"),
    condition_summary = chr("condition_summary"),
    shape_regime = chr("shape_regime"),
    tail_regime = chr("tail_regime"),
    contamination_direction = chr("contamination_direction"),
    contamination_structure = chr("contamination_structure"),
    rate_regime = chr("rate_regime"),
    scale_regime = chr("scale_regime"),
    scale_group_regime = chr("scale_group_regime"),
    severity_regime = chr("severity_regime"),
    status = chr("status"),
    skip_reason = chr("skip_reason"),
    n_scenarios = int("n_scenarios"),
    n_exact_scenarios = int("n_exact_scenarios"),
    n_neighbor_scenarios = int("n_neighbor_scenarios"),
    n_neighbor_regimes = int("n_neighbor_regimes"),
    neighborhood_used = logi("neighborhood_used", FALSE),
    n_train_scenarios = int("n_train_scenarios"),
    n_heldout_scenarios = int("n_heldout_scenarios"),
    winner_type = winner_type,
    final_selected_type = final_type,
    final_selected_estimator = chr("final_selected_estimator"),
    gate_pass = gate,
    ga_candidate_k = int("ga_candidate_k"),
    ga_candidate_source = chr("ga_candidate_source"),
    ga_candidate_protected_elite = logi("ga_candidate_protected_elite", FALSE),
    protected_elite_origin = chr("protected_elite_origin"),
    protected_elite_one_stage_only = logi("protected_elite_one_stage_only", FALSE),
    n_protected_elites_competing = int("n_protected_elites_competing"),
    ga_heldout_q95_mse = num("ga_robust_q95_mse"),
    ga_heldout_mean_mse = num("ga_robust_mean_mse"),
    final_selected_q95_mse = num("final_selected_q95_mse"),
    final_selected_mean_mse = num("final_selected_mean_mse"),
    best_benchmark_q95_estimator = chr("best_benchmark_q95_estimator"),
    best_benchmark_mean_estimator = chr("best_benchmark_mean_estimator"),
    ga_rel_improvement_q95 = num("ga_rel_improvement_q95"),
    ga_rel_improvement_mean = num("ga_rel_improvement_mean"),
    profile_audit_run = logi("profile_audit_run", FALSE),
    profile_match_level = chr("profile_match_level"),
    profile_audit_n_available = int("profile_audit_n_available"),
    profile_audit_n_scenarios = int("profile_audit_n_scenarios"),
    profile_audit_excluded_source_scenarios = int("profile_audit_excluded_source_scenarios"),
    profile_audit_gate_pass = logi("profile_audit_gate_pass", FALSE),
    profile_audit_final_selected_type = chr("profile_audit_final_selected_type"),
    profile_audit_final_selected_estimator = chr("profile_audit_final_selected_estimator"),
    profile_audit_rel_improvement_q95 = num("profile_audit_rel_improvement_q95"),
    profile_audit_rel_improvement_mean = num("profile_audit_rel_improvement_mean"),
    profile_audit_interpretation = chr("profile_audit_interpretation"),
    profile_to_heldout_q95_ratio = num("profile_to_heldout_q95_ratio"),
    profile_to_heldout_mean_ratio = num("profile_to_heldout_mean_ratio"),
    winner_weight_vector = chr("winner_weight_vector"),
    stringsAsFactors = FALSE
  )

  # Attach component weights when present.
  w_cols <- grep("^w_", names(x), value = TRUE)
  if (length(w_cols)) {
    for (cc in w_cols) out[[cc]] <- suppressWarnings(as.numeric(x[[cc]]))
  }

  # Summarise cross-family profile-matched audit by source family/regime.
  out$interfamily_profile_audit_run <- FALSE
  out$interfamily_profile_n_targets <- 0L
  out$interfamily_profile_n_passes <- 0L
  out$interfamily_profile_best_rel_improvement_q95 <- NA_real_
  out$interfamily_profile_best_rel_improvement_mean <- NA_real_
  out$interfamily_profile_interpretations <- NA_character_
  out$interfamily_profile_target_families <- NA_character_

  if (!is.null(interfamily_summary) && is.data.frame(interfamily_summary) && nrow(interfamily_summary)) {
    y <- as.data.frame(interfamily_summary)
    if (all(c("source_family", "source_specialist_regime_id") %in% names(y))) {
      y$key <- paste(as.character(y$source_family), as.character(y$source_specialist_regime_id), sep = "::")
      out$key <- paste(out$family, out$specialist_regime_id, sep = "::")
      for (kk in unique(out$key)) {
        yy <- y[y$key == kk, , drop = FALSE]
        if (!nrow(yy)) next
        ix <- which(out$key == kk)
        out$interfamily_profile_audit_run[ix] <- TRUE
        out$interfamily_profile_n_targets[ix] <- nrow(yy)
        if ("target_profile_gate_pass" %in% names(yy)) {
          out$interfamily_profile_n_passes[ix] <- sum(yy$target_profile_gate_pass %in% TRUE, na.rm = TRUE)
        }
        if ("target_profile_rel_improvement_q95" %in% names(yy)) {
          v <- suppressWarnings(as.numeric(yy$target_profile_rel_improvement_q95))
          out$interfamily_profile_best_rel_improvement_q95[ix] <- if (any(is.finite(v))) max(v, na.rm = TRUE) else NA_real_
        }
        if ("target_profile_rel_improvement_mean" %in% names(yy)) {
          v <- suppressWarnings(as.numeric(yy$target_profile_rel_improvement_mean))
          out$interfamily_profile_best_rel_improvement_mean[ix] <- if (any(is.finite(v))) max(v, na.rm = TRUE) else NA_real_
        }
        if ("interfamily_profile_interpretation" %in% names(yy)) {
          out$interfamily_profile_interpretations[ix] <- paste(unique(as.character(yy$interfamily_profile_interpretation)), collapse = ";")
        }
        if ("target_family" %in% names(yy)) {
          out$interfamily_profile_target_families[ix] <- paste(unique(as.character(yy$target_family)), collapse = ";")
        }
      }
      out$key <- NULL
    }
  }

  # Thesis-friendly high-level label.
  out$thesis_result_label <- ifelse(
    out$gate_pass & out$profile_audit_gate_pass & out$interfamily_profile_n_passes > 0,
    "ga_wins_local_profile_and_cross_family",
    ifelse(out$gate_pass & out$profile_audit_gate_pass,
           "ga_wins_local_and_profile",
           ifelse(out$gate_pass & !out$profile_audit_gate_pass,
                  "ga_wins_local_only_no_free_lunch",
                  "benchmark_dominant_local"))
  )
  out
}

# ===== Main launcher function =====

run_all_one_shot <- function(..., regime_first_mode = TRUE) {
  # Final architecture: regime-first only.
  if (!isTRUE(regime_first_mode)) {
    stop("Only the regime-first pipeline is supported in this workflow.", call. = FALSE)
  }
  args <- list(...)
  seeds_arg <- args$seeds
  if (is.null(seeds_arg)) seeds_arg <- c(101L)
  seeds_arg <- unique(as.integer(seeds_arg))
  seeds_arg <- seeds_arg[is.finite(seeds_arg)]
  if (!length(seeds_arg)) seeds_arg <- c(101L)

  # Multi-seed orchestration: each seed gets a complete, independent run folder.
  # A master folder then binds the key outputs and writes stability summaries.
  if (length(seeds_arg) > 1L) {
    out_root <- args$out_root %||% OUT_ROOT
    master_id <- paste0("GA_REGIME_FIRST_MULTI_SEED_", RUN_TS())
    master_dir <- file.path(out_root, master_id)
    mkdirp(master_dir)

    read_csv_safe <- function(path) {
      if (!file.exists(path)) return(data.frame())
      tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) data.frame())
    }
    bind_seed_file <- function(seed_runs, filename) {
      rows <- list()
      for (rr in seed_runs) {
        fp <- file.path(rr$out_dir, filename)
        df <- read_csv_safe(fp)
        if (nrow(df)) {
          df$run_seed <- rr$seed
          df$seed_run_id <- basename(rr$out_dir)
          rows[[length(rows) + 1L]] <- df
        }
      }
      if (length(rows)) dplyr::bind_rows(rows) else data.frame()
    }

    seed_runs <- list()
    for (ss in seeds_arg) {
      catf("[MULTI-SEED] Starting independent seed run: %d", ss)
      args_i <- args
      args_i$seeds <- c(ss)
      args_i$out_root <- master_dir
      res_i <- do.call(run_regime_first_one_shot, args_i)
      seed_runs[[length(seed_runs) + 1L]] <- data.frame(
        run_seed = ss,
        seed = ss,
        out_dir = as.character(res_i$out_dir),
        architecture = "regime_first_multiseed_child",
        stringsAsFactors = FALSE
      )
    }
    seed_index <- dplyr::bind_rows(seed_runs)
    safe_write_csv(seed_index, file.path(master_dir, "MULTISEED_RUN_INDEX.csv"))

    key_files <- c(
      "REGIME_FIRST_THESIS_RESULTS_COMPACT.csv",
      "REGIME_FIRST__ALL_FAMILIES_SUMMARY.csv",
      "REGIME_FIRST_PROFILE_MATCHED_AUDIT__ALL_FAMILIES.csv",
      "REGIME_INTERFAMILY_PROFILE_MATCHED_AUDIT__ALL_FAMILIES.csv",
      "REGIME_FIRST_METHOD_AUDIT_SUMMARY.csv",
      "runtime_log.csv"
    )
    bound_outputs <- list()
    for (ff in key_files) {
      df <- bind_seed_file(seed_runs, ff)
      if (nrow(df)) {
        out_name <- sub("\\.csv$", "__ALL_SEEDS.csv", ff)
        safe_write_csv(df, file.path(master_dir, out_name))
        bound_outputs[[ff]] <- df
      }
    }

    compact_all <- bound_outputs[["REGIME_FIRST_THESIS_RESULTS_COMPACT.csv"]]
    if (!is.null(compact_all) && is.data.frame(compact_all) && nrow(compact_all)) {
      key_cols <- c("family", "regime_id", "regime_key")
      key_cols <- key_cols[key_cols %in% names(compact_all)]
      if (length(key_cols) >= 2L) {
        compact_all$gate_pass_num <- as.integer(compact_all$gate_pass %in% TRUE | tolower(as.character(compact_all$winner_type)) == "ga")
        for (nm in c("ga_rel_improvement_q95", "ga_rel_improvement_mean", "q95_improvement", "mean_improvement")) {
          if (nm %in% names(compact_all)) compact_all[[nm]] <- suppressWarnings(as.numeric(compact_all[[nm]]))
        }
        qcol <- if ("ga_rel_improvement_q95" %in% names(compact_all)) "ga_rel_improvement_q95" else if ("q95_improvement" %in% names(compact_all)) "q95_improvement" else NA_character_
        mcol <- if ("ga_rel_improvement_mean" %in% names(compact_all)) "ga_rel_improvement_mean" else if ("mean_improvement" %in% names(compact_all)) "mean_improvement" else NA_character_
        stability <- compact_all |>
          dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
          dplyr::summarise(
            n_seeds_observed = dplyr::n_distinct(run_seed),
            ga_win_count = sum(gate_pass_num, na.rm = TRUE),
            ga_win_rate_across_seeds = mean(gate_pass_num, na.rm = TRUE),
            mean_q95_improvement = if (!is.na(qcol)) mean(.data[[qcol]], na.rm = TRUE) else NA_real_,
            mean_mse_improvement = if (!is.na(mcol)) mean(.data[[mcol]], na.rm = TRUE) else NA_real_,
            seed_stability_label = dplyr::case_when(
              ga_win_count == n_seeds_observed ~ "stable_ga_win",
              ga_win_count == 0L ~ "stable_benchmark_dominance",
              TRUE ~ "seed_sensitive"
            ),
            .groups = "drop"
          )
        safe_write_csv(stability, file.path(master_dir, "REGIME_FIRST_SEED_STABILITY_SUMMARY.csv"))
      }
    }

    meta <- data.frame(
      architecture = "regime_first_multiseed_master",
      seeds = paste(seeds_arg, collapse = ","),
      n_seed_runs = length(seeds_arg),
      hpf1_scenario_frac = args$hp1_scenario_frac %||% NA,
      hpf2_scenario_frac = args$hp2_scenario_frac %||% NA,
      specialist_final_pop_size = args$regime_specialist_final_pop_size %||% NA,
      specialist_final_gens_frac = args$regime_specialist_final_gens_frac %||% NA,
      generations_per_fold = args$generations_per_fold %||% NA,
      topk_heldout_finalists = args$regime_specialist_finalists_per_regime %||% NA,
      note = "Each seed is a complete independent run. Master CSVs bind child outputs and add seed stability summaries.",
      stringsAsFactors = FALSE
    )
    safe_write_csv(meta, file.path(master_dir, "MULTISEED_METHOD_AUDIT_SUMMARY.csv"))
    return(invisible(list(out_dir = master_dir, seed_runs = seed_index, architecture = "regime_first_multiseed")))
  }

  run_regime_first_one_shot(...)
}



# =============================================================================
# SPECIALIST SINGLE-HALVING
# =============================================================================
# The regime-first architecture uses HPF1 and HPF2 as screening stages. After
# HPF2 selects candidate regime/configuration pairs, this helper performs one
# specialist halving stage:
#   rung 1: all selected regimes/candidates, reduced specialist budget;
#   rung 2: only the best regime/candidate survivors, full specialist budget;
#   final: heldout benchmark gate, no final retraining.
# This keeps the scientific objective focused on identifying where GA specialists
# beat benchmarks, without training a universal/global winner.
# =============================================================================
run_regime_specialist_single_halving <- function(selected_regimes, dist_name, dist_param_grid, cfg,
                                                scenario_universe_full, sample_sizes, num_samples,
                                                generations_per_fold, k_folds, objective, use_parallel,
                                                lambda_instab, bootstrap_B, t_size, elitism,
                                                immigrant_rate, mutation_rate_init, init_alpha,
                                                alpha_mut, check_every, patience, min_delta,
                                                mix_w_q95, mix_w_max, crn_env, fam_key, run_dir,
                                                finalists_per_regime = 3L, holdout_frac = 0.30,
                                                specialist_gens_frac = 0.50,
                                                specialist_num_samples_frac = 0.60,
                                                specialist_bootstrap_frac = 0.60,
                                                specialist_pop_size = NA_integer_,
                                                specialist_stage1_gens_frac = NA_real_,
                                                specialist_stage1_pop_size = NA_integer_,
                                                specialist_final_gens_frac = NA_real_,
                                                specialist_final_pop_size = NA_integer_,
                                                specialist_halving_stage1_budget_frac = 0.50,
                                                specialist_halving_keep_frac = 0.50,
                                                specialist_halving_min_keep = 1L,
                                                regime_neighborhood = TRUE,
                                                regime_neighborhood_trigger_scenarios = 10L,
                                                regime_neighborhood_max_neighbors = 3L,
                                                regime_neighborhood_max_scenarios = 24L,
                                                regime_neighborhood_max_rate_distance = 1L,
                                                regime_neighborhood_max_scale_distance = 1L,
                                                regime_neighborhood_min_promise_quantile = 0.40,
                                                regime_global_audit = TRUE,
                                                regime_global_audit_scenario_frac = 0.10,
                                                regime_global_audit_min_scenarios = 24L,
                                                regime_global_audit_num_samples = 20L,
                                                regime_global_audit_bootstrap_B = 40L,
                                                min_unique_scenarios = 6L,
                                                min_train_scenarios = 5L,
                                                min_heldout_scenarios = 1L,
                                                protected_elites = NULL,
                                                protected_elite_max_per_regime = 3L,
                                                seed_offset = 85000L) {
  if (is.null(selected_regimes) || !is.data.frame(selected_regimes) || !nrow(selected_regimes)) return(data.frame())
  selected <- selected_regimes[selected_regimes$selected_for_specialist_training %in% TRUE, , drop = FALSE]
  if (!nrow(selected)) return(data.frame())

  specialist_halving_stage1_budget_frac <- as.numeric(specialist_halving_stage1_budget_frac)[1]
  if (!is.finite(specialist_halving_stage1_budget_frac) || specialist_halving_stage1_budget_frac <= 0 || specialist_halving_stage1_budget_frac > 1) {
    specialist_halving_stage1_budget_frac <- 0.50
  }
  specialist_halving_keep_frac <- as.numeric(specialist_halving_keep_frac)[1]
  if (!is.finite(specialist_halving_keep_frac) || specialist_halving_keep_frac <= 0 || specialist_halving_keep_frac > 1) {
    specialist_halving_keep_frac <- 0.50
  }
  specialist_halving_min_keep <- max(1L, as.integer(specialist_halving_min_keep)[1])

  # Resolve specialist-halving budgets explicitly. Stage 1 stays light/moderate;
  # the final rung can receive full specialist resources without inflating HPF1/HPF2
  # or the stage-1 screening rung.
  specialist_stage1_gens_frac <- suppressWarnings(as.numeric(specialist_stage1_gens_frac)[1])
  if (!is.finite(specialist_stage1_gens_frac) || specialist_stage1_gens_frac <= 0) {
    specialist_stage1_gens_frac <- max(0.10, specialist_gens_frac * specialist_halving_stage1_budget_frac)
  }
  specialist_final_gens_frac <- suppressWarnings(as.numeric(specialist_final_gens_frac)[1])
  if (!is.finite(specialist_final_gens_frac) || specialist_final_gens_frac <= 0) {
    specialist_final_gens_frac <- specialist_gens_frac
  }
  specialist_stage1_pop_size <- suppressWarnings(as.integer(specialist_stage1_pop_size)[1])
  if (!is.finite(specialist_stage1_pop_size) || specialist_stage1_pop_size < 2L) {
    specialist_stage1_pop_size <- NA_integer_
  }
  specialist_final_pop_size <- suppressWarnings(as.integer(specialist_final_pop_size)[1])
  if (!is.finite(specialist_final_pop_size) || specialist_final_pop_size < 2L) {
    specialist_final_pop_size <- specialist_pop_size
  }

  mkdirp(run_dir)
  stage1_dir <- file.path(run_dir, "REGIME_FIRST_SPECIALIST_HALVING_STAGE1")
  final_dir  <- file.path(run_dir, "REGIME_FIRST_SPECIALIST_HALVING_FINAL")
  mkdirp(stage1_dir); mkdirp(final_dir)

  # Stage 1: all HPF2-selected regimes/candidates, smaller budget.
  stage1 <- tryCatch(run_regime_specialist_ga(
    selected_regimes = selected_regimes,
    dist_name = dist_name,
    dist_param_grid = dist_param_grid,
    cfg = cfg,
    scenario_universe_full = scenario_universe_full,
    sample_sizes = sample_sizes,
    num_samples = num_samples,
    generations_per_fold = generations_per_fold,
    k_folds = k_folds,
    objective = objective,
    use_parallel = use_parallel,
    lambda_instab = lambda_instab,
    bootstrap_B = bootstrap_B,
    t_size = t_size,
    elitism = elitism,
    immigrant_rate = immigrant_rate,
    mutation_rate_init = mutation_rate_init,
    init_alpha = init_alpha,
    alpha_mut = alpha_mut,
    check_every = check_every,
    patience = patience,
    min_delta = min_delta,
    mix_w_q95 = mix_w_q95,
    mix_w_max = mix_w_max,
    crn_env = crn_env,
    fam_key = fam_key,
    run_dir = stage1_dir,
    finalists_per_regime = max(1L, min(1L, as.integer(finalists_per_regime)[1])),
    holdout_frac = holdout_frac,
    specialist_gens_frac = specialist_stage1_gens_frac,
    specialist_num_samples_frac = max(0.10, specialist_num_samples_frac * specialist_halving_stage1_budget_frac),
    specialist_bootstrap_frac = max(0.10, specialist_bootstrap_frac * specialist_halving_stage1_budget_frac),
    specialist_pop_size = specialist_stage1_pop_size,
    regime_neighborhood = regime_neighborhood,
    regime_neighborhood_trigger_scenarios = regime_neighborhood_trigger_scenarios,
    regime_neighborhood_max_neighbors = regime_neighborhood_max_neighbors,
    regime_neighborhood_max_scenarios = max(min_unique_scenarios, as.integer(ceiling(regime_neighborhood_max_scenarios * specialist_halving_stage1_budget_frac))),
    regime_neighborhood_max_rate_distance = regime_neighborhood_max_rate_distance,
    regime_neighborhood_max_scale_distance = regime_neighborhood_max_scale_distance,
    regime_neighborhood_min_promise_quantile = regime_neighborhood_min_promise_quantile,
    regime_global_audit = FALSE,
    min_unique_scenarios = min_unique_scenarios,
    min_train_scenarios = min_train_scenarios,
    min_heldout_scenarios = min_heldout_scenarios,
    protected_elites = protected_elites,
    protected_elite_max_per_regime = protected_elite_max_per_regime,
    seed_offset = seed_offset
  ), error = function(e) {
    catf("[REGIME-FIRST] %s specialist halving stage 1 failed safely: %s", dist_name, conditionMessage(e))
    data.frame()
  })

  if (is.null(stage1) || !is.data.frame(stage1) || !nrow(stage1)) {
    catf("[REGIME-FIRST] %s specialist halving stage 1 produced no rows; final stage skipped.", dist_name)
    return(data.frame())
  }
  stage1$specialist_halving_rung <- "stage1_screen"
  safe_write_csv(stage1, file.path(run_dir, sprintf("REGIME_FIRST_SPECIALIST_HALVING_STAGE1_SUMMARY__seed%d.csv", as.integer(cfg$seed)[1])))

  # Rank by strict pass first, then a q95-focused benchmark-relative score.
  # q95 receives 75% of the survivor score because tail-risk improvement is the
  # core robustness objective; mean MSE remains as a secondary guard.
  rank_df <- stage1
  rank_df$.gate_rank <- ifelse(rank_df$gate_pass %in% TRUE, 1L, 0L)
  rank_df$.q95_rank <- suppressWarnings(as.numeric(rank_df$ga_rel_improvement_q95))
  rank_df$.mean_rank <- suppressWarnings(as.numeric(rank_df$ga_rel_improvement_mean))
  rank_df$.q95_rank[!is.finite(rank_df$.q95_rank)] <- -Inf
  rank_df$.mean_rank[!is.finite(rank_df$.mean_rank)] <- -Inf
  rank_df$.specialist_survivor_score <- 0.75*rank_df$.q95_rank + 0.25*rank_df$.mean_rank
  rank_df <- rank_df[order(-rank_df$.gate_rank, -rank_df$.specialist_survivor_score, -rank_df$.q95_rank, -rank_df$.mean_rank, rank_df$regime_key), , drop = FALSE]

  keep_n <- max(specialist_halving_min_keep, as.integer(ceiling(nrow(rank_df) * specialist_halving_keep_frac)))
  keep_n <- min(keep_n, nrow(rank_df))
  keep_keys <- unique(as.character(rank_df$regime_key[seq_len(keep_n)]))
  survivors <- selected[as.character(selected$regime_key) %in% keep_keys, , drop = FALSE]
  survivors$selected_for_specialist_training <- TRUE
  final_pool <- selected_regimes
  final_pool$selected_for_specialist_training <- as.character(final_pool$regime_key) %in% keep_keys

  surv_log <- rank_df[as.character(rank_df$regime_key) %in% keep_keys, , drop = FALSE]
  surv_log$specialist_halving_selected_for_final <- TRUE
  safe_write_csv(surv_log, file.path(run_dir, sprintf("REGIME_FIRST_SPECIALIST_HALVING_SELECTED__seed%d.csv", as.integer(cfg$seed)[1])))

  # Final rung: survivors only, full specialist budget. This is the final
  # estimator-discovery stage. There is no extra retraining after this; output is
  # evaluated through the heldout benchmark gate inside run_regime_specialist_ga.
  final <- tryCatch(run_regime_specialist_ga(
    selected_regimes = final_pool,
    dist_name = dist_name,
    dist_param_grid = dist_param_grid,
    cfg = cfg,
    scenario_universe_full = scenario_universe_full,
    sample_sizes = sample_sizes,
    num_samples = num_samples,
    generations_per_fold = generations_per_fold,
    k_folds = k_folds,
    objective = objective,
    use_parallel = use_parallel,
    lambda_instab = lambda_instab,
    bootstrap_B = bootstrap_B,
    t_size = t_size,
    elitism = elitism,
    immigrant_rate = immigrant_rate,
    mutation_rate_init = mutation_rate_init,
    init_alpha = init_alpha,
    alpha_mut = alpha_mut,
    check_every = check_every,
    patience = patience,
    min_delta = min_delta,
    mix_w_q95 = mix_w_q95,
    mix_w_max = mix_w_max,
    crn_env = crn_env,
    fam_key = fam_key,
    run_dir = final_dir,
    finalists_per_regime = finalists_per_regime,
    holdout_frac = holdout_frac,
    specialist_gens_frac = specialist_final_gens_frac,
    specialist_num_samples_frac = specialist_num_samples_frac,
    specialist_bootstrap_frac = specialist_bootstrap_frac,
    specialist_pop_size = specialist_final_pop_size,
    regime_neighborhood = regime_neighborhood,
    regime_neighborhood_trigger_scenarios = regime_neighborhood_trigger_scenarios,
    regime_neighborhood_max_neighbors = regime_neighborhood_max_neighbors,
    regime_neighborhood_max_scenarios = regime_neighborhood_max_scenarios,
    regime_neighborhood_max_rate_distance = regime_neighborhood_max_rate_distance,
    regime_neighborhood_max_scale_distance = regime_neighborhood_max_scale_distance,
    regime_neighborhood_min_promise_quantile = regime_neighborhood_min_promise_quantile,
    regime_global_audit = regime_global_audit,
    regime_global_audit_scenario_frac = regime_global_audit_scenario_frac,
    regime_global_audit_min_scenarios = regime_global_audit_min_scenarios,
    regime_global_audit_num_samples = regime_global_audit_num_samples,
    regime_global_audit_bootstrap_B = regime_global_audit_bootstrap_B,
    min_unique_scenarios = min_unique_scenarios,
    min_train_scenarios = min_train_scenarios,
    min_heldout_scenarios = min_heldout_scenarios,
    protected_elites = surv_log,
    protected_elite_max_per_regime = max(1L, as.integer(finalists_per_regime)[1]),
    seed_offset = seed_offset + 10000L
  ), error = function(e) {
    catf("[REGIME-FIRST] %s specialist halving final rung failed safely: %s", dist_name, conditionMessage(e))
    data.frame()
  })

  if (is.null(final) || !is.data.frame(final) || !nrow(final)) return(data.frame())
  final$specialist_halving_rung <- "final_full_budget"
  final$n_stage1_candidates <- nrow(selected)
  final$n_final_survivors <- nrow(survivors)
  final$specialist_halving_keep_frac <- specialist_halving_keep_frac
  final$specialist_halving_stage1_budget_frac <- specialist_halving_stage1_budget_frac
  final$specialist_stage1_gens_frac <- specialist_stage1_gens_frac
  final$specialist_final_gens_frac <- specialist_final_gens_frac
  final$specialist_stage1_pop_size <- specialist_stage1_pop_size
  final$specialist_final_pop_size <- specialist_final_pop_size
  final
}

# =============================================================================
# REGIME-FIRST EVOLUTIONARY DISCOVERY
# =============================================================================
# This architecture removes the expensive global Halving + global Final Eval as
# the center of the experiment. HPF1 and HPF2 are used as screening stages to
# identify promising hyperparameter configurations and regime-level opportunity.
# The actual estimator discovery is then performed by regime specialists, with
# heldout evaluation and the same benchmark gate used throughout this pipeline.
# =============================================================================
run_regime_first_one_shot <- function(...) {
  .args <- list(...)
  .get <- function(name, default = NULL) {
    if (name %in% names(.args) && !is.null(.args[[name]])) .args[[name]] else default
  }
  .safe1 <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) < 1L) return(default)
    v <- suppressWarnings(as.numeric(x[[1]]))
    if (!is.finite(v)) return(default)
    v
  }
  .stage_score_rf <- function(res_cv) {
    s <- tryCatch(res_cv$final$best_val_score, error = function(e) NA_real_)
    if (is.null(s) || length(s) != 1L || !is.finite(s)) {
      ov <- tryCatch(res_cv$final$overall, error = function(e) NULL)
      s <- if (!is.null(ov)) .safe1(ov$robust_q95_mse, Inf) else Inf
    }
    if (!is.finite(s)) Inf else s
  }
  .make_tag_rf <- function(prefix, cfg, B, ns, sf, sc_seed) {
    sprintf("%s__ps%d_mr%.2f_a%.2f_am%.2f_im%.2f_t%d_e%d_li%.2f_B%d_ns%d_sf%.2f_sc%d_seed%d",
            prefix,
            as.integer(cfg$pop_size), as.numeric(cfg$mutation_rate_init),
            as.numeric(cfg$init_alpha), as.numeric(cfg$alpha_mut),
            as.numeric(cfg$immigrant_rate), as.integer(cfg$t_size),
            as.integer(cfg$elitism), as.numeric(cfg$lambda_instab),
            as.integer(B), as.integer(ns), as.numeric(sf), as.integer(sc_seed),
            as.integer(cfg$seed))
  }
  .add_source_cfg <- function(df, cfg, tag, idx, stage = "HPF2") {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
    df$source_stage <- stage
    df$source_config_index <- idx
    df$source_config_tag <- tag
    df$source_pop_size <- as.integer(cfg$pop_size)
    df$source_mutation_rate_init <- as.numeric(cfg$mutation_rate_init)
    df$source_init_alpha <- as.numeric(cfg$init_alpha)
    df$source_alpha_mut <- as.numeric(cfg$alpha_mut)
    df$source_immigrant_rate <- as.numeric(cfg$immigrant_rate)
    df$source_t_size <- as.integer(cfg$t_size)
    df$source_elitism <- as.integer(cfg$elitism)
    df$source_seed <- as.integer(cfg$seed)
    df$source_lambda_instab <- as.numeric(cfg$lambda_instab)
    df
  }

  families_to_run <- .get("families_to_run", c("normal","lognormal","weibull","invgauss","exgaussian","exwald"))
  pop_sizes       <- .get("pop_sizes", c(100))
  mutation_rates  <- .get("mutation_rates", c(0.12, 0.18))
  init_alphas     <- .get("init_alphas", c(0.5, 1.0))
  alpha_mut_set   <- .get("alpha_mut_set", c(0.5, 1.0))
  immigrant_rates <- .get("immigrant_rates", c(0.05, 0.10))
  t_sizes         <- .get("t_sizes", c(2L, 3L))
  elitism_set     <- .get("elitism_set", c(1L, 2L))
  seeds           <- .get("seeds", c(101))

  sample_sizes <- .get("sample_sizes", c(300, 500, 1000, 2000, 5000))
  num_samples  <- .get("num_samples", 40)
  generations_per_fold <- .get("generations_per_fold", 20)
  k_folds <- .get("k_folds", 3)
  objective <- .get("objective", "mixed")
  lambda_instab_default <- .get("lambda_instab_default", 0.15)
  bootstrap_B <- .get("bootstrap_B", 100L)
  check_every <- .get("check_every", 5L)
  patience <- .get("patience", 3L)
  min_delta <- .get("min_delta", 0.005)
  use_parallel <- .get("use_parallel", TRUE)
  parallel_safety_enabled <- .get("parallel_safety_enabled", TRUE)
  parallel_reserve_cores <- as.integer(.get("parallel_reserve_cores", 3L))
  parallel_max_workers <- .get("parallel_max_workers", NA_integer_)
  parallel_min_workers <- as.integer(.get("parallel_min_workers", 1L))
  if (isTRUE(parallel_safety_enabled)) {
    Sys.setenv(GA_PARALLEL_RESERVE_CORES = as.character(parallel_reserve_cores))
    if (is.finite(suppressWarnings(as.numeric(parallel_max_workers)))) {
      Sys.setenv(GA_PARALLEL_MAX_WORKERS = as.character(as.integer(parallel_max_workers)))
    } else {
      Sys.unsetenv("GA_PARALLEL_MAX_WORKERS")
    }
    Sys.setenv(GA_PARALLEL_MIN_WORKERS = as.character(parallel_min_workers))
  }
  mix_w_q95 <- .get("mix_w_q95", 0.7)
  mix_w_max <- .get("mix_w_max", 0.3)
  out_root <- .get("out_root", OUT_ROOT)
  hp1_frac <- .get("hp1_frac", 0.25)
  hp1_k <- .get("hp1_k", NULL)
  hp1_gens <- max(3L, as.integer(.get("hp1_gens", 8L)))
  hp1_num_samples <- .get("hp1_num_samples", max(1L, round(0.4 * num_samples)))
  hp1_bootstrap_B <- .get("hp1_bootstrap_B", max(20L, round(0.33 * bootstrap_B)))
  hp1_minibatch_frac <- .get("hp1_minibatch_frac", 0.50)
  hp1_minibatch_min <- .get("hp1_minibatch_min", 8L)
  hp1_scenario_frac <- .get("hp1_scenario_frac", 0.60)
  hp2_k <- .get("hp2_k", 8L)
  hp2_gens <- max(4L, as.integer(.get("hp2_gens", 15L)))
  hp2_num_samples <- .get("hp2_num_samples", max(1L, round(0.6 * num_samples)))
  hp2_bootstrap_B <- .get("hp2_bootstrap_B", max(40L, round(0.6 * bootstrap_B)))
  hp2_minibatch_frac <- .get("hp2_minibatch_frac", 0.50)
  hp2_minibatch_min <- .get("hp2_minibatch_min", 10L)
  hp2_scenario_frac <- .get("hp2_scenario_frac", 0.80)
  fair_scenario_subsets <- .get("fair_scenario_subsets", TRUE)

  regime_top_k_per_family <- .get("regime_top_k_per_family", 5L)
  regime_min_rows <- .get("regime_min_rows", 6L)
  regime_near_margin <- .get("regime_near_margin", 0.10)
  regime_diversity_quota_per_group <- .get("regime_diversity_quota_per_group", 1L)
  regime_diversity_max_extra <- .get("regime_diversity_max_extra", 3L)
  regime_holdout_frac <- .get("regime_holdout_frac", 0.30)
  regime_min_train_scenarios <- .get("regime_min_train_scenarios", 5L)
  regime_min_heldout_scenarios <- .get("regime_min_heldout_scenarios", 1L)
  regime_specialist_finalists_per_regime <- .get("regime_specialist_finalists_per_regime", 5L)
  regime_specialist_gens_frac <- .get("regime_specialist_gens_frac", 0.50)
  regime_specialist_num_samples_frac <- .get("regime_specialist_num_samples_frac", 0.60)
  regime_specialist_bootstrap_frac <- .get("regime_specialist_bootstrap_frac", 0.60)
  regime_specialist_pop_size <- .get("regime_specialist_pop_size", NA_integer_)
  regime_specialist_stage1_gens_frac <- .get("regime_specialist_stage1_gens_frac", 0.25)
  regime_specialist_stage1_pop_size <- .get("regime_specialist_stage1_pop_size", NA_integer_)
  regime_specialist_final_gens_frac <- .get("regime_specialist_final_gens_frac", 1.00)
  regime_specialist_final_pop_size <- .get("regime_specialist_final_pop_size", 100L)
  specialist_halving_stage1_budget_frac <- .get("specialist_halving_stage1_budget_frac", 0.50)
  specialist_halving_keep_frac <- .get("specialist_halving_keep_frac", 0.50)
  specialist_halving_min_keep <- .get("specialist_halving_min_keep", 1L)
  regime_neighborhood <- .get("regime_neighborhood", TRUE)
  regime_neighborhood_trigger_scenarios <- .get("regime_neighborhood_trigger_scenarios", 10L)
  regime_neighborhood_max_neighbors <- .get("regime_neighborhood_max_neighbors", 3L)
  regime_neighborhood_max_scenarios <- .get("regime_neighborhood_max_scenarios", 24L)
  regime_neighborhood_max_rate_distance <- .get("regime_neighborhood_max_rate_distance", 1L)
  regime_neighborhood_max_scale_distance <- .get("regime_neighborhood_max_scale_distance", 1L)
  regime_neighborhood_min_promise_quantile <- .get("regime_neighborhood_min_promise_quantile", 0.40)
  regime_global_audit <- .get("regime_global_audit", TRUE)
  regime_global_audit_scenario_frac <- .get("regime_global_audit_scenario_frac", 0.10)
  regime_global_audit_min_scenarios <- .get("regime_global_audit_min_scenarios", 24L)
  regime_global_audit_num_samples <- .get("regime_global_audit_num_samples", 20L)
  regime_global_audit_bootstrap_B <- .get("regime_global_audit_bootstrap_B", 40L)
  regime_interfamily_profile_audit <- .get("regime_interfamily_profile_audit", TRUE)
  regime_interfamily_profile_audit_only_ga_winners <- .get("regime_interfamily_profile_audit_only_ga_winners", TRUE)
  regime_interfamily_profile_audit_target_families <- .get("regime_interfamily_profile_audit_target_families", families_to_run)
  regime_interfamily_profile_audit_scenario_frac <- .get("regime_interfamily_profile_audit_scenario_frac", 0.10)
  regime_interfamily_profile_audit_min_scenarios <- .get("regime_interfamily_profile_audit_min_scenarios", 24L)
  regime_interfamily_profile_audit_num_samples <- .get("regime_interfamily_profile_audit_num_samples", regime_global_audit_num_samples)
  regime_interfamily_profile_audit_bootstrap_B <- .get("regime_interfamily_profile_audit_bootstrap_B", regime_global_audit_bootstrap_B)
  regime_interfamily_profile_audit_max_targets_per_winner <- .get("regime_interfamily_profile_audit_max_targets_per_winner", 3L)
  stage_elite_topk_per_config <- max(1L, as.integer(.get("stage_elite_topk_per_config", 3L)))
  stage_elite_max_hpf1_to_hpf2 <- max(1L, as.integer(.get("stage_elite_max_hpf1_to_hpf2", 12L)))
  stage_elite_max_hpf2_to_specialist <- max(1L, as.integer(.get("stage_elite_max_hpf2_to_specialist", 3L)))

  run_id <- paste0("GA_REGIME_FIRST_", RUN_TS())
  root_out <- file.path(out_root, run_id); mkdirp(root_out)
  index_init()
  sc_universe_full  <- write_scenarios_universe(root_out, scenario_mode = "full")
  sc_universe_light <- write_scenarios_universe(root_out, scenario_mode = "light")
  meta <- list(
    run_id = run_id,
    architecture = "regime_first_hpf_screening_specialist_halving",
    note = "HPF1 and HPF2 screen regime/config candidates; a single specialist-halving stage selects survivors; heldout benchmark gate selects final winners without retraining.",
    families = families_to_run,
    k_folds = k_folds,
    objective = objective,
    seeds = paste(seeds, collapse=","),
    pop_sizes = pop_sizes,
    num_samples = num_samples,
    generations_per_fold = generations_per_fold,
    bootstrap_B = bootstrap_B,
    hp1_frac = hp1_frac,
    hp1_gens = hp1_gens,
    hp1_num_samples = hp1_num_samples,
    hp1_bootstrap_B = hp1_bootstrap_B,
    hp1_scenario_frac = hp1_scenario_frac,
    hp2_k = hp2_k,
    hp2_gens = hp2_gens,
    hp2_num_samples = hp2_num_samples,
    hp2_bootstrap_B = hp2_bootstrap_B,
    hp2_scenario_frac = hp2_scenario_frac,
    regime_top_k_per_family = regime_top_k_per_family,
    regime_min_rows = regime_min_rows,
    regime_specialist_gens_frac = regime_specialist_gens_frac,
    regime_specialist_num_samples_frac = regime_specialist_num_samples_frac,
    regime_specialist_bootstrap_frac = regime_specialist_bootstrap_frac,
    regime_specialist_stage1_gens_frac = regime_specialist_stage1_gens_frac,
    regime_specialist_stage1_pop_size = regime_specialist_stage1_pop_size,
    regime_specialist_final_gens_frac = regime_specialist_final_gens_frac,
    regime_specialist_final_pop_size = regime_specialist_final_pop_size,
    specialist_halving_stage1_budget_frac = specialist_halving_stage1_budget_frac,
    specialist_halving_keep_frac = specialist_halving_keep_frac,
    specialist_halving_min_keep = specialist_halving_min_keep,
    regime_neighborhood = regime_neighborhood,
    regime_global_audit = regime_global_audit,
    regime_global_audit_scenario_frac = regime_global_audit_scenario_frac,
    regime_global_audit_min_scenarios = regime_global_audit_min_scenarios,
    regime_global_audit_num_samples = regime_global_audit_num_samples,
    regime_global_audit_bootstrap_B = regime_global_audit_bootstrap_B,
    regime_interfamily_profile_audit = regime_interfamily_profile_audit,
    regime_interfamily_profile_audit_only_ga_winners = regime_interfamily_profile_audit_only_ga_winners,
    regime_interfamily_profile_audit_target_families = paste(regime_interfamily_profile_audit_target_families, collapse = ","),
    regime_interfamily_profile_audit_scenario_frac = regime_interfamily_profile_audit_scenario_frac,
    regime_interfamily_profile_audit_min_scenarios = regime_interfamily_profile_audit_min_scenarios,
    regime_interfamily_profile_audit_num_samples = regime_interfamily_profile_audit_num_samples,
    regime_interfamily_profile_audit_bootstrap_B = regime_interfamily_profile_audit_bootstrap_B,
    regime_interfamily_profile_audit_max_targets_per_winner = regime_interfamily_profile_audit_max_targets_per_winner,
    stage_elite_topk_per_config = stage_elite_topk_per_config,
    stage_elite_max_hpf1_to_hpf2 = stage_elite_max_hpf1_to_hpf2,
    stage_elite_max_hpf2_to_specialist = stage_elite_max_hpf2_to_specialist,
    use_parallel = use_parallel,
    parallel_safety_enabled = parallel_safety_enabled,
    parallel_total_cores_detected = suppressWarnings(parallel::detectCores(logical = TRUE)),
    parallel_reserve_cores = parallel_reserve_cores,
    parallel_max_workers = parallel_max_workers,
    parallel_min_workers = parallel_min_workers,
    parallel_workers_planned = if (exists(".ga_parallel_worker_count", mode = "function", inherits = TRUE)) {
      .ga_parallel_worker_count(use_parallel)$workers
    } else {
      NA_integer_
    }
  )
  write_manifest(root_out, meta)

  crn_env <- make_crn_indices(families = families_to_run, sample_sizes = sample_sizes,
                              B_boot = bootstrap_B, num_samples = num_samples)
  cfg_grid <- expand.grid(
    pop_size           = pop_sizes,
    mutation_rate_init = mutation_rates,
    init_alpha         = init_alphas,
    alpha_mut          = alpha_mut_set,
    immigrant_rate     = immigrant_rates,
    t_size             = t_sizes,
    elitism            = elitism_set,
    seed               = seeds,
    lambda_instab      = lambda_instab_default,
    stringsAsFactors   = FALSE
  )
  total_steps <- length(families_to_run) * (max(1L, ceiling(nrow(cfg_grid) * hp1_frac)) + hp2_k + regime_top_k_per_family)
  progress_init(max(1L, total_steps))

  winners_acc <- list(); runtime_rows <- list(); global_audit_acc <- list(); t0 <- proc.time()[["elapsed"]]

  for (dist in families_to_run) {
    message(sprintf("\n========== REGIME-FIRST FAMILY: %s ==========\n", dist))
    t_family <- proc.time()[["elapsed"]]
    grid <- param_grids[[dist]]
    fam_dir <- file.path(root_out, dist); mkdirp(fam_dir)
    sc_u_light <- add_scenario_ids(build_scenarios("light"))
    sc_u_full  <- add_scenario_ids(build_scenarios("full"))

    fam_pool <- cfg_grid
    fam_pool_phase1 <- if (!is.null(hp1_k)) {
      pick_minibatch_configs(fam_pool, k = hp1_k, seed = 13, stratify_cols = c("pop_size","t_size"))
    } else {
      pick_minibatch_configs(fam_pool, frac = hp1_frac, seed = 13, stratify_cols = c("pop_size","t_size"))
    }

    message(sprintf("[RF-HPF1 | CONFIG SCREEN] %s | configs=%d | gens=%d | ns=%d | B=%d",
                    dist, nrow(fam_pool_phase1), hp1_gens, hp1_num_samples, hp1_bootstrap_B))
    sc_seed_hpf1 <- if (isTRUE(fair_scenario_subsets)) .stable_int_seed(paste0(dist,"::RF_HPF1"), base_seed = 10001L) else 13L
    sc_used_hpf1 <- pick_scenario_subset(sc_u_light, scenario_frac = hp1_scenario_frac, min_n = k_folds, seed = sc_seed_hpf1,
                                         subset_tag = paste0(dist, "::RF_HPF1"))
    safe_write_csv(sc_used_hpf1, file.path(fam_dir, sprintf("scenarios_used_%s_RF_HPF1.csv", dist)))
    safe_write_csv(stage_difficulty(sc_used_hpf1), file.path(fam_dir, sprintf("difficulty_%s_RF_HPF1.csv", dist)))

    phase1_rows <- vector("list", nrow(fam_pool_phase1))
    discovery_hpf1_rows <- list()
    hpf1_stage_elite_rows <- list()
    for (i in seq_len(nrow(fam_pool_phase1))) {
      cfg <- fam_pool_phase1[i, ]
      tag <- .make_tag_rf("RF_HPF1", cfg, hp1_bootstrap_B, hp1_num_samples, hp1_scenario_frac, sc_seed_hpf1)
      res1 <- evolve_universal_estimator_per_family_cv(
        dist_name=dist, dist_param_grid=grid, pop_size=cfg$pop_size, generations_per_fold=hp1_gens,
        seed=cfg$seed, objective=objective, use_parallel=use_parallel, k_folds=k_folds,
        lambda_instab=cfg$lambda_instab, bootstrap_B=hp1_bootstrap_B, t_size=cfg$t_size,
        elitism=cfg$elitism, immigrant_rate=cfg$immigrant_rate, mutation_rate_init=cfg$mutation_rate_init,
        init_alpha=cfg$init_alpha, alpha_mut=cfg$alpha_mut, check_every=check_every,
        patience=patience, min_delta=min_delta, final_retrain=FALSE, mix_w_q95=mix_w_q95,
        mix_w_max=mix_w_max, minibatch_frac=hp1_minibatch_frac, minibatch_min=hp1_minibatch_min,
        crn_env=crn_env, fam_key=dist, sample_sizes=c(300, 1000), num_samples=hp1_num_samples,
        force_scenario_mode="light", scenario_frac=hp1_scenario_frac, scenario_seed=sc_seed_hpf1,
        subset_tag=paste0("RF_HPF1__", dist), scenario_universe=sc_u_light,
        scenario_subset_override=sc_used_hpf1, run_dir=fam_dir, stage_name="RF_HPF1", checkpoint_dir=fam_dir)
      ov <- res1$final$overall; sc <- .stage_score_rf(res1)
      phase1_rows[[i]] <- data.frame(family=dist, config_tag=tag, source_config_index=i, stage="RF_HPF1",
        score_stage=sc, robust_mean_mse_stage=.safe1(ov$robust_mean_mse), robust_q95_mse_stage=.safe1(ov$robust_q95_mse),
        robust_max_mse_stage=.safe1(ov$robust_max_mse), stringsAsFactors=FALSE)
      hpf1_stage_elite_rows[[length(hpf1_stage_elite_rows)+1L]] <- .make_stage_elite_archive(
        res1, family = dist, source_stage = "RF_HPF1", source_config_index = i,
        source_config_tag = tag, source_score = sc, K = stage_elite_topk_per_config
      )
      # HPF1 also performs a broad, low-cost regime screen. These rows are not
      # final evidence; they are early discovery signals that can survive into
      # HPF2/specialist if they remain promising. This prevents HPF1 from acting
      # only as config screening while missing sparse but potentially valuable regimes.
      disc1 <- tryCatch(build_regime_discovery_summary(res1$final$scenario_table, distribution=dist,
                  ga_estimator_name="robust", include_sample_size_bin=FALSE, min_rel_improvement=0.00),
                  error=function(e) data.frame())
      if (!is.null(disc1) && nrow(disc1)) discovery_hpf1_rows[[length(discovery_hpf1_rows)+1L]] <- .add_source_cfg(disc1, cfg, tag, i, "RF_HPF1")
      progress_step(sprintf("RF_HPF1 %s | cfg %d/%d", dist, i, nrow(fam_pool_phase1)))
    }
    df1 <- dplyr::bind_rows(phase1_rows); ord1 <- order(df1$score_stage)
    keep1 <- max(1L, min(hp2_k, nrow(df1)))
    fam_pool <- fam_pool_phase1[ord1[seq_len(keep1)], , drop=FALSE]
    safe_write_csv(df1[ord1, ], file.path(fam_dir, sprintf("REGIME_FIRST_HPF1_CONFIG_LOG_%s.csv", dist)))
    hpf1_disc_all <- dplyr::bind_rows(discovery_hpf1_rows)
    if (!is.null(hpf1_disc_all) && nrow(hpf1_disc_all)) {
      hpf1_disc_all <- hpf1_disc_all |>
        dplyr::arrange(dplyr::desc(gate_pass_regime), dplyr::desc(promise_score),
                       dplyr::desc(ga_rel_improvement_q95), dplyr::desc(ga_rel_improvement_mean), regime_key) |>
        dplyr::group_by(distribution, regime_key) |> dplyr::slice(1) |> dplyr::ungroup() |>
        dplyr::mutate(hpf_screen_stage = "RF_HPF1", .before = 1)
      safe_write_csv(hpf1_disc_all, file.path(fam_dir, sprintf("REGIME_FIRST_HPF1_DISCOVERY_SUMMARY__seed%d.csv", as.integer(seeds[1]))))
    }
    hpf1_stage_elites <- dplyr::bind_rows(hpf1_stage_elite_rows)
    if (!is.null(hpf1_stage_elites) && nrow(hpf1_stage_elites)) {
      safe_write_csv(hpf1_stage_elites, file.path(fam_dir, sprintf("REGIME_FIRST_HPF1_STAGE_ELITES__seed%d.csv", as.integer(seeds[1]))))
    }
    hpf1_protected_matrix <- .stage_elite_matrix(hpf1_stage_elites, max_total = stage_elite_max_hpf1_to_hpf2)

    message(sprintf("[RF-HPF2 | REGIME SCREEN] %s | configs=%d | gens=%d | ns=%d | B=%d",
                    dist, nrow(fam_pool), hp2_gens, hp2_num_samples, hp2_bootstrap_B))
    sc_seed_hpf2 <- if (isTRUE(fair_scenario_subsets)) .stable_int_seed(paste0(dist,"::RF_HPF2"), base_seed = 20002L) else 13L
    sc_used_hpf2 <- pick_scenario_subset(sc_u_full, scenario_frac = hp2_scenario_frac, min_n = k_folds, seed = sc_seed_hpf2,
                                         subset_tag = paste0(dist, "::RF_HPF2"))
    safe_write_csv(sc_used_hpf2, file.path(fam_dir, sprintf("scenarios_used_%s_RF_HPF2.csv", dist)))
    safe_write_csv(stage_difficulty(sc_used_hpf2), file.path(fam_dir, sprintf("difficulty_%s_RF_HPF2.csv", dist)))

    phase2_rows <- vector("list", nrow(fam_pool)); discovery_hpf2_rows <- list(); hpf2_stage_elite_rows <- list()
    for (i in seq_len(nrow(fam_pool))) {
      cfg <- fam_pool[i, ]
      tag <- .make_tag_rf("RF_HPF2", cfg, hp2_bootstrap_B, hp2_num_samples, hp2_scenario_frac, sc_seed_hpf2)
      res2 <- evolve_universal_estimator_per_family_cv(
        dist_name=dist, dist_param_grid=grid, pop_size=cfg$pop_size, generations_per_fold=hp2_gens,
        seed=cfg$seed, objective=objective, use_parallel=use_parallel, k_folds=k_folds,
        lambda_instab=cfg$lambda_instab, bootstrap_B=hp2_bootstrap_B, t_size=cfg$t_size,
        elitism=cfg$elitism, immigrant_rate=cfg$immigrant_rate, mutation_rate_init=cfg$mutation_rate_init,
        init_alpha=cfg$init_alpha, alpha_mut=cfg$alpha_mut, check_every=check_every,
        patience=patience, min_delta=min_delta, final_retrain=FALSE, mix_w_q95=mix_w_q95,
        mix_w_max=mix_w_max, minibatch_frac=hp2_minibatch_frac, minibatch_min=hp2_minibatch_min,
        crn_env=crn_env, fam_key=dist, sample_sizes=c(300, 500, 1000), num_samples=hp2_num_samples,
        force_scenario_mode="full", scenario_frac=hp2_scenario_frac, scenario_seed=sc_seed_hpf2,
        subset_tag=paste0("RF_HPF2__", dist), scenario_universe=sc_u_full,
        scenario_subset_override=sc_used_hpf2, run_dir=fam_dir, stage_name="RF_HPF2", checkpoint_dir=fam_dir,
        protected_elite_matrix = hpf1_protected_matrix)
      ov <- res2$final$overall; sc <- .stage_score_rf(res2)
      phase2_rows[[i]] <- data.frame(family=dist, config_tag=tag, source_config_index=i, stage="RF_HPF2",
        score_stage=sc, robust_mean_mse_stage=.safe1(ov$robust_mean_mse), robust_q95_mse_stage=.safe1(ov$robust_q95_mse),
        robust_max_mse_stage=.safe1(ov$robust_max_mse), stringsAsFactors=FALSE)
      hpf2_stage_elite_rows[[length(hpf2_stage_elite_rows)+1L]] <- .make_stage_elite_archive(
        res2, family = dist, source_stage = "RF_HPF2", source_config_index = i,
        source_config_tag = tag, source_score = sc, K = stage_elite_topk_per_config
      )
      disc <- tryCatch(build_regime_discovery_summary(res2$final$scenario_table, distribution=dist,
                  ga_estimator_name="robust", include_sample_size_bin=FALSE, min_rel_improvement=0.00),
                  error=function(e) data.frame())
      if (!is.null(disc) && nrow(disc)) discovery_hpf2_rows[[length(discovery_hpf2_rows)+1L]] <- .add_source_cfg(disc, cfg, tag, i, "RF_HPF2")
      progress_step(sprintf("RF_HPF2 %s | cfg %d/%d", dist, i, nrow(fam_pool)))
    }
    df2 <- dplyr::bind_rows(phase2_rows); safe_write_csv(df2[order(df2$score_stage), ], file.path(fam_dir, sprintf("REGIME_FIRST_HPF2_CONFIG_LOG_%s.csv", dist)))
    hpf2_disc_all <- dplyr::bind_rows(discovery_hpf2_rows)
    if (!is.null(hpf2_disc_all) && nrow(hpf2_disc_all)) {
      hpf2_disc_all <- hpf2_disc_all |>
        dplyr::arrange(dplyr::desc(gate_pass_regime), dplyr::desc(promise_score),
                       dplyr::desc(ga_rel_improvement_q95), dplyr::desc(ga_rel_improvement_mean), regime_key) |>
        dplyr::group_by(distribution, regime_key) |> dplyr::slice(1) |> dplyr::ungroup() |>
        dplyr::mutate(hpf_screen_stage = "RF_HPF2", .before = 1)
      safe_write_csv(hpf2_disc_all, file.path(fam_dir, sprintf("REGIME_FIRST_HPF2_DISCOVERY_SUMMARY__seed%d.csv", as.integer(seeds[1]))))
    }
    hpf2_stage_elites <- dplyr::bind_rows(hpf2_stage_elite_rows)
    if (!is.null(hpf2_stage_elites) && nrow(hpf2_stage_elites)) {
      safe_write_csv(hpf2_stage_elites, file.path(fam_dir, sprintf("REGIME_FIRST_HPF2_STAGE_ELITES__seed%d.csv", as.integer(seeds[1]))))
    }
    disc_all <- dplyr::bind_rows(hpf1_disc_all, hpf2_disc_all)
    if (is.null(disc_all) || !nrow(disc_all)) {
      catf("[REGIME-FIRST] %s: no regime discovery rows from HPF1/HPF2; skipping specialists.", dist)
      next
    }
    disc_all <- disc_all |>
      dplyr::mutate(hpf_stage_priority = dplyr::case_when(source_stage == "RF_HPF2" ~ 2L,
                                                          source_stage == "RF_HPF1" ~ 1L,
                                                          TRUE ~ 0L)) |>
      dplyr::arrange(dplyr::desc(gate_pass_regime), dplyr::desc(promise_score),
                     dplyr::desc(ga_rel_improvement_q95), dplyr::desc(ga_rel_improvement_mean),
                     dplyr::desc(hpf_stage_priority), regime_key) |>
      dplyr::group_by(distribution, regime_key) |> dplyr::slice(1) |> dplyr::ungroup() |>
      dplyr::mutate(regime_rank = dplyr::row_number(), hpf_screen_stage = source_stage, .before = 1)
    safe_write_csv(disc_all, file.path(fam_dir, sprintf("REGIME_FIRST_DISCOVERY_SUMMARY__seed%d.csv", as.integer(seeds[1]))))

    selected <- select_regimes_for_specialist_training(disc_all, distribution=dist,
      top_k=regime_top_k_per_family, min_rows=regime_min_rows, near_margin=regime_near_margin,
      holdout_frac=regime_holdout_frac, seed=as.integer(seeds[1]),
      min_train_scenarios=regime_min_train_scenarios, min_heldout_scenarios=regime_min_heldout_scenarios)
    selected_path <- file.path(fam_dir, sprintf("REGIME_FIRST_SELECTED_FOR_SPECIALIST_TRAINING__seed%d.csv", as.integer(seeds[1])))
    safe_write_csv(selected, selected_path)

    hpf2_protected_for_specialist <- .make_regime_protected_elites_from_selected(
      selected_regimes = selected[selected$selected_for_specialist_training %in% TRUE, , drop = FALSE],
      stage_elites = hpf2_stage_elites,
      max_per_regime = stage_elite_max_hpf2_to_specialist
    )
    if (!is.null(hpf2_protected_for_specialist) && nrow(hpf2_protected_for_specialist)) {
      safe_write_csv(hpf2_protected_for_specialist, file.path(fam_dir, sprintf("REGIME_FIRST_HPF2_PROTECTED_ELITES_FOR_SPECIALIST__seed%d.csv", as.integer(seeds[1]))))
    }

    # Use the best HPF2 config as fallback. If selected rows carry source config
    # columns, run_regime_specialist_ga will override per regime.
    best_i <- which.min(df2$score_stage)
    cfg_default <- fam_pool[best_i, , drop=FALSE]
    specialist_summary <- tryCatch(run_regime_specialist_single_halving(
      selected_regimes=selected, dist_name=dist, dist_param_grid=grid, cfg=cfg_default,
      scenario_universe_full=sc_u_full, sample_sizes=sample_sizes, num_samples=num_samples,
      generations_per_fold=generations_per_fold, k_folds=k_folds, objective=objective,
      use_parallel=use_parallel, lambda_instab=cfg_default$lambda_instab, bootstrap_B=bootstrap_B,
      t_size=cfg_default$t_size, elitism=cfg_default$elitism, immigrant_rate=cfg_default$immigrant_rate,
      mutation_rate_init=cfg_default$mutation_rate_init, init_alpha=cfg_default$init_alpha,
      alpha_mut=cfg_default$alpha_mut, check_every=check_every, patience=patience, min_delta=min_delta,
      mix_w_q95=mix_w_q95, mix_w_max=mix_w_max, crn_env=crn_env, fam_key=dist, run_dir=fam_dir,
      finalists_per_regime=regime_specialist_finalists_per_regime, holdout_frac=regime_holdout_frac,
      specialist_gens_frac=regime_specialist_gens_frac, specialist_num_samples_frac=regime_specialist_num_samples_frac,
      specialist_bootstrap_frac=regime_specialist_bootstrap_frac, specialist_pop_size=regime_specialist_pop_size,
      specialist_stage1_gens_frac=regime_specialist_stage1_gens_frac,
      specialist_stage1_pop_size=regime_specialist_stage1_pop_size,
      specialist_final_gens_frac=regime_specialist_final_gens_frac,
      specialist_final_pop_size=regime_specialist_final_pop_size,
      specialist_halving_stage1_budget_frac=specialist_halving_stage1_budget_frac,
      specialist_halving_keep_frac=specialist_halving_keep_frac,
      specialist_halving_min_keep=specialist_halving_min_keep,
      regime_neighborhood=regime_neighborhood, regime_neighborhood_trigger_scenarios=regime_neighborhood_trigger_scenarios,
      regime_neighborhood_max_neighbors=regime_neighborhood_max_neighbors, regime_neighborhood_max_scenarios=regime_neighborhood_max_scenarios,
      regime_neighborhood_max_rate_distance=regime_neighborhood_max_rate_distance, regime_neighborhood_max_scale_distance=regime_neighborhood_max_scale_distance,
      regime_neighborhood_min_promise_quantile=regime_neighborhood_min_promise_quantile,
      regime_global_audit=regime_global_audit,
      regime_global_audit_scenario_frac=regime_global_audit_scenario_frac,
      regime_global_audit_min_scenarios=regime_global_audit_min_scenarios,
      regime_global_audit_num_samples=regime_global_audit_num_samples,
      regime_global_audit_bootstrap_B=regime_global_audit_bootstrap_B,
      min_unique_scenarios=regime_min_rows,
      min_train_scenarios=regime_min_train_scenarios, min_heldout_scenarios=regime_min_heldout_scenarios,
      protected_elites = hpf2_protected_for_specialist,
      protected_elite_max_per_regime = stage_elite_max_hpf2_to_specialist),
      error=function(e){catf("[REGIME-FIRST] %s specialist failed: %s", dist, conditionMessage(e)); data.frame()})
    if (!is.null(specialist_summary) && nrow(specialist_summary)) {
      out_sum <- file.path(fam_dir, sprintf("REGIME_FIRST_WINNER_SUMMARY__seed%d.csv", as.integer(seeds[1])))
      safe_write_csv(specialist_summary, out_sum)
      ga_wins <- sum(specialist_summary$gate_pass %in% TRUE, na.rm=TRUE)
      evaluated <- sum(specialist_summary$status == "evaluated", na.rm=TRUE)
      best_q95 <- suppressWarnings(max(specialist_summary$ga_rel_improvement_q95, na.rm=TRUE))
      best_mean <- suppressWarnings(max(specialist_summary$ga_rel_improvement_mean, na.rm=TRUE))
      if (!is.finite(best_q95)) best_q95 <- NA_real_
      if (!is.finite(best_mean)) best_mean <- NA_real_
      global_audit_runs <- sum(specialist_summary$global_audit_run %in% TRUE, na.rm=TRUE)
      global_audit_passes <- sum(specialist_summary$global_audit_gate_pass %in% TRUE, na.rm=TRUE)
      nfl_count <- sum(as.character(specialist_summary$global_audit_interpretation) == "regime_specific_no_free_lunch", na.rm=TRUE)
      global_bonus_count <- sum(as.character(specialist_summary$global_audit_interpretation) == "regime_and_global_bonus", na.rm=TRUE)
      global_audit_acc[[length(global_audit_acc)+1L]] <- specialist_summary[specialist_summary$status == "evaluated", , drop=FALSE]
      winners_acc[[length(winners_acc)+1L]] <- data.frame(family=dist, architecture="regime_first", evaluated_regimes=evaluated,
        ga_regime_wins=ga_wins, benchmark_regime_wins=max(0L, evaluated-ga_wins), best_ga_rel_improvement_q95=best_q95,
        best_ga_rel_improvement_mean=best_mean,
        global_audit_runs=global_audit_runs, global_audit_passes=global_audit_passes,
        no_free_lunch_regime_specific_count=nfl_count,
        global_generalization_bonus_count=global_bonus_count,
        stringsAsFactors=FALSE)
    }
    runtime_rows[[length(runtime_rows)+1L]] <- data.frame(family=dist, phase="regime_first_family", elapsed_secs=round(proc.time()[["elapsed"]]-t_family,2),
      n_configs_eval=nrow(fam_pool_phase1)+nrow(fam_pool), n_scenarios=nrow(sc_u_full), stringsAsFactors=FALSE)
  }
  progress_finalize()
  winners_df <- if (length(winners_acc)) dplyr::bind_rows(winners_acc) else data.frame()
  safe_write_csv(winners_df, file.path(root_out, "REGIME_FIRST__ALL_FAMILIES_SUMMARY.csv"))
  audit_all <- if (length(global_audit_acc)) dplyr::bind_rows(global_audit_acc) else data.frame()
  if (nrow(audit_all)) {
    safe_write_csv(audit_all, file.path(root_out, "REGIME_FIRST_PROFILE_MATCHED_AUDIT__ALL_FAMILIES.csv"))
  }
  interfamily_profile_out <- list(summary = data.frame(), scenarios = data.frame())
  if (isTRUE(regime_interfamily_profile_audit) && nrow(audit_all)) {
    interfamily_profile_out <- tryCatch(
      run_interfamily_profile_matched_audit(
        specialist_summary = audit_all,
        target_families = regime_interfamily_profile_audit_target_families,
        param_grids = param_grids,
        sample_sizes = sample_sizes,
        crn_env = crn_env,
        out_dir = root_out,
        source_families = families_to_run,
        num_samples = regime_interfamily_profile_audit_num_samples,
        bootstrap_B = regime_interfamily_profile_audit_bootstrap_B,
        scenario_frac = regime_interfamily_profile_audit_scenario_frac,
        min_scenarios = regime_interfamily_profile_audit_min_scenarios,
        max_targets_per_winner = regime_interfamily_profile_audit_max_targets_per_winner,
        only_ga_winners = regime_interfamily_profile_audit_only_ga_winners,
        max_rate_distance = regime_neighborhood_max_rate_distance,
        max_scale_distance = regime_neighborhood_max_scale_distance,
        seed = as.integer(seeds[1])
      ),
      error = function(e) {
        catf("[REGIME-FIRST] Inter-family profile-matched audit failed safely: %s", conditionMessage(e))
        list(summary = data.frame(), scenarios = data.frame())
      }
    )
  }
  thesis_compact <- .build_regime_first_thesis_results_compact(
    local_summary = audit_all,
    interfamily_summary = if (is.list(interfamily_profile_out) && is.data.frame(interfamily_profile_out$summary)) interfamily_profile_out$summary else data.frame()
  )
  if (nrow(thesis_compact)) {
    safe_write_csv(thesis_compact, file.path(root_out, "REGIME_FIRST_THESIS_RESULTS_COMPACT.csv"))
  }

  method_audit <- data.frame(
    architecture = "regime_first_hpf_screening_specialist_halving_with_profile_matched_audit",
    global_training_removed = TRUE,
    hpf1_role = "broad_regime_screen_on_60pct_light_subset",
    hpf2_role = "richer_regime_screen_on_80pct_full_subset",
    specialist_halving_role = "reduced_budget_screen_then_top5_full_budget_survivor_evaluation_with_early_stopping",
    heldout_gate_role = "select_final_winner_by_regime_without_extra_retraining",
    pre_neighborhood_selection_role = "coarse_regime_grouping_then_elite_neighborhood_then_eligibility",
    profile_matched_audit_role = "fixed_weight_intra_family_profile_matched_generalization_without_retraining",
    interfamily_profile_matched_audit_role = "fixed_weight_cross_family_profile_matched_generalization_for_GA_winners_only_without_retraining",
    interfamily_profile_audit_run = isTRUE(regime_interfamily_profile_audit),
    interfamily_profile_audit_rows = if (is.list(interfamily_profile_out) && is.data.frame(interfamily_profile_out$summary)) nrow(interfamily_profile_out$summary) else 0L,
    stage_elite_inheritance = TRUE,
    multiseed_ready = TRUE,
    stage_elite_rule = "HPF1_to_HPF2__HPF2_to_specialist_stage1__stage1_to_final_heldout",
    stage_elite_protection_duration = "one_stage_only",
    legacy_global_transfer_removed = TRUE,
    legacy_global_audit_csv_aliases_removed = TRUE,
    compact_thesis_summary_written = nrow(thesis_compact) > 0L,
    benchmark_gate_kept = TRUE,
    retraining_after_heldout = FALSE,
    stringsAsFactors = FALSE
  )
  safe_write_csv(method_audit, file.path(root_out, "REGIME_FIRST_METHOD_AUDIT_SUMMARY.csv"))
  if (length(runtime_rows)) {
    rt <- dplyr::bind_rows(runtime_rows); rt$total_experiment_secs <- round(proc.time()[["elapsed"]]-t0,2)
    safe_write_csv(rt, file.path(root_out, "runtime_log.csv"))
  }
  invisible(list(out_dir=root_out, winners=winners_df, architecture="regime_first"))
}


# ====================== MULTI-NODE WRAPPER ======================
# Runs the same pipeline, but distributes families across multiple machines.
# Design:
#   - One PSOCK worker per host (master+workers)
#   - Each worker receives a subset of families (capacity-aware)
#   - Inside each node, existing use_parallel=TRUE uses local cores

run_all_one_shot_multinode <- function(
    hosts,
    families_to_run,
    script_path = NULL,
    bench_seconds = 0.25,
    seed = 123,
    outfile = "",
    family_weights = NULL,
    ...
) {
  stopifnot(is.character(hosts), length(hosts) >= 1L)
  stopifnot(is.character(families_to_run), length(families_to_run) >= 1L)
  
  # 1) Build controller (1 worker per node) and benchmark capacity
  ctrl <- setup_multinode_controller(hosts, bench_seconds = bench_seconds, seed = seed, outfile = outfile)
  cl   <- ctrl$cl
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  
  # 2) Ensure workers have the full function environment
  if (is.null(script_path)) {
    script_path <- tryCatch(normalizePath(sys.frame(1)$ofile), error = function(e) NA_character_)
    if (!is.character(script_path) || is.na(script_path) || !nzchar(script_path)) script_path <- NA_character_
  }
  if (!is.character(script_path) || is.na(script_path) || !nzchar(script_path)) {
    stop("run_all_one_shot_multinode requires script_path that exists on ALL nodes (shared filesystem recommended).")
  }
  # Source the script on each worker node.
  parallel::clusterCall(cl, function(p) { source(p, local = FALSE); NULL }, script_path)
  
  # 3) Capacity-aware family assignment
  fams <- families_to_run
  
  if (is.null(family_weights)) {
    if (exists("param_grids", mode = "list")) {
      family_weights <- vapply(fams, function(d) {
        g <- param_grids[[d]]
        if (is.null(g)) 1 else nrow(g)
      }, numeric(1))
      names(family_weights) <- fams
    } else {
      family_weights <- setNames(rep(1, length(fams)), fams)
    }
  }
  
  buckets <- .assign_families_by_capacity(fams, node_scores = ctrl$scores, family_weights = family_weights)
  
  # 4) Run each bucket on its node. Each node will create its own out_dir.
  # Make output paths node-specific to avoid collisions.
  res <- parallel::parLapply(cl, seq_along(buckets), function(i) {
    my_fams <- buckets[[i]]
    if (length(my_fams) == 0L) return(NULL)
    run_all_one_shot(families_to_run = my_fams, ...)
  })
  
  list(
    hosts = hosts,
    node_caps = ctrl$caps,
    node_scores = ctrl$scores,
    family_buckets = buckets,
    results = res
  )
}


# The only supported architecture is regime-first specialist halving.

.FULL_PARAMS <- list(
  families_to_run = c("normal","lognormal","weibull","invgauss","exgaussian","exwald"),
  
  # ── Hyperparameter search grid ─────────────────────────────────────────────
  # Reduced to 64 combinations (two-level factorial on 6 params, fixed pop and
  # seed) to fit a 48-60h single-machine budget. Two-level design covers the
  # extremes of each dimension — standard practice in ML HP search.
  pop_sizes       = c(100),           
  mutation_rates  = c(0.12, 0.18),
  init_alphas     = c(0.5, 1.0),
  alpha_mut_set   = c(0.5, 1.0),
  immigrant_rates = c(0.05, 0.10),
  t_sizes         = c(2L, 3L),
  elitism_set     = c(1L, 2L),
  seeds           = c(101, 202),            
  
  # ── Scenario / CV budget ───────────────────────────────────────────────────
  # R=40: stable for bootstrap Q95 (literature standard: R=25-50).
  # G=30: converges with warm-start + early stopping (confirmed in smoke log).
  # B=100: bootstrap stable at R=40.
  sample_sizes         = c(300, 500, 1000, 2000, 5000),
  num_samples          = 40,
  generations_per_fold = 20,
  k_folds              = 3,
  
  # ── Final regime selection: heldout benchmark gate, no extra retrain ─────
  regime_specialist_finalists_per_regime = 5L,
  
  # ── Fitness objective and regularization ───────────────────────────────────
  objective             = "mixed",
  mix_w_q95             = 0.7,
  mix_w_max             = 0.3,
  lambda_instab_default = 0.15,
  bootstrap_B           = 100L,
  check_every           = 5L,
  patience              = 3L,
  min_delta             = 0.005,
  
  use_parallel                  = TRUE,
  parallel_safety_enabled       = TRUE,
  parallel_reserve_cores        = 3L,
  parallel_max_workers          = NA_integer_,
  parallel_min_workers          = 1L,
  out_root                      = OUT_ROOT,
  suppress_intermediate_saves   = FALSE,
  
  # ── Phase 1: HP/regime screening (broader 40% scenario exposure) ───────────
  hp_strict          = TRUE,
  hp1_frac           = 0.25,
  hp1_k              = NULL,
  hp1_gens           = 8L,
  hp1_num_samples    = 15L,
  hp1_bootstrap_B    = 40L,
  hp1_minibatch_frac = 0.50,
  hp1_minibatch_min  = 8L,
  hp1_scenario_frac  = 0.60,
  
  # ── Phase 2: HP/regime refinement (60% full-grid exposure) ────────────────
  hp2_k              = 8L,
  hp2_gens           = 15L,
  hp2_num_samples    = 25L,
  hp2_bootstrap_B    = 60L,
  hp2_minibatch_frac = 0.50,
  hp2_minibatch_min  = 10L,
  hp2_scenario_frac  = 0.80,
  
  # ── Specialist single-halving (regime-first only) ────────────────────────
  # Stage 1 evaluates all HPF2-selected regime/config candidates at reduced
  # specialist budget; the final rung evaluates only survivors at full specialist
  # budget and applies the heldout benchmark gate. No global halving/final eval.
  specialist_halving_stage1_budget_frac = 0.50,
  specialist_halving_keep_frac          = 0.50,
  specialist_halving_min_keep           = 1L,
  
  regime_discovery = TRUE,
  regime_top_k_per_family = 5L,
  regime_min_rows = 6L,
  regime_near_margin = 0.10,
  regime_diversity_quota_per_group = 1L,
  regime_diversity_max_extra = 3L,
  regime_holdout_frac = 0.30,
  regime_min_train_scenarios = 5L,
  regime_min_heldout_scenarios = 1L,
  regime_specialist_training = TRUE,
  # Specialist-halving resources. HPF1/HPF2 receive broader scenario exposure;
  # final specialist uses pop=100 and up to 20 generations with early stopping.
  # Top-5 fixed candidates are evaluated in heldout without retraining.
  regime_specialist_gens_frac = 0.50,
  regime_specialist_num_samples_frac = 0.60,
  regime_specialist_bootstrap_frac = 0.60,
  regime_specialist_pop_size = NA_integer_,
  regime_specialist_stage1_gens_frac = 0.25,
  regime_specialist_stage1_pop_size = NA_integer_,
  regime_specialist_final_gens_frac = 1.00,
  regime_specialist_final_pop_size = 100L,
  regime_neighborhood = TRUE,
  regime_neighborhood_trigger_scenarios = 10L,
  regime_neighborhood_max_neighbors = 3L,
  regime_neighborhood_max_scenarios = 24L,
  regime_neighborhood_max_rate_distance = 1L,
  regime_neighborhood_max_scale_distance = 1L,
  regime_neighborhood_min_promise_quantile = 0.40,
  regime_global_audit = TRUE,
  regime_global_audit_scenario_frac = 0.10,
  regime_global_audit_min_scenarios = 24L,
  regime_global_audit_num_samples = 20L,
  regime_global_audit_bootstrap_B = 40L,
  regime_interfamily_profile_audit = TRUE,
  regime_interfamily_profile_audit_only_ga_winners = TRUE,
  regime_interfamily_profile_audit_target_families = c("normal","lognormal","weibull","invgauss","exgaussian","exwald"),
  regime_interfamily_profile_audit_scenario_frac = 0.10,
  regime_interfamily_profile_audit_min_scenarios = 24L,
  regime_interfamily_profile_audit_num_samples = 20L,
  regime_interfamily_profile_audit_bootstrap_B = 40L,
  regime_interfamily_profile_audit_max_targets_per_winner = 3L,
  # One-stage elite inheritance across the whole funnel:
  # HPF1 elites -> protected in HPF2; HPF2 elites -> protected in specialist stage1;
  # specialist stage1 elites -> protected in final heldout. Protection lasts one stage only.
  stage_elite_topk_per_config = 3L,
  stage_elite_max_hpf1_to_hpf2 = 12L,
  stage_elite_max_hpf2_to_specialist = 3L,
  regime_first_mode = TRUE
)

# ── Smoke-results override (set by run_experiment.R when RUN_MODE=smoke_results) ──
# Overrides families and budget for a 2-family viability check.
# Does NOT change the algorithm — only the scope and budget.
if (nzchar(Sys.getenv("SMOKE_FAMILIES", ""))) {
  fam_override <- trimws(strsplit(Sys.getenv("SMOKE_FAMILIES"), ",")[[1]])
  .FULL_PARAMS$families_to_run <- fam_override
  r_val  <- suppressWarnings(as.integer(Sys.getenv("SMOKE_R",  "25")))
  g_val  <- suppressWarnings(as.integer(Sys.getenv("SMOKE_G",  "20")))
  b_val  <- suppressWarnings(as.integer(Sys.getenv("SMOKE_B",  "60")))
  hp2k   <- suppressWarnings(as.integer(Sys.getenv("SMOKE_HP2_K",    "4")))
  sfrac  <- suppressWarnings(as.numeric(Sys.getenv("SMOKE_SCEN_FRAC","0.5")))
  .FULL_PARAMS$num_samples          <- r_val
  .FULL_PARAMS$generations_per_fold <- g_val
  .FULL_PARAMS$bootstrap_B          <- as.integer(b_val)
  .FULL_PARAMS$hp2_k                <- as.integer(hp2k)
  .FULL_PARAMS$hp1_scenario_frac    <- sfrac * 0.5
  .FULL_PARAMS$hp2_scenario_frac    <- sfrac
  catf("[SMOKE] Overrides applied: families=%s | R=%d | G=%d | B=%d\n",
       paste(fam_override, collapse="+"), r_val, g_val, b_val)
}

if (RUN_MODE == "micro") {
  catf("RUN_MODE=micro | preflight only | OUT=%s", OUT_ROOT)
  sc_micro <- add_scenario_ids(build_scenarios_light())
  sc_micro <- .add_regime_columns(sc_micro, distribution = "normal", include_sample_size_bin = FALSE)
  micro_report <- data.frame(
    check = c("modules_loaded", "scenario_grid_built", "regime_columns_added", "specialist_function_available", "specialist_halving_available"),
    status = c(TRUE, nrow(sc_micro) > 0L, "regime_key" %in% names(sc_micro), exists("run_regime_specialist_ga", mode = "function", inherits = TRUE), exists("run_regime_specialist_single_halving", mode = "function", inherits = TRUE)),
    stringsAsFactors = FALSE
  )
  mkdirp(OUT_ROOT)
  safe_write_csv(micro_report, file.path(OUT_ROOT, "MICRO_PREFLIGHT_REPORT.csv"))
  safe_write_csv(sc_micro, file.path(OUT_ROOT, "MICRO_REGIME_SCENARIOS.csv"))
  out_micro <- list(status = "PASS", checks = micro_report)
  print(out_micro)
  catf("Micro preflight complete. Outputs: %s", normalizePath(OUT_ROOT, winslash="/", mustWork=FALSE))

} else if (RUN_MODE == "quick") {
  # ============================================================================
  # QUICK — single-machine smoke test
  # Runs 1 family with a tiny budget to verify the full pipeline produces
  # outputs end-to-end. The 3-phase funnel structure is preserved but all
  # budgets are micro-sized so it completes in a few minutes.
  # ============================================================================
  catf("RUN_MODE=quick | OS=%s | cores=%d | OUT=%s",
       .Platform$OS.type,
       max(1L, parallel::detectCores() - 1L),
       OUT_ROOT)
  
  out_quick <- run_all_one_shot(
    families_to_run      = c("normal"),
    
    # ── Micro hyperparameter pool (single config) ──────────────────────────
    pop_sizes       = c(20),
    mutation_rates  = c(0.15),
    init_alphas     = c(0.7),
    alpha_mut_set   = c(0.7),
    immigrant_rates = c(0.08),
    t_sizes         = c(2L),
    elitism_set     = c(1L),
    seeds           = c(101),
    
    sample_sizes         = c(300),
    num_samples          = 10,
    generations_per_fold = 8,
    k_folds              = 2,
    
    objective             = "mixed",
    mix_w_q95             = 0.7,
    mix_w_max             = 0.3,
    lambda_instab_default = 0.15,
    bootstrap_B           = 30L,
    check_every           = 2L,
    patience              = 1L,
    min_delta             = 0.02,
    
    use_parallel                  = TRUE,
    out_root                      = OUT_ROOT,
    suppress_intermediate_saves   = FALSE,
    
    # ── Regime-first funnel: HPF1/HPF2 screen, specialist halving decides ───
    hp_strict           = TRUE,
    hp1_frac            = 0.50,
    hp1_k               = NULL,
    hp1_gens            = 3L,
    hp1_num_samples     = 6,
    hp1_bootstrap_B     = 20L,
    hp1_minibatch_frac  = 0.60,
    hp1_minibatch_min   = 6L,
    
    hp2_k               = 4L,
    hp2_gens            = 4L,
    hp2_num_samples     = 8,
    hp2_bootstrap_B     = 25L,
    hp2_minibatch_frac  = 0.60,
    hp2_minibatch_min   = 8L,
    
    specialist_halving_stage1_budget_frac = 0.50,
    specialist_halving_keep_frac          = 0.50,
    specialist_halving_min_keep           = 1L
  )
  
  print(out_quick)
  catf("Quick run complete. Check: %s",
       normalizePath(OUT_ROOT, winslash="/", mustWork=FALSE))
  
} else if (RUN_MODE %in% c("full", "final")) {
  # ============================================================================
  # FULL EXPERIMENT — regime-first funnel with specialist single-halving
  #
  # Automatically selects single-node or multi-node mode:
  #
  #   MULTI-NODE  (Linux + CLUSTER_HOSTS set):
  #     Families are distributed across the cluster using capacity-aware
  #     bin-packing. Each node runs its own PSOCK intra-node parallelism.
  #     Requires SCRIPT_PATH pointing to this file on a shared filesystem.
  #
  #   SINGLE-NODE (without CLUSTER_HOSTS):
  #     Standard PSOCK parallelism across local cores (detectCores()-1).
  #     Identical algorithm and parameters; just no cross-machine dispatch.
  #
  # Architecture:
  #   HPF1 screens regime/config candidates at low budget.
  #   HPF2 screens surviving regime/config candidates with more budget.
  #   Specialist single-halving evaluates selected regimes only.
  #   Heldout benchmark gate selects GA or benchmark without any final retrain.
  # ============================================================================
  
  is_linux       <- (.Platform$OS.type == "unix")
  cluster_hosts  <- trimws(Sys.getenv("CLUSTER_HOSTS", unset = ""))
  use_multinode  <- is_linux && nzchar(cluster_hosts)
  
  catf("RUN_MODE=full | OS=%s | multi-node=%s | OUT=%s",
       .Platform$OS.type,
       if (use_multinode) "YES" else "NO (single-machine)",
       OUT_ROOT)
  
  if (use_multinode) {
    # ── MULTI-NODE path ───────────────────────────────────────────────────────
    hosts       <- trimws(strsplit(cluster_hosts, ",")[[1]])
    script_path <- Sys.getenv("SCRIPT_PATH",
                              unset = normalizePath(
                                file.path(this_dir, "08_LocationEstimators.R"),
                                winslash = "/", mustWork = FALSE))
    
    catf("Cluster nodes: %s", paste(hosts, collapse = ", "))
    catf("Script path on nodes: %s", script_path)
    
    out_full <- do.call(
      run_all_one_shot_multinode,
      c(list(hosts       = hosts,
             script_path = script_path,
             seed        = 123L),
        .FULL_PARAMS)
    )
    
    catf("Multi-node full run complete.")
    catf("Node assignments:")
    for (i in seq_along(out_full$family_buckets)) {
      catf("  Node %d (%s): %s",
           i, hosts[i],
           paste(out_full$family_buckets[[i]], collapse=", "))
    }
    
  } else {
    # ── SINGLE-NODE path ──────────────────────────────────────────────────────
    if (!is_linux) {
      catf("Windows detected — running single-machine PSOCK parallelism.")
    } else {
      catf("Linux single-machine mode (set CLUSTER_HOSTS to enable multi-node).")
    }
    
    out_full <- do.call(run_all_one_shot, .FULL_PARAMS)
    
    catf("Single-node full run complete.")
  }
  
  catf("Outputs: %s",
       normalizePath(OUT_ROOT, winslash="/", mustWork=FALSE))
  
} else {
  # RUN_MODE = "none" or unrecognised — do nothing when sourced as a module
  invisible(NULL)
}

mark_module_done("08_LocationEstimators.R")
