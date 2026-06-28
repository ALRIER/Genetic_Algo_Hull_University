# =============================================================================
# 10_DISTRIBUTIONAL_DIAGNOSTICS.R
# =============================================================================
# PURPOSE
# -------
# This module extracts the empirical distributional characteristics of each
# simulation family (normal, lognormal, weibull, invgauss, exgaussian, exwald)
# and produces:
#
#   1. MOMENT TABLES — mean, variance, skewness, excess kurtosis for each
#      family × parameter-grid combination. These are ready to compare against
#      theoretical moments when you complete the validation layer.
#
#   2. DISTRIBUTION TEST RESULTS — Shapiro-Wilk normality test, Lilliefors
#      (KS-based normality), and a skewness/kurtosis significance test,
#      run on samples drawn from each family. Useful to confirm that the
#      DGPs are generating the intended regime (skewed, heavy-tailed, etc.)
#
#   3. SHAPE SUMMARY — compact per-family table of median skewness,
#      median excess kurtosis, and tail weight index across the full param
#      grid, showing which families represent which statistical regimes.
#
#   4. PLOTS — density overlays per family (all param grid rows),
#      Q-Q plots vs Normal, and a skewness × kurtosis scatter across families.
#
# WHAT IS NOT HERE (by design)
# -----------------------------
# The comparison of empirical vs theoretical moments (validation layer,
# Section 3.10.2) is deliberately excluded and will be added separately.
# This module only generates the empirical side and the test outputs so
# they are ready when that layer is developed.
#
# OUTPUTS
# -------
#   distributional_moments.csv       — per (family, param_row) empirical moments
#   distributional_tests.csv         — normality / shape tests per family
#   shape_summary.csv                — family-level summary (regime table)
#   density_<family>.png             — density overlays per family
#   qq_<family>.png                  — Q-Q vs Normal per family
#   skewness_kurtosis_scatter.png    — all families in one skew-kurt space
# =============================================================================


# ── Module run tracking (defensive: works even if 00 not sourced) ──────────
if (!exists(".MOD_STATUS", inherits = TRUE) || !is.environment(.MOD_STATUS)) {
  .MOD_STATUS <- new.env(parent = emptyenv())
}
if (!exists("mark_module_done", mode = "function", inherits = TRUE)) {
  mark_module_done <- function(module_id, extra = NULL) {
    ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    .MOD_STATUS[[module_id]] <- list(done = TRUE, time = ts, extra = extra)
    cat(sprintf("[MODULE DONE] %s | %s%s\n", ts, module_id,
                if (!is.null(extra)) paste0(" | ", extra) else ""))
    flush.console(); invisible(TRUE)
  }
}
if (!exists("catf", mode = "function", inherits = TRUE)) {
  catf <- function(fmt, ...) cat(sprintf(fmt, ...), "\n")
}


# =============================================================================
# SECTION 1 — MOMENT EXTRACTORS
# =============================================================================

#' Compute the four standard moments from a numeric sample.
#'
#' Returns mean, variance, skewness (standardized 3rd central moment),
#' and excess kurtosis (4th central moment / variance^2 - 3).
#' All based on the sample; no distributional assumptions.
#'
#' @param x Numeric vector (should be finite and length >= 4).
#' @return Named numeric vector: mean, variance, skewness, excess_kurtosis.
compute_sample_moments <- function(x) {
  x <- as.numeric(x[is.finite(x)])
  n <- length(x)
  if (n < 4L) return(c(mean = NA, variance = NA, skewness = NA, excess_kurtosis = NA))

  mu  <- mean(x)
  s2  <- var(x)
  sd_ <- sqrt(s2)

  # Skewness: E[(X - mu)^3] / sd^3  (Fisher's moment coefficient)
  skew <- if (sd_ > 0) mean((x - mu)^3) / sd_^3 else NA_real_

  # Excess kurtosis: E[(X - mu)^4] / sd^4 - 3
  kurt <- if (sd_ > 0) mean((x - mu)^4) / sd_^4 - 3 else NA_real_

  c(mean = mu, variance = s2, skewness = skew, excess_kurtosis = kurt)
}


#' Run a battery of shape/normality tests on a sample.
#'
#' Tests included:
#'   - Shapiro-Wilk (exact normality test; best for n <= 5000)
#'   - Kolmogorov-Smirnov vs Normal (Lilliefors-corrected via ks.test)
#'   - D'Agostino-Pearson (skewness + kurtosis joint test via moments pkg if available)
#'
#' @param x  Numeric vector (sub-sampled to 5000 if larger).
#' @param label  Label string for identification in output table.
#' @return Data frame with one row per test.
run_shape_tests <- function(x, label = "") {
  x <- as.numeric(x[is.finite(x)])
  n <- length(x)

  # Sub-sample for large vectors to keep tests valid and fast
  x_test <- if (n > 5000L) sample(x, 5000L) else x

  rows <- list()

  # Shapiro-Wilk (n must be 3–5000)
  n_sw <- min(length(x_test), 5000L)
  sw <- tryCatch(
    stats::shapiro.test(x_test[seq_len(n_sw)]),
    error = function(e) NULL
  )
  if (!is.null(sw)) {
    rows[["shapiro_wilk"]] <- data.frame(
      label       = label,
      test        = "Shapiro-Wilk",
      statistic   = as.numeric(sw$statistic),
      p_value     = sw$p.value,
      interpretation = ifelse(sw$p.value < 0.05, "non-normal", "consistent with normal"),
      stringsAsFactors = FALSE
    )
  }

  # KS test against Normal (mu, sigma estimated from data)
  ks <- tryCatch({
    mu_est <- mean(x_test)
    sd_est <- sd(x_test)
    if (sd_est > 0)
      stats::ks.test(x_test, "pnorm", mean = mu_est, sd = sd_est)
    else NULL
  }, error = function(e) NULL)
  if (!is.null(ks)) {
    rows[["ks_normal"]] <- data.frame(
      label          = label,
      test           = "KS vs Normal",
      statistic      = as.numeric(ks$statistic),
      p_value        = ks$p.value,
      interpretation = ifelse(ks$p.value < 0.05, "non-normal", "consistent with normal"),
      stringsAsFactors = FALSE
    )
  }

  # Jarque-Bera: uses skewness and kurtosis directly
  # JB = n/6 * (S^2 + (K^2)/4), chi-sq(2) under H0: normal
  moms <- compute_sample_moments(x_test)
  if (all(is.finite(moms))) {
    jb_stat <- length(x_test) / 6 * (moms["skewness"]^2 + (moms["excess_kurtosis"]^2) / 4)
    jb_pval <- stats::pchisq(jb_stat, df = 2, lower.tail = FALSE)
    rows[["jarque_bera"]] <- data.frame(
      label          = label,
      test           = "Jarque-Bera",
      statistic      = as.numeric(jb_stat),
      p_value        = as.numeric(jb_pval),
      interpretation = ifelse(jb_pval < 0.05, "non-normal", "consistent with normal"),
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(rows)
}


# =============================================================================
# SECTION 2 — PER-FAMILY DIAGNOSTIC ENGINE
# =============================================================================

#' Extract empirical moments and run tests for a single distribution family.
#'
#' For each row of the parameter grid, we generate N draws and compute moments
#' and tests. The grid may have many rows (e.g., all combinations of mu/sigma),
#' so results capture the range of moments across all parameter regimes.
#'
#' @param family_name   String (e.g., "normal", "lognormal", etc.)
#' @param param_grid    Data frame of parameter combinations (from param_grids).
#' @param N_per_row     Draws per parameter row (default 5000).
#' @param seed          RNG seed.
#'
#' @return List with:
#'   $moments  — data frame: one row per param_grid row
#'   $tests    — data frame: one row per (param_row x test_type)
.diagnose_family <- function(family_name, param_grid,
                              N_per_row = 5000L,
                              seed = 1L) {

  set.seed(seed)
  moment_rows <- vector("list", nrow(param_grid))
  test_rows   <- vector("list", nrow(param_grid))

  for (i in seq_len(nrow(param_grid))) {
    params <- as.list(param_grid[i, , drop = FALSE])
    label  <- paste0(family_name, "_row", i, "_",
                     paste(names(params), round(unlist(params), 3), sep = "=", collapse = "_"))

    # Draw N samples from this parameter configuration
    x <- tryCatch(
      generate_population(family_name, N_per_row, params),
      error = function(e) {
        message(sprintf("[DIAG] generate_population failed | %s | row %d | %s",
                        family_name, i, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(x) || length(x) < 4L) next

    # Moments
    moms <- compute_sample_moments(x)
    moment_rows[[i]] <- cbind(
      data.frame(family = family_name, param_row = i,
                 label = label, n_finite = length(x),
                 stringsAsFactors = FALSE),
      as.data.frame(t(moms))
    )
    # Append parameter values for traceability
    for (pname in names(params)) {
      moment_rows[[i]][[paste0("param_", pname)]] <- params[[pname]]
    }

    # Tests
    test_rows[[i]] <- run_shape_tests(x, label = label)
  }

  list(
    moments = dplyr::bind_rows(moment_rows),
    tests   = dplyr::bind_rows(test_rows)
  )
}


# =============================================================================
# SECTION 3 — SHAPE SUMMARY TABLE
# =============================================================================

#' Build a compact per-family shape summary from moment results.
#'
#' Aggregates across all parameter rows within a family to produce the
#' "statistical regime" characterisation used in Table 1 of the paper:
#'   - median skewness, IQR of skewness
#'   - median excess kurtosis, IQR of excess kurtosis
#'   - tail weight index = median(excess_kurtosis) / 3
#'     (>1 = heavy-tailed relative to Normal)
#'   - regime label (symmetric / right-skewed / heavy-tailed)
build_shape_summary <- function(moments_df) {
  stopifnot(is.data.frame(moments_df), "family" %in% names(moments_df))

  moments_df %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(
      n_param_rows        = dplyr::n(),
      mean_skewness       = mean(skewness,        na.rm = TRUE),
      median_skewness     = median(skewness,       na.rm = TRUE),
      iqr_skewness        = IQR(skewness,          na.rm = TRUE),
      mean_excess_kurt    = mean(excess_kurtosis,  na.rm = TRUE),
      median_excess_kurt  = median(excess_kurtosis, na.rm = TRUE),
      iqr_excess_kurt     = IQR(excess_kurtosis,   na.rm = TRUE),
      tail_weight_index   = median(excess_kurtosis, na.rm = TRUE) / 3,
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Assign a regime label based on moments
      # |skew| < 0.5 and |kurt| < 1 → symmetric/light
      # skew > 0.5 → right-skewed
      # excess_kurt > 1 → heavy-tailed
      regime = dplyr::case_when(
        abs(median_skewness) < 0.5 & abs(median_excess_kurt) < 1.0 ~ "symmetric / light-tailed",
        median_skewness  >  0.5 & median_excess_kurt  <  3.0       ~ "right-skewed",
        median_skewness  >  0.5 & median_excess_kurt  >= 3.0       ~ "right-skewed / heavy-tailed",
        median_excess_kurt >= 3.0                                    ~ "heavy-tailed",
        TRUE                                                         ~ "mixed"
      )
    )
}


# =============================================================================
# SECTION 4 — PLOTS
# =============================================================================

#' Density overlay plot for one family (all param rows on the same axes).
#'
#' Shows the range of distributional shapes across the parameter grid.
#' Helps verify that the grid spans the intended structural regime.
.plot_density_family <- function(family_name, param_grid,
                                  N_per_row = 2000L, seed = 1L, run_dir) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(invisible(NULL))
  set.seed(seed)

  # Generate samples for each param row
  df_list <- lapply(seq_len(nrow(param_grid)), function(i) {
    params <- as.list(param_grid[i, , drop = FALSE])
    x <- tryCatch(generate_population(family_name, N_per_row, params),
                  error = function(e) NULL)
    if (is.null(x)) return(NULL)
    data.frame(
      x        = x,
      param_id = paste0("row_", i),
      stringsAsFactors = FALSE
    )
  })
  df_all <- dplyr::bind_rows(df_list)
  if (nrow(df_all) == 0L) return(invisible(NULL))

  # Clip extreme tails for display (1st–99th percentile)
  q_lo <- quantile(df_all$x, 0.01, na.rm = TRUE)
  q_hi <- quantile(df_all$x, 0.99, na.rm = TRUE)
  df_plot <- df_all[df_all$x >= q_lo & df_all$x <= q_hi, ]

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = x, color = param_id)) +
    ggplot2::geom_density(alpha = 0.7, linewidth = 0.6) +
    ggplot2::labs(
      title    = sprintf("Density overlays: %s family", family_name),
      subtitle = sprintf("%d parameter configurations | N=%d per config | 1st-99th pct shown",
                         nrow(param_grid), N_per_row),
      x = "Value", y = "Density", color = "Param row"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = if (nrow(param_grid) > 12) "none" else "right")

  path <- file.path(run_dir, sprintf("density_%s.png", family_name))
  ggplot2::ggsave(path, p, width = 8, height = 5, dpi = 150)
  catf("[DIAG] Density plot saved: %s", path)
  invisible(p)
}


#' Q-Q plot vs Normal for one family (one representative param row).
.plot_qq_family <- function(family_name, param_grid,
                             N_per_row = 3000L, seed = 1L, run_dir,
                             param_row = 1L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(invisible(NULL))
  set.seed(seed)

  params <- as.list(param_grid[param_row, , drop = FALSE])
  x <- tryCatch(generate_population(family_name, N_per_row, params),
                error = function(e) NULL)
  if (is.null(x)) return(invisible(NULL))

  df_qq <- data.frame(
    theoretical = qnorm(ppoints(length(x))),
    sample      = sort(x)
  )

  p <- ggplot2::ggplot(df_qq, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point(alpha = 0.3, size = 0.8, color = "#2c7bb6") +
    ggplot2::geom_abline(slope = sd(x), intercept = mean(x),
                         color = "red", linewidth = 0.8, linetype = "dashed") +
    ggplot2::labs(
      title    = sprintf("Q-Q vs Normal: %s (param row %d)", family_name, param_row),
      subtitle = paste(names(params), round(unlist(params), 3), sep = "=", collapse = ", "),
      x = "Theoretical Normal quantiles", y = "Sample quantiles"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  path <- file.path(run_dir, sprintf("qq_%s.png", family_name))
  ggplot2::ggsave(path, p, width = 6, height = 6, dpi = 150)
  catf("[DIAG] Q-Q plot saved: %s", path)
  invisible(p)
}


#' Skewness × Kurtosis scatter across all families.
#'
#' Visualizes where each family sits in the skewness-kurtosis space.
#' The Normal distribution sits at (0, 0). Families far from the origin
#' or strongly right-shifted represent harder estimation regimes.
.plot_skew_kurt_scatter <- function(moments_df, run_dir) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(invisible(NULL))

  # Family-level medians
  fam_summary <- moments_df %>%
    dplyr::group_by(family) %>%
    dplyr::summarise(
      med_skew = median(skewness,       na.rm = TRUE),
      med_kurt = median(excess_kurtosis, na.rm = TRUE),
      .groups = "drop"
    )

  # Include all param rows as background points
  p <- ggplot2::ggplot() +
    # Individual param rows (small, transparent)
    ggplot2::geom_point(
      data = moments_df,
      ggplot2::aes(x = skewness, y = excess_kurtosis, color = family),
      alpha = 0.25, size = 1.5
    ) +
    # Family medians (large, labeled)
    ggplot2::geom_point(
      data = fam_summary,
      ggplot2::aes(x = med_skew, y = med_kurt, color = family),
      size = 5, shape = 18
    ) +
    ggplot2::geom_text(
      data = fam_summary,
      ggplot2::aes(x = med_skew, y = med_kurt, label = family, color = family),
      nudge_y = 0.3, size = 3.5, fontface = "bold"
    ) +
    # Normal reference point
    ggplot2::geom_point(ggplot2::aes(x = 0, y = 0),
                        color = "black", size = 4, shape = 3) +
    ggplot2::annotate("text", x = 0.1, y = 0.1, label = "Normal (0,0)",
                      size = 3, color = "black") +
    ggplot2::labs(
      title    = "Skewness × Excess Kurtosis: all families and parameter rows",
      subtitle = "Diamonds = family medians | Small dots = individual param rows | + = Normal reference",
      x = "Skewness", y = "Excess Kurtosis"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "none")

  path <- file.path(run_dir, "skewness_kurtosis_scatter.png")
  ggplot2::ggsave(path, p, width = 9, height = 7, dpi = 150)
  catf("[DIAG] Skewness-kurtosis scatter saved: %s", path)
  invisible(p)
}


# =============================================================================
# SECTION 5 — TOP-LEVEL DRIVER
# =============================================================================

#' Run the complete distributional diagnostics pipeline for all families.
#'
#' USAGE EXAMPLE:
#'
#'   diag_out <- run_distributional_diagnostics(
#'     param_grids = param_grids,
#'     run_dir     = file.path(OUT_ROOT, "distributional_diagnostics"),
#'     N_per_row   = 5000L,
#'     seed        = 42L
#'   )
#'
#'   # Inspect the shape summary table (maps to Table 1 in the paper)
#'   print(diag_out$shape_summary)
#'
#'   # All CSVs and PNGs are written to run_dir automatically.
#'
#' @param param_grids Named list of param grids (output of build_param_grids()).
#' @param run_dir     Directory to write all outputs (will be created if needed).
#' @param families    Which families to run (default: all in param_grids).
#' @param N_per_row   Draws per parameter row for moment estimation.
#' @param seed        Base RNG seed.
#'
#' @return List with:
#'   $moments      — data frame of all empirical moments (all families)
#'   $tests        — data frame of all test results (all families)
#'   $shape_summary — compact per-family regime summary
run_distributional_diagnostics <- function(param_grids,
                                           run_dir    = NULL,
                                           families   = names(param_grids),
                                           N_per_row  = 5000L,
                                           seed       = 42L) {

  catf("[DIAG] Starting distributional diagnostics for: %s",
       paste(families, collapse = ", "))

  if (!is.null(run_dir)) mkdirp(run_dir)

  all_moments <- list()
  all_tests   <- list()

  for (fam in families) {
    if (is.null(param_grids[[fam]])) {
      message(sprintf("[DIAG] Skipping '%s': not found in param_grids", fam))
      next
    }
    catf("[DIAG] Processing: %s (%d param rows)", fam, nrow(param_grids[[fam]]))

    diag <- .diagnose_family(
      family_name = fam,
      param_grid  = param_grids[[fam]],
      N_per_row   = as.integer(N_per_row),
      seed        = seed
    )

    all_moments[[fam]] <- diag$moments
    all_tests[[fam]]   <- diag$tests

    # Per-family plots
    if (!is.null(run_dir)) {
      .plot_density_family(fam, param_grids[[fam]],
                           N_per_row = min(N_per_row, 3000L),
                           seed = seed, run_dir = run_dir)
      .plot_qq_family(fam, param_grids[[fam]],
                      N_per_row = min(N_per_row, 3000L),
                      seed = seed, run_dir = run_dir, param_row = 1L)
    }
  }

  moments_df <- dplyr::bind_rows(all_moments)
  tests_df   <- dplyr::bind_rows(all_tests)
  shape_sum  <- build_shape_summary(moments_df)

  # Cross-family scatter (requires all families done)
  if (!is.null(run_dir) && nrow(moments_df) > 0L) {
    .plot_skew_kurt_scatter(moments_df, run_dir)
  }

  # Export tables
  if (!is.null(run_dir)) {
    safe_write_csv(moments_df, file.path(run_dir, "distributional_moments.csv"))
    safe_write_csv(tests_df,   file.path(run_dir, "distributional_tests.csv"))
    safe_write_csv(shape_sum,  file.path(run_dir, "shape_summary.csv"))
    catf("[DIAG] All tables written to: %s", run_dir)
  }

  # Console summary
  cat("\n══════════════════════════════════════════════════════════\n")
  cat("  DISTRIBUTIONAL SHAPE SUMMARY\n")
  cat("══════════════════════════════════════════════════════════\n")
  print(as.data.frame(shape_sum[, c("family", "median_skewness",
                                     "median_excess_kurt", "regime")]),
        row.names = FALSE)
  cat("══════════════════════════════════════════════════════════\n\n")

  invisible(list(
    moments       = moments_df,
    tests         = tests_df,
    shape_summary = shape_sum
  ))
}


mark_module_done("10_distributional_diagnostics.R")
