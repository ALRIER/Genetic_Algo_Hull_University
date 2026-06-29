# =============================================================================
# GENERATE_EXTERNAL_REPORT_V5.R
# =============================================================================
# Standalone PDF report generator for the V5 real-world external battery.
# It reads the clean V5 CSV layout and writes an interpretation-first PDF report
# with English captions, taxonomy logic, FDR status, tables, and base-R figures.
# No non-base R package is required.
# =============================================================================

.gr_read_csv_split <- function(path) {
  if (!file.exists(path)) return(data.frame())
  x <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE),
                error = function(e) data.frame())
  if (!nrow(x)) return(x)
  if (all(c("part_file", "rows") %in% names(x))) {
    base_dir <- dirname(path)
    parts <- file.path(base_dir, x$part_file)
    parts <- parts[file.exists(parts)]
    if (!length(parts)) return(x)
    out <- lapply(parts, function(p) tryCatch(utils::read.csv(p, stringsAsFactors = FALSE, check.names = TRUE),
                                              error = function(e) data.frame()))
    out <- out[vapply(out, nrow, integer(1)) > 0L]
    if (!length(out)) return(data.frame())
    return(do.call(rbind, out))
  }
  x
}

.gr_num <- function(x) suppressWarnings(as.numeric(x))
.gr_bool <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}
.gr_ntrue <- function(x) sum(.gr_bool(x), na.rm = TRUE)
.gr_pct <- function(n, d) if (is.finite(d) && d > 0) sprintf("%.1f%%", 100 * n / d) else "NA"
.gr_safe <- function(x, default = "") if (length(x) && !is.na(x[1]) && nzchar(as.character(x[1]))) as.character(x[1]) else default
.gr_wrap <- function(x, width = 105) paste(strwrap(as.character(x), width = width), collapse = "\n")
.gr_nonempty <- function(x) is.data.frame(x) && nrow(x) > 0L

.gr_page <- function(title, subtitle = NULL) {
  graphics::plot.new()
  graphics::par(mar = c(1, 1, 1, 1))
  graphics::text(0.03, 0.96, title, adj = c(0, 1), cex = 1.55, font = 2)
  if (!is.null(subtitle)) graphics::text(0.03, 0.90, .gr_wrap(subtitle, 120), adj = c(0, 1), cex = 0.85)
}

.gr_text_block <- function(lines, x = 0.04, y = 0.84, cex = 0.82, line_height = 0.045) {
  if (!length(lines)) return(invisible(NULL))
  yy <- y
  for (ln in lines) {
    wrapped <- strwrap(as.character(ln), width = 118)
    if (!length(wrapped)) wrapped <- ""
    for (w in wrapped) {
      graphics::text(x, yy, w, adj = c(0, 1), cex = cex)
      yy <- yy - line_height
      if (yy < 0.05) return(invisible(yy))
    }
    yy <- yy - line_height * 0.30
  }
  invisible(yy)
}

.gr_table_page <- function(title, df, subtitle = NULL, max_rows = 12L) {
  .gr_page(title, subtitle)
  if (!.gr_nonempty(df)) {
    .gr_text_block("No rows available for this table.")
    return(invisible(NULL))
  }
  df <- df[seq_len(min(max_rows, nrow(df))), , drop = FALSE]
  df[] <- lapply(df, function(z) {
    z <- as.character(z)
    z[is.na(z)] <- ""
    z <- substr(z, 1, 28)
    z
  })
  nr <- nrow(df); nc <- ncol(df)
  x0 <- 0.03; y0 <- 0.82; w <- 0.94 / max(nc, 1); h <- min(0.055, 0.62 / max(nr + 1, 1))
  graphics::rect(x0, y0, x0 + 0.94, y0 - h, border = NA, col = grDevices::gray(0.90))
  for (j in seq_len(nc)) {
    graphics::text(x0 + (j - 0.5) * w, y0 - h * 0.55, substr(names(df)[j], 1, 18), cex = 0.50, font = 2)
  }
  for (i in seq_len(nr)) {
    y <- y0 - h * i
    if (i %% 2 == 0) graphics::rect(x0, y, x0 + 0.94, y - h, border = NA, col = grDevices::gray(0.96))
    for (j in seq_len(nc)) {
      graphics::text(x0 + (j - 0.5) * w, y - h * 0.55, df[i, j], cex = 0.48)
    }
  }
  graphics::text(0.03, 0.06, sprintf("Showing %d of %d rows.", nr, nrow(df)), adj = c(0, 1), cex = 0.70)
}

.gr_bar_page <- function(title, counts, subtitle = NULL, ylab = "Count") {
  .gr_page(title, subtitle)
  counts <- counts[is.finite(counts) & counts >= 0]
  if (!length(counts)) {
    .gr_text_block("No count data available for this figure.")
    return(invisible(NULL))
  }
  old <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(8, 5, 4, 1))
  graphics::barplot(counts, las = 2, cex.names = 0.65, ylab = ylab, main = "")
  graphics::mtext("Figure legend: counts are computed from V5 final CSVs. Primary interpretation should use dataset-level FDR columns, not raw gate-row totals.", side = 1, line = 6.8, cex = 0.65)
}

.gr_hist_page <- function(title, values, subtitle = NULL, xlab = "Value") {
  .gr_page(title, subtitle)
  values <- values[is.finite(values)]
  if (!length(values)) {
    .gr_text_block("No numeric values available for this figure.")
    return(invisible(NULL))
  }
  old <- graphics::par(no.readonly = TRUE); on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(5, 5, 4, 1))
  graphics::hist(values, breaks = "FD", xlab = xlab, main = "")
  graphics::mtext("Figure legend: this distribution is descriptive and should be read together with CI and BH-FDR fields.", side = 1, line = 3.5, cex = 0.65)
}

generate_external_report_v5 <- function(run_dir,
                                        basic_dir = file.path(run_dir, "01_results_basic"),
                                        audit_dir = file.path(run_dir, "03_audit_ready"),
                                        report_dir = file.path(run_dir, "02_reports"),
                                        run_label = basename(run_dir),
                                        output_pdf = file.path(report_dir, "external_realworld_results_report_V5.pdf")) {
  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
  compact <- .gr_read_csv_split(file.path(basic_dir, "external_primary_results_compact.csv"))
  survivors <- .gr_read_csv_split(file.path(basic_dir, "external_fdr_survivors_dataset_level.csv"))
  saturation <- .gr_read_csv_split(file.path(basic_dir, "external_dataset_saturation_flags.csv"))
  taxonomy <- .gr_read_csv_split(file.path(basic_dir, "external_taxonomy_summary.csv"))
  grid <- .gr_read_csv_split(file.path(basic_dir, "external_evidence_grid.csv"))
  survival <- .gr_read_csv_split(file.path(basic_dir, "external_survival_map.csv"))
  methods <- .gr_read_csv_split(file.path(basic_dir, "external_methods_audit_flags.csv"))
  registry <- .gr_read_csv_split(file.path(audit_dir, "external_dataset_registry.csv"))
  gate <- .gr_read_csv_split(file.path(audit_dir, "external_gate_decisions.csv"))
  oracle <- .gr_read_csv_split(file.path(audit_dir, "external_oracle_audit.csv"))

  n_datasets <- if (.gr_nonempty(registry) && "dataset_id" %in% names(registry)) length(unique(registry$dataset_id[.gr_bool(registry$load_ok)])) else NA_integer_
  n_primary_rows <- if (.gr_nonempty(compact)) nrow(compact) else 0L
  n_strong <- if (.gr_nonempty(compact) && "any_primary_strong_win" %in% names(compact)) .gr_ntrue(compact$any_primary_strong_win) else 0L
  n_fdr05 <- if (.gr_nonempty(compact) && "dataset_level_fdr_pass_primary" %in% names(compact)) .gr_ntrue(compact$dataset_level_fdr_pass_primary) else 0L
  n_fdr10 <- if (.gr_nonempty(compact) && "dataset_level_fdr_pass_secondary" %in% names(compact)) .gr_ntrue(compact$dataset_level_fdr_pass_secondary) else 0L
  n_sat <- if (.gr_nonempty(saturation)) nrow(saturation) else 0L
  n_oracle <- if (.gr_nonempty(oracle)) nrow(oracle) else 0L

  grDevices::pdf(output_pdf, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  .gr_page("V5 Real-World External Battery Report", sprintf("Run label: %s", run_label))
  .gr_text_block(c(
    sprintf("Loaded dataset count recorded in registry: %s", as.character(n_datasets)),
    sprintf("Dataset-level primary decision rows: %d", n_primary_rows),
    sprintf("Primary strong-win rows before dataset-level BH-FDR: %d", n_strong),
    sprintf("Rows surviving BH-FDR at 5%%: %d", n_fdr05),
    sprintf("Rows surviving BH-FDR at 10%% sensitivity: %d", n_fdr10),
    sprintf("Saturated datasets flagged by the 10-win cap: %d", n_sat),
    sprintf("Secondary oracle audit rows: %d", n_oracle),
    "Interpretation principle: the primary claim is not the raw number of gate-row wins. The primary claim is the number, profile, and concentration of eligible in-regime dataset-level signals after paired CI and BH-FDR correction. Secondary depth probes and oracle rows are transparency layers, not independent discoveries.",
    "No Free Lunch interpretation: broad failure to dominate is not a weakness. It is evidence that the GA specialist is conditional. The important signal is whether the few surviving gains are concentrated in the predicted empirical profiles rather than scattered across unrelated regimes."
  ))

  .gr_page("Pre-registered winner taxonomy", "This page defines the final tournament grid used in the CSVs and PDF.")
  .gr_text_block(c(
    "1. Eligibility layer: eligible means the empirical dataset profile matches the specialist family under fixed thresholds. Not eligible means control or cross-regime audit, not a primary loss or win.",
    "2. Regime layer: in_regime rows define the confirmatory path. cross_regime_audit rows are retained for transparency and to detect accidental wins outside the predicted regime.",
    "3. Benchmark layer: primary_preregistered uses the fixed benchmark assigned by profile. Oracle comparisons are written separately and must not be promoted to the primary claim.",
    "4. Evidence layer: strong_win requires joint improvement and CI support; strong_point_signal has positive gate evidence but not full CI support; pareto_signal improves one metric without material harm to the other; equal_weight_gain shows learned weights beat naive equal weights; no_win means the benchmark remains dominant.",
    "5. CI layer: ci_confirmed requires the paired bootstrap interval for the reported gain to stay above zero. near_ci means the point estimate is positive but the interval touches zero. descriptive means no statistical support.",
    "6. FDR layer: dataset_level_q_value_bh is the primary multiplicity correction. Gate-row FDR is retained for audit only because condition rows within a dataset are correlated."
  ))

  if (.gr_nonempty(methods)) {
    method_lines <- c(
      paste("Profile gate policy:", .gr_safe(methods$profile_gate_policy)),
      paste("FDR policy:", .gr_safe(methods$fdr_policy)),
      paste("Output layout:", .gr_safe(methods$output_layout_policy)),
      paste("CI policy:", .gr_safe(methods$paired_ci_policy)),
      sprintf("Saturation cap: %s countable primary wins per dataset.", .gr_safe(methods$wins_saturation_cap)),
      sprintf("Depth probes secondary only: %s.", .gr_safe(methods$depth_probe_secondary_only))
    )
    .gr_page("Audit policies embedded in the output CSVs", "These policies are also written into external_methods_audit_flags.csv and relevant decision tables.")
    .gr_text_block(method_lines)
  }

  if (.gr_nonempty(compact) && "empirical_profile" %in% names(compact)) {
    counts <- sort(table(compact$empirical_profile), decreasing = TRUE)
    .gr_bar_page("Dataset-level coverage by empirical profile", counts,
                 "A balanced external battery should avoid concentration in a single profile or catalog source.",
                 ylab = "Dataset-level rows")
  }

  if (.gr_nonempty(compact) && "best_evidence_layer" %in% names(compact)) {
    counts <- sort(table(compact$best_evidence_layer), decreasing = TRUE)
    .gr_bar_page("Best evidence layer by dataset-level decision", counts,
                 "This is the main hierarchy of evidence. Strong wins are the only primary winners; Pareto and equal-weight outcomes are supporting evidence.",
                 ylab = "Dataset-level rows")
  }

  if (.gr_nonempty(compact) && "best_ci_layer" %in% names(compact)) {
    counts <- sort(table(compact$best_ci_layer), decreasing = TRUE)
    .gr_bar_page("CI layer distribution", counts,
                 "The paired bootstrap interval is aligned to the same gain statistic reported in the decision table.",
                 ylab = "Dataset-level rows")
  }

  if (.gr_nonempty(compact) && "dataset_level_q_value_bh" %in% names(compact)) {
    .gr_hist_page("Dataset-level BH q-value distribution", .gr_num(compact$dataset_level_q_value_bh),
                  "The 5% and 10% FDR flags in the CSVs should be used as the final multiplicity layer.",
                  xlab = "BH q-value")
  }

  if (.gr_nonempty(compact) && all(c("best_gain_q95", "dataset_id") %in% names(compact))) {
    z <- compact[is.finite(.gr_num(compact$best_gain_q95)), , drop = FALSE]
    if (nrow(z)) {
      z$gain <- .gr_num(z$best_gain_q95)
      z <- z[order(-z$gain), , drop = FALSE]
      top <- z[seq_len(min(15L, nrow(z))), c(intersect(c("dataset_id", "empirical_profile", "specialist_family", "best_gain_q95", "best_ci_low", "dataset_level_q_value_bh", "best_evidence_layer"), names(z))), drop = FALSE]
      .gr_table_page("Top dataset-level q95 gains", top,
                     "Sorted by best q95 gain. This table is descriptive until CI and FDR columns are checked.",
                     max_rows = 15L)
    }
  }

  if (.gr_nonempty(survivors)) {
    keep <- intersect(c("dataset_id", "empirical_profile", "specialist_family", "best_gain_q95", "best_ci_low", "dataset_level_q_value_bh", "best_final_evidence_label"), names(survivors))
    .gr_table_page("Rows surviving dataset-level BH-FDR at the sensitivity threshold", survivors[, keep, drop = FALSE],
                   "These are the most defensible candidates. Use the 5% flag for the strict claim and 10% as sensitivity analysis.",
                   max_rows = 15L)
  } else {
    .gr_page("Rows surviving dataset-level BH-FDR", "No rows survived the FDR threshold in the available CSVs, or the survivor table was not generated.")
    .gr_text_block(c(
      "This outcome is still publishable if interpreted correctly: it indicates that the external battery supports conditionality and benchmark retention more strongly than broad transfer dominance.",
      "Report this as evidence against universal dominance, with equal-weight gains and Pareto signals treated as secondary evidence rather than confirmatory wins."
    ))
  }

  if (.gr_nonempty(saturation)) {
    keep <- intersect(c("dataset_id", "empirical_profile", "specialist_family", "n_countable_primary_wins", "n_uncapped_primary_strong_wins", "dataset_saturated", "dataset_stop_reason"), names(saturation))
    .gr_table_page("Saturated datasets", saturation[, keep, drop = FALSE],
                   "Saturation prevents one dataset from inflating the primary N. Additional signal remains visible but not count-inflating.",
                   max_rows = 12L)
  }

  if (.gr_nonempty(grid)) {
    .gr_table_page("Taxonomy grid counts", grid,
                   "This grid cross-tabulates eligibility, regime, benchmark, evidence, and CI layers.",
                   max_rows = 18L)
  }

  if (.gr_nonempty(survival)) {
    keep <- intersect(c("specialist_id", "domain", "n_rows", "n_strong_win", "n_pareto_signal", "n_equal_weight_gain", "median_q95_gain", "best_dataset"), names(survival))
    .gr_table_page("Specialist-domain survival map", survival[, keep, drop = FALSE],
                   "This table identifies where specialist families retain useful transfer signal and where benchmark control dominates.",
                   max_rows = 18L)
  }

  .gr_page("Substantive interpretation", "Suggested reading of V5 outputs.")
  interpretation <- c(
    sprintf("The strict primary result is the FDR-adjusted dataset-level count: %d rows at 5%% and %d rows at 10%% sensitivity. This should be reported before any larger condition-level or depth-probe counts.", n_fdr05, n_fdr10),
    "If surviving rows concentrate in lognormal_like or weibull_like profiles, the correct interpretation is not universal superiority. It is regime-conditional transfer: GA specialists retain value where the external dataset resembles the discovery regime.",
    "If most datasets remain no_win or near_ci, that supports the No Free Lunch frame: robust benchmarks continue to dominate outside the specialist regime, and gains are localized rather than universal.",
    "If a dataset becomes saturated, the cap prevents one public source or one data structure from dominating the primary count, while still preserving the repeated signal in the audit trail.",
    "Depth probes should be discussed as within-dataset mechanism checks. They can deepen the explanation of a signal but should never be counted as independent datasets.",
    "The oracle audit asks what happens against the best available benchmark. It is intentionally secondary because an oracle benchmark is stronger than a pre-registered real comparator and can be biased by selection across many benchmarks."
  )
  .gr_text_block(interpretation)

  md <- c(
    "# V5 Real-World External Battery Report",
    "",
    sprintf("Run label: `%s`", run_label),
    sprintf("Dataset-level rows: %d", n_primary_rows),
    sprintf("Strong wins before dataset-level FDR: %d", n_strong),
    sprintf("BH-FDR survivors at 5%%: %d", n_fdr05),
    sprintf("BH-FDR survivors at 10%%: %d", n_fdr10),
    sprintf("Saturated datasets: %d", n_sat),
    "",
    "Primary interpretation: use dataset-level FDR survivors as the strict confirmatory claim. Use Pareto, equal-weight, oracle, and depth-probe outputs as secondary evidence only."
  )
  writeLines(md, file.path(report_dir, "external_realworld_results_report_V5.md"))
  invisible(output_pdf)
}
