# =============================================================================
# generate_external_report.R
# =============================================================================
# Generates a concise PDF summary from the completed external-battery tables.
# The report is descriptive and uses the dataset-level FDR layer as the primary
# multiplicity summary. Base R is sufficient.
# =============================================================================

.read_split_csv <- function(path) {
  if (!file.exists(path)) return(data.frame())
  x <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = TRUE),
    error = function(e) data.frame()
  )
  if (!nrow(x)) return(x)

  if (all(c("part_file", "rows") %in% names(x))) {
    part_paths <- file.path(dirname(path), x$part_file)
    part_paths <- part_paths[file.exists(part_paths)]
    if (!length(part_paths)) return(data.frame())
    parts <- lapply(part_paths, function(p) {
      tryCatch(utils::read.csv(p, stringsAsFactors = FALSE, check.names = TRUE),
               error = function(e) data.frame())
    })
    parts <- parts[vapply(parts, nrow, integer(1)) > 0L]
    if (!length(parts)) return(data.frame())
    return(do.call(rbind, parts))
  }

  x
}

.as_bool <- function(x) {
  if (is.logical(x)) return(x)
  tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}

.is_nonempty <- function(x) is.data.frame(x) && nrow(x) > 0L

.new_page <- function(title, subtitle = NULL) {
  graphics::plot.new()
  graphics::par(mar = c(1, 1, 1, 1))
  graphics::text(0.04, 0.95, title, adj = c(0, 1), cex = 1.45, font = 2)
  if (!is.null(subtitle)) {
    graphics::text(0.04, 0.89, paste(strwrap(subtitle, 110), collapse = "\n"),
                   adj = c(0, 1), cex = 0.82)
  }
}

.text_page <- function(title, lines, subtitle = NULL) {
  .new_page(title, subtitle)
  y <- 0.82
  for (line in lines) {
    wrapped <- strwrap(as.character(line), width = 105)
    if (!length(wrapped)) wrapped <- ""
    for (part in wrapped) {
      graphics::text(0.05, y, part, adj = c(0, 1), cex = 0.82)
      y <- y - 0.047
      if (y < 0.07) return(invisible(NULL))
    }
    y <- y - 0.012
  }
  invisible(NULL)
}

.bar_page <- function(title, values, subtitle = NULL) {
  values <- values[is.finite(values) & values >= 0]
  if (!length(values)) return(invisible(NULL))
  old <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old), add = TRUE)
  graphics::par(mar = c(8, 5, 4, 1))
  graphics::barplot(values, las = 2, cex.names = 0.7, ylab = "Count", main = title)
  if (!is.null(subtitle)) graphics::mtext(subtitle, side = 1, line = 6.7, cex = 0.68)
  invisible(NULL)
}

.table_page <- function(title, df, columns, max_rows = 12L) {
  columns <- intersect(columns, names(df))
  if (!length(columns) || !nrow(df)) return(invisible(NULL))
  x <- df[seq_len(min(max_rows, nrow(df))), columns, drop = FALSE]
  x[] <- lapply(x, function(z) {
    z <- as.character(z)
    z[is.na(z)] <- ""
    substr(z, 1, 26)
  })

  .new_page(title)
  nr <- nrow(x)
  nc <- ncol(x)
  x0 <- 0.03
  y0 <- 0.82
  cell_w <- 0.94 / nc
  cell_h <- min(0.055, 0.65 / (nr + 1))

  graphics::rect(x0, y0, x0 + 0.94, y0 - cell_h,
                 border = NA, col = grDevices::gray(0.90))
  for (j in seq_len(nc)) {
    graphics::text(x0 + (j - 0.5) * cell_w, y0 - cell_h * 0.55,
                   substr(names(x)[j], 1, 18), cex = 0.50, font = 2)
  }
  for (i in seq_len(nr)) {
    y <- y0 - cell_h * i
    if (i %% 2 == 0) {
      graphics::rect(x0, y, x0 + 0.94, y - cell_h,
                     border = NA, col = grDevices::gray(0.96))
    }
    for (j in seq_len(nc)) {
      graphics::text(x0 + (j - 0.5) * cell_w, y - cell_h * 0.55,
                     x[i, j], cex = 0.48)
    }
  }
  invisible(NULL)
}

generate_external_report <- function(
    run_dir,
    basic_dir = file.path(run_dir, "01_results_basic"),
    audit_dir = file.path(run_dir, "03_audit_ready"),
    report_dir = file.path(run_dir, "02_reports"),
    run_label = basename(run_dir),
    output_pdf = file.path(report_dir, "REALWORLD_EXTERNAL_BATTERY_RESULTS_REPORT.pdf")) {

  dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

  compact <- .read_split_csv(file.path(basic_dir, "external_primary_results_compact.csv"))
  survivors <- .read_split_csv(file.path(basic_dir, "external_fdr_survivors_dataset_level.csv"))
  registry <- .read_split_csv(file.path(audit_dir, "external_dataset_registry.csv"))

  n_requested <- if (.is_nonempty(registry)) nrow(registry) else NA_integer_
  n_loaded <- if (.is_nonempty(registry) && "load_ok" %in% names(registry)) {
    sum(.as_bool(registry$load_ok), na.rm = TRUE)
  } else NA_integer_
  n_primary <- if (.is_nonempty(compact)) nrow(compact) else 0L
  n_strong <- if (.is_nonempty(compact) && "any_primary_strong_win" %in% names(compact)) {
    sum(.as_bool(compact$any_primary_strong_win), na.rm = TRUE)
  } else 0L
  n_fdr05 <- if (.is_nonempty(compact) && "dataset_level_fdr_pass_primary" %in% names(compact)) {
    sum(.as_bool(compact$dataset_level_fdr_pass_primary), na.rm = TRUE)
  } else 0L
  n_fdr10 <- if (.is_nonempty(compact) && "dataset_level_fdr_pass_secondary" %in% names(compact)) {
    sum(.as_bool(compact$dataset_level_fdr_pass_secondary), na.rm = TRUE)
  } else 0L

  grDevices::pdf(output_pdf, width = 11, height = 8.5, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)

  .text_page(
    "Real-World External Battery Results",
    c(
      sprintf("Run: %s", run_label),
      sprintf("Registry rows: %s", n_requested),
      sprintf("Loaded sources: %s", n_loaded),
      sprintf("Dataset-level decision rows: %d", n_primary),
      sprintf("Strong-win rows before dataset-level FDR: %d", n_strong),
      sprintf("Rows passing dataset-level BH-FDR at 5%%: %d", n_fdr05),
      sprintf("Rows passing dataset-level BH-FDR at 10%%: %d", n_fdr10),
      "Primary inference uses eligible, profile-matched dataset-level comparisons after Benjamini-Hochberg FDR correction. Depth probes and oracle comparisons are retained as secondary audit layers."
    )
  )

  if (.is_nonempty(compact) && "best_evidence_layer" %in% names(compact)) {
    .bar_page(
      "Dataset-Level Evidence",
      sort(table(compact$best_evidence_layer), decreasing = TRUE),
      "Counts summarize the strongest evidence label assigned to each dataset-level row."
    )
  }

  if (.is_nonempty(compact) && "empirical_profile" %in% names(compact)) {
    .bar_page(
      "Empirical Profile Coverage",
      sort(table(compact$empirical_profile), decreasing = TRUE),
      "Profile counts show where the external validation evidence is concentrated."
    )
  }

  if (.is_nonempty(survivors)) {
    .table_page(
      "Dataset-Level FDR Survivors",
      survivors,
      c("dataset_id", "empirical_profile", "specialist_id", "specialist_family",
        "best_gain_q95", "best_ci_low", "dataset_level_q_value_bh"),
      max_rows = 15L
    )
  }

  invisible(output_pdf)
}

# Compatibility alias used by the recorded external-battery runner.
generate_external_report_v5 <- generate_external_report
