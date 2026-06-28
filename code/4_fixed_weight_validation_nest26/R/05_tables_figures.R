# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — TABLE, AUDIT, MANIFEST, AND FIGURE EXPORTS
# =============================================================================

q1_plot_stability <- function(stability, config) {
  q1_make_dir(file.path(config$output_root, "figures"))
  f <- file.path(config$output_root, "figures", "q1_selection_probability_by_regime.png")
  grDevices::png(f, width = 1800, height = 1100, res = 160)
  op <- par(mar = c(10, 5, 3, 1))
  on.exit({par(op); grDevices::dev.off()}, add = TRUE)
  ord <- order(stability$expanded_gate_pass_mean, decreasing = TRUE)
  labs <- paste(stability$distribution[ord], stability$specialist_regime_id[ord], stability$validation_mode[ord], sep = "-")
  barplot(stability$expanded_gate_pass_mean[ord], names.arg = labs, las = 2,
          ylim = c(0, 1), ylab = "P(GA passes expanded benchmark gate)",
          main = "Fixed-weight validation: selection probability across additional seeds")
  abline(h = c(0.5, 0.8), lty = c(2, 3))
  invisible(f)
}

q1_plot_gain_ci <- function(ci_df, config) {
  q1_make_dir(file.path(config$output_root, "figures"))
  f <- file.path(config$output_root, "figures", "q1_gain_scatter_with_seed_ci.png")
  grDevices::png(f, width = 1600, height = 1200, res = 160)
  on.exit(grDevices::dev.off(), add = TRUE)
  col <- ifelse(ci_df$validation_mode == "locked_unseen_similar", "#C23B22", "#2E6F9E")
  plot(ci_df$mean_gain, ci_df$q95_gain,
       xlab = "Mean MSE relative gain vs expanded benchmark",
       ylab = "q95 MSE relative gain vs expanded benchmark",
       main = "Fixed-weight validation: paired gains across validation seeds",
       pch = 19, col = col)
  abline(h = 0, v = 0, lty = 2)
  legend("topright", legend = c("original", "locked unseen"), pch = 19,
         col = c("#2E6F9E", "#C23B22"), bty = "n")
  invisible(f)
}

q1_plot_survival_heatmap <- function(summary_df, config) {
  q1_make_dir(file.path(config$output_root, "figures"))
  f <- file.path(config$output_root, "figures", "q1_survival_heatmap_by_seed.png")
  key <- paste(summary_df$distribution, summary_df$specialist_regime_id, summary_df$source_seed, summary_df$validation_mode, sep = "-")
  seeds <- sort(unique(summary_df$validation_seed))
  rows <- unique(key)
  mat <- matrix(NA_real_, nrow = length(rows), ncol = length(seeds), dimnames = list(rows, seeds))
  for (i in seq_len(nrow(summary_df))) mat[key[i], as.character(summary_df$validation_seed[i])] <- as.numeric(summary_df$expanded_gate_pass[i])
  grDevices::png(f, width = 1800, height = max(900, 55 * nrow(mat)), res = 160)
  op <- par(mar = c(5, 14, 3, 2))
  on.exit({par(op); grDevices::dev.off()}, add = TRUE)
  image(t(mat[nrow(mat):1, , drop = FALSE]), axes = FALSE, col = c("#D9D9D9", "#1B7837"), zlim = c(0, 1), main = "Fixed-weight validation survival heatmap")
  axis(1, at = seq(0, 1, length.out = length(seeds)), labels = seeds)
  axis(2, at = seq(0, 1, length.out = nrow(mat)), labels = rev(rownames(mat)), las = 2)
  invisible(f)
}

q1_plot_mode_comparison <- function(stability, config) {
  q1_make_dir(file.path(config$output_root, "figures"))
  f <- file.path(config$output_root, "figures", "q1_original_vs_locked_unseen.png")
  wide <- reshape(stability[, c("validation_id", "distribution", "specialist_regime_id", "source_seed", "validation_mode", "expanded_gate_pass_mean")],
                  idvar = c("validation_id", "distribution", "specialist_regime_id", "source_seed"),
                  timevar = "validation_mode", direction = "wide")
  x <- wide$expanded_gate_pass_mean.original_regime
  y <- wide$expanded_gate_pass_mean.locked_unseen_similar
  grDevices::png(f, width = 1400, height = 1200, res = 160)
  on.exit(grDevices::dev.off(), add = TRUE)
  plot(x, y, xlim = c(0, 1), ylim = c(0, 1), pch = 19,
       xlab = "Original-regime survival probability",
       ylab = "Locked-unseen survival probability",
       main = "Fixed-weight validation transfer check: original vs locked unseen")
  abline(0, 1, lty = 2)
  abline(h = 0.8, v = 0.8, lty = 3)
  invisible(f)
}

q1_run_manifest <- function(config, res, selected) {
  pkgs <- c("MASS", "statmod", "modeest", "arrow")
  data.frame(
    field = c("timestamp", "project_root", "discovery_root", "output_root", "workers_configured", "workers_effective",
              "validation_seeds", "target_regimes", "selected_regimes", "monte_carlo_R", "bootstrap_B",
              "validation_modes", "output_formats", "r_version", paste0("pkg_", pkgs, "_available")),
    value = c(q1_now_iso(), config$project_root, config$discovery_root, config$output_root,
              config$workers, q1_worker_count(config), paste(config$validation_seeds, collapse = ";"),
              config$target_regimes, nrow(selected), config$monte_carlo_R, config$bootstrap_B,
              paste(unique(res$tasks$validation_mode), collapse = ";"), paste(getOption("q1.output_formats", "csv"), collapse = ";"),
              paste(R.version$major, R.version$minor, sep = "."),
              vapply(pkgs, function(p) as.character(requireNamespace(p, quietly = TRUE)), character(1))),
    stringsAsFactors = FALSE
  )
}

q1_code_hashes <- function(config) {
  q1_files <- list.files(file.path(dirname(dirname(config$output_root)), "q1_validation"), recursive = TRUE, full.names = TRUE)
  if (!length(q1_files)) q1_files <- list.files("q1_validation", recursive = TRUE, full.names = TRUE)
  original <- file.path(config$project_root, c("00_utils_debug_io.R", "01_paths_repro.R", "02_scenarios_sampling.R",
                                              "03_distributions_params.R", "04_estimators_registry.R", "05_ga_core.R",
                                              "06_data_prep.R", "07_fitness_objectives.R"))
  q1_md5(c(q1_files, original))
}

q1_discovery_input_manifest <- function(config) {
  files <- q1_find_winner_files(config$discovery_root)
  out <- do.call(rbind, lapply(files, function(f) {
    x <- q1_read_csv(f)
    data.frame(path = q1_norm_path(f), rows = nrow(x), cols = ncol(x), md5 = unname(tools::md5sum(f)), stringsAsFactors = FALSE)
  }))
  out
}

q1_benchmark_registry <- function(config) {
  rows <- list(
    data.frame(benchmark = c("mean", "median", "trimmed20", "trimean", "huber", "biweight", "harmonic", "geometric"),
               family = "base_original_admissible", tuning = NA_character_, stringsAsFactors = FALSE),
    data.frame(benchmark = paste0("winsorized_", config$winsor_probs), family = "expanded", tuning = as.character(config$winsor_probs), stringsAsFactors = FALSE),
    data.frame(benchmark = paste0("mom_k", config$mom_blocks), family = "expanded", tuning = as.character(config$mom_blocks), stringsAsFactors = FALSE),
    data.frame(benchmark = paste0("catoni_a", config$catoni_alpha_grid), family = "expanded", tuning = as.character(config$catoni_alpha_grid), stringsAsFactors = FALSE),
    data.frame(benchmark = paste0("huber_k", config$huber_k_grid), family = "expanded", tuning = as.character(config$huber_k_grid), stringsAsFactors = FALSE),
    data.frame(benchmark = "equal_weight_composite", family = "expanded", tuning = NA_character_, stringsAsFactors = FALSE)
  )
  do.call(rbind, rows)
}

q1_locked_unseen_audit <- function(selected, config) {
  out <- lapply(seq_len(nrow(selected)), function(i) {
    row <- selected[i, , drop = FALSE]
    orig <- q1_conditions_for_mode(row, config, "original_regime")
    unseen <- q1_conditions_for_mode(row, config, "locked_unseen_similar")
    data.frame(validation_id = row$validation_id,
               distribution = row$distribution,
               specialist_regime_id = row$specialist_regime_id,
               source_seed = row$source_seed,
               original_rates = paste(unique(orig$contamination_rate), collapse = ";"),
               original_scales = paste(unique(orig$outlier_scale), collapse = ";"),
               original_types = paste(unique(orig$contamination_type), collapse = ";"),
               unseen_rates = paste(unique(unseen$contamination_rate), collapse = ";"),
               unseen_scales = paste(unique(unseen$outlier_scale), collapse = ";"),
               unseen_types = paste(unique(unseen$contamination_type), collapse = ";"),
               unseen_reason = paste(unique(unseen$locked_unseen_reason), collapse = " | "),
               stringsAsFactors = FALSE)
  })
  q1_align_rbind(out)
}

q1_seed_survival_matrix <- function(summary_df) {
  x <- summary_df[, c("validation_id", "distribution", "specialist_regime_id", "source_seed", "validation_mode", "validation_seed", "expanded_gate_pass", "rel_gain_mean_vs_best_mean", "rel_gain_q95_vs_best_q95")]
  x$pass_num <- as.integer(x$expanded_gate_pass)
  reshape(x[, c("validation_id", "distribution", "specialist_regime_id", "source_seed", "validation_mode", "validation_seed", "pass_num")],
          idvar = c("validation_id", "distribution", "specialist_regime_id", "source_seed", "validation_mode"),
          timevar = "validation_seed", direction = "wide")
}

q1_best_benchmark_frequency <- function(summary_df) {
  tab <- as.data.frame(table(summary_df$validation_mode, summary_df$best_mean_benchmark, summary_df$best_q95_benchmark), stringsAsFactors = FALSE)
  names(tab) <- c("validation_mode", "best_mean_benchmark", "best_q95_benchmark", "frequency")
  tab[tab$frequency > 0, , drop = FALSE]
}

q1_final_decision_table <- function(stability) {
  wide <- reshape(stability[, c("validation_id", "distribution", "specialist_regime_id", "source_seed", "validation_class", "validation_mode", "expanded_gate_pass_mean", "ci_confirmed_mean", "mean_gain_mean", "q95_gain_mean")],
                  idvar = c("validation_id", "distribution", "specialist_regime_id", "source_seed", "validation_class"),
                  timevar = "validation_mode", direction = "wide")
  getc <- function(nm) if (nm %in% names(wide)) suppressWarnings(as.numeric(wide[[nm]])) else rep(NA_real_, nrow(wide))
  orig <- getc("expanded_gate_pass_mean.original_regime")
  locked <- getc("expanded_gate_pass_mean.locked_unseen_similar")
  orig_ci <- getc("ci_confirmed_mean.original_regime")
  locked_ci <- getc("ci_confirmed_mean.locked_unseen_similar")
  decision <- ifelse(wide$validation_class == "benchmark_control" & orig < 0.5 & locked < 0.5, "negative_control_success",
                     ifelse(orig >= 0.8 & locked >= 0.8 & orig_ci >= 0.8 & locked_ci >= 0.8, "confirmed_ga_win",
                            ifelse(orig >= 0.8 & (is.na(locked) | locked < 0.8), "local_ga_win",
                                   ifelse(pmax(orig, locked, na.rm = TRUE) >= 0.5, "marginal_ga_win", "benchmark_retained"))))
  wide$final_q1_decision <- decision
  wide
}

q1_negative_control_summary <- function(stability) {
  x <- stability[stability$validation_class == "benchmark_control", , drop = FALSE]
  if (!nrow(x)) return(data.frame())
  aggregate(expanded_gate_pass_mean ~ distribution + validation_mode, data = x,
            FUN = function(z) c(n = length(z), mean = mean(z, na.rm = TRUE), max = max(z, na.rm = TRUE)))
}

q1_local_transfer_summary <- function(stability) {
  final <- q1_final_decision_table(stability)
  as.data.frame(table(final$final_q1_decision), stringsAsFactors = FALSE)
}

q1_export_tables_figures <- function(summary_df, ci_df, stability, config, res = NULL, selected = NULL) {
  q1_make_dir(file.path(config$output_root, "tables"))
  q1_make_dir(file.path(config$output_root, "audit"))
  q1_write_csv(summary_df, file.path(config$output_root, "tables", "q1_table_seed_level_expanded_gate.csv"))
  q1_write_csv(ci_df, file.path(config$output_root, "tables", "q1_table_bootstrap_ci.csv"))
  q1_write_csv(stability, file.path(config$output_root, "tables", "q1_table_regime_stability.csv"))

  if (!is.null(res)) {
    q1_write_csv(res$estimator_metrics, file.path(config$output_root, "tables", "q1_estimator_level_metrics.csv"))
    q1_write_csv(res$runtime, file.path(config$output_root, "audit", "q1_runtime_by_task.csv"))
    q1_write_csv(q1_seed_survival_matrix(summary_df), file.path(config$output_root, "tables", "q1_seed_survival_matrix.csv"))
    q1_write_csv(q1_best_benchmark_frequency(summary_df), file.path(config$output_root, "tables", "q1_best_benchmark_frequency.csv"))
  }
  if (!is.null(selected)) {
    q1_write_csv(selected, file.path(config$output_root, "audit", "q1_selected_regimes_audit.csv"))
    q1_write_csv(q1_locked_unseen_audit(selected, config), file.path(config$output_root, "audit", "q1_locked_unseen_construction.csv"))
  }

  q1_write_csv(q1_final_decision_table(stability), file.path(config$output_root, "tables", "q1_final_decision_table.csv"))
  q1_write_csv(q1_negative_control_summary(stability), file.path(config$output_root, "tables", "q1_negative_control_summary.csv"))
  q1_write_csv(q1_local_transfer_summary(stability), file.path(config$output_root, "tables", "q1_local_vs_transfer_summary.csv"))
  q1_write_csv(q1_benchmark_registry(config), file.path(config$output_root, "audit", "q1_benchmark_registry.csv"))
  q1_write_csv(q1_discovery_input_manifest(config), file.path(config$output_root, "audit", "q1_discovery_input_manifest.csv"))
  q1_write_csv(q1_code_hashes(config), file.path(config$output_root, "audit", "q1_code_hashes.csv"))
  if (!is.null(res) && !is.null(selected)) q1_write_csv(q1_run_manifest(config, res, selected), file.path(config$output_root, "audit", "q1_run_manifest.csv"))
  capture.output(str(config), file = file.path(config$output_root, "audit", "q1_config_snapshot.txt"))

  figs <- c(q1_plot_stability(stability, config),
            q1_plot_gain_ci(ci_df, config),
            q1_plot_survival_heatmap(summary_df, config),
            q1_plot_mode_comparison(stability, config))
  invisible(figs)
}
