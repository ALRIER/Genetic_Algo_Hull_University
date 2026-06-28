# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — TABLE AND FIGURE EXPORTS
# =============================================================================

q1_plot_stability <- function(stability, config) {
  q1_make_dir(file.path(config$output_root, "figures"))
  f <- file.path(config$output_root, "figures", "q1_selection_probability_by_regime.png")
  grDevices::png(f, width = 1800, height = 1100, res = 160)
  op <- par(mar = c(10, 5, 3, 1))
  on.exit({par(op); grDevices::dev.off()}, add = TRUE)
  ord <- order(stability$expanded_gate_pass_mean, decreasing = TRUE)
  labs <- paste(stability$distribution[ord], stability$specialist_regime_id[ord], sep = "-")
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
  plot(ci_df$mean_gain, ci_df$q95_gain,
       xlab = "Mean MSE relative gain vs expanded benchmark",
       ylab = "q95 MSE relative gain vs expanded benchmark",
       main = "Fixed-weight validation: paired gains across validation seeds",
       pch = 19)
  abline(h = 0, v = 0, lty = 2)
  invisible(f)
}

q1_export_tables_figures <- function(summary_df, ci_df, stability, config) {
  q1_write_csv(summary_df, file.path(config$output_root, "tables", "q1_table_seed_level_expanded_gate.csv"))
  q1_write_csv(ci_df, file.path(config$output_root, "tables", "q1_table_bootstrap_ci.csv"))
  q1_write_csv(stability, file.path(config$output_root, "tables", "q1_table_regime_stability.csv"))
  figs <- c(q1_plot_stability(stability, config), q1_plot_gain_ci(ci_df, config))
  invisible(figs)
}
