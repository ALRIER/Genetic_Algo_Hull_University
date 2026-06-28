# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — CONFIRMATORY EVALUATION
# =============================================================================

q1_draw_clean_sample <- function(distribution, n, param_grid) {
  # Preserve the original super-population idea by drawing parameter rows with
  # equal probability and then sampling from that row's family distribution.
  i <- sample.int(nrow(param_grid), size = 1L)
  generate_population(distribution, n, as.list(param_grid[i, , drop = FALSE]))
}

q1_inject_from_summary <- function(x, rate, scale, type) {
  # Reuses the original contamination injector when available. The original code
  # expects contamination mechanism names used by the scenario universe.
  out <- tryCatch({
    inject_outliers_realistic(x,
                              contamination_rate = rate,
                              outlier_scale_mad = scale,
                              type = type)
  }, error = function(e) {
    # Conservative fallback for unexpected naming mismatch.
    med <- stats::median(x, na.rm = TRUE)
    mad0 <- stats::mad(x, constant = 1, na.rm = TRUE)
    if (!is.finite(mad0) || mad0 <= 0) mad0 <- stats::sd(x, na.rm = TRUE)
    if (!is.finite(mad0) || mad0 <= 0) return(x)
    m <- max(0L, floor(length(x) * rate))
    if (m == 0L) return(x)
    idx <- sample.int(length(x), m)
    z <- switch(type,
                upper_tail = abs(stats::rnorm(m)),
                lower_tail = -abs(stats::rnorm(m)),
                symmetric_t = stats::rt(m, df = 3),
                point_mass = rep(1, m),
                mixture_bimodal_near = sample(c(-1, 1), m, replace = TRUE),
                mixture_bimodal_far = sample(c(-1, 1), m, replace = TRUE),
                stats::rnorm(m))
    x[idx] <- med + scale * mad0 * z
    x
  })
  as.numeric(out)
}

q1_expand_regime_conditions <- function(row, config) {
  rates <- q1_parse_semicolon_values(row$exact_contamination_rates, numeric = TRUE)
  scales <- q1_parse_semicolon_values(row$exact_outlier_scales, numeric = TRUE)
  types <- q1_parse_semicolon_values(row$exact_contamination_types, numeric = FALSE)
  ns <- q1_parse_semicolon_values(row$exact_sample_sizes, numeric = TRUE)
  if (!length(rates)) rates <- 0
  if (!length(scales)) scales <- 0
  if (!length(types)) types <- "none"
  if (!length(ns)) ns <- config$sample_sizes_default
  expand.grid(contamination_rate = rates,
              outlier_scale = scales,
              contamination_type = types,
              sample_size = as.integer(ns),
              KEEP.OUT.ATTRS = FALSE,
              stringsAsFactors = FALSE)
}

q1_evaluate_one_regime_seed <- function(row, seed, config) {
  set.seed(as.integer(seed))
  distribution <- tolower(as.character(row$distribution))
  param_grid <- param_grids[[distribution]]
  if (is.null(param_grid)) stop("No param_grid for distribution: ", distribution, call. = FALSE)

  w <- q1_parse_semicolon_values(row$ga_weight_vector, numeric = TRUE)
  if (length(w) != N_EST) w <- q1_weight_vector_from_row(row)
  w <- w / sum(w)

  validation_mode <- if ("validation_mode" %in% names(row)) as.character(row$validation_mode[1]) else "original_regime"
  conditions <- q1_conditions_for_mode(row, config, validation_mode = validation_mode)
  if (!nrow(conditions)) stop("No conditions expanded for regime.", call. = FALSE)

  records <- vector("list", nrow(conditions) * config$monte_carlo_R)
  rr <- 0L

  for (cc in seq_len(nrow(conditions))) {
    cond <- conditions[cc, , drop = FALSE]
    n <- as.integer(cond$sample_size)
    rate <- as.numeric(cond$contamination_rate)
    scale <- as.numeric(cond$outlier_scale)
    type <- as.character(cond$contamination_type)

    for (r in seq_len(config$monte_carlo_R)) {
      x <- q1_draw_clean_sample(distribution, n, param_grid)
      if (rate > 0 && !identical(type, "none")) {
        x <- q1_inject_from_summary(x, rate = rate, scale = scale, type = type)
      }

      # Target is the clean-family parameter-grid super-population mean proxy:
      # equal-weight average of analytic means across grid rows, matching the
      # original pooled grid logic.
      true_mu <- mean(vapply(seq_len(nrow(param_grid)), function(i) {
        analytic_mean_from_params(distribution, as.list(param_grid[i, , drop = FALSE]))
      }, numeric(1)), na.rm = TRUE)

      ga_est <- custom_estimator(x, w, distribution = distribution, target = "arithmetic_mean")
      bench <- q1_expanded_benchmarks(x, config, distribution = distribution)

      ests <- c(ga_composite = ga_est, bench)
      sqerr <- (ests - true_mu)^2
      rr <- rr + 1L
      records[[rr]] <- data.frame(
        validation_id = row$validation_id,
        distribution = distribution,
        specialist_regime_id = row$specialist_regime_id,
        source_seed = row$source_seed,
        validation_seed = seed,
        validation_class = row$validation_class,
        validation_mode = validation_mode,
        condition_id = cc,
        replicate = r,
        sample_size = n,
        contamination_rate = rate,
        outlier_scale = scale,
        contamination_type = type,
        locked_unseen_reason = if ("locked_unseen_reason" %in% names(cond)) as.character(cond$locked_unseen_reason[1]) else NA_character_,
        estimator = names(sqerr),
        estimate = as.numeric(ests),
        true_mean = true_mu,
        squared_error = as.numeric(sqerr),
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, records[seq_len(rr)])
}

q1_summarise_regime_seed <- function(long_df, config) {
  split_est <- split(long_df, long_df$estimator)
  stats <- do.call(rbind, lapply(names(split_est), function(est) {
    z <- split_est[[est]]$squared_error
    data.frame(estimator = est,
               mean_mse = mean(z, na.rm = TRUE),
               q95_mse = as.numeric(stats::quantile(z, probs = config$q, na.rm = TRUE, type = 8)),
               max_mse = max(z, na.rm = TRUE),
               stringsAsFactors = FALSE)
  }))
  ga <- stats[stats$estimator == "ga_composite", , drop = FALSE]
  benches <- stats[stats$estimator != "ga_composite", , drop = FALSE]
  best_mean <- benches[which.min(benches$mean_mse), , drop = FALSE]
  best_q95 <- benches[which.min(benches$q95_mse), , drop = FALSE]
  best_joint <- benches[which.min(0.35 * benches$mean_mse / min(benches$mean_mse) +
                                    0.65 * benches$q95_mse / min(benches$q95_mse)), , drop = FALSE]
  data.frame(
    validation_id = long_df$validation_id[1],
    distribution = long_df$distribution[1],
    specialist_regime_id = long_df$specialist_regime_id[1],
    source_seed = long_df$source_seed[1],
    validation_seed = long_df$validation_seed[1],
    validation_class = long_df$validation_class[1],
    validation_mode = long_df$validation_mode[1],
    ga_mean_mse = ga$mean_mse,
    ga_q95_mse = ga$q95_mse,
    best_mean_benchmark = best_mean$estimator,
    best_mean_benchmark_mse = best_mean$mean_mse,
    best_q95_benchmark = best_q95$estimator,
    best_q95_benchmark_mse = best_q95$q95_mse,
    best_joint_benchmark = best_joint$estimator,
    best_joint_mean_mse = best_joint$mean_mse,
    best_joint_q95_mse = best_joint$q95_mse,
    rel_gain_mean_vs_best_mean = (best_mean$mean_mse - ga$mean_mse) / best_mean$mean_mse,
    rel_gain_q95_vs_best_q95 = (best_q95$q95_mse - ga$q95_mse) / best_q95$q95_mse,
    expanded_gate_pass = (ga$mean_mse < best_mean$mean_mse) && (ga$q95_mse < best_q95$q95_mse),
    stringsAsFactors = FALSE
  )
}

q1_run_validation <- function(selected, config) {
  q1_make_dir(config$output_root)
  q1_write_csv(selected, file.path(config$output_root, "q1_selected_regimes.csv"))

  validation_modes <- character(0)
  if (isTRUE(config$run_original_regime_validation)) validation_modes <- c(validation_modes, "original_regime")
  if (isTRUE(config$run_locked_unseen_validation)) validation_modes <- c(validation_modes, "locked_unseen_similar")
  if (!length(validation_modes)) stop("At least one validation mode must be enabled.", call. = FALSE)

  tasks <- expand.grid(row_index = seq_len(nrow(selected)),
                       validation_seed = config$validation_seeds,
                       validation_mode = validation_modes,
                       KEEP.OUT.ATTRS = FALSE,
                       stringsAsFactors = FALSE)
  q1_write_csv(tasks, file.path(config$output_root, "q1_validation_tasks.csv"))

  cl <- q1_start_cluster(config)
  on.exit(q1_stop_cluster(cl), add = TRUE)

  parallel::clusterExport(cl, varlist = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(MASS)
      library(statmod)
      library(modeest)
    })
    NULL
  })

  worker_fun <- function(i) {
    task <- tasks[i, , drop = FALSE]
    row <- selected[task$row_index, , drop = FALSE]
    row$validation_mode <- task$validation_mode
    long <- q1_evaluate_one_regime_seed(row, task$validation_seed, config)
    summary <- q1_summarise_regime_seed(long, config)
    list(long = long, summary = summary)
  }

  q1_msg("Running ", nrow(tasks), " validation tasks: ", nrow(selected),
         " regimes × ", length(config$validation_seeds), " seeds × ",
         length(unique(tasks$validation_mode)), " validation modes.")
  res <- parallel::parLapplyLB(cl, seq_len(nrow(tasks)), worker_fun)
  long_all <- do.call(rbind, lapply(res, `[[`, "long"))
  summary_all <- do.call(rbind, lapply(res, `[[`, "summary"))

  q1_write_csv(summary_all, file.path(config$output_root, "q1_validation_summary_by_seed.csv"))
  q1_write_csv(long_all, file.path(config$output_root, "q1_validation_long_squared_errors.csv"))
  invisible(list(long = long_all, summary = summary_all))
}
