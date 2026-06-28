# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — CONFIRMATORY EVALUATION
# =============================================================================

q1_draw_clean_sample <- function(distribution, n, param_grid) {
  i <- sample.int(nrow(param_grid), size = 1L)
  generate_population(distribution, n, as.list(param_grid[i, , drop = FALSE]))
}

q1_inject_from_summary <- function(x, rate, scale, type) {
  out <- tryCatch({
    inject_outliers_realistic(x,
                              contamination_rate = rate,
                              outlier_scale_mad = scale,
                              type = type)
  }, error = function(e) {
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
                clustered_upper = abs(stats::rnorm(m)),
                clustered_symmetric = stats::rt(m, df = 3),
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

q1_true_mean_for_family <- function(distribution, param_grid) {
  mean(vapply(seq_len(nrow(param_grid)), function(i) {
    analytic_mean_from_params(distribution, as.list(param_grid[i, , drop = FALSE]))
  }, numeric(1)), na.rm = TRUE)
}

q1_evaluate_one_regime_seed <- function(row, seed, config, task_id = NA_character_) {
  set.seed(as.integer(seed))
  distribution <- tolower(as.character(row$distribution[1]))
  param_grid <- param_grids[[distribution]]
  if (is.null(param_grid)) stop("No param_grid for distribution: ", distribution, call. = FALSE)

  w <- q1_parse_semicolon_values(row$ga_weight_vector, numeric = TRUE)
  if (length(w) != N_EST) w <- q1_weight_vector_from_row(row)
  w <- w / sum(w)

  validation_mode <- if ("validation_mode" %in% names(row)) as.character(row$validation_mode[1]) else "original_regime"
  conditions <- q1_conditions_for_mode(row, config, validation_mode = validation_mode)
  if (!nrow(conditions)) stop("No conditions expanded for regime.", call. = FALSE)

  true_mu <- q1_true_mean_for_family(distribution, param_grid)
  records <- vector("list", nrow(conditions) * config$monte_carlo_R)
  rr <- 0L
  q1_append_status(config, task_id, "running", paste0("mode=", validation_mode, "; conditions=", nrow(conditions), "; R=", config$monte_carlo_R))

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

      ga_est <- custom_estimator(x, w, distribution = distribution, target = "arithmetic_mean")
      bench <- q1_expanded_benchmarks(x, config, distribution = distribution)
      ests <- c(ga_composite = ga_est, bench)
      sqerr <- (ests - true_mu)^2

      rr <- rr + 1L
      records[[rr]] <- data.frame(
        validation_id = row$validation_id[1],
        distribution = distribution,
        specialist_regime_id = row$specialist_regime_id[1],
        source_seed = row$source_seed[1],
        validation_seed = seed,
        validation_class = row$validation_class[1],
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

      hb <- as.integer(config$heartbeat_every_replicates)
      if (is.finite(hb) && hb > 0L && (r %% hb == 0L || r == config$monte_carlo_R)) {
        q1_append_status(config, task_id, "heartbeat", paste0("condition=", cc, "/", nrow(conditions), "; replicate=", r, "/", config$monte_carlo_R))
      }
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

q1_estimator_metrics <- function(long_df, config) {
  split_est <- split(long_df, long_df$estimator)
  do.call(rbind, lapply(names(split_est), function(est) {
    z <- split_est[[est]]
    se <- z$squared_error
    data.frame(
      validation_id = z$validation_id[1],
      distribution = z$distribution[1],
      specialist_regime_id = z$specialist_regime_id[1],
      source_seed = z$source_seed[1],
      validation_seed = z$validation_seed[1],
      validation_class = z$validation_class[1],
      validation_mode = z$validation_mode[1],
      estimator = est,
      n_rows = length(se),
      mean_mse = mean(se, na.rm = TRUE),
      median_mse = stats::median(se, na.rm = TRUE),
      q75_mse = as.numeric(stats::quantile(se, 0.75, na.rm = TRUE, type = 8)),
      q95_mse = as.numeric(stats::quantile(se, config$q, na.rm = TRUE, type = 8)),
      q99_mse = as.numeric(stats::quantile(se, 0.99, na.rm = TRUE, type = 8)),
      max_mse = max(se, na.rm = TRUE),
      mean_estimate = mean(z$estimate, na.rm = TRUE),
      bias = mean(z$estimate - z$true_mean, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
}

q1_task_id_from_task <- function(task) {
  sprintf("T%04d_R%03d_S%s_%s",
          as.integer(task$task_index[1]),
          as.integer(task$row_index[1]),
          as.character(task$validation_seed[1]),
          as.character(task$validation_mode[1]))
}

q1_write_task_artifacts <- function(out, config) {
  if (!isTRUE(config$export_task_outputs)) return(invisible(FALSE))
  task_dir <- q1_make_dir(file.path(config$output_root, "tasks", out$task_id))
  q1_write_csv(out$summary, file.path(task_dir, "summary.csv"))
  q1_write_csv(out$estimator_metrics, file.path(task_dir, "estimator_metrics.csv"))
  q1_write_csv(out$runtime, file.path(task_dir, "runtime.csv"))
  if (isTRUE(config$export_long_squared_errors)) q1_write_csv(out$long, file.path(task_dir, "long_squared_errors.csv"))
  invisible(TRUE)
}

q1_run_single_task <- function(i, selected, tasks, config) {
  task <- tasks[i, , drop = FALSE]
  task_id <- as.character(task$task_id[1])
  checkpoint <- q1_task_checkpoint_path(config, task_id)
  if (isTRUE(config$enable_recovery) && !isTRUE(config$overwrite_completed_tasks)) {
    old <- q1_read_rds_if_valid(checkpoint)
    if (is.list(old) && identical(old$status, "completed")) {
      q1_append_status(config, task_id, "skipped_checkpoint", "valid completed checkpoint found")
      return(old)
    }
  }

  q1_append_status(config, task_id, "started", "task evaluation started")
  start_time <- Sys.time()
  row <- selected[task$row_index, , drop = FALSE]
  row$validation_mode <- task$validation_mode

  out <- tryCatch({
    long <- q1_evaluate_one_regime_seed(row, task$validation_seed, config, task_id = task_id)
    summary <- q1_summarise_regime_seed(long, config)
    estimator_metrics <- q1_estimator_metrics(long, config)
    end_time <- Sys.time()
    runtime <- data.frame(
      task_id = task_id,
      row_index = task$row_index,
      validation_seed = task$validation_seed,
      validation_mode = task$validation_mode,
      status = "completed",
      started_at = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
      ended_at = format(end_time, "%Y-%m-%dT%H:%M:%S%z"),
      elapsed_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_long_rows = nrow(long),
      stringsAsFactors = FALSE
    )
    list(status = "completed", task_id = task_id, task = task, long = long,
         summary = summary, estimator_metrics = estimator_metrics, runtime = runtime)
  }, error = function(e) {
    end_time <- Sys.time()
    runtime <- data.frame(
      task_id = task_id,
      row_index = task$row_index,
      validation_seed = task$validation_seed,
      validation_mode = task$validation_mode,
      status = "failed",
      started_at = format(start_time, "%Y-%m-%dT%H:%M:%S%z"),
      ended_at = format(end_time, "%Y-%m-%dT%H:%M:%S%z"),
      elapsed_seconds = as.numeric(difftime(end_time, start_time, units = "secs")),
      n_long_rows = 0L,
      error = conditionMessage(e),
      stringsAsFactors = FALSE
    )
    list(status = "failed", task_id = task_id, task = task, runtime = runtime, error = conditionMessage(e))
  })

  q1_write_rds_atomic(out, checkpoint)
  if (identical(out$status, "completed")) {
    q1_write_task_artifacts(out, config)
    q1_append_status(config, task_id, "completed", "checkpoint written")
  } else {
    q1_append_status(config, task_id, "failed", out$error)
    stop(out$error, call. = FALSE)
  }
  out
}

q1_run_validation <- function(selected, config) {
  q1_make_dir(config$output_root)
  q1_make_dir(file.path(config$output_root, "logs"))
  q1_make_dir(file.path(config$output_root, "tasks"))
  q1_write_csv(selected, file.path(config$output_root, "q1_selected_regimes.csv"))
  q1_write_rds_atomic(selected, q1_stage_checkpoint_path(config, "selected_regimes"))

  validation_modes <- character(0)
  if (isTRUE(config$run_original_regime_validation)) validation_modes <- c(validation_modes, "original_regime")
  if (isTRUE(config$run_locked_unseen_validation)) validation_modes <- c(validation_modes, "locked_unseen_similar")
  if (!length(validation_modes)) stop("At least one validation mode must be enabled.", call. = FALSE)

  tasks <- expand.grid(row_index = seq_len(nrow(selected)),
                       validation_seed = config$validation_seeds,
                       validation_mode = validation_modes,
                       KEEP.OUT.ATTRS = FALSE,
                       stringsAsFactors = FALSE)
  tasks$task_index <- seq_len(nrow(tasks))
  tasks$task_id <- vapply(seq_len(nrow(tasks)), function(i) q1_task_id_from_task(tasks[i, , drop = FALSE]), character(1))
  q1_write_csv(tasks, file.path(config$output_root, "q1_validation_tasks.csv"))
  q1_write_rds_atomic(tasks, q1_stage_checkpoint_path(config, "validation_tasks"))

  completed <- vapply(tasks$task_id, function(tid) {
    old <- q1_read_rds_if_valid(q1_task_checkpoint_path(config, tid))
    is.list(old) && identical(old$status, "completed")
  }, logical(1))
  pending_idx <- which(!completed | isTRUE(config$overwrite_completed_tasks) | !isTRUE(config$enable_recovery))

  q1_msg("Running ", length(pending_idx), " pending validation tasks out of ", nrow(tasks), ".")
  q1_log_event(config, "run_validation_start", paste0("pending=", length(pending_idx), "; total=", nrow(tasks)))

  if (length(pending_idx)) {
    cl <- q1_start_cluster(config)
    on.exit(q1_stop_cluster(cl), add = TRUE)
    parallel::clusterExport(cl, varlist = ls(envir = .GlobalEnv, all.names = TRUE), envir = .GlobalEnv)
    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(MASS)
        library(statmod)
        library(modeest)
      })
      NULL
    })
    worker_fun <- function(i) q1_run_single_task(i, selected, tasks, config)
    batch_size <- max(1L, q1_worker_count(config) * 2L)
    batches <- split(pending_idx, ceiling(seq_along(pending_idx) / batch_size))
    for (bb in seq_along(batches)) {
      q1_console_progress(config, paste0("validation batch ", bb, "/", length(batches), " start"), total_tasks = nrow(tasks))
      parallel::parLapplyLB(cl, batches[[bb]], worker_fun)
      q1_console_progress(config, paste0("validation batch ", bb, "/", length(batches), " done"), total_tasks = nrow(tasks))
    }
  } else {
    q1_console_progress(config, "validation recovery", total_tasks = nrow(tasks))
  }

  q1_msg("Consolidating completed task checkpoints without loading long outputs.")
  paths <- vapply(tasks$task_id, function(tid) q1_task_checkpoint_path(config, tid), character(1))
  summary_parts <- vector("list", length(paths))
  estimator_parts <- vector("list", length(paths))
  runtime_parts <- vector("list", length(paths))
  failed <- character(0)
  for (i in seq_along(paths)) {
    one <- q1_read_rds_if_valid(paths[i])
    if (!is.list(one) || !identical(one$status, "completed")) {
      failed <- c(failed, tasks$task_id[i])
      next
    }
    summary_parts[[i]] <- one$summary
    estimator_parts[[i]] <- one$estimator_metrics
    runtime_parts[[i]] <- one$runtime
    rm(one)
    if (i %% 25L == 0L || i == length(paths)) {
      q1_msg("Consolidation progress: ", i, "/", length(paths), " checkpoints scanned.")
      gc(FALSE)
    }
  }
  if (length(failed)) {
    stop("Missing or failed task checkpoints: ", paste(failed, collapse = ", "), call. = FALSE)
  }

  summary_all <- q1_align_rbind(summary_parts)
  estimator_all <- q1_align_rbind(estimator_parts)
  runtime_all <- q1_align_rbind(runtime_parts)

  q1_write_csv(summary_all, file.path(config$output_root, "q1_validation_summary_by_seed.csv"))
  q1_write_csv(estimator_all, file.path(config$output_root, "q1_estimator_level_metrics.csv"))
  q1_write_csv(runtime_all, file.path(config$output_root, "q1_runtime_by_task.csv"))
  q1_write_rds_atomic(list(summary = summary_all, estimator_metrics = estimator_all, runtime = runtime_all, tasks = tasks), q1_stage_checkpoint_path(config, "validation_complete"))
  q1_log_event(config, "run_validation_completed", paste0("tasks=", nrow(tasks), "; long_outputs=per_task_only"))
  invisible(list(long = NULL, summary = summary_all, estimator_metrics = estimator_all, runtime = runtime_all, tasks = tasks))
}
