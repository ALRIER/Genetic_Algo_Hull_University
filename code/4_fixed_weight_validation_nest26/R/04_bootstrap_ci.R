# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — PAIRED BOOTSTRAP CONFIDENCE INTERVALS
# =============================================================================

q1_bootstrap_one <- function(df, config) {
  ga <- df[df$estimator == "ga_composite", , drop = FALSE]
  if (!nrow(ga)) stop("No GA rows in bootstrap input.", call. = FALSE)

  best_est <- df[df$estimator != "ga_composite", , drop = FALSE]
  by_est <- split(best_est, best_est$estimator)
  bench_mean <- names(which.min(vapply(by_est, function(z) mean(z$squared_error), numeric(1))))
  bench_q95 <- names(which.min(vapply(by_est, function(z) as.numeric(stats::quantile(z$squared_error, config$q, type = 8)), numeric(1))))

  bmean <- df[df$estimator == bench_mean, , drop = FALSE]
  bq95 <- df[df$estimator == bench_q95, , drop = FALSE]
  ga$key <- paste(ga$condition_id, ga$replicate, sep = "__")
  bmean$key <- paste(bmean$condition_id, bmean$replicate, sep = "__")
  bq95$key <- paste(bq95$condition_id, bq95$replicate, sep = "__")
  common <- Reduce(intersect, list(ga$key, bmean$key, bq95$key))
  ga <- ga[match(common, ga$key), ]
  bmean <- bmean[match(common, bmean$key), ]
  bq95 <- bq95[match(common, bq95$key), ]
  n <- length(common)
  if (!n) stop("No paired rows available for bootstrap.", call. = FALSE)

  point_mean_gain <- (mean(bmean$squared_error) - mean(ga$squared_error)) / mean(bmean$squared_error)
  point_q95_gain <- (as.numeric(stats::quantile(bq95$squared_error, config$q, type = 8)) -
                       as.numeric(stats::quantile(ga$squared_error, config$q, type = 8))) /
    as.numeric(stats::quantile(bq95$squared_error, config$q, type = 8))

  set.seed(as.integer(df$validation_seed[1]) + 100000L)
  boot <- replicate(config$bootstrap_B, {
    ii <- sample.int(n, n, replace = TRUE)
    c(
      mean_gain = (mean(bmean$squared_error[ii]) - mean(ga$squared_error[ii])) / mean(bmean$squared_error[ii]),
      q95_gain = (as.numeric(stats::quantile(bq95$squared_error[ii], config$q, type = 8)) -
                    as.numeric(stats::quantile(ga$squared_error[ii], config$q, type = 8))) /
        as.numeric(stats::quantile(bq95$squared_error[ii], config$q, type = 8))
    )
  })
  alpha <- (1 - config$ci_level) / 2
  data.frame(
    validation_id = df$validation_id[1],
    distribution = df$distribution[1],
    specialist_regime_id = df$specialist_regime_id[1],
    source_seed = df$source_seed[1],
    validation_seed = df$validation_seed[1],
    validation_class = df$validation_class[1],
    validation_mode = if ("validation_mode" %in% names(df)) df$validation_mode[1] else "original_regime",
    bootstrap_B = config$bootstrap_B,
    paired_n = n,
    best_mean_benchmark_ci = bench_mean,
    best_q95_benchmark_ci = bench_q95,
    mean_gain = point_mean_gain,
    mean_gain_ci_low = as.numeric(stats::quantile(boot["mean_gain", ], alpha, na.rm = TRUE, type = 8)),
    mean_gain_ci_high = as.numeric(stats::quantile(boot["mean_gain", ], 1 - alpha, na.rm = TRUE, type = 8)),
    mean_gain_boot_median = as.numeric(stats::median(boot["mean_gain", ], na.rm = TRUE)),
    q95_gain = point_q95_gain,
    q95_gain_ci_low = as.numeric(stats::quantile(boot["q95_gain", ], alpha, na.rm = TRUE, type = 8)),
    q95_gain_ci_high = as.numeric(stats::quantile(boot["q95_gain", ], 1 - alpha, na.rm = TRUE, type = 8)),
    q95_gain_boot_median = as.numeric(stats::median(boot["q95_gain", ], na.rm = TRUE)),
    ci_confirmed = point_mean_gain > 0 && point_q95_gain > 0 &&
      (!isTRUE(config$require_positive_ci) ||
         (as.numeric(stats::quantile(boot["mean_gain", ], alpha, na.rm = TRUE, type = 8)) > 0 &&
            as.numeric(stats::quantile(boot["q95_gain", ], alpha, na.rm = TRUE, type = 8)) > 0)),
    stringsAsFactors = FALSE
  )
}

q1_bootstrap_all <- function(long_df, config) {
  groups <- split(long_df, paste(long_df$validation_id, long_df$validation_seed, long_df$validation_mode, sep = "__"))
  out <- vector("list", length(groups))
  names(out) <- names(groups)
  for (nm in names(groups)) {
    checkpoint <- q1_stage_checkpoint_path(config, paste0("bootstrap_", gsub("[^A-Za-z0-9_]+", "_", nm)))
    old <- if (isTRUE(config$enable_recovery) && !isTRUE(config$overwrite_completed_tasks)) q1_read_rds_if_valid(checkpoint) else NULL
    if (is.data.frame(old)) {
      out[[nm]] <- old
      q1_append_status(config, nm, "bootstrap_skipped_checkpoint", "valid bootstrap checkpoint found")
      next
    }
    q1_append_status(config, nm, "bootstrap_started", "paired bootstrap started")
    one <- q1_bootstrap_one(groups[[nm]], config)
    q1_write_rds_atomic(one, checkpoint)
    out[[nm]] <- one
    q1_append_status(config, nm, "bootstrap_completed", "bootstrap checkpoint written")
  }
  out <- q1_align_rbind(out)
  q1_write_csv(out, file.path(config$output_root, "q1_bootstrap_ci_by_seed.csv"))
  q1_write_csv(out[, c("validation_id", "distribution", "specialist_regime_id", "source_seed", "validation_seed",
                       "validation_mode", "bootstrap_B", "paired_n", "mean_gain_boot_median", "q95_gain_boot_median",
                       "mean_gain_ci_low", "mean_gain_ci_high", "q95_gain_ci_low", "q95_gain_ci_high")],
               file.path(config$output_root, "q1_bootstrap_distribution_summary.csv"))
  out
}

q1_stability_summary <- function(summary_df, ci_df, config) {
  merged <- merge(summary_df, ci_df,
                  by = c("validation_id", "distribution", "specialist_regime_id", "source_seed",
                         "validation_seed", "validation_class", "validation_mode"),
                  all.x = TRUE)
  keys <- c("validation_id", "distribution", "specialist_regime_id", "source_seed", "validation_class", "validation_mode")
  groups <- split(merged, interaction(merged[keys], drop = TRUE, lex.order = TRUE))
  out <- q1_align_rbind(lapply(groups, function(z) {
    metric <- function(v, fun) fun(as.numeric(v), na.rm = TRUE)
    data.frame(
      validation_id = z$validation_id[1],
      distribution = z$distribution[1],
      specialist_regime_id = z$specialist_regime_id[1],
      source_seed = z$source_seed[1],
      validation_class = z$validation_class[1],
      validation_mode = z$validation_mode[1],
      expanded_gate_pass_mean = metric(z$expanded_gate_pass, mean),
      expanded_gate_pass_min = metric(z$expanded_gate_pass, min),
      expanded_gate_pass_max = metric(z$expanded_gate_pass, max),
      ci_confirmed_mean = metric(z$ci_confirmed, mean),
      ci_confirmed_min = metric(z$ci_confirmed, min),
      ci_confirmed_max = metric(z$ci_confirmed, max),
      rel_gain_mean_vs_best_mean_mean = metric(z$rel_gain_mean_vs_best_mean, mean),
      rel_gain_q95_vs_best_q95_mean = metric(z$rel_gain_q95_vs_best_q95, mean),
      mean_gain_mean = metric(z$mean_gain, mean),
      q95_gain_mean = metric(z$q95_gain, mean),
      validation_seeds_n = length(unique(z$validation_seed)),
      stringsAsFactors = FALSE
    )
  }))
  out$confirmed_label <- ifelse(out$expanded_gate_pass_mean >= 0.80 & out$ci_confirmed_mean >= 0.80, "confirmed_stable_ga_win",
                                ifelse(out$expanded_gate_pass_mean >= 0.50, "marginal_or_seed_sensitive",
                                       "expanded_benchmark_retained"))
  q1_write_csv(out, file.path(config$output_root, "q1_stability_summary.csv"))
  out
}

q1_bootstrap_all_from_tasks <- function(tasks, config) {
  out <- vector("list", nrow(tasks))
  q1_msg("Bootstrap streaming from task checkpoints: ", nrow(tasks), " groups.")
  for (i in seq_len(nrow(tasks))) {
    if (i == 1L || i %% 10L == 0L) q1_console_progress(config, paste0("bootstrap progress ", i, "/", nrow(tasks)), total_tasks = nrow(tasks))
    task <- tasks[i, , drop = FALSE]
    task_id <- as.character(task$task_id[1])
    task_checkpoint <- q1_task_checkpoint_path(config, task_id)
    task_result <- q1_read_rds_if_valid(task_checkpoint)
    if (!is.list(task_result) || !identical(task_result$status, "completed")) {
      stop("Missing completed task checkpoint for bootstrap: ", task_id, call. = FALSE)
    }
    group_id <- paste(task_result$summary$validation_id[1], task_result$summary$validation_seed[1], task_result$summary$validation_mode[1], sep = "__")
    checkpoint <- q1_stage_checkpoint_path(config, paste0("bootstrap_", gsub("[^A-Za-z0-9_]+", "_", group_id)))
    old <- if (isTRUE(config$enable_recovery) && !isTRUE(config$overwrite_completed_tasks)) q1_read_rds_if_valid(checkpoint) else NULL
    if (is.data.frame(old)) {
      out[[i]] <- old
      q1_append_status(config, group_id, "bootstrap_skipped_checkpoint", "valid bootstrap checkpoint found")
      next
    }
    q1_append_status(config, group_id, "bootstrap_started", paste0("task_id=", task_id))
    one <- q1_bootstrap_one(task_result$long, config)
    q1_write_rds_atomic(one, checkpoint)
    out[[i]] <- one
    q1_append_status(config, group_id, "bootstrap_completed", paste0("task_id=", task_id))
    rm(task_result)
    if (i %% 25L == 0L || i == nrow(tasks)) {
      q1_msg("Bootstrap progress: ", i, "/", nrow(tasks), " groups processed.")
      gc(FALSE)
    }
  }
  q1_msg("Bootstrap aggregation: combining small CI rows.")
  out <- q1_align_rbind(out)
  q1_write_csv(out, file.path(config$output_root, "q1_bootstrap_ci_by_seed.csv"))
  q1_write_csv(out[, c("validation_id", "distribution", "specialist_regime_id", "source_seed", "validation_seed",
                       "validation_mode", "bootstrap_B", "paired_n", "mean_gain_boot_median", "q95_gain_boot_median",
                       "mean_gain_ci_low", "mean_gain_ci_high", "q95_gain_ci_low", "q95_gain_ci_high")],
               file.path(config$output_root, "q1_bootstrap_distribution_summary.csv"))
  out
}
