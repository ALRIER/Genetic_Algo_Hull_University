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

  # Pair by condition and replicate to preserve CRN pairing.
  key_cols <- c("condition_id", "replicate")
  ga$key <- paste(ga$condition_id, ga$replicate, sep = "__")
  bmean$key <- paste(bmean$condition_id, bmean$replicate, sep = "__")
  bq95$key <- paste(bq95$condition_id, bq95$replicate, sep = "__")
  common <- Reduce(intersect, list(ga$key, bmean$key, bq95$key))
  ga <- ga[match(common, ga$key), ]
  bmean <- bmean[match(common, bmean$key), ]
  bq95 <- bq95[match(common, bq95$key), ]
  n <- length(common)

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
    best_mean_benchmark_ci = bench_mean,
    best_q95_benchmark_ci = bench_q95,
    mean_gain = point_mean_gain,
    mean_gain_ci_low = as.numeric(stats::quantile(boot["mean_gain", ], alpha, na.rm = TRUE, type = 8)),
    mean_gain_ci_high = as.numeric(stats::quantile(boot["mean_gain", ], 1 - alpha, na.rm = TRUE, type = 8)),
    q95_gain = point_q95_gain,
    q95_gain_ci_low = as.numeric(stats::quantile(boot["q95_gain", ], alpha, na.rm = TRUE, type = 8)),
    q95_gain_ci_high = as.numeric(stats::quantile(boot["q95_gain", ], 1 - alpha, na.rm = TRUE, type = 8)),
    ci_confirmed = point_mean_gain > 0 && point_q95_gain > 0 &&
      (!isTRUE(config$require_positive_ci) ||
         (as.numeric(stats::quantile(boot["mean_gain", ], alpha, na.rm = TRUE, type = 8)) > 0 &&
            as.numeric(stats::quantile(boot["q95_gain", ], alpha, na.rm = TRUE, type = 8)) > 0)),
    stringsAsFactors = FALSE
  )
}

q1_bootstrap_all <- function(long_df, config) {
  groups <- split(long_df, paste(long_df$validation_id, long_df$validation_seed, long_df$validation_mode, sep = "__"))
  out <- do.call(rbind, lapply(groups, q1_bootstrap_one, config = config))
  q1_write_csv(out, file.path(config$output_root, "q1_bootstrap_ci_by_seed.csv"))
  out
}

q1_stability_summary <- function(summary_df, ci_df, config) {
  merged <- merge(summary_df, ci_df,
                  by = c("validation_id", "distribution", "specialist_regime_id", "source_seed",
                         "validation_seed", "validation_class", "validation_mode"),
                  all.x = TRUE)
  agg <- aggregate(cbind(expanded_gate_pass, ci_confirmed, rel_gain_mean_vs_best_mean,
                         rel_gain_q95_vs_best_q95, mean_gain, q95_gain) ~
                     validation_id + distribution + specialist_regime_id + source_seed + validation_class + validation_mode,
                   data = merged,
                   FUN = function(z) c(mean = mean(as.numeric(z), na.rm = TRUE),
                                       min = min(as.numeric(z), na.rm = TRUE),
                                       max = max(as.numeric(z), na.rm = TRUE)))

  flat <- data.frame(agg[, 1:6], stringsAsFactors = FALSE)
  for (nm in names(agg)[7:ncol(agg)]) {
    mat <- do.call(rbind, agg[[nm]])
    flat[[paste0(nm, "_mean")]] <- mat[, "mean"]
    flat[[paste0(nm, "_min")]] <- mat[, "min"]
    flat[[paste0(nm, "_max")]] <- mat[, "max"]
  }
  flat$validation_seeds_n <- length(config$validation_seeds)
  flat$confirmed_label <- ifelse(flat$expanded_gate_pass_mean >= 0.80, "confirmed_stable_ga_win",
                                 ifelse(flat$expanded_gate_pass_mean >= 0.50, "marginal_or_seed_sensitive",
                                        "expanded_benchmark_retained"))
  q1_write_csv(flat, file.path(config$output_root, "q1_stability_summary.csv"))
  flat
}
