# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — REGIME SELECTION FROM DISCOVERY OUTPUTS
# =============================================================================

q1_find_winner_files <- function(discovery_root) {
  files <- list.files(discovery_root, pattern = "REGIME_FIRST_WINNER_SUMMARY__seed[0-9]+\\.csv$",
                      recursive = TRUE, full.names = TRUE)
  if (!length(files)) stop("No REGIME_FIRST_WINNER_SUMMARY__seed*.csv files found under DISCOVERY_ROOT.", call. = FALSE)
  files
}

q1_read_all_winners <- function(discovery_root) {
  files <- q1_find_winner_files(discovery_root)
  out <- lapply(files, function(f) {
    x <- q1_read_csv(f)
    seed <- sub(".*__seed([0-9]+)\\.csv$", "\\1", basename(f))
    x$source_seed <- as.integer(seed)
    x$source_file <- q1_norm_path(f)
    x
  })
  q1_align_rbind(out)
}

q1_label_candidates <- function(x) {
  x$gate_pass <- as.logical(x$gate_pass)
  x$validation_class <- "benchmark_control"
  x$validation_priority <- 40L

  x$validation_class[x$gate_pass] <- "accepted_ga_win"
  x$validation_priority[x$gate_pass] <- 10L

  rel_q95 <- suppressWarnings(as.numeric(x$ga_rel_improvement_q95))
  rel_mean <- suppressWarnings(as.numeric(x$ga_rel_improvement_mean))
  near <- !x$gate_pass & is.finite(rel_q95) & is.finite(rel_mean) &
    (rel_q95 > -0.10 | rel_mean > -0.10)
  x$validation_class[near] <- "borderline_near_gate"
  x$validation_priority[near] <- 20L

  # If the discovery output already contains seed-sensitive labels in later files,
  # this hook keeps them high priority without requiring a separate stability table.
  seed_sensitive_cols <- grep("seed.*sensitive|sensitive", names(x), value = TRUE, ignore.case = TRUE)
  if (length(seed_sensitive_cols)) {
    sens <- Reduce(`|`, lapply(seed_sensitive_cols, function(cc) grepl("sensitive|unstable|mixed", x[[cc]], ignore.case = TRUE)))
    x$validation_class[sens] <- "seed_sensitive_or_mixed"
    x$validation_priority[sens] <- 15L
  }

  x
}

q1_pick_negative_controls <- function(x, config) {
  controls <- x[!as.logical(x$gate_pass) & tolower(x$distribution) %in% tolower(config$negative_control_families), , drop = FALSE]
  if (!nrow(controls)) return(controls)
  controls$abs_rel_q95 <- abs(suppressWarnings(as.numeric(controls$ga_rel_improvement_q95)))
  controls$abs_rel_mean <- abs(suppressWarnings(as.numeric(controls$ga_rel_improvement_mean)))
  controls$control_score <- rowSums(cbind(controls$abs_rel_q95, controls$abs_rel_mean), na.rm = TRUE)
  controls <- controls[order(controls$distribution, controls$control_score), , drop = FALSE]
  out <- do.call(rbind, lapply(split(controls, controls$distribution), function(z) head(z, config$negative_controls_per_family)))
  if (is.null(out)) controls[0, names(x), drop = FALSE] else out[, names(x), drop = FALSE]
}

q1_select_regimes <- function(discovery_root, config) {
  x <- q1_read_all_winners(discovery_root)
  x <- q1_label_candidates(x)

  accepted <- x[x$validation_class == "accepted_ga_win", , drop = FALSE]
  borderline <- x[x$validation_class %in% c("borderline_near_gate", "seed_sensitive_or_mixed"), , drop = FALSE]
  controls <- q1_pick_negative_controls(x, config)
  if (nrow(controls)) {
    controls$validation_class <- "benchmark_control"
    controls$validation_priority <- 30L
    control_keys <- paste(controls$distribution, controls$specialist_regime_id, controls$source_seed, sep = "__")
    borderline_keys <- paste(borderline$distribution, borderline$specialist_regime_id, borderline$source_seed, sep = "__")
    borderline <- borderline[!borderline_keys %in% control_keys, , drop = FALSE]
  }

  reserved_controls <- controls
  remaining_slots <- max(0L, config$target_regimes - nrow(accepted) - nrow(reserved_controls))
  if (nrow(borderline) > remaining_slots) borderline <- borderline[seq_len(remaining_slots), , drop = FALSE]
  pool <- rbind(accepted, reserved_controls, borderline)
  if (!nrow(pool)) stop("No regimes available for fixed-weight validation selection.", call. = FALSE)

  # Deduplicate exact family/regime/seed rows while preserving priority.  We keep
  # source seed because the GA winner weights may differ by seed.
  pool$key <- paste(pool$distribution, pool$specialist_regime_id, pool$source_seed, sep = "__")
  pool <- pool[order(pool$validation_priority, pool$distribution, pool$specialist_regime_id, pool$source_seed), , drop = FALSE]
  pool <- pool[!duplicated(pool$key), , drop = FALSE]

  if (nrow(pool) > config$target_regimes) pool <- pool[seq_len(config$target_regimes), , drop = FALSE]
  pool$validation_id <- sprintf("FWV%03d", seq_len(nrow(pool)))
  pool$ga_weight_vector <- vapply(seq_len(nrow(pool)), function(i) {
    paste(sprintf("%.12g", q1_weight_vector_from_row(pool[i, , drop = FALSE])), collapse = ";")
  }, character(1))
  pool
}
