# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — GENERAL HELPERS
# =============================================================================

q1_msg <- function(..., .sep = "") {
  cat(sprintf("[FWV %s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      paste0(..., collapse = .sep), "\n", sep = "")
  flush.console()
}

q1_norm_path <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)

q1_make_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  q1_norm_path(path)
}

q1_read_csv <- function(path) {
  if (!file.exists(path)) stop("Missing CSV: ", path, call. = FALSE)
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

q1_write_csv <- function(x, path) {
  q1_make_dir(dirname(path))
  utils::write.csv(x, path, row.names = FALSE)
  formats <- getOption("q1.output_formats", default = "csv")
  if ("parquet" %in% formats && requireNamespace("arrow", quietly = TRUE)) {
    parquet_path <- sub("\\.csv\\z", ".parquet", path)
    try(arrow::write_parquet(as.data.frame(x), parquet_path), silent = TRUE)
  }
  invisible(path)
}

q1_parse_semicolon_values <- function(x, numeric = TRUE) {
  if (is.null(x) || length(x) == 0L || is.na(x[1]) || !nzchar(trimws(x[1]))) return(character(0))
  y <- unlist(strsplit(as.character(x[1]), ";", fixed = TRUE), use.names = FALSE)
  y <- trimws(y)
  y <- y[nzchar(y)]
  if (numeric) suppressWarnings(as.numeric(y)) else y
}

q1_weight_vector_from_row <- function(row) {
  expected_n <- if (exists("N_EST", inherits = TRUE)) get("N_EST", inherits = TRUE) else NA_integer_
  if (exists("ESTIMATOR_NAMES", inherits = TRUE)) {
    weight_cols <- paste0("w_", get("ESTIMATOR_NAMES", inherits = TRUE))
  } else {
    weight_cols <- grep("^w_", names(row), value = TRUE)
  }
  if (length(weight_cols) && all(weight_cols %in% names(row))) {
    w <- as.numeric(row[1, weight_cols, drop = TRUE])
    if (is.finite(expected_n) && length(w) != expected_n) {
      stop("GA weight columns have length ", length(w), " but N_EST is ", expected_n, ".", call. = FALSE)
    }
    w[!is.finite(w)] <- 0
    if (sum(w) <= 0) stop("Invalid winner weights in discovery row.", call. = FALSE)
    return(w / sum(w))
  }
  if ("winner_weight_vector" %in% names(row)) {
    w <- q1_parse_semicolon_values(row$winner_weight_vector, numeric = TRUE)
    if (is.finite(expected_n) && length(w) != expected_n) {
      stop("winner_weight_vector has length ", length(w), " but N_EST is ", expected_n, ".", call. = FALSE)
    }
    w[!is.finite(w)] <- 0
    if (sum(w) <= 0) stop("Invalid winner_weight_vector in discovery row.", call. = FALSE)
    return(w / sum(w))
  }
  stop("Could not find GA weight columns in discovery row.", call. = FALSE)
}

q1_source_original_modules <- function(project_root) {
  project_root <- q1_norm_path(project_root)
  modules <- c(
    "00_utils_debug_io.R",
    "01_paths_repro.R",
    "02_scenarios_sampling.R",
    "03_distributions_params.R",
    "04_estimators_registry.R",
    "05_ga_core.R",
    "06_data_prep.R",
    "07_fitness_objectives.R"
  )
  missing <- modules[!file.exists(file.path(project_root, modules))]
  if (length(missing)) {
    stop("PROJECT_ROOT does not contain required original modules: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(project_root)
  for (m in modules) source(file.path(project_root, m), chdir = TRUE)
  invisible(TRUE)
}

q1_available_cores <- function() {
  n <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  if (!is.finite(n) || is.na(n)) 1L else as.integer(n)
}

q1_worker_count <- function(config) {
  max(1L, min(as.integer(config$workers), q1_available_cores() - as.integer(config$reserve_cores)))
}

q1_start_cluster <- function(config) {
  n_workers <- q1_worker_count(config)
  q1_msg("Starting PSOCK cluster with ", n_workers, " workers.")
  parallel::makeCluster(n_workers, type = "PSOCK")
}

q1_stop_cluster <- function(cl) {
  if (!is.null(cl)) try(parallel::stopCluster(cl), silent = TRUE)
  invisible(TRUE)
}

q1_now_iso <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
}

q1_log_file <- function(config) {
  file.path(config$output_root, "logs", "q1_run.log")
}

q1_log_event <- function(config, event, ..., task_id = NA_character_) {
  q1_make_dir(file.path(config$output_root, "logs"))
  msg <- paste0(..., collapse = "")
  line <- paste(q1_now_iso(), event, task_id, msg, sep = ",")
  cat(line, "\n", file = q1_log_file(config), append = TRUE)
  invisible(line)
}

q1_status_file <- function(config) {
  file.path(config$output_root, "logs", "q1_task_status.csv")
}

q1_append_status <- function(config, task_id, status, message = "") {
  q1_make_dir(file.path(config$output_root, "logs"))
  f <- q1_status_file(config)
  if (!file.exists(f)) {
    cat("timestamp,task_id,status,message\n", file = f)
  }
  line <- paste(q1_now_iso(), task_id, status, gsub("[\r\n,]", " ", message), sep = ",")
  cat(line, "\n", file = f, append = TRUE)
  invisible(line)
}

q1_checkpoint_dir <- function(config) {
  q1_make_dir(file.path(config$output_root, "checkpoints"))
}

q1_task_checkpoint_path <- function(config, task_id) {
  file.path(q1_checkpoint_dir(config), paste0("task_", task_id, ".rds"))
}

q1_stage_checkpoint_path <- function(config, stage) {
  file.path(q1_checkpoint_dir(config), paste0("stage_", stage, ".rds"))
}

q1_write_rds_atomic <- function(x, path) {
  q1_make_dir(dirname(path))
  tmp <- paste0(path, ".tmp")
  saveRDS(x, tmp)
  if (file.exists(path)) unlink(path)
  file.rename(tmp, path)
  invisible(path)
}

q1_read_rds_if_valid <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

q1_md5 <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(data.frame(path = character(0), md5 = character(0)))
  sums <- tools::md5sum(paths)
  data.frame(path = q1_norm_path(names(sums)), md5 = unname(as.character(sums)), stringsAsFactors = FALSE)
}

q1_align_rbind <- function(xs) {
  xs <- xs[vapply(xs, nrow, integer(1)) >= 0L]
  if (!length(xs)) return(data.frame())
  all_names <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(x) {
    missing <- setdiff(all_names, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[, all_names, drop = FALSE]
  })
  do.call(rbind, xs)
}

q1_row_id <- function(row) {
  paste(row$distribution, row$specialist_regime_id, row$source_seed, row$validation_class, sep = "__")
}

q1_count_task_checkpoints <- function(config) {
  length(list.files(q1_checkpoint_dir(config), pattern = "^task_.*\\.rds$", full.names = TRUE))
}

q1_status_counts <- function(config) {
  f <- q1_status_file(config)
  if (!file.exists(f)) return(data.frame(status = character(0), n = integer(0)))
  x <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(x) || !"status" %in% names(x)) return(data.frame(status = character(0), n = integer(0)))
  as.data.frame(table(x$status), stringsAsFactors = FALSE) |>
    stats::setNames(c("status", "n"))
}

q1_latest_status_line <- function(config) {
  f <- q1_status_file(config)
  if (!file.exists(f)) return(NA_character_)
  x <- tryCatch(utils::tail(readLines(f, warn = FALSE), 1L), error = function(e) NA_character_)
  if (!length(x)) NA_character_ else x
}

q1_console_progress <- function(config, label, total_tasks = NA_integer_) {
  completed <- q1_count_task_checkpoints(config)
  counts <- q1_status_counts(config)
  count_txt <- if (nrow(counts)) paste(paste0(counts$status, "=", counts$n), collapse = "; ") else "no status rows yet"
  total_txt <- if (is.finite(total_tasks)) paste0("/", total_tasks) else ""
  latest <- q1_latest_status_line(config)
  q1_msg(label, " | task checkpoints=", completed, total_txt, " | ", count_txt)
  if (!is.na(latest)) q1_msg(label, " | latest: ", latest)
  invisible(TRUE)
}
