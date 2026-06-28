# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — GENERAL HELPERS
# =============================================================================

q1_msg <- function(..., .sep = "") {
  cat(sprintf("[Q1 %s] ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
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
  weight_cols <- c("w_mean", "w_median", "w_trimmed20", "w_harmonic", "w_geometric",
                   "w_mode_hsm", "w_mode_parzen", "w_trimean", "w_huber", "w_biweight")
  if (all(weight_cols %in% names(row))) {
    w <- as.numeric(row[weight_cols])
    w[!is.finite(w)] <- 0
    if (sum(w) <= 0) stop("Invalid winner weights in discovery row.", call. = FALSE)
    return(w / sum(w))
  }
  if ("winner_weight_vector" %in% names(row)) {
    w <- q1_parse_semicolon_values(row$winner_weight_vector, numeric = TRUE)
    if (length(w) != 10L) stop("winner_weight_vector does not have length 10.", call. = FALSE)
    w[!is.finite(w)] <- 0
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

q1_row_id <- function(row) {
  paste(row$distribution, row$specialist_regime_id, row$source_seed, row$validation_class, sep = "__")
}
