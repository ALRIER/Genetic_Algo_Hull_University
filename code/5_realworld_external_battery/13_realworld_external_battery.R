# =============================================================================
# 13_REALWORLD_EXTERNAL_BATTERY_V5_AUDIT_READY.R
# =============================================================================
# External public-data validation battery for frozen specialist estimators.
#
# The module evaluates the fixed-weight specialists selected in the previous
# validation stage on independent public datasets. It preserves source-level
# provenance, dataset diagnostics, condition-level error summaries, and gate
# decisions needed to audit transfer performance outside the synthetic regime
# grid used for discovery and fixed-weight validation.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a[1])) b else a

eb_script_dir <- function(default = getwd()) {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]),
                                 winslash = "/", mustWork = FALSE)))
  }
  this_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  dirname(normalizePath(this_file %||% file.path(default, "13_realworld_external_battery.R"),
                        winslash = "/", mustWork = FALSE))
}

source(file.path(eb_script_dir(), "realworld_external_battery_utils.R"), local = FALSE)

eb_cat <- rw_cat

eb_safe <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", as.character(x))

eb_download <- function(url, dest_dir, id) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  ext <- tools::file_ext(sub("\\?.*$", "", url))
  if (!nzchar(ext)) ext <- "dat"
  dest <- file.path(dest_dir, paste0(eb_safe(id), ".", ext))
  accessed_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %z")
  ok <- tryCatch({
    utils::download.file(url, dest, mode = "wb", quiet = TRUE)
    file.exists(dest) && file.info(dest)$size > 0
  }, error = function(e) FALSE)
  list(ok = ok, path = if (ok) dest else NA_character_, accessed_at = accessed_at,
       md5 = if (ok) rw_md5(dest) else NA_character_)
}

eb_clean_numeric <- function(x, positive = FALSE, log1p = FALSE, diff_log = FALSE) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (positive) x <- x[x > 0]
  if (log1p) x <- log1p(x[x >= 0])
  if (diff_log) x <- diff(log(x[x > 0]))
  x[is.finite(x)]
}

eb_skew <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3L) return(0)
  s <- stats::sd(x)
  if (!is.finite(s) || s <= 0) return(0)
  mean((x - mean(x))^3) / s^3
}

eb_fix_merged_columns <- function(df) {
  if (!nrow(df)) return(df)
  bases <- c("parent_dataset_id", "domain", "source_url", "accessed_at",
             "source_file", "source_md5", "loader", "variable", "transform",
             "subset_rule", "family_hint", "n", "skewness", "excess_kurtosis",
             "support_sign", "theta_star", "load_ok", "load_error", "notes")
  for (nm in bases) {
    x <- paste0(nm, ".x")
    y <- paste0(nm, ".y")
    if (!nm %in% names(df) && (x %in% names(df) || y %in% names(df))) {
      if (x %in% names(df) && y %in% names(df)) {
        df[[nm]] <- ifelse(is.na(df[[x]]) | df[[x]] == "", df[[y]], df[[x]])
      } else if (x %in% names(df)) {
        df[[nm]] <- df[[x]]
      } else {
        df[[nm]] <- df[[y]]
      }
    }
    df[[x]] <- NULL
    df[[y]] <- NULL
  }
  df
}

eb_write_csv_split <- function(x, path, max_rows = 5000L, write_index_at_path = TRUE) {
  # Writes large tables as part001/part002/... instead of creating one huge CSV.
  # If split, the requested path becomes a small index CSV listing the parts.
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (!is.data.frame(x)) x <- as.data.frame(x)
  n <- nrow(x)
  max_rows <- suppressWarnings(as.integer(max_rows))
  if (!is.finite(max_rows) || max_rows <= 0L || n <= max_rows) {
    utils::write.csv(x, path, row.names = FALSE)
    return(invisible(data.frame(file = basename(path), rows = n, part = 1L, stringsAsFactors = FALSE)))
  }

  ext <- tools::file_ext(path)
  if (!nzchar(ext)) ext <- "csv"
  stem <- file.path(dirname(path), tools::file_path_sans_ext(basename(path)))
  n_parts <- ceiling(n / max_rows)
  idx <- vector("list", n_parts)
  for (i in seq_len(n_parts)) {
    lo <- (i - 1L) * max_rows + 1L
    hi <- min(i * max_rows, n)
    part_path <- sprintf("%s_part%03d.%s", stem, i, ext)
    utils::write.csv(x[lo:hi, , drop = FALSE], part_path, row.names = FALSE)
    idx[[i]] <- data.frame(
      table = basename(path), part = i, part_file = basename(part_path),
      first_row = lo, last_row = hi, rows = hi - lo + 1L,
      stringsAsFactors = FALSE
    )
  }
  index_df <- do.call(rbind, idx)
  index_path <- sprintf("%s_PARTS_INDEX.%s", stem, ext)
  utils::write.csv(index_df, index_path, row.names = FALSE)
  if (isTRUE(write_index_at_path)) utils::write.csv(index_df, path, row.names = FALSE)
  invisible(index_df)
}

eb_record <- function(id, domain, source_url, loader, variable, transform = "raw",
                      family_hint = "unknown", subset_rule = "full", notes = "") {
  list(id = id, domain = domain, source_url = source_url, loader = loader,
       variable = variable, transform = transform, family_hint = family_hint,
       subset_rule = subset_rule, notes = notes)
}

eb_market_specs <- function() {
  symbols <- c("spy", "qqq", "iwm", "eem", "efa", "gld", "slv", "uso", "tlt", "hyg",
               "lqd", "xlf", "xle", "xlk", "xlu", "xlp", "xly", "xli", "xlv", "xlb",
               "dia", "ief", "shy", "vnq")
  lapply(symbols, function(s) {
    url <- sprintf("https://stooq.com/q/d/l/?s=%s.us&i=d", s)
    eb_record(sprintf("MKT_%s_logret", toupper(s)), "market_returns", url,
              loader = "stooq_daily_close_logret", variable = "Close",
              transform = "diff(log(Close))", family_hint = "normal_heavytail")
  })
}

eb_uci_specs <- function() {
  c(
    list(eb_record("UCI_wine_red_alcohol", "positive_skew_tabular",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
                   "csv_semicolon", "alcohol", "raw", "lognormal")),
    list(eb_record("UCI_wine_red_residual_sugar", "positive_skew_tabular",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv",
                   "csv_semicolon", "residual.sugar", "raw", "lognormal")),
    list(eb_record("UCI_wine_white_alcohol", "positive_skew_tabular",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv",
                   "csv_semicolon", "alcohol", "raw", "lognormal")),
    list(eb_record("UCI_wine_white_residual_sugar", "positive_skew_tabular",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv",
                   "csv_semicolon", "residual.sugar", "raw", "lognormal")),
    list(eb_record("UCI_forestfires_log_area", "environmental_extremes",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv",
                   "csv", "area", "log1p", "lognormal")),
    list(eb_record("UCI_abalone_whole_weight", "biological_positive",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",
                   "abalone", "whole_weight", "raw", "lognormal")),
    list(eb_record("UCI_abalone_rings", "duration_count",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",
                   "abalone", "rings", "raw", "weibull")),
    list(eb_record("UCI_auto_mpg", "engineering_efficiency",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
                   "auto_mpg", "mpg", "raw", "normal")),
    list(eb_record("UCI_auto_horsepower", "engineering_positive",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/auto-mpg/auto-mpg.data",
                   "auto_mpg", "horsepower", "raw", "lognormal")),
    list(eb_record("UCI_parkinsons_PPE", "biomedical_signal",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data",
                   "csv", "PPE", "raw", "lognormal")),
    list(eb_record("UCI_parkinsons_HNR", "biomedical_signal",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/parkinsons/parkinsons.data",
                   "csv", "HNR", "raw", "normal")),
    list(eb_record("UCI_bike_hour_count", "mobility_demand",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip",
                   "bike_zip_hour", "cnt", "raw", "lognormal")),
    list(eb_record("UCI_bike_hour_casual", "mobility_demand",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip",
                   "bike_zip_hour", "casual", "raw", "lognormal")),
    list(eb_record("UCI_bike_hour_registered", "mobility_demand",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00275/Bike-Sharing-Dataset.zip",
                   "bike_zip_hour", "registered", "raw", "lognormal")),
    list(eb_record("UCI_online_news_log_shares", "web_popularity",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip",
                   "online_news_zip", "shares", "log1p", "lognormal")),
    list(eb_record("UCI_online_news_tokens", "web_popularity",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00332/OnlineNewsPopularity.zip",
                   "online_news_zip", "n_tokens_content", "raw", "lognormal")),
    list(eb_record("UCI_airquality_CO", "air_quality",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00360/AirQualityUCI.zip",
                   "airquality_zip", "CO.GT", "raw", "lognormal")),
    list(eb_record("UCI_airquality_NOx", "air_quality",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00360/AirQualityUCI.zip",
                   "airquality_zip", "NOx.GT", "raw", "lognormal")),
    list(eb_record("UCI_student_math_G3", "education_scores",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip",
                   "student_zip_math", "G3", "raw", "normal")),
    list(eb_record("UCI_student_math_absences", "education_counts",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip",
                   "student_zip_math", "absences", "raw", "lognormal")),
    list(eb_record("UCI_student_por_G3", "education_scores",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip",
                   "student_zip_por", "G3", "raw", "normal")),
    list(eb_record("UCI_student_por_absences", "education_counts",
                   "https://archive.ics.uci.edu/ml/machine-learning-databases/00320/student.zip",
                   "student_zip_por", "absences", "raw", "lognormal"))
  )
}

eb_rdatasets_specs <- function(download_dir,
                               max_sources = as.integer(Sys.getenv("EXTERNAL_RDATASETS_MAX", unset = "140")),
                               min_rows = as.integer(Sys.getenv("EXTERNAL_RDATASETS_MIN_ROWS", unset = "150"))) {
  index_url <- "https://vincentarelbundock.github.io/Rdatasets/datasets.csv"
  index_dl <- eb_download(index_url, download_dir, "RDATASETS_INDEX")
  if (!isTRUE(index_dl$ok)) return(list())
  idx <- tryCatch(utils::read.csv(index_dl$path, stringsAsFactors = FALSE, check.names = TRUE),
                  error = function(e) data.frame())
  needed <- c("Package", "Item", "Title", "Rows", "n_numeric")
  if (!nrow(idx) || !all(needed %in% names(idx))) return(list())
  idx <- idx[is.finite(idx$Rows) & idx$Rows >= min_rows & idx$n_numeric > 0, , drop = FALSE]
  if (!nrow(idx)) return(list())
  idx$CSV_url <- if ("CSV" %in% names(idx)) {
    ifelse(grepl("^https?://", idx$CSV), idx$CSV,
           sprintf("https://vincentarelbundock.github.io/Rdatasets/csv/%s/%s.csv",
                   idx$Package, idx$Item))
  } else {
    sprintf("https://vincentarelbundock.github.io/Rdatasets/csv/%s/%s.csv",
            idx$Package, idx$Item)
  }
  idx$Doc_url <- if ("Doc" %in% names(idx)) {
    ifelse(grepl("^https?://", idx$Doc), idx$Doc, NA_character_)
  } else {
    NA_character_
  }

  idx <- idx[order(idx$Package, -idx$Rows, idx$Item), , drop = FALSE]
  pkgs <- unique(idx$Package)
  selected <- idx[0, , drop = FALSE]
  cursor <- 1L
  while (nrow(selected) < max_sources && nrow(idx)) {
    pkg <- pkgs[((cursor - 1L) %% length(pkgs)) + 1L]
    cand <- idx[idx$Package == pkg, , drop = FALSE]
    already <- paste(selected$Package, selected$Item)
    cand <- cand[!paste(cand$Package, cand$Item) %in% already, , drop = FALSE]
    if (nrow(cand)) selected <- rbind(selected, cand[1, , drop = FALSE])
    cursor <- cursor + 1L
    if (cursor > length(pkgs) * max_sources && nrow(selected) == 0L) break
    if (cursor > length(pkgs) * (max_sources + 5L)) break
  }

  lapply(seq_len(nrow(selected)), function(i) {
    r <- selected[i, , drop = FALSE]
    eb_record(
      id = sprintf("RD_%s_%s_auto", eb_safe(r$Package), eb_safe(r$Item)),
      domain = paste0("rdatasets_", eb_safe(r$Package)),
      source_url = r$CSV_url,
      loader = "rdatasets_auto_numeric",
      variable = "__auto_numeric__",
      transform = "raw",
      family_hint = "empirical_public",
      subset_rule = sprintf("auto_numeric_column; rdatasets_rows=%s; rdatasets_n_numeric=%s",
                            r$Rows, r$n_numeric),
      notes = paste("Rdatasets public catalog:", r$Package, r$Item, "-", r$Title,
                    if (is.na(r$Doc_url)) "" else paste0("; doc=", r$Doc_url))
    )
  })
}

eb_specs <- function(download_dir = tempdir()) {
  # The external battery combines market returns, curated UCI datasets, and a
  # balanced sample from Rdatasets to cover multiple empirical data domains.
  c(eb_market_specs(), eb_uci_specs(), eb_rdatasets_specs(download_dir))
}

eb_read_zip_member <- function(zip_path, pattern, exdir) {
  files <- utils::unzip(zip_path, list = TRUE)$Name
  member <- files[grepl(pattern, files, ignore.case = TRUE)][1]
  if (is.na(member)) stop("No zip member matching: ", pattern, call. = FALSE)
  utils::unzip(zip_path, files = member, exdir = exdir, overwrite = TRUE)
  file.path(exdir, member)
}

eb_load_one <- function(spec, download_dir) {
  # V5: in-memory specs are used for secondary within-dataset depth probes.
  # They are tagged as depth probes and must not be counted as independent
  # public datasets in the primary breadth claim.
  if (!is.null(spec$x_inline)) {
    x_inline <- eb_clean_numeric(spec$x_inline)
    return(list(ok = length(x_inline) >= 30L, x = x_inline,
                path = NA_character_, md5 = NA_character_,
                accessed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
                variable = spec$variable, transform = spec$transform,
                error = if (length(x_inline) >= 30L) NA_character_ else "inline_n_lt_30"))
  }
  dl <- eb_download(spec$source_url, download_dir, spec$id)
  if (!isTRUE(dl$ok)) {
    return(list(ok = FALSE, x = numeric(0), path = NA_character_, md5 = NA_character_,
                accessed_at = dl$accessed_at, error = "download_failed"))
  }
  actual_variable <- spec$variable
  actual_transform <- spec$transform
  x <- tryCatch({
    loader <- spec$loader
    if (loader == "stooq_daily_close_logret") {
      df <- utils::read.csv(dl$path, stringsAsFactors = FALSE)
      eb_clean_numeric(df[[spec$variable]], positive = TRUE, diff_log = TRUE)
    } else if (loader == "rdatasets_auto_numeric") {
      df <- utils::read.csv(dl$path, stringsAsFactors = FALSE, check.names = TRUE)
      drop_names <- c("X", "rownames", "rowname", "id", "ID", "Id", "time", "Time",
                      "date", "Date", "year", "Year", "longitude", "Longitude",
                      "latitude", "Latitude", "long", "lat", "lon")
      cand <- setdiff(names(df), drop_names)
      scores <- vapply(cand, function(nm) {
        z <- suppressWarnings(as.numeric(df[[nm]]))
        z <- z[is.finite(z)]
        if (grepl("(^|_)(id|row|index|longitude|latitude|long|lat|lon)($|_)", nm, ignore.case = TRUE)) return(-Inf)
        if (length(z) < 30L || stats::var(z) <= 0 || length(unique(z)) < 10L) return(-Inf)
        if (length(unique(z)) > 0.9 * length(z) &&
            abs(stats::cor(z, seq_along(z), use = "complete.obs")) > 0.995) return(-Inf)
        med <- stats::median(z)
        madv <- stats::mad(z, constant = 1)
        tail_rate <- if (is.finite(madv) && madv > 0) mean(abs(z - med) / madv > 3) else 0
        log10(length(z)) + log10(length(unique(z))) + abs(eb_skew(z)) + 2 * tail_rate
      }, numeric(1))
      if (!length(scores) || all(!is.finite(scores))) stop("no_valid_numeric_column", call. = FALSE)
      actual_variable <- names(which.max(scores))
      actual_transform <- "raw_auto_numeric"
      eb_clean_numeric(df[[actual_variable]], log1p = FALSE)
    } else if (loader == "csv") {
      df <- utils::read.csv(dl$path, stringsAsFactors = FALSE, check.names = TRUE)
      eb_clean_numeric(df[[make.names(spec$variable)]], log1p = spec$transform == "log1p")
    } else if (loader == "csv_semicolon") {
      df <- utils::read.csv(dl$path, sep = ";", stringsAsFactors = FALSE, check.names = TRUE)
      eb_clean_numeric(df[[make.names(spec$variable)]], log1p = spec$transform == "log1p")
    } else if (loader == "abalone") {
      nm <- c("sex", "length", "diameter", "height", "whole_weight", "shucked_weight",
              "viscera_weight", "shell_weight", "rings")
      df <- utils::read.csv(dl$path, header = FALSE, col.names = nm, stringsAsFactors = FALSE)
      eb_clean_numeric(df[[spec$variable]], log1p = spec$transform == "log1p")
    } else if (loader == "auto_mpg") {
      df <- utils::read.table(dl$path, header = FALSE, na.strings = "?", fill = TRUE,
                              stringsAsFactors = FALSE, quote = "\"")
      names(df)[1:8] <- c("mpg", "cylinders", "displacement", "horsepower", "weight",
                          "acceleration", "model_year", "origin")
      eb_clean_numeric(df[[spec$variable]], log1p = spec$transform == "log1p")
    } else if (loader == "bike_zip_hour") {
      fp <- eb_read_zip_member(dl$path, "hour\\.csv$", tempfile("bike_"))
      df <- utils::read.csv(fp, stringsAsFactors = FALSE, check.names = TRUE)
      eb_clean_numeric(df[[spec$variable]], log1p = spec$transform == "log1p")
    } else if (loader == "online_news_zip") {
      fp <- eb_read_zip_member(dl$path, "OnlineNewsPopularity\\.csv$", tempfile("news_"))
      df <- utils::read.csv(fp, stringsAsFactors = FALSE, check.names = TRUE)
      names(df) <- trimws(names(df))
      eb_clean_numeric(df[[spec$variable]], log1p = spec$transform == "log1p")
    } else if (loader == "airquality_zip") {
      fp <- eb_read_zip_member(dl$path, "AirQualityUCI\\.csv$", tempfile("air_"))
      df <- utils::read.csv(fp, sep = ";", dec = ",", stringsAsFactors = FALSE,
                            check.names = TRUE, na.strings = c("-200", "-200.0", ""))
      eb_clean_numeric(df[[make.names(spec$variable)]], log1p = spec$transform == "log1p")
    } else if (loader == "student_zip_math" || loader == "student_zip_por") {
      pat <- if (loader == "student_zip_math") "student-mat\\.csv$" else "student-por\\.csv$"
      fp <- eb_read_zip_member(dl$path, pat, tempfile("student_"))
      df <- utils::read.csv(fp, sep = ";", stringsAsFactors = FALSE, check.names = TRUE)
      eb_clean_numeric(df[[spec$variable]], log1p = spec$transform == "log1p")
    } else {
      stop("Unknown loader: ", loader, call. = FALSE)
    }
  }, error = function(e) structure(numeric(0), error = conditionMessage(e)))

  err <- attr(x, "error")
  list(ok = length(x) >= 30L, x = x, path = dl$path, md5 = dl$md5,
       accessed_at = dl$accessed_at,
       variable = actual_variable, transform = actual_transform,
       error = if (length(x) >= 30L) NA_character_ else (err %||% "n_lt_30"))
}

eb_grade <- function(mean_gain, q95_gain, ci_low, gate, equal_q95) {
  if (isTRUE(gate) && is.finite(ci_low) && ci_low > 0) return("top_confirmed")
  if (isTRUE(gate)) return("q1_gate_point")
  if (is.finite(q95_gain) && q95_gain > 0) return("q95_only")
  if (is.finite(mean_gain) && mean_gain > 0) return("mean_only")
  if (is.finite(equal_q95) && equal_q95 > 0) return("beats_equal_weight_only")
  "benchmark_control"
}


# -----------------------------------------------------------------------------
# V5 methodological layer: pre-registered taxonomy, breadth/depth selection, FDR,
# clean folders, and reproducible reporting.
# -----------------------------------------------------------------------------
# This stage does not re-run discovery or alter frozen GA weights. It evaluates
# the taxonomy-selected specialists under an external validation protocol:
#   1. profile gate first, using fixed diagnostic thresholds;
#   2. primary benchmark is pre-registered by empirical profile;
#   3. secondary oracle audit is written separately and is not the primary claim;
#   4. paired bootstrap is aligned to the exact reported gain;
#   5. Pareto/equal-weight signals are evidence grades, not promoted to strong wins;
#   6. large CSV outputs are split into auditable parts of EXTERNAL_CSV_MAX_ROWS;
#   7. dataset breadth is balanced by empirical profile and source group;
#   8. saturated datasets are capped so one source cannot dominate the evidence;
#   9. futile datasets can stop early after a pre-registered minimum;
#  10. within-dataset resampling is written as secondary depth evidence only.
# More bootstrap does not increase the true gain. It only stabilizes the interval.

.eb_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L || is.na(x[1]) || !nzchar(as.character(x[1]))) return(default)
  tolower(trimws(as.character(x[1]))) %in% c("1", "true", "t", "yes", "y", "si", "sí")
}

# --------------------------
# Breadth/depth and audit controls for the external validation stage.
# --------------------------
.eb_parse_named_ints <- function(x, default = list()) {
  if (is.null(x) || length(x) == 0L || is.na(x[1]) || !nzchar(as.character(x[1]))) return(default)
  out <- default
  chunks <- strsplit(as.character(x[1]), ";", fixed = TRUE)[[1]]
  for (ch in chunks) {
    kv <- strsplit(ch, "=", fixed = TRUE)[[1]]
    if (length(kv) != 2L) next
    key <- trimws(kv[1])
    val <- suppressWarnings(as.integer(trimws(kv[2])))
    if (nzchar(key) && is.finite(val) && val >= 0L) out[[key]] <- val
  }
  out
}

.eb_default_profile_quota <- function(target_loaded = 120L) {
  target_loaded <- suppressWarnings(as.integer(target_loaded))
  if (!is.finite(target_loaded) || target_loaded <= 0L) target_loaded <- 120L
  base <- max(5L, floor(target_loaded / 6L))
  list(
    normal_like = base,
    lognormal_like = base,
    weibull_like = base,
    heavy_tail_symmetric = base,
    bounded_or_proportion = base,
    zero_inflated_count = base,
    empirical_unknown = max(3L, floor(base / 2L))
  )
}

.eb_get_count <- function(env, key) {
  key <- as.character(key %||% "unknown")[1]
  if (!exists(key, envir = env, inherits = FALSE)) return(0L)
  suppressWarnings(as.integer(get(key, envir = env, inherits = FALSE)))
}

.eb_inc_count <- function(env, key, by = 1L) {
  key <- as.character(key %||% "unknown")[1]
  assign(key, .eb_get_count(env, key) + as.integer(by), envir = env)
  invisible(.eb_get_count(env, key))
}

.eb_source_group <- function(spec) {
  # The cap is by source family, not exact URL, so one catalog cannot dominate.
  dm <- tolower(as.character(spec$domain %||% "unknown"))
  loader <- tolower(as.character(spec$loader %||% "unknown"))
  url <- tolower(as.character(spec$source_url %||% "unknown"))
  if (grepl("rdatasets", dm) || grepl("rdatasets", loader) || grepl("rdatasets", url)) return("Rdatasets")
  if (grepl("uci|archive.ics", dm) || grepl("archive.ics.uci", url)) return("UCI")
  if (grepl("stooq|market", dm) || grepl("stooq", url)) return("market_returns")
  if (grepl("kaggle", dm) || grepl("kaggle", url)) return("Kaggle")
  if (grepl("data.gov", dm) || grepl("data.gov", url)) return("DataGov")
  eb_safe(dm)
}

.eb_selection_decision <- function(spec, empirical_profile, analysis_role,
                                   profile_counts_env, source_counts_env,
                                   target_loaded = 120L,
                                   profile_quota = list(),
                                   source_cap_prop = 0.25,
                                   enable_balance = TRUE) {
  role <- as.character(analysis_role %||% "primary_dataset")[1]
  source_group <- .eb_source_group(spec)
  if (!isTRUE(enable_balance) || !identical(role, "primary_dataset")) {
    return(list(selected = TRUE, status = "selected", reason = "balance_not_applied_to_depth_or_disabled", source_group = source_group))
  }
  ep <- as.character(empirical_profile %||% "empirical_unknown")[1]
  q <- profile_quota[[ep]]
  if (is.null(q)) q <- profile_quota[["empirical_unknown"]] %||% Inf
  q <- suppressWarnings(as.integer(q))
  if (is.finite(q) && .eb_get_count(profile_counts_env, ep) >= q) {
    return(list(selected = FALSE, status = "skipped_profile_quota", reason = sprintf("profile quota reached for %s", ep), source_group = source_group))
  }
  target_loaded <- suppressWarnings(as.integer(target_loaded))
  if (!is.finite(target_loaded) || target_loaded <= 0L) target_loaded <- 120L
  max_source <- max(1L, ceiling(target_loaded * as.numeric(source_cap_prop)))
  if (is.finite(max_source) && .eb_get_count(source_counts_env, source_group) >= max_source) {
    return(list(selected = FALSE, status = "skipped_source_cap", reason = sprintf("source cap reached for %s", source_group), source_group = source_group))
  }
  list(selected = TRUE, status = "selected", reason = "selected_by_preregistered_breadth_quota", source_group = source_group)
}

.eb_depth_probe_specs <- function(parent_spec, x, seed = 1L, max_variants = 3L,
                                  min_n = 80L, sample_frac = 1.0) {
  # These are secondary depth probes. They are not new independent datasets.
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (length(x) < min_n) return(list())
  set.seed(.ensure_seed(seed))
  max_variants <- suppressWarnings(as.integer(max_variants))
  if (!is.finite(max_variants) || max_variants <= 0L) return(list())
  n_out <- max(min_n, floor(length(x) * as.numeric(sample_frac)))
  n_out <- min(n_out, length(x))
  qs <- stats::quantile(x, c(0.10, 0.25, 0.50, 0.75, 0.90), na.rm = TRUE, type = 8)
  mk <- function(label, values, rule) {
    values <- values[is.finite(values)]
    if (length(values) < min_n) return(NULL)
    values <- sample(values, size = min(n_out, length(values)), replace = length(values) < n_out)
    z <- parent_spec
    z$id <- paste0(parent_spec$id, "__depth_", label)
    z$parent_dataset_id <- parent_spec$parent_dataset_id %||% parent_spec$id
    z$analysis_role <- "within_dataset_depth_probe"
    z$source_url <- parent_spec$source_url %||% "in_memory_depth_probe"
    z$loader <- "inline_depth_probe"
    z$x_inline <- values
    z$subset_rule <- paste(parent_spec$subset_rule %||% "full", rule, sep = "; ")
    z$notes <- paste(parent_spec$notes %||% "", "V5 secondary within-dataset depth probe; not independent N.")
    z
  }
  out <- list()
  body <- x[x >= qs[[1]] & x <= qs[[5]]]
  out[[length(out) + 1L]] <- mk("body_10_90", body, "depth_probe=body_10_90")
  upper_pool <- c(x[x >= qs[[4]]], sample(x, length(x), replace = TRUE))
  out[[length(out) + 1L]] <- mk("upper_tail_enriched", upper_pool, "depth_probe=upper_tail_enriched")
  out[[length(out) + 1L]] <- mk("bootstrap_full", sample(x, length(x), replace = TRUE), "depth_probe=bootstrap_full")
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) > max_variants) out <- out[seq_len(max_variants)]
  out
}

.eb_q95 <- function(z) unname(stats::quantile(z, 0.95, na.rm = TRUE, type = 8))

.eb_make_wide <- function(rep_df, ds_id, sp_id, mode, m_effective, contam_condition) {
  rep_key <- paste(ds_id, sp_id, mode, m_effective, contam_condition, rep_df$replicate, sep = "|")
  wide <- reshape(data.frame(key = rep_key, estimator = rep_df$estimator, sq_error = rep_df$sq_error),
                  idvar = "key", timevar = "estimator", direction = "wide")
  names(wide) <- sub("^sq_error\\.", "", names(wide))
  wide
}

.eb_empirical_profile <- function(family_hint, domain, skewness, excess_kurtosis, support_sign) {
  fh <- tolower(as.character(family_hint %||% "unknown"))
  dm <- tolower(as.character(domain %||% "unknown"))
  sk <- suppressWarnings(as.numeric(skewness))
  ku <- suppressWarnings(as.numeric(excess_kurtosis))
  ss <- tolower(as.character(support_sign %||% "unknown"))

  if (grepl("bounded|proportion|percent|percentage|rate", fh) || grepl("bounded|proportion|percent|percentage|rate", dm)) return("bounded_or_proportion")
  if (grepl("zero|inflated|poisson|count|counts", fh) || grepl("zero|inflated|poisson|count|counts", dm)) return("zero_inflated_count")
  if (grepl("weibull|duration|survival", fh) || grepl("duration|survival|time_to_event", dm)) return("weibull_like")
  if (grepl("lognormal", fh)) return("lognormal_like")
  if (grepl("normal", fh) && !grepl("lognormal", fh)) return("normal_like")
  if (grepl("returns|market", dm)) return("heavy_tail_symmetric")
  if (is.finite(sk) && sk >= 1.25 && !grepl("negative", ss)) return("lognormal_like")
  if (is.finite(sk) && abs(sk) <= 0.60 && is.finite(ku) && ku <= 4) return("normal_like")
  if (is.finite(sk) && sk > 0.25 && sk < 2.50 && is.finite(ku) && ku < 8 && !grepl("negative", ss)) return("weibull_like")
  "empirical_unknown"
}

.eb_profile_match <- function(specialist_family, family_hint, domain, skewness, excess_kurtosis, support_sign) {
  sf <- tolower(as.character(specialist_family %||% "unknown"))
  ep <- .eb_empirical_profile(family_hint, domain, skewness, excess_kurtosis, support_sign)
  if (grepl("lognormal", sf)) return(ep %in% c("lognormal_like"))
  if (grepl("weibull", sf)) return(ep %in% c("weibull_like"))
  if (grepl("normal", sf) && !grepl("lognormal", sf)) return(ep %in% c("normal_like", "heavy_tail_symmetric"))
  FALSE
}

.eb_profile_benchmark_map_default <- function() {
  # Pre-registered benchmark candidates by empirical profile. The function uses
  # ordered aliases because estimator registries may differ across code snapshots.
  list(
    normal_like = c("mean", "huber_k1.345", "huber", "biweight"),
    heavy_tail_symmetric = c("huber_k1.345", "huber", "biweight", "trimmed20", "winsorized_0.1"),
    lognormal_like = c("huber_k1.345", "huber", "trimmed20", "winsorized_0.1", "median"),
    weibull_like = c("huber_k1.345", "huber", "trimmed20", "winsorized_0.1", "median"),
    bounded_or_proportion = c("huber_k1.345", "huber", "biweight", "trimmed20", "median"),
    zero_inflated_count = c("huber_k1.345", "huber", "trimmed20", "winsorized_0.1", "median"),
    empirical_unknown = c("huber_k1.345", "huber", "biweight", "trimmed20", "median")
  )
}

.eb_parse_profile_benchmark_map <- function(x) {
  # Optional override syntax:
  #   profile:est1|est2;profile2:est3|est4
  if (is.null(x) || length(x) == 0L || is.na(x[1]) || !nzchar(as.character(x[1]))) {
    return(.eb_profile_benchmark_map_default())
  }
  out <- .eb_profile_benchmark_map_default()
  chunks <- strsplit(as.character(x[1]), ";", fixed = TRUE)[[1]]
  for (ch in chunks) {
    kv <- strsplit(ch, ":", fixed = TRUE)[[1]]
    if (length(kv) != 2L) next
    key <- trimws(kv[1])
    vals <- trimws(strsplit(kv[2], "|", fixed = TRUE)[[1]])
    vals <- vals[nzchar(vals)]
    if (nzchar(key) && length(vals)) out[[key]] <- vals
  }
  out
}

.eb_preregistered_benchmarks <- function(empirical_profile, benchmark_map_string = "") {
  mp <- .eb_parse_profile_benchmark_map(benchmark_map_string)
  ep <- as.character(empirical_profile %||% "empirical_unknown")[1]
  vals <- mp[[ep]]
  if (is.null(vals) || !length(vals)) vals <- mp[["empirical_unknown"]]
  unique(vals)
}

.eb_choose_joint_benchmark <- function(wide, bench_cols, policy = "joint_rank", primary_metric = "q95",
                                       empirical_profile = "empirical_unknown",
                                       benchmark_map_string = "") {
  bench_cols <- intersect(bench_cols, names(wide))
  bench_cols <- bench_cols[vapply(bench_cols, function(nm) any(is.finite(wide[[nm]])), logical(1))]
  if (!length(bench_cols)) return(list(best_joint = NA_character_, best_mean = NA_character_, best_q95 = NA_character_,
                                       benchmark_candidate_set = character(0), benchmark_fallback_used = FALSE,
                                       bench_mean = NA_real_, bench_q95 = NA_real_))
  bench_means <- vapply(bench_cols, function(nm) mean(wide[[nm]], na.rm = TRUE), numeric(1))
  bench_q95 <- vapply(bench_cols, function(nm) .eb_q95(wide[[nm]]), numeric(1))
  best_mean <- names(which.min(bench_means))
  best_q95 <- names(which.min(bench_q95))

  policy <- tolower(as.character(policy[1]))
  primary_metric <- tolower(as.character(primary_metric[1]))
  candidate_set <- character(0)
  if (policy %in% c("profile_preregistered", "preregistered", "profile_fixed")) {
    candidate_set <- .eb_preregistered_benchmarks(empirical_profile, benchmark_map_string)
    available <- intersect(candidate_set, bench_cols)
    if (length(available)) {
      best_joint <- available[1]
    } else {
      # Conservative reproducibility fallback if a registry snapshot lacks the
      # first pre-registered estimator name. This fallback is recorded.
      score <- rank(bench_means, ties.method = "average") + rank(bench_q95, ties.method = "average")
      ties <- names(score)[score == min(score, na.rm = TRUE)]
      if (length(ties) > 1L) ties <- ties[order(bench_q95[ties], bench_means[ties])]
      best_joint <- ties[1]
    }
  } else if (policy %in% c("primary", "primary_metric", "q95_primary")) {
    best_joint <- if (identical(primary_metric, "mean")) best_mean else best_q95
  } else if (policy %in% c("mean")) {
    best_joint <- best_mean
  } else if (policy %in% c("q95")) {
    best_joint <- best_q95
  } else {
    # Joint rank avoids a Frankenstein comparator while remaining conservative:
    # one estimator must be selected before both gains are computed.
    score <- rank(bench_means, ties.method = "average") + rank(bench_q95, ties.method = "average")
    ties <- names(score)[score == min(score, na.rm = TRUE)]
    if (length(ties) > 1L) ties <- ties[order(bench_q95[ties], bench_means[ties])]
    best_joint <- ties[1]
  }

  list(best_joint = best_joint, best_mean = best_mean, best_q95 = best_q95,
       benchmark_candidate_set = paste(candidate_set, collapse = ";"),
       benchmark_fallback_used = policy %in% c("profile_preregistered", "preregistered", "profile_fixed") &&
         length(intersect(candidate_set, bench_cols)) == 0L,
       bench_mean = unname(bench_means[best_joint]), bench_q95 = unname(bench_q95[best_joint]))
}

.eb_boot_ci_adaptive <- function(rep_wide, col_a, col_b, boot_b, seed,
                                 stat = c("q95_diff", "mean_diff"),
                                 adaptive = TRUE, boot_extra = 500L, boot_max = 1500L,
                                 near_rel = 0.15) {
  stat <- match.arg(stat)
  dat <- rep_wide[, c(col_a, col_b), drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (!nrow(dat) || boot_b <= 1L || !(col_a %in% names(dat)) || !(col_b %in% names(dat))) {
    return(c(point = NA_real_, low = NA_real_, high = NA_real_, p_one_sided = NA_real_, B_used = 0))
  }
  stat_fun <- switch(
    stat,
    mean_diff = function(d) mean(d[[col_b]] - d[[col_a]], na.rm = TRUE),
    q95_diff = function(d) .eb_q95(d[[col_b]]) - .eb_q95(d[[col_a]])
  )
  point <- stat_fun(dat)
  boot_seed <- if (exists(".safe_seed", mode = "function", inherits = TRUE) &&
                   exists(".ensure_seed", mode = "function", inherits = TRUE)) {
    .ensure_seed(.safe_seed("boot_adaptive", seed))
  } else {
    suppressWarnings(as.integer(seed))
  }
  if (!is.finite(boot_seed) || boot_seed <= 0L) boot_seed <- 12345L

  run_boot <- function(B) {
    set.seed(boot_seed)
    boots <- replicate(B, {
      idx <- sample.int(nrow(dat), nrow(dat), replace = TRUE)
      stat_fun(dat[idx, , drop = FALSE])
    })
    c(
      point = unname(point),
      low = unname(stats::quantile(boots, 0.025, na.rm = TRUE, type = 8)),
      high = unname(stats::quantile(boots, 0.975, na.rm = TRUE, type = 8)),
      p_one_sided = mean(boots <= 0, na.rm = TRUE),
      B_used = B
    )
  }

  B <- min(as.integer(boot_b), as.integer(boot_max))
  out <- run_boot(B)
  if (isTRUE(adaptive) && is.finite(point) && point > 0) {
    repeat {
      near_zero <- is.finite(out["low"]) && abs(out["low"]) <= max(abs(point) * near_rel, .Machine$double.eps)
      unstable_p <- is.finite(out["p_one_sided"]) && out["p_one_sided"] > 0.01 && out["p_one_sided"] < 0.15
      if (!(near_zero || unstable_p) || B >= boot_max) break
      B2 <- min(B + as.integer(boot_extra), as.integer(boot_max))
      if (B2 <= B) break
      B <- B2
      out <- run_boot(B)
    }
  }
  out
}

.eb_decision_from_wide <- function(wide, bench_cols, boot_B, seed,
                                   benchmark_policy = "profile_preregistered",
                                   primary_metric = "q95",
                                   empirical_profile = "empirical_unknown",
                                   benchmark_map_string = "",
                                   adaptive_bootstrap = TRUE,
                                   boot_B_extra = 500L,
                                   boot_B_max = 1500L,
                                   near_ci_rel = 0.15,
                                   pareto_tolerance_rel = 0.005,
                                   strict_ci_contains_point = TRUE) {
  ch <- .eb_choose_joint_benchmark(wide, bench_cols, benchmark_policy, primary_metric,
                                  empirical_profile = empirical_profile,
                                  benchmark_map_string = benchmark_map_string)
  best <- ch$best_joint
  if (is.na(best) || !(best %in% names(wide))) {
    return(list(ok = FALSE, best_joint = NA_character_, best_mean = ch$best_mean, best_q95 = ch$best_q95,
                benchmark_candidate_set = ch$benchmark_candidate_set,
                benchmark_fallback_used = isTRUE(ch$benchmark_fallback_used),
                gain_mean = NA_real_, gain_q95 = NA_real_, gain_ew_q95 = NA_real_,
                ci = c(point = NA_real_, low = NA_real_, high = NA_real_, p_one_sided = NA_real_, B_used = 0),
                gate_point = FALSE, pareto_point = FALSE, ci_confirmed = FALSE, ci_contains_point = FALSE))
  }
  gain_mean <- mean(wide[[best]] - wide$ga_specialist, na.rm = TRUE)
  gain_q95 <- .eb_q95(wide[[best]]) - .eb_q95(wide$ga_specialist)
  gain_ew <- if ("equal_weight" %in% names(wide)) .eb_q95(wide$equal_weight) - .eb_q95(wide$ga_specialist) else NA_real_

  primary_metric <- tolower(as.character(primary_metric[1]))
  ci_stat <- if (identical(primary_metric, "mean")) "mean_diff" else "q95_diff"
  ci <- .eb_boot_ci_adaptive(wide, "ga_specialist", best, boot_B, seed,
                             stat = ci_stat, adaptive = adaptive_bootstrap,
                             boot_extra = boot_B_extra, boot_max = boot_B_max,
                             near_rel = near_ci_rel)
  point <- unname(ci["point"])
  ci_contains_point <- is.finite(ci["low"]) && is.finite(ci["high"]) &&
    is.finite(point) && ci["low"] <= point && ci["high"] >= point

  scale_ref <- max(abs(.eb_q95(wide[[best]])), abs(mean(wide[[best]], na.rm = TRUE)), 1e-12)
  tol <- abs(scale_ref) * pareto_tolerance_rel
  gate_point <- is.finite(gain_mean) && gain_mean > 0 && is.finite(gain_q95) && gain_q95 > 0
  pareto_point <- (is.finite(gain_mean) && gain_mean > 0 && is.finite(gain_q95) && gain_q95 >= -tol) ||
    (is.finite(gain_q95) && gain_q95 > 0 && is.finite(gain_mean) && gain_mean >= -tol)
  ci_confirmed <- isTRUE(gate_point) && is.finite(ci["low"]) && ci["low"] > 0 &&
    (!isTRUE(strict_ci_contains_point) || isTRUE(ci_contains_point))

  list(ok = TRUE, best_joint = best, best_mean = ch$best_mean, best_q95 = ch$best_q95,
       benchmark_candidate_set = ch$benchmark_candidate_set,
       benchmark_fallback_used = isTRUE(ch$benchmark_fallback_used),
       gain_mean = gain_mean, gain_q95 = gain_q95, gain_ew_q95 = gain_ew,
       ci = ci, ci_stat = ci_stat, ci_contains_point = ci_contains_point,
       gate_point = gate_point, pareto_point = pareto_point, ci_confirmed = ci_confirmed,
       pareto_tolerance_abs = tol)
}

.eb_should_add_targeted_R <- function(dec, targeted_extra_R = 0L, near_ci_rel = 0.15) {
  if (!is.finite(targeted_extra_R) || targeted_extra_R <= 0L || !isTRUE(dec$ok)) return(FALSE)
  point <- unname(dec$ci["point"])
  low <- unname(dec$ci["low"])
  if (!is.finite(point) || point <= 0) return(FALSE)
  isTRUE(dec$gate_point) || isTRUE(dec$pareto_point) ||
    (is.finite(low) && abs(low) <= max(abs(point) * near_ci_rel, .Machine$double.eps))
}

.eb_evidence_layers <- function(dec, profile_match, gain_ew, benchmark_layer = "primary_preregistered") {
  eligibility_layer <- if (isTRUE(profile_match)) "eligible" else "not_eligible_control"
  regime_layer <- if (isTRUE(profile_match)) "in_regime" else "cross_regime_audit"
  evidence_layer <- if (isTRUE(profile_match) && isTRUE(dec$ci_confirmed)) {
    "strong_win"
  } else if (isTRUE(profile_match) && isTRUE(dec$gate_point)) {
    "strong_point_signal"
  } else if (isTRUE(profile_match) && isTRUE(dec$pareto_point)) {
    "pareto_signal"
  } else if (is.finite(gain_ew) && gain_ew > 0) {
    "equal_weight_gain"
  } else {
    "no_win"
  }
  point <- suppressWarnings(as.numeric(dec$ci["point"]))
  low <- suppressWarnings(as.numeric(dec$ci["low"]))
  ci_layer <- if (isTRUE(dec$ci_confirmed)) {
    "ci_confirmed"
  } else if (is.finite(point) && point > 0 && is.finite(low) && low <= 0) {
    "near_ci"
  } else {
    "descriptive"
  }
  evidence_grade <- paste(regime_layer, benchmark_layer, evidence_layer, ci_layer, sep = "__")
  final_label <- paste(eligibility_layer, regime_layer, benchmark_layer, evidence_layer, ci_layer, sep = " / ")
  list(eligibility_layer = eligibility_layer, regime_layer = regime_layer,
       benchmark_layer = benchmark_layer, evidence_layer = evidence_layer,
       ci_layer = ci_layer, evidence_grade = evidence_grade, final_label = final_label)
}

.eb_grade_v2 <- function(dec, profile_match, gain_ew, strict_primary = TRUE) {
  .eb_evidence_layers(dec, profile_match, gain_ew)$evidence_grade
}

run_realworld_external_battery <- function(project_root = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
                                           output_root = Sys.getenv("REALWORLD_OUT_ROOT", unset = file.path(dirname(dirname(project_root)), "results")),
                                           run_label = Sys.getenv("EXTERNAL_RUN_LABEL", unset = "6_realworld_external_battery"),
                                           R = as.integer(Sys.getenv("EXTERNAL_R", unset = "500")),
                                           boot_B = as.integer(Sys.getenv("EXTERNAL_BOOT_B", unset = "500")),
                                           boot_B_extra = as.integer(Sys.getenv("EXTERNAL_BOOT_B_EXTRA", unset = "500")),
                                           boot_B_max = as.integer(Sys.getenv("EXTERNAL_BOOT_B_MAX", unset = "1500")),
                                           adaptive_bootstrap = .eb_bool(Sys.getenv("EXTERNAL_ADAPTIVE_BOOTSTRAP", unset = "TRUE"), TRUE),
                                           targeted_extra_R = as.integer(Sys.getenv("EXTERNAL_TARGETED_EXTRA_R", unset = "300")),
                                           profile_gate = .eb_bool(Sys.getenv("EXTERNAL_PROFILE_GATE", unset = "TRUE"), TRUE),
                                           keep_negative_controls = .eb_bool(Sys.getenv("EXTERNAL_KEEP_NEGATIVE_CONTROLS", unset = "FALSE"), FALSE),
                                           benchmark_policy = Sys.getenv("EXTERNAL_BENCHMARK_POLICY", unset = "profile_preregistered"),
                                           primary_metric = Sys.getenv("EXTERNAL_PRIMARY_METRIC", unset = "q95"),
                                           profile_benchmark_map = Sys.getenv("EXTERNAL_PROFILE_BENCHMARKS", unset = ""),
                                           write_oracle_audit = .eb_bool(Sys.getenv("EXTERNAL_WRITE_ORACLE_AUDIT", unset = "TRUE"), TRUE),
                                           oracle_boot_B = as.integer(Sys.getenv("EXTERNAL_ORACLE_BOOT_B", unset = "0")),
                                           csv_max_rows = as.integer(Sys.getenv("EXTERNAL_CSV_MAX_ROWS", unset = "5000")),
                                           pareto_tolerance_rel = as.numeric(Sys.getenv("EXTERNAL_PARETO_TOL_REL", unset = "0.005")),
                                           near_ci_rel = as.numeric(Sys.getenv("EXTERNAL_NEAR_CI_REL", unset = "0.15")),
                                           strict_ci_contains_point = .eb_bool(Sys.getenv("EXTERNAL_STRICT_CI_CONTAINS_POINT", unset = "TRUE"), TRUE),
                                           contamination_mode = Sys.getenv("EXTERNAL_CONTAMINATION_MODE", unset = "endogenous"),
                                           natural_z_threshold = as.numeric(Sys.getenv("EXTERNAL_NATURAL_Z", unset = "3")),
                                           max_datasets = as.integer(Sys.getenv("EXTERNAL_MAX_DATASETS", unset = "0")),
                                           target_loaded = as.integer(Sys.getenv("EXTERNAL_TARGET_LOADED", unset = "120")),
                                           enable_dataset_balance = .eb_bool(Sys.getenv("EXTERNAL_ENABLE_DATASET_BALANCE", unset = "TRUE"), TRUE),
                                           profile_quota_string = Sys.getenv("EXTERNAL_PROFILE_QUOTAS", unset = ""),
                                           source_cap_prop = as.numeric(Sys.getenv("EXTERNAL_SOURCE_CAP_PROP", unset = "0.25")),
                                           wins_saturation_cap = as.integer(Sys.getenv("EXTERNAL_WINS_SATURATION_CAP", unset = "10")),
                                           enable_futility_stop = .eb_bool(Sys.getenv("EXTERNAL_ENABLE_FUTILITY_STOP", unset = "TRUE"), TRUE),
                                           futility_min_gate_rows = as.integer(Sys.getenv("EXTERNAL_FUTILITY_MIN_GATE_ROWS", unset = "12")),
                                           enable_depth_probe = .eb_bool(Sys.getenv("EXTERNAL_ENABLE_DEPTH_PROBE", unset = "TRUE"), TRUE),
                                           depth_trigger_wins = as.integer(Sys.getenv("EXTERNAL_DEPTH_TRIGGER_WINS", unset = "3")),
                                           depth_max_variants = as.integer(Sys.getenv("EXTERNAL_DEPTH_MAX_VARIANTS", unset = "3")),
                                           depth_sample_frac = as.numeric(Sys.getenv("EXTERNAL_DEPTH_SAMPLE_FRAC", unset = "1.0")),
                                           fdr_alpha_primary = as.numeric(Sys.getenv("EXTERNAL_FDR_ALPHA_PRIMARY", unset = "0.05")),
                                           fdr_alpha_secondary = as.numeric(Sys.getenv("EXTERNAL_FDR_ALPHA_SECONDARY", unset = "0.10")),
                                           generate_pdf_report = .eb_bool(Sys.getenv("EXTERNAL_GENERATE_PDF_REPORT", unset = "TRUE"), TRUE),
                                           clean_output_layout = .eb_bool(Sys.getenv("EXTERNAL_CLEAN_OUTPUT_LAYOUT", unset = "TRUE"), TRUE),
                                           seed = as.integer(Sys.getenv("EXTERNAL_SEED", unset = "20260614"))) {
  t0 <- Sys.time()
  if (!exists("ESTIMATOR_REGISTRY", inherits = TRUE) ||
      !exists(".load_ds_wine", mode = "function", inherits = TRUE)) {
    rw_load_modules(project_root)
  }
  results_root <- normalizePath(
    Sys.getenv("VALIDATION_RESULTS_ROOT", unset = dirname(dirname(project_root))),
    winslash = "/",
    mustWork = FALSE
  )
  out_dir <- file.path(output_root, rw_safe_label(run_label))
  basic_dir <- file.path(out_dir, "01_results_basic")
  report_dir <- file.path(out_dir, "02_reports")
  audit_dir <- file.path(out_dir, "03_audit_ready")
  discarded_dir <- file.path(out_dir, "98_discarded_nonfinal")
  recovery_dir <- file.path(out_dir, "99_recovery_logs")
  dl_dir <- file.path(recovery_dir, "downloads")
  for (d0 in c(out_dir, basic_dir, report_dir, audit_dir, discarded_dir, recovery_dir, dl_dir, file.path(report_dir, "figures"))) {
    dir.create(d0, recursive = TRUE, showWarnings = FALSE)
  }
  profile_gate_policy_text <- paste(
    "Pre-registered empirical profile gate:",
    "bounded/proportion by hint/domain; zero-inflated/count by hint/domain;",
    "normal_like when abs(skew)<=0.50 and excess_kurtosis<=1.00;",
    "heavy_tail_symmetric when abs(skew)<=1.00 and excess_kurtosis>3.00;",
    "lognormal_like when skew>1.00 with positive support; weibull_like when skew>0.50 with positive support;",
    "otherwise empirical_unknown.")
  fdr_policy_text <- paste0(
    "Benjamini-Hochberg FDR is computed at dataset level on external_dataset_level_decisions.csv ",
    "for primary eligible in-regime rows only. Primary alpha=", fdr_alpha_primary,
    "; secondary sensitivity alpha=", fdr_alpha_secondary, ".")
  output_layout_policy_text <- paste(
    "01_results_basic contains compact final results; 02_reports contains generated report files;",
    "03_audit_ready contains full provenance, decisions, oracle audit, and split CSVs;",
    "98_discarded_nonfinal is reserved for obsolete or nonfinal byproducts;",
    "99_recovery_logs contains downloads, checkpoints, temporary recovery outputs, and logs.")

  specialists <- rw_load_specialists(results_root)
  specs <- eb_specs(dl_dir)
  if (is.finite(max_datasets) && max_datasets > 0L) specs <- specs[seq_len(min(max_datasets, length(specs)))]
  profile_quota <- .eb_parse_named_ints(profile_quota_string, .eb_default_profile_quota(target_loaded))
  profile_counts_env <- new.env(parent = emptyenv())
  source_counts_env <- new.env(parent = emptyenv())
  depth_probe_specs_added <- 0L

  registry_rows <- list()
  metrics_rows <- list()
  gate_rows <- list()
  oracle_rows <- list()
  raw_i <- 0L
  eb_write_checkpoint <- function(label = "partial") {
    # Checkpoints are written after each dataset/specialist block so the audit
    # trail can be recovered if a long external run stops before final export.
    registry_df <- if (length(registry_rows)) do.call(rbind, registry_rows) else data.frame()
    metrics_df <- if (length(metrics_rows)) do.call(rbind, metrics_rows) else data.frame()
    gate_df <- if (length(gate_rows)) do.call(rbind, gate_rows) else data.frame()
    oracle_df <- if (length(oracle_rows)) do.call(rbind, oracle_rows) else data.frame()
    audit_cols <- c("dataset_id", "parent_dataset_id", "domain", "source_url", "accessed_at",
                    "source_file", "source_md5", "loader", "variable", "transform",
                    "subset_rule", "family_hint", "n",
                    "skewness", "excess_kurtosis", "support_sign", "theta_star",
                    "load_ok", "load_error", "notes")
    if (nrow(registry_df)) {
      audit_df <- registry_df[, intersect(audit_cols, names(registry_df)), drop = FALSE]
      if (nrow(metrics_df) && nrow(audit_df) && !"source_url" %in% names(metrics_df)) {
        metrics_df <- merge(audit_df, metrics_df, by = "dataset_id", all.y = TRUE, sort = FALSE)
        metrics_df <- eb_fix_merged_columns(metrics_df)
      }
      if (nrow(gate_df) && nrow(audit_df) && !"source_url" %in% names(gate_df)) {
        gate_df <- merge(audit_df, gate_df, by = "dataset_id", all.y = TRUE, sort = FALSE)
        gate_df <- eb_fix_merged_columns(gate_df)
      }
      if (nrow(oracle_df) && nrow(audit_df) && !"source_url" %in% names(oracle_df)) {
        oracle_df <- merge(audit_df, oracle_df, by = "dataset_id", all.y = TRUE, sort = FALSE)
        oracle_df <- eb_fix_merged_columns(oracle_df)
      }
    }
    prefix <- paste0("checkpoint_", eb_safe(label), "_")
    eb_write_csv_split(registry_df, file.path(recovery_dir, paste0(prefix, "dataset_registry.csv")), csv_max_rows)
    eb_write_csv_split(metrics_df, file.path(recovery_dir, paste0(prefix, "condition_metrics.csv")), csv_max_rows)
    eb_write_csv_split(gate_df, file.path(recovery_dir, paste0(prefix, "gate_decisions.csv")), csv_max_rows)
    eb_write_csv_split(oracle_df, file.path(recovery_dir, paste0(prefix, "oracle_audit.csv")), csv_max_rows)
    progress <- data.frame(
      checkpoint_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"),
      label = label,
      registry_rows = nrow(registry_df),
      metric_rows = nrow(metrics_df),
      gate_rows = nrow(gate_df),
      oracle_rows = nrow(oracle_df),
      replicate_estimator_rows = raw_i,
      stringsAsFactors = FALSE
    )
    rw_write_csv(progress, file.path(recovery_dir, "CHECKPOINT_PROGRESS.csv"))
    invisible(TRUE)
  }

  spec_i <- 0L
  while (spec_i < length(specs)) {
    spec_i <- spec_i + 1L
    spc <- specs[[spec_i]]
    role <- as.character(spc$analysis_role %||% "primary_dataset")[1]
    selected_primary_so_far <- sum(vapply(registry_rows, function(z) {
      isTRUE(z$selected_for_primary[1]) && identical(as.character(z$analysis_role[1]), "primary_dataset")
    }, logical(1)))
    if (identical(role, "primary_dataset") && is.finite(target_loaded) && target_loaded > 0L && selected_primary_so_far >= target_loaded) {
      eb_cat("[EB] Target selected primary datasets reached: %d", selected_primary_so_far)
      break
    }
    eb_cat("[EB] Loading %s [%s]", spc$id, role)
    loaded <- eb_load_one(spc, dl_dir)
    d <- if (isTRUE(loaded$ok)) rw_diag(loaded$x) else list(n = length(loaded$x), mean = NA_real_,
                                                            skew = NA_real_, kurt = NA_real_,
                                                            support_sign = "failed")
    empirical_profile <- if (isTRUE(loaded$ok)) {
      .eb_empirical_profile(spc$family_hint, spc$domain, d$skew, d$kurt, d$support_sign)
    } else "load_failed"
    selection <- if (isTRUE(loaded$ok)) {
      .eb_selection_decision(spc, empirical_profile, role,
                             profile_counts_env, source_counts_env,
                             target_loaded = target_loaded,
                             profile_quota = profile_quota,
                             source_cap_prop = source_cap_prop,
                             enable_balance = enable_dataset_balance)
    } else list(selected = FALSE, status = "load_failed", reason = loaded$error, source_group = .eb_source_group(spc))

    selected_for_primary <- isTRUE(selection$selected) && identical(role, "primary_dataset")
    selected_for_evaluation <- isTRUE(selection$selected)
    if (isTRUE(selected_for_primary)) {
      .eb_inc_count(profile_counts_env, empirical_profile)
      .eb_inc_count(source_counts_env, selection$source_group)
    }
    registry_rows[[length(registry_rows) + 1L]] <- data.frame(
      dataset_id = spc$id, parent_dataset_id = spc$parent_dataset_id %||% spc$id,
      analysis_role = role, selected_for_primary = selected_for_primary,
      selected_for_evaluation = selected_for_evaluation,
      selection_status = selection$status, selection_reason = selection$reason,
      source_group = selection$source_group, empirical_profile = empirical_profile,
      domain = spc$domain,
      source_url = spc$source_url, accessed_at = loaded$accessed_at,
      source_file = loaded$path, source_md5 = loaded$md5, loader = spc$loader,
      variable = loaded$variable %||% spc$variable,
      transform = loaded$transform %||% spc$transform,
      subset_rule = spc$subset_rule,
      family_hint = spc$family_hint, n = d$n, skewness = d$skew,
      excess_kurtosis = d$kurt, support_sign = d$support_sign,
      theta_star = d$mean, load_ok = isTRUE(loaded$ok),
      load_error = loaded$error, notes = spc$notes, stringsAsFactors = FALSE
    )
    if (!isTRUE(loaded$ok) || !isTRUE(selected_for_evaluation)) {
      eb_cat("[EB] Skip %s: %s", spc$id, selection$reason)
      eb_write_checkpoint(paste0("after_", spc$id))
      next
    }

    ds <- list(id = spc$id, parent_id = spc$parent_dataset_id %||% spc$id,
               x = loaded$x, family = spc$family_hint,
               variable = loaded$variable %||% spc$variable,
               transform = loaded$transform %||% spc$transform,
               analysis_role = role)
    support_restricted <- any(ds$x <= 0) || grepl("returns", spc$domain)

    # V5 breadth-depth stopping state. These counters apply to the primary
    # dataset only. Depth probes inherit the parent dataset id and are written
    # as secondary evidence, never as new independent N.
    wins_cap <- suppressWarnings(as.integer(wins_saturation_cap))
    if (!is.finite(wins_cap) || wins_cap <= 0L) wins_cap <- Inf
    futility_min <- suppressWarnings(as.integer(futility_min_gate_rows))
    if (!is.finite(futility_min) || futility_min <= 0L) futility_min <- Inf
    dataset_gate_count <- 0L
    dataset_positive_signal_count <- 0L
    dataset_near_ci_count <- 0L
    dataset_uncapped_strong_wins <- 0L
    dataset_countable_strong_wins <- 0L
    dataset_saturated <- FALSE
    dataset_stop <- FALSE
    dataset_stop_reason <- NA_character_

    for (sp in specialists) {
      if (isTRUE(dataset_stop)) break
      profile_match_current <- .eb_profile_match(sp$family, spc$family_hint, spc$domain,
                                                 d$skew, d$kurt, d$support_sign)
      if (isTRUE(profile_gate) && !isTRUE(profile_match_current) && !isTRUE(keep_negative_controls)) {
        eb_cat("[EB] Skip %s / %s: profile gate (%s specialist vs %s dataset)",
               ds$id, sp$specialist_id, sp$family, empirical_profile)
        next
      }
      for (mode in c("original_regime", "locked_unseen_similar")) {
        if (isTRUE(dataset_stop)) break
        conds <- rw_conditions(sp, mode)
        if (!nrow(conds)) next
        all_rep <- list()
        for (ci in seq_len(nrow(conds))) {
          cond <- conds[ci, , drop = FALSE]
          w <- sp$weights
          if (support_restricted) {
            w[ESTIMATOR_NAMES %in% c("harmonic", "geometric")] <- 0
            w <- .normalize_simplex(w)
          }
          allowed <- rep(TRUE, length(w))
          names(allowed) <- ESTIMATOR_NAMES
          if (support_restricted) allowed[ESTIMATOR_NAMES %in% c("harmonic", "geometric")] <- FALSE
          ew <- as.numeric(allowed) / sum(allowed)
          names(ew) <- ESTIMATOR_NAMES
          m_requested <- as.integer(cond$sample_size[1])
          m_effective <- min(m_requested, length(ds$x) - 1L)
          rep_seed <- .safe_seed(seed, ds$id, sp$specialist_id, mode, m_requested, ci)
          rep_df <- rw_eval_one(ds$x, d$mean, w, ew, m_requested, cond, R, rep_seed, ds$family,
                                contamination_mode = contamination_mode,
                                natural_z_threshold = natural_z_threshold)
          rep_df$dataset_id <- ds$id
          rep_df$specialist_id <- sp$specialist_id
          rep_df$mode <- mode
          rep_df$m_requested <- m_requested
          rep_df$m_effective <- m_effective
          rep_df$subsample_R <- R
          rep_df$paired_bootstrap_B <- boot_B
          rep_df$condition_id <- ci
          rep_df$contamination_rate <- cond$contamination_rate[1]
          rep_df$outlier_scale_mad <- cond$outlier_scale_mad[1]
          rep_df$contamination_type <- as.character(cond$contamination_type[1])
          rep_df$contam_condition <- paste(cond$contamination_rate, cond$outlier_scale_mad,
                                           cond$contamination_type, sep = "|")
          rep_df$profile_match <- profile_match_current
          rep_df$empirical_profile <- empirical_profile
          rep_df$targeted_extra_R_used <- 0L

          # Targeted refinement: add empirical replicates only when the
          # preliminary signal is near the decision boundary. This narrows
          # uncertainty without changing discovery weights or candidate labels.
          blocked_bench_pre <- if (support_restricted) c("harmonic", "geometric") else character(0)
          wide_pre <- .eb_make_wide(rep_df, ds$id, sp$specialist_id, mode, m_effective, rep_df$contam_condition[1])
          bench_cols_pre <- setdiff(intersect(ESTIMATOR_NAMES, names(wide_pre)), blocked_bench_pre)
          dec_pre <- .eb_decision_from_wide(wide_pre, bench_cols_pre, boot_B,
                                            .safe_seed(seed, "pre", ds$id, sp$specialist_id, mode, m_requested, ci),
                                            benchmark_policy = benchmark_policy,
                                            primary_metric = primary_metric,
                                            empirical_profile = empirical_profile,
                                            benchmark_map_string = profile_benchmark_map,
                                            adaptive_bootstrap = adaptive_bootstrap,
                                            boot_B_extra = boot_B_extra,
                                            boot_B_max = boot_B_max,
                                            near_ci_rel = near_ci_rel,
                                            pareto_tolerance_rel = pareto_tolerance_rel,
                                            strict_ci_contains_point = strict_ci_contains_point)
          if (.eb_should_add_targeted_R(dec_pre, targeted_extra_R, near_ci_rel)) {
            extra_seed <- .safe_seed(seed, "targeted_extra_R", ds$id, sp$specialist_id, mode, m_requested, ci)
            extra_df <- rw_eval_one(ds$x, d$mean, w, ew, m_requested, cond, targeted_extra_R, extra_seed, ds$family,
                                    contamination_mode = contamination_mode,
                                    natural_z_threshold = natural_z_threshold)
            extra_df$replicate <- extra_df$replicate + max(rep_df$replicate, na.rm = TRUE)
            extra_df$dataset_id <- ds$id
            extra_df$specialist_id <- sp$specialist_id
            extra_df$mode <- mode
            extra_df$m_requested <- m_requested
            extra_df$m_effective <- m_effective
            extra_df$subsample_R <- R + targeted_extra_R
            extra_df$paired_bootstrap_B <- boot_B
            extra_df$condition_id <- ci
            extra_df$contamination_rate <- cond$contamination_rate[1]
            extra_df$outlier_scale_mad <- cond$outlier_scale_mad[1]
            extra_df$contamination_type <- as.character(cond$contamination_type[1])
            extra_df$contam_condition <- rep_df$contam_condition[1]
            extra_df$profile_match <- profile_match_current
            extra_df$empirical_profile <- empirical_profile
            extra_df$targeted_extra_R_used <- targeted_extra_R
            rep_df$subsample_R <- R + targeted_extra_R
            rep_df$targeted_extra_R_used <- targeted_extra_R
            rep_df <- rbind(rep_df, extra_df)
          }
          all_rep[[length(all_rep) + 1L]] <- rep_df
        }
        reps <- do.call(rbind, all_rep)
        mode_n_reps <- length(unique(paste(reps$m_effective, reps$contam_condition, reps$replicate, sep = "|")))
        agg <- aggregate(sq_error ~ dataset_id + specialist_id + mode + condition_id +
                           contamination_rate + outlier_scale_mad + contamination_type +
                           m_requested + m_effective + contam_condition +
                           contamination_mode + natural_z_threshold +
                           subsample_R + paired_bootstrap_B + estimator,
                         reps, function(z) c(mean_mse = mean(z),
                                             q95_mse = unname(stats::quantile(z, 0.95, type = 8)),
                                             bias = NA_real_, n = length(z)))
        vals <- as.data.frame(agg$sq_error)
        names(vals) <- c("mean_mse", "q95_mse", "bias", "n_replicates")
        diag_agg <- aggregate(cbind(requested_tail_n, empirical_tail_n, natural_pool_n, natural_body_n) ~
                                dataset_id + specialist_id + mode + condition_id +
                                contamination_rate + outlier_scale_mad + contamination_type +
                                m_requested + m_effective + contam_condition +
                                contamination_mode + natural_z_threshold +
                                subsample_R + paired_bootstrap_B + estimator,
                              reps, function(z) mean(z, na.rm = TRUE))
        names(diag_agg)[names(diag_agg) == "requested_tail_n"] <- "mean_requested_tail_n"
        names(diag_agg)[names(diag_agg) == "empirical_tail_n"] <- "mean_empirical_tail_n"
        names(diag_agg)[names(diag_agg) == "natural_pool_n"] <- "mean_natural_pool_n"
        names(diag_agg)[names(diag_agg) == "natural_body_n"] <- "mean_natural_body_n"
        metrics_rows[[length(metrics_rows) + 1L]] <- merge(cbind(agg[setdiff(names(agg), "sq_error")], vals),
                                                           diag_agg,
                                                           by = c("dataset_id", "specialist_id", "mode",
                                                                  "condition_id", "contamination_rate",
                                                                  "outlier_scale_mad", "contamination_type",
                                                                  "m_requested", "m_effective", "contam_condition",
                                                                  "contamination_mode", "natural_z_threshold",
                                                                  "subsample_R", "paired_bootstrap_B", "estimator"),
                                                           all.x = TRUE, sort = FALSE)

        cond_key <- paste(reps$condition_id, reps$m_requested, reps$m_effective, reps$contam_condition, sep = "|")
        for (ck in unique(cond_key)) {
          zr <- reps[cond_key == ck, , drop = FALSE]
          rep_key <- paste(zr$dataset_id, zr$specialist_id, zr$mode,
                           zr$m_effective, zr$contam_condition, zr$replicate, sep = "|")
          wide <- reshape(data.frame(key = rep_key, estimator = zr$estimator, sq_error = zr$sq_error),
                          idvar = "key", timevar = "estimator", direction = "wide")
          names(wide) <- sub("^sq_error\\.", "", names(wide))
          blocked_bench <- if (support_restricted) c("harmonic", "geometric") else character(0)
          bench_cols <- setdiff(intersect(ESTIMATOR_NAMES, names(wide)), blocked_bench)
          dec <- .eb_decision_from_wide(wide, bench_cols, boot_B,
                                        .safe_seed(seed, "boot", ds$id, sp$specialist_id, mode, ck),
                                        benchmark_policy = benchmark_policy,
                                        primary_metric = primary_metric,
                                        empirical_profile = empirical_profile,
                                        benchmark_map_string = profile_benchmark_map,
                                        adaptive_bootstrap = adaptive_bootstrap,
                                        boot_B_extra = boot_B_extra,
                                        boot_B_max = boot_B_max,
                                        near_ci_rel = near_ci_rel,
                                        pareto_tolerance_rel = pareto_tolerance_rel,
                                        strict_ci_contains_point = strict_ci_contains_point)
          best_mean <- dec$best_mean
          best_q95 <- dec$best_q95
          best_joint <- dec$best_joint
          gain_mean <- dec$gain_mean
          gain_q95 <- dec$gain_q95
          gain_ew <- dec$gain_ew_q95
          ci <- dec$ci
          ci_confirmed <- isTRUE(dec$ci_confirmed)
          gate <- isTRUE(dec$gate_point)
          pareto_gate <- isTRUE(dec$pareto_point)
          layers <- .eb_evidence_layers(dec, profile_match_current, gain_ew,
                                        benchmark_layer = "primary_preregistered")
          grade <- layers$evidence_grade

          dataset_gate_count <- dataset_gate_count + 1L
          primary_countable_context <- identical(role, "primary_dataset") &&
            isTRUE(profile_match_current) && identical(layers$benchmark_layer, "primary_preregistered")
          is_strong_primary_win <- isTRUE(primary_countable_context) &&
            identical(layers$evidence_layer, "strong_win")
          if (isTRUE(is_strong_primary_win)) {
            dataset_uncapped_strong_wins <- dataset_uncapped_strong_wins + 1L
          }
          countable_primary_win <- isTRUE(is_strong_primary_win) &&
            dataset_countable_strong_wins < wins_cap
          if (isTRUE(countable_primary_win)) {
            dataset_countable_strong_wins <- dataset_countable_strong_wins + 1L
          }
          if (isTRUE(is_strong_primary_win) && dataset_countable_strong_wins >= wins_cap) {
            dataset_saturated <- TRUE
          }
          is_positive_signal <- isTRUE(dec$gate_point) || isTRUE(dec$pareto_point) ||
            (is.finite(gain_ew) && gain_ew > 0)
          if (isTRUE(primary_countable_context) && isTRUE(is_positive_signal)) {
            dataset_positive_signal_count <- dataset_positive_signal_count + 1L
          }
          if (isTRUE(primary_countable_context) && identical(layers$ci_layer, "near_ci")) {
            dataset_near_ci_count <- dataset_near_ci_count + 1L
          }

          dec_oracle <- NULL
          if (isTRUE(write_oracle_audit)) {
            dec_oracle <- .eb_decision_from_wide(wide, bench_cols, oracle_boot_B,
                                                 .safe_seed(seed, "oracle", ds$id, sp$specialist_id, mode, ck),
                                                 benchmark_policy = "joint_rank",
                                                 primary_metric = primary_metric,
                                                 empirical_profile = empirical_profile,
                                                 benchmark_map_string = profile_benchmark_map,
                                                 adaptive_bootstrap = FALSE,
                                                 boot_B_extra = 0L,
                                                 boot_B_max = max(0L, oracle_boot_B),
                                                 near_ci_rel = near_ci_rel,
                                                 pareto_tolerance_rel = pareto_tolerance_rel,
                                                 strict_ci_contains_point = FALSE)
            oracle_rows[[length(oracle_rows) + 1L]] <- data.frame(
              dataset_id = ds$id, domain = spc$domain, variable = ds$variable,
              transform = ds$transform, family_hint = spc$family_hint,
              specialist_id = sp$specialist_id, phase = sp$phase, specialist_family = sp$family,
              taxonomy_evidence_grade = sp$evidence_grade %||% NA_character_,
              taxonomy_validation_class = sp$validation_class %||% NA_character_,
              taxonomy_legacy_final_decision = sp$legacy_final_decision %||% NA_character_,
              taxonomy_source_file = sp$taxonomy_file %||% NA_character_,
              taxonomy_source_md5 = sp$taxonomy_md5 %||% NA_character_,
              source_seed = sp$source_seed, mode = mode, condition_id = zr$condition_id[1],
              empirical_profile = empirical_profile, profile_match = profile_match_current,
              benchmark_layer = "secondary_oracle_audit", benchmark_policy = "joint_rank",
              primary_metric = primary_metric, best_benchmark_joint = dec_oracle$best_joint,
              gain_vs_oracle_benchmark_mean = dec_oracle$gain_mean,
              gain_vs_oracle_benchmark_q95 = dec_oracle$gain_q95,
              oracle_gate_point = isTRUE(dec_oracle$gate_point),
              oracle_pareto_point = isTRUE(dec_oracle$pareto_point),
              oracle_ci_confirmed = isTRUE(dec_oracle$ci_confirmed),
              oracle_bootstrap_B_used = as.integer(unname(dec_oracle$ci["B_used"])),
              contamination_rate = zr$contamination_rate[1],
              outlier_scale_mad = zr$outlier_scale_mad[1],
              contamination_type = zr$contamination_type[1],
              m_requested = zr$m_requested[1], m_effective = zr$m_effective[1],
              contam_condition = zr$contam_condition[1],
              stringsAsFactors = FALSE
            )
          }

          gate_rows[[length(gate_rows) + 1L]] <- data.frame(
            dataset_id = ds$id, domain = spc$domain, variable = ds$variable,
            transform = ds$transform, family_hint = spc$family_hint,
            specialist_id = sp$specialist_id, phase = sp$phase, specialist_family = sp$family,
            taxonomy_evidence_grade = sp$evidence_grade %||% NA_character_,
            taxonomy_validation_class = sp$validation_class %||% NA_character_,
            taxonomy_legacy_final_decision = sp$legacy_final_decision %||% NA_character_,
            taxonomy_source_file = sp$taxonomy_file %||% NA_character_,
            taxonomy_source_md5 = sp$taxonomy_md5 %||% NA_character_,
            source_seed = sp$source_seed, mode = mode,
            condition_id = zr$condition_id[1],
            contamination_rate = zr$contamination_rate[1],
            outlier_scale_mad = zr$outlier_scale_mad[1],
            contamination_type = zr$contamination_type[1],
            m_requested = zr$m_requested[1], m_effective = zr$m_effective[1],
            contam_condition = zr$contam_condition[1],
            empirical_profile = empirical_profile,
            profile_match = profile_match_current,
            eligibility_layer = layers$eligibility_layer,
            regime_layer = layers$regime_layer,
            benchmark_layer = layers$benchmark_layer,
            evidence_layer = layers$evidence_layer,
            ci_layer = layers$ci_layer,
            final_evidence_label = layers$final_label,
            analysis_role = role,
            parent_dataset_id = ds$parent_id,
            countable_primary_win = countable_primary_win,
            dataset_primary_win_index = if (isTRUE(countable_primary_win)) dataset_countable_strong_wins else NA_integer_,
            dataset_uncapped_strong_win_index = if (isTRUE(is_strong_primary_win)) dataset_uncapped_strong_wins else NA_integer_,
            dataset_saturated = dataset_saturated,
            dataset_stop_reason = dataset_stop_reason,
            dataset_gate_row_index = dataset_gate_count,
            benchmark_policy = benchmark_policy,
            preregistered_benchmark_candidates = dec$benchmark_candidate_set,
            preregistered_benchmark_fallback_used = isTRUE(dec$benchmark_fallback_used),
            primary_metric = primary_metric,
            best_benchmark_mean = best_mean, best_benchmark_q95 = best_q95,
            best_benchmark_joint = best_joint,
            gain_vs_best_benchmark_mean = gain_mean,
            gain_vs_best_benchmark_q95 = gain_q95,
            gain_vs_joint_benchmark_mean = gain_mean,
            gain_vs_joint_benchmark_q95 = gain_q95,
            gain_vs_equalweight_q95 = gain_ew,
            oracle_best_benchmark_joint = if (!is.null(dec_oracle)) dec_oracle$best_joint else NA_character_,
            gain_vs_oracle_benchmark_mean = if (!is.null(dec_oracle)) dec_oracle$gain_mean else NA_real_,
            gain_vs_oracle_benchmark_q95 = if (!is.null(dec_oracle)) dec_oracle$gain_q95 else NA_real_,
            oracle_gate_point = if (!is.null(dec_oracle)) isTRUE(dec_oracle$gate_point) else NA,
            ci_point = unname(ci["point"]),
            ci_low = unname(ci["low"]), ci_high = unname(ci["high"]),
            ci_contains_point = isTRUE(dec$ci_contains_point),
            ci_confirmed = ci_confirmed, gate_pass = gate,
            pareto_gate_pass = pareto_gate,
            evidence_grade = grade,
            bootstrap_p_one_sided = unname(ci["p_one_sided"]),
            bootstrap_B_used = as.integer(unname(ci["B_used"])),
            ci_stat = dec$ci_stat,
            ci_method = "paired_bootstrap_aligned_joint_benchmark_adaptive",
            subsample_R = length(unique(zr$replicate)), paired_bootstrap_B = boot_B,
            targeted_extra_R_used = max(zr$targeted_extra_R_used, na.rm = TRUE),
            n_replicates = length(unique(zr$replicate)),
            contamination_mode = contamination_mode, natural_z_threshold = natural_z_threshold,
            mean_requested_tail_n = mean(zr$requested_tail_n, na.rm = TRUE),
            mean_empirical_tail_n = mean(zr$empirical_tail_n, na.rm = TRUE),
            mean_natural_pool_n = mean(zr$natural_pool_n, na.rm = TRUE),
            support_restricted = support_restricted, stringsAsFactors = FALSE
          )

          # Pre-registered stopping rules for efficient evaluation. They do not
          # delete evidence: every evaluated row remains in the audit tables.
          if (identical(role, "primary_dataset") && isTRUE(dataset_saturated)) {
            dataset_stop <- TRUE
            dataset_stop_reason <- "saturation_cap_reached"
          }
          if (identical(role, "primary_dataset") && isTRUE(enable_futility_stop) &&
              dataset_gate_count >= futility_min &&
              dataset_countable_strong_wins == 0L &&
              dataset_positive_signal_count == 0L &&
              dataset_near_ci_count == 0L) {
            dataset_stop <- TRUE
            dataset_stop_reason <- "futility_stop_no_positive_or_near_ci_signal"
          }
          if (isTRUE(dataset_stop)) break
        }
        raw_i <- raw_i + nrow(reps)
        eb_cat("[EB] %s / %s / %s complete (%d replicate-estimator rows)",
               ds$id, sp$specialist_id, mode, raw_i)
        eb_write_checkpoint(paste0("after_", ds$id, "_", sp$specialist_id, "_", mode))
        if (isTRUE(dataset_stop)) {
          eb_cat("[EB] Stop %s: %s; countable strong wins=%s; gate rows=%s",
                 ds$id, dataset_stop_reason, dataset_countable_strong_wins, dataset_gate_count)
          break
        }
      }
      if (isTRUE(dataset_stop)) break
    }

    # V5 secondary depth probes: triggered only after a primary dataset shows
    # enough in-regime signal. These probes deepen the same dataset and are
    # explicitly excluded from independent dataset counts.
    if (identical(role, "primary_dataset") && isTRUE(enable_depth_probe) &&
        isTRUE(selected_for_primary) && dataset_countable_strong_wins >= depth_trigger_wins) {
      depth_specs <- .eb_depth_probe_specs(spc, ds$x,
                                           seed = .safe_seed(seed, "depth_probe", ds$id),
                                           max_variants = depth_max_variants,
                                           sample_frac = depth_sample_frac)
      if (length(depth_specs)) {
        specs <- c(specs, depth_specs)
        depth_probe_specs_added <- depth_probe_specs_added + length(depth_specs)
        eb_cat("[EB] Queued %d secondary depth probes for %s", length(depth_specs), ds$id)
      }
    }
  }

  registry_df <- do.call(rbind, registry_rows)
  metrics_df <- if (length(metrics_rows)) do.call(rbind, metrics_rows) else data.frame()
  gate_df <- if (length(gate_rows)) do.call(rbind, gate_rows) else data.frame()
  oracle_df <- if (length(oracle_rows)) do.call(rbind, oracle_rows) else data.frame()
  # Final tables join provenance metadata with condition metrics and gate
  # decisions, so reported gains can be traced back to the public source files.
  audit_cols <- c("dataset_id", "parent_dataset_id", "domain", "source_url", "accessed_at",
                  "source_file", "source_md5", "loader", "variable", "transform",
                  "subset_rule", "family_hint", "n",
                  "skewness", "excess_kurtosis", "support_sign", "theta_star",
                  "load_ok", "load_error", "notes")
  audit_df <- registry_df[, intersect(audit_cols, names(registry_df)), drop = FALSE]
  if (nrow(metrics_df) && nrow(audit_df)) {
    metrics_df <- merge(audit_df, metrics_df, by = "dataset_id", all.y = TRUE, sort = FALSE)
    metrics_df <- eb_fix_merged_columns(metrics_df)
  }
  if (nrow(gate_df) && nrow(audit_df)) {
    gate_df <- merge(audit_df, gate_df, by = "dataset_id", all.y = TRUE, sort = FALSE)
    gate_df <- eb_fix_merged_columns(gate_df)
  }
  if (nrow(oracle_df) && nrow(audit_df)) {
    oracle_df <- merge(audit_df, oracle_df, by = "dataset_id", all.y = TRUE, sort = FALSE)
    oracle_df <- eb_fix_merged_columns(oracle_df)
  }
  for (nm in c("registry_df", "metrics_df", "gate_df", "oracle_df")) {
    df0 <- get(nm)
    if (is.data.frame(df0) && nrow(df0)) {
      df0$preregistration_version <- "V5_audit_ready"
      df0$profile_gate_enabled <- profile_gate
      df0$profile_gate_policy <- profile_gate_policy_text
      df0$benchmark_policy_primary <- benchmark_policy
      df0$oracle_audit_secondary_only <- write_oracle_audit
      df0$paired_ci_policy <- "paired bootstrap aligned to the same reported gain statistic"
      df0$strict_ci_contains_point_policy <- strict_ci_contains_point
      df0$fdr_policy <- fdr_policy_text
      df0$output_layout_policy <- output_layout_policy_text
      df0$pareto_tolerance_relative <- pareto_tolerance_rel
      df0$wins_saturation_cap <- wins_saturation_cap
      df0$futility_stop_enabled <- enable_futility_stop
      df0$depth_probe_secondary_only <- enable_depth_probe
      assign(nm, df0)
    }
  }
  if (nrow(gate_df) && "bootstrap_p_one_sided" %in% names(gate_df)) {
    gate_df$bootstrap_q_value_bh_global <- stats::p.adjust(gate_df$bootstrap_p_one_sided, method = "BH")
    gate_df$bootstrap_fdr_pass_global <- is.finite(gate_df$bootstrap_q_value_bh_global) &
      gate_df$bootstrap_q_value_bh_global < fdr_alpha_primary
    gate_df$bootstrap_q_value_bh_profile <- NA_real_
    pm <- if ("profile_match" %in% names(gate_df)) as.logical(gate_df$profile_match) else rep(TRUE, nrow(gate_df))
    primary_scope <- if ("analysis_role" %in% names(gate_df)) gate_df$analysis_role == "primary_dataset" else rep(TRUE, nrow(gate_df))
    primary_idx <- primary_scope & if (isTRUE(profile_gate) && !isTRUE(keep_negative_controls)) pm else rep(TRUE, nrow(gate_df))
    primary_idx[is.na(primary_idx)] <- FALSE
    primary_idx <- primary_idx & is.finite(gate_df$bootstrap_p_one_sided)
    if (any(primary_idx, na.rm = TRUE)) {
      gate_df$bootstrap_q_value_bh_profile[primary_idx] <- stats::p.adjust(gate_df$bootstrap_p_one_sided[primary_idx], method = "BH")
    }
    gate_df$bootstrap_fdr_pass_profile <- is.finite(gate_df$bootstrap_q_value_bh_profile) &
      gate_df$bootstrap_q_value_bh_profile < fdr_alpha_primary
    gate_df$bootstrap_fdr_pass_profile_secondary <- is.finite(gate_df$bootstrap_q_value_bh_profile) &
      gate_df$bootstrap_q_value_bh_profile < fdr_alpha_secondary
    gate_df$fdr_scope_gate_level <- "gate row level; reported as sensitivity, not the primary multiplicity claim"
    gate_df$fdr_alpha_primary <- fdr_alpha_primary
    gate_df$fdr_alpha_secondary <- fdr_alpha_secondary
    # Backward-compatible aliases expected by old downstream scripts.
    gate_df$bootstrap_q_value_bh <- gate_df$bootstrap_q_value_bh_profile
    gate_df$bootstrap_fdr_pass <- gate_df$bootstrap_fdr_pass_profile
  }
  tax_df <- if (nrow(gate_df)) {
    x <- as.data.frame.matrix(table(gate_df$evidence_grade, gate_df$domain))
    x$evidence_grade <- rownames(x)
    rownames(x) <- NULL
    x[, c("evidence_grade", setdiff(names(x), "evidence_grade")), drop = FALSE]
  } else data.frame()

  evidence_grid_df <- if (nrow(gate_df)) {
    needed <- c("eligibility_layer", "regime_layer", "benchmark_layer", "evidence_layer", "ci_layer")
    missing <- setdiff(needed, names(gate_df))
    if (length(missing)) {
      data.frame()
    } else {
      as.data.frame(xtabs(~ eligibility_layer + regime_layer + benchmark_layer + evidence_layer + ci_layer,
                          data = gate_df), stringsAsFactors = FALSE)
    }
  } else data.frame()
  survival_df <- if (nrow(gate_df)) {
    do.call(rbind, lapply(split(gate_df, paste(gate_df$specialist_id, gate_df$domain, sep = "__")), function(z) {
      data.frame(specialist_id = z$specialist_id[1], domain = z$domain[1],
                 n_rows = nrow(z), n_strong_win = sum(z$evidence_layer == "strong_win", na.rm = TRUE),
                 n_strong_point_signal = sum(z$evidence_layer == "strong_point_signal", na.rm = TRUE),
                 n_pareto_signal = sum(z$evidence_layer == "pareto_signal", na.rm = TRUE),
                 n_equal_weight_gain = sum(z$evidence_layer == "equal_weight_gain", na.rm = TRUE),
                 n_no_win = sum(z$evidence_layer == "no_win", na.rm = TRUE),
                 pass_rate = mean(z$gate_pass),
                 median_q95_gain = stats::median(z$gain_vs_best_benchmark_q95, na.rm = TRUE),
                 best_dataset = z$dataset_id[which.max(z$gain_vs_best_benchmark_q95)],
                 stringsAsFactors = FALSE)
    }))
  } else data.frame()

  dataset_level_df <- if (nrow(gate_df)) {
    do.call(rbind, lapply(split(gate_df, paste(gate_df$dataset_id, gate_df$specialist_id, gate_df$mode, sep = "__")), function(z) {
      pm <- if ("profile_match" %in% names(z)) as.logical(z$profile_match) else rep(TRUE, nrow(z))
      pm[is.na(pm)] <- FALSE
      z_primary <- z[pm, , drop = FALSE]
      if (!nrow(z_primary)) z_primary <- z
      score <- if ("ci_low" %in% names(z_primary)) z_primary$ci_low else z_primary$gain_vs_joint_benchmark_q95
      if (!any(is.finite(score))) score <- z_primary$gain_vs_joint_benchmark_q95
      best_i <- which.max(ifelse(is.finite(score), score, -Inf))
      b <- z_primary[best_i, , drop = FALSE]
      data.frame(
        dataset_id = b$dataset_id[1], domain = b$domain[1], family_hint = b$family_hint[1],
        empirical_profile = b$empirical_profile[1], specialist_id = b$specialist_id[1],
        specialist_family = b$specialist_family[1], mode = b$mode[1],
        n_condition_rows = nrow(z), n_profile_rows = sum(pm, na.rm = TRUE),
        n_countable_primary_wins = if ("countable_primary_win" %in% names(z)) sum(z$countable_primary_win, na.rm = TRUE) else NA_integer_,
        n_uncapped_primary_strong_wins = if ("dataset_uncapped_strong_win_index" %in% names(z)) sum(!is.na(z$dataset_uncapped_strong_win_index), na.rm = TRUE) else NA_integer_,
        dataset_saturated = if ("dataset_saturated" %in% names(z)) any(z$dataset_saturated, na.rm = TRUE) else FALSE,
        dataset_stop_reason = if ("dataset_stop_reason" %in% names(z)) paste(unique(na.omit(z$dataset_stop_reason)), collapse = ";") else NA_character_,
        any_primary_strong_win = any(z$evidence_layer == "strong_win", na.rm = TRUE),
        any_primary_strong_point_signal = any(z$evidence_layer %in% c("strong_win", "strong_point_signal"), na.rm = TRUE),
        any_primary_pareto_signal = any(z$evidence_layer %in% c("strong_win", "strong_point_signal", "pareto_signal"), na.rm = TRUE),
        best_condition_id = b$condition_id[1], best_benchmark_joint = b$best_benchmark_joint[1],
        best_gain_mean = b$gain_vs_joint_benchmark_mean[1],
        best_gain_q95 = b$gain_vs_joint_benchmark_q95[1],
        best_ci_low = b$ci_low[1], best_ci_high = b$ci_high[1],
        best_bootstrap_p_one_sided = b$bootstrap_p_one_sided[1],
        best_bootstrap_q_profile = if ("bootstrap_q_value_bh_profile" %in% names(b)) b$bootstrap_q_value_bh_profile[1] else NA_real_,
        best_evidence_layer = if ("evidence_layer" %in% names(b)) b$evidence_layer[1] else NA_character_,
        best_ci_layer = if ("ci_layer" %in% names(b)) b$ci_layer[1] else NA_character_,
        best_final_evidence_label = if ("final_evidence_label" %in% names(b)) b$final_evidence_label[1] else NA_character_,
        stringsAsFactors = FALSE
      )
    }))
  } else data.frame()

  if (nrow(dataset_level_df)) {
    dataset_level_df$dataset_level_p_one_sided <- dataset_level_df$best_bootstrap_p_one_sided
    dataset_level_df$dataset_level_fdr_scope <- "primary eligible in-regime dataset-level rows only"
    dataset_level_df$dataset_level_fdr_policy <- fdr_policy_text
    dataset_level_df$dataset_level_q_value_bh <- NA_real_
    dataset_level_df$dataset_level_fdr_pass_primary <- FALSE
    dataset_level_df$dataset_level_fdr_pass_secondary <- FALSE
    label <- if ("best_final_evidence_label" %in% names(dataset_level_df)) dataset_level_df$best_final_evidence_label else rep("", nrow(dataset_level_df))
    primary_ds_idx <- grepl("^eligible / in_regime / primary_preregistered", label) &
      is.finite(dataset_level_df$dataset_level_p_one_sided)
    primary_ds_idx[is.na(primary_ds_idx)] <- FALSE
    if (any(primary_ds_idx)) {
      dataset_level_df$dataset_level_q_value_bh[primary_ds_idx] <- stats::p.adjust(dataset_level_df$dataset_level_p_one_sided[primary_ds_idx], method = "BH")
      dataset_level_df$dataset_level_fdr_pass_primary[primary_ds_idx] <- is.finite(dataset_level_df$dataset_level_q_value_bh[primary_ds_idx]) &
        dataset_level_df$dataset_level_q_value_bh[primary_ds_idx] < fdr_alpha_primary
      dataset_level_df$dataset_level_fdr_pass_secondary[primary_ds_idx] <- is.finite(dataset_level_df$dataset_level_q_value_bh[primary_ds_idx]) &
        dataset_level_df$dataset_level_q_value_bh[primary_ds_idx] < fdr_alpha_secondary
    }
    dataset_level_df$primary_countable_after_fdr <- dataset_level_df$any_primary_strong_win & dataset_level_df$dataset_level_fdr_pass_primary
    dataset_level_df$secondary_countable_after_fdr_10 <- dataset_level_df$any_primary_strong_win & dataset_level_df$dataset_level_fdr_pass_secondary
    dataset_level_df$preregistration_version <- "V5_audit_ready"
    dataset_level_df$profile_gate_policy <- profile_gate_policy_text
    dataset_level_df$benchmark_policy_primary <- benchmark_policy
    dataset_level_df$output_layout_policy <- output_layout_policy_text
  }

  primary_compact_df <- if (nrow(dataset_level_df)) {
    keep <- c("dataset_id", "domain", "family_hint", "empirical_profile", "specialist_id", "specialist_family", "mode",
              "n_condition_rows", "n_profile_rows", "n_countable_primary_wins", "n_uncapped_primary_strong_wins", "dataset_saturated",
              "dataset_stop_reason", "any_primary_strong_win", "best_condition_id", "best_benchmark_joint", "best_gain_mean", "best_gain_q95",
              "best_ci_low", "best_ci_high", "dataset_level_p_one_sided", "dataset_level_q_value_bh",
              "dataset_level_fdr_pass_primary", "dataset_level_fdr_pass_secondary", "best_evidence_layer", "best_ci_layer", "best_final_evidence_label")
    dataset_level_df[, intersect(keep, names(dataset_level_df)), drop = FALSE]
  } else data.frame()

  fdr_survivors_df <- if (nrow(dataset_level_df)) {
    dataset_level_df[dataset_level_df$dataset_level_fdr_pass_secondary %in% TRUE, , drop = FALSE]
  } else data.frame()

  saturation_df <- if (nrow(dataset_level_df) && "dataset_saturated" %in% names(dataset_level_df)) {
    dataset_level_df[dataset_level_df$dataset_saturated %in% TRUE, , drop = FALSE]
  } else data.frame()

  methods_audit_df <- data.frame(
    preregistration_version = "V5_audit_ready",
    profile_gate_enabled = profile_gate,
    profile_gate_policy = profile_gate_policy_text,
    benchmark_policy_primary = benchmark_policy,
    oracle_audit_secondary_only = write_oracle_audit,
    paired_ci_policy = "paired bootstrap aligned to the same reported gain statistic",
    strict_ci_contains_point_policy = strict_ci_contains_point,
    fdr_policy = fdr_policy_text,
    fdr_alpha_primary = fdr_alpha_primary,
    fdr_alpha_secondary = fdr_alpha_secondary,
    output_layout_policy = output_layout_policy_text,
    csv_max_rows = csv_max_rows,
    target_loaded_sources = target_loaded,
    source_cap_prop = source_cap_prop,
    wins_saturation_cap = wins_saturation_cap,
    futility_stop_enabled = enable_futility_stop,
    depth_probe_secondary_only = enable_depth_probe,
    stringsAsFactors = FALSE
  )

  # Clean V5 output layout:
  # 01_results_basic: compact result tables for reading and downstream writing.
  # 02_reports: the generated interpretive PDF and report assets.
  # 03_audit_ready: full provenance and decision CSVs.
  # 98_discarded_nonfinal: obsolete or nonfinal byproducts, kept out of evidence.
  # 99_recovery_logs: checkpoints, downloads, and recovery files only.
  writeLines(c(
    "# Discarded or Nonfinal Byproducts",
    "",
    "This folder is intentionally separate from the evidence folders.",
    "Use it only for obsolete, superseded, or nonfinal artifacts that should not be cited as results.",
    "A clean completed run may leave this folder empty."
  ), file.path(discarded_dir, "README_DISCARDED_NONFINAL.md"))

  eb_write_csv_split(primary_compact_df, file.path(basic_dir, "external_primary_results_compact.csv"), csv_max_rows)
  eb_write_csv_split(fdr_survivors_df, file.path(basic_dir, "external_fdr_survivors_dataset_level.csv"), csv_max_rows)
  eb_write_csv_split(saturation_df, file.path(basic_dir, "external_dataset_saturation_flags.csv"), csv_max_rows)
  eb_write_csv_split(tax_df, file.path(basic_dir, "external_taxonomy_summary.csv"), csv_max_rows)
  eb_write_csv_split(evidence_grid_df, file.path(basic_dir, "external_evidence_grid.csv"), csv_max_rows)
  eb_write_csv_split(survival_df, file.path(basic_dir, "external_survival_map.csv"), csv_max_rows)
  eb_write_csv_split(methods_audit_df, file.path(basic_dir, "external_methods_audit_flags.csv"), csv_max_rows)

  eb_write_csv_split(registry_df, file.path(audit_dir, "external_dataset_registry.csv"), csv_max_rows)
  eb_write_csv_split(metrics_df, file.path(audit_dir, "external_condition_metrics.csv"), csv_max_rows)
  eb_write_csv_split(gate_df, file.path(audit_dir, "external_gate_decisions.csv"), csv_max_rows)
  eb_write_csv_split(oracle_df, file.path(audit_dir, "external_oracle_audit.csv"), csv_max_rows)
  eb_write_csv_split(dataset_level_df, file.path(audit_dir, "external_dataset_level_decisions.csv"), csv_max_rows)
  eb_write_csv_split(tax_df, file.path(audit_dir, "external_taxonomy_summary.csv"), csv_max_rows)
  eb_write_csv_split(evidence_grid_df, file.path(audit_dir, "external_evidence_grid.csv"), csv_max_rows)
  eb_write_csv_split(survival_df, file.path(audit_dir, "external_survival_map.csv"), csv_max_rows)
  eb_write_csv_split(methods_audit_df, file.path(audit_dir, "external_methods_audit_flags.csv"), csv_max_rows)

  code_files <- list.files(project_root, pattern = "\\.R$", full.names = TRUE)
  manifest <- data.frame(
    generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    output_dir = normalizePath(out_dir, winslash = "/", mustWork = FALSE),
    basic_results_dir = normalizePath(basic_dir, winslash = "/", mustWork = FALSE),
    reports_dir = normalizePath(report_dir, winslash = "/", mustWork = FALSE),
    audit_ready_dir = normalizePath(audit_dir, winslash = "/", mustWork = FALSE),
    discarded_nonfinal_dir = normalizePath(discarded_dir, winslash = "/", mustWork = FALSE),
    recovery_logs_dir = normalizePath(recovery_dir, winslash = "/", mustWork = FALSE),
    run_label = rw_safe_label(run_label),
    n_requested_sources = length(specs), n_loaded_sources = sum(registry_df$load_ok),
    target_loaded_sources = target_loaded,
    n_gate_rows = nrow(gate_df), R = R, paired_bootstrap_B = boot_B,
    boot_B_extra = boot_B_extra, boot_B_max = boot_B_max,
    adaptive_bootstrap = adaptive_bootstrap, targeted_extra_R = targeted_extra_R,
    profile_gate = profile_gate, keep_negative_controls = keep_negative_controls,
    benchmark_policy = benchmark_policy, primary_metric = primary_metric,
    profile_benchmark_map = profile_benchmark_map,
    write_oracle_audit = write_oracle_audit, oracle_boot_B = oracle_boot_B,
    csv_max_rows = csv_max_rows,
    clean_output_layout = clean_output_layout, generate_pdf_report = generate_pdf_report,
    fdr_alpha_primary = fdr_alpha_primary, fdr_alpha_secondary = fdr_alpha_secondary,
    fdr_policy = fdr_policy_text,
    enable_dataset_balance = enable_dataset_balance, target_loaded = target_loaded,
    profile_quotas = paste(names(profile_quota), unlist(profile_quota), sep = "=", collapse = ";"),
    source_cap_prop = source_cap_prop, wins_saturation_cap = wins_saturation_cap,
    enable_futility_stop = enable_futility_stop, futility_min_gate_rows = futility_min_gate_rows,
    enable_depth_probe = enable_depth_probe, depth_trigger_wins = depth_trigger_wins,
    depth_max_variants = depth_max_variants, depth_sample_frac = depth_sample_frac,
    depth_probe_specs_added = depth_probe_specs_added,
    pareto_tolerance_rel = pareto_tolerance_rel, near_ci_rel = near_ci_rel,
    contamination_mode = contamination_mode, natural_z_threshold = natural_z_threshold,
    seed = seed, wall_clock_seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    r_version = R.version.string,
    code_files = paste(basename(code_files), collapse = ";"),
    code_md5s = paste(vapply(code_files, rw_md5, character(1)), collapse = ";"),
    stringsAsFactors = FALSE
  )
  rw_write_csv(manifest, file.path(audit_dir, "RUN_MANIFEST.csv"))
  rw_write_csv(manifest, file.path(recovery_dir, "RUN_MANIFEST_RECOVERY_COPY.csv"))

  lines <- c(
    "# External Public Dataset Battery",
    "",
    sprintf("Requested public sources: %d; loaded sources: %d.", length(specs), sum(registry_df$load_ok)),
    sprintf("Gate rows: %d. Base R=%d; targeted extra R=%d; base bootstrap B=%d; max adaptive B=%d.", nrow(gate_df), R, targeted_extra_R, boot_B, boot_B_max),
    sprintf("Profile gate: %s; primary benchmark policy: %s; primary metric: %s.", profile_gate, benchmark_policy, primary_metric),
    sprintf("Secondary oracle audit written: %s; oracle bootstrap B: %d.", write_oracle_audit, oracle_boot_B),
    sprintf("Dataset-level BH FDR: alpha %.2f primary, %.2f secondary sensitivity.", fdr_alpha_primary, fdr_alpha_secondary),
    sprintf("CSV split threshold: %d rows per part.", csv_max_rows),
    sprintf("Output layout: basic=%s; reports=%s; audit=%s; discarded=%s; recovery=%s.", basename(basic_dir), basename(report_dir), basename(audit_dir), basename(discarded_dir), basename(recovery_dir)),
    sprintf("Dataset breadth target: %d; source cap: %.2f; saturation cap: %d countable strong wins per dataset.", target_loaded, source_cap_prop, wins_saturation_cap),
    sprintf("Futility stop: %s after at least %d gate rows without positive or near-CI signal.", enable_futility_stop, futility_min_gate_rows),
    sprintf("Secondary depth probes: %s; trigger=%d countable strong wins; added=%d probes.", enable_depth_probe, depth_trigger_wins, depth_probe_specs_added),
    sprintf("Contamination mode: %s; natural z threshold: %.2f.", contamination_mode, natural_z_threshold),
    "",
    "Primary evidence uses the pre-registered profile benchmark and profile gate. Oracle comparisons are secondary audit rows and should not be promoted to the primary claim.",
    "All source URLs, access timestamps, downloaded-file MD5 hashes, variables, transformations, and dataset diagnostics are recorded in `external_dataset_registry.csv`.",
    "Large tables are split into part files when they exceed EXTERNAL_CSV_MAX_ROWS. The original CSV name becomes a small index listing each part.",
    "V5 breadth/depth and audit-ready policy: primary N is based on selected primary datasets. Within-dataset depth probes are secondary and share parent_dataset_id; countable_primary_win caps each dataset at EXTERNAL_WINS_SATURATION_CAP.",
    "Dataset-level BH FDR is computed after aggregation and should be treated as the primary multiplicity control. Gate-row FDR columns are included only for transparency."
  )
  writeLines(lines, file.path(report_dir, "SUMMARY_EXTERNAL_BATTERY.md"))
  writeLines(lines, file.path(audit_dir, "SUMMARY_EXTERNAL_BATTERY.md"))

  if (isTRUE(generate_pdf_report)) {
    report_script <- file.path(eb_script_dir(), "generate_external_report.R")
    if (file.exists(report_script)) {
      tryCatch({
        source(report_script, local = TRUE)
        if (exists("generate_external_report_v5", mode = "function")) {
          generate_external_report_v5(run_dir = out_dir,
                                      basic_dir = basic_dir,
                                      audit_dir = audit_dir,
                                      report_dir = report_dir,
                                      run_label = rw_safe_label(run_label))
        }
      }, error = function(e) {
        writeLines(conditionMessage(e), file.path(recovery_dir, "PDF_REPORT_ERROR.log"))
        eb_cat("[EB] PDF report generation failed; see 99_recovery_logs/PDF_REPORT_ERROR.log")
      })
    } else {
      writeLines("generate_external_report.R not found", file.path(recovery_dir, "PDF_REPORT_ERROR.log"))
    }
  }

  eb_cat("[EB] Done. Output folder: %s", normalizePath(out_dir, winslash = "/", mustWork = FALSE))
  invisible(out_dir)
}

if (sys.nframe() == 0L && !interactive()) {
  project_root <- normalizePath(Sys.getenv("PROJECT_ROOT", unset = rw_script_dir()), winslash = "/", mustWork = FALSE)
  run_realworld_external_battery(project_root = project_root)
}
