## Shared utilities for the external public-data validation battery.
##
## These routines load the frozen specialist weights, construct original and
## locked-unseen validation conditions, evaluate fixed-weight specialists on
## empirical samples, and compute paired bootstrap intervals. They are kept
## separate from the battery catalog so the main module remains readable.

rw_cat <- function(...) {
  cat(sprintf(...), "\n", sep = "")
  flush.console()
}

rw_script_dir <- function(default = getwd()) {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(default, winslash = "/", mustWork = FALSE)
}

rw_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(x, path, row.names = FALSE)
  invisible(path)
}

rw_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

rw_first_existing <- function(root, relative_paths, label) {
  paths <- file.path(root, relative_paths)
  hit <- paths[file.exists(paths)][1]
  if (!is.na(hit)) return(hit)
  stop("Required ", label, " file not found. Checked: ",
       paste(paths, collapse = "; "), call. = FALSE)
}

rw_safe_label <- function(x) {
  x <- as.character(x)[1]
  if (!nzchar(x) || is.na(x)) return("realworld_external_battery")
  x <- gsub("[^A-Za-z0-9_.-]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) "realworld_external_battery" else x
}

rw_num_list <- function(x) {
  if (length(x) == 0L || is.na(x[1]) || !nzchar(as.character(x[1]))) return(numeric(0))
  suppressWarnings(as.numeric(strsplit(as.character(x[1]), ";", fixed = TRUE)[[1]]))
}

rw_chr_list <- function(x) {
  if (length(x) == 0L || is.na(x[1]) || !nzchar(as.character(x[1]))) return(character(0))
  trimws(strsplit(as.character(x[1]), ";", fixed = TRUE)[[1]])
}

rw_num_values <- function(x) {
  if (length(x) == 0L) return(numeric(0))
  if (length(x) == 1L && (is.na(x[1]) || !nzchar(as.character(x[1])))) return(numeric(0))
  if (length(x) > 1L) return(suppressWarnings(as.numeric(x)))
  rw_num_list(x)
}

rw_chr_values <- function(x) {
  if (length(x) == 0L) return(character(0))
  if (length(x) == 1L && (is.na(x[1]) || !nzchar(as.character(x[1])))) return(character(0))
  if (length(x) > 1L) return(trimws(as.character(x)))
  rw_chr_list(x)
}

rw_diag <- function(x) {
  x <- as.numeric(x[is.finite(x)])
  s <- stats::sd(x)
  list(
    n = length(x),
    mean = mean(x),
    skew = if (is.finite(s) && s > 0) mean((x - mean(x))^3) / s^3 else NA_real_,
    kurt = if (is.finite(s) && s > 0) mean((x - mean(x))^4) / s^4 - 3 else NA_real_,
    support_sign = if (all(x > 0)) "strictly_positive" else if (any(x <= 0)) "contains_nonpositive" else "unknown"
  )
}

rw_seed_fallbacks <- function() {
  if (!exists(".stable_int_seed", mode = "function", inherits = TRUE)) {
    .stable_int_seed <<- function(key, base_seed = 0L) {
      key <- paste0(key)
      ints <- utf8ToInt(key)
      h <- as.numeric(base_seed %% 2147483647)
      for (v in ints) h <- (h * 131 + v) %% 2147483647
      h_int <- suppressWarnings(as.integer(h))
      if (length(h_int) != 1L || is.na(h_int) || h_int <= 0L) h_int <- 1L
      h_int
    }
  }
  if (!exists(".ensure_seed", mode = "function", inherits = TRUE)) {
    .ensure_seed <<- function(seed, fallback = 12345L) {
      s <- suppressWarnings(as.integer(seed))
      if (length(s) != 1L || is.na(s) || s <= 0L) s <- as.integer(fallback)
      s
    }
  }
  if (!exists(".seed_scope", mode = "function", inherits = TRUE)) {
    .seed_scope <<- function(seed, expr) {
      s_backup_exists <- exists(".Random.seed", inherits = FALSE)
      if (s_backup_exists) s_backup <- .Random.seed
      on.exit({
        if (s_backup_exists) .Random.seed <<- s_backup
      }, add = TRUE)
      set.seed(.ensure_seed(seed))
      force(expr)
    }
  }
  if (!exists(".safe_seed", mode = "function", inherits = TRUE)) {
    .safe_seed <<- function(...) {
      key <- paste(..., collapse = "|")
      .ensure_seed(.stable_int_seed(key))
    }
  }
  invisible(TRUE)
}

rw_load_modules <- function(project_root) {
  rw_seed_fallbacks()
  mods <- c(
    "00_utils_debug_io.R",
    "02_scenarios_sampling.R",
    "03_distributions_params.R",
    "04_estimators_registry.R",
    "06_data_prep.R",
    "11_validation_suite.R"
  )
  for (m in mods) {
    fp <- file.path(project_root, m)
    if (!file.exists(fp)) stop("Missing module: ", fp, call. = FALSE)
    source(fp, local = FALSE)
  }
  .load_ds_wine <<- function() {
    path <- Sys.getenv("WINE_DATA_PATH", unset = "")
    if (nzchar(path) && file.exists(path)) {
      df <- rw_read_real_csv(path, header = FALSE)
      if (!is.null(df) && ncol(df) >= 2L) {
        x <- as.numeric(df[, 2L])
        return(x[is.finite(x)])
      }
    }
    catf("[VAL-L4]  Downloading Wine dataset ...")
    url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
    df <- tryCatch(utils::read.csv(url, header = FALSE),
                   error = function(e) { catf("[VAL-L4]  FAILED: %s", conditionMessage(e)); NULL })
    if (!is.null(df) && ncol(df) >= 2L) {
      x <- as.numeric(df[, 2L])
      return(x[is.finite(x)])
    }
    NULL
  }
  .load_ds_forestfires <<- function() {
    path <- Sys.getenv("FORESTFIRES_DATA_PATH", unset = "")
    if (nzchar(path) && file.exists(path)) {
      df <- rw_read_real_csv(path, header = TRUE)
      if (!is.null(df) && "area" %in% names(df)) {
        x <- log(as.numeric(df$area) + 1)
        return(x[is.finite(x) & x > 0])
      }
    }
    catf("[VAL-L4]  Downloading Forest Fires dataset ...")
    url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
    df <- tryCatch(utils::read.csv(url, header = TRUE, stringsAsFactors = FALSE),
                   error = function(e) { catf("[VAL-L4]  FAILED: %s", conditionMessage(e)); NULL })
    if (!is.null(df) && "area" %in% names(df)) {
      x <- log(as.numeric(df$area) + 1)
      return(x[is.finite(x) & x > 0])
    }
    NULL
  }
  .load_ds_rt <<- function() {
    catf("[VAL-L4]  Loading RT data (DS4) — multi-strategy loader ...")
    .rt_clean <- function(x, label) {
      x <- as.numeric(x)
      x <- x[is.finite(x) & !is.na(x) & x > 0]
      if (length(x) < 30L) return(NULL)
      med_x <- median(x, na.rm = TRUE)
      if (is.finite(med_x) && med_x > 100) x <- x / 1000
      x <- x[x >= 0.10 & x <= 5.00]
      if (length(x) < 30L) return(NULL)
      q25 <- quantile(x, 0.25, na.rm = TRUE)
      q75 <- quantile(x, 0.75, na.rm = TRUE)
      iqr  <- q75 - q25
      if (is.finite(iqr) && iqr > 0) x <- x[x >= (q25 - 3 * iqr) & x <= (q75 + 3 * iqr)]
      if (length(x) < 30L) return(NULL)
      catf("[VAL-L4]  DS4 via %-38s — n=%d  median=%.3f s  skew≈%.2f",
           label, length(x), median(x), { s <- sd(x); if (s > 0) mean((x - mean(x))^3) / s^3 else NA })
      x
    }
    .pkg_data <- function(pkg, dsname) {
      if (!requireNamespace(pkg, quietly = TRUE)) return(NULL)
      env <- new.env(parent = emptyenv())
      tryCatch({ utils::data(list = dsname, package = pkg, envir = env); get(dsname, envir = env) },
               error = function(e) NULL)
    }
    .find_rt_col <- function(df) {
      if (!is.data.frame(df)) return(NA_character_)
      cands <- intersect(c("rt", "RT", "latency", "response_time", "time"), names(df))
      if (length(cands)) return(cands[1])
      for (nm in names(df)) {
        v <- suppressWarnings(as.numeric(df[[nm]]))
        v <- v[is.finite(v) & v > 0]
        if (length(v) > 30 && median(v) > 0.05 && median(v) < 20000) return(nm)
      }
      NA_character_
    }
    .filter_correct <- function(df, rt_col) {
      resp_col <- intersect(c("response", "resp", "correct", "acc", "accuracy", "corr"), names(df))[1]
      rt_vec <- as.numeric(df[[rt_col]])
      if (is.na(resp_col)) return(rt_vec)
      rv <- df[[resp_col]]
      mask <- if (is.character(rv) || is.factor(rv)) {
        tolower(as.character(rv)) %in% c("correct", "corr", "1", "true", "yes", "upper", "hit") & !is.na(rv)
      } else if (is.logical(rv)) {
        rv == TRUE & !is.na(rv)
      } else {
        rv == 1 & !is.na(rv)
      }
      x_filt <- rt_vec[mask]
      if (sum(is.finite(x_filt) & x_filt > 0) < 30L) return(rt_vec)
      x_filt
    }
    if (requireNamespace("rtdists", quietly = TRUE)) {
      df1 <- .pkg_data("rtdists", "speed_acc")
      if (!is.null(df1) && is.data.frame(df1)) {
        rc1 <- .find_rt_col(df1)
        if (!is.na(rc1)) {
          x <- .rt_clean(.filter_correct(df1, rc1), "rtdists::speed_acc")
          if (!is.null(x)) return(x)
          x <- .rt_clean(as.numeric(df1[[rc1]]), "rtdists::speed_acc (all)")
          if (!is.null(x)) return(x)
        }
      }
      df2 <- .pkg_data("rtdists", "rrt")
      if (!is.null(df2) && is.data.frame(df2)) {
        rc2 <- .find_rt_col(df2)
        if (!is.na(rc2)) {
          x <- .rt_clean(.filter_correct(df2, rc2), "rtdists::rrt")
          if (!is.null(x)) return(x)
        }
      }
    }
    if (requireNamespace("fddm", quietly = TRUE)) {
      df4 <- .pkg_data("fddm", "med_dec")
      if (!is.null(df4) && is.data.frame(df4)) {
        rc4 <- .find_rt_col(df4)
        if (!is.na(rc4)) {
          x <- .rt_clean(.filter_correct(df4, rc4), "fddm::med_dec")
          if (!is.null(x)) return(x)
          x <- .rt_clean(as.numeric(df4[[rc4]]), "fddm::med_dec (all)")
          if (!is.null(x)) return(x)
        }
      }
    }
    catf("[VAL-L4]  Strategy 5: public CSV — Balota2007 subject means ...")
    rt_urls <- c(
      "https://raw.githubusercontent.com/PerceptionCognitionLab/data0/master/lexDec-Balota2007/BalotaEtAl2007_LDT_data.csv",
      "https://osf.io/download/3mxdg/"
    )
    for (url in rt_urls) {
      df5 <- tryCatch(utils::read.csv(url, header = TRUE, stringsAsFactors = FALSE, nrows = 80000L),
                      error = function(e) NULL)
      if (!is.null(df5) && is.data.frame(df5) && nrow(df5) > 30) {
        rc5 <- .find_rt_col(df5)
        if (!is.na(rc5)) {
          subj_col <- intersect(c("Subject", "subject", "subj", "participant", "Participant", "ID", "id"), names(df5))[1]
          if (!is.na(subj_col)) {
            raw_rt <- suppressWarnings(as.numeric(.filter_correct(df5, rc5)))
            subj <- df5[[subj_col]]
            valid <- is.finite(raw_rt) & raw_rt > 0
            subj_means <- tapply(raw_rt[valid], subj[valid], mean, na.rm = TRUE)
            x <- .rt_clean(as.numeric(subj_means), "Balota2007 subject means")
            if (!is.null(x)) return(x)
          }
          x <- .rt_clean(.filter_correct(df5, rc5), "Balota2007 all trials")
          if (!is.null(x)) return(x)
        }
      }
    }
    NULL
  }
  .load_ds_spy <<- function() {
    catf("[VAL-L4]  Downloading SPY log-returns via quantmod ...")
    if (!requireNamespace("quantmod", quietly = TRUE)) {
      catf("[VAL-L4]  quantmod not available — skipping DS5")
      return(NULL)
    }
    prices <- tryCatch({
      e <- new.env()
      quantmod::getSymbols("SPY", src = "yahoo", from = "2010-01-01",
                           to = as.character(Sys.Date()), env = e, auto.assign = TRUE)
      as.numeric(quantmod::Ad(e$SPY))
    }, error = function(err) {
      catf("[VAL-L4]  quantmod FAILED: %s", conditionMessage(err)); NULL
    })
    if (is.null(prices) || length(prices) < 50L) return(NULL)
    rets <- diff(log(prices[is.finite(prices)]))
    rets[is.finite(rets)]
  }
  invisible(TRUE)
}

rw_read_real_csv <- function(path, header = TRUE, ...) {
  if (!nzchar(path) || !file.exists(path)) return(NULL)
  tryCatch(utils::read.csv(path, header = header, stringsAsFactors = FALSE, ...),
           error = function(e) NULL)
}

rw_weight_from_row <- function(row, phase, specialist_id, source_file) {
  wcols <- grep("^w_", names(row), value = TRUE)
  w <- if (length(wcols)) {
    as.numeric(row[1, wcols, drop = TRUE])
  } else if ("ga_weight_vector" %in% names(row)) {
    rw_num_list(row$ga_weight_vector[1])
  } else {
    numeric(0)
  }
  if (!length(w) || any(!is.finite(w))) {
    stop("Could not parse weights for ", specialist_id, " from ", source_file, call. = FALSE)
  }
  if (phase == "P1" && length(w) == 10L && exists("N_EST", inherits = TRUE) && N_EST > 10L) {
    w <- c(w, rep(0, N_EST - 10L))
  }
  if (length(w) != N_EST) {
    stop(sprintf("Weight length mismatch for %s: got %d, expected %d", specialist_id, length(w), N_EST),
         call. = FALSE)
  }
  w <- .normalize_simplex(w)
  names(w) <- ESTIMATOR_NAMES
  list(
    phase = phase,
    specialist_id = specialist_id,
    validation_id = as.character(row$validation_id[1]),
    family = as.character(row$distribution[1]),
    source_seed = as.character(row$source_seed[1]),
    source_file = normalizePath(source_file, winslash = "/", mustWork = FALSE),
    source_md5 = rw_md5(source_file),
    original_rates = rw_num_list(row$exact_contamination_rates[1]),
    original_scales = rw_num_list(row$exact_outlier_scales[1]),
    original_types = rw_chr_list(row$exact_contamination_types[1]),
    original_sample_sizes = rw_num_list(row$exact_sample_sizes[1]),
    weights = w
  )
}

rw_load_specialists <- function(root) {
  taxonomy_file <- rw_first_existing(root, c(
    file.path("results", "5_evidence_taxonomy_nest26",
              "tables/evidence_taxonomy_all_candidates.csv"),
    file.path("05_EVIDENCE_TAXONOMY_NEST26", "evidence_results_20260611",
              "tables/evidence_taxonomy_all_candidates.csv"),
    file.path("GA_Results", "05_expanded_fixed_weight_validation_evidence_taxonomy_NEST26",
              "tables/evidence_taxonomy_all_candidates.csv")
  ), "taxonomy")
  p2_file <- rw_first_existing(root, c(
    file.path("results", "4_fixed_weight_validation_nest26",
              "root_summaries", "post_discovery_fixed_weight_validation_selected_regimes.csv"),
    file.path("04_EXPANDED_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST26",
              "raw_full_run_output_with_tasks_checkpoints_20260611", "q1_selected_regimes.csv"),
    file.path("GA_Results", "04_expanded_post_discovery_fixed_weight_validation_NEST26",
              "root_summaries", "post_discovery_fixed_weight_validation_selected_regimes.csv")
  ), "selected-regime")
  p2_locked <- rw_first_existing(root, c(
    file.path("results", "4_fixed_weight_validation_nest26",
              "audit", "post_discovery_fixed_weight_validation_locked_unseen_construction.csv"),
    file.path("04_EXPANDED_POST_DISCOVERY_FIXED_WEIGHT_VALIDATION_NEST26",
              "raw_full_run_output_with_tasks_checkpoints_20260611",
              "audit", "q1_locked_unseen_construction.csv"),
    file.path("GA_Results", "04_expanded_post_discovery_fixed_weight_validation_NEST26",
              "audit", "post_discovery_fixed_weight_validation_locked_unseen_construction.csv")
  ), "locked-unseen")

  taxonomy <- utils::read.csv(taxonomy_file, check.names = FALSE, stringsAsFactors = FALSE)
  p2 <- utils::read.csv(p2_file, check.names = FALSE, stringsAsFactors = FALSE)
  l2 <- utils::read.csv(p2_locked, check.names = FALSE, stringsAsFactors = FALSE)
  if (!nrow(taxonomy)) stop("No taxonomy candidates found in: ", taxonomy_file, call. = FALSE)
  if (!"validation_id" %in% names(taxonomy)) {
    stop("Specialist taxonomy is missing validation_id: ", taxonomy_file, call. = FALSE)
  }

  include_classes <- Sys.getenv("EXTERNAL_TAXONOMY_INCLUDE_CLASSES",
                                unset = "accepted_ga_win;borderline_near_gate;benchmark_control")
  include_classes <- rw_chr_values(include_classes)
  include_controls <- tolower(Sys.getenv("EXTERNAL_INCLUDE_TAXONOMY_CONTROLS", unset = "TRUE")) %in%
    c("1", "true", "t", "yes", "y", "si", "sí")
  if ("validation_class" %in% names(taxonomy) && length(include_classes)) {
    taxonomy <- taxonomy[taxonomy$validation_class %in% include_classes, , drop = FALSE]
  }
  if ("validation_class" %in% names(taxonomy) && !isTRUE(include_controls)) {
    taxonomy <- taxonomy[taxonomy$validation_class != "benchmark_control", , drop = FALSE]
  }
  if (!nrow(taxonomy)) {
    stop("No taxonomy candidates remain after EXTERNAL_TAXONOMY_INCLUDE_CLASSES filtering.",
         call. = FALSE)
  }

  ids <- unique(as.character(taxonomy$validation_id))
  ids <- ids[nzchar(ids) & !is.na(ids)]
  get_taxonomy_specialist <- function(id) {
    row <- p2[p2$validation_id == id, , drop = FALSE]
    if (!nrow(row)) {
      stop("Taxonomy specialist ", id, " not found in latest NEST26 selected-regime table: ",
           p2_file, call. = FALSE)
    }
    s <- rw_weight_from_row(row[1, , drop = FALSE], "NEST26_TAXONOMY",
                            paste0("TAX-", id), p2_file)
    tx <- taxonomy[taxonomy$validation_id == id, , drop = FALSE][1, , drop = FALSE]
    s$taxonomy_file <- normalizePath(taxonomy_file, winslash = "/", mustWork = FALSE)
    s$taxonomy_md5 <- rw_md5(taxonomy_file)
    s$evidence_grade <- as.character(tx$evidence_grade[1] %||% NA_character_)
    s$validation_class <- as.character(tx$validation_class[1] %||% NA_character_)
    s$legacy_final_decision <- as.character(tx$legacy_final_decision[1] %||% NA_character_)
    s
  }

  specs <- lapply(ids, get_taxonomy_specialist)

  add_locked <- function(s, locked_df, locked_file) {
    lr <- locked_df[locked_df$validation_id == s$validation_id, , drop = FALSE]
    s$locked_rates <- if (nrow(lr)) rw_num_list(lr$unseen_rates[1]) else numeric(0)
    s$locked_scales <- if (nrow(lr)) rw_num_list(lr$unseen_scales[1]) else numeric(0)
    s$locked_types <- if (nrow(lr)) rw_chr_list(lr$unseen_types[1]) else character(0)
    s$locked_reason <- if (nrow(lr) && "unseen_reason" %in% names(lr)) as.character(lr$unseen_reason[1]) else NA_character_
    s$locked_file <- normalizePath(locked_file, winslash = "/", mustWork = FALSE)
    s$locked_md5 <- rw_md5(locked_file)
    s
  }
  lapply(specs, add_locked, locked_df = l2, locked_file = p2_locked)
}

rw_load_datasets <- function() {
  loaders <- list(
    DS1_Wine_Alcohol = list(loader = .load_ds_wine, family = "normal", role = "negative_control"),
    DS2_ForestFires_logArea = list(loader = .load_ds_forestfires, family = "lognormal", role = "positive_test"),
    DS3_GBSG2_Survival = list(loader = .load_ds_gbsg2, family = "weibull", role = "positive_transfer"),
    DS4_RT_LexicalDecision = list(loader = .load_ds_rt, family = "exgaussian", role = "negative_control"),
    DS5_SPY_LogReturns = list(loader = .load_ds_spy, family = "normal_heavytail", role = "edge_case")
  )
  out <- list()
  for (id in names(loaders)) {
    rw_cat("[RW] Loading %s", id)
    x <- loaders[[id]]$loader()
    if (is.null(x) || length(x) < 30L) stop("Dataset failed to load or has <30 observations: ", id, call. = FALSE)
    out[[id]] <- c(loaders[[id]], list(id = id, x = as.numeric(x[is.finite(x)])))
  }
  out
}

rw_dataset_specs <- function(specialists) {
  # Legacy small-battery helper retained for compatibility with older ad hoc
  # checks. The V5 public-data battery evaluates the taxonomy-selected
  # specialists directly against the expanded external catalog.
  list(
    DS1_Wine_Alcohol = specialists,
    DS2_ForestFires_logArea = specialists,
    DS3_GBSG2_Survival = specialists,
    DS4_RT_LexicalDecision = specialists,
    DS5_SPY_LogReturns = specialists
  )
}

rw_nearest_not_in <- function(value, grid, used) {
  grid <- sort(unique(as.numeric(grid)))
  used <- unique(as.numeric(used))
  cand <- grid[!grid %in% used]
  if (!length(cand)) cand <- grid
  cand[which.min(abs(cand - as.numeric(value)[1]))]
}

rw_type_group <- function(types) {
  types <- tolower(as.character(types))
  if (any(grepl("upper|clustered_upper", types))) return("upper")
  if (any(grepl("lower", types))) return("lower")
  if (any(grepl("sym", types))) return("symmetric")
  if (any(grepl("bimodal|mixture", types))) return("bimodal")
  if (any(grepl("point", types))) return("point")
  "generic"
}

rw_similar_type_candidates <- function(types, all_types) {
  all_types <- as.character(all_types)
  used <- unique(tolower(as.character(types)))
  group <- rw_type_group(types)
  cand <- switch(group,
                 upper = c("upper_tail", "clustered_upper"),
                 lower = c("lower_tail"),
                 symmetric = c("symmetric_t", "clustered_symmetric"),
                 bimodal = c("mixture_bimodal_near", "mixture_bimodal_far"),
                 point = c("point_mass"),
                 all_types)
  cand <- intersect(cand, all_types)
  cand2 <- cand[!tolower(cand) %in% used]
  if (length(cand2)) cand2 else cand
}

rw_conditions <- function(spec, mode) {
  rates <- rw_num_values(if (identical(mode, "locked_unseen_similar")) spec$locked_rates else spec$original_rates)
  scales <- rw_num_values(if (identical(mode, "locked_unseen_similar")) spec$locked_scales else spec$original_scales)
  types <- rw_chr_values(if (identical(mode, "locked_unseen_similar")) spec$locked_types else spec$original_types)
  ns <- rw_num_values(spec$original_sample_sizes)
  if (!length(rates)) rates <- numeric(0)
  if (!length(scales)) scales <- numeric(0)
  if (!length(types)) types <- character(0)
  if (!length(ns)) ns <- numeric(0)

  if (identical(mode, "locked_unseen_similar")) {
    out <- expand.grid(
      contamination_rate = if (length(rates)) rates else 0,
      outlier_scale_mad = if (length(scales)) scales else 3,
      contamination_type = if (length(types)) types else "upper_tail",
      sample_size = as.integer(if (length(ns)) ns else c(300, 500, 1000)),
      stringsAsFactors = FALSE
    )
    out$locked_unseen_reason <- if (!is.null(spec$locked_reason) && length(spec$locked_reason)) {
      as.character(spec$locked_reason[1])
    } else {
      NA_character_
    }
    return(out)
  }

  out <- expand.grid(
    contamination_rate = if (length(rates)) rates else 0,
    outlier_scale_mad = if (length(scales)) scales else 0,
    contamination_type = if (length(types)) types else "none",
    sample_size = if (length(ns)) as.integer(ns) else c(300L, 500L, 1000L),
    stringsAsFactors = FALSE
  )
  out$locked_unseen_reason <- NA_character_
  out
}

rw_tail_sides <- function(type) {
  type <- tolower(as.character(type)[1])
  if (grepl("lower", type)) return("lower")
  if (grepl("symmetric|bimodal|mixture|clustered_symmetric", type)) return("both")
  "upper"
}

rw_empirical_tail_pool <- function(x, type, z_threshold) {
  center <- stats::median(x, na.rm = TRUE)
  scale <- stats::mad(x, constant = 1, na.rm = TRUE)
  if (!is.finite(scale) || scale <= 0) scale <- stats::IQR(x, na.rm = TRUE) / 1.349
  if (!is.finite(scale) || scale <= 0) scale <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(scale) || scale <= 0) {
    return(list(tail = integer(0), body = seq_along(x), z = rep(0, length(x)), center = center, scale = scale))
  }
  z <- (x - center) / scale
  side <- rw_tail_sides(type)
  tail <- switch(side,
                 upper = which(is.finite(z) & z >= z_threshold),
                 lower = which(is.finite(z) & z <= -z_threshold),
                 both = which(is.finite(z) & abs(z) >= z_threshold))
  body <- setdiff(which(is.finite(x)), tail)
  list(tail = tail, body = body, z = z, center = center, scale = scale)
}

rw_draw_empirical_condition <- function(x, m, cond, contamination_mode, natural_z_threshold) {
  n <- length(x)
  m <- min(as.integer(m), n - 1L)
  rate <- as.numeric(cond$contamination_rate[1])
  requested_tail_n <- if (is.finite(rate) && rate > 0) min(m, floor(m * rate)) else 0L

  if (identical(contamination_mode, "injected")) {
    base <- x[sample.int(n, m, replace = FALSE)]
    out <- if (requested_tail_n > 0L) {
      inject_outliers_realistic(
        base,
        contamination_rate = rate,
        outlier_scale_mad = cond$outlier_scale_mad[1],
        type = as.character(cond$contamination_type[1])
      )
    } else {
      base
    }
    return(list(x = out, requested_tail_n = requested_tail_n, empirical_tail_n = NA_integer_,
                natural_pool_n = NA_integer_, natural_body_n = NA_integer_))
  }

  pools <- rw_empirical_tail_pool(x, cond$contamination_type[1], natural_z_threshold)
  tail_n <- min(requested_tail_n, length(pools$tail))
  body_n <- min(m - tail_n, length(pools$body))
  idx <- integer(0)
  if (tail_n > 0L) idx <- c(idx, sample(pools$tail, tail_n, replace = FALSE))
  if (body_n > 0L) idx <- c(idx, sample(pools$body, body_n, replace = FALSE))
  if (length(idx) < m) {
    remaining <- setdiff(which(is.finite(x)), idx)
    fill_n <- min(m - length(idx), length(remaining))
    if (fill_n > 0L) idx <- c(idx, sample(remaining, fill_n, replace = FALSE))
  }
  list(x = x[idx], requested_tail_n = requested_tail_n, empirical_tail_n = tail_n,
       natural_pool_n = length(pools$tail), natural_body_n = length(pools$body))
}

rw_eval_one <- function(x, theta, weights, equal_weights, m, cond, n_reps, seed, dataset_family,
                        contamination_mode = "endogenous", natural_z_threshold = 3) {
  n <- length(x)
  m <- min(as.integer(m), n - 1L)
  set.seed(.ensure_seed(seed))
  rows <- vector("list", n_reps)
  for (i in seq_len(n_reps)) {
    draw <- rw_draw_empirical_condition(x, m, cond, contamination_mode, natural_z_threshold)
    xc <- draw$x
    est <- vapply(ESTIMATOR_REGISTRY, function(f) {
      z <- tryCatch(f(xc), error = function(e) NA_real_)
      if (!is.finite(z)) stats::median(xc, na.rm = TRUE) else z
    }, numeric(1))
    ga <- sum(weights * est)
    ew <- sum(equal_weights * est)
    rows[[i]] <- data.frame(
      replicate = i,
      estimator = c(names(est), "ga_specialist", "equal_weight"),
      estimate = c(as.numeric(est), ga, ew),
      contamination_mode = contamination_mode,
      natural_z_threshold = natural_z_threshold,
      requested_tail_n = draw$requested_tail_n,
      empirical_tail_n = draw$empirical_tail_n,
      natural_pool_n = draw$natural_pool_n,
      natural_body_n = draw$natural_body_n,
      stringsAsFactors = FALSE
    )
  }
  rep_df <- do.call(rbind, rows)
  rep_df$sq_error <- (rep_df$estimate - theta)^2
  rep_df
}

rw_boot_ci <- function(rep_wide, col_a, col_b, boot_b, seed, stat = c("q95_diff", "mean_diff")) {
  stat <- match.arg(stat)
  dat <- rep_wide[, c(col_a, col_b), drop = FALSE]
  dat <- dat[stats::complete.cases(dat), , drop = FALSE]
  if (!nrow(dat) || boot_b <= 1L) {
    return(c(point = NA_real_, low = NA_real_, high = NA_real_, p_one_sided = NA_real_))
  }

  stat_fun <- switch(
    stat,
    mean_diff = function(d) mean(d[[col_b]] - d[[col_a]]),
    q95_diff = function(d) {
      stats::quantile(d[[col_b]], 0.95, na.rm = TRUE, type = 8) -
        stats::quantile(d[[col_a]], 0.95, na.rm = TRUE, type = 8)
    }
  )

  point <- stat_fun(dat)
  boot_seed <- if (exists(".safe_seed", mode = "function", inherits = TRUE) &&
                   exists(".ensure_seed", mode = "function", inherits = TRUE)) {
    .ensure_seed(.safe_seed("boot", seed))
  } else {
    suppressWarnings(as.integer(seed))
  }
  if (!is.finite(boot_seed) || boot_seed <= 0L) boot_seed <- 12345L
  set.seed(boot_seed)
  boots <- replicate(boot_b, {
    idx <- sample.int(nrow(dat), nrow(dat), replace = TRUE)
    stat_fun(dat[idx, , drop = FALSE])
  })

  c(
    point = unname(point),
    low = unname(stats::quantile(boots, 0.025, na.rm = TRUE, type = 8)),
    high = unname(stats::quantile(boots, 0.975, na.rm = TRUE, type = 8)),
    p_one_sided = mean(boots <= 0, na.rm = TRUE)
  )
}
