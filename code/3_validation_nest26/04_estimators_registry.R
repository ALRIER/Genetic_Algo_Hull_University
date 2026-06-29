# =============================================================================
# ESTIMATOR REGISTRY AND ROBUST LOCATION FUNCTIONALS
# =============================================================================
# This module defines the estimator “building blocks” used by the GA to construct a weighted composite
# location estimator. Each component is implemented defensively so the pipeline remains stable under
# heavy contamination: estimators should not crash on pathological samples and should return a sensible
# fallback (typically the median) if an algorithm fails or yields non-finite outputs.
# =============================================================================


# Module tracking is defined in 00_utils_debug_io.R. This fallback only lets the file source on its own.
if (!exists("mark_module_done", mode = "function", inherits = TRUE)) {
  mark_module_done <- function(module_id, extra = NULL) invisible(TRUE)
  is_module_done   <- function(module_id) FALSE
}



# =============================================================================
# HELPER UTILITIES
# =============================================================================

# Computes the 25th, 50th, and 75th percentiles with stable defaults (type=8) and NA handling. These
# quartiles are used to construct robust summary statistics such as the trimean, which combines median
# and quartiles to reduce sensitivity to outliers while retaining reasonable efficiency under clean data.
.qs3 <- function(x) {
  stats::quantile(x, probs = c(0.25, 0.50, 0.75), names = FALSE, type = 8, na.rm = TRUE)
}


# =============================================================================
# ROBUST MEAN VARIANTS
# =============================================================================

# Harmonic mean is highly sensitive to zeros/negatives and non-finite values. This “safe” version restricts
# to positive finite observations; if none exist, it falls back to the sample median so the estimator can
# still be evaluated in contaminated regimes without returning Inf/NaN that would break fitness scoring.
harmonic_mean_safe <- function(x) {
  x <- as.numeric(x); if (!length(x)) return(NA_real_)
  xp <- x[is.finite(x) & x > 0]
  if (!length(xp)) return(stats::median(x, na.rm = TRUE))
  length(xp) / sum(1 / xp)
}

# Geometric mean requires positive values because it uses log(). This safe variant filters to positive finite
# entries and computes exp(mean(log(x))). If positivity is violated (common under some contaminations), it
# returns the sample median as a robust fallback rather than erroring or producing NaNs.
geometric_mean_safe <- function(x) {
  x <- as.numeric(x); if (!length(x)) return(NA_real_)
  xp <- x[is.finite(x) & x > 0]
  if (!length(xp)) return(stats::median(x, na.rm = TRUE))
  exp(base::mean(log(xp)))
}


# =============================================================================
# ROBUST MODE ESTIMATORS
# =============================================================================

# Half-sample mode (HSM) is a robust mode estimator designed to resist outliers. We wrap it in tryCatch
# because mode estimation can fail on degenerate inputs (e.g., all NA, length 0, extreme ties). If the
# result is non-finite, we fall back to the median to keep downstream GA evaluation numerically stable.
mode_hsm_safe <- function(x) {
  x <- as.numeric(x)
  out <- tryCatch(modeest::hsm(x), error = function(e) NA_real_)
  if (!is.finite(out)) stats::median(x, na.rm = TRUE) else out
}

# Parzen window-based mode estimation depends on kernel density-like procedures that can fail or produce
# unexpected outputs for small samples or pathological distributions. This wrapper forces numeric output,
# catches errors, and falls back to the median when needed so the estimator remains usable as a GA component.
mode_parzen_safe <- function(x) {
  x <- as.numeric(x)
  out <- tryCatch(as.numeric(modeest::mlv(x, method = "parzen")),
                  error = function(e) NA_real_)
  if (!is.finite(out)) stats::median(x, na.rm = TRUE) else out
}


# =============================================================================
# TRIMEAN AND M-ESTIMATORS
# =============================================================================

# Tukey’s trimean combines Q1, median, and Q3 with weights (1,2,1)/4. It is more robust than the mean
# (downweights extreme tails) but often more efficient than the median under clean data. It relies only
# on quantiles, making it stable under many contamination types used in this project.
trimean_safe <- function(x) {
  x <- as.numeric(x); q <- .qs3(x); (q[1] + 2*q[2] + q[3]) / 4
}

# Huber M-estimator: computed via robust regression (rlm) with Huber psi. This yields a location estimate
# that behaves like the mean near the center but limits the influence of extreme residuals. We use MAD
# scale estimation and cap iterations; failures revert to the sample median to avoid breaking the GA loop.
huber_mean_safe <- function(x) {
  x <- as.numeric(x)
  out <- tryCatch({
    fit <- MASS::rlm(x ~ 1, psi = MASS::psi.huber, scale.est = "MAD",
                     maxit = 50, na.action = na.omit)
    as.numeric(coef(fit)[1])
  }, error = function(e) NA_real_)
  if (!is.finite(out)) stats::median(x, na.rm = TRUE) else out
}

# Tukey biweight (bisquare) M-estimator: stronger downweighting of large residuals than Huber. This is
# useful under heavy contamination because extreme points can be almost fully ignored. As with huber_mean,
# we defensively catch failures and return the median when the robust regression cannot produce a finite fit.
biweight_mean_safe <- function(x) {
  x <- as.numeric(x)
  out <- tryCatch({
    fit <- MASS::rlm(x ~ 1, psi = MASS::psi.bisquare, scale.est = "MAD",
                     maxit = 50, na.action = na.omit)
    as.numeric(coef(fit)[1])
  }, error = function(e) NA_real_)
  if (!is.finite(out)) stats::median(x, na.rm = TRUE) else out
}


# =============================================================================
# EXPANDED VALIDATION BENCHMARK ESTIMATORS
# =============================================================================
finite_numeric <- function(x) {
  x <- as.numeric(x)
  x[is.finite(x)]
}

winsorized_mean_safe <- function(x, p = 0.10) {
  x <- finite_numeric(x)
  if (!length(x)) return(NA_real_)
  qs <- stats::quantile(x, probs = c(p, 1 - p), na.rm = TRUE, names = FALSE, type = 8)
  mean(pmin(pmax(x, qs[1]), qs[2]))
}

median_of_means_safe <- function(x, k = 10L) {
  x <- finite_numeric(x)
  n <- length(x)
  if (n == 0L) return(NA_real_)
  k <- max(1L, min(as.integer(k), n))
  idx <- split(seq_len(n), cut(seq_len(n), breaks = k, labels = FALSE))
  stats::median(vapply(idx, function(ii) mean(x[ii]), numeric(1)), na.rm = TRUE)
}

catoni_psi_safe <- function(u) {
  sign(u) * log1p(abs(u) + 0.5 * u^2)
}

catoni_mean_safe <- function(x, alpha = 0.20, maxit = 100L, tol = 1e-08) {
  x <- finite_numeric(x)
  if (!length(x)) return(NA_real_)
  if (length(unique(x)) == 1L) return(x[1])
  alpha <- as.numeric(alpha)
  if (!is.finite(alpha) || alpha <= 0) alpha <- 0.20
  f <- function(theta) sum(catoni_psi_safe(alpha * (x - theta)))
  sdx <- stats::sd(x)
  if (!is.finite(sdx)) sdx <- 0
  lo <- min(x) - sdx - 1
  hi <- max(x) + sdx + 1
  out <- tryCatch(stats::uniroot(f, interval = c(lo, hi), tol = tol, maxiter = maxit)$root,
                  error = function(e) NA_real_)
  if (!is.finite(out)) stats::median(x) else out
}

huber_tuned_mean_safe <- function(x, k = 1.345) {
  x <- finite_numeric(x)
  if (!length(x)) return(NA_real_)
  out <- tryCatch({
    fit <- MASS::rlm(x ~ 1, psi = MASS::psi.huber, k = k,
                     scale.est = "MAD", maxit = 50, na.action = na.omit)
    as.numeric(stats::coef(fit)[1])
  }, error = function(e) NA_real_)
  if (!is.finite(out)) stats::median(x) else out
}

# =============================================================================
# ESTIMATOR REGISTRY
# =============================================================================
# Maps human-readable estimator names to functions. This registry is the “single source of truth” for
# component definitions: the GA treats each entry as one feature in a convex combination, and downstream
# code relies on this ordering to interpret weights, export formulas, and ensure reproducibility across runs.
# =============================================================================

# The distribution family "invgauss" (Inverse Gaussian) is used
# elsewhere in this project (03_distributions_params.R, 06_data_prep.R) as a
# data-generating mechanism. It is NOT listed here because it is a probability
# distribution, not a location estimator. This registry contains only statistical
# functionals T_j(x) that map a sample to a scalar location estimate. There is
# no naming conflict: "invgauss" as a DGP and the estimators below are entirely
# separate concepts. Do not add invgauss here.
ESTIMATOR_REGISTRY <- list(
  mean        = function(x) base::mean(x, na.rm = TRUE),
  median      = function(x) stats::median(x, na.rm = TRUE),
  trimmed20   = function(x) base::mean(x, trim = 0.20, na.rm = TRUE),
  harmonic    = harmonic_mean_safe,
  geometric   = geometric_mean_safe,
  mode_hsm    = mode_hsm_safe,
  mode_parzen = mode_parzen_safe,
  trimean     = trimean_safe,
  huber       = huber_mean_safe,
  biweight    = biweight_mean_safe,
  winsorized_0.05 = function(x) winsorized_mean_safe(x, p = 0.05),
  winsorized_0.1  = function(x) winsorized_mean_safe(x, p = 0.10),
  winsorized_0.2  = function(x) winsorized_mean_safe(x, p = 0.20),
  mom_k5      = function(x) median_of_means_safe(x, k = 5L),
  mom_k10     = function(x) median_of_means_safe(x, k = 10L),
  mom_k20     = function(x) median_of_means_safe(x, k = 20L),
  catoni_a0.05 = function(x) catoni_mean_safe(x, alpha = 0.05),
  catoni_a0.1  = function(x) catoni_mean_safe(x, alpha = 0.10),
  catoni_a0.2  = function(x) catoni_mean_safe(x, alpha = 0.20),
  catoni_a0.35 = function(x) catoni_mean_safe(x, alpha = 0.35),
  catoni_a0.5  = function(x) catoni_mean_safe(x, alpha = 0.50),
  huber_k0.75  = function(x) huber_tuned_mean_safe(x, k = 0.75),
  huber_k1     = function(x) huber_tuned_mean_safe(x, k = 1.00),
  huber_k1.345 = function(x) huber_tuned_mean_safe(x, k = 1.345),
  huber_k1.75  = function(x) huber_tuned_mean_safe(x, k = 1.75),
  huber_k2     = function(x) huber_tuned_mean_safe(x, k = 2.00)
)

# Convenience metadata used throughout the project: estimator names define column labels, and N_EST is
# the dimensionality of the GA weight vector. Many parts of the pipeline (simplex normalization, warm-start
# matrices, perturbation tests) assume length(weights) == N_EST, so this central definition is critical.
ESTIMATOR_NAMES <- names(ESTIMATOR_REGISTRY)
N_EST <- length(ESTIMATOR_NAMES)


# =============================================================================
# ESTIMATOR METADATA, ADMISSIBILITY, AND TARGET COMPATIBILITY
# =============================================================================
# These helpers keep the estimator basis fixed (same N_EST and same columns) while
# enforcing theory-driven admissibility at the weight level. This is deliberately
# conservative: inadmissible estimators are not removed from the registry, because
# many downstream matrices assume a stable 10-column estimator basis. Instead, their
# weights are set to zero and the remaining weights are renormalized.

ESTIMATOR_METADATA <- data.frame(
  estimator = ESTIMATOR_NAMES,
  requires_positive = ESTIMATOR_NAMES %in% c("harmonic", "geometric"),
  target_role = c(
    mean        = "arithmetic_mean",
    median      = "robust_center",
    trimmed20   = "robust_arithmetic_mean",
    harmonic    = "reciprocal_center",
    geometric   = "multiplicative_center",
    mode_hsm    = "modal_center",
    mode_parzen = "modal_center",
    trimean     = "robust_center",
    huber       = "robust_arithmetic_mean",
    biweight    = "robust_arithmetic_mean"
  )[ESTIMATOR_NAMES],
  stringsAsFactors = FALSE,
  row.names = NULL
)

family_support_class <- function(distribution = NULL) {
  # Returns a coarse support label used only for estimator admissibility.
  # Unknown families are treated permissively to avoid breaking exploratory runs.
  if (is.null(distribution) || length(distribution) == 0L || is.na(distribution[1])) {
    return("unknown")
  }
  d <- tolower(as.character(distribution[1]))
  d <- gsub("[^a-z0-9_]+", "", d)
  if (d %in% c("normal", "gaussian", "exgaussian", "ex_gaussian")) {
    return("can_be_nonpositive")
  }
  if (d %in% c("lognormal", "weibull", "invgauss", "inversegaussian",
               "inverse_gaussian", "exwald", "ex_wald")) {
    return("strictly_positive")
  }
  "unknown"
}

family_allows_nonpositive <- function(distribution = NULL) {
  identical(family_support_class(distribution), "can_be_nonpositive")
}

admissible_estimator_mask <- function(distribution = NULL,
                                      target = "arithmetic_mean",
                                      strict_support = TRUE) {
  # A named TRUE/FALSE vector aligned with ESTIMATOR_NAMES.
  # Harmonic and geometric means are blocked when the family can
  # generate zero/negative values. Target-compatibility is recorded in metadata;
  # stronger target penalties belong to the fitness layer.
  mask <- rep(TRUE, N_EST)
  names(mask) <- ESTIMATOR_NAMES

  if (isTRUE(strict_support) && family_allows_nonpositive(distribution)) {
    positive_only <- ESTIMATOR_METADATA$estimator[isTRUE(TRUE) & ESTIMATOR_METADATA$requires_positive]
    mask[positive_only] <- FALSE
  }

  mask
}

apply_estimator_admissibility <- function(weights,
                                          distribution = NULL,
                                          target = "arithmetic_mean",
                                          strict_support = TRUE) {
  stopifnot(length(weights) == N_EST)
  w <- .normalize_simplex(as.numeric(weights))
  mask <- admissible_estimator_mask(distribution = distribution,
                                    target = target,
                                    strict_support = strict_support)
  w[!mask] <- 0
  if (!any(is.finite(w)) || sum(w, na.rm = TRUE) <= 0) {
    # Defensive fallback: if all weight was on blocked estimators, redistribute
    # uniformly over allowed estimators. If the family is unknown and everything
    # is somehow blocked, fall back to the full simplex.
    allowed <- which(mask)
    if (!length(allowed)) allowed <- seq_len(N_EST)
    w <- numeric(N_EST)
    w[allowed] <- 1 / length(allowed)
  } else {
    w <- .normalize_simplex(w)
  }
  names(w) <- ESTIMATOR_NAMES
  as.numeric(w)
}

estimator_admissibility_report <- function(distribution = NULL,
                                           target = "arithmetic_mean",
                                           strict_support = TRUE) {
  mask <- admissible_estimator_mask(distribution = distribution,
                                    target = target,
                                    strict_support = strict_support)
  data.frame(
    distribution = if (is.null(distribution)) NA_character_ else as.character(distribution[1]),
    support_class = family_support_class(distribution),
    target = target,
    estimator = ESTIMATOR_NAMES,
    allowed = as.logical(mask),
    requires_positive = ESTIMATOR_METADATA$requires_positive,
    target_role = ESTIMATOR_METADATA$target_role,
    stringsAsFactors = FALSE
  )
}


# =============================================================================
# GA COMPOSITE ESTIMATOR
# =============================================================================
# Defines the composite estimator optimized by the GA: it computes each registered component on the same
# sample, then returns a convex combination under weights normalized to the simplex. This ensures the final
# estimator is interpretable as a weighted average of named components. If any component fails, it is replaced
# by the sample median so the composite remains well-defined and fitness can be computed without NA cascades.
# =============================================================================

custom_estimator <- function(sample, weights, distribution = NULL, target = "arithmetic_mean") {
  stopifnot(length(weights) == N_EST)
  w <- apply_estimator_admissibility(as.numeric(weights), distribution = distribution, target = target)
  x <- as.numeric(sample)
  comps <- vapply(ESTIMATOR_REGISTRY, function(f) {
    out <- tryCatch(f(x), error = function(e) NA_real_)
    if (!is.finite(out)) stats::median(x, na.rm = TRUE) else out
  }, numeric(1))
  sum(w * comps)
}


# Converts weight vector into readable symbolic formula
# Produces a compact, human-readable representation of the GA solution by pairing each normalized weight
# with its estimator name (rounded for readability). This is used in logs and result tables so researchers
# can quickly interpret which components dominate and how the composite compares across distributions/stages.
weights_to_formula <- function(weights, distribution = NULL, target = "arithmetic_mean") {
  stopifnot(length(weights) == N_EST)
  w <- round(apply_estimator_admissibility(as.numeric(weights), distribution = distribution, target = target), 3)
  paste0(paste0(w, "*", ESTIMATOR_NAMES), collapse = " + ")
}
