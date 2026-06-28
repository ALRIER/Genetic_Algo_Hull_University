# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — EXPANDED ROBUST MEAN BENCHMARKS
# =============================================================================

q1_finite_x <- function(x) {
  x <- as.numeric(x)
  x[is.finite(x)]
}

q1_winsorized_mean <- function(x, p = 0.10) {
  x <- q1_finite_x(x); if (!length(x)) return(NA_real_)
  qs <- stats::quantile(x, probs = c(p, 1 - p), na.rm = TRUE, names = FALSE, type = 8)
  mean(pmin(pmax(x, qs[1]), qs[2]))
}

q1_median_of_means <- function(x, k = 10L) {
  x <- q1_finite_x(x); n <- length(x)
  if (n == 0L) return(NA_real_)
  k <- max(1L, min(as.integer(k), n))
  # Deterministic block split after random sample order has already been generated
  # by the scenario seed. This keeps CRN comparisons stable across estimators.
  idx <- split(seq_len(n), cut(seq_len(n), breaks = k, labels = FALSE))
  stats::median(vapply(idx, function(ii) mean(x[ii]), numeric(1)), na.rm = TRUE)
}

q1_catoni_psi <- function(u) {
  # Smooth bounded-influence score used by Catoni-type robust mean estimators.
  sign(u) * log1p(abs(u) + 0.5 * u^2)
}

q1_catoni_mean <- function(x, alpha = 0.20, maxit = 100L, tol = 1e-08) {
  x <- q1_finite_x(x); if (!length(x)) return(NA_real_)
  if (length(unique(x)) == 1L) return(x[1])
  alpha <- as.numeric(alpha)
  if (!is.finite(alpha) || alpha <= 0) alpha <- 0.20
  f <- function(theta) sum(q1_catoni_psi(alpha * (x - theta)))
  lo <- min(x) - stats::sd(x) - 1
  hi <- max(x) + stats::sd(x) + 1
  out <- tryCatch(stats::uniroot(f, interval = c(lo, hi), tol = tol, maxiter = maxit)$root,
                  error = function(e) NA_real_)
  if (!is.finite(out)) stats::median(x) else out
}

q1_huber_tuned <- function(x, k = 1.345) {
  x <- q1_finite_x(x); if (!length(x)) return(NA_real_)
  out <- tryCatch({
    fit <- MASS::rlm(x ~ 1, psi = MASS::psi.huber, k = k,
                     scale.est = "MAD", maxit = 50, na.action = na.omit)
    as.numeric(stats::coef(fit)[1])
  }, error = function(e) NA_real_)
  if (!is.finite(out)) stats::median(x) else out
}

q1_base_benchmarks <- function(x, distribution = NULL) {
  # Reuses the original estimator registry names where available.
  x <- q1_finite_x(x)
  out <- c(
    mean = mean(x),
    median = stats::median(x),
    trimmed20 = mean(x, trim = 0.20),
    trimean = {
      q <- stats::quantile(x, probs = c(0.25, 0.50, 0.75), names = FALSE, type = 8)
      (q[1] + 2 * q[2] + q[3]) / 4
    },
    huber = huber_mean_safe(x),
    biweight = biweight_mean_safe(x)
  )

  support <- tryCatch(family_support_class(distribution), error = function(e) "unknown")
  if (identical(support, "strictly_positive")) {
    out <- c(out,
             harmonic = harmonic_mean_safe(x),
             geometric = geometric_mean_safe(x))
  }
  out
}

q1_expanded_benchmarks <- function(x, config, distribution = NULL) {
  out <- q1_base_benchmarks(x, distribution = distribution)

  if (isTRUE(config$include_winsorized)) {
    for (p in config$winsor_probs) out[paste0("winsorized_", p)] <- q1_winsorized_mean(x, p = p)
  }
  if (isTRUE(config$include_median_of_means)) {
    for (k in config$mom_blocks) out[paste0("mom_k", k)] <- q1_median_of_means(x, k = k)
  }
  if (isTRUE(config$include_catoni)) {
    for (a in config$catoni_alpha_grid) out[paste0("catoni_a", a)] <- q1_catoni_mean(x, alpha = a)
  }
  if (isTRUE(config$include_huber_tuned)) {
    for (k in config$huber_k_grid) out[paste0("huber_k", k)] <- q1_huber_tuned(x, k = k)
  }
  if (isTRUE(config$include_equal_weight_composite)) {
    w <- rep(1 / N_EST, N_EST)
    out["equal_weight_composite"] <- custom_estimator(x, w, distribution = distribution)
  }

  out[is.finite(out)]
}
