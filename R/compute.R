# ---- Core computations ----

#' Calculate Hedges' J correction factor
#'
#' @param df Degrees of freedom
#' @return Correction factor
#' @keywords internal
hedges_J <- function(df) {
  if (is.na(df) || df <= 0) {
    return(NA_real_)
  }
  1 - (3 / (4 * df - 1))
}

#' Calculate Cohen's d from t-statistic (Independent Samples)
#'
#' @param t t-statistic
#' @param n1 Sample size 1
#' @param n2 Sample size 2
#' @return Cohen's d
#' @keywords internal
d_ind_from_t <- function(t, n1, n2) {
  if (is.na(t) || is.na(n1) || is.na(n2) || n1 <= 0 || n2 <= 0) {
    return(NA_real_)
  }
  t * sqrt(1 / n1 + 1 / n2)
}

#' Calculate Hedges' g from t-statistic
#'
#' @param t t-statistic
#' @param n1 Sample size 1
#' @param n2 Sample size 2
#' @return Hedges' g
#' @keywords internal
g_ind_from_t <- function(t, n1, n2) {
  df <- n1 + n2 - 2
  d <- d_ind_from_t(t, n1, n2)
  J <- hedges_J(df)
  d * J
}

# ---- CI Computation Framework ----

# CI computation result structure
# Returns list with: bounds (c(lower, upper)), method (character), success (logical), reason (character)
ci_result <- function(bounds = c(NA_real_, NA_real_), method = NA_character_,
                      success = FALSE, reason = NA_character_) {
  list(bounds = bounds, method = method, success = success, reason = reason)
}

# Check if effectsize package is available
has_effectsize <- function() {
  requireNamespace("effectsize", quietly = TRUE)
}

# CI computation priority system
# Priority 1: effectsize::confidence_interval()
# Priority 2: Analytic formulas
# Priority 3: Large-sample approximations
# Priority 4: Explicit failure with reason

#' Approximate CI for independent d
#'
#' Uses Hedges/CMC large-sample approximation.
#'
#' @param d Cohen's d
#' @param n1 Sample size 1
#' @param n2 Sample size 2
#' @param level Confidence level (default 0.95)
#' @return Vector of bounds (lower, upper)
#' @keywords internal
ci_d_ind_approx <- function(d, n1, n2, level = 0.95) {
  if (any(is.na(c(d, n1, n2)))) {
    return(c(NA_real_, NA_real_))
  }
  N <- n1 + n2
  se_d <- sqrt((N / (n1 * n2)) + (d^2 / (2 * (N - 2))))
  z <- qnorm(1 - (1 - level) / 2)
  c(d - z * se_d, d + z * se_d)
}

#' Noncentral t CI for independent d
#'
#' Uses noncentral t-distribution (via MBESS if available).
#'
#' @param d Cohen's d
#' @param n1 Sample size 1
#' @param n2 Sample size 2
#' @param level Confidence level (default 0.95)
#' @return Vector of bounds (lower, upper)
#' @keywords internal
ci_d_ind_noncentral_t <- function(d, n1, n2, level = 0.95) {
  if (any(is.na(c(d, n1, n2)))) {
    return(c(NA_real_, NA_real_))
  }

  # v0.3.1 (E1): Preempt MBESS noncentral-t overflow spam.
  # R's qt() / MBESS::conf.limits.nct iteratively prints a warning when |ncp|
  # exceeds ~37.62 (R's accuracy limit for the noncentral t-distribution).
  # At that regime the noncentral-t result is unreliable anyway, so we fall
  # through to the large-sample approximation rather than calling MBESS.
  ncp_val <- d * sqrt((n1 * n2) / (n1 + n2))
  if (!is.finite(ncp_val) || abs(ncp_val) > 37.62) {
    return(ci_d_ind_approx(d, n1, n2, level))
  }

  # Try MBESS if available
  if (requireNamespace("MBESS", quietly = TRUE)) {
    # Silence any stray print()/cat()/message()/warning() from MBESS internals
    # so batch stdout stays readable (MetaESCI E1).
    bounds <- tryCatch(
      {
        ci_res <- NULL
        utils::capture.output(
          ci_res <- suppressMessages(suppressWarnings(
            MBESS::ci.smd(
              ncp = ncp_val,
              n.1 = n1, n.2 = n2, conf.level = level
            )
          )),
          type = "output"
        )
        c(
          as.numeric(ci_res$Lower.Conf.Limit.smd),
          as.numeric(ci_res$Upper.Conf.Limit.smd)
        )
      },
      error = function(e) c(NA_real_, NA_real_)
    )
    if (!any(is.na(bounds))) {
      return(bounds)
    }
  }

  # Fallback to approximation
  return(ci_d_ind_approx(d, n1, n2, level))
}

#' Comprehensive CI computation for independent d
#'
#' Tries multiple methods in order of priority: effectsize -> noncentral t -> approximation.
#'
#' @param d Cohen's d
#' @param n1 Sample size 1
#' @param n2 Sample size 2
#' @param level Confidence level (default 0.95)
#' @param prefer_noncentral Logical, prefer noncentral t method
#' @return ci_result list
#' @keywords internal
ci_d_ind <- function(d, n1, n2, level = 0.95, prefer_noncentral = TRUE) {
  if (any(is.na(c(d, n1, n2)))) {
    return(ci_result(reason = "Missing required parameters (d, n1, n2)"))
  }

  # Priority 1: Try effectsize package
  if (has_effectsize()) {
    tryCatch(
      {
        # Would need to create effectsize object first
        # For now, skip this and use analytic methods
      },
      error = function(e) {
        # Fall through to next method
      }
    )
  }

  # Priority 2: Noncentral t (analytic, most accurate)
  if (prefer_noncentral) {
    bounds <- ci_d_ind_noncentral_t(d, n1, n2, level)
    if (!any(is.na(bounds))) {
      return(ci_result(bounds = bounds, method = "noncentral_t", success = TRUE))
    }
  }

  # Priority 3: Large-sample approximation
  bounds <- ci_d_ind_approx(d, n1, n2, level)
  if (!any(is.na(bounds))) {
    return(ci_result(bounds = bounds, method = "normal_approx", success = TRUE))
  }

  # Priority 4: Failure
  return(ci_result(reason = "No valid CI method available for independent d with given parameters"))
}

#' Calculate CI for Cohen's dz (paired design)
#'
#' Uses the noncentral t approach or large-sample approximation.
#' SE(dz) = sqrt(1/n + dz^2 / (2*n)) for the normal approximation.
#'
#' @param dz Cohen's dz
#' @param n Sample size (number of pairs)
#' @param level Confidence level (default 0.95)
#' @return ci_result list
#' @keywords internal
ci_dz <- function(dz, n, level = 0.95) {
  if (any(is.na(c(dz, n))) || n <= 1) {
    return(ci_result(reason = "Missing or invalid parameters for dz CI"))
  }

  # Noncentral-t inversion (Algina & Keselman 2003): find the ncp values
  # whose alpha/2 and 1 - alpha/2 quantiles equal the observed t = dz*sqrt(n).
  # dz CI = [ncp_lo, ncp_hi] / sqrt(n).
  bounds <- ci_dz_noncentral_t(dz, n, level)
  if (!any(is.na(bounds))) {
    return(ci_result(bounds = bounds, method = "noncentral_t", success = TRUE))
  }

  # Fallback: large-sample normal approximation
  se_dz <- sqrt(1 / n + dz^2 / (2 * n))
  z <- stats::qnorm(1 - (1 - level) / 2)
  bounds <- c(dz - z * se_dz, dz + z * se_dz)
  ci_result(bounds = bounds, method = "normal_approx", success = TRUE)
}

# Invert noncentral-t to get a CI for dz (Algina & Keselman 2003).
# Observed t = dz * sqrt(n), df = n - 1.
# Prefer MBESS::ci.sm when available (reference implementation); otherwise
# solve for ncp via uniroot, with a wide but bounded search window.
ci_dz_noncentral_t <- function(dz, n, level = 0.95) {
  df <- n - 1
  t_obs <- dz * sqrt(n)
  if (!is.finite(t_obs) || df < 1) {
    return(c(NA_real_, NA_real_))
  }

  if (requireNamespace("MBESS", quietly = TRUE)) {
    bounds <- tryCatch(
      {
        ci_res <- NULL
        utils::capture.output(
          ci_res <- suppressMessages(suppressWarnings(
            MBESS::ci.sm(sm = dz, N = n, conf.level = level)
          )),
          type = "output"
        )
        c(
          as.numeric(ci_res$Lower.Conf.Limit.Standardized.Mean),
          as.numeric(ci_res$Upper.Conf.Limit.Standardized.Mean)
        )
      },
      error = function(e) c(NA_real_, NA_real_)
    )
    if (!any(is.na(bounds))) {
      return(bounds)
    }
  }

  # uniroot fallback: find ncp_lo, ncp_hi such that
  #   qt(1 - alpha/2, df, ncp_lo) = t_obs  (lower bound)
  #   qt(    alpha/2, df, ncp_hi) = t_obs  (upper bound)
  alpha <- 1 - level
  search_hi <- max(50, abs(t_obs) * 5 + 20)
  f_lo <- function(nc) {
    suppressWarnings(stats::qt(1 - alpha / 2, df = df, ncp = nc)) - t_obs
  }
  f_hi <- function(nc) {
    suppressWarnings(stats::qt(alpha / 2, df = df, ncp = nc)) - t_obs
  }
  bounds <- tryCatch(
    {
      lo <- stats::uniroot(
        f_lo,
        interval = c(-search_hi, search_hi), extendInt = "yes"
      )$root
      hi <- stats::uniroot(
        f_hi,
        interval = c(-search_hi, search_hi), extendInt = "yes"
      )$root
      c(lo, hi) / sqrt(n)
    },
    error = function(e) c(NA_real_, NA_real_)
  )
  bounds
}

#' Calculate Cohen's dz from t-statistic (Paired)
#'
#' @param t t-statistic
#' @param n Sample size (number of pairs)
#' @return Cohen's dz
#' @keywords internal
dz_from_t <- function(t, n) {
  if (any(is.na(c(t, n))) || n < 2) {
    return(NA_real_)
  }
  t / sqrt(n)
}

#' Convert dz to dav (Cohen's d for average variance)
#'
#' @param dz Cohen's dz
#' @param r Correlation between measures
#' @return Cohen's dav
#' @keywords internal
dav_from_dz <- function(dz, r) {
  if (any(is.na(c(dz, r))) || r < 0 || r >= 1) {
    return(NA_real_)
  }
  dz / sqrt(2 * (1 - r))
}

#' Convert dz to drm (Cohen's d for raw means)
#'
#' @param dz Cohen's dz
#' @param r Correlation (unused, for interface compatibility)
#' @return Cohen's drm
#' @keywords internal
drm_from_dz <- function(dz, r = NA_real_) {
  if (is.na(dz)) {
    return(NA_real_)
  }
  # drm (repeated measures d, Morris & DeShon 2002) = dz * sqrt(2*(1-r))
  # Without correlation, return NA since the conversion is not possible
  if (is.na(r) || r >= 1) {
    return(NA_real_)
  }
  dz * sqrt(2 * (1 - r))
}

#' Compute range of plausible dav values
#'
#' Calculates dav across a grid of possible correlations.
#'
#' @param dz Cohen's dz
#' @param r_grid Vector of correlations to test
#' @return List with min, max, median, and values
#' @keywords internal
compute_dav_range <- function(dz, r_grid = c(seq(0.1, 0.9, by = 0.1), 0.95)) {
  if (is.na(dz)) {
    return(list(
      min = NA_real_,
      max = NA_real_,
      median = NA_real_,
      values = numeric(0)
    ))
  }

  dav_values <- sapply(r_grid, function(r) dav_from_dz(dz, r))
  dav_values <- dav_values[!is.na(dav_values)]

  if (length(dav_values) == 0) {
    return(list(
      min = NA_real_,
      max = NA_real_,
      median = NA_real_,
      values = numeric(0)
    ))
  }

  list(
    min = min(dav_values),
    max = max(dav_values),
    median = stats::median(dav_values),
    values = dav_values
  )
}

#' Calculate correlation r from t-statistic
#'
#' @param t t-statistic
#' @param df Degrees of freedom
#' @return Correlation r
#' @keywords internal
r_from_t <- function(t, df) {
  if (any(is.na(c(t, df))) || df <= 0) {
    return(NA_real_)
  }
  t / sqrt(t^2 + df)
}

#' Fisher's z-transformation CI for correlation
#'
#' @param r Correlation coefficient
#' @param n Sample size
#' @param level Confidence level
#' @return Vector of bounds (lower, upper)
#' @keywords internal
fisher_ci_r <- function(r, n, level = 0.95) {
  if (any(is.na(c(r, n))) || n <= 3 || abs(r) >= 1) {
    return(c(NA_real_, NA_real_))
  }
  z <- atanh(r)
  se <- 1 / sqrt(n - 3)
  z_crit <- qnorm(1 - (1 - level) / 2)
  lo <- z - z_crit * se
  hi <- z + z_crit * se
  c(tanh(lo), tanh(hi))
}

#' Comprehensive CI computation for correlation
#'
#' @param r Correlation coefficient
#' @param n Sample size
#' @param level Confidence level
#' @return ci_result list
#' @keywords internal
ci_r <- function(r, n, level = 0.95) {
  if (any(is.na(c(r, n)))) {
    return(ci_result(reason = "Missing required parameters (r, n)"))
  }

  if (n <= 3 || abs(r) >= 1) {
    return(ci_result(reason = "Invalid parameters: n must be > 3 and |r| < 1"))
  }

  # Priority 1: Try effectsize package
  if (has_effectsize()) {
    tryCatch(
      {
        # Would need effectsize object
        # Skip for now, use analytic method
      },
      error = function(e) {
        # Fall through
      }
    )
  }

  # Priority 2: Fisher z-transform (analytic, standard method)
  bounds <- fisher_ci_r(r, n, level)
  if (!any(is.na(bounds))) {
    return(ci_result(bounds = bounds, method = "fisher_z", success = TRUE))
  }

  # Priority 4: Failure
  return(ci_result(reason = "No valid CI method available for correlation r"))
}

#' Calculate phi coefficient from Chi-square
#'
#' @param chisq Chi-square statistic
#' @param N Total sample size
#' @return Phi coefficient
#' @keywords internal
phi_from_chisq <- function(chisq, N) {
  if (any(is.na(c(chisq, N))) || N <= 0) {
    return(NA_real_)
  }
  sqrt(chisq / N)
}

#' Calculate Cramer's V from Chi-square
#'
#' @param chisq Chi-square statistic
#' @param N Total sample size
#' @param m Smaller dimension - 1 (min(r-1, c-1))
#' @return Cramer's V
#' @keywords internal
V_from_chisq <- function(chisq, N, m) {
  if (any(is.na(c(chisq, N, m))) || N <= 0 || m <= 0) {
    return(NA_real_)
  }
  sqrt(chisq / (N * m))
}

# For 2x2 tables, phi equals Pearson r. Use Fisher CI as a pragmatic default.
phi_ci_fisher <- function(phi, N, level = 0.95) {
  fisher_ci_r(phi, N, level)
}

# Bonett-Price CI for Cramer's V
# This is an approximation; exact method is complex
bonett_price_ci_V <- function(V, N, m, level = 0.95) {
  if (any(is.na(c(V, N, m))) || N <= 0 || m <= 0) {
    return(c(NA_real_, NA_real_))
  }
  if (V < 0 || V >= 1) {
    return(c(NA_real_, NA_real_))
  }

  # Bonett-Price method for Cramer's V
  # Simplified version - full implementation would be more complex
  # Use logit transformation approximation
  if (V == 0) {
    return(c(0, 0))
  }
  if (V >= 1) {
    return(c(NA_real_, NA_real_))
  }

  # Approximation using normal approximation on transformed scale
  z_crit <- qnorm(1 - (1 - level) / 2)
  se_V <- sqrt((1 - V^2) / (N * m))
  c(max(0, V - z_crit * se_V), min(1, V + z_crit * se_V))
}

# Comprehensive CI computation for phi with method tracking
ci_phi <- function(phi, N, level = 0.95) {
  if (any(is.na(c(phi, N)))) {
    return(ci_result(reason = "Missing required parameters (phi, N)"))
  }

  if (N <= 0) {
    return(ci_result(reason = "Invalid parameter: N must be > 0"))
  }

  # Priority 2: Fisher z-transform (treating phi as correlation for 2x2)
  bounds <- phi_ci_fisher(phi, N, level)
  if (!any(is.na(bounds))) {
    return(ci_result(bounds = bounds, method = "fisher_z", success = TRUE))
  }

  # Priority 4: Failure
  return(ci_result(reason = "No valid CI method available for phi"))
}

# Comprehensive CI computation for Cramer's V with method tracking
ci_V <- function(V, N, m, level = 0.95) {
  if (any(is.na(c(V, N, m)))) {
    return(ci_result(reason = "Missing required parameters (V, N, m)"))
  }

  if (N <= 0 || m <= 0) {
    return(ci_result(reason = "Invalid parameters: N and m must be > 0"))
  }

  # Priority 2: Bonett-Price method (analytic)
  bounds <- bonett_price_ci_V(V, N, m, level)
  if (!any(is.na(bounds))) {
    return(ci_result(bounds = bounds, method = "bonett_price", success = TRUE))
  }

  # Priority 3: Large-sample approximation (fallback)
  z_crit <- qnorm(1 - (1 - level) / 2)
  se_V <- sqrt((1 - V^2) / (N * m))
  bounds_approx <- c(max(0, V - z_crit * se_V), min(1, V + z_crit * se_V))
  if (!any(is.na(bounds_approx))) {
    return(ci_result(bounds = bounds_approx, method = "normal_approx", success = TRUE))
  }

  # Priority 4: Failure
  return(ci_result(reason = "No valid CI method available for Cramer's V"))
}

# ============================================================================
# Multi-method CI computation functions
# Return ALL successful CI methods (not just the first) for multi-method matching.
# ============================================================================

#' Compute CIs for independent d via all available methods
#' @param d Cohen's d
#' @param n1 Sample size 1
#' @param n2 Sample size 2
#' @param level Confidence level
#' @return Named list of ci_result objects (one per successful method)
#' @keywords internal
ci_d_ind_all <- function(d, n1, n2, level = 0.95) {
  if (any(is.na(c(d, n1, n2)))) return(list())
  results <- list()
  # Method 1: Noncentral t (most accurate)
  bounds_nct <- tryCatch(ci_d_ind_noncentral_t(d, n1, n2, level),
                         error = function(e) c(NA_real_, NA_real_))
  if (!any(is.na(bounds_nct)))
    results$noncentral_t <- ci_result(bounds = bounds_nct, method = "noncentral_t", success = TRUE)
  # Method 2: Normal approximation (symmetric)
  bounds_na <- ci_d_ind_approx(d, n1, n2, level)
  if (!any(is.na(bounds_na)))
    results$normal_approx <- ci_result(bounds = bounds_na, method = "normal_approx", success = TRUE)
  results
}

#' Compute CIs for R-squared via partial-eta-squared NCP F-inversion (v0.3.5)
#'
#' R^2 in a single-omnibus regression (or a one-predictor model) is
#' mathematically identical to partial eta-squared. We route through
#' ci_etap2_all() and tag the method names so the matcher distinguishes
#' R^2-routed CIs from native eta_p^2 CIs. Caller supplies the F statistic
#' (when available) or back-computes from R^2 + df1 + df2.
#'
#' @param R2 R-squared (point estimate, in [0, 1))
#' @param df1 Numerator df (number of predictors)
#' @param df2 Denominator df (residual df, N - df1 - 1)
#' @param F_val Optional F statistic; computed from R2/df1/df2 if NA
#' @param level Confidence level
#' @return Named list of ci_result objects with R2-tagged methods
#' @keywords internal
ci_R2_all <- function(R2, df1, df2, F_val = NA_real_, level = 0.95) {
  if (is.na(R2) || is.na(df1) || is.na(df2)) return(list())
  if (R2 < 0 || R2 >= 1 || df1 <= 0 || df2 <= 0) return(list())
  if (is.na(F_val)) {
    F_val <- (R2 / df1) / ((1 - R2) / df2)
  }
  res <- ci_etap2_all(F_val, df1, df2, level)
  if (length(res) == 0L) return(list())
  for (m in names(res)) {
    res[[m]]$method <- paste0(res[[m]]$method, "_via_etap2")
  }
  names(res) <- paste0(names(res), "_via_etap2")
  res
}

#' Compute CIs for odds ratio via all available methods (v0.3.5)
#'
#' Implements Wald CI on log(OR) (the standard psychology-paper method) and
#' optionally an exact Fisher CI when 2x2 cell counts are supplied.
#'
#' Wald-on-log uses SE_logOR if supplied, otherwise back-derives it from the
#' reported CI bounds when both are available, otherwise estimates it from
#' the reported p-value (when p > 0). If none of those are available it
#' returns an empty list — there is no information to construct a CI from
#' the point estimate alone.
#'
#' @param OR Odds ratio (point estimate)
#' @param SE_logOR Standard error of log(OR), if known
#' @param level Confidence level (default 0.95)
#' @param cells Numeric length-4 vector c(a, b, c, d) for 2x2 table (optional)
#' @param p_value Reported p-value (optional, used to back-derive SE)
#' @return Named list of ci_result objects
#' @keywords internal
ci_OR_all <- function(OR, SE_logOR = NULL, level = 0.95,
                      cells = NULL, p_value = NULL) {
  if (is.na(OR) || OR <= 0 || !is.finite(OR)) return(list())
  results <- list()
  z <- stats::qnorm(1 - (1 - level) / 2)
  log_or <- log(OR)

  # Method 1: Wald CI on log(OR) using supplied SE
  if (!is.null(SE_logOR) && length(SE_logOR) == 1L &&
      !is.na(SE_logOR) && is.finite(SE_logOR) && SE_logOR > 0) {
    bounds <- c(exp(log_or - z * SE_logOR), exp(log_or + z * SE_logOR))
    results$wald_log <- ci_result(bounds = bounds, method = "wald_log", success = TRUE)
  }

  # Method 2: Fisher exact CI from 2x2 cells (when supplied)
  if (!is.null(cells) && length(cells) == 4L &&
      all(is.finite(cells)) && all(cells >= 0)) {
    ft <- tryCatch(
      stats::fisher.test(matrix(cells, nrow = 2L), conf.level = level),
      error = function(e) NULL
    )
    if (!is.null(ft) && !is.null(ft$conf.int)) {
      bounds <- c(ft$conf.int[1], ft$conf.int[2])
      results$fisher_exact <- ci_result(bounds = bounds, method = "fisher_exact", success = TRUE)
    }
  }

  # Method 3: Back-derive SE from p-value (Wald inversion).
  # If p comes from a two-sided test of H0: log(OR) = 0, then z* = log(OR)/SE
  # and SE = |log(OR)| / qnorm(1 - p/2). Only valid for nonzero log(OR) and p in (0,1).
  if (length(results) == 0L && !is.null(p_value) && length(p_value) == 1L &&
      !is.na(p_value) && p_value > 0 && p_value < 1 && abs(log_or) > 1e-9) {
    z_obs <- stats::qnorm(1 - p_value / 2)
    if (is.finite(z_obs) && z_obs > 0) {
      se_est <- abs(log_or) / z_obs
      bounds <- c(exp(log_or - z * se_est), exp(log_or + z * se_est))
      results$wald_log_from_p <- ci_result(
        bounds = bounds, method = "wald_log_from_p", success = TRUE
      )
    }
  }

  results
}

#' Compute CIs for paired dz via all available methods
#' @param dz Cohen's dz
#' @param n Sample size (number of pairs)
#' @param level Confidence level
#' @return Named list of ci_result objects
#' @keywords internal
ci_dz_all <- function(dz, n, level = 0.95) {
  if (any(is.na(c(dz, n))) || n <= 1) return(list())
  results <- list()
  # Method 1: Noncentral-t inversion (Algina & Keselman 2003)
  bounds_nc <- ci_dz_noncentral_t(dz, n, level)
  if (!any(is.na(bounds_nc))) {
    results$noncentral_t <- ci_result(
      bounds = bounds_nc, method = "noncentral_t", success = TRUE
    )
  }
  # Method 2: Normal approximation (symmetric)
  se_dz <- sqrt(1 / n + dz^2 / (2 * n))
  z <- stats::qnorm(1 - (1 - level) / 2)
  bounds_na <- c(dz - z * se_dz, dz + z * se_dz)
  results$normal_approx <- ci_result(bounds = bounds_na, method = "normal_approx", success = TRUE)
  results
}

#' Compute CIs for partial eta-squared at multiple confidence levels
#' @param F_val F statistic
#' @param df1 df1
#' @param df2 df2
#' @param level Primary confidence level (default 0.95)
#' @return Named list of ci_result objects (95% + 90% per Steiger 2004)
#' @keywords internal
ci_etap2_all <- function(F_val, df1, df2, level = 0.95) {
  if (any(is.na(c(F_val, df1, df2)))) return(list())
  results <- list()
  # Method 1: NCP F-inversion at primary level
  res <- ci_etap2(F_val, df1, df2, level)
  if (res$success) results$ncf_inversion <- res
  # Method 2: NCP F-inversion at 90% (Steiger 2004 recommendation for eta2)
  if (level != 0.90) {
    res_90 <- ci_etap2(F_val, df1, df2, 0.90)
    if (res_90$success) {
      res_90$method <- "ncf_90pct"
      results$ncf_90pct <- res_90
    }
  }
  results
}

# Enumerate plausible m = min(r-1, c-1) given df
enumerate_m_from_df <- function(df) {
  if (is.na(df) || df < 1) {
    return(integer(0))
  }
  # df = (r-1)(c-1). For small df, plausible m values are all divisors' minima.
  ms <- c()
  for (a in 1:(df)) {
    if ((df %% a) == 0) {
      b <- df / a
      ms <- c(ms, min(a, b))
    }
  }
  sort(unique(ms))
}

# ---- ANOVA / F-test effect sizes ----

# Eta-squared (\u03b7\u00b2) from F-statistic
# Formula: \u03b7\u00b2 = (F * df1) / (F * df1 + df2)
# This is the standard eta-squared for between-subjects designs
eta2_from_F <- function(F_val, df1, df2) {
  # Ensure scalar inputs
  F_val <- as.numeric(F_val[1])
  df1 <- as.numeric(df1[1])
  df2 <- as.numeric(df2[1])

  if (any(is.na(c(F_val, df1, df2))) || F_val < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  (F_val * df1) / (F_val * df1 + df2)
}

# Partial eta-squared (\u03b7p\u00b2) from F-statistic
# Formula: \u03b7p\u00b2 = (F * df1) / (F * df1 + df2)
# Note: For one-way ANOVA, partial eta^2 = eta^2
# For factorial designs, partial eta^2 controls for other factors
partial_eta2_from_F <- function(F_val, df1, df2) {
  # Ensure scalar inputs
  F_val <- as.numeric(F_val[1])
  df1 <- as.numeric(df1[1])
  df2 <- as.numeric(df2[1])

  if (any(is.na(c(F_val, df1, df2))) || F_val < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  # Partial eta^2 formula is the same as eta^2 for one-way designs
  # For factorial designs, this would need SS_effect and SS_error explicitly
  # Here we use the approximation that works for one-way and some factorial cases
  (F_val * df1) / (F_val * df1 + df2)
}

# Generalized eta-squared (eta_G^2) from F-statistic
# Formula: eta_G^2 = SS_effect / (SS_effect + SS_error + SS_subjects)
# This requires more information than just F, df1, df2
# Approximation for between-subjects: same as eta^2
# For within-subjects: requires knowledge of design structure
generalized_eta2_from_F <- function(F_val, df1, df2, design = "between") {
  # Ensure scalar inputs
  F_val <- as.numeric(F_val[1])
  df1 <- as.numeric(df1[1])
  df2 <- as.numeric(df2[1])

  if (any(is.na(c(F_val, df1, df2))) || F_val < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  # For between-subjects, generalized eta^2 ~= eta^2
  if (design == "between") {
    return(eta2_from_F(F_val, df1, df2))
  }
  # For within-subjects, we need more information (SS_subjects)
  # Return NA with note that more info needed
  NA_real_
}

# Omega-squared (omega^2) from F-statistic
# Formula: omega^2 = (F * df1 - df1) / (F * df1 + df2 + 1)
# This is a less biased estimate than eta^2
omega2_from_F <- function(F_val, df1, df2) {
  # Ensure scalar inputs
  F_val <- as.numeric(F_val[1])
  df1 <- as.numeric(df1[1])
  df2 <- as.numeric(df2[1])

  if (any(is.na(c(F_val, df1, df2))) || F_val < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  numerator <- (F_val * df1 - df1)
  denominator <- (F_val * df1 + df2 + 1)
  if (denominator <= 0) {
    return(NA_real_)
  }
  result <- numerator / denominator
  # Omega\u00b2 can be negative (indicating very small effect)
  # Clamp to 0 for interpretability, but could return negative
  max(0, result)
}

# Epsilon-squared from F-statistic (Kelley formula for ANOVA)
# Less biased than eta2; default in JASP/jamovi
# Formula: epsilon2 = (F * df1 - df1) / (F * df1 + df2)
# Compare: omega2 = (F * df1 - df1) / (F * df1 + df2 + 1)
epsilon2_anova_from_F <- function(F_val, df1, df2) {
  F_val <- as.numeric(F_val[1])
  df1 <- as.numeric(df1[1])
  df2 <- as.numeric(df2[1])
  if (any(is.na(c(F_val, df1, df2))) || F_val < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  numerator <- F_val * df1 - df1
  denominator <- F_val * df1 + df2
  if (denominator <= 0) return(NA_real_)
  max(0, numerator / denominator)
}

# Partial omega-squared from F-statistic (for factorial ANOVA)
# Used by SPSS for multi-factor designs
# Formula: partial_omega2 = (df1 * (F - 1)) / (df1 * (F - 1) + N)
# where N = df1 + df2 + 1 (total sample size for one-way)
partial_omega2_from_F <- function(F_val, df1, df2) {
  F_val <- as.numeric(F_val[1])
  df1 <- as.numeric(df1[1])
  df2 <- as.numeric(df2[1])
  if (any(is.na(c(F_val, df1, df2))) || F_val < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  N <- df1 + df2 + 1
  numerator <- df1 * (F_val - 1)
  denominator <- df1 * (F_val - 1) + N
  if (denominator <= 0) return(NA_real_)
  max(0, numerator / denominator)
}

# Bias-corrected eta-squared (epsilon-hat-squared)
# Small-sample correction for eta2; used by R effectsize package
# Formula: epsilon_hat2 = 1 - (1 - eta2) * (N - 1) / (N - df1 - 1)
bias_corrected_eta2 <- function(eta2, N, df1) {
  if (any(is.na(c(eta2, N, df1))) || eta2 < 0 || eta2 > 1 ||
      N <= df1 + 1) {
    return(NA_real_)
  }
  result <- 1 - (1 - eta2) * (N - 1) / (N - df1 - 1)
  max(0, result)
}

# Cohen's f from F-statistic
# Formula: f = sqrt(\u03b7\u00b2 / (1 - \u03b7\u00b2))
# Or directly: f = sqrt((F * df1) / df2)
cohens_f_from_F <- function(F_val, df1, df2) {
  # Ensure scalar inputs
  F_val <- as.numeric(F_val[1])
  df1 <- as.numeric(df1[1])
  df2 <- as.numeric(df2[1])

  if (any(is.na(c(F_val, df1, df2))) || F_val < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  # Direct formula
  sqrt((F_val * df1) / df2)
}

# Cohen's f from eta-squared
cohens_f_from_eta2 <- function(eta2) {
  if (is.na(eta2) || eta2 < 0 || eta2 >= 1) {
    return(NA_real_)
  }
  sqrt(eta2 / (1 - eta2))
}

# Cohen's f from omega-squared
# Some software (G*Power, JASP) derives f from omega2 rather than eta2
# Since omega2 < eta2, f_omega < f_eta for the same F-test
cohens_f_from_omega2 <- function(omega2) {
  if (is.na(omega2) || omega2 < 0 || omega2 >= 1) {
    return(NA_real_)
  }
  sqrt(omega2 / (1 - omega2))
}

# Compute all ANOVA effect size variants when design is unclear
# Returns a list with all computed values
compute_all_anova_effects <- function(F_val, df1, df2, design = "unclear") {
  results <- list(
    eta = NA_real_,
    eta2 = NA_real_,
    partial_eta2 = NA_real_,
    generalized_eta2 = NA_real_,
    omega2 = NA_real_,
    cohens_f = NA_real_
  )

  if (any(is.na(c(F_val, df1, df2)))) {
    return(results)
  }

  # Always compute these (design-independent)
  results$eta2 <- eta2_from_F(F_val, df1, df2)
  if (!is.na(results$eta2)) {
    results$eta <- sqrt(results$eta2)
  }
  results$partial_eta2 <- partial_eta2_from_F(F_val, df1, df2)
  results$omega2 <- omega2_from_F(F_val, df1, df2)
  results$cohens_f <- cohens_f_from_F(F_val, df1, df2)

  # Generalized eta^2 depends on design
  if (design == "between") {
    results$generalized_eta2 <- generalized_eta2_from_F(F_val, df1, df2, "between")
  } else if (design == "within") {
    # Would need SS_subjects for accurate computation
    results$generalized_eta2 <- NA_real_
  } else if (design == "mixed") {
    # Would need SS_subjects and design structure
    results$generalized_eta2 <- NA_real_
  } else {
    # Design unclear - compute what we can
    results$generalized_eta2 <- generalized_eta2_from_F(F_val, df1, df2, "between")
  }

  results
}

# ---- Regression & GLM effect sizes ----

# Standardized beta (beta) from t-statistic
# Formula: beta = t / sqrt(t^2 + df)
# This is an approximation; exact beta requires raw coefficients and SDs
standardized_beta_from_t <- function(t, df) {
  if (any(is.na(c(t, df))) || df <= 0) {
    return(NA_real_)
  }
  t / sqrt(t^2 + df)
}

# Partial correlation (r_partial) from t-statistic
# Formula: r_partial = t / sqrt(t^2 + df)
# This is the same as standardized beta for simple regression
partial_r_from_t <- function(t, df) {
  if (any(is.na(c(t, df))) || df <= 0) {
    return(NA_real_)
  }
  t / sqrt(t^2 + df)
}

#' Wald-on-beta CI for standardized beta (v0.3.5)
#'
#' Normal approximation: beta +/- z * SE_beta. SE is supplied directly when
#' the paper reports SE alongside b/beta; otherwise back-derive from t and df
#' via SE_beta_approx = beta / t (valid when t = beta / SE_beta).
#'
#' @param beta Standardized regression coefficient
#' @param SE_beta Standard error of standardized beta (optional)
#' @param t_stat Test statistic (optional, used to back-derive SE)
#' @param df Residual df (optional, used to bound back-derived SE)
#' @param level Confidence level
#' @return Named list of ci_result objects
#' @keywords internal
ci_standardized_beta_all <- function(beta, SE_beta = NULL,
                                     t_stat = NULL, df = NULL,
                                     level = 0.95) {
  if (is.na(beta) || !is.finite(beta)) return(list())
  results <- list()
  z <- stats::qnorm(1 - (1 - level) / 2)
  if (!is.null(SE_beta) && length(SE_beta) == 1L &&
      !is.na(SE_beta) && is.finite(SE_beta) && SE_beta > 0) {
    bounds <- c(beta - z * SE_beta, beta + z * SE_beta)
    results$normal_approx <- ci_result(bounds = bounds, method = "normal_approx", success = TRUE)
  } else if (!is.null(t_stat) && length(t_stat) == 1L &&
             !is.na(t_stat) && abs(t_stat) > 1e-9) {
    se_est <- abs(beta) / abs(t_stat)
    bounds <- c(beta - z * se_est, beta + z * se_est)
    results$normal_approx_from_t <- ci_result(bounds = bounds, method = "normal_approx_from_t", success = TRUE)
  }
  results
}

#' Fisher-z CI for partial correlation (v0.3.5)
#'
#' Same Fisher-z transform used in ci_phi(); df gives N - k - 1.
#' Treats the partial r as a correlation with effective N = df + 2 (i.e. as
#' if there were no covariates), which is the standard simplification used
#' in psychology software when the full residual df is supplied.
#'
#' @param r Partial r
#' @param df Residual df
#' @param level Confidence level
#' @return Named list of ci_result objects
#' @keywords internal
ci_partial_r_all <- function(r, df, level = 0.95) {
  if (is.na(r) || is.na(df) || !is.finite(r) || df <= 1) return(list())
  if (abs(r) >= 1) return(list())
  N_eff <- df + 2L
  z_r <- atanh(r)
  se_z <- 1 / sqrt(N_eff - 3)
  z <- stats::qnorm(1 - (1 - level) / 2)
  bounds <- c(tanh(z_r - z * se_z), tanh(z_r + z * se_z))
  list(fisher_z = ci_result(bounds = bounds, method = "fisher_z", success = TRUE))
}

#' Fisher-z CI for semi-partial correlation (v0.3.5)
#'
#' Same Fisher-z transform applied to the semi-partial r value (an
#' approximation; the exact distribution of semi-partial r depends on the
#' full covariance structure). Documented as an approximation.
#'
#' @param r Semi-partial r
#' @param df Residual df
#' @param level Confidence level
#' @return Named list of ci_result objects
#' @keywords internal
ci_semi_partial_r_all <- function(r, df, level = 0.95) {
  if (is.na(r) || is.na(df) || !is.finite(r) || df <= 1) return(list())
  if (abs(r) >= 1) return(list())
  N_eff <- df + 2L
  z_r <- atanh(r)
  se_z <- 1 / sqrt(N_eff - 3)
  z <- stats::qnorm(1 - (1 - level) / 2)
  bounds <- c(tanh(z_r - z * se_z), tanh(z_r + z * se_z))
  list(fisher_z_approx = ci_result(bounds = bounds, method = "fisher_z_approx", success = TRUE))
}

# Semi-partial correlation (r_semi) from t-statistic and R^2
# Formula: r_semi = t / sqrt(t^2 + df) * sqrt(1 - R^2_other)
# Requires R^2 of model without this predictor
# Approximation: if R^2_other unknown, use r_semi ~= r_partial * sqrt(1 - R^2_full)
semi_partial_r_from_t <- function(t, df, R2_full = NA_real_, R2_other = NA_real_) {
  if (any(is.na(c(t, df))) || df <= 0) {
    return(NA_real_)
  }

  r_partial <- partial_r_from_t(t, df)

  # If we have R^2 information, use it
  if (!is.na(R2_full) && !is.na(R2_other)) {
    return(r_partial * sqrt(1 - R2_other))
  } else if (!is.na(R2_full)) {
    # Approximation: assume this predictor explains some of R^2
    # Conservative: use R^2_full as upper bound
    return(r_partial * sqrt(1 - R2_full))
  } else {
    # No R^2 info - return NA
    return(NA_real_)
  }
}

# Cohen's f\u00b2 from R\u00b2
# Formula: f^2 = R^2 / (1 - R^2)
# Adjusted R-squared from R-squared
# Formula: R²_adj = 1 - (1-R²)(N-1)/(N-k-1) where k = number of predictors (df1)
# N estimated from F-test as df1 + df2 + 1
#' @keywords internal
adjusted_R2_from_R2 <- function(R2, N, k) {
  if (any(is.na(c(R2, N, k))) || N <= k + 1 || R2 < 0 || R2 > 1) return(NA_real_)
  1 - (1 - R2) * (N - 1) / (N - k - 1)
}

cohens_f2_from_R2 <- function(R2) {
  if (is.na(R2) || R2 < 0 || R2 >= 1) {
    return(NA_real_)
  }
  if (R2 == 1) {
    return(Inf)
  } # Perfect fit
  R2 / (1 - R2)
}

# Cohen's f^2 from F-statistic and df
# Formula: f\u00b2 = (F * df1) / df2
cohens_f2_from_F <- function(F_val, df1, df2) {
  if (any(is.na(c(F_val, df1, df2))) || F_val < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  (F_val * df1) / df2
}

# R\u00b2 from F-statistic
# Formula: R^2 = (F * df1) / (F * df1 + df2)
# This is the same as eta^2 for regression models
R2_from_F <- function(F_val, df1, df2) {
  if (any(is.na(c(F_val, df1, df2))) || F_val < 0 || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }
  (F_val * df1) / (F_val * df1 + df2)
}

# Compute regression effect sizes when available
# Returns a list with computed values
compute_regression_effects <- function(t = NA_real_, df = NA_real_,
                                       F_val = NA_real_, df1 = NA_real_, df2 = NA_real_,
                                       R2 = NA_real_) {
  results <- list(
    standardized_beta = NA_real_,
    partial_r = NA_real_,
    semi_partial_r = NA_real_,
    cohens_f2 = NA_real_,
    R2 = NA_real_
  )

  # Compute from t if available
  if (!is.na(t) && !is.na(df)) {
    results$standardized_beta <- standardized_beta_from_t(t, df)
    results$partial_r <- partial_r_from_t(t, df)
  }

  # Compute from F if available
  if (!is.na(F_val) && !is.na(df1) && !is.na(df2)) {
    results$cohens_f2 <- cohens_f2_from_F(F_val, df1, df2)
    results$R2 <- R2_from_F(F_val, df1, df2)

    # If we have R^2, can compute semi-partial r approximation
    if (!is.na(results$R2) && !is.na(results$partial_r)) {
      results$semi_partial_r <- semi_partial_r_from_t(t, df, R2_full = results$R2)
    }
  }

  # If R^2 directly provided, use it
  if (!is.na(R2)) {
    results$R2 <- R2
    results$cohens_f2 <- cohens_f2_from_R2(R2)
  }

  results
}

# ---- Regression coefficient verification ----

#' Verify t-statistic from regression coefficient and SE
#'
#' Checks if t = b/SE is consistent with the reported t-value.
#'
#' @param b Regression coefficient
#' @param SE Standard error of b
#' @param reported_t Reported t-value
#' @param tol Tolerance for matching (default 0.01)
#' @return List with computed_t, delta, and consistent (logical)
#' @keywords internal
verify_t_from_b_SE <- function(b, SE, reported_t, tol = 0.01) {
  if (any(is.na(c(b, SE, reported_t))) || SE == 0) {
    return(list(computed_t = NA_real_, delta = NA_real_, consistent = NA))
  }
  computed_t <- b / SE
  delta <- abs(abs(computed_t) - abs(reported_t))
  list(
    computed_t = computed_t,
    delta = delta,
    consistent = delta <= tol
  )
}

#' Compute adjusted R-squared
#'
#' @param R2 R-squared value
#' @param n Sample size
#' @param p Number of predictors
#' @return Adjusted R-squared
#' @keywords internal
adjusted_R2 <- function(R2, n, p) {
  if (any(is.na(c(R2, n, p))) || n <= p + 1 || R2 < 0 || R2 > 1) {
    return(NA_real_)
  }
  1 - (1 - R2) * (n - 1) / (n - p - 1)
}

#' Verify adjusted R-squared consistency
#'
#' @param R2 R-squared value
#' @param adj_R2_reported Reported adjusted R-squared
#' @param n Sample size
#' @param p Number of predictors
#' @param tol Tolerance (default 0.01)
#' @return List with computed, delta, consistent
#' @keywords internal
verify_adj_R2 <- function(R2, adj_R2_reported, n, p, tol = 0.01) {
  if (any(is.na(c(R2, adj_R2_reported, n, p)))) {
    return(list(computed = NA_real_, delta = NA_real_, consistent = NA))
  }
  computed <- adjusted_R2(R2, n, p)
  if (is.na(computed)) {
    return(list(computed = NA_real_, delta = NA_real_, consistent = NA))
  }
  delta <- abs(computed - adj_R2_reported)
  list(computed = computed, delta = delta, consistent = delta <= tol)
}

# ---- Ratio measures (OR, RR, IRR) ----

# Odds Ratio (OR) - typically reported directly
# CI computation: log(OR) \u00b1 z * SE(log(OR))
# If SE not available, approximate from CI bounds if provided
or_ci_from_bounds <- function(OR, ciL, ciU, level = 0.95) {
  if (any(is.na(c(OR, ciL, ciU)))) {
    return(c(NA_real_, NA_real_))
  }
  # If bounds provided, return them
  c(ciL, ciU)
}

# Risk Ratio (RR) - typically reported directly
# CI computation similar to OR
rr_ci_from_bounds <- function(RR, ciL, ciU, level = 0.95) {
  if (any(is.na(c(RR, ciL, ciU)))) {
    return(c(NA_real_, NA_real_))
  }
  c(ciL, ciU)
}

# Incidence Rate Ratio (IRR) - typically reported directly
# CI computation similar to OR/RR
irr_ci_from_bounds <- function(IRR, ciL, ciU, level = 0.95) {
  if (any(is.na(c(IRR, ciL, ciU)))) {
    return(c(NA_real_, NA_real_))
  }
  c(ciL, ciU)
}

# Note: OR, RR, IRR are typically extracted from text rather than computed
# These functions are placeholders for future enhancement if cell counts available

# ---- Nonparametric effect sizes ----

# Rank-biserial correlation (r_rb) from Mann-Whitney U
# Formula: r_rb = 1 - (2*U) / (n1 * n2)
# Or from z-score: r_rb \u2248 z / sqrt(N)
rank_biserial_r_from_U <- function(U, n1, n2) {
  if (any(is.na(c(U, n1, n2))) || n1 <= 0 || n2 <= 0) {
    return(NA_real_)
  }
  if (U < 0 || U > n1 * n2) {
    return(NA_real_)
  }
  1 - (2 * U) / (n1 * n2)
}

# Rank-biserial r from z-score (approximation)
# Cohen's d from z-statistic (equal groups assumption)
# Formula: d = 2z / sqrt(N)
# Same family as d = 2t/sqrt(df) for large samples where z approximates t
#' @keywords internal
d_from_z <- function(z, N) {
  if (is.na(z) || is.na(N) || N <= 0) return(NA_real_)
  2 * z / sqrt(N)
}

# Paired (within-subjects) d from z: dz = z/sqrt(N)
# Analogous to dz_from_t = t/sqrt(n). When z comes from Wilcoxon signed-rank,
# N is the number of pairs.
#' @keywords internal
dz_from_z <- function(z, N) {
  if (is.na(z) || is.na(N) || N <= 0) return(NA_real_)
  z / sqrt(N)
}

rank_biserial_r_from_z <- function(z, N) {
  if (any(is.na(c(z, N))) || N <= 0) {
    return(NA_real_)
  }
  z / sqrt(N)
}

# Cliff's delta (\u03b4) from Mann-Whitney U
# Formula: \u03b4 = (2*U) / (n1 * n2) - 1
cliffs_delta_from_U <- function(U, n1, n2) {
  if (any(is.na(c(U, n1, n2))) || n1 <= 0 || n2 <= 0) {
    return(NA_real_)
  }
  if (U < 0 || U > n1 * n2) {
    return(NA_real_)
  }
  (2 * U) / (n1 * n2) - 1
}

# Kendall's W (coefficient of concordance) from chi-square
# Formula: W = \u03c7\u00b2 / (N * (k - 1))
# where k = number of groups/raters, N = number of items
kendalls_W_from_chisq <- function(chisq, N, k) {
  if (any(is.na(c(chisq, N, k))) || N <= 0 || k <= 1) {
    return(NA_real_)
  }
  if (chisq < 0) {
    return(NA_real_)
  }
  W <- chisq / (N * (k - 1))
  # W is bounded [0, 1]
  pmin(pmax(W, 0), 1)
}

# Epsilon-squared (\u03b5\u00b2) from Kruskal-Wallis H \u2248 eta\u00b2
# Formula: \u03b5\u00b2 = (H - k + 1) / (N - k)
# where k = number of groups, N = total sample size
epsilon_squared_from_H <- function(H, N, k) {
  if (any(is.na(c(H, N, k))) || N <= 0 || k <= 1) {
    return(NA_real_)
  }
  if (H < 0) {
    return(NA_real_)
  }
  numerator <- H - k + 1
  denominator <- N - k
  if (denominator <= 0) {
    return(NA_real_)
  }
  result <- numerator / denominator
  # Epsilon\u00b2 can be negative (very small effect)
  max(0, result)
}

# Epsilon-squared from chi-square (approximation for some tests)
# Formula: \u03b5\u00b2 = \u03c7\u00b2 / N (for some nonparametric tests)
epsilon_squared_from_chisq <- function(chisq, N) {
  if (any(is.na(c(chisq, N))) || N <= 0) {
    return(NA_real_)
  }
  if (chisq < 0) {
    return(NA_real_)
  }
  chisq / N
}
# ---- Advanced CI Utilities (NCP-based) ----

#' Find Non-Centrality Parameter (NCP) Confidence Limits for F-distribution
#'
#' Uses uniroot to invert the non-central F CDF.
#'
#' @param F_val Observed F statistic
#' @param df1 Numerator degrees of freedom
#' @param df2 Denominator degrees of freedom
#' @param level Confidence level (default 0.95)
#' @return Vector c(lambda_low, lambda_high)
#' @keywords internal
get_ncp_F <- function(F_val, df1, df2, level = 0.95) {
  if (is.na(F_val) || is.na(df1) || is.na(df2) || F_val < 0) {
    return(c(NA_real_, NA_real_))
  }

  alpha <- 1 - level
  probs <- c(1 - alpha / 2, alpha / 2) # Upper/Lower probabilities for the limits

  # Function to minimize: Cumulative Prob(F_obs | lambda) - target_prob = 0
  # Note:
  # Lower limit lambda_L: P(F > F_obs | lambda_L) = alpha/2  => P(F < F_obs | lambda_L) = 1 - alpha/2
  # Upper limit lambda_U: P(F > F_obs | lambda_U) = 1 - alpha/2 => P(F < F_obs | lambda_U) = alpha/2

  limits <- c(NA_real_, NA_real_)

  # Search range for lambda. F-values shouldn't imply massive lambdas usually.
  # Max lambda roughly corresponds to huge effect.
  max_lambda <- 1000 * max(1, F_val)

  # Find Lower Limit (corresponds to Upper Tail Prob = alpha/2, or CDF = 1 - alpha/2)
  # BUT: If F_obs is small, lambda_L might be 0.
  # Check P(F < F_obs | lambda=0)
  p0 <- tryCatch(stats::pf(F_val, df1, df2, ncp = 0), error = function(e) NA)

  # Upper limit for LAMBDA corresponds to LOWER CDF probability (alpha/2)
  # Lower limit for LAMBDA corresponds to HIGHER CDF probability (1 - alpha/2)
  # This is because as lambda increases, P(F < F_obs) decreases.

  # 1. Find Upper Limit (Prob = alpha/2)
  f_upper <- function(lam) {
    stats::pf(F_val, df1, df2, ncp = lam) - (alpha / 2)
  }

  # Check if root is bracketed
  if (!is.na(p0) && p0 > alpha / 2) {
    # Valid upper limit > 0 exists
    ul_try <- tryCatch(
      {
        stats::uniroot(f_upper, c(0, max_lambda))$root
      },
      error = function(e) NA_real_
    )
    limits[2] <- as.numeric(ul_try)
  } else {
    # Even at lambda=0, probability is too low (unlikely for upper limit)
    limits[2] <- NA_real_
  }

  # 2. Find Lower Limit (Prob = 1 - alpha/2)
  f_lower <- function(lam) {
    stats::pf(F_val, df1, df2, ncp = lam) - (1 - alpha / 2)
  }

  if (!is.na(p0) && p0 > (1 - alpha / 2)) {
    # Valid lower limit > 0
    ll_try <- tryCatch(
      {
        stats::uniroot(f_lower, c(0, max_lambda))$root
      },
      error = function(e) NA_real_
    )
    limits[1] <- as.numeric(ll_try)
  } else {
    limits[1] <- 0 # If p_null is too small, lower bound is 0
  }

  return(limits)
}

#' Calculate CI for Partial Eta-Squared
#'
#' @param F_val F statistic
#' @param df1 df1
#' @param df2 df2
#' @param level CI level
#' @return ci_result list
#' @keywords internal
ci_etap2 <- function(F_val, df1, df2, level = 0.95) {
  if (any(is.na(c(F_val, df1, df2)))) {
    return(ci_result(reason = "Missing parameters"))
  }

  lambdas <- get_ncp_F(F_val, df1, df2, level)
  if (any(is.na(lambdas))) {
    return(ci_result(reason = "Could not converge on NCP"))
  }

  # Convert noncentrality parameter to partial eta-squared
  # The standard formula (Steiger 2004, used in MBESS/effectsize):
  #   eta_p^2 = (F * df1) / (F * df1 + df2)
  # Since lambda = F * df1, this gives:
  #   eta_p^2 = lambda / (lambda + df2)
  calc_eta <- function(lam) {
    lam / (lam + df2)
  }

  bounds <- c(calc_eta(lambdas[1]), calc_eta(lambdas[2]))
  ci_result(bounds = bounds, method = "ncf_inversion", success = TRUE)
}

#' Alias for ci_etap2 (used for eta2 when design implies equivalence or as approximation)
#' @inheritParams ci_etap2
#' @keywords internal
ci_eta2 <- ci_etap2

#' Calculate CI for Cohen's f
#'
#' Derived from Partial Eta-Squared CI.
#'
#' @param F_val F statistic
#' @param df1 df1
#' @param df2 df2
#' @param level CI level
#' @return ci_result list
#' @keywords internal
ci_cohens_f <- function(F_val, df1, df2, level = 0.95) {
  # Get eta_p2 CI
  res <- ci_etap2(F_val, df1, df2, level)
  if (!res$success) {
    return(res)
  }

  # Convert eta2 bounds to f bounds
  # f = sqrt(eta2 / (1 - eta2))
  convert <- function(e) {
    if (is.na(e) || e >= 1) Inf else sqrt(e / (1 - e))
  }

  bounds <- c(convert(res$bounds[1]), convert(res$bounds[2]))
  # v0.3.0m: Guard against all-infinite bounds (eta2 near 1)
  if (all(!is.finite(bounds))) {
    return(ci_result(reason = "CI bounds infinite (eta2 near 1)"))
  }
  ci_result(bounds = bounds, method = "from_eta2_ncf", success = TRUE)
}
