#' Effect Size Type Definitions
#'
#' Maps reported effect size names to their family and variants.
#' Based on Guide to Effect Sizes and Confidence Intervals (Jan\u00e9 et al., 2024)
#' https://matthewbjane.quarto.pub/
#'
#' @keywords internal
EFFECT_SIZE_FAMILIES <- list(
  # Cohen's d family (standardized mean differences)
  d = list(
    family = "d",
    variants = c("d_ind", "d_ind_equalN", "d_ind_min", "d_ind_max", "dz", "dav", "drm"),
    alternatives = c("g_ind"),
    description = "Cohen's d - standardized mean difference"
  ),
  g = list(
    family = "g",
    variants = c("g_ind", "gz", "gav", "grm"),
    alternatives = c("d_ind"),
    description = "Hedges' g - bias-corrected standardized mean difference"
  ),
  dz = list(
    family = "d",
    variants = c("dz", "dav", "drm"),
    alternatives = c("d_ind", "g_ind"),
    description = "Cohen's dz - paired/within-subjects effect size"
  ),
  dav = list(
    family = "d",
    variants = c("dav", "dz", "drm"),
    alternatives = c("d_ind", "g_ind"),
    description = "Cohen's dav - average SD method for paired designs"
  ),
  drm = list(
    family = "d",
    variants = c("drm", "dz", "dav"),
    alternatives = c("d_ind", "g_ind"),
    description = "Cohen's drm - repeated measures effect size"
  ),
  # Correlation family
  r = list(
    family = "r",
    variants = c("r"),
    alternatives = c("r_squared", "partial_r"),
    description = "Pearson correlation coefficient"
  ),
  eta = list(
    family = "eta2",
    variants = c("eta", "eta2", "partial_eta2", "generalized_eta2"),
    alternatives = c("omega2", "cohens_f"),
    description = "Eta - often used for eta-squared or partial eta-squared"
  ),
  # ANOVA effect sizes
  eta2 = list(
    family = "eta2",
    variants = c("eta2", "eta", "partial_eta2", "generalized_eta2"),
    alternatives = c("omega2", "cohens_f"),
    description = "Eta-squared - proportion of variance explained"
  ),
  etap2 = list(
    family = "eta2",
    variants = c("partial_eta2", "eta2", "eta", "generalized_eta2"),
    alternatives = c("omega2", "cohens_f"),
    description = "Partial eta-squared - controls for other factors"
  ),
  omega2 = list(
    family = "omega2",
    variants = c("omega2"),
    alternatives = c("eta2", "partial_eta2", "cohens_f"),
    description = "Omega-squared - population estimate (less biased)"
  ),
  cohens_f = list(
    family = "f",
    variants = c("cohens_f"),
    alternatives = c("eta2", "partial_eta2", "omega2"),
    description = "Cohen's f - ANOVA effect size"
  ),
  # Chi-square effect sizes
  phi = list(
    family = "phi",
    variants = c("phi"),
    alternatives = c("V"),
    description = "Phi coefficient - 2x2 contingency tables"
  ),
  V = list(
    family = "V",
    variants = c("V"),
    alternatives = c("phi"),
    description = "Cramer's V - larger contingency tables"
  ),
  # Regression effect sizes
  beta = list(
    family = "beta",
    variants = c("standardized_beta"),
    alternatives = c("partial_r", "semi_partial_r"),
    description = "Standardized regression coefficient"
  ),
  R2 = list(
    family = "R2",
    variants = c("R2"),
    alternatives = c("cohens_f2", "adjusted_R2"),
    description = "R-squared - proportion of variance explained"
  ),
  f2 = list(
    family = "f2",
    variants = c("cohens_f2"),
    alternatives = c("R2"),
    description = "Cohen's f\u00b2 - regression effect size"
  )
)

#' Variant Metadata
#'
#' Provides assumptions and usage information for each effect size variant.
#' @keywords internal
VARIANT_METADATA <- list(
  d_ind = list(
    name = "Cohen's d (independent)",
    assumptions = "Independent samples, known group sizes",
    when_to_use = "Between-subjects design with known n1 and n2",
    formula = "d = t * sqrt(1/n1 + 1/n2)"
  ),
  d_ind_equalN = list(
    name = "Cohen's d (equal n)",
    assumptions = "Independent samples, equal group sizes assumed",
    when_to_use = "Between-subjects design when group sizes unknown but assumed equal",
    formula = "d = t * sqrt(2/N)"
  ),
  d_ind_min = list(
    name = "Cohen's d (min bound)",
    assumptions = "Independent samples, extreme imbalance (1 vs N-1)",
    when_to_use = "Lower bound when group sizes unknown",
    formula = "d = t * sqrt(1/1 + 1/(N-1))"
  ),
  d_ind_max = list(
    name = "Cohen's d (max bound)",
    assumptions = "Independent samples, extreme imbalance (N-1 vs 1)",
    when_to_use = "Upper bound when group sizes unknown",
    formula = "d = t * sqrt(1/(N-1) + 1/1)"
  ),
  g_ind = list(
    name = "Hedges' g",
    assumptions = "Independent samples, bias-corrected",
    when_to_use = "Recommended for small samples (n < 20), meta-analysis",
    formula = "g = d * J, where J = 1 - 3/(4*df - 1)"
  ),
  dz = list(
    name = "Cohen's dz",
    assumptions = "Paired/within-subjects design",
    when_to_use = "Repeated measures, pre-post designs",
    formula = "dz = t / sqrt(n)"
  ),
  dav = list(
    name = "Cohen's dav",
    assumptions = "Paired design, average SD method",
    when_to_use = "When pre-post SDs differ substantially",
    formula = "dav = dz / sqrt(2*(1-r))"
  ),
  drm = list(
    name = "Cohen's drm",
    assumptions = "Repeated measures design",
    when_to_use = "Repeated measures ANOVA contexts",
    formula = "drm \u2248 dz (often used interchangeably)"
  ),
  r = list(
    name = "Pearson r",
    assumptions = "Bivariate normal distribution",
    when_to_use = "Correlation between two continuous variables",
    formula = "r = t / sqrt(t\u00b2 + df)"
  ),
  eta = list(
    name = "Eta",
    assumptions = "Unsquared correlation ratio",
    when_to_use = "Sometimes reported instead of eta-squared",
    formula = "\u03B7 = sqrt(SS_effect / SS_total)"
  ),
  eta2 = list(
    name = "Eta-squared",
    assumptions = "Between-subjects ANOVA",
    when_to_use = "Simple one-way ANOVA, proportion of total variance",
    formula = "\u03b7\u00b2 = SS_effect / SS_total"
  ),
  partial_eta2 = list(
    name = "Partial eta-squared",
    assumptions = "Factorial ANOVA",
    when_to_use = "When controlling for other factors in ANOVA",
    formula = "ηp² = SS_effect / (SS_effect + SS_error)"
  ),
  generalized_eta2 = list(
    name = "Generalized eta-squared",
    assumptions = "Mixed/repeated measures ANOVA",
    when_to_use = "Comparing effects across different designs",
    formula = "ηG² = SS_effect / (SS_effect + SS_subjects + SS_error)"
  ),
  omega2 = list(
    name = "Omega-squared",
    assumptions = "Population estimate",
    when_to_use = "Less biased than eta², better for small samples",
    formula = "ω² = (SS_effect - df_effect*MS_error) / (SS_total + MS_error)"
  ),
  cohens_f = list(
    name = "Cohen's f",
    assumptions = "ANOVA effect size",
    when_to_use = "Power analysis, comparing across studies",
    formula = "f = sqrt(\u03b7\u00b2 / (1 - \u03b7\u00b2))"
  ),
  phi = list(
    name = "Phi coefficient",
    assumptions = "2x2 contingency table",
    when_to_use = "Association in 2x2 tables",
    formula = "φ = sqrt(χ² / N)"
  ),
  V = list(
    name = "Cramer's V",
    assumptions = "Larger contingency tables",
    when_to_use = "Association in tables larger than 2x2",
    formula = "V = sqrt(χ² / (N * min(r-1, c-1)))"
  ),
  standardized_beta = list(
    name = "Standardized beta",
    assumptions = "Multiple regression",
    when_to_use = "Comparing predictor importance in regression",
    formula = "β = b * (SD_x / SD_y)"
  ),
  partial_r = list(
    name = "Partial r",
    assumptions = "Multiple regression",
    when_to_use = "Unique contribution controlling for other predictors",
    formula = "partial r = t / sqrt(t² + df_residual)"
  ),
  cohens_f2 = list(
    name = "Cohen's f²",
    assumptions = "Regression effect size",
    when_to_use = "Effect size for R² change, power analysis",
    formula = "f² = R² / (1 - R²)"
  ),
  R2 = list(
    name = "R-squared",
    assumptions = "Linear regression",
    when_to_use = "Proportion of variance explained by model",
    formula = "R² = SS_regression / SS_total"
  )
)

#' Compute effects and compare to reported values for one parsed row
#'
#' This function implements type-matched comparison: it compares reported effect
#' sizes against computed variants of the SAME type. When design is ambiguous,
#' it computes all variants and finds the closest match among same-type variants.
#'
#' @param row A single row from parsed data
#' @param ci_level Default CI level
#' @param alpha Significance threshold
#' @param one_tailed Whether to use one-tailed tests
#' @param paired_r_grid Grid of r values for paired t-test computations
#' @param assume_equal_ns_when_missing Whether to assume equal n when missing
#' @param tol_effect List of tolerances by effect type
#' @param tol_ci Tolerance for CI bounds
#' @param tol_p Tolerance for p-values
#' @return A tibble with comparison results
#' @keywords internal
compute_and_compare_one <- function(row,
                                    ci_level = 0.95,
                                    alpha = 0.05,
                                    one_tailed = FALSE,
                                    paired_r_grid = seq(0.1, 0.9, by = 0.1),
                                    assume_equal_ns_when_missing = TRUE,
                                    tol_effect = list(d = 0.02, r = 0.005, phi = 0.02, V = 0.02),
                                    tol_ci = 0.02,
                                    tol_p = 0.001) {
  # ============================================================================
  # PHASE 1: Extract and validate input data
  # ============================================================================

  uncertainty <- character(0)
  assumptions <- character(0)

  # Extract as single values to avoid "length = 2 in coercion to logical(1)" errors
  tt <- if (length(row$test_type) > 0) as.character(row$test_type[1]) else NA_character_
  df1 <- if (length(row$df1) > 0) as.numeric(row$df1[1]) else NA_real_
  df2 <- if ("df2" %in% names(row) && length(row$df2) > 0) as.numeric(row$df2[1]) else NA_real_
  stat <- if (length(row$stat_value) > 0) as.numeric(row$stat_value[1]) else NA_real_
  N <- if (length(row$N) > 0) as.numeric(row$N[1]) else NA_real_
  n1 <- if (length(row$n1) > 0) as.numeric(row$n1[1]) else NA_real_
  n2 <- if (length(row$n2) > 0) as.numeric(row$n2[1]) else NA_real_
  table_r <- if (length(row$table_r) > 0) as.numeric(row$table_r[1]) else NA_real_
  table_c <- if (length(row$table_c) > 0) as.numeric(row$table_c[1]) else NA_real_
  effect_reported <- if (length(row$effect_reported) > 0) as.numeric(row$effect_reported[1]) else NA_real_
  effect_reported_name <- if (length(row$effect_reported_name) > 0) as.character(row$effect_reported_name[1]) else NA_character_
  ciL_rep <- if (length(row$ciL_reported) > 0) as.numeric(row$ciL_reported[1]) else NA_real_
  ciU_rep <- if (length(row$ciU_reported) > 0) as.numeric(row$ciU_reported[1]) else NA_real_

  # P-value extraction (now numeric from parse.R with validation)
  p_reported <- if ("p_reported" %in% names(row) && length(row$p_reported) > 0) {
    as.numeric(row$p_reported[1])
  } else {
    NA_real_
  }

  # Add uncertainty if p-value was out of range
  if ("p_out_of_range" %in% names(row) && isTRUE(row$p_out_of_range[1])) {
    uncertainty <- c(uncertainty, "Reported p-value could not be parsed or is out of valid range [0,1]")
  }

  # Use CI level from parsed data if available
  ci_level_used <- if (!is.null(row$ci_level) && !is.na(row$ci_level[1])) {
    as.numeric(row$ci_level[1])
  } else {
    ci_level
  }

  # Add assumption if CI level was assumed (Phase 2H)
  if ("ci_level_source" %in% names(row) &&
    !is.na(row$ci_level_source[1]) &&
    row$ci_level_source[1] == "assumed_95") {
    assumptions <- c(assumptions, "Assumed 95% confidence level (not explicitly stated)")
  }

  # ============================================================================
  # PHASE 2: Determine reported effect size type and family
  # ============================================================================

  # Normalize effect name
  reported_type <- tolower(trimws(effect_reported_name))
  if (is.na(reported_type) || reported_type == "" || reported_type == "na") {
    reported_type <- NA_character_
  }

  # Map to canonical type
  type_mapping <- list(
    "d" = "d", "cohen's d" = "d", "cohens d" = "d",
    "g" = "g", "hedges' g" = "g", "hedges g" = "g",
    "dz" = "dz", "d_z" = "dz",
    "dav" = "dav", "d_av" = "dav",
    "drm" = "drm", "d_rm" = "drm",
    "r" = "r", "correlation" = "r",
    "eta" = "eta", "\u03b7" = "eta",
    "eta2" = "eta2", "eta-squared" = "eta2", "eta squared" = "eta2", "\u03b7\u00b2" = "eta2",
    "etap2" = "etap2", "partial eta2" = "etap2", "partial eta-squared" = "etap2",
    "partial eta squared" = "etap2", "\u03b7p\u00b2" = "etap2", "partial \u03b7\u00b2" = "etap2",
    "omega2" = "omega2", "omega-squared" = "omega2", "omega squared" = "omega2", "\u03c9\u00b2" = "omega2",
    "f" = "cohens_f", "cohen's f" = "cohens_f", "cohens f" = "cohens_f",
    "phi" = "phi", "\u03c6" = "phi",
    "v" = "V", "cramer's v" = "V", "cramers v" = "V",
    "beta" = "beta", "\u03b2" = "beta", "standardized beta" = "beta",
    "r2" = "R2", "r-squared" = "R2", "r squared" = "R2",
    "f2" = "f2", "f-squared" = "f2", "f squared" = "f2", "cohen's f2" = "f2"
  )

  canonical_type <- if (length(reported_type) > 0 && !is.na(reported_type) && reported_type %in% names(type_mapping)) {
    type_mapping[[reported_type]]
  } else if (length(reported_type) > 0) {
    reported_type
  } else {
    NA_character_
  }

  # Special handling: If t-test reports "f" (Cohen's f), interpret as "d" (Cohen's d)
  # A one-way ANOVA with two conditions is equivalent to a t-test, and f can be converted to d.
  # However, Cohen's d is more appropriate and commonly used for t-tests.
  if (!is.na(tt) && tt == "t" && !is.na(canonical_type) && canonical_type == "cohens_f") {
    uncertainty <- c(uncertainty, "Reported 'f' (Cohen's f) for t-test - converted to 'd' (Cohen's d). f was converted using d = 2f.")
    canonical_type <- "d"
    # Convert value: d = 2 * f
    if (!is.na(effect_reported)) {
      effect_reported <- effect_reported * 2
    }
    # Update reported_type for display
    if (!is.na(reported_type) && (reported_type == "f" || reported_type == "cohens_f")) {
      reported_type <- "d"
    }
  }

  # ============================================================================
  # PHASE 2.5: Effect Size - Test Type Compatibility Validation (Phase 2D)
  # ============================================================================
  if (!is.na(canonical_type) && !is.na(tt)) {
    valid_for_test <- TRUE
    suggestion <- NULL

    if (tt == "t") {
      # t-tests: Cohen's d, Hedges' g, paired variants (dz, dav, drm), correlation (r)
      valid_types <- c("d", "g", "dz", "dav", "drm", "r", "R2")
      if (!canonical_type %in% valid_types) {
        valid_for_test <- FALSE
        suggestion <- "For t-tests, typical effect sizes are Cohen's d, Hedges' g (between-subjects), dz/dav/drm (within-subjects), or correlation r"
      }
    } else if (tt == "F") {
      # F-tests/ANOVA: eta\u00b2, partial eta\u00b2, omega\u00b2, Cohen's f, R\u00b2
      valid_types <- c("eta", "eta2", "etap2", "omega2", "cohens_f", "R2", "f2")
      if (!canonical_type %in% valid_types) {
        valid_for_test <- FALSE
        suggestion <- "For F-tests/ANOVA, typical effect sizes are eta-squared (\u03b7\u00b2), partial eta-squared (\u03b7p\u00b2), omega-squared (\u03c9\u00b2), or Cohen's f"
      }
    } else if (tt == "r") {
      # Correlation: r or R\u00b2
      valid_types <- c("r", "R2")
      if (!canonical_type %in% valid_types) {
        valid_for_test <- FALSE
        suggestion <- "For correlations, typical effect sizes are Pearson's r or R-squared"
      }
    } else if (tt == "chisq") {
      # Chi-square: phi (2\u00d72), Cramer's V (larger tables)
      valid_types <- c("phi", "V")
      if (!canonical_type %in% valid_types) {
        valid_for_test <- FALSE
        suggestion <- "For chi-square tests, typical effect sizes are phi (\u03c6) for 2\u00d72 tables or Cramer's V for larger tables"
      }
    } else if (tt == "z") {
      # z-tests: similar to t-tests
      valid_types <- c("d", "g", "r", "R2")
      if (!canonical_type %in% valid_types) {
        valid_for_test <- FALSE
        suggestion <- "For z-tests, typical effect sizes are Cohen's d, Hedges' g, or correlation r"
      }
    }

    if (!valid_for_test) {
      test_name <- switch(tt,
        "t" = "t",
        "F" = "F/ANOVA",
        "r" = "correlation",
        "chisq" = "chi-square",
        "z" = "z",
        tt
      )
      uncertainty <- c(
        uncertainty,
        sprintf(
          "Effect size type '%s' is unusual for %s test. %s",
          canonical_type, test_name, suggestion
        )
      )
    }
  }

  # Add uncertainty if effect size matched via fallback pattern (Phase 2F)
  if ("effect_fallback" %in% names(row) && isTRUE(row$effect_fallback[1])) {
    uncertainty <- c(
      uncertainty,
      sprintf(
        "Effect size symbol (%s) unclear - may be OCR/PDF extraction error or non-standard notation",
        effect_reported_name
      )
    )
  }

  # Get family info
  family_info <- if (!is.na(canonical_type) && canonical_type %in% names(EFFECT_SIZE_FAMILIES)) {
    EFFECT_SIZE_FAMILIES[[canonical_type]]
  } else {
    NULL
  }

  # ============================================================================
  # PHASE 2.5: Validate effect size / test type compatibility
  # ============================================================================

  # Define which effect sizes are valid for which test types
  valid_effects_for_test <- list(
    t = c("d", "g", "dz", "dav", "drm", "r"), # t-tests: d family, g family, r
    F = c("eta2", "etap2", "omega2", "cohens_f", "R2", "f2"), # F-tests: ANOVA effects
    r = c("r", "R2"), # Correlation tests
    chisq = c("phi", "V"), # Chi-square tests
    z = c("r", "d", "g", "beta") # z-tests: various
  )

  # Check compatibility
  effect_test_mismatch <- FALSE
  if (length(canonical_type) > 0 && !is.na(canonical_type) && length(tt) > 0 && !is.na(tt)) {
    valid_for_this_test <- valid_effects_for_test[[tt]]
    if (!is.null(valid_for_this_test) && !(canonical_type %in% valid_for_this_test)) {
      effect_test_mismatch <- TRUE
      uncertainty <- c(uncertainty, sprintf(
        "Reported effect size '%s' is unusual for %s-test (expected: %s)",
        canonical_type, tt, paste(valid_for_this_test, collapse = ", ")
      ))
    }
  }

  # ============================================================================
  # PHASE 3: Initialize computed values storage
  # ============================================================================

  # All computed variants (will be populated based on test type)
  computed_variants <- list()

  # Alternative suggestions (different effect size types)
  alternatives <- list()

  # Note: uncertainty and assumptions already initialized at top of function (line 275-276)
  # Do NOT reset them here or earlier messages will be lost

  # ============================================================================
  # PHASE 4: Compute effect sizes based on test type
  # ============================================================================

  if (tt == "t") {
    # ------ T-TEST COMPUTATIONS ------

    # ========================================================================
    # SAMPLE SIZE VALIDATION AND INFERENCE (Enhanced Phase 2B)
    # ========================================================================

    # Detect Welch's t-test (non-integer df indicates unequal variances)
    is_welch <- !is.na(df1) && abs(df1 - round(df1)) > 0.01

    if (is_welch) {
      uncertainty <- c(
        uncertainty,
        sprintf("Non-integer df (%.2f) suggests Welch's t-test (unequal variances)", df1)
      )
      assumptions <- c(assumptions, "Welch's t-test assumed (unequal variances)")
    }

    # Validate reported N against df (if both available and not Welch's)
    if (!is.na(N) && !is.na(df1) && df1 > 0 && !is_welch) {
      # Expected ranges:
      # - Paired/one-sample: N = df + 1
      # - Independent: N = df + 2
      min_expected_N <- df1 + 1
      max_expected_N <- df1 + 2

      if (N < min_expected_N - 0.5) {
        # N is too small - likely error
        uncertainty <- c(
          uncertainty,
          sprintf(
            "Reported N (%d) is less than expected minimum (%.0f) for df=%.0f",
            as.integer(N), min_expected_N, df1
          )
        )

        # Re-infer based on effect type
        N_original <- N
        N <- if (!is.na(canonical_type) && canonical_type %in% c("dz", "dav", "drm")) {
          df1 + 1
        } else {
          df1 + 2
        }
        assumptions <- c(
          assumptions,
          sprintf(
            "Replaced N=%d with inferred N=%.0f from df",
            as.integer(N_original), N
          )
        )
      } else if (N > max_expected_N + 10) {
        # N is much larger than expected - flag it but don't override
        uncertainty <- c(
          uncertainty,
          sprintf(
            "Reported N (%d) is larger than expected (%.0f-%.0f) for df=%.0f",
            as.integer(N), min_expected_N, max_expected_N, df1
          )
        )
      }
    }

    # Infer N when not reported (or if replaced above)
    if (is.na(N) && !is.na(df1) && df1 > 0) {
      if (is_welch) {
        # For Welch's, use conservative estimate
        N <- round(df1 + 2)
        assumptions <- c(
          assumptions,
          sprintf("Welch's t-test: conservatively estimated N \u2248 %.0f (actual N uncertain)", N)
        )
        uncertainty <- c(
          uncertainty,
          "Sample size estimation uncertain for Welch's test - computed plausible variants"
        )
      } else {
        # Use reported effect type to infer design if available
        if (!is.na(canonical_type) && canonical_type %in% c("dz", "dav", "drm")) {
          # Paired/within-subjects effect size
          N <- df1 + 1
          assumptions <- c(assumptions, "Inferred N = df + 1 (paired t-test based on reported effect type)")
          design_inferred <- "paired"
        } else if (!is.na(canonical_type) && canonical_type %in% c("d", "g")) {
          # Between-subjects effect size
          N <- df1 + 2
          assumptions <- c(assumptions, "Inferred N = df + 2 (independent t-test based on reported effect type)")
          design_inferred <- "independent"
        } else {
          # AMBIGUOUS - must compute both
          N <- df1 + 2 # Use for independent calculations
          N_paired <- df1 + 1 # Store for paired calculations
          design_inferred <- "ambiguous"
          assumptions <- c(
            assumptions,
            sprintf(
              "Design unclear - will compute both paired (N=%d) and independent (N=%d) variants",
              as.integer(N_paired), as.integer(N)
            )
          )
          uncertainty <- c(
            uncertainty,
            "Cannot determine if paired or independent design from reported information"
          )
        }
      }
    } else if (!is.na(N) && !is.na(df1) && !exists("design_inferred")) {
      # N is known but design may still be ambiguous - set flag for variant computation
      if (is.na(canonical_type) || (!canonical_type %in% c("d", "g", "dz", "dav", "drm"))) {
        N_paired <- df1 + 1
        design_inferred <- "ambiguous"
        uncertainty <- c(uncertainty, "Design (paired vs independent) unclear - computed both variants")
      }
    }

    # Store both N values for variant computation if ambiguous
    if (exists("design_inferred") && design_inferred == "ambiguous") {
      if (!exists("N_paired")) N_paired <- df1 + 1
      N_independent <- N
    } else {
      N_independent <- N
      N_paired <- N
    }

    # Add assumptions/uncertainty based on N source (Phase 2C)
    if ("N_source" %in% names(row) && !is.na(row$N_source[1])) {
      if (row$N_source[1] == "extended_context") {
        assumptions <- c(
          assumptions,
          "Sample size N found in extended context (not immediately adjacent to statistic)"
        )
      } else if (row$N_source[1] == "global_text") {
        assumptions <- c(
          assumptions,
          "Sample size N inferred from methods section (verify it applies to this specific test)"
        )
        uncertainty <- c(
          uncertainty,
          "Sample size may not apply to this specific comparison - found distant from statistic"
        )
      }
    }

    # ----- Compute INDEPENDENT samples d variants -----
    if (!is.na(n1) && !is.na(n2) && n1 > 0 && n2 > 0) {
      # Have explicit group sizes
      d_ind <- tryCatch(d_ind_from_t(stat, n1, n2), error = function(e) NA_real_)
      if (!is.na(d_ind)) {
        computed_variants$d_ind <- list(
          value = d_ind,
          metadata = VARIANT_METADATA$d_ind
        )
      }

      # Also compute Hedges' g as alternative
      g_ind <- tryCatch(g_ind_from_t(stat, n1, n2), error = function(e) NA_real_)
      if (!is.na(g_ind)) {
        alternatives$g_ind <- list(
          value = g_ind,
          metadata = VARIANT_METADATA$g_ind,
          why_consider = "Bias-corrected version of d, recommended for small samples (n < 20)"
        )
      }

      # Compute CI for d_ind
      if (!is.na(d_ind)) {
        d_ci <- tryCatch(ci_d_ind(d_ind, n1, n2, ci_level_used), error = function(e) list(success = FALSE))
        if (d_ci$success) computed_variants$d_ind$ci <- d_ci$bounds
      }
    } else if (!is.na(N) && N > 0) {
      # No explicit group sizes - compute with assumptions
      if (assume_equal_ns_when_missing) {
        n1_eq <- floor(N / 2)
        n2_eq <- N - n1_eq
        if (n1_eq > 0 && n2_eq > 0) {
          d_ind_equalN <- tryCatch(d_ind_from_t(stat, n1_eq, n2_eq), error = function(e) NA_real_)
          if (!is.na(d_ind_equalN)) {
            computed_variants$d_ind_equalN <- list(
              value = d_ind_equalN,
              metadata = VARIANT_METADATA$d_ind_equalN
            )
          }

          # Hedges' g for equal N
          g_ind <- tryCatch(g_ind_from_t(stat, n1_eq, n2_eq), error = function(e) NA_real_)
          if (!is.na(g_ind)) {
            alternatives$g_ind <- list(
              value = g_ind,
              metadata = VARIANT_METADATA$g_ind,
              why_consider = "Bias-corrected version of d, recommended for small samples"
            )
          }

          # Compute CI for d_ind_equalN
          if (!is.na(d_ind_equalN)) {
            d_ci <- tryCatch(ci_d_ind(d_ind_equalN, n1_eq, n2_eq, ci_level_used), error = function(e) list(success = FALSE))
            if (d_ci$success) computed_variants$d_ind_equalN$ci <- d_ci$bounds
          }
        }
        assumptions <- c(assumptions, "Assumed equal group sizes (n1=n2=N/2) for d_ind computation")
      }

      # Compute bounds for extreme imbalance
      if (N > 1) {
        d1 <- tryCatch(d_ind_from_t(stat, 1, N - 1), error = function(e) NA_real_)
        d2 <- tryCatch(d_ind_from_t(stat, N - 1, 1), error = function(e) NA_real_)
        if (!is.na(d1) && !is.na(d2)) {
          computed_variants$d_ind_min <- list(
            value = min(d1, d2),
            metadata = VARIANT_METADATA$d_ind_min
          )
          computed_variants$d_ind_max <- list(
            value = max(d1, d2),
            metadata = VARIANT_METADATA$d_ind_max
          )
        }
      }
      uncertainty <- c(uncertainty, "Independent-samples group sizes unknown; computed bounds")
    }

    # ----- Compute PAIRED samples d variants -----
    n_paired <- NA_real_
    if (!is.na(N) && N > 0) {
      n_paired <- N
    } else if (!is.na(df1) && df1 >= 0) {
      n_paired <- df1 + 1
      assumptions <- c(assumptions, "Assumed paired-sample n = df + 1")
    }

    if (!is.na(n_paired) && n_paired > 0) {
      dz <- tryCatch(dz_from_t(stat, n_paired), error = function(e) NA_real_)
      if (!is.na(dz)) {
        computed_variants$dz <- list(
          value = dz,
          metadata = VARIANT_METADATA$dz
        )

        # Compute dav range across r grid
        d_av_grid <- sapply(paired_r_grid, function(r) {
          tryCatch(dav_from_dz(dz, r), error = function(e) NA_real_)
        })
        d_av_grid <- d_av_grid[!is.na(d_av_grid)]

        if (length(d_av_grid) > 0) {
          computed_variants$dav <- list(
            value = stats::median(d_av_grid),
            range = c(min(d_av_grid), max(d_av_grid)),
            metadata = VARIANT_METADATA$dav
          )
        }

        # Compute drm
        drm <- tryCatch(drm_from_dz(dz), error = function(e) NA_real_)
        if (!is.na(drm)) {
          computed_variants$drm <- list(
            value = drm,
            metadata = VARIANT_METADATA$drm
          )
        }

        # Compute CI for dz
        if (!is.na(dz)) {
          dz_ci <- tryCatch(ci_dz(dz, n_paired, ci_level_used), error = function(e) list(success = FALSE))
          if (dz_ci$success) computed_variants$dz$ci <- dz_ci$bounds
        }
      }
    }

    # ----- Compute CORRELATION (r) from t-test -----
    # Always compute r as an alternative effect size for t-tests
    if (!is.na(stat) && !is.na(df1) && df1 > 0) {
      r_value <- tryCatch(r_from_t(stat, df1), error = function(e) NA_real_)
      if (!is.na(r_value)) {
        alternatives$r <- list(
          value = r_value,
          metadata = list(
            name = "Correlation (r)",
            description = "Correlation coefficient equivalent of t-test"
          ),
          why_consider = "Alternative way to express t-test effect size, useful for meta-analysis"
        )

        # Compute CI for r
        n_r <- if (!is.na(N)) N else df1 + 2
        if (!is.na(n_r) && n_r > 3) {
          r_ci <- tryCatch(fisher_ci_r(r_value, n_r, ci_level_used), error = function(e) c(NA_real_, NA_real_))
          if (!any(is.na(r_ci))) {
            alternatives$r$ci <- r_ci
          }
        }
      }
    }

    # If reported type is d/g but we don't know design, add paired variants as alternatives
    if (!is.na(canonical_type) && canonical_type %in% c("d", "g")) {
      # Move paired variants to alternatives if they exist
      if ("dz" %in% names(computed_variants)) {
        alternatives$dz <- computed_variants$dz
        alternatives$dz$why_consider <- "If this is actually a paired/within-subjects design"
      }
    }
  } else if (tt == "r") {
    # ------ CORRELATION COMPUTATIONS ------
    r_value <- stat # For r tests, the statistic IS the effect size

    if (!is.na(r_value)) {
      computed_variants$r <- list(
        value = r_value,
        metadata = VARIANT_METADATA$r
      )

      # Compute CI
      n_r <- if (!is.na(df1)) df1 + 2 else N
      if (!is.na(n_r)) {
        r_ci_result <- ci_r(r_value, n_r, ci_level_used)
        if (r_ci_result$success) {
          computed_variants$r$ci <- r_ci_result$bounds
          computed_variants$r$ci_method <- r_ci_result$method
        }
      }

      # Add r\u00b2 as alternative
      alternatives$r_squared <- list(
        value = r_value^2,
        metadata = list(
          name = "R-squared (from r)",
          assumptions = "Squared correlation",
          when_to_use = "Proportion of variance explained"
        ),
        why_consider = "Variance explained interpretation"
      )
    }
  } else if (tt == "F") {
    # ------ ANOVA F-TEST COMPUTATIONS ------
    df1 <- as.numeric(df1[1])
    df2 <- as.numeric(df2[1])
    stat <- as.numeric(stat[1])

    if (!is.na(df1) && !is.na(df2) && df1 > 0 && df2 > 0) {
      # Infer design from context
      design <- "unclear"
      if ("context_window" %in% names(row) && !is.na(row$context_window[1])) {
        context_lower <- tolower(as.character(row$context_window[1]))
        if (grepl("between|between-subjects|between-groups", context_lower)) {
          design <- "between"
        } else if (grepl("within|within-subjects|repeated measures", context_lower)) {
          design <- "within"
        } else if (grepl("mixed|split-plot", context_lower)) {
          design <- "mixed"
        }
      }

      # Compute all ANOVA effect sizes
      anova_error_msg <- NULL
      anova_effects <- tryCatch(
        {
          compute_all_anova_effects(stat, df1, df2, design)
        },
        error = function(e) {
          anova_error_msg <<- paste0("Error computing ANOVA effects: ", conditionMessage(e))
          list(
            eta = NA_real_, eta2 = NA_real_, partial_eta2 = NA_real_,
            generalized_eta2 = NA_real_, omega2 = NA_real_, cohens_f = NA_real_
          )
        }
      )

      if (!is.null(anova_error_msg)) {
        uncertainty <- c(uncertainty, anova_error_msg)
      }

      # Add to computed variants based on reported type
      if (!is.na(anova_effects$eta)) {
        computed_variants$eta <- list(
          value = anova_effects$eta,
          metadata = VARIANT_METADATA$eta
        )
      }
      if (!is.na(anova_effects$eta2)) {
        computed_variants$eta2 <- list(
          value = anova_effects$eta2,
          metadata = VARIANT_METADATA$eta2
        )
      }
      if (!is.na(anova_effects$partial_eta2)) {
        computed_variants$partial_eta2 <- list(
          value = anova_effects$partial_eta2,
          metadata = VARIANT_METADATA$partial_eta2
        )
      }
      if (!is.na(anova_effects$generalized_eta2)) {
        computed_variants$generalized_eta2 <- list(
          value = anova_effects$generalized_eta2,
          metadata = VARIANT_METADATA$generalized_eta2
        )
      }

      # omega2 and cohens_f as alternatives (or primary if reported)
      if (!is.na(anova_effects$omega2)) {
        if (!is.na(canonical_type) && canonical_type == "omega2") {
          computed_variants$omega2 <- list(
            value = anova_effects$omega2,
            metadata = VARIANT_METADATA$omega2
          )
        } else {
          alternatives$omega2 <- list(
            value = anova_effects$omega2,
            metadata = VARIANT_METADATA$omega2,
            why_consider = "Less biased population estimate than eta\u00b2"
          )
        }
      }
      if (!is.na(anova_effects$cohens_f)) {
        if (!is.na(canonical_type) && canonical_type == "cohens_f") {
          computed_variants$cohens_f <- list(
            value = anova_effects$cohens_f,
            metadata = VARIANT_METADATA$cohens_f
          )
        } else {
          alternatives$cohens_f <- list(
            value = anova_effects$cohens_f,
            metadata = VARIANT_METADATA$cohens_f,
            why_consider = "Useful for power analysis and cross-study comparison"
          )
        }
        # Compute CI for Cohen's f
        f_ci <- tryCatch(ci_cohens_f(stat, df1, df2, ci_level_used), error = function(e) list(success = FALSE))
        if (f_ci$success) {
          if (!is.null(computed_variants$cohens_f)) {
            computed_variants$cohens_f$ci <- f_ci$bounds
          } else if (!is.null(alternatives$cohens_f)) {
            alternatives$cohens_f$ci <- f_ci$bounds
          }
        }
      }

      # Add d and r as alternatives for F-tests (helpful for meta-analysis)
      if (df1 == 1) {
        # Equivalence to t-test: t = sqrt(F)
        # d = 2*r / sqrt(1-r^2) where r = sqrt(eta2)
        # Simplified: d = 2 * sqrt(F / df2) approx
        # r = sqrt(F / (F + df2))

        r_equiv <- sqrt(stat / (stat + df2))
        d_equiv <- 2 * sqrt(stat / df2)

        alternatives$r <- list(
          value = r_equiv,
          metadata = VARIANT_METADATA$r,
          why_consider = "Correlation equivalent (since df1=1)"
        )
        alternatives$d <- list(
          value = d_equiv,
          metadata = VARIANT_METADATA$d_ind,
          why_consider = "Cohen's d equivalent (assuming equal groups)"
        )

        # Try CIs for these
        n_equiv <- df1 + df2 + 1 # Approx N
        ci_r_val <- ci_r(r_equiv, n_equiv, ci_level_used)
        if (ci_r_val$success) alternatives$r$ci <- ci_r_val$bounds

        ci_d_val <- ci_d_ind_approx(d_equiv, n_equiv / 2, n_equiv / 2, ci_level_used)
        if (!any(is.na(ci_d_val))) alternatives$d$ci <- ci_d_val
      }

      # Compute CIs for eta-squared variants
      if (!is.na(anova_effects$partial_eta2)) {
        pe_ci <- tryCatch(ci_etap2(stat, df1, df2, ci_level_used), error = function(e) list(success = FALSE))
        if (pe_ci$success) computed_variants$partial_eta2$ci <- pe_ci$bounds
      }
      if (!is.na(anova_effects$eta2)) {
        e_ci <- tryCatch(ci_eta2(stat, df1, df2, ci_level_used), error = function(e) list(success = FALSE))
        if (e_ci$success) computed_variants$eta2$ci <- e_ci$bounds
      }

      if (design == "unclear") {
        uncertainty <- c(uncertainty, "ANOVA design unclear (between/within/mixed) \u2014 computed all variants")
      }
    }
  } else if (tt == "chisq") {
    # ------ CHI-SQUARE COMPUTATIONS ------

    # ========================================================================
    # CHI-SQUARE TABLE DIMENSION VALIDATION (Phase 2G)
    # For chi-square: df = (r-1) * (c-1)
    # ========================================================================

    # Helper function: Find all (r,c) pairs such that (r-1)*(c-1) = df
    find_compatible_dimensions <- function(df) {
      dims <- character(0)
      # Limit search to reasonable table sizes (2x2 up to 20x20)
      for (r in 2:20) {
        for (c in 2:20) {
          if ((r - 1) * (c - 1) == df) {
            dims <- c(dims, sprintf("%d\u00d7%d", r, c))
          }
        }
      }
      dims
    }

    # Validate if dimensions are reported
    if (!is.na(table_r) && !is.na(table_c) && !is.na(df1)) {
      expected_df <- (table_r - 1) * (table_c - 1)

      if (abs(df1 - expected_df) > 0.01) {
        # Mismatch!
        uncertainty <- c(
          uncertainty,
          sprintf(
            "Table dimensions (%d\u00d7%d) imply df=%d, but reported df=%.0f - dimensions or df may be incorrect",
            as.integer(table_r), as.integer(table_c),
            as.integer(expected_df), df1
          )
        )

        # Suggest compatible dimensions
        compatible <- find_compatible_dimensions(df1)
        if (length(compatible) > 0) {
          assumptions <- c(
            assumptions,
            sprintf(
              "For df=%.0f, compatible table dimensions are: %s",
              df1, paste(compatible, collapse = ", ")
            )
          )
        }
      }
    } else if ((is.na(table_r) || is.na(table_c)) && !is.na(df1)) {
      # Dimensions not reported - suggest from df
      compatible <- find_compatible_dimensions(df1)

      if (length(compatible) == 0) {
        uncertainty <- c(
          uncertainty,
          sprintf("df=%.0f does not correspond to standard rectangular contingency table", df1)
        )
      } else if (length(compatible) == 1) {
        assumptions <- c(
          assumptions,
          sprintf("Table dimensions inferred as %s from df=%.0f", compatible[1], df1)
        )
      } else {
        uncertainty <- c(
          uncertainty,
          sprintf(
            "Table dimensions not specified - df=%.0f compatible with: %s",
            df1, paste(compatible, collapse = ", ")
          )
        )
      }
    }

    if (!is.na(N) && N > 0) {
      phi_val <- tryCatch(phi_from_chisq(stat, N), error = function(e) NA_real_)
      if (!is.na(phi_val)) {
        computed_variants$phi <- list(
          value = phi_val,
          metadata = VARIANT_METADATA$phi
        )

        # Compute CI
        phi_ci_result <- tryCatch(ci_phi(phi_val, N, ci_level_used), error = function(e) {
          list(success = FALSE, bounds = c(NA_real_, NA_real_))
        })
        if (phi_ci_result$success) {
          computed_variants$phi$ci <- phi_ci_result$bounds
        }
      }

      # Cramer's V
      m_candidates <- NA
      if (!is.na(table_r) && !is.na(table_c)) {
        m_candidates <- min(table_r - 1, table_c - 1)
      } else if (!is.na(df1)) {
        m_candidates <- enumerate_m_from_df(df1)
        if (length(m_candidates) == 0) m_candidates <- NA
      }

      if (!is.na(m_candidates[1])) {
        V_val <- tryCatch(V_from_chisq(stat, N, m_candidates[1]), error = function(e) NA_real_)
        if (!is.na(V_val)) {
          if (!is.na(canonical_type) && canonical_type == "V") {
            computed_variants$V <- list(
              value = V_val,
              metadata = VARIANT_METADATA$V
            )
          } else {
            alternatives$V <- list(
              value = V_val,
              metadata = VARIANT_METADATA$V,
              why_consider = "Better for tables larger than 2x2"
            )
          }
        }
      }
    }
  } else if (tt == "z") {
    # ------ Z-TEST COMPUTATIONS ------
    if (!is.na(df1)) {
      beta_val <- tryCatch(standardized_beta_from_t(stat, df1), error = function(e) NA_real_)
      if (!is.na(beta_val)) {
        computed_variants$standardized_beta <- list(
          value = beta_val,
          metadata = VARIANT_METADATA$standardized_beta
        )
      }

      partial_r_val <- tryCatch(partial_r_from_t(stat, df1), error = function(e) NA_real_)
      if (!is.na(partial_r_val)) {
        alternatives$partial_r <- list(
          value = partial_r_val,
          metadata = VARIANT_METADATA$partial_r,
          why_consider = "Correlation-based interpretation"
        )
      }
    }
  }

  # ============================================================================
  # PHASE 5: Type-matched comparison - find closest SAME-TYPE variant
  # ============================================================================

  matched_variant <- NA_character_
  matched_value <- NA_real_
  delta_effect_abs <- NA_real_
  ambiguity_level <- "clear"
  ambiguity_reason <- NA_character_

  # Build list of same-type variants for comparison
  same_type_variants <- list()

  if (!is.na(effect_reported) && !is.na(canonical_type)) {
    # Get family info to determine which variants are same-type
    if (!is.null(family_info)) {
      valid_variants <- family_info$variants

      # Filter computed_variants to only same-type
      for (vname in names(computed_variants)) {
        # Check if this variant belongs to the same family
        base_name <- gsub("_equalN|_min|_max", "", vname)
        if (base_name %in% valid_variants || vname %in% valid_variants) {
          same_type_variants[[vname]] <- computed_variants[[vname]]
        }
      }
    } else {
      # Unknown type - use all computed variants
      same_type_variants <- computed_variants
      ambiguity_level <- "ambiguous"
      ambiguity_reason <- paste0("Unknown effect size type '", effect_reported_name, "' - compared to all computed variants")
    }

    # Find closest match among same-type variants
    if (length(same_type_variants) > 0) {
      diffs <- sapply(same_type_variants, function(v) {
        if (is.null(v$value) || is.na(v$value)) {
          return(Inf)
        }
        abs(abs(v$value) - abs(effect_reported))
      })

      if (any(is.finite(diffs))) {
        k <- which.min(diffs)
        matched_variant <- names(same_type_variants)[k]
        matched_value <- same_type_variants[[k]]$value
        delta_effect_abs <- diffs[k]

        # Check if multiple variants are close (ambiguous)
        close_variants <- names(diffs)[diffs <= delta_effect_abs * 1.5 & is.finite(diffs)]
        if (length(close_variants) > 1) {
          ambiguity_level <- "ambiguous"
          ambiguity_reason <- paste0("Multiple variants match similarly: ", paste(close_variants, collapse = ", "))
        }
      }
    } else {
      # No same-type variants computed - fall back to all variants
      ambiguity_level <- "highly_ambiguous"
      ambiguity_reason <- paste0("No same-type variants available for '", canonical_type, "' - using all computed variants")

      if (length(computed_variants) > 0) {
        diffs <- sapply(computed_variants, function(v) {
          if (is.null(v$value) || is.na(v$value)) {
            return(Inf)
          }
          abs(abs(v$value) - abs(effect_reported))
        })

        if (any(is.finite(diffs))) {
          k <- which.min(diffs)
          matched_variant <- names(computed_variants)[k]
          matched_value <- computed_variants[[k]]$value
          delta_effect_abs <- diffs[k]
        }
      }
    }
  } else if (!is.na(effect_reported)) {
    # No reported type - compare to all variants
    ambiguity_level <- "highly_ambiguous"
    ambiguity_reason <- "Effect size type not specified - compared to all computed variants"

    if (length(computed_variants) > 0) {
      diffs <- sapply(computed_variants, function(v) {
        if (is.null(v$value) || is.na(v$value)) {
          return(Inf)
        }
        abs(abs(v$value) - abs(effect_reported))
      })

      if (any(is.finite(diffs))) {
        k <- which.min(diffs)
        matched_variant <- names(computed_variants)[k]
        matched_value <- computed_variants[[k]]$value
        delta_effect_abs <- diffs[k]
      }
    }
  }

  # ============================================================================
  # PHASE 6: CI consistency checking
  # ============================================================================

  ci_match <- as.logical(NA)
  ci_delta_lower <- NA_real_
  ci_delta_upper <- NA_real_
  computed_ciL <- NA_real_
  computed_ciU <- NA_real_

  if (!is.na(ciL_rep) && !is.na(ciU_rep) && !is.na(matched_variant)) {
    # Get CI from matched variant if available
    if (matched_variant %in% names(computed_variants) &&
      !is.null(computed_variants[[matched_variant]]$ci)) {
      computed_ciL <- computed_variants[[matched_variant]]$ci[1]
      computed_ciU <- computed_variants[[matched_variant]]$ci[2]
    } else if (matched_variant %in% names(same_type_variants) &&
      !is.null(same_type_variants[[matched_variant]]$ci)) {
      computed_ciL <- same_type_variants[[matched_variant]]$ci[1]
      computed_ciU <- same_type_variants[[matched_variant]]$ci[2]
    }

    if (!is.na(computed_ciL) && !is.na(computed_ciU)) {
      ci_delta_lower <- abs(computed_ciL - ciL_rep)
      ci_delta_upper <- abs(computed_ciU - ciU_rep)
      ci_match <- (ci_delta_lower <= tol_ci) && (ci_delta_upper <= tol_ci)
    }
  }

  # If no effect was reported but we computed some, extract the "primary" CI
  # This ensures computed CIs are shown even when user didn't report an effect
  if (is.na(computed_ciL) || is.na(computed_ciU)) {
    # Try to find the primary computed effect's CI based on test type
    primary_effects <- if (tt == "t") {
      c("d_ind", "d_ind_equalN", "dz")
    } else if (tt == "F") {
      c("eta2", "partial_eta2")
    } else if (tt == "r") {
      c("r")
    } else if (tt == "chisq") {
      c("phi", "V")
    } else if (tt == "z") {
      c("d", "r")
    } else {
      c()
    }

    for (eff_name in primary_effects) {
      if (eff_name %in% names(computed_variants) &&
        !is.null(computed_variants[[eff_name]]$ci)) {
        computed_ciL <- computed_variants[[eff_name]]$ci[1]
        computed_ciU <- computed_variants[[eff_name]]$ci[2]
        break
      }
    }
  }

  # ============================================================================
  # PHASE 7: Status determination
  # ============================================================================

  status <- "WARN"

  # Get tolerance for this effect size type
  tol_eff <- tol_effect[[canonical_type]]
  if (is.null(tol_eff)) {
    default_tols <- list(
      d = 0.02, g = 0.02, dz = 0.02, dav = 0.02, drm = 0.02,
      r = 0.005, phi = 0.02, V = 0.02,
      eta2 = 0.01, etap2 = 0.01, omega2 = 0.01, cohens_f = 0.02,
      beta = 0.01, partial_r = 0.005, f2 = 0.02, R2 = 0.01
    )
    tol_eff <- default_tols[[canonical_type]]
    if (is.null(tol_eff)) tol_eff <- 0.02
  }

  if (!is.na(delta_effect_abs)) {
    # APA 7 Rounding-aware check: if both round to 2 decimals effect sizes (or 3 decimals p-values), it's a pass
    # This prevents warnings when calculation (e.g. 0.017) rounds to reported (e.g. 0.02)
    is_rounding_match <- FALSE
    if (!is.na(effect_reported) && !is.na(matched_value)) {
      if (round(abs(effect_reported), 2) == round(abs(matched_value), 2)) {
        is_rounding_match <- TRUE
      }
    }

    if (delta_effect_abs <= tol_eff || is_rounding_match) {
      if (ambiguity_level == "clear" || is_rounding_match) {
        status <- "PASS"
      } else {
        status <- "WARN"
        uncertainty <- c(uncertainty, "Match is good but comparison is ambiguous")
      }
    } else if (delta_effect_abs <= (3 * tol_eff)) {
      status <- "WARN"
    } else if (delta_effect_abs > (5 * tol_eff)) {
      status <- "ERROR"
    } else {
      status <- "WARN"
    }
  }

  # CI affects status
  if (!is.na(ci_match) && !ci_match) {
    if (status == "PASS") status <- "WARN"
    uncertainty <- c(uncertainty, sprintf(
      "CI bounds mismatch: lower diff=%.3f, upper diff=%.3f",
      ci_delta_lower, ci_delta_upper
    ))
  }

  # ============================================================================
  # PHASE 8: Design inference
  # ============================================================================

  # Initialize design_inferred only if not already set (Phase 2B may have set it)
  if (!exists("design_inferred")) {
    design_inferred <- "unclear"
  }

  if (tt == "t") {
    context_text <- ""
    if ("context_window" %in% names(row) && !is.na(row$context_window)) {
      context_text <- row$context_window
    }

    if (nchar(context_text) > 0) {
      context_lower <- tolower(context_text)
      paired_patterns <- c(
        "paired", "repeated measures", "within-subjects", "within subjects",
        "same participants", "dependent samples", "pre-post", "before-after"
      )
      independent_patterns <- c(
        "independent", "between-subjects", "between subjects",
        "between-groups", "different groups", "two-sample"
      )

      if (any(sapply(paired_patterns, function(p) grepl(p, context_lower)))) {
        design_inferred <- "paired"
      } else if (any(sapply(independent_patterns, function(p) grepl(p, context_lower)))) {
        design_inferred <- "independent"
      }
    }

    # Infer from reported effect type
    if (design_inferred == "unclear" && !is.na(canonical_type)) {
      if (canonical_type %in% c("dz", "dav", "drm")) {
        design_inferred <- "paired"
      } else if (canonical_type %in% c("d", "g")) {
        design_inferred <- "independent"
      }
    }
  } else if (tt == "F") {
    context_text <- ""
    if ("context_window" %in% names(row) && !is.na(row$context_window[1])) {
      context_text <- as.character(row$context_window[1])
    }

    if (nchar(context_text) > 0) {
      context_lower <- tolower(context_text)
      if (grepl("between|between-subjects", context_lower)) {
        design_inferred <- "between"
      } else if (grepl("within|within-subjects|repeated measures", context_lower)) {
        design_inferred <- "within"
      } else if (grepl("mixed|split-plot", context_lower)) {
        design_inferred <- "mixed"
      }
    }
  }

  # ============================================================================
  # PHASE 9: P-value and decision error detection
  # ============================================================================

  p_computed <- NA_real_
  decision_error <- FALSE

  if (!is.na(stat) && !is.na(df1)) {
    p_computed <- tryCatch(
      {
        if (tt == "t") {
          if (one_tailed) {
            stats::pt(abs(stat), df = df1, lower.tail = FALSE)
          } else {
            2 * stats::pt(abs(stat), df = df1, lower.tail = FALSE)
          }
        } else if (tt == "F" && !is.na(df2)) {
          stats::pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)
        } else if (tt == "z") {
          if (one_tailed) {
            stats::pnorm(abs(stat), lower.tail = FALSE)
          } else {
            2 * stats::pnorm(abs(stat), lower.tail = FALSE)
          }
        } else if (tt == "chisq") {
          stats::pchisq(stat, df = df1, lower.tail = FALSE)
        } else {
          NA_real_
        }
      },
      error = function(e) NA_real_
    )
  }

  if (!is.na(p_computed) && !is.na(p_reported)) {
    reported_significant <- p_reported < alpha
    computed_significant <- p_computed < alpha
    decision_error <- reported_significant != computed_significant

    if (decision_error) {
      if (reported_significant && !computed_significant) {
        uncertainty <- c(
          uncertainty,
          sprintf(
            "Decision error: reported p=%.4f (significant) but computed p=%.4f (not significant)",
            p_reported, p_computed
          )
        )
      } else {
        uncertainty <- c(
          uncertainty,
          sprintf(
            "Decision error: reported p=%.4f (not significant) but computed p=%.4f (significant)",
            p_reported, p_computed
          )
        )
      }
      if (status == "PASS") status <- "WARN"
    }
  }

  # ============================================================================
  # PHASE 10: Build variants_tested string and all_variants structure
  # ============================================================================

  # Build variants_tested string for backward compatibility
  variants_tested_parts <- c()
  for (vname in names(computed_variants)) {
    v <- computed_variants[[vname]]
    if (!is.null(v$value) && !is.na(v$value)) {
      variants_tested_parts <- c(
        variants_tested_parts,
        paste0(vname, "=", signif(v$value, 4))
      )
    }
  }
  variants_tested_str <- paste(variants_tested_parts, collapse = "; ")

  # Build all_variants JSON-like structure for new API
  all_variants_list <- list(
    same_type = lapply(same_type_variants, function(v) {
      list(
        value = v$value,
        assumptions = if (!is.null(v$metadata$assumptions)) v$metadata$assumptions else "",
        when_to_use = if (!is.null(v$metadata$when_to_use)) v$metadata$when_to_use else ""
      )
    }),
    alternatives = lapply(alternatives, function(v) {
      list(
        value = v$value,
        type = if (!is.null(v$metadata$name)) v$metadata$name else "",
        why_consider = if (!is.null(v$why_consider)) v$why_consider else ""
      )
    })
  )

  # Serialize to JSON string for storage
  all_variants_json <- tryCatch(
    {
      jsonlite::toJSON(all_variants_list, auto_unbox = TRUE)
    },
    error = function(e) {
      "{}"
    }
  )

  # ============================================================================
  # PHASE 11: Uncertainty level determination
  # ============================================================================

  uncertainty <- unique(uncertainty)
  assumptions <- unique(assumptions)

  uncertainty_level <- "low"
  if (ambiguity_level == "highly_ambiguous") {
    uncertainty_level <- "high"
  } else if (ambiguity_level == "ambiguous") {
    uncertainty_level <- "medium"
  }

  if (length(uncertainty) >= 3 || any(grepl("missing|unknown|unavailable|cannot", uncertainty, ignore.case = TRUE))) {
    uncertainty_level <- "high"
  } else if (length(uncertainty) >= 1 && uncertainty_level == "low") {
    uncertainty_level <- "medium"
  }

  # ============================================================================
  # PHASE 12: Assemble output tibble
  # ============================================================================

  # Extract individual computed values for backward compatibility columns
  # d_ind: prefer explicit d_ind, fall back to d_ind_equalN (when group sizes assumed equal)
  d_ind <- if ("d_ind" %in% names(computed_variants)) {
    computed_variants$d_ind$value
  } else if ("d_ind_equalN" %in% names(computed_variants)) {
    computed_variants$d_ind_equalN$value # Use equal-N variant if available
  } else {
    NA_real_
  }
  d_ind_equalN <- if ("d_ind_equalN" %in% names(computed_variants)) computed_variants$d_ind_equalN$value else NA_real_
  d_ind_min <- if ("d_ind_min" %in% names(computed_variants)) computed_variants$d_ind_min$value else NA_real_
  d_ind_max <- if ("d_ind_max" %in% names(computed_variants)) computed_variants$d_ind_max$value else NA_real_
  g_ind <- if ("g_ind" %in% names(alternatives)) alternatives$g_ind$value else NA_real_
  dz <- if ("dz" %in% names(computed_variants)) computed_variants$dz$value else NA_real_
  dav_val <- if ("dav" %in% names(computed_variants)) computed_variants$dav$value else NA_real_
  dav_min <- if ("dav" %in% names(computed_variants) && !is.null(computed_variants$dav$range)) computed_variants$dav$range[1] else NA_real_
  dav_max <- if ("dav" %in% names(computed_variants) && !is.null(computed_variants$dav$range)) computed_variants$dav$range[2] else NA_real_
  drm <- if ("drm" %in% names(computed_variants)) computed_variants$drm$value else NA_real_
  # r: prefer computed_variants (from r-tests), fall back to alternatives (from t-tests)
  r_val <- if ("r" %in% names(computed_variants)) {
    computed_variants$r$value
  } else if ("r" %in% names(alternatives)) {
    alternatives$r$value
  } else {
    NA_real_
  }
  r_ciL <- if ("r" %in% names(computed_variants) && !is.null(computed_variants$r$ci)) {
    computed_variants$r$ci[1]
  } else if ("r" %in% names(alternatives) && !is.null(alternatives$r$ci)) {
    alternatives$r$ci[1]
  } else {
    NA_real_
  }
  r_ciU <- if ("r" %in% names(computed_variants) && !is.null(computed_variants$r$ci)) {
    computed_variants$r$ci[2]
  } else if ("r" %in% names(alternatives) && !is.null(alternatives$r$ci)) {
    alternatives$r$ci[2]
  } else {
    NA_real_
  }

  phi_val <- if ("phi" %in% names(computed_variants)) computed_variants$phi$value else NA_real_
  phi_ciL <- if ("phi" %in% names(computed_variants) && !is.null(computed_variants$phi$ci)) computed_variants$phi$ci[1] else NA_real_
  phi_ciU <- if ("phi" %in% names(computed_variants) && !is.null(computed_variants$phi$ci)) computed_variants$phi$ci[2] else NA_real_
  V_val <- if ("V" %in% names(computed_variants)) computed_variants$V$value else if ("V" %in% names(alternatives)) alternatives$V$value else NA_real_
  eta_val <- if ("eta" %in% names(computed_variants)) computed_variants$eta$value else NA_real_
  eta2 <- if ("eta2" %in% names(computed_variants)) computed_variants$eta2$value else NA_real_
  partial_eta2 <- if ("partial_eta2" %in% names(computed_variants)) computed_variants$partial_eta2$value else NA_real_
  generalized_eta2 <- if ("generalized_eta2" %in% names(computed_variants)) computed_variants$generalized_eta2$value else NA_real_
  omega2 <- if ("omega2" %in% names(computed_variants)) computed_variants$omega2$value else if ("omega2" %in% names(alternatives)) alternatives$omega2$value else NA_real_
  cohens_f <- if ("cohens_f" %in% names(computed_variants)) computed_variants$cohens_f$value else if ("cohens_f" %in% names(alternatives)) alternatives$cohens_f$value else NA_real_
  standardized_beta <- if ("standardized_beta" %in% names(computed_variants)) computed_variants$standardized_beta$value else NA_real_
  partial_r <- if ("partial_r" %in% names(alternatives)) alternatives$partial_r$value else NA_real_

  tibble::tibble(
    location = row$location,
    raw_text = row$raw_text,
    context_window = if ("context_window" %in% names(row)) row$context_window else NA_character_,
    test_type = tt,
    df1 = df1,
    df2 = df2,
    stat_value = stat,
    N = N, n1 = n1, n2 = n2, table_r = table_r, table_c = table_c,

    # Reported values
    reported_type = canonical_type,
    effect_reported_name = effect_reported_name,
    effect_reported = effect_reported,
    ci_level = ci_level_used,
    ciL_reported = ciL_rep,
    ciU_reported = ciU_rep,

    # Phase 2 new columns (from parse.R)
    p_symbol = if ("p_symbol" %in% names(row)) row$p_symbol else NA_character_,
    p_valid = if ("p_valid" %in% names(row)) row$p_valid else NA,
    p_out_of_range = if ("p_out_of_range" %in% names(row)) row$p_out_of_range else NA,
    N_source = if ("N_source" %in% names(row)) row$N_source else NA_character_,
    effect_fallback = if ("effect_fallback" %in% names(row)) row$effect_fallback else NA,
    ci_level_source = if ("ci_level_source" %in% names(row)) row$ci_level_source else NA_character_,

    # Type-matched comparison results (NEW)
    matched_variant = matched_variant,
    matched_value = matched_value,
    delta_effect = delta_effect_abs,
    ambiguity_level = ambiguity_level,
    ambiguity_reason = if (!is.na(ambiguity_reason)) ambiguity_reason else "",

    # All variants (NEW - JSON structure)
    all_variants = as.character(all_variants_json),

    # Backward compatibility: individual computed values
    d_ind = if (!is.na(d_ind)) d_ind else if ("d" %in% names(alternatives)) alternatives$d$value else NA_real_,
    d_ind_equalN = d_ind_equalN,
    d_ind_min = d_ind_min,
    d_ind_max = d_ind_max,
    g_ind = g_ind,
    dz = dz,
    d_av_median = dav_val,
    d_av_min = dav_min,
    d_av_max = dav_max,
    drm = drm,
    r_from_t_or_reported = if (!is.na(r_val)) r_val else if ("r" %in% names(alternatives)) alternatives$r$value else NA_real_,
    r_ciL = r_ciL,
    r_ciU = r_ciU,
    phi = phi_val,
    phi_ciL = phi_ciL,
    phi_ciU = phi_ciU,
    V = V_val,
    eta = eta_val,
    eta2 = eta2,
    partial_eta2 = partial_eta2,
    generalized_eta2 = generalized_eta2,
    omega2 = omega2,
    cohens_f = cohens_f,
    standardized_beta = standardized_beta,
    partial_r = partial_r,
    semi_partial_r = NA_real_,
    cohens_f2 = NA_real_,
    R2 = NA_real_,

    # Comparison results
    closest_method = matched_variant, # Backward compat alias
    delta_effect_abs = delta_effect_abs, # Backward compat alias
    p_reported = p_reported,
    p_computed = p_computed,
    decision_error = decision_error,
    ci_match = ci_match,
    ciL_computed = computed_ciL,
    ciU_computed = computed_ciU,
    ci_delta_lower = ci_delta_lower,
    # Status and metadata

    # REPRO code generation
    repro_code = {
      code <- c(
        sprintf("# Reproducing check for %s test", tt),
        sprintf("stat <- %.4f", stat),
        if (!is.na(df1)) sprintf("df1 <- %.2f", df1) else NULL,
        if (!is.na(df2)) sprintf("df2 <- %.2f", df2) else NULL,
        if (!is.na(N)) sprintf("N <- %s", N) else NULL,
        "# Compute P-value"
      )

      # P-value code
      p_code <- if (tt == "t") {
        sprintf("p_val <- 2 * pt(abs(stat), df1, lower.tail = FALSE) # %.5g", p_computed)
      } else if (tt == "F") {
        sprintf("p_val <- pf(stat, df1, df2, lower.tail = FALSE) # %.5g", p_computed)
      } else if (tt == "z") {
        sprintf("p_val <- 2 * pnorm(abs(stat), lower.tail = FALSE) # %.5g", p_computed)
      } else if (tt == "chisq") {
        sprintf("p_val <- pchisq(stat, df1, lower.tail = FALSE) # %.5g", p_computed)
      } else if (tt == "r") {
        sprintf("# t-test for r\nt_stat <- stat * sqrt((df1) / (1 - stat^2))\np_val <- 2 * pt(abs(t_stat), df1, lower.tail = FALSE) # %.5g", p_computed)
      } else {
        "# Unknown test type"
      }
      code <- c(code, p_code)

      code <- c(code, "# Effect Size Computations")

      # Effect size code
      es_code <- if (tt == "t") {
        c(
          "d_ind <- 2 * stat / sqrt(df1) # Cohens d (approx)",
          if (!is.na(N)) sprintf("d_exact <- stat * sqrt(1/%.1f + 1/%.1f) # Assuming equal n", N / 2, N / 2) else NULL
        )
      } else if (tt == "F") {
        c(
          "eta2 <- (stat * df1) / (stat * df1 + df2)",
          "cohens_f <- sqrt(eta2 / (1 - eta2))",
          "# Non-centrality based CI (requires effectcheck or MBESS)",
          sprintf("# effectcheck:::ci_cohens_f(%.2f, %.0f, %.0f)", stat, df1, df2)
        )
      } else if (tt == "chisq") {
        if (!is.na(N)) "phi <- sqrt(stat / N)" else "# N needed for phi"
      } else {
        NULL
      }
      code <- c(code, es_code)

      paste(code, collapse = "\n")
    },

    # REPRO output generation (Simulated)
    repro_output = {
      out <- c(
        sprintf("> stat"),
        sprintf("[1] %.4f", stat),
        sprintf("> p_val"),
        sprintf("[1] %.5g", p_computed)
      )

      if (tt == "t") {
        if ("d_ind" %in% names(computed_variants)) {
          out <- c(out, "> d_ind", sprintf("[1] %.4f", computed_variants$d_ind$value))
        } else if ("d_ind" %in% names(alternatives)) { # Fallback code is calculating d_ind as primary
          out <- c(out, "> d_ind", sprintf("[1] %.4f", alternatives$d_ind$value))
        } else {
          # Fallback calc for output display if not stored (e.g. from code logic)
          out <- c(out, "> d_ind", sprintf("[1] %.4f", 2 * stat / sqrt(df1)))
        }
      } else if (tt == "F") {
        if ("eta2" %in% names(computed_variants)) {
          out <- c(out, "> eta2", sprintf("[1] %.4f", computed_variants$eta2$value))
        } else {
          eta2_sim <- (stat * df1) / (stat * df1 + df2)
          out <- c(out, "> eta2", sprintf("[1] %.4f", eta2_sim))
        }

        if ("cohens_f" %in% names(computed_variants)) {
          out <- c(out, "> cohens_f", sprintf("[1] %.4f", computed_variants$cohens_f$value))
        } else {
          eta2_sim <- (stat * df1) / (stat * df1 + df2)
          f_sim <- sqrt(eta2_sim / (1 - eta2_sim))
          out <- c(out, "> cohens_f", sprintf("[1] %.4f", f_sim))
        }
      } else if (tt == "chisq") {
        if ("phi" %in% names(computed_variants)) {
          out <- c(out, "> phi", sprintf("[1] %.4f", computed_variants$phi$value))
        }
      } else if (tt == "r") {
        out <- c(out, "> r", sprintf("[1] %.4f", stat))
      } else if (tt == "z") {
        # d or r
        if ("d" %in% names(computed_variants)) {
          out <- c(out, "> d_equiv", sprintf("[1] %.4f", computed_variants$d$value))
        }
      }

      paste(out, collapse = "\n")
    },

    # Status and metadata
    status = status,
    design_ambiguous = (ambiguity_level != "clear"),
    sign_mismatch = FALSE, # TODO: implement sign checking
    design_inferred = design_inferred,
    variants_tested = variants_tested_str,
    uncertainty_level = uncertainty_level,
    uncertainty_reasons = if (length(uncertainty)) paste(uncertainty, collapse = "; ") else "",
    assumptions_used = if (length(assumptions)) paste(assumptions, collapse = "; ") else "",
    insufficient_data = (length(computed_variants) == 0)
  )
}

#' Check raw text for statistical consistency
#'
#' Parses APA-style statistical results from text and checks for consistency
#' between reported and computed values. Uses type-matched comparison to ensure
#' reported effect sizes are compared against the same type of computed values.
#'
#' @param text Character vector of text to check
#' @param stats Character vector of test types to check (default: all supported types)
#' @param ci_level Default confidence interval level (default 0.95)
#' @param alpha Significance threshold for decision error detection (default 0.05)
#' @param one_tailed Logical, assume one-tailed tests (default FALSE)
#' @param paired_r_grid Numeric vector of correlation values for paired t-test grid search
#' @param assume_equal_ns_when_missing Logical, assume equal group sizes when missing (default TRUE)
#' @param ci_method_phi CI method for phi coefficient (default "bonett_price")
#' @param ci_method_V CI method for Cramer's V (default "bonett_price")
#' @param tol_effect List of tolerances for effect sizes by type
#' @param tol_ci Tolerance for CI bounds (default 0.02)
#' @param tol_p Tolerance for p-values (default 0.001)
#' @param messages Logical, show progress messages (default FALSE)
#' @return An effectcheck S3 object with consistency check results
#' @export
#' @examples
#' \dontrun{
#' text <- "t(28) = 2.21, p = .035, d = 0.80, 95% CI [0.12, 1.48]"
#' result <- check_text(text)
#' print(result)
#' summary(result)
#' }
check_text <- function(text,
                       stats = c("t", "F", "r", "chisq", "z"),
                       ci_level = 0.95,
                       alpha = 0.05,
                       one_tailed = FALSE,
                       paired_r_grid = seq(0.1, 0.9, by = 0.1),
                       assume_equal_ns_when_missing = TRUE,
                       ci_method_phi = "bonett_price",
                       ci_method_V = "bonett_price",
                       tol_effect = list(d = 0.02, r = 0.005, phi = 0.02, V = 0.02),
                       tol_ci = 0.02,
                       tol_p = 0.001,
                       messages = FALSE,
                       max_text_length = 10^7,
                       max_stats_per_text = 10000) {
  # Resource Limit Check: Text Length
  if (sum(nchar(text)) > max_text_length) {
    stop("Text too long. Maximum total length: ", max_text_length, " characters")
  }

  # Store settings for reproducibility
  settings <- list(
    stats = stats,
    ci_level = ci_level,
    alpha = alpha,
    one_tailed = one_tailed,
    paired_r_grid = paired_r_grid,
    assume_equal_ns_when_missing = assume_equal_ns_when_missing,
    ci_method_phi = ci_method_phi,
    ci_method_V = ci_method_V,
    tol_effect = tol_effect,
    tol_ci = tol_ci,
    tol_p = tol_p,
    max_text_length = max_text_length,
    max_stats_per_text = max_stats_per_text
  )

  parsed <- parse_text(text)

  # Resource Limit Check: Number of statistics
  if (nrow(parsed) > max_stats_per_text) {
    if (messages) {
      message("Too many statistics found (", nrow(parsed), "). Processing first ", max_stats_per_text, ".")
    }
    warning("Truncating results: Found ", nrow(parsed), " statistics, but limit is ", max_stats_per_text)
    parsed <- parsed[1:max_stats_per_text, ]
  }

  if (nrow(parsed) == 0) {
    return(new_effectcheck(tibble::tibble(), call = match.call(), settings = settings))
  }

  # Filter by requested test types
  if (!is.null(stats) && "test_type" %in% names(parsed)) {
    parsed <- parsed[parsed$test_type %in% stats, ]
    if (nrow(parsed) == 0) {
      return(new_effectcheck(tibble::tibble(), call = match.call(), settings = settings))
    }
  }

  rows <- split(parsed, seq_len(nrow(parsed)))
  res <- purrr::map_dfr(seq_along(rows), function(i) {
    rw <- rows[[i]]
    ci_lvl <- if (!is.null(rw$ci_level) && length(rw$ci_level) > 0 && !is.na(rw$ci_level[1])) {
      as.numeric(rw$ci_level[1])
    } else {
      ci_level
    }

    tryCatch(
      {
        compute_and_compare_one(rw,
          ci_level = ci_lvl,
          alpha = alpha,
          one_tailed = one_tailed,
          paired_r_grid = paired_r_grid,
          assume_equal_ns_when_missing = assume_equal_ns_when_missing,
          tol_effect = tol_effect,
          tol_ci = tol_ci,
          tol_p = tol_p
        )
      },
      error = function(e) {
        error_msg <- conditionMessage(e)
        # Log error with context for debugging
        warning(
          sprintf(
            "Error processing text at location %s: %s\nText snippet: %s",
            if (!is.null(rw$location)) rw$location else i,
            error_msg,
            if (!is.null(rw$raw_text)) substr(rw$raw_text, 1, 100) else "N/A"
          ),
          call. = FALSE
        )

        tibble::tibble(
          location = if (!is.null(rw$location)) rw$location else i,
          raw_text = if (!is.null(rw$raw_text)) rw$raw_text else "",
          context_window = if (!is.null(rw$context_window)) rw$context_window else "",
          test_type = if (!is.null(rw$test_type)) rw$test_type else "unknown",
          df1 = if (!is.null(rw$df1)) rw$df1 else NA_real_,
          df2 = if (!is.null(rw$df2)) rw$df2 else NA_real_,
          stat_value = if (!is.null(rw$stat_value)) rw$stat_value else NA_real_,
          reported_type = NA_character_,
          effect_reported_name = if (!is.null(rw$effect_reported_name)) rw$effect_reported_name else "",
          effect_reported = if (!is.null(rw$effect_reported)) rw$effect_reported else NA_real_,
          matched_variant = NA_character_,
          matched_value = NA_real_,
          delta_effect = NA_real_,
          ambiguity_level = "highly_ambiguous",
          ambiguity_reason = paste0("Processing error: ", error_msg),
          all_variants = "{}",
          status = "ERROR",
          design_inferred = "unclear",
          variants_tested = "",
          uncertainty_level = "high",
          uncertainty_reasons = paste0("Processing error: ", error_msg),
          assumptions_used = "",
          insufficient_data = TRUE,
          decision_error = FALSE,
          p_reported = NA_real_,
          p_computed = NA_real_,
          d_ind = NA_real_, d_ind_equalN = NA_real_, d_ind_min = NA_real_, d_ind_max = NA_real_,
          g_ind = NA_real_, dz = NA_real_, d_av_median = NA_real_, d_av_min = NA_real_,
          d_av_max = NA_real_, drm = NA_real_, r_from_t_or_reported = NA_real_,
          r_ciL = NA_real_, r_ciU = NA_real_, phi = NA_real_, phi_ciL = NA_real_,
          phi_ciU = NA_real_, V = NA_real_, eta2 = NA_real_, partial_eta2 = NA_real_,
          generalized_eta2 = NA_real_, omega2 = NA_real_, cohens_f = NA_real_,
          standardized_beta = NA_real_, partial_r = NA_real_, semi_partial_r = NA_real_,
          cohens_f2 = NA_real_, R2 = NA_real_, closest_method = NA_character_,
          delta_effect_abs = NA_real_, ci_match = as.logical(NA),
          ci_delta_lower = NA_real_, ci_delta_upper = NA_real_
        )
      }
    )
  })

  # Mark insufficient_data
  res$insufficient_data <- with(
    res,
    is.na(d_ind) & is.na(d_ind_equalN) & is.na(dz) & is.na(r_from_t_or_reported) &
      is.na(phi) & is.na(V) & is.na(eta2) & is.na(partial_eta2) & is.na(omega2) & is.na(cohens_f) &
      is.na(standardized_beta) & is.na(partial_r) & is.na(cohens_f2) & is.na(R2)
  )

  res$status[res$insufficient_data & (is.na(res$status) | res$status == "")] <- "INSUFFICIENT_DATA"
  res$source <- "text"

  new_effectcheck(res, call = match.call(), settings = settings)
}

#' Check files for statistical consistency
#'
#' Reads one or more files and checks all detected statistics for consistency.
#'
#' @param paths Character vector of file paths (.pdf, .html, .docx, or .txt)
#' @param try_tables Logical, attempt table extraction from PDFs (default TRUE)
#' @param try_ocr Logical, attempt OCR for scanned PDFs (default FALSE)
#' @param messages Logical, show progress messages (default TRUE)
#' @param ... Additional arguments passed to check_text()
#' @return An effectcheck S3 object with consistency check results
#' @export
#' @examples
#' \dontrun{
#' results <- check_files(c("paper1.pdf", "paper2.docx"))
#' print(results)
#' summary(results)
#' }
check_files <- function(paths, try_tables = TRUE, try_ocr = FALSE, messages = TRUE, ...) {
  if (length(paths) == 0) {
    return(new_effectcheck(tibble::tibble(), call = match.call()))
  }

  if (messages) {
    message(sprintf("Processing %d file(s)...", length(paths)))
  }

  all_results <- purrr::map(seq_along(paths), function(i) {
    p <- paths[i]

    if (messages) {
      message(sprintf("[%d/%d] %s", i, length(paths), basename(p)))
    }

    tryCatch(
      {
        txt <- read_any_text(p, try_tables = try_tables, try_ocr = try_ocr)
        out <- check_text(txt, messages = FALSE, ...)
        if (nrow(out) > 0) {
          out$source <- basename(p)
          out$source_path <- p
        }
        out
      },
      error = function(e) {
        if (messages) {
          message(sprintf("  Error: %s", e$message))
        }
        tibble::tibble()
      }
    )
  })

  combined <- dplyr::bind_rows(all_results)

  if (messages) {
    message(sprintf("\nFound %d statistics in %d file(s)", nrow(combined), length(paths)))
  }

  new_effectcheck(combined, call = match.call(), settings = list(...))
}
