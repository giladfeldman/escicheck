#' Effect Size Type Definitions
#'
#' Maps reported effect size names to their family and variants.
#' Based on Guide to Effect Sizes and Confidence Intervals (Jane et al., 2024)
#' https://matthewbjane.quarto.pub/
#'
#' @keywords internal
EFFECT_SIZE_FAMILIES <- list(
  # Cohen's d family (standardized mean differences)
  d = list(
    family = "d",
    variants = c("d_ind", "d_ind_equalN", "d_ind_min", "d_ind_max", "dz", "dav", "drm", "g_ind"),
    alternatives = character(0),
    description = "Cohen's d - standardized mean difference"
  ),
  g = list(
    family = "g",
    variants = c("g_ind", "g_ind_Nm1", "gz", "gav", "grm"),
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
  partial_omega2 = list(
    family = "omega2",
    variants = c("partial_omega2", "omega2"),
    alternatives = c("eta2", "partial_eta2", "cohens_f"),
    description = "Partial omega-squared - SPSS factorial ANOVA"
  ),
  cohens_f = list(
    family = "f",
    variants = c("cohens_f", "cohens_f_omega"),
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
    variants = c("R2", "adjusted_R2"),
    alternatives = c("cohens_f2"),
    description = "R-squared - proportion of variance explained"
  ),
  adjusted_R2 = list(
    family = "R2",
    variants = c("adjusted_R2", "R2"),
    alternatives = c("cohens_f2"),
    description = "Adjusted R-squared - corrected for number of predictors"
  ),
  f2 = list(
    family = "f2",
    variants = c("cohens_f2"),
    alternatives = c("R2"),
    description = "Cohen's f\u00b2 - regression effect size"
  ),
  # Nonparametric effect sizes
  rank_biserial_r = list(
    family = "rank_biserial_r",
    variants = c("rank_biserial_r"),
    alternatives = c("cliffs_delta"),
    description = "Rank-biserial correlation from Mann-Whitney U"
  ),
  cliffs_delta = list(
    family = "cliffs_delta",
    variants = c("cliffs_delta"),
    alternatives = c("rank_biserial_r"),
    description = "Cliff's delta from Mann-Whitney U"
  ),
  epsilon_squared = list(
    family = "epsilon_squared",
    variants = c("epsilon_squared"),
    alternatives = c("kendalls_W"),
    description = "Epsilon-squared from Kruskal-Wallis H"
  ),
  kendalls_W = list(
    family = "kendalls_W",
    variants = c("kendalls_W"),
    alternatives = c("epsilon_squared"),
    description = "Kendall's W coefficient of concordance"
  ),
  # Odds/Risk ratio family (parsed but not yet computable from test statistics)
  OR = list(
    family = "OR",
    variants = character(0),
    alternatives = c("RR"),
    description = "Odds ratio"
  ),
  RR = list(
    family = "RR",
    variants = character(0),
    alternatives = c("OR"),
    description = "Risk ratio"
  ),
  IRR = list(
    family = "IRR",
    variants = character(0),
    alternatives = character(0),
    description = "Incidence rate ratio"
  ),
  # Cohen's h (proportion-based effect size)
  h = list(
    family = "h",
    variants = character(0),
    alternatives = c("phi"),
    description = "Cohen's h - effect size for comparing proportions"
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
    formula = "\u03b7p\u00b2 = SS_effect / (SS_effect + SS_error)"
  ),
  generalized_eta2 = list(
    name = "Generalized eta-squared",
    assumptions = "Mixed/repeated measures ANOVA",
    when_to_use = "Comparing effects across different designs",
    formula = "\u03b7G\u00b2 = SS_effect / (SS_effect + SS_subjects + SS_error)"
  ),
  omega2 = list(
    name = "Omega-squared",
    assumptions = "Population estimate",
    when_to_use = "Less biased than \u03b7\u00b2, better for small samples",
    formula = "\u03c9\u00b2 = (SS_effect - df_effect*MS_error) / (SS_total + MS_error)"
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
    formula = "\u03c6 = sqrt(\u03c7\u00b2 / N)"
  ),
  V = list(
    name = "Cramer's V",
    assumptions = "Larger contingency tables",
    when_to_use = "Association in tables larger than 2x2",
    formula = "V = sqrt(\u03c7\u00b2 / (N * min(r-1, c-1)))"
  ),
  standardized_beta = list(
    name = "Standardized beta",
    assumptions = "Multiple regression",
    when_to_use = "Comparing predictor importance in regression",
    formula = "\u03b2 = b * (SD_x / SD_y)"
  ),
  partial_r = list(
    name = "Partial r",
    assumptions = "Multiple regression",
    when_to_use = "Unique contribution controlling for other predictors",
    formula = "partial r = t / sqrt(t\u00b2 + df_residual)"
  ),
  cohens_f2 = list(
    name = "Cohen's f\u00b2",
    assumptions = "Regression effect size",
    when_to_use = "Effect size for R\u00b2 change, power analysis",
    formula = "f\u00b2 = R\u00b2 / (1 - R\u00b2)"
  ),
  R2 = list(
    name = "R-squared",
    assumptions = "Linear regression",
    when_to_use = "Proportion of variance explained by model",
    formula = "R\u00b2 = SS_regression / SS_total"
  ),
  rank_biserial_r = list(
    name = "Rank-biserial r",
    assumptions = "Mann-Whitney U test, known group sizes",
    when_to_use = "Effect size for nonparametric two-group comparison",
    formula = "r_rb = 1 - 2*U / (n1*n2)"
  ),
  cliffs_delta = list(
    name = "Cliff's delta",
    assumptions = "Mann-Whitney U test, known group sizes",
    when_to_use = "Probability-based nonparametric effect size",
    formula = "delta = 2*U / (n1*n2) - 1"
  ),
  epsilon_squared = list(
    name = "Epsilon-squared",
    assumptions = "Kruskal-Wallis H test",
    when_to_use = "Nonparametric analog of eta-squared",
    formula = "epsilon2 = (H - k + 1) / (N - k)"
  ),
  kendalls_W = list(
    name = "Kendall's W",
    assumptions = "Friedman test or concordance analysis",
    when_to_use = "Agreement among multiple raters/repeated measures",
    formula = "W = chi2 / (N * (k - 1))"
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
#' @param cross_type_action Action when cross-type match found ("NOTE", "WARN", or "ERROR")
#' @param ci_affects_status Whether CI mismatches affect status (default TRUE)
#' @param plausibility_filter Whether to apply plausibility bounds filter (default TRUE)
#' @param sign_sensitive Whether sign differences affect status (default FALSE)
#' @param method_context_action Action when method context detected in chunk ("NOTE", "WARN", "SKIP")
#' @param design_ambiguous_action Action when design-ambiguous t-test (or F(1,df)) effect size ERROR occurs ("WARN", "NOTE", or "ERROR"; default "WARN")
#' @param unknown_groups_action Action when d/g ERROR occurs with unknown group sizes n1/n2 ("WARN", "NOTE", or "ERROR"; default "WARN")
#' @return A tibble with comparison results
#' @keywords internal
compute_and_compare_one <- function(row,
                                    ci_level = 0.95,
                                    alpha = 0.05,
                                    one_tailed = FALSE,
                                    paired_r_grid = c(seq(0.1, 0.9, by = 0.1), 0.95),
                                    assume_equal_ns_when_missing = TRUE,
                                    tol_effect = list(d = 0.02, r = 0.005, phi = 0.02, V = 0.02),
                                    tol_ci = 0.02,
                                    tol_p = 0.001,
                                    cross_type_action = "NOTE",
                                    ci_affects_status = TRUE,
                                    plausibility_filter = TRUE,
                                    sign_sensitive = FALSE,
                                    method_context_action = "NOTE",
                                    design_ambiguous_action = "WARN",
                                    unknown_groups_action = "WARN") {
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

  # P-value symbol extraction for inequality handling (< vs = vs >)
  p_sym <- if ("p_symbol" %in% names(row) && length(row$p_symbol) > 0) {
    as.character(row$p_symbol[1])
  } else {
    NA_character_
  }
  # Flag: p was reported as inequality (e.g., "p < .001")
  p_is_inequality <- !is.na(p_sym) && grepl("<", p_sym)

  # Flag: p was reported as "ns" / "n.s." (not significant)
  p_ns <- if ("p_ns" %in% names(row) && length(row$p_ns) > 0) isTRUE(row$p_ns[1]) else FALSE

  # Flag: one-tailed test detected from text near this result
  one_tailed_local <- if ("one_tailed_detected" %in% names(row) &&
                          length(row$one_tailed_detected) > 0)
    isTRUE(row$one_tailed_detected[1]) else FALSE

  # Flag: two-tailed test explicitly detected from text near this result
  two_tailed_local <- if ("two_tailed_detected" %in% names(row) &&
                          length(row$two_tailed_detected) > 0)
    isTRUE(row$two_tailed_detected[1]) else FALSE

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
    "partial_omega2" = "partial_omega2", "partial omega2" = "partial_omega2",
    "partial omega-squared" = "partial_omega2", "partial omega squared" = "partial_omega2",
    "f" = "cohens_f", "cohen's f" = "cohens_f", "cohens f" = "cohens_f",
    "phi" = "phi", "\u03c6" = "phi",
    "v" = "V", "cramer's v" = "V", "cramers v" = "V",
    "beta" = "beta", "\u03b2" = "beta", "standardized beta" = "beta",
    "r2" = "R2", "r-squared" = "R2", "r squared" = "R2",
    "adjusted r2" = "adjusted_R2", "adj. r2" = "adjusted_R2", "r2adj" = "adjusted_R2",
    "r2_adj" = "adjusted_R2", "adjusted r-squared" = "adjusted_R2",
    "f2" = "f2", "f-squared" = "f2", "f squared" = "f2", "cohen's f2" = "f2",
    "rank_biserial_r" = "rank_biserial_r", "rank biserial r" = "rank_biserial_r",
    "cliffs_delta" = "cliffs_delta", "cliff's delta" = "cliffs_delta",
    "epsilon_squared" = "epsilon_squared", "epsilon squared" = "epsilon_squared",
    "kendalls_w" = "kendalls_W", "kendall's w" = "kendalls_W",
    "or" = "OR", "odds ratio" = "OR",
    "rr" = "RR", "risk ratio" = "RR",
    "irr" = "IRR", "incidence rate ratio" = "IRR",
    "h" = "h", "cohen's h" = "h", "cohens h" = "h"
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
      # When df1=1, F is equivalent to t\u00b2, so d/g/r are also valid
      valid_types <- c("eta", "eta2", "etap2", "omega2", "partial_omega2", "epsilon_squared", "cohens_f", "R2", "f2")
      if (!is.na(df1) && df1 == 1) {
        valid_types <- c(valid_types, "d", "g", "dz", "dav", "drm", "r")
      }
      if (!canonical_type %in% valid_types) {
        valid_for_test <- FALSE
        suggestion <- "For F-tests/ANOVA, typical effect sizes are eta-squared (\u03b7\u00b2), partial eta-squared (\u03b7p\u00b2), omega-squared (\u03c9\u00b2), or Cohen's f"
      }
    } else if (tt == "r") {
      # Correlation: r, R\u00b2, or f\u00b2
      valid_types <- c("r", "R2", "f2")
      if (!canonical_type %in% valid_types) {
        valid_for_test <- FALSE
        suggestion <- "For correlations, typical effect sizes are Pearson's r, R-squared, or Cohen's f\u00b2"
      }
    } else if (tt == "chisq") {
      # Chi-square: phi (2\u00d72), Cramer's V (larger tables), OR, RR, h
      valid_types <- c("phi", "V", "OR", "RR", "IRR", "h")
      if (!canonical_type %in% valid_types) {
        valid_for_test <- FALSE
        suggestion <- "For chi-square tests, typical effect sizes are phi (\u03c6), Cramer's V, OR, RR, or Cohen's h"
      }
    } else if (tt == "z") {
      # z-tests: similar to t-tests, plus OR/RR/h
      valid_types <- c("d", "g", "r", "R2", "OR", "RR", "IRR", "h")
      if (!canonical_type %in% valid_types) {
        valid_for_test <- FALSE
        suggestion <- "For z-tests, typical effect sizes are Cohen's d, Hedges' g, correlation r, OR, RR, or Cohen's h"
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
    z = c("r", "d", "g", "beta"), # z-tests: various
    regression = c("beta", "partial_r", "R2", "f2", "d", "r"), # Regression coefficients
    U = c("rank_biserial_r", "cliffs_delta"), # Mann-Whitney U
    W = c("rank_biserial_r"), # Wilcoxon W
    H = c("epsilon_squared", "kendalls_W") # Kruskal-Wallis H
  )
  # F(df1=1) is equivalent to t^2: d, g, r are also valid
  if (!is.na(tt) && tt == "F" && !is.na(df1) && as.numeric(df1[1]) == 1) {
    valid_effects_for_test[["F"]] <- c(valid_effects_for_test[["F"]], "d", "g", "dz", "dav", "drm", "r")
  }

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

  mixed_model_F_detected <- FALSE  # v0.2.9: initialized for all test types
  n_much_larger_than_df <- FALSE   # v0.2.9d: N >> df+1 detection flag
  phi_to_v_reinterpreted <- FALSE  # v0.2.9: initialized for all test types

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

    # Fix 2: Minimum N guard -- reject implausibly small N for any t-test
    # Any t-test requires at least 2 observations; N < df+1 is impossible
    if (!is.na(N) && !is.na(df1) && df1 > 0 && N < round(df1) + 1) {
      uncertainty <- c(uncertainty,
        sprintf("Extracted N=%d is implausibly small for df=%.1f (minimum %d). Likely parsing error.",
                as.integer(N), df1, as.integer(round(df1) + 1)))
      N <- NA_real_ # Force re-inference from df below
    }

    # Validate reported N against df
    if (!is.na(N) && !is.na(df1) && df1 > 0) {
      if (is_welch) {
        # Fix 3: Welch validation -- N >= round(df) + 2 guaranteed by
        # Welch-Satterthwaite equation (df_w <= n1+n2-2, so N >= df_w+2)
        min_N_welch <- round(df1) + 2
        if (N < min_N_welch) {
          N_original <- N
          N <- NA_real_ # Force re-inference below
          uncertainty <- c(uncertainty,
            sprintf("N=%d below Welch minimum (df+2=%d). Will re-estimate.",
                    as.integer(N_original), as.integer(min_N_welch)))
        } else if ("N_source" %in% names(row) && !is.na(row$N_source[1]) &&
                    row$N_source[1] == "global_text" &&
                    N > 1.5 * min_N_welch &&
                    !is.na(effect_reported) && abs(effect_reported) > 0.001 &&
                    !is.na(canonical_type) && canonical_type %in% c("d", "g")) {
          # Global N is much larger than df+2 -- likely from a different study.
          # Cross-validate: back-compute N from reported d (equal-n assumption)
          N_from_d <- round(4 * stat^2 / effect_reported^2)
          # Allow small tolerance: back-computation assumes equal groups, which
          # may underestimate N slightly relative to df+2
          if (N_from_d >= (min_N_welch - 5) && N_from_d < N) {
            N_original <- N
            N <- N_from_d
            assumptions <- c(assumptions,
              sprintf("Welch: global N=%d likely from different study; used N=%d from reported d",
                      as.integer(N_original), as.integer(N)))
            uncertainty <- c(uncertainty,
              "Global sample size overridden by back-computation from reported effect size")
          }
        }
      } else {
        # Original non-Welch validation
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
          # v0.2.9d: Set flag for Phase 8F downgrade
          n_much_larger_than_df <- TRUE
          uncertainty <- c(
            uncertainty,
            sprintf(
              "Reported N (%d) is larger than expected (%.0f-%.0f) for df=%.0f",
              as.integer(N), min_expected_N, max_expected_N, df1
            )
          )
        }
      }
    }

    # Infer N when not reported (or if replaced above)
    if (is.na(N) && !is.na(df1) && df1 > 0) {
      if (is_welch) {
        # Fix 4: Enhanced Welch N estimation
        # Primary: N >= round(df+2) from Welch-Satterthwaite lower bound
        N_from_df <- round(df1 + 2)

        # Secondary: back-compute N from reported d (equal-n assumption)
        # d = t * sqrt(1/n1 + 1/n2); with n1=n2=N/2: d = 2t/sqrt(N), so N = 4t^2/d^2
        N_from_d <- NA_real_
        if (!is.na(effect_reported) && abs(effect_reported) > 0.001 &&
            !is.na(canonical_type) && canonical_type %in% c("d", "g")) {
          N_from_d <- round(4 * stat^2 / effect_reported^2)
          # Cross-validate: N from d must be >= df+2 lower bound
          if (N_from_d < N_from_df) N_from_d <- NA_real_
        }

        if (!is.na(N_from_d)) {
          N <- N_from_d
          assumptions <- c(assumptions,
            sprintf("Welch: estimated N=%d from reported d (validated against df+2=%d)",
                    as.integer(N), as.integer(N_from_df)))
        } else {
          N <- N_from_df
          assumptions <- c(assumptions,
            sprintf("Welch: estimated N~%d (df+2 lower bound)", as.integer(N)))
        }
        uncertainty <- c(uncertainty,
          "Sample size uncertain for Welch's test - actual N may be larger"
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

    # Store both N values for variant computation
    # Paired N is ALWAYS df+1 (by definition), regardless of inferred design or extracted N
    if (exists("design_inferred") && design_inferred == "ambiguous") {
      if (!exists("N_paired")) N_paired <- df1 + 1
      N_independent <- N
    } else {
      N_independent <- N
      N_paired <- if (!is.na(df1) && df1 > 0) df1 + 1 else N
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

    # Fix 5A: Final N sanity check -- if N is still < 2 after all inference,
    # mark as unreliable rather than computing garbage effect sizes
    if (!is.na(N) && N < 2) {
      N <- NA_real_
      assumptions <- c(assumptions, "Could not determine a valid sample size (N >= 2)")
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

      # Also compute Hedges' g -- promote to computed_variants when canonical_type is "g"
      g_ind <- tryCatch(g_ind_from_t(stat, n1, n2), error = function(e) NA_real_)
      if (!is.na(g_ind)) {
        if (!is.na(canonical_type) && canonical_type == "g") {
          computed_variants$g_ind <- list(value = g_ind, metadata = VARIANT_METADATA$g_ind)
        } else {
          alternatives$g_ind <- list(
            value = g_ind,
            metadata = VARIANT_METADATA$g_ind,
            why_consider = "Bias-corrected version of d, recommended for small samples (n < 20)"
          )
        }
      }

      # v0.3.0: J(N-1) variant of Hedges' g (some software convention)
      # Standard uses J(df) where df=n1+n2-2; some use J(N-1) where N=n1+n2
      if (!is.na(d_ind) && !is.na(n1) && !is.na(n2)) {
        N_total <- n1 + n2
        J_Nm1 <- hedges_J(N_total - 1)
        if (!is.na(J_Nm1)) {
          g_Nm1 <- d_ind * J_Nm1
          alternatives$g_ind_Nm1 <- list(
            value = g_Nm1,
            metadata = list(name = "Hedges' g (J with N-1)",
                            assumptions = "J = 1-3/(4(N-1)-1); some SAS/SPSS versions"),
            why_consider = "Software convention: J(N-1) vs J(N-2) for independent samples"
          )
        }
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

          # Hedges' g for equal N -- promote when canonical_type is "g"
          g_ind <- tryCatch(g_ind_from_t(stat, n1_eq, n2_eq), error = function(e) NA_real_)
          if (!is.na(g_ind)) {
            if (!is.na(canonical_type) && canonical_type == "g") {
              computed_variants$g_ind <- list(value = g_ind, metadata = VARIANT_METADATA$g_ind)
            } else {
              alternatives$g_ind <- list(
                value = g_ind,
                metadata = VARIANT_METADATA$g_ind,
                why_consider = "Bias-corrected version of d, recommended for small samples"
              )
            }
          }

          # v0.3.0: J(N-1) variant for equal-N case
          if (!is.na(d_ind_equalN)) {
            J_Nm1 <- hedges_J(N - 1)
            if (!is.na(J_Nm1)) {
              g_Nm1 <- d_ind_equalN * J_Nm1
              alternatives$g_ind_Nm1 <- list(
                value = g_Nm1,
                metadata = list(name = "Hedges' g (J with N-1)",
                                assumptions = "J = 1-3/(4(N-1)-1)"),
                why_consider = "Software convention: J(N-1) vs J(N-2)"
              )
            }
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
    # Paired n is ALWAYS df+1 (by definition). Only fall back to N when df is missing.
    n_paired <- NA_real_
    if (!is.na(df1) && df1 >= 0) {
      n_paired <- df1 + 1
    } else if (!is.na(N) && N > 0) {
      n_paired <- N
      assumptions <- c(assumptions, "Using N for paired computation (df unavailable)")
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
            grid_values = d_av_grid,
            metadata = VARIANT_METADATA$dav
          )
        }

        # Compute drm range across r grid (drm = dz * sqrt(2*(1-r)))
        drm_grid <- sapply(paired_r_grid, function(r) {
          tryCatch(drm_from_dz(dz, r), error = function(e) NA_real_)
        })
        drm_grid <- drm_grid[!is.na(drm_grid)]

        if (length(drm_grid) > 0) {
          computed_variants$drm <- list(
            value = stats::median(drm_grid),
            range = c(min(drm_grid), max(drm_grid)),
            grid_values = drm_grid,
            metadata = VARIANT_METADATA$drm
          )
        }

        # Compute CI for dz
        if (!is.na(dz)) {
          dz_ci <- tryCatch(ci_dz(dz, n_paired, ci_level_used), error = function(e) list(success = FALSE))
          if (dz_ci$success) computed_variants$dz$ci <- dz_ci$bounds
        }

        # v0.2.7: Compute Hedges-corrected paired variants (gz, gav, grm)
        # gz = dz * J(n-1), gav = dav * J(n-1), grm = drm * J(n-1)
        # These are declared in EFFECT_SIZE_FAMILIES but were never computed.
        # Fixes 52/112 (46%) Hedges' g ERRORs from MetaESCI audit.
        df_paired <- n_paired - 1
        if (df_paired > 0) {
          J_paired <- hedges_J(df_paired)
          if (!is.na(J_paired)) {
            # gz = dz * J
            gz <- dz * J_paired
            computed_variants$gz <- list(
              value = gz,
              metadata = list(
                name = "Hedges' gz (paired)",
                assumptions = "Paired/within-subjects, bias-corrected dz",
                formula = "gz = dz * J(n-1)",
                when_to_use = "Paired design with Hedges correction"
              )
            )

            # gav = dav * J for each r in grid
            if ("dav" %in% names(computed_variants) && !is.null(computed_variants$dav$value)) {
              dav_val <- computed_variants$dav$value
              gav <- dav_val * J_paired
              computed_variants$gav <- list(
                value = gav,
                metadata = list(
                  name = "Hedges' gav (paired, average SD)",
                  assumptions = "Paired/within-subjects, bias-corrected dav",
                  formula = "gav = dav * J(n-1)",
                  when_to_use = "Paired design, average SD pooling, with Hedges correction"
                )
              )
              # Also store range if dav has range
              if (!is.null(computed_variants$dav$range)) {
                computed_variants$gav$range <- computed_variants$dav$range * J_paired
              }
            }

            # grm = drm * J for each r in grid
            if ("drm" %in% names(computed_variants) && !is.null(computed_variants$drm$value)) {
              drm_val <- computed_variants$drm$value
              grm <- drm_val * J_paired
              computed_variants$grm <- list(
                value = grm,
                metadata = list(
                  name = "Hedges' grm (repeated measures)",
                  assumptions = "Paired/within-subjects, bias-corrected drm",
                  formula = "grm = drm * J(n-1)",
                  when_to_use = "Repeated-measures design with Hedges correction"
                )
              )
              if (!is.null(computed_variants$drm$range)) {
                computed_variants$grm$range <- computed_variants$drm$range * J_paired
              }
            }
          }
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
    # v0.2.7: also move dav, drm, gz, gav, grm (not just dz)
    if (!is.na(canonical_type) && canonical_type %in% c("d", "g")) {
      for (paired_var in c("dz", "dav", "drm", "gz", "gav", "grm")) {
        if (paired_var %in% names(computed_variants)) {
          alternatives[[paired_var]] <- computed_variants[[paired_var]]
          alternatives[[paired_var]]$why_consider <- "If this is actually a paired/within-subjects design"
        }
      }
    }
    # Fix 5B: Graceful failure -- when N is unknown and no variants computed,
    # set explicit message rather than showing garbage downstream
    if (tt == "t" && length(computed_variants) == 0 && is.na(N) && (is.na(n1) || is.na(n2))) {
      uncertainty <- c(uncertainty,
        "Cannot verify effect size: sample size could not be determined from reported information")
    }

  } else if (tt == "r") {
    # ------ CORRELATION COMPUTATIONS ------

    # Derive df from N for Pearson r (df = N - 2)
    # When multiple N values were found in context, try each to find best p-match
    if (is.na(df1) && !is.na(N) && N > 2) {
      if (!is.na(p_reported) && "N_candidates_str" %in% names(row) &&
          !is.na(row$N_candidates_str[1])) {
        N_alts <- as.numeric(unlist(strsplit(as.character(row$N_candidates_str[1]), ";")))
        N_alts <- N_alts[!is.na(N_alts) & N_alts > 2]
        if (length(N_alts) > 1) {
          best_N <- N
          best_p_diff <- Inf
          for (N_try in N_alts) {
            df_try <- N_try - 2
            if (abs(stat) < 1 && df_try > 0) {
              t_try <- stat * sqrt(df_try / (1 - stat^2))
              p_try <- 2 * stats::pt(abs(t_try), df = df_try, lower.tail = FALSE)
              p_diff <- abs(p_try - p_reported)
              if (p_diff < best_p_diff) {
                best_p_diff <- p_diff
                best_N <- N_try
              }
            }
          }
          if (best_N != N) {
            N <- best_N
            assumptions <- c(assumptions,
              sprintf("Multiple sample sizes found in context (N=%s); N=%d selected as best p-value match - please verify this is the correct sample for this correlation",
                      paste(N_alts, collapse = ", "), as.integer(N)))
          }
        }
      }
      df1 <- N - 2
      assumptions <- c(assumptions,
        sprintf("df=%d inferred from N=%d (Pearson r: df = N - 2)",
                as.integer(df1), as.integer(N)))
    }

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

      # Add Cohen's f\u00b2 as alternative (f\u00b2 = r\u00b2 / (1 - r\u00b2))
      r_sq <- r_value^2
      if (r_sq < 1) {
        f2_val <- r_sq / (1 - r_sq)
        if (!is.na(canonical_type) && canonical_type == "f2") {
          computed_variants$cohens_f2 <- list(
            value = f2_val,
            metadata = list(
              name = "Cohen's f\u00b2 (from r)",
              assumptions = "Derived from correlation",
              when_to_use = "Effect size for regression/correlation"
            )
          )
        } else {
          alternatives$cohens_f2 <- list(
            value = f2_val,
            metadata = list(
              name = "Cohen's f\u00b2 (from r)",
              assumptions = "Derived from correlation",
              when_to_use = "Effect size for regression/correlation"
            ),
            why_consider = "Alternative effect size measure for correlations"
          )
        }
      }
    }
  } else if (tt == "F") {
    # ------ ANOVA F-TEST COMPUTATIONS ------
    df1 <- as.numeric(df1[1])
    df2 <- as.numeric(df2[1])
    stat <- as.numeric(stat[1])

    # v0.2.9 Fix 4: Non-integer df2 detection for F-tests
    # Non-integer df2 indicates mixed-effects model or Satterthwaite/KR approximation
    mixed_model_F_detected <- FALSE
    if (!is.na(df2) && abs(df2 - round(df2)) > 0.01) {
      mixed_model_F_detected <- TRUE
      uncertainty <- c(uncertainty,
        sprintf("Non-integer df2 (%.2f) suggests mixed-effects model; standard effect size formula may not apply", df2))
    }

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
      anova_result <- tryCatch(
        {
          list(
            effects = compute_all_anova_effects(stat, df1, df2, design),
            error_msg = NULL
          )
        },
        error = function(e) {
          list(
            effects = list(
              eta = NA_real_, eta2 = NA_real_, partial_eta2 = NA_real_,
              generalized_eta2 = NA_real_, omega2 = NA_real_, cohens_f = NA_real_
            ),
            error_msg = paste0("Error computing ANOVA effects: ", conditionMessage(e))
          )
        }
      )
      anova_effects <- anova_result$effects

      if (!is.null(anova_result$error_msg)) {
        uncertainty <- c(uncertainty, anova_result$error_msg)
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

      # R2 as computed variant when reported type is R2 (R2 = eta2 for single-factor)
      if (!is.na(canonical_type) && canonical_type == "R2" && !is.na(anova_effects$eta2)) {
        computed_variants$R2 <- list(
          value = anova_effects$eta2,
          metadata = list(
            name = "R-squared (from F)",
            assumptions = "R\u00b2 = SS_effect / SS_total = eta\u00b2 for single-factor designs",
            when_to_use = "Proportion of variance explained"
          )
        )
      }

      # v0.2.7: Compute adjusted R2 as alternative for F-tests
      if (!is.na(anova_effects$eta2) && !is.na(df1) && !is.na(df2)) {
        N_est <- df1 + df2 + 1
        adj_R2 <- tryCatch(adjusted_R2_from_R2(anova_effects$eta2, N_est, df1),
                            error = function(e) NA_real_)
        if (!is.na(adj_R2) && adj_R2 >= 0 && adj_R2 <= 1) {
          if (!is.na(canonical_type) && canonical_type == "adjusted_R2") {
            computed_variants$adjusted_R2 <- list(
              value = adj_R2,
              metadata = list(
                name = "Adjusted R-squared",
                assumptions = "Adjusted for number of predictors, N estimated from df",
                formula = "R\u00b2_adj = 1 - (1-R\u00b2)(N-1)/(N-k-1)"
              )
            )
          } else {
            alternatives$adjusted_R2 <- list(
              value = adj_R2,
              metadata = list(
                name = "Adjusted R-squared (from F)",
                assumptions = "Adjusted for number of predictors",
                formula = "R\u00b2_adj = 1 - (1-R\u00b2)(N-1)/(N-k-1)"
              ),
              why_consider = "Authors may report adjusted R\u00b2 rather than unadjusted"
            )
          }
        }
      }

      # v0.2.7: Compute Cohen's f2 for F-tests (f2 = R2/(1-R2) = F*df1/df2)
      # v0.2.9 Fix 6: Also compute partial f2 for within-subjects designs
      if (!is.na(anova_effects$eta2) && anova_effects$eta2 < 1) {
        f2_val <- tryCatch(cohens_f2_from_R2(anova_effects$eta2),
                           error = function(e) NA_real_)
        # v0.2.9: Partial f2 = F*df1/df2 (within-subjects error term)
        partial_f2_val <- tryCatch(
          (stat * df1) / df2,
          error = function(e) NA_real_)
        if (!is.na(f2_val)) {
          if (!is.na(canonical_type) && canonical_type == "f2") {
            # For within-subjects, use whichever is closer to reported
            if (design %in% c("within", "mixed") && !is.na(partial_f2_val) &&
                !is.na(effect_reported)) {
              delta_standard <- abs(f2_val - abs(effect_reported))
              delta_partial <- abs(partial_f2_val - abs(effect_reported))
              if (delta_partial < delta_standard) {
                computed_variants$cohens_f2 <- list(
                  value = partial_f2_val,
                  metadata = list(
                    name = "Partial f\u00b2 (within-subjects)",
                    assumptions = "f\u00b2_partial = F*df1/df2 (within-subjects error)",
                    when_to_use = "Within-subjects/repeated-measures designs"))
                alternatives$cohens_f2_standard <- list(
                  value = f2_val,
                  metadata = list(name = "Cohen's f\u00b2 (standard)",
                                  assumptions = "f\u00b2 = R\u00b2/(1-R\u00b2)"),
                  why_consider = "Standard (between-subjects) formula")
              } else {
                computed_variants$cohens_f2 <- list(
                  value = f2_val,
                  metadata = list(name = "Cohen's f\u00b2 (from F)",
                                  assumptions = "f\u00b2 = R\u00b2/(1-R\u00b2)",
                                  when_to_use = "Effect size for regression"))
              }
            } else {
              computed_variants$cohens_f2 <- list(
                value = f2_val,
                metadata = list(name = "Cohen's f\u00b2 (from F)",
                                assumptions = "f\u00b2 = R\u00b2/(1-R\u00b2)",
                                when_to_use = "Effect size for regression"))
            }
          } else {
            alternatives$cohens_f2 <- list(
              value = f2_val,
              metadata = list(name = "Cohen's f\u00b2 (from F)",
                              assumptions = "f\u00b2 = R\u00b2/(1-R\u00b2)"),
              why_consider = "Alternative effect size for regression/ANOVA"
            )
          }
        }
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

      # v0.3.0: Cohen's f from omega2 (some software derives f this way)
      if (!is.na(anova_effects$omega2) && anova_effects$omega2 > 0) {
        f_omega <- cohens_f_from_omega2(anova_effects$omega2)
        if (!is.na(f_omega)) {
          if (!is.na(canonical_type) && canonical_type == "cohens_f") {
            # When author reports Cohen's f, include omega2-derived f as variant
            computed_variants$cohens_f_omega <- list(
              value = f_omega,
              metadata = list(name = "Cohen's f (from omega-squared)",
                              assumptions = "f = sqrt(omega2/(1-omega2))")
            )
          } else {
            alternatives$cohens_f_omega <- list(
              value = f_omega,
              metadata = list(name = "Cohen's f (from omega-squared)",
                              assumptions = "f = sqrt(omega2/(1-omega2))"),
              why_consider = "G*Power and JASP derive f from omega2, not eta2"
            )
          }
        }
      }

      # v0.3.0: ANOVA epsilon-squared (Kelley formula, JASP/jamovi default)
      eps2_anova <- epsilon2_anova_from_F(stat, df1, df2)
      if (!is.na(eps2_anova)) {
        if (!is.na(canonical_type) && canonical_type == "epsilon_squared") {
          computed_variants$epsilon2_anova <- list(
            value = eps2_anova,
            metadata = list(name = "Epsilon-squared (ANOVA, Kelley)",
                            assumptions = "eps2 = (F*df1 - df1) / (F*df1 + df2)")
          )
        } else {
          alternatives$epsilon2_anova <- list(
            value = eps2_anova,
            metadata = list(name = "Epsilon-squared (ANOVA, Kelley)",
                            assumptions = "eps2 = (F*df1 - df1) / (F*df1 + df2)"),
            why_consider = "Less biased than eta2; JASP/jamovi default"
          )
        }
      }

      # v0.3.0: Partial omega-squared (SPSS for factorial ANOVA)
      p_omega2 <- partial_omega2_from_F(stat, df1, df2)
      if (!is.na(p_omega2)) {
        alternatives$partial_omega2 <- list(
          value = p_omega2,
          metadata = list(name = "Partial omega-squared",
                          assumptions = "p_omega2 = df1*(F-1) / (df1*(F-1) + N)"),
          why_consider = "SPSS reports this for factorial ANOVA designs"
        )
      }

      # v0.3.0: Bias-corrected eta-squared (R effectsize package)
      if (!is.na(anova_effects$eta2)) {
        N_est <- df1 + df2 + 1
        bc_eta2 <- bias_corrected_eta2(anova_effects$eta2, N_est, df1)
        if (!is.na(bc_eta2)) {
          alternatives$bias_corrected_eta2 <- list(
            value = bc_eta2,
            metadata = list(name = "Bias-corrected eta-squared",
                            assumptions = "bc_eta2 = 1 - (1-eta2)*(N-1)/(N-df1-1)"),
            why_consider = "Small-sample correction used by R effectsize package"
          )
        }
      }

      # F(1, df2) is equivalent to t^2 with df=df2: compute d and r equivalents
      if (df1 == 1) {
        t_equiv <- sqrt(stat)
        n_equiv <- df2 + 2  # Total N assuming equal groups
        n1_eq <- floor(n_equiv / 2)
        n2_eq <- n_equiv - n1_eq

        r_equiv <- sqrt(stat / (stat + df2))
        d_equiv <- tryCatch(d_ind_from_t(t_equiv, n1_eq, n2_eq), error = function(e) 2 * sqrt(stat / df2))
        dz_equiv <- tryCatch(dz_from_t(t_equiv, df2 + 1), error = function(e) t_equiv / sqrt(df2 + 1))

        # Try CIs
        ci_r_val <- ci_r(r_equiv, n_equiv, ci_level_used)
        ci_d_val <- ci_d_ind_approx(d_equiv, n1_eq, n2_eq, ci_level_used)

        # When reported type is d/g: promote to computed_variants for same-type matching
        if (!is.na(canonical_type) && canonical_type %in% c("d", "g", "dz", "dav", "drm")) {
          computed_variants$d_ind_equalN <- list(
            value = d_equiv,
            metadata = VARIANT_METADATA$d_ind_equalN
          )
          if (!any(is.na(ci_d_val))) computed_variants$d_ind_equalN$ci <- ci_d_val

          # Hedges' g (bias-corrected d) -- Issue A fix
          J_factor <- 1 - 3 / (4 * df2 - 1)
          g_equiv <- d_equiv * J_factor
          computed_variants$g_ind <- list(
            value = g_equiv,
            metadata = VARIANT_METADATA$g_ind
          )

          # d_ind_min and d_ind_max when actual N is known
          if (!is.na(N) && N > 2) {
            d_min <- tryCatch(d_ind_from_t(t_equiv, 1, N - 1), error = function(e) NA_real_)
            d_max <- tryCatch(d_ind_from_t(t_equiv, N - 1, 1), error = function(e) NA_real_)
            if (!is.na(d_min)) {
              computed_variants$d_ind_min <- list(
                value = d_min,
                metadata = VARIANT_METADATA$d_ind_min
              )
            }
            if (!is.na(d_max)) {
              computed_variants$d_ind_max <- list(
                value = d_max,
                metadata = VARIANT_METADATA$d_ind_max
              )
            }
          }

          computed_variants$dz <- list(
            value = dz_equiv,
            metadata = VARIANT_METADATA$dz
          )
          dz_ci <- tryCatch(ci_dz(dz_equiv, df2 + 1, ci_level_used), error = function(e) list(success = FALSE))
          if (isTRUE(dz_ci$success)) computed_variants$dz$ci <- dz_ci$bounds

          # v0.2.9b: Compute dav/drm via r-grid + Hedges-corrected gz/gav/grm
          # Without these, Phase 8A-bis can't detect structural ambiguity for g
          d_av_grid <- sapply(paired_r_grid, function(rr) {
            tryCatch(dav_from_dz(dz_equiv, rr), error = function(e) NA_real_)
          })
          d_av_grid <- d_av_grid[!is.na(d_av_grid)]
          if (length(d_av_grid) > 0) {
            computed_variants$dav <- list(
              value = stats::median(d_av_grid),
              range = c(min(d_av_grid), max(d_av_grid)),
              grid_values = d_av_grid,
              metadata = VARIANT_METADATA$dav
            )
          }
          drm_grid <- sapply(paired_r_grid, function(rr) {
            tryCatch(drm_from_dz(dz_equiv, rr), error = function(e) NA_real_)
          })
          drm_grid <- drm_grid[!is.na(drm_grid)]
          if (length(drm_grid) > 0) {
            computed_variants$drm <- list(
              value = stats::median(drm_grid),
              range = c(min(drm_grid), max(drm_grid)),
              grid_values = drm_grid,
              metadata = VARIANT_METADATA$drm
            )
          }
          # Hedges-corrected paired variants for g-family matching
          N_for_J <- df2 + 1
          if (N_for_J > 1) {
            J_paired <- hedges_J(N_for_J - 1)
            if (!is.na(J_paired)) {
              computed_variants$gz <- list(
                value = dz_equiv * J_paired,
                metadata = list(name = "Hedges' gz (paired, from F)", assumptions = "gz = dz * J(N-1)")
              )
              if (length(d_av_grid) > 0) {
                gav_vals <- d_av_grid * J_paired
                computed_variants$gav <- list(
                  value = stats::median(gav_vals),
                  range = c(min(gav_vals), max(gav_vals)),
                  metadata = list(name = "Hedges' gav (paired, from F)", assumptions = "gav = dav * J(N-1)")
                )
              }
              if (length(drm_grid) > 0) {
                grm_vals <- drm_grid * J_paired
                computed_variants$grm <- list(
                  value = stats::median(grm_vals),
                  range = c(min(grm_vals), max(grm_vals)),
                  metadata = list(name = "Hedges' grm (paired, from F)", assumptions = "grm = drm * J(N-1)")
                )
              }
            }
          }

          # Keep r as alternative
          alternatives$r <- list(
            value = r_equiv,
            metadata = VARIANT_METADATA$r,
            why_consider = "Correlation equivalent (since df1=1)"
          )
          if (ci_r_val$success) alternatives$r$ci <- ci_r_val$bounds

        } else if (!is.na(canonical_type) && canonical_type == "r") {
          # Promote r to computed_variants
          computed_variants$r <- list(
            value = r_equiv,
            metadata = VARIANT_METADATA$r
          )
          if (ci_r_val$success) computed_variants$r$ci <- ci_r_val$bounds

          # Keep d as alternative
          alternatives$d <- list(
            value = d_equiv,
            metadata = VARIANT_METADATA$d_ind,
            why_consider = "Cohen's d equivalent (assuming equal groups)"
          )
          if (!any(is.na(ci_d_val))) alternatives$d$ci <- ci_d_val

        } else {
          # Default: keep both as alternatives (no cross-type promotion needed)
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
          if (ci_r_val$success) alternatives$r$ci <- ci_r_val$bounds
          if (!any(is.na(ci_d_val))) alternatives$d$ci <- ci_d_val
        }
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

    # Back-calculate N from reported effect size when N is missing
    if (is.na(N) && !is.na(effect_reported) && !is.na(stat) && stat > 0) {
      if (!is.na(canonical_type) && canonical_type == "phi" && abs(effect_reported) > 0) {
        N_back <- round(stat / (effect_reported^2))
        if (N_back >= 4 && N_back <= 1e6) {
          N <- N_back
          assumptions <- c(assumptions,
            sprintf("N=%d back-calculated from reported phi=%.2f and chi2=%.2f", as.integer(N), effect_reported, stat))
        }
      } else if (!is.na(canonical_type) && canonical_type == "V" && abs(effect_reported) > 0) {
        # V = sqrt(chi2 / (N * m)), so N = chi2 / (V^2 * m)
        # m = min(r-1, c-1); must match the m used downstream for V computation
        # (enumerate_m_from_df at line ~1404). Using df1 directly is WRONG for df>1.
        m_val <- if (!is.na(table_r) && !is.na(table_c)) {
          min(table_r - 1, table_c - 1)
        } else if (!is.na(df1)) {
          m_cands <- enumerate_m_from_df(df1)
          if (length(m_cands) > 0) m_cands[1] else 1
        } else {
          1
        }
        N_back <- round(stat / (effect_reported^2 * m_val))
        if (N_back >= 4 && N_back <= 1e6) {
          N <- N_back
          assumptions <- c(assumptions,
            sprintf("N=%d back-calculated from reported V=%.2f and chi2=%.2f (m=%d)",
                    as.integer(N), effect_reported, stat, as.integer(m_val)))
        }
      }
    }

    # v0.2.7: Cross-validate N against reported V
    # Back-calculate N_back = chi2 / (V^2 * m). If N_back is plausible and
    # substantially smaller than current N, override with back-calculated N.
    # v0.2.9d: Extended from global_text only to ALL N_source types when
    # the delta would be large (> 0.15). chi_inline N is still protected
    # by the N_back < 0.8*N guard (inline N is usually correct).
    if (!is.na(N) && !is.na(effect_reported) && abs(effect_reported) > 0 &&
        !is.na(stat) && stat > 0 &&
        !is.na(canonical_type) && canonical_type == "V") {
      n_source_val <- if ("N_source" %in% names(row) && length(row$N_source) > 0)
        as.character(row$N_source[1]) else NA_character_
      if (!is.na(n_source_val) && n_source_val != "chi_inline") {
        m_back <- if (!is.na(table_r) && !is.na(table_c)) {
          min(table_r - 1, table_c - 1)
        } else if (!is.na(df1)) {
          m_cands <- enumerate_m_from_df(df1)
          if (length(m_cands) > 0) m_cands[1] else 1
        } else { 1 }

        N_back <- round(stat / (effect_reported^2 * m_back))
        min_N_chisq <- if (!is.na(df1)) df1 + 1 else 4

        if (N_back >= min_N_chisq && N_back <= N && N_back < 0.8 * N) {
          N_original_chisq <- N
          N <- N_back
          assumptions <- c(assumptions,
            sprintf("N=%d back-calculated from V=%.3f (was %d from %s, m=%d)",
                    as.integer(N), effect_reported, as.integer(N_original_chisq),
                    n_source_val, as.integer(m_back)))
          uncertainty <- c(uncertainty,
            sprintf("N overridden by back-calculation from reported Cramer's V (was %s)", n_source_val))
        }
      }
    }

    # v0.2.9 Fix 8: Zero chi-square extraction guard
    # When chi2=0 but author reports V/phi > 0, the statistic was likely
    # misextracted. Computing V from chi2=0 gives 0 regardless of N,
    # guaranteeing a false ERROR. Skip effect size computation entirely.
    skip_chisq_computation <- FALSE
    if (!is.na(stat) && stat == 0 &&
        !is.na(effect_reported) && abs(effect_reported) > 0) {
      skip_chisq_computation <- TRUE
      uncertainty <- c(uncertainty,
        "chi-square = 0 but non-zero effect size reported; likely extraction artifact")
    }

    if (!is.na(N) && N > 0 && !skip_chisq_computation) {
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

      # v0.2.9 Fix 7: Phi vs V disambiguation for non-2x2 tables
      # When author reports phi but df >= 2, table is NOT 2x2.
      # Phi is only valid for 2x2 (df=1). Reinterpret as Cramer's V.
      if (!is.na(canonical_type) && canonical_type == "phi" &&
          !is.na(df1) && df1 >= 2) {
        canonical_type <- "V"
        uncertainty <- c(uncertainty,
          sprintf("df=%.0f implies non-2x2 table; phi reinterpreted as Cramer's V", df1))
        # Cap confidence since we overrode the author's type label
        phi_to_v_reinterpreted <- TRUE
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
        # v0.2.8: When multiple m candidates exist and author reported V,
        # try ALL candidates and pick the one producing V closest to reported.
        # This handles cases where df allows multiple table dimensions
        # (e.g., df=4 could be 5x2 with m=1 or 3x3 with m=2).
        V_val <- NA_real_
        if (length(m_candidates) > 1 && !is.na(canonical_type) &&
            canonical_type == "V" && !is.na(effect_reported)) {
          v_vals <- sapply(m_candidates, function(m) {
            tryCatch(V_from_chisq(stat, N, m), error = function(e) NA_real_)
          })
          valid <- !is.na(v_vals)
          if (any(valid)) {
            diffs <- abs(v_vals[valid] - abs(effect_reported))
            best_idx <- which(valid)[which.min(diffs)]
            V_val <- v_vals[best_idx]
            if (best_idx != 1) {
              assumptions <- c(assumptions,
                sprintf("Cramer's V: best match with m=%d (tried m=%s)",
                        m_candidates[best_idx],
                        paste(m_candidates, collapse = ",")))
            }
          }
        } else {
          V_val <- tryCatch(V_from_chisq(stat, N, m_candidates[1]), error = function(e) NA_real_)
        }

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

    # v0.2.7: Compute d from z when N is available
    # v0.2.8: Compute BOTH independent (d = 2z/sqrt(N)) and paired (dz = z/sqrt(N))
    # variants. z-tests can come from Mann-Whitney (independent) or Wilcoxon
    # signed-rank (paired) -- we cannot tell from the statistic alone.
    # v0.2.9 Fix 9: When reported |d| > 3 for z-test, the d likely belongs to
    # a different analysis (e.g., Bayesian posterior, different test). Skip
    # effect size computation to avoid false ERROR.
    skip_z_effect <- FALSE
    if (!is.na(canonical_type) && canonical_type %in% c("d", "g") &&
        !is.na(effect_reported) && abs(effect_reported) > 3.0) {
      skip_z_effect <- TRUE
      uncertainty <- c(uncertainty,
        sprintf("|d| = %.2f unusually large for z-test; may be from a different analysis",
                abs(effect_reported)))
    }

    if (!is.na(stat) && !is.na(N) && N > 0 && !skip_z_effect) {
      d_z <- tryCatch(d_from_z(stat, N), error = function(e) NA_real_)
      dz_z <- tryCatch(dz_from_z(stat, N), error = function(e) NA_real_)
      if (!is.na(d_z)) {
        if (!is.na(canonical_type) && canonical_type %in% c("d", "g")) {
          # Independent assumption: d = 2z/sqrt(N)
          computed_variants$d_ind_equalN <- list(
            value = d_z,
            metadata = VARIANT_METADATA$d_ind_equalN
          )

          # Paired assumption: dz = z/sqrt(N), treating N as number of pairs
          if (!is.na(dz_z)) {
            computed_variants$dz <- list(
              value = dz_z,
              metadata = VARIANT_METADATA$dz
            )

            # dav and drm via r-grid (same pattern as t-test block)
            d_av_grid <- sapply(paired_r_grid, function(r) {
              tryCatch(dav_from_dz(dz_z, r), error = function(e) NA_real_)
            })
            d_av_grid <- d_av_grid[!is.na(d_av_grid)]
            if (length(d_av_grid) > 0) {
              computed_variants$dav <- list(
                value = stats::median(d_av_grid),
                range = c(min(d_av_grid), max(d_av_grid)),
                grid_values = d_av_grid,
                metadata = VARIANT_METADATA$dav
              )
            }

            drm_grid <- sapply(paired_r_grid, function(r) {
              tryCatch(drm_from_dz(dz_z, r), error = function(e) NA_real_)
            })
            drm_grid <- drm_grid[!is.na(drm_grid)]
            if (length(drm_grid) > 0) {
              computed_variants$drm <- list(
                value = stats::median(drm_grid),
                range = c(min(drm_grid), max(drm_grid)),
                grid_values = drm_grid,
                metadata = VARIANT_METADATA$drm
              )
            }

            # Hedges-corrected paired variants: gz, gav, grm
            if (N > 1) {
              J_z <- hedges_J(N - 1)
              if (!is.na(J_z)) {
                computed_variants$gz <- list(
                  value = dz_z * J_z,
                  metadata = list(
                    name = "Hedges' gz (paired, from z)",
                    assumptions = "Paired/within-subjects, bias-corrected dz from z-test",
                    formula = "gz = dz_from_z * J(N-1)",
                    when_to_use = "Paired design with Hedges correction"
                  )
                )
                if (length(d_av_grid) > 0) {
                  computed_variants$gav <- list(
                    value = stats::median(d_av_grid) * J_z,
                    metadata = list(
                      name = "Hedges' gav (paired, from z)",
                      assumptions = "Paired/within-subjects, bias-corrected dav from z-test",
                      formula = "gav = dav_from_z * J(N-1)",
                      when_to_use = "Paired design with Hedges correction"
                    )
                  )
                }
                if (length(drm_grid) > 0) {
                  computed_variants$grm <- list(
                    value = stats::median(drm_grid) * J_z,
                    metadata = list(
                      name = "Hedges' grm (paired, from z)",
                      assumptions = "Paired/within-subjects, bias-corrected drm from z-test",
                      formula = "grm = drm_from_z * J(N-1)",
                      when_to_use = "Paired design with Hedges correction"
                    )
                  )
                }
              }
            }
          }

          # Also compute Hedges' g (independent)
          if (N > 2) {
            J <- hedges_J(N - 2)
            if (!is.na(J)) {
              g_z <- d_z * J
              if (!is.na(canonical_type) && canonical_type == "g") {
                computed_variants$g_ind <- list(
                  value = g_z,
                  metadata = VARIANT_METADATA$g_ind
                )
              } else {
                alternatives$g_ind <- list(
                  value = g_z,
                  metadata = VARIANT_METADATA$g_ind,
                  why_consider = "Bias-corrected d from z-test (independent assumption)"
                )
              }
            }
          }
          assumptions <- c(assumptions, "z-test design unknown: computed both independent (d = 2z/sqrt(N)) and paired (dz = z/sqrt(N)) variants")
        } else {
          alternatives$d <- list(
            value = d_z,
            metadata = VARIANT_METADATA$d_ind,
            why_consider = "Cohen's d equivalent from z-statistic"
          )
        }
      }
    }
  } else if (tt == "regression") {
    # ------ REGRESSION COEFFICIENT COMPUTATIONS ------
    # Extract regression-specific fields
    b_val <- if ("b_coeff" %in% names(row) && length(row$b_coeff) > 0) as.numeric(row$b_coeff[1]) else NA_real_
    SE_val <- if ("SE_coeff" %in% names(row) && length(row$SE_coeff) > 0) as.numeric(row$SE_coeff[1]) else NA_real_
    adj_R2_reported <- if ("adj_R2" %in% names(row) && length(row$adj_R2) > 0) as.numeric(row$adj_R2[1]) else NA_real_

    # Verify t = b/SE consistency
    if (!is.na(b_val) && !is.na(SE_val) && !is.na(stat)) {
      t_check <- verify_t_from_b_SE(b_val, SE_val, stat)
      if (!is.na(t_check$consistent)) {
        if (t_check$consistent) {
          assumptions <- c(assumptions, sprintf("t = b/SE verified: b=%.4f, SE=%.4f, computed t=%.4f", b_val, SE_val, t_check$computed_t))
        } else {
          uncertainty <- c(uncertainty, sprintf(
            "t = b/SE inconsistency: b=%.4f, SE=%.4f gives t=%.4f, but reported t=%.4f (delta=%.4f)",
            b_val, SE_val, t_check$computed_t, stat, t_check$delta
          ))
        }
      }
    }

    # Compute standardized beta and partial r using t and df
    if (!is.na(stat) && !is.na(df1) && df1 > 0) {
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
          why_consider = "Partial correlation from regression t-value"
        )
      }

      # Compute d family as alternatives (regression t is like t-test)
      if (!is.na(N) && N > 0) {
        n_half <- floor(N / 2)
        if (n_half > 0) {
          d_val <- tryCatch(d_ind_from_t(stat, n_half, N - n_half), error = function(e) NA_real_)
          if (!is.na(d_val)) {
            alternatives$d <- list(
              value = d_val,
              metadata = VARIANT_METADATA$d_ind,
              why_consider = "Cohen's d equivalent (assuming equal groups)"
            )
          }
        }
      }
    }
  } else if (tt == "U") {
    # ------ MANN-WHITNEY U COMPUTATIONS ------
    if (!is.na(stat) && !is.na(n1) && !is.na(n2) && n1 > 0 && n2 > 0) {
      rb_val <- tryCatch(rank_biserial_r_from_U(stat, n1, n2), error = function(e) NA_real_)
      if (!is.na(rb_val)) {
        computed_variants$rank_biserial_r <- list(
          value = rb_val,
          metadata = VARIANT_METADATA$rank_biserial_r
        )
      }

      cd_val <- tryCatch(cliffs_delta_from_U(stat, n1, n2), error = function(e) NA_real_)
      if (!is.na(cd_val)) {
        alternatives$cliffs_delta <- list(
          value = cd_val,
          metadata = VARIANT_METADATA$cliffs_delta,
          why_consider = "Alternative nonparametric effect size (= -rank_biserial_r)"
        )
      }
    } else if (!is.na(stat) && !is.na(N) && N > 0) {
      # No group sizes - try z_auxiliary if available
      z_aux <- if ("z_auxiliary" %in% names(row) && length(row$z_auxiliary) > 0) {
        as.numeric(row$z_auxiliary[1])
      } else {
        NA_real_
      }
      if (!is.na(z_aux)) {
        rb_z <- tryCatch(rank_biserial_r_from_z(z_aux, N), error = function(e) NA_real_)
        if (!is.na(rb_z)) {
          computed_variants$rank_biserial_r <- list(
            value = rb_z,
            metadata = VARIANT_METADATA$rank_biserial_r
          )
          assumptions <- c(assumptions, "Rank-biserial r approximated from z-score (r = z/sqrt(N))")
        }
      }
      uncertainty <- c(uncertainty, "Group sizes (n1, n2) not found - U-based r requires group sizes for exact computation")
    }

    # v0.2.7: When no n1/n2 and no z_aux but N is known, approximate with equal split
    if (length(computed_variants) == 0 && !is.na(stat) && !is.na(N) && N > 2 &&
        (is.na(n1) || n1 == 0) && (is.na(n2) || n2 == 0)) {
      n1_eq <- floor(N / 2)
      n2_eq <- N - n1_eq
      rb_approx <- tryCatch(rank_biserial_r_from_U(stat, n1_eq, n2_eq), error = function(e) NA_real_)
      if (!is.na(rb_approx) && abs(rb_approx) <= 1) {
        computed_variants$rank_biserial_r <- list(
          value = rb_approx,
          metadata = VARIANT_METADATA$rank_biserial_r
        )
        assumptions <- c(assumptions, sprintf("Rank-biserial r with assumed equal groups (n1=%d, n2=%d)", n1_eq, n2_eq))
        uncertainty <- c(uncertainty, "U-test group sizes unknown; equal-split assumption may affect accuracy")
      }
    }
  } else if (tt == "W") {
    # ------ WILCOXON W COMPUTATIONS ------
    # W is less directly convertible to effect size without group info
    z_aux <- if ("z_auxiliary" %in% names(row) && length(row$z_auxiliary) > 0) {
      as.numeric(row$z_auxiliary[1])
    } else {
      NA_real_
    }
    if (!is.na(z_aux) && !is.na(N) && N > 0) {
      rb_z <- tryCatch(rank_biserial_r_from_z(z_aux, N), error = function(e) NA_real_)
      if (!is.na(rb_z)) {
        computed_variants$rank_biserial_r <- list(
          value = rb_z,
          metadata = VARIANT_METADATA$rank_biserial_r
        )
        assumptions <- c(assumptions, "Rank-biserial r approximated from z-score")
      }
    } else {
      uncertainty <- c(uncertainty, "Wilcoxon W requires z-score and N for effect size computation")
    }
  } else if (tt == "H") {
    # ------ KRUSKAL-WALLIS H COMPUTATIONS ------
    if (!is.na(stat) && !is.na(df1) && df1 > 0 && !is.na(N) && N > 0) {
      k <- df1 + 1 # Number of groups = df + 1

      eps2 <- tryCatch(epsilon_squared_from_H(stat, N, k), error = function(e) NA_real_)
      if (!is.na(eps2)) {
        computed_variants$epsilon_squared <- list(
          value = eps2,
          metadata = VARIANT_METADATA$epsilon_squared
        )
      }

      # Kendall's W as alternative (W = H / (N*(k-1)) is same as epsilon_squared_from_chisq in some formulations)
      W_val <- tryCatch(kendalls_W_from_chisq(stat, N, k), error = function(e) NA_real_)
      if (!is.na(W_val)) {
        alternatives$kendalls_W <- list(
          value = W_val,
          metadata = VARIANT_METADATA$kendalls_W,
          why_consider = "Concordance coefficient interpretation"
        )
      }
    } else {
      uncertainty <- c(uncertainty, "Kruskal-Wallis H requires df and N for effect size computation")
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

      # v0.2.4: Also include same-type variants from alternatives
      # (e.g., g_ind is stored as alternative for t-tests but IS same-type for reported "d")
      # v0.3.0: Also check family_info$alternatives for cross-formula matches
      # Only for ANOVA/chi-square families where alternatives are different formulas
      # for the same quantity (e.g., omega2 for eta2, V for phi).
      # NOT for d/g families where alternatives are different-design variants.
      cross_formula_families <- c("eta2", "omega2", "f", "phi", "V",
                                  "rank_biserial_r", "cliffs_delta",
                                  "epsilon_squared", "kendalls_W",
                                  "R2", "f2")
      family_alts <- character(0)
      if (!is.null(family_info$family) && family_info$family %in% cross_formula_families) {
        family_alts <- if (!is.null(family_info$alternatives)) family_info$alternatives else character(0)
      }
      for (vname in names(alternatives)) {
        base_name <- gsub("_equalN|_min|_max", "", vname)
        if ((base_name %in% valid_variants || vname %in% valid_variants ||
             base_name %in% family_alts || vname %in% family_alts) &&
            !(vname %in% names(same_type_variants))) {
          same_type_variants[[vname]] <- alternatives[[vname]]
        }
      }
    } else {
      # Unknown type - use all computed variants
      same_type_variants <- computed_variants
      ambiguity_level <- "ambiguous"
      ambiguity_reason <- paste0("Unknown effect size type '", effect_reported_name, "' - compared to all computed variants")
    }

    # Find closest match among same-type variants
    # v0.3.0a: Two-pass range-aware matching for variants with $range
    # Pass 1: In-range variants ALWAYS beat out-of-range variants
    # Pass 2: Among in-range, prefer the one where value is most central
    # Output delta uses median distance (for status determination)
    if (length(same_type_variants) > 0) {
      abs_reported <- abs(effect_reported)

      # Compute deltas for each variant
      # For variants WITH $range: use min(dist_to_median, dist_to_nearest_bound)
      # For variants WITHOUT $range: use distance to value (standard)
      # Track which variants contain the reported value in their range
      range_contains <- logical(length(same_type_variants))
      names(range_contains) <- names(same_type_variants)

      selection_diffs <- sapply(seq_along(same_type_variants), function(i) {
        v <- same_type_variants[[i]]
        vname <- names(same_type_variants)[i]
        if (is.null(v$value) || is.na(v$value)) return(Inf)
        dist_to_median <- abs(abs(v$value) - abs_reported)
        if (!is.null(v$range) && length(v$range) == 2 &&
            !any(is.na(v$range))) {
          rng_min <- min(abs(v$range))
          rng_max <- max(abs(v$range))
          if (abs_reported >= rng_min && abs_reported <= rng_max) {
            range_contains[vname] <<- TRUE
            # IN RANGE: use distance to nearest bound (always <= median dist)
            return(min(dist_to_median,
                       abs(abs_reported - rng_min),
                       abs(abs_reported - rng_max)))
          }
          # OUT OF RANGE: use median distance (same as non-range variants)
          # This preserves pre-v0.3.0 behavior for out-of-range cases
          return(dist_to_median)
        }
        dist_to_median
      })
      names(selection_diffs) <- names(same_type_variants)

      # v0.3.0a: Among range-variants only, prefer in-range over out-of-range.
      # Non-range variants (d_ind, g_ind, dz, etc.) compete normally.
      if (any(range_contains)) {
        for (vname in names(same_type_variants)) {
          v <- same_type_variants[[vname]]
          has_range <- !is.null(v$range) && length(v$range) == 2 && !any(is.na(v$range))
          if (has_range && !range_contains[vname]) {
            # This range-variant is out of range while another is in-range:
            # penalize so it loses to the in-range variant
            selection_diffs[vname] <- selection_diffs[vname] + 100
          }
        }
      }

      # Output deltas for status determination
      # v0.3.0b: When value is within a variant's r-grid range, find the
      # nearest grid point and use that distance. This is much more precise
      # than using min/max/median anchors.
      output_diffs <- sapply(same_type_variants, function(v) {
        if (is.null(v$value) || is.na(v$value)) return(Inf)
        dist_to_median <- abs(abs(v$value) - abs_reported)
        if (!is.null(v$range) && length(v$range) == 2 &&
            !any(is.na(v$range))) {
          rng_min <- min(abs(v$range))
          rng_max <- max(abs(v$range))
          if (abs_reported >= rng_min && abs_reported <= rng_max) {
            # In range: use distance to nearest grid point if available
            if (!is.null(v$grid_values) && length(v$grid_values) > 0) {
              return(min(abs(abs(v$grid_values) - abs_reported)))
            }
            # Fallback to nearest of {min, median, max}
            return(min(dist_to_median,
                       abs(abs_reported - rng_min),
                       abs(abs_reported - rng_max)))
          }
        }
        dist_to_median
      })

      if (any(is.finite(selection_diffs))) {
        k <- which.min(selection_diffs)
        matched_variant <- names(same_type_variants)[k]
        mv <- same_type_variants[[k]]
        # Use output delta (median distance) for status, not selection delta
        delta_effect_abs <- output_diffs[k]
        matched_value <- mv$value

        # v0.3.0: Add assumption note when matched via range
        if (!is.null(mv$range) && length(mv$range) == 2 &&
            !any(is.na(mv$range))) {
          rng_min <- min(abs(mv$range))
          rng_max <- max(abs(mv$range))
          if (abs_reported >= rng_min && abs_reported <= rng_max) {
            assumptions <- c(assumptions,
              paste0("Matched ", matched_variant,
                     " within r-grid range [", round(rng_min, 3), ", ",
                     round(rng_max, 3),
                     "] (correlation-dependent)"))
          }
        }

        # Check if multiple variants are close (ambiguous)
        close_variants <- names(selection_diffs)[selection_diffs <= max(selection_diffs[k] * 1.5, 0.001) & is.finite(selection_diffs)]
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
  #
  # Five-level system:
  #   PASS  - Effect size and p-value verified and consistent
  #   OK    - No effect size to compare, but p-value consistent (technical check passed)
  #   NOTE  - Verified but with minor caveats (ambiguous design, rounding notes)
  #   WARN  - Moderate discrepancy or uncertain match that deserves human review
  #   ERROR - Large discrepancy or clear inconsistency
  # ============================================================================

  status <- "WARN"
  has_effect_reported <- !is.na(effect_reported)

  # Fix 5C: Graceful failure -- if no variant could be computed and N is unknown,
  # don't flag as ERROR with misleading delta; report as NOTE
  if (is.na(matched_value) && is.na(N) && has_effect_reported) {
    status <- "NOTE"
    uncertainty <- c(uncertainty,
      "Effect size verification not possible without sample size information")
  }

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
    # APA 7 Rounding-aware check: if both round to 2 decimals effect sizes, it's a pass
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
      } else if (ambiguity_level == "ambiguous") {
        # Good numerical match overrides design ambiguity (Issue 3 fix)
        status <- "PASS"
        uncertainty <- c(uncertainty, "Design ambiguous but closest variant matches well")
      } else {
        # highly_ambiguous (cross-type fallback, unknown type) -- use cross_type_action param
        status <- cross_type_action
        uncertainty <- c(uncertainty, "Match is good but comparison is highly ambiguous (cross-type or unknown effect type)")
      }
    } else if (delta_effect_abs <= (3 * tol_eff)) {
      status <- "WARN"
    } else if (delta_effect_abs > (5 * tol_eff)) {
      # Cross-type fallback should not produce ERROR -- use cross_type_action (Issue 1D fix)
      if (ambiguity_level == "highly_ambiguous" && grepl("No same-type", ambiguity_reason)) {
        status <- cross_type_action
        uncertainty <- c(uncertainty, "Cross-type comparison (no same-type variants available) \u2014 delta not meaningful")
      } else {
        status <- "ERROR"
      }
    } else {
      status <- "WARN"
    }
  }

  # CI affects status (controlled by ci_affects_status parameter)
  if (!is.na(ci_match) && !ci_match) {
    if (ci_affects_status && status == "PASS") status <- "NOTE"
    uncertainty <- c(uncertainty, sprintf(
      "CI bounds mismatch: lower diff=%.3f, upper diff=%.3f",
      ci_delta_lower, ci_delta_upper
    ))
  }

  # Determine check_type (Issue 4): what drove the status classification
  check_type <- if (has_effect_reported && !is.na(delta_effect_abs)) {
    "effect_size"
  } else if (!is.na(p_reported)) {
    "p_value"
  } else {
    "extraction_only"
  }
  # v0.3.0c: Correlation guard — r-tests without df or N cannot be verified
  # For r-tests, stat IS the r-value, so computed_variants$r trivially matches
  # the reported effect. Without df (for p-value) or N (for CI), this is a
  # self-referential match with no independent verification. Route to extraction_only.
  # Also keep a generic guard for any other test type where stat is truly NA.
  if (tt == "r" && is.na(df1) && is.na(df2) && is.na(N) && check_type == "effect_size") {
    check_type <- "extraction_only"
    matched_value <- NA_real_
    matched_variant <- NA_character_
    delta_effect_abs <- NA_real_
    if (status %in% c("ERROR", "WARN", "PASS")) status <- "NOTE"
    uncertainty <- c(uncertainty,
      "Correlation without df or N \u2014 cannot independently verify effect size")
  } else if (is.na(stat) && is.na(df1) && is.na(df2) && check_type == "effect_size") {
    check_type <- "extraction_only"
    matched_value <- NA_real_
    matched_variant <- NA_character_
    delta_effect_abs <- NA_real_
    if (status %in% c("ERROR", "WARN", "PASS")) status <- "NOTE"
    uncertainty <- c(uncertainty,
      "Effect size reported but no test statistic or df to verify against \u2014 treating as extraction only")
  }

  # CI mismatch overrides check_type when it downgrades status
  if (ci_affects_status && !is.na(ci_match) && !ci_match && status == "NOTE" && has_effect_reported) {
    check_type <- "ci"
  }

  # v0.2.7: Clean matched_value/variant when only p-value or extraction-only
  # Prevents literal "NA" strings in JSON serialization from inflating ES-checkable counts
  if (check_type %in% c("p_value", "extraction_only")) {
    matched_value <- NA_real_
    matched_variant <- NA_character_
    delta_effect_abs <- NA_real_
  }

  # Extreme delta flag (Issue 5): flag likely extraction errors
  extraction_suspect <- FALSE
  if (!is.na(delta_effect_abs) && delta_effect_abs > EXTREME_DELTA_THRESHOLD) {
    extraction_suspect <- TRUE
    uncertainty <- c(uncertainty,
      sprintf("Extreme discrepancy (delta=%.2f) likely reflects data extraction error rather than reporting error",
              delta_effect_abs))
  }

  # Plausibility filter (Issue C): flag implausibly large effect sizes
  if (plausibility_filter && !is.na(effect_reported) && !is.na(canonical_type)) {
    bound <- EFFECT_PLAUSIBILITY[[canonical_type]]
    if (!is.null(bound) && abs(effect_reported) > bound) {
      extraction_suspect <- TRUE
      uncertainty <- c(uncertainty,
        sprintf("Reported effect size |%s| = %.2f exceeds plausibility bound (%.1f) \u2014 likely extraction error",
                canonical_type, abs(effect_reported), bound))
      # Downgrade ERROR to NOTE for implausible extractions
      if (status == "ERROR") {
        status <- "NOTE"
      }
    }
  }

  # Garbled p-value detection: "p < X" where X > 0.5 is nonsensical
  # (you wouldn't write "p < 0.645" -- this is a PDF extraction artifact)
  if (p_is_inequality && !is.na(p_reported) && p_reported > 0.5) {
    extraction_suspect <- TRUE
    uncertainty <- c(uncertainty,
      sprintf("Suspicious p-value: 'p < %.3f' is likely a PDF extraction artifact (no standard threshold > 0.5 uses '<')",
              p_reported))
  }

  # v0.2.4: non-inequality p > 0.5 with large computed discrepancy
  # (p_computed checked later in Phase 9B after it's been set)

  # Computed-side plausibility (v0.2.4): if the COMPUTED effect size is implausible,
  # the test statistic or df was likely garbled by PDF extraction
  if (plausibility_filter && !is.na(matched_value) && !is.na(canonical_type)) {
    computed_bound <- EFFECT_PLAUSIBILITY[[canonical_type]]
    if (!is.null(computed_bound) && abs(matched_value) > computed_bound) {
      extraction_suspect <- TRUE
      uncertainty <- c(uncertainty,
        sprintf("Computed effect size |%s| = %.2f exceeds plausibility bound (%.1f) \u2014 likely garbled test statistic or df",
                matched_variant, abs(matched_value), computed_bound))
      if (status == "ERROR") status <- "NOTE"
    }
  }

  # Stat value plausibility (v0.2.4): flag extremely large test statistics
  if (!is.na(stat)) {
    if (tt == "t" && abs(stat) > 100) {
      extraction_suspect <- TRUE
      uncertainty <- c(uncertainty,
        sprintf("Test statistic |t| = %.1f is unusually large \u2014 possible extraction artifact", abs(stat)))
    }
    if (tt == "F" && stat > 10000) {
      extraction_suspect <- TRUE
      uncertainty <- c(uncertainty,
        sprintf("Test statistic F = %.1f is unusually large \u2014 possible extraction artifact", stat))
    }
  }

  # DF plausibility (v0.2.4): flag zero, negative, or impossibly large df
  if (!is.na(df1) && (df1 <= 0 || df1 > 50000)) {
    extraction_suspect <- TRUE
    uncertainty <- c(uncertainty,
      sprintf("Degrees of freedom df1 = %.0f appears implausible \u2014 possible extraction artifact", df1))
  }

  # ============================================================================
  # PHASE 5B: Computation-guided decimal recovery (v0.2.5)
  # When extraction_suspect is TRUE and a large delta or plausibility violation
  # was detected, try all possible decimal placements of the reported value
  # and check if any matches the computed value. This uses the computation
  # pipeline as an oracle to unambiguously recover dropped decimals.
  # ============================================================================
  decimal_recovered <- FALSE

  # 5B-1: Effect size decimal recovery
  # Only attempt when we have a computed value to compare against AND the
  # reported effect size triggered a plausibility or extreme delta flag
  if (extraction_suspect && !is.na(effect_reported) && !is.na(matched_value) &&
      !is.na(delta_effect_abs) && delta_effect_abs > EXTREME_DELTA_THRESHOLD) {
    original_effect <- effect_reported
    # Generate decimal candidates by dividing by powers of 10
    # e.g., 615 -> c(61.5, 6.15, 0.615, 0.0615)
    candidates <- abs(original_effect) / (10^(1:4))
    # Preserve sign
    if (original_effect < 0) candidates <- -candidates

    best_candidate <- NA_real_
    best_delta <- Inf
    best_variant <- NA_character_

    for (cand in candidates) {
      # Compare candidate against all same-type computed variants
      for (vname in names(same_type_variants)) {
        vval <- same_type_variants[[vname]]$value
        if (!is.null(vval) && !is.na(vval)) {
          d <- abs(abs(cand) - abs(vval))
          if (d < best_delta) {
            best_delta <- d
            best_candidate <- cand
            best_variant <- vname
          }
        }
      }
      # Also try computed_variants (cross-type fallback)
      for (vname in names(computed_variants)) {
        vval <- computed_variants[[vname]]$value
        if (!is.null(vval) && !is.na(vval)) {
          d <- abs(abs(cand) - abs(vval))
          if (d < best_delta) {
            best_delta <- d
            best_candidate <- cand
            best_variant <- vname
          }
        }
      }
    }

    # Accept recovery if the best candidate matches within tolerance
    if (!is.null(tol_eff) && best_delta <= tol_eff * 3 && !is.na(best_candidate)) {
      effect_reported <- best_candidate
      matched_value <- if (best_variant %in% names(same_type_variants)) {
        same_type_variants[[best_variant]]$value
      } else if (best_variant %in% names(computed_variants)) {
        computed_variants[[best_variant]]$value
      } else matched_value
      matched_variant <- best_variant
      delta_effect_abs <- best_delta
      decimal_recovered <- TRUE

      # Re-evaluate status with corrected value
      if (delta_effect_abs <= tol_eff) {
        status <- "PASS"
      } else if (delta_effect_abs <= 3 * tol_eff) {
        status <- "WARN"
      }
      # Keep extraction_suspect TRUE -- the correction is still flagged
      uncertainty <- c(uncertainty,
        sprintf("Decimal recovery: reported %s = %.2f likely %s = %.4f (matches computed %s = %.4f, delta = %.4f)",
                effect_reported_name, original_effect,
                effect_reported_name, effect_reported,
                best_variant, matched_value, delta_effect_abs))
    }
  }

  # 5B-2: P-value decimal recovery
  # When p_decimal_corrected flag was set by normalize_text(), add assumption note
  if ("p_decimal_corrected" %in% names(row) && isTRUE(row$p_decimal_corrected[1])) {
    uncertainty <- c(uncertainty,
      "P-value decimal corrected: original text had dropped decimal (e.g., 'p = 484' corrected to 'p = .484')")
    extraction_suspect <- TRUE
  }

  # Type-incompatible effect sizes (v0.2.4): when the reported effect size type
  # is incompatible with the test statistic type, cap at NOTE
  if (effect_test_mismatch && status == "ERROR") {
    status <- "NOTE"
    extraction_suspect <- TRUE
    uncertainty <- c(uncertainty,
      "Status capped at NOTE: reported effect size type is incompatible with test statistic type")
  }

  # Method context status cap (v0.2.4): when method keyword is IN the chunk
  # (not just nearby), the stat is likely from a power analysis, meta-analysis, etc.
  # Cap status at method_context_action to avoid false ERRORs.
  method_in_chunk <- if ("method_context_in_chunk" %in% names(row) &&
                          length(row$method_context_in_chunk) > 0)
    isTRUE(row$method_context_in_chunk[1]) else FALSE

  if (method_in_chunk && status == "ERROR") {
    status <- method_context_action
    uncertainty <- c(uncertainty,
      "Status capped: statistic appears within methodological context (power analysis, meta-analysis, etc.)")
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
  # PHASE 8A-bis: Structural design ambiguity detection (v0.2.8)
  # When F(1,df), t-test, or z-test computes variants from BOTH the independent
  # family (d_ind, g_ind) AND the paired family (dz, dav, drm), the design is
  # structurally ambiguous regardless of which variant matches best. Override
  # ambiguity_level to "ambiguous" so Phase 8B can fire.
  # Also: z-tests with d/g canonical_type are inherently design-ambiguous
  # (Mann-Whitney vs Wilcoxon) even if only one family was computed.
  # ============================================================================

  structural_design_ambiguous <- FALSE
  # v0.3.0: Also detect structural ambiguity for WARN (not just ERROR)
  # Range-aware matching can reduce deltas, making ERROR->WARN, but the
  # design ambiguity is still real and should be flagged
  if (status %in% c("ERROR", "WARN") && check_type == "effect_size" &&
      !is.na(canonical_type) && canonical_type %in% c("d", "g", "dz", "dav", "drm")) {
    ind_variant_names <- c("d_ind", "d_ind_equalN", "d_ind_min", "d_ind_max", "g_ind")
    paired_variant_names <- c("dz", "dav", "drm", "gz", "gav", "grm")
    has_ind <- any(names(same_type_variants) %in% ind_variant_names)
    has_paired <- any(names(same_type_variants) %in% paired_variant_names)

    is_F1_or_t <- (tt == "t") || (tt == "F" && !is.na(df1) && df1 == 1)
    is_z_with_d <- (tt == "z" && canonical_type %in% c("d", "g"))

    if ((is_F1_or_t || is_z_with_d) && has_ind && has_paired) {
      structural_design_ambiguous <- TRUE
      if (ambiguity_level == "clear") {
        ambiguity_level <- "ambiguous"
        ambiguity_reason <- sprintf(
          "Structural design ambiguity: both independent (%s) and paired (%s) variants computed for %s-test, matched=%s",
          paste(intersect(names(same_type_variants), ind_variant_names), collapse = ", "),
          paste(intersect(names(same_type_variants), paired_variant_names), collapse = ", "),
          tt, matched_variant
        )
      }
    }

    # z-tests are inherently design-ambiguous even if only independent variant computed
    if (is_z_with_d && !structural_design_ambiguous) {
      structural_design_ambiguous <- TRUE
      if (ambiguity_level == "clear") {
        ambiguity_level <- "ambiguous"
        ambiguity_reason <- "z-test design inherently ambiguous: could be Mann-Whitney (independent) or Wilcoxon signed-rank (paired)"
      }
    }
  }

  # ============================================================================
  # PHASE 8B: Design-ambiguous effect size ERROR downgrade (v0.2.6)
  # When a t-test, F(1,df), or z-test has ambiguous variant matching and the
  # status is ERROR from effect size comparison, downgrade to design_ambiguous_action.
  # Rationale: d-from-t (d = 2t/sqrt(df)) systematically differs from d computed
  # from raw data (means/SDs) when groups are unequal or different pooling methods
  # are used. This is a fundamental limitation, not a reporting error.
  # v0.2.8: Relaxed extraction_suspect guard for design ambiguity. Large deltas
  # are EXPECTED when design is ambiguous (d-from-t vs d-from-raw differs ~2x
  # for paired designs). Also extended to z-tests with d/g.
  # ============================================================================

  design_ambiguous_downgraded <- FALSE
  mixed_model_downgraded <- FALSE

  # v0.2.8: Design ambiguity causes systematically large deltas that trigger
  # extraction_suspect (EXTREME_DELTA_THRESHOLD=1.0). But these are NOT extraction
  # errors -- they are design assumption artifacts. Relax the guard when both
  # independent and paired variants exist (same pattern as Phase 8C n2_inflation_pattern).
  design_ambiguity_pattern <- FALSE
  if (ambiguity_level == "ambiguous" && !is.na(matched_value) && !is.na(effect_reported)) {
    ind_check_names <- c("d_ind", "d_ind_equalN", "d_ind_min", "d_ind_max", "g_ind")
    paired_check_names <- c("dz", "dav", "drm", "gz", "gav", "grm")
    has_ind_check <- any(names(same_type_variants) %in% ind_check_names)
    has_paired_check <- any(names(same_type_variants) %in% paired_check_names)
    if (has_ind_check && has_paired_check) {
      # Guard: reported value must be plausibly between the independent and paired
      # variants (with 50% margin). If it's far from ALL variants, it's genuinely wrong.
      all_vals <- abs(sapply(same_type_variants, function(v) v$value))
      variant_range <- range(all_vals, na.rm = TRUE)
      margin <- max(0.5, diff(variant_range) * 0.5)
      reported_in_range <- abs(effect_reported) >= (variant_range[1] - margin) &&
                           abs(effect_reported) <= (variant_range[2] + margin)
      if (reported_in_range) design_ambiguity_pattern <- TRUE
    }
  }
  # z-tests with d/g are inherently design-ambiguous (same range guard applies)
  if (tt == "z" && !is.na(canonical_type) && canonical_type %in% c("d", "g") &&
      !is.na(effect_reported) && length(same_type_variants) > 0) {
    all_vals_z <- abs(sapply(same_type_variants, function(v) v$value))
    variant_range_z <- range(all_vals_z, na.rm = TRUE)
    margin_z <- max(0.5, diff(variant_range_z) * 0.5)
    reported_in_range_z <- abs(effect_reported) >= (variant_range_z[1] - margin_z) &&
                           abs(effect_reported) <= (variant_range_z[2] + margin_z)
    if (reported_in_range_z) design_ambiguity_pattern <- TRUE
  }

  if (status == "ERROR" && check_type == "effect_size" &&
      (!extraction_suspect || design_ambiguity_pattern)) {
    is_design_ambiguous_t <- (tt == "t" && ambiguity_level == "ambiguous")
    is_design_ambiguous_F1 <- (tt == "F" && !is.na(df1) && df1 == 1 &&
                                ambiguity_level == "ambiguous" &&
                                !is.na(canonical_type) && canonical_type %in% c("d", "g"))
    is_design_ambiguous_z <- (tt == "z" && !is.na(canonical_type) &&
                               canonical_type %in% c("d", "g"))

    if (is_design_ambiguous_t || is_design_ambiguous_F1 || is_design_ambiguous_z) {
      status <- design_ambiguous_action
      design_ambiguous_downgraded <- TRUE

      ratio_text <- if (!is.na(matched_value) && matched_value != 0) {
        ratio <- abs(effect_reported) / abs(matched_value)
        sprintf(" (reported is %.0f%% %s than computed)",
                abs(ratio - 1) * 100,
                if (ratio > 1) "larger" else "smaller")
      } else ""

      method_explanation <- if (tt == "z") {
        "d = 2z/sqrt(N) assumes independent groups (Mann-Whitney). If z is from Wilcoxon signed-rank (paired), dz = z/sqrt(N) applies instead"
      } else {
        "d from raw data vs d from t-statistic"
      }
      uncertainty <- c(uncertainty,
        sprintf(
          "Design-ambiguous: reported %s=%.3f vs computed %s=%.3f (delta=%.3f)%s. Likely reflects d computation method difference (%s) rather than reporting error",
          effect_reported_name, effect_reported,
          matched_variant, matched_value, delta_effect_abs,
          ratio_text, method_explanation
        ))
    }
  }

  # ============================================================================
  # PHASE 8C: Unknown group sizes ERROR downgrade (v0.2.7)
  # When d/g is ERROR and n1/n2 are unknown (N/2 assumption used), the error
  # likely reflects unequal groups rather than a reporting error. Downgrade to
  # unknown_groups_action. Only fires for t-tests and F(1,df).
  # ============================================================================

  unknown_groups_downgraded <- FALSE

  # Note: extraction_suspect guard is relaxed when computed > reported (the
  # systematic direction of N/2 inflation). When equal-split assumption is used,
  # computed d/g is LARGER than actual because the minority group is smaller
  # than N/2. This triggers extraction_suspect for large deltas, but these are
  # NOT extraction errors -- they are N/2 assumption artifacts (MetaESCI audit:
  # 84.8% of g errors from this pattern). However, when reported > computed
  # (the opposite direction), keep extraction_suspect guard since the error
  # pattern is inconsistent with N/2 inflation and may be a genuine extraction issue.
  n2_inflation_pattern <- !is.na(matched_value) && !is.na(effect_reported) &&
    abs(matched_value) > abs(effect_reported)  # computed > reported = N/2 pattern
  if (status == "ERROR" && check_type == "effect_size" &&
      (!extraction_suspect || n2_inflation_pattern) &&
      is.na(n1) && is.na(n2) && !is.na(N) &&
      !is.na(canonical_type) && canonical_type %in% c("d", "g")) {
    is_affected <- (tt == "t") || (tt == "F" && !is.na(df1) && df1 == 1) || (tt == "z")
    if (is_affected) {
      status <- unknown_groups_action
      unknown_groups_downgraded <- TRUE
      # Note: confidence cap applied in Phase 11 (line ~2634) using unknown_groups_downgraded flag

      ratio_text <- if (!is.na(matched_value) && matched_value != 0) {
        ratio <- abs(effect_reported) / abs(matched_value)
        sprintf(" (reported is %.0f%% %s than computed)",
                abs(ratio - 1) * 100,
                if (ratio > 1) "larger" else "smaller")
      } else ""

      uncertainty <- c(uncertainty,
        sprintf(
          "Group sizes unknown (n1, n2 not found) - equal-split N/2=%d assumed. Reported %s=%.3f vs computed %s=%.3f (delta=%.3f)%s. May reflect unequal groups rather than reporting error",
          as.integer(floor(N / 2)),
          effect_reported_name, effect_reported,
          matched_variant, matched_value, delta_effect_abs,
          ratio_text
        ))
    }
  }

  # ============================================================================
  # PHASE 8D: R2 cross-pairing detection for F-tests (v0.2.7)
  # When F-test is paired with R2 and the status is ERROR, check text signals
  # to detect cross-pairing artifacts from regression tables.
  # V028 simulation: 275 -> 45 ERRORs (84% reduction), 0 regressions.
  # ============================================================================

  r2_cross_pairing_detected <- FALSE

  if (status == "ERROR" && tt == "F" && check_type == "effect_size" &&
      !is.na(canonical_type) && canonical_type %in% c("R2", "adjusted_R2", "f2", "cohens_f") &&
      !is.na(delta_effect_abs)) {

    raw <- if (!is.null(row$raw_text) && length(row$raw_text) > 0)
      as.character(row$raw_text[1]) else ""
    ctx <- if (!is.null(row$context_window) && length(row$context_window) > 0)
      as.character(row$context_window[1]) else ""
    full_ctx <- paste(raw, ctx)

    # Signal 1: R2 position relative to F in raw_text
    f_pos <- regexpr("F\\s*[\\(\\[]", raw)
    r2_pos <- regexpr("R\\^?2\\s*=|R-squared|R squared", raw, ignore.case = TRUE)
    r2_before_F <- (r2_pos > 0 && f_pos > 0 && r2_pos < f_pos)
    r2_after_F <- (r2_pos > 0 && f_pos > 0 && r2_pos > f_pos)

    # Signal 2: Line break between F and R2
    newline_between <- FALSE
    if (r2_pos > 0 && f_pos > 0) {
      between_text <- substr(raw, min(f_pos, r2_pos), max(f_pos, r2_pos))
      newline_between <- grepl("\n", between_text)
    }

    # Signal 3: Regression keywords in context
    regression_context <- grepl(
      "regression|regress|predictor|hierarchical|stepwise|linear model|accounted\\s*for|model\\s*fit|model\\s*summary",
      full_ctx, ignore.case = TRUE
    )

    # Signal 4: ANOVA keywords (negative signal -- do NOT apply regression downgrades)
    anova_context <- grepl(
      "ANOVA|analysis\\s*of\\s*variance|partial\\s*eta|\\u03B7\\u00B2|\\u03B7p\\u00B2",
      full_ctx, ignore.case = TRUE
    )

    # Signal 5: df1 > 1 (multiple predictors suggest regression)
    df1_gt1 <- !is.na(df1) && df1 > 1

    # Signal 6: Multiple F-tests in context (table indicator)
    f_count <- length(gregexpr("F\\s*\\(\\s*\\d+", full_ctx)[[1]])
    multi_F_context <- f_count >= 3

    # Signal 7: Table header in context
    table_header <- grepl("Table\\s*\\d", full_ctx, ignore.case = TRUE)

    # Signal 8: Hierarchical/step regression
    hierarchical <- grepl(
      "step\\s*\\d|step\\s*R|hierarchical|block\\s*\\d",
      full_ctx, ignore.case = TRUE
    )

    # Signal 9 (v0.2.8->v0.2.9 Fix 2A): Large R2/f2 delta -- R2 bounded [0,1].
    # v0.2.9: Lowered threshold from 0.3 to 0.05 (5x R2 tolerance of 0.01).
    # Extended to f2 (Fix 3) since f2 = R2/(1-R2) amplifies R2 mismatches.
    large_r2_delta <- (!is.na(delta_effect_abs) && delta_effect_abs > 0.05 &&
                        !is.na(canonical_type) && canonical_type %in% c("R2", "adjusted_R2", "f2"))

    # Signal 10 (v0.2.8): Extreme mismatch direction
    extreme_r2_mismatch <- (!is.na(effect_reported) && !is.na(matched_value) &&
                             ((abs(effect_reported) < 0.10 && abs(matched_value) > 0.80) ||
                              (abs(effect_reported) > 0.80 && abs(matched_value) < 0.10)))

    # Signal 11 (v0.2.9 Fix 2B): df1-based R2 estimation
    # When df1>1, R2_from_F gives the ANOVA R2 (=eta2). If this matches our
    # computed value but NOT the reported value, it confirms cross-pairing.
    df1_r2_mismatch <- FALSE
    if (!is.na(df1) && df1 > 1 && !is.na(matched_value) && !is.na(effect_reported)) {
      r2_expected <- (stat * df1) / (stat * df1 + df2)
      if (!is.na(r2_expected) &&
          abs(r2_expected - abs(matched_value)) < 0.01 &&
          abs(r2_expected - abs(effect_reported)) > 0.05) {
        df1_r2_mismatch <- TRUE
      }
    }

    # Signal 12 (v0.2.9 Fix 2C): Extreme ratio -- computed/reported > 5x
    extreme_ratio <- FALSE
    # v0.2.9d: Lowered floor from 0.001 to 0.0001 -- R2 can be < 0.001 for
    # single predictors in large-N studies (e.g., F(1,7892) gives R2=0.00066)
    if (!is.na(effect_reported) && abs(effect_reported) > 0.0001 &&
        !is.na(matched_value) && abs(matched_value) > 0.0001) {
      ratio <- abs(matched_value) / abs(effect_reported)
      extreme_ratio <- (ratio > 5 || ratio < 0.2)
    }

    # Apply detection logic (priority order from V028->V029 simulation)
    # v0.2.9: Multiple independent signals for cross-pairing
    if (!r2_cross_pairing_detected) {
      # Signal 11: df1-based R2 mismatch is strong evidence alone
      if (df1_r2_mismatch) {
        status <- "WARN"
        r2_cross_pairing_detected <- TRUE
        uncertainty <- c(uncertainty,
          sprintf("F-test R2 (eta2=%.3f) matches computed but not reported (%.3f): likely cross-paired",
                  (stat * df1) / (stat * df1 + df2), abs(effect_reported)))
      }
      # Large delta with contextual signals
      if (!r2_cross_pairing_detected && large_r2_delta) {
        if (regression_context || hierarchical || df1_gt1 || multi_F_context ||
            table_header || df1_r2_mismatch || extreme_ratio) {
          status <- "WARN"
          r2_cross_pairing_detected <- TRUE
          uncertainty <- c(uncertainty,
            sprintf("R2/f2 discrepancy (delta=%.3f) with cross-pairing signals: likely from different model",
                    delta_effect_abs))
        } else if (extreme_r2_mismatch) {
          status <- "WARN"
          r2_cross_pairing_detected <- TRUE
          uncertainty <- c(uncertainty,
            sprintf("Extreme R2 mismatch (reported=%.2f, computed=%.2f): likely from different analyses",
                    abs(effect_reported), abs(matched_value)))
        }
      }
      # Extreme ratio alone (Fix 2C)
      if (!r2_cross_pairing_detected && extreme_ratio &&
          !is.na(canonical_type) && canonical_type %in% c("R2", "adjusted_R2", "f2")) {
        status <- "WARN"
        r2_cross_pairing_detected <- TRUE
        uncertainty <- c(uncertainty,
          sprintf("Computed/reported ratio > 5x for %s: likely cross-paired", canonical_type))
      }
      # Signal 13 (v0.2.9b): Cohen's f / f2 standalone cross-pairing
      # Cohen's f and f2 are almost always reported alongside the F-test they
      # derive from. Large delta alone is sufficient evidence of cross-pairing
      # because f = sqrt(F*df1/df2) is deterministic -- any mismatch means wrong F.
      if (!r2_cross_pairing_detected &&
          !is.na(canonical_type) && canonical_type %in% c("cohens_f", "f2") &&
          !is.na(delta_effect_abs) && delta_effect_abs > 0.10) {
        status <- "WARN"
        r2_cross_pairing_detected <- TRUE
        uncertainty <- c(uncertainty,
          sprintf("Cohen's %s delta=%.3f > 0.10: f/f2 derives deterministically from F -- mismatch indicates cross-pairing",
                  canonical_type, delta_effect_abs))
      }
    }

    if (!r2_cross_pairing_detected && !anova_context) {
      if (r2_before_F) {
        # R2 before F is more reliable -- only downgrade with strong evidence
        if (hierarchical) {
          status <- "NOTE"
          r2_cross_pairing_detected <- TRUE
          uncertainty <- c(uncertainty,
            "Hierarchical regression: R2 may be incremental (delta-R2), not total")
        }
        # Otherwise keep ERROR -- position suggests correct pairing but values don't match
      } else {
        # R2 after F or unknown position -- higher cross-pairing risk
        if (hierarchical) {
          status <- "NOTE"
          r2_cross_pairing_detected <- TRUE
          uncertainty <- c(uncertainty,
            "Hierarchical regression: R2 may be incremental, not total")
        } else if (r2_after_F && newline_between) {
          status <- "NOTE"
          r2_cross_pairing_detected <- TRUE
          uncertainty <- c(uncertainty,
            "R2 appears on different line from F-test, possible table cross-pairing")
        } else if (multi_F_context && r2_after_F) {
          status <- "NOTE"
          r2_cross_pairing_detected <- TRUE
          uncertainty <- c(uncertainty,
            "Multiple F-tests in context with R2 after F: possible table extraction artifact")
        } else if (regression_context) {
          status <- "WARN"
          r2_cross_pairing_detected <- TRUE
          uncertainty <- c(uncertainty,
            "Regression context detected: R2 may be from different model than F-test")
        } else if (df1_gt1) {
          status <- "WARN"
          r2_cross_pairing_detected <- TRUE
          uncertainty <- c(uncertainty,
            "Multi-predictor F-test with R2: likely regression, possible model mismatch")
        } else if (table_header) {
          status <- "WARN"
          r2_cross_pairing_detected <- TRUE
          uncertainty <- c(uncertainty,
            "Table context: R2 may be from adjacent row")
        }
      }
    }
    # Note: do NOT set confidence here (Phase 11 handles it via flag)
  }

  # ============================================================================
  # PHASE 8E: Mixed-model F-test downgrade (v0.2.9 Fix 4)
  # When F-test has non-integer df2 and status is ERROR for effect size,
  # downgrade to WARN because standard formulas don't apply.
  # ============================================================================

  if (mixed_model_F_detected && status == "ERROR" &&
      check_type == "effect_size" && tt == "F") {
    status <- "WARN"
    mixed_model_downgraded <- TRUE
    uncertainty <- c(uncertainty,
      "Mixed-effects model F-test: effect size formula assumes standard ANOVA error term")
  }

  # ============================================================================
  # PHASE 8D-bis: d cross-pairing detection for F-tests (v0.2.9d)
  # When reported d is below half of the smallest computed d variant,
  # the d likely comes from a different analysis than the F-test.
  # ============================================================================

  if (status == "ERROR" && tt == "F" && check_type == "effect_size" &&
      !is.na(canonical_type) && canonical_type %in% c("d", "g", "dz", "dav", "drm") &&
      !is.na(effect_reported) && length(computed_variants) > 0) {
    # Filter to d-family variants only (not r, eta2, etc.)
    d_family_names <- c("d_ind", "d_ind_equalN", "d_ind_min", "d_ind_max",
                        "g_ind", "dz", "dav", "drm", "gz", "gav", "grm")
    d_variant_values <- sapply(names(computed_variants), function(nm) {
      if (nm %in% d_family_names && !is.null(computed_variants[[nm]]$value) &&
          !is.na(computed_variants[[nm]]$value))
        abs(computed_variants[[nm]]$value) else NA_real_
    })
    d_variant_values <- d_variant_values[!is.na(d_variant_values)]
    if (length(d_variant_values) > 0) {
      min_variant <- min(d_variant_values)
      if (min_variant > 0 && abs(effect_reported) < min_variant * 0.5) {
        status <- "WARN"
        r2_cross_pairing_detected <- TRUE  # reuse flag for confidence cap
        uncertainty <- c(uncertainty,
          sprintf("Reported %s=%.2f below all computed variants (min=%.2f); likely from different analysis",
                  canonical_type, abs(effect_reported), min_variant))
      }
    }
  }

  # ============================================================================
  # PHASE 8F: N >> df+1 downgrade (v0.2.9d)
  # When N is much larger than expected from df and design is ambiguous,
  # all computed variants are unreliable. ERROR is inappropriate.
  # ============================================================================

  n_mismatch_downgraded <- FALSE
  if (n_much_larger_than_df && status == "ERROR" &&
      check_type == "effect_size" && tt %in% c("t", "F")) {
    status <- "WARN"
    n_mismatch_downgraded <- TRUE
    uncertainty <- c(uncertainty,
      "N much larger than expected from df; computed effect sizes may use wrong N")
  }

  # ============================================================================
  # PHASE 9: P-value and decision error detection
  # ============================================================================

  p_computed <- NA_real_
  decision_error <- FALSE

  # Helper: for one-tailed tests, compute both tails and match the reported p.
  # This handles the case where a negative t-statistic has an upper-tail p > 0.5.
  match_one_tailed_p <- function(p_upper, p_rep) {
    p_lower <- 1 - p_upper
    if (!is.na(p_rep) && abs(p_lower - p_rep) < abs(p_upper - p_rep)) {
      p_lower
    } else {
      p_upper
    }
  }

  # Determine primary tail interpretation:
  # - If two-tailed explicitly detected in chunk: two-tailed
  # - If one-tailed explicitly detected in chunk (and not two-tailed): one-tailed
  # - Otherwise: use global one_tailed parameter (default FALSE = two-tailed)
  primary_one_tailed <- if (two_tailed_local) FALSE
                        else if (one_tailed_local) TRUE
                        else one_tailed  # global param, default FALSE

  # Helper: compute p-value for a given one-tailed flag
  compute_p_for_tail <- function(use_ot) {
    tryCatch(
      {
        if (tt == "t" && !is.na(df1)) {
          if (use_ot) {
            p_upper <- stats::pt(abs(stat), df = df1, lower.tail = FALSE)
            match_one_tailed_p(p_upper, p_reported)
          } else {
            2 * stats::pt(abs(stat), df = df1, lower.tail = FALSE)
          }
        } else if (tt == "F" && !is.na(df1) && !is.na(df2)) {
          stats::pf(stat, df1 = df1, df2 = df2, lower.tail = FALSE)
        } else if (tt == "z") {
          if (use_ot) {
            p_upper <- stats::pnorm(abs(stat), lower.tail = FALSE)
            match_one_tailed_p(p_upper, p_reported)
          } else {
            2 * stats::pnorm(abs(stat), lower.tail = FALSE)
          }
        } else if (tt == "chisq" && !is.na(df1)) {
          stats::pchisq(stat, df = df1, lower.tail = FALSE)
        } else if (tt == "r" && !is.na(df1)) {
          if (abs(stat) >= 1) {
            if (abs(stat) > 1) NA_real_ else 0
          } else {
            t_from_r <- stat * sqrt(df1 / (1 - stat^2))
            if (use_ot) {
              p_upper <- stats::pt(abs(t_from_r), df = df1, lower.tail = FALSE)
              match_one_tailed_p(p_upper, p_reported)
            } else {
              2 * stats::pt(abs(t_from_r), df = df1, lower.tail = FALSE)
            }
          }
        } else if (tt == "regression" && !is.na(df1)) {
          if (use_ot) {
            p_upper <- stats::pt(abs(stat), df = df1, lower.tail = FALSE)
            match_one_tailed_p(p_upper, p_reported)
          } else {
            2 * stats::pt(abs(stat), df = df1, lower.tail = FALSE)
          }
        } else if (tt == "H" && !is.na(df1)) {
          stats::pchisq(stat, df = df1, lower.tail = FALSE)
        } else if (tt %in% c("U", "W")) {
          z_aux <- if ("z_auxiliary" %in% names(row) && length(row$z_auxiliary) > 0) {
            as.numeric(row$z_auxiliary[1])
          } else {
            NA_real_
          }
          if (!is.na(z_aux)) {
            2 * stats::pnorm(abs(z_aux), lower.tail = FALSE)
          } else {
            NA_real_
          }
        } else {
          NA_real_
        }
      },
      error = function(e) NA_real_
    )
  }

  # Primary p-value computation
  if (!is.na(stat)) {
    p_computed <- compute_p_for_tail(primary_one_tailed)
  }

  if (!is.na(p_computed) && !is.na(p_reported)) {
    reported_significant <- p_reported < alpha
    computed_significant <- p_computed < alpha

    # P-value inequality awareness:
    # When p is reported as "< .001", the actual p could be anywhere below .001
    # Don't flag as decision error if both are on the same side of alpha
    # and the reported p is an inequality bound
    p_inequality_consistent <- FALSE
    if (p_is_inequality) {
      if (p_reported <= 0.001 && p_computed < 0.001) {
        p_inequality_consistent <- TRUE
      }
      if (p_reported <= alpha && p_computed < alpha) {
        p_inequality_consistent <- TRUE
      }
    }

    decision_error <- reported_significant != computed_significant
    if (p_inequality_consistent) {
      decision_error <- FALSE
    }
    # Decision error requires a reported p-value (v0.2.6) -- without one,
    # there is no author's significance decision to check. This prevents false
    # decision errors for extraction-only results (e.g., regression z-statistics
    # from coefficient tables where z = 9.47** but no p is reported).
    if (is.na(p_reported) && decision_error) {
      decision_error <- FALSE
    }

    # Universal one-tailed/two-tailed fallback:
    # If there's a decision error, try the OTHER tail interpretation
    # Only for test types that support one/two-tailed (not F, chisq, H)
    if (decision_error && tt %in% c("t", "z", "r", "regression")) {
      alt_one_tailed <- !primary_one_tailed
      p_alt <- compute_p_for_tail(alt_one_tailed)
      if (!is.na(p_alt)) {
        alt_computed_sig <- p_alt < alpha
        # Also check inequality consistency for fallback
        alt_ineq_consistent <- FALSE
        if (p_is_inequality) {
          if (p_reported <= 0.001 && p_alt < 0.001) alt_ineq_consistent <- TRUE
          if (p_reported <= alpha && p_alt < alpha) alt_ineq_consistent <- TRUE
        }
        alt_decision_error <- reported_significant != alt_computed_sig
        if (alt_ineq_consistent) alt_decision_error <- FALSE

        if (!alt_decision_error) {
          # Fallback resolves the mismatch
          p_computed <- p_alt
          decision_error <- FALSE
          tail_note <- if (alt_one_tailed) "one-tailed" else "two-tailed"
          uncertainty <- c(uncertainty,
            sprintf("P-value is consistent if interpreted as %s test (p_computed=%.4f)",
                    tail_note, p_alt))
          # Downgrade to NOTE: decision error cleared, but caveat about interpretation
          if (status %in% c("PASS", "OK", "WARN")) status <- "NOTE"
        }
      }
    }

    # Method-context detection: suppress decision_error for p-curve, equivalence tests, etc.
    method_context <- if ("method_context_detected" %in% names(row) &&
                          length(row$method_context_detected) > 0)
      isTRUE(row$method_context_detected[1]) else FALSE
    if (method_context && decision_error) {
      decision_error <- FALSE
      uncertainty <- c(uncertainty,
        "Decision error suppressed: statistic appears in methodological context (p-curve, equivalence test, etc.)")
      if (status == "WARN") status <- "NOTE"
    }

    # r-test with globally-inferred N: suppress decision_error (v0.2.6)
    # When N comes from the methods/intro section rather than adjacent text,
    # it may not apply to this specific correlation (e.g., subgroup analysis).
    # The p-value discrepancy likely reflects wrong N, not a reporting error.
    n_source <- if ("N_source" %in% names(row) && length(row$N_source) > 0)
      as.character(row$N_source[1]) else NA_character_
    if (tt == "r" && decision_error && !is.na(n_source) && n_source == "global_text") {
      decision_error <- FALSE
      uncertainty <- c(uncertainty,
        "Decision error suppressed: r-test with globally-inferred N (from methods section) \u2014 p-value discrepancy may reflect N applied to different analysis")
      if (status == "WARN") status <- "NOTE"
    }

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
      # v0.2.4: When effect size match is excellent (< 0.5x tolerance) AND
      # ambiguity is clear, keep PASS -- the p-value discrepancy is likely
      # rounding, not a genuine inconsistency.
      has_strong_effect_match <- !is.na(delta_effect_abs) && !is.na(tol_eff) &&
        delta_effect_abs < (0.5 * tol_eff) && ambiguity_level == "clear"

      if (has_strong_effect_match) {
        uncertainty <- c(uncertainty,
          "Decision error detected but effect size match is excellent (delta < 0.5x tolerance) \u2014 p-value discrepancy likely rounding")
      } else {
        if (status == "PASS") status <- "WARN"
        if (status == "OK") status <- "WARN"
        if (status == "NOTE") status <- "WARN"
      }
    }
  }

  # ============================================================================
  # PHASE 9B: Determine OK/NOTE for p-value-only and r-test cases
  #
  # After all effect-based checks, if we still have default WARN and there's
  # no effect to compare, evaluate p-value consistency for OK/NOTE upgrade.
  # ============================================================================

  # Handle "ns" (not significant) p-value notation
  if (status == "WARN" && p_ns && !has_effect_reported && !is.na(p_computed)) {
    if (p_computed > alpha) {
      status <- "OK"
      uncertainty <- c(uncertainty,
        sprintf("Reported as 'not significant'; computed p=%.4f confirms (p > %.2f)", p_computed, alpha))
    } else {
      decision_error <- TRUE
      uncertainty <- c(uncertainty,
        sprintf("Decision error: reported as 'not significant' but computed p=%.4f (p < %.2f)", p_computed, alpha))
      # status stays WARN
    }
  }

  # Handle implicit non-significance context: "absent", "no effect", "did not differ", etc.
  # when p-value is missing and computed p confirms non-significance
  if (status == "WARN" && !p_ns && is.na(p_reported) && !is.na(p_computed) &&
      "context_window" %in% names(row) && !is.na(row$context_window[1])) {
    ctx_lower <- tolower(as.character(row$context_window[1]))
    nonsig_phrases <- c("was absent", "were absent", "no effect", "no difference",
                        "did not differ", "did not reach significance",
                        "not significant", "non-significant", "nonsignificant",
                        "no significant", "no reliable", "failed to reach")
    has_nonsig_context <- any(sapply(nonsig_phrases, function(ph) grepl(ph, ctx_lower, fixed = TRUE)))
    if (has_nonsig_context && p_computed > alpha) {
      status <- "OK"
      uncertainty <- c(uncertainty,
        sprintf("No p-value reported but context implies non-significance; computed p=%.4f confirms (p > %.2f)",
                p_computed, alpha))
    }
  }

  if (status == "WARN" && !has_effect_reported) {
    # No effect size was reported - evaluate based on p-value consistency
    if (!is.na(p_computed) && !is.na(p_reported)) {
      p_diff <- abs(p_reported - p_computed)
      p_directions_match <- (p_reported < alpha) == (p_computed < alpha)

      # Case 1: p reported as inequality (< .001) and computed also satisfies it
      if (p_is_inequality && p_reported <= 0.001 && p_computed < 0.001) {
        status <- "OK"
      # Case 2: p-values very close (within rounding)
      } else if (p_diff < 0.005) {
        status <- "OK"
      # Case 3: p-value inequality with consistent direction
      } else if (p_is_inequality && p_directions_match) {
        status <- "OK"
      # Case 4: Both agree on significance direction, moderate p-diff
      } else if (p_directions_match && p_diff < 0.05) {
        status <- "NOTE"
        uncertainty <- c(uncertainty,
          sprintf("P-value differs by %.3f but significance direction is consistent", p_diff))
      # Case 5: Significance direction mismatch -> stays WARN
      } else if (!p_directions_match) {
        # Keep as WARN, this is a genuine concern
      # Case 6: Large p-diff but same direction
      } else if (p_directions_match) {
        status <- "NOTE"
        uncertainty <- c(uncertainty,
          sprintf("P-value differs by %.3f; significance direction is consistent", p_diff))
      }
    } else if (is.na(p_computed) && !is.na(p_reported)) {
      # P-value reported but couldn't be computed (e.g., U/W tests without z)
      status <- "NOTE"
      uncertainty <- c(uncertainty, "P-value could not be independently verified (insufficient data for recomputation)")
    }

    # Special case: r-test where the stat IS the effect (r-value)
    # For r-tests, the stat_value is the r-value itself, so the "effect" is verified
    if (tt == "r" && !is.na(stat) && !is.na(p_computed) && !is.na(p_reported)) {
      p_directions_match <- (p_reported < alpha) == (p_computed < alpha)
      if (p_directions_match || (p_is_inequality && p_reported <= 0.001 && p_computed < 0.001)) {
        # r-test with consistent p-value: the r IS the effect, this is verified
        if (status != "OK") status <- "OK"
      }
    }
  }

  # Extraction-only results: nothing was checked -> SKIP (not analyzed)
  # Exclude p_ns cases: "ns"/"n.s." IS a form of p-value reporting that was verified
  if (check_type == "extraction_only" && !decision_error && !p_ns) {
    status <- "SKIP"
  }

  # v0.2.4: non-inequality p > 0.5 with large computed discrepancy
  if (!p_is_inequality && !is.na(p_reported) && p_reported > 0.5 &&
      !is.na(p_computed) && abs(p_reported - p_computed) > 0.3) {
    extraction_suspect <- TRUE
    uncertainty <- c(uncertainty,
      "Reported p-value > 0.5 with large computed discrepancy \u2014 possible extraction artifact")
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
  # PHASE 11B: Deterministic confidence score (v0.2.4)
  # Aggregates observable quality signals into a 0-10 integer score.
  # ============================================================================

  confidence <- 5L  # baseline

  # Ambiguity level
  if (ambiguity_level == "clear") confidence <- confidence + 3L
  else if (ambiguity_level == "ambiguous") confidence <- confidence - 1L
  else confidence <- confidence - 3L  # highly_ambiguous

  # Same-type vs cross-type
  if (length(same_type_variants) > 0) confidence <- confidence + 1L
  else confidence <- confidence - 2L

  # Delta quality
  if (!is.na(delta_effect_abs) && !is.na(tol_eff)) {
    if (delta_effect_abs < tol_eff * 0.25) confidence <- confidence + 1L
    else if (delta_effect_abs > tol_eff * 5) confidence <- confidence + 1L
    else if (delta_effect_abs > tol_eff * 0.8 && delta_effect_abs < tol_eff * 3)
      confidence <- confidence - 1L
  }

  # Design inference (t-tests)
  if (tt == "t") {
    if (design_inferred %in% c("independent", "paired") && ambiguity_level == "clear")
      confidence <- confidence + 1L
    else if (design_inferred %in% c("ambiguous", "unclear"))
      confidence <- confidence - 1L
  }

  # Design-ambiguous downgrade confidence cap (v0.2.6)
  if (design_ambiguous_downgraded) confidence <- min(confidence, 4L)
  if (unknown_groups_downgraded) confidence <- min(confidence, 4L)
  if (r2_cross_pairing_detected) confidence <- min(confidence, 3L)
  if (phi_to_v_reinterpreted) confidence <- min(confidence, 4L)
  if (mixed_model_downgraded) confidence <- min(confidence, 4L)
  if (n_mismatch_downgraded) confidence <- min(confidence, 3L)

  # Extraction quality signals
  if (extraction_suspect) confidence <- confidence - 2L
  if (!is.na(row$raw_text[1]) && nchar(row$raw_text[1]) < 20) confidence <- confidence - 1L

  # Method context penalty
  if (method_in_chunk) confidence <- confidence - 1L

  confidence <- max(0L, min(10L, as.integer(confidence)))

  # ============================================================================
  # PHASE 11C: User feedback assembly (v0.3.0)
  # Generate software_notes, alternative_formulas, best_practice_notes
  # ============================================================================

  software_notes <- NA_character_
  alternative_formulas <- NA_character_
  best_practice_notes <- NA_character_

  if (!is.na(check_type) && check_type == "effect_size" && !is.na(canonical_type)) {
    # Software notes: explain why values might differ across tools
    if (tt == "F" && canonical_type %in% c("eta2", "etap2", "partial_eta2", "eta")) {
      software_notes <- paste0(
        "JASP and jamovi report omega-squared by default (computed: ",
        round(if ("omega2" %in% names(alternatives)) alternatives$omega2$value
              else if ("omega2" %in% names(computed_variants)) computed_variants$omega2$value
              else NA_real_, 3),
        "), which is less biased than eta-squared (",
        round(if (!is.null(anova_effects) && !is.na(anova_effects$eta2)) anova_effects$eta2 else NA_real_, 3),
        "). If your software used omega-squared, that may explain the difference.")
    } else if (tt == "F" && canonical_type == "cohens_f") {
      software_notes <- paste0(
        "Cohen's f can be derived from eta-squared (f=",
        round(if ("cohens_f" %in% names(computed_variants)) computed_variants$cohens_f$value else NA_real_, 3),
        ") or from omega-squared (f=",
        round(if ("cohens_f_omega" %in% names(computed_variants)) computed_variants$cohens_f_omega$value
              else if ("cohens_f_omega" %in% names(alternatives)) alternatives$cohens_f_omega$value
              else NA_real_, 3),
        "). G*Power uses omega-squared; SPSS uses eta-squared.")
    } else if (tt %in% c("t", "F") && canonical_type %in% c("d", "g") &&
               !is.na(ambiguity_level) && ambiguity_level == "ambiguous") {
      software_notes <- paste0(
        "For paired designs, three d variants exist: ",
        "dz (divides by sqrt(n)), dav (by average SD), ",
        "drm (repeated measures). ",
        "The choice depends on the correlation between measures.")
    } else if (tt %in% c("t", "F") && canonical_type == "g") {
      software_notes <- paste0(
        "Hedges' g correction factor J differs across software: ",
        "J(N-2) is standard for independent samples, but some packages use J(N-1). ",
        "For your sample size, this produces a difference of ~",
        round(abs(if ("g_ind" %in% names(computed_variants)) computed_variants$g_ind$value
                  else if ("g_ind" %in% names(alternatives)) alternatives$g_ind$value
                  else 0) -
              abs(if ("g_ind_Nm1" %in% names(alternatives)) alternatives$g_ind_Nm1$value
                  else 0), 3), ".")
    } else if (tt == "chisq" && canonical_type %in% c("phi", "V")) {
      if (!is.na(df1) && df1 >= 2) {
        software_notes <- "For tables larger than 2x2, Cramer's V is preferred over phi for comparability across table sizes."
      }
    }

    # Alternative formulas: what else the author could consider
    if (tt == "F" && canonical_type %in% c("eta2", "etap2", "partial_eta2")) {
      alternative_formulas <- "Consider reporting omega-squared alongside eta-squared. Omega-squared is less biased, especially for small samples."
    } else if (tt %in% c("t", "F") && canonical_type %in% c("d", "g") &&
               !is.na(ambiguity_level) && ambiguity_level == "ambiguous") {
      alternative_formulas <- paste0(
        "Multiple d variants apply: d_ind (independent), dz (paired), ",
        "dav (average SD), drm (repeated measures). ",
        "If the design is paired, specifying the pre-post correlation ",
        "would resolve the ambiguity.")
    }

    # Best practice notes
    if (canonical_type == "d" && !is.na(N) && N < 30) {
      best_practice_notes <- "For small samples (N < 30), Hedges' g corrects the upward bias in Cohen's d."
    } else if (tt == "F" && canonical_type %in% c("eta2", "etap2", "partial_eta2")) {
      best_practice_notes <- "APA recommends reporting effect sizes with confidence intervals. For factorial ANOVA, partial eta-squared controls for other factors."
    } else if (tt == "chisq" && canonical_type == "phi" && !is.na(df1) && df1 >= 2) {
      best_practice_notes <- "For chi-square tables larger than 2x2, Cramer's V is the standard measure for comparability."
    }
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
    p_ns = if ("p_ns" %in% names(row)) row$p_ns else FALSE,
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
    semi_partial_r = if ("semi_partial_r" %in% names(computed_variants)) computed_variants$semi_partial_r$value
                     else if ("semi_partial_r" %in% names(alternatives)) alternatives$semi_partial_r$value
                     else NA_real_,
    cohens_f2 = if ("cohens_f2" %in% names(computed_variants)) computed_variants$cohens_f2$value
                else if ("cohens_f2" %in% names(alternatives)) alternatives$cohens_f2$value
                else if ("cohens_f2_standard" %in% names(alternatives)) alternatives$cohens_f2_standard$value
                else NA_real_,
    R2 = if ("R2" %in% names(computed_variants)) computed_variants$R2$value
         else if ("R2" %in% names(alternatives)) alternatives$R2$value
         else NA_real_,

    # Nonparametric effect sizes
    rank_biserial_r = if ("rank_biserial_r" %in% names(computed_variants)) computed_variants$rank_biserial_r$value else NA_real_,
    cliffs_delta = if ("cliffs_delta" %in% names(computed_variants)) computed_variants$cliffs_delta$value else if ("cliffs_delta" %in% names(alternatives)) alternatives$cliffs_delta$value else NA_real_,
    epsilon_squared = if ("epsilon_squared" %in% names(computed_variants)) computed_variants$epsilon_squared$value else NA_real_,
    kendalls_W = if ("kendalls_W" %in% names(computed_variants)) computed_variants$kendalls_W$value else if ("kendalls_W" %in% names(alternatives)) alternatives$kendalls_W$value else NA_real_,
    z_auxiliary = if ("z_auxiliary" %in% names(row) && length(row$z_auxiliary) > 0) as.numeric(row$z_auxiliary[1]) else NA_real_,

    # Regression coefficients
    b_coeff = if ("b_coeff" %in% names(row) && length(row$b_coeff) > 0) as.numeric(row$b_coeff[1]) else NA_real_,
    SE_coeff = if ("SE_coeff" %in% names(row) && length(row$SE_coeff) > 0) as.numeric(row$SE_coeff[1]) else NA_real_,
    adj_R2 = if ("adj_R2" %in% names(row) && length(row$adj_R2) > 0) as.numeric(row$adj_R2[1]) else NA_real_,

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
    check_type = check_type,
    check_scope = if (check_type == "effect_size") "effect_size_checked"
                  else if (check_type == "p_value") "p_value_only"
                  else if (check_type == "ci") "ci_checked"
                  else "extraction_only",
    extraction_suspect = extraction_suspect,
    decimal_recovered = decimal_recovered,
    result_context = if (method_in_chunk) "method" else "study",
    confidence = confidence,
    design_ambiguous = (ambiguity_level != "clear"),
    unknown_groups_downgraded = unknown_groups_downgraded,
    r2_cross_pairing_detected = r2_cross_pairing_detected,
    decision_error_downgraded = (decision_error &&
      (design_ambiguous_downgraded || unknown_groups_downgraded || r2_cross_pairing_detected)),
    sign_mismatch = if (!is.na(effect_reported) && !is.na(matched_value) &&
                        effect_reported != 0 && matched_value != 0)
                     sign(effect_reported) != sign(matched_value) else FALSE,
    design_inferred = design_inferred,
    variants_tested = variants_tested_str,
    uncertainty_level = uncertainty_level,
    uncertainty_reasons = if (length(uncertainty)) paste(uncertainty, collapse = "; ") else "",
    assumptions_used = if (length(assumptions)) paste(assumptions, collapse = "; ") else "",
    insufficient_data = (length(computed_variants) == 0),

    # v0.3.0: User feedback fields
    software_notes = software_notes,
    alternative_formulas = alternative_formulas,
    best_practice_notes = best_practice_notes
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
#' @param max_text_length Maximum total text length in characters (default 10^7)
#' @param max_stats_per_text Maximum number of stats to process per text (default 10000)
#' @param cross_type_action Action when cross-type match found ("NOTE", "WARN", or "ERROR"; default "NOTE")
#' @param ci_affects_status Whether CI mismatches affect status (default TRUE)
#' @param plausibility_filter Whether to apply plausibility bounds filter (default TRUE)
#' @param sign_sensitive Whether sign differences affect status (default FALSE)
#' @param method_context_action Action when method context detected in chunk ("NOTE", "WARN", or "SKIP"; default "NOTE")
#' @param design_ambiguous_action Action when design-ambiguous t-test (or F(1,df)) effect size ERROR occurs ("WARN", "NOTE", or "ERROR"; default "WARN")
#' @param unknown_groups_action Action when d/g ERROR occurs with unknown group sizes n1/n2 ("WARN", "NOTE", or "ERROR"; default "WARN")
#' @param min_confidence Minimum confidence score (0-10) for results to be included in output (default 0)
#' @return An effectcheck S3 object with consistency check results
#' @export
#' @examples
#' result <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' print(result)
#' summary(result)
check_text <- function(text,
                       stats = c("t", "F", "r", "chisq", "z", "U", "W", "H", "regression"),
                       ci_level = 0.95,
                       alpha = 0.05,
                       one_tailed = FALSE,
                       paired_r_grid = c(seq(0.1, 0.9, by = 0.1), 0.95),
                       assume_equal_ns_when_missing = TRUE,
                       ci_method_phi = "bonett_price",
                       ci_method_V = "bonett_price",
                       tol_effect = list(d = 0.02, r = 0.005, phi = 0.02, V = 0.02),
                       tol_ci = 0.02,
                       tol_p = 0.001,
                       messages = FALSE,
                       max_text_length = 10^7,
                       max_stats_per_text = 10000,
                       cross_type_action = "NOTE",
                       ci_affects_status = TRUE,
                       plausibility_filter = TRUE,
                       sign_sensitive = FALSE,
                       method_context_action = "NOTE",
                       design_ambiguous_action = "WARN",
                       unknown_groups_action = "WARN",
                       min_confidence = 0L) {
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
    max_stats_per_text = max_stats_per_text,
    cross_type_action = cross_type_action,
    ci_affects_status = ci_affects_status,
    plausibility_filter = plausibility_filter,
    sign_sensitive = sign_sensitive,
    method_context_action = method_context_action,
    design_ambiguous_action = design_ambiguous_action,
    unknown_groups_action = unknown_groups_action,
    min_confidence = min_confidence
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
          tol_p = tol_p,
          cross_type_action = cross_type_action,
          ci_affects_status = ci_affects_status,
          plausibility_filter = plausibility_filter,
          sign_sensitive = sign_sensitive,
          method_context_action = method_context_action,
          design_ambiguous_action = design_ambiguous_action,
          unknown_groups_action = unknown_groups_action
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
          check_type = NA_character_,
          check_scope = "error",
          extraction_suspect = FALSE,
          decimal_recovered = FALSE,
          result_context = "study",
          confidence = 0L,
          design_inferred = "unclear",
          variants_tested = "",
          uncertainty_level = "high",
          uncertainty_reasons = paste0("Processing error: ", error_msg),
          assumptions_used = "",
          insufficient_data = TRUE,
          software_notes = NA_character_,
          alternative_formulas = NA_character_,
          best_practice_notes = NA_character_,
          unknown_groups_downgraded = FALSE,
          r2_cross_pairing_detected = FALSE,
          decision_error_downgraded = FALSE,
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
          cohens_f2 = NA_real_, R2 = NA_real_,
          rank_biserial_r = NA_real_, cliffs_delta = NA_real_,
          epsilon_squared = NA_real_, kendalls_W = NA_real_, z_auxiliary = NA_real_,
          b_coeff = NA_real_, SE_coeff = NA_real_, adj_R2 = NA_real_,
          closest_method = NA_character_,
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
      is.na(standardized_beta) & is.na(partial_r) & is.na(cohens_f2) & is.na(R2) &
      is.na(rank_biserial_r) & is.na(epsilon_squared)
  )

  res$status[res$insufficient_data & (is.na(res$status) | res$status == "")] <- "INSUFFICIENT_DATA"
  res$source <- "text"

  # ============================================================================
  # PHASE 13: Cascading N sweep (v0.2.9 Fix 1)
  # When multiple chi-square ERRORs share the same N and have diverse
  # back-calculated N values, the N is likely from the overall study,
  # not the individual tests. Downgrade to WARN.
  # ============================================================================

  if (nrow(res) >= 3 && "test_type" %in% names(res) &&
      "status" %in% names(res) && "check_type" %in% names(res)) {
    chisq_errors <- which(
      res$test_type == "chisq" &
      res$status == "ERROR" &
      res$check_type == "effect_size" &
      !is.na(res$N)
    )
    if (length(chisq_errors) >= 3) {
      # Group by shared N
      n_vals <- res$N[chisq_errors]
      n_table <- table(n_vals)
      for (shared_n in names(n_table)[n_table >= 3]) {
        idx <- chisq_errors[n_vals == as.numeric(shared_n)]
        # Back-calculate N from reported effect size
        n_backs <- sapply(idx, function(i) {
          eff <- abs(res$effect_reported[i])
          sv <- res$stat_value[i]
          if (is.na(eff) || eff == 0 || is.na(sv) || sv == 0) return(NA_real_)
          round(sv / (eff^2))  # Simple N_back for phi/V with m=1
        })
        n_backs <- n_backs[!is.na(n_backs)]
        if (length(n_backs) >= 3) {
          # Diversity test: sd/mean > 0.1 means different subsamples
          cv <- stats::sd(n_backs) / mean(n_backs)
          if (!is.na(cv) && cv > 0.1) {
            # Downgrade all to WARN
            for (i in idx) {
              res$status[i] <- "WARN"
              res$confidence[i] <- min(res$confidence[i], 3L, na.rm = TRUE)
              unc <- res$uncertainty_reasons[i]
              note <- sprintf(
                "N=%d appears to be from overall study (back-calc N varies: %s)",
                as.integer(as.numeric(shared_n)),
                paste(utils::head(sort(n_backs), 5), collapse = ","))
              res$uncertainty_reasons[i] <- if (is.na(unc)) note
                else paste(unc, note, sep = "; ")
            }
          }
        }
      }
    }
  }

  # v0.2.4: min_confidence filter
  if (min_confidence > 0 && "confidence" %in% names(res)) {
    res <- res[is.na(res$confidence) | res$confidence >= min_confidence, ]
  }

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
#' \donttest{
#' tmp <- tempfile(fileext = ".txt")
#' writeLines("t(28) = 2.21, p = .035, d = 0.80", tmp)
#' results <- check_files(tmp)
#' print(results)
#' unlink(tmp)
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
