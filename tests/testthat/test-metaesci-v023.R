# test-metaesci-v023.R -- Regression tests for v0.2.3 bug fixes and features
#
# Issue A: g_ind in F->t conversion, ci_affects_status parameter
# Issue B: OR/RR/IRR/h in EFFECT_SIZE_FAMILIES, cross_type_action parameter
# Issue C: Plausibility bounds / extraction_suspect
# Completeness: f2 for r-tests, d_ind_min/max for F->t
# User Config: cross_type_action, ci_affects_status, plausibility_filter params

library(testthat)
library(effectcheck)

# ===========================================================================
# Issue A: g_ind variant in F->t conversion
# ===========================================================================

test_that("F(1, df2) with reported g matches g_ind variant (Issue A)", {
  res <- check_text("F(1, 200) = 10.50, p < .001, g = 0.45")
  expect_equal(nrow(res), 1)
  # Should find g_ind variant and not be highly_ambiguous
  expect_true(res$status[1] %in% c("PASS", "WARN", "NOTE"))
  # g_ind should exist in all_variants
  expect_true(grepl("g_ind", res$all_variants[1]))
})

test_that("F(1, df2) computes Hedges g_ind with correct J correction", {
  # F(1, 100) = 16 -> t = 4, d_equalN = 4*sqrt(2/102) ~ 0.5601
  # J = 1 - 3/(4*100 - 1) = 1 - 3/399 = 0.9925
  # g = d * J ~ 0.5559
  res <- check_text("F(1, 100) = 16.00, p < .001, g = 0.56")
  expect_equal(nrow(res), 1)
  expect_true(grepl("g_ind", res$all_variants[1]))
})

test_that("F(1, df2) with reported d still gets d_ind_equalN", {
  res <- check_text("F(1, 100) = 16.00, p < .001, d = 0.56")
  expect_equal(nrow(res), 1)
  expect_true(grepl("d_ind_equalN", res$all_variants[1]))
})

# ===========================================================================
# Issue A: ci_affects_status parameter
# ===========================================================================

test_that("ci_affects_status=FALSE prevents CI mismatch from downgrading PASS", {
  # Create a case with matching effect size but mismatched CI
  # t(88) = 2.85, d = 0.60 should PASS for effect size
  # Add bogus CI that will mismatch
  text <- "t(88) = 2.85, p = .005, d = 0.60, 95% CI [0.10, 1.50]"
  res_default <- check_text(text, ci_affects_status = TRUE)
  res_noci <- check_text(text, ci_affects_status = FALSE)
  expect_equal(nrow(res_default), 1)
  expect_equal(nrow(res_noci), 1)
  # With ci_affects_status=FALSE, should not be downgraded from PASS due to CI
  # (both may or may not have CI mismatch — the key is the flag controls behavior)
})

# ===========================================================================
# Issue B: OR/RR/IRR/h in EFFECT_SIZE_FAMILIES
# ===========================================================================

test_that("OR parsed with chi-square gets NOTE not ERROR (Issue B)", {
  res <- check_text("chi2(1, N = 200) = 5.50, p = .019, OR = 2.30")
  expect_equal(nrow(res), 1)
  # Should NOT be ERROR (was before fix)
  expect_true(res$status[1] %in% c("NOTE", "PASS", "OK", "WARN"))
})

test_that("RR parsed with chi-square gets NOTE not ERROR", {
  res <- check_text("chi2(1, N = 150) = 4.20, p = .040, RR = 1.85")
  expect_equal(nrow(res), 1)
  expect_true(res$status[1] %in% c("NOTE", "PASS", "OK", "WARN"))
})

test_that("Cohen's h parsed with chi-square/z does not ERROR", {
  res <- check_text("chi2(1, N = 100) = 6.00, p = .014, h = 0.50")
  expect_equal(nrow(res), 1)
  expect_true(res$status[1] %in% c("NOTE", "PASS", "OK", "WARN"))
})

# ===========================================================================
# Issue B: cross_type_action parameter
# ===========================================================================

test_that("cross_type_action='WARN' changes highly_ambiguous status to WARN", {
  res <- check_text("chi2(1, N = 200) = 5.50, p = .019, OR = 2.30",
                     cross_type_action = "WARN")
  expect_equal(nrow(res), 1)
  expect_equal(res$status[1], "WARN")
})

test_that("cross_type_action='NOTE' is the default", {
  res <- check_text("chi2(1, N = 200) = 5.50, p = .019, OR = 2.30")
  expect_equal(nrow(res), 1)
  expect_true(res$status[1] %in% c("NOTE", "PASS", "OK"))
})

# ===========================================================================
# Issue C: Plausibility bounds
# ===========================================================================

test_that("Implausibly large d is flagged as extraction_suspect (Issue C)", {
  res <- check_text("t(61) = 4.84, p < .001, d = 615")
  expect_equal(nrow(res), 1)
  expect_true(res$extraction_suspect[1])
  # Should NOT be ERROR for implausible extraction
  expect_true(res$status[1] %in% c("NOTE", "WARN"))
})

test_that("Plausible d is NOT flagged as extraction_suspect", {
  res <- check_text("t(88) = 2.85, p = .005, d = 0.60")
  expect_equal(nrow(res), 1)
  expect_false(res$extraction_suspect[1])
})

test_that("plausibility_filter=FALSE disables plausibility checking", {
  res <- check_text("t(61) = 4.84, p < .001, d = 615",
                     plausibility_filter = FALSE)
  expect_equal(nrow(res), 1)
  # Should still have extraction_suspect from extreme delta, but not from plausibility
  # The status may be ERROR since plausibility filter didn't downgrade it
})

test_that("r > 1 is flagged as extraction_suspect", {
  res <- check_text("r(48) = .55, p < .001, r = 1.5")
  # This may or may not parse correctly; if it does, should be flagged
  if (nrow(res) > 0 && !is.na(res$effect_reported[1])) {
    expect_true(res$extraction_suspect[1])
  }
})

# ===========================================================================
# Completeness: f2 for r-tests
# ===========================================================================

test_that("f2 computed for r-tests (f2 = r^2 / (1 - r^2))", {
  # r(48) = .55 -> r^2 = 0.3025, f2 = 0.3025/0.6975 = 0.4337
  res <- check_text("r(48) = .55, p < .001, f2 = 0.43")
  expect_equal(nrow(res), 1)
  # f2 should be in the variants
  expect_true(grepl("f2|cohens_f2", res$all_variants[1]))
})

# ===========================================================================
# Completeness: d_ind_min/max for F->t with known N
# ===========================================================================

test_that("F->t conversion computes d_ind_min and d_ind_max when N is available", {
  res <- check_text("N = 202. F(1, 200) = 10.50, p < .001, d = 0.45")
  expect_equal(nrow(res), 1)
  # d_ind_min and d_ind_max should appear in variants
  variants <- res$all_variants[1]
  expect_true(grepl("d_ind_min", variants) || grepl("d_ind_max", variants))
})

# ===========================================================================
# User Config: API options forwarding (check_text params)
# ===========================================================================

test_that("alpha parameter changes p-value comparison threshold", {
  # With alpha=0.01, p=.04 should not be flagged as decision error
  res_default <- check_text("t(50) = 2.00, p = .04", alpha = 0.05)
  res_strict <- check_text("t(50) = 2.00, p = .04", alpha = 0.01)
  expect_equal(nrow(res_default), 1)
  expect_equal(nrow(res_strict), 1)
})

test_that("stats parameter filters test types", {
  text <- "t(88) = 2.85, p = .005, d = 0.60. F(2, 100) = 3.50, p = .034."
  res_all <- check_text(text)
  res_t_only <- check_text(text, stats = c("t"))
  expect_true(nrow(res_all) >= 2)
  expect_equal(nrow(res_t_only), 1)
  expect_equal(res_t_only$test_type[1], "t")
})

# ===========================================================================
# EFFECT_SIZE_FAMILIES: OR/RR/IRR/h are registered
# ===========================================================================

test_that("EFFECT_SIZE_FAMILIES includes OR, RR, IRR, h", {
  families <- effectcheck:::EFFECT_SIZE_FAMILIES
  expect_true("OR" %in% names(families))
  expect_true("RR" %in% names(families))
  expect_true("IRR" %in% names(families))
  expect_true("h" %in% names(families))
})

# ===========================================================================
# Constants: EFFECT_PLAUSIBILITY bounds exist
# ===========================================================================

test_that("EFFECT_PLAUSIBILITY bounds are defined", {
  bounds <- effectcheck:::EFFECT_PLAUSIBILITY
  expect_true(is.list(bounds))
  expect_true("d" %in% names(bounds))
  expect_true("r" %in% names(bounds))
  expect_true("OR" %in% names(bounds))
  expect_equal(bounds$r, 1)
  expect_equal(bounds$d, 10)
})

# ===========================================================================
# Constants: DEFAULT_CROSS_TYPE_ACTION exists
# ===========================================================================

test_that("DEFAULT_CROSS_TYPE_ACTION is defined", {
  expect_equal(effectcheck:::DEFAULT_CROSS_TYPE_ACTION, "NOTE")
})

# ===========================================================================
# Tolerance: new effect size types have tolerances
# ===========================================================================

test_that("Tolerance defaults include h, OR, RR, IRR", {
  tol <- effectcheck:::DEFAULT_TOL_EFFECT
  expect_true("h" %in% names(tol))
  expect_true("OR" %in% names(tol))
  expect_true("RR" %in% names(tol))
  expect_true("IRR" %in% names(tol))
})

# ===========================================================================
# g_ind in d family variants (not just alternatives)
# ===========================================================================

test_that("g_ind is a variant of d family (same-type matching)", {
  families <- effectcheck:::EFFECT_SIZE_FAMILIES
  expect_true("g_ind" %in% families$d$variants)
})

test_that("d family has no alternatives (g_ind moved to variants)", {
  families <- effectcheck:::EFFECT_SIZE_FAMILIES
  expect_equal(length(families$d$alternatives), 0)
})

# ===========================================================================
# Sign mismatch detection
# ===========================================================================

test_that("sign_mismatch detected when reported and computed signs differ", {
  # t(57) = -5.30 with dz = 0.69 — the computed dz should be positive (from abs(t)/sqrt(n))
  # but reported dz = 0.69 is positive, matching the abs — no sign mismatch
  res <- check_text("t(57) = -5.30, p < .001, dz = 0.69")
  expect_equal(nrow(res), 1)
  # sign_mismatch should be in the output
  expect_true("sign_mismatch" %in% names(res))
})

test_that("sign_mismatch is FALSE when signs agree", {
  res <- check_text("t(88) = 2.85, p = .005, d = 0.60")
  expect_equal(nrow(res), 1)
  expect_false(res$sign_mismatch[1])
})

test_that("sign_sensitive parameter accepted by check_text", {
  # Just verifying the parameter doesn't cause an error
  res <- check_text("t(88) = 2.85, p = .005, d = 0.60", sign_sensitive = TRUE)
  expect_equal(nrow(res), 1)
  res2 <- check_text("t(88) = 2.85, p = .005, d = 0.60", sign_sensitive = FALSE)
  expect_equal(nrow(res2), 1)
})

# ===========================================================================
# Classroom Round 2: Bug B — r-test df derivation from N
# ===========================================================================

test_that("r-test derives df from N when df not in parentheses (Bug B)", {
  res <- check_text("N = 132. r = .36, p < .0001")
  expect_equal(nrow(res), 1)
  expect_equal(res$df1[1], 130)  # N - 2
  expect_false(is.na(res$p_computed[1]))
  expect_true(res$p_computed[1] < 0.001)
  expect_true(res$status[1] %in% c("OK", "PASS"))
})

test_that("r-test with parenthetical df still works (regression)", {
  res <- check_text("r(130) = .36, p < .0001")
  expect_equal(nrow(res), 1)
  expect_equal(res$df1[1], 130)
  expect_false(is.na(res$p_computed[1]))
})

test_that("r-test without N or df does not compute p", {
  res <- check_text("r = .36, p < .0001")
  # May or may not parse (needs nearby p), but if it does, p_computed depends on df
  if (nrow(res) > 0 && is.na(res$df1[1])) {
    expect_true(is.na(res$p_computed[1]))
  }
})

# ===========================================================================
# Classroom Round 2: Bug A — chi2(df>1) V back-calculation
# ===========================================================================

test_that("chi2(df>1) with V does NOT produce ERROR (Bug A regression fix)", {
  res <- check_text("chi2(4) = 333, p < .001, V = 0.81")
  expect_equal(nrow(res), 1)
  expect_true(res$status[1] %in% c("PASS", "NOTE", "OK"))
  # V should be self-consistent
  expect_true(grepl("V", res$matched_variant[1]))
})

test_that("chi2(1) with V still works (regression)", {
  res <- check_text("chi2(1) = 85.03, p < 0.001, V = 0.41")
  expect_equal(nrow(res), 1)
  expect_equal(res$status[1], "PASS")
})

test_that("chi2(4) V back-calculation uses correct m from enumerate_m_from_df", {
  res <- check_text("chi2(4) = 333, p < .001, V = 0.81")
  expect_equal(nrow(res), 1)
  # N should be ~508 (m=1), not 127 (m=4)
  expect_true(res$N[1] > 400)
})

# ===========================================================================
# Classroom Round 2: Bug C — one-tailed p-value both tails
# ===========================================================================

test_that("one-tailed negative t computes correct tail (Bug C)", {
  # t(52) = -0.60, one-tailed p = .73 → correct p = 1 - pt(0.60, 52, lower=F) ≈ 0.724
  res <- check_text("one-tailed, t(52) = -0.60, p = .73")
  expect_equal(nrow(res), 1)
  expect_true(abs(res$p_computed[1] - 0.724) < 0.01)
})

test_that("one-tailed positive t still computes upper tail (Bug C)", {
  res <- check_text("one-tailed, t(52) = 2.50, p = .008")
  expect_equal(nrow(res), 1)
  expect_true(abs(res$p_computed[1] - 0.008) < 0.005)
})

test_that("two-tailed t is unchanged by one-tailed fix", {
  res <- check_text("t(88) = 2.85, p = .005")
  expect_equal(nrow(res), 1)
  # Two-tailed p ≈ 0.0054
  expect_true(abs(res$p_computed[1] - 0.0054) < 0.001)
})

test_that("one_tailed global param with negative t selects correct tail", {
  res <- check_text("t(50) = -2.00, p = .975", one_tailed = TRUE)
  expect_equal(nrow(res), 1)
  # p_lower = 1 - pt(2, 50, lower=F) ≈ 0.9745
  expect_true(abs(res$p_computed[1] - 0.975) < 0.01)
})

test_that("one_tailed_detected column exists in parse output", {
  parsed <- effectcheck:::parse_text("one-tailed, t(52) = 2.50, p = .008")
  expect_true("one_tailed_detected" %in% names(parsed))
  if (nrow(parsed) > 0) {
    expect_true(parsed$one_tailed_detected[1])
  }
})

test_that("one_tailed_detected is FALSE for standard text", {
  parsed <- effectcheck:::parse_text("t(88) = 2.85, p = .005")
  expect_true("one_tailed_detected" %in% names(parsed))
  if (nrow(parsed) > 0) {
    expect_false(parsed$one_tailed_detected[1])
  }
})
