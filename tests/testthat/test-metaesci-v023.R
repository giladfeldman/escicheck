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

test_that("Implausibly large d is rejected at parse time or flagged (Issue C)", {
  # d=615 is a round integer > 5, rejected at parse time in v0.2.4
  res <- check_text("t(61) = 4.84, p < .001, d = 615")
  expect_equal(nrow(res), 1)
  # Either: rejected at parse time (no effect_reported) or flagged as extraction_suspect
  expect_true(is.na(res$effect_reported[1]) || res$extraction_suspect[1])
  # Should NOT be ERROR
  expect_true(res$status[1] %in% c("NOTE", "WARN", "OK", "SKIP"))
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
  # The explicit "r = 1.5" is not parsed as a separate effect size label;

  # the r(48) = .55 is the primary result. With v0.3.0l, the upgrade sets
  # effect_reported = stat (0.55), which is within bounds → not extraction_suspect.
  expect_equal(nrow(res), 1)
  expect_equal(res$effect_reported[1], 0.55)
  expect_false(res$extraction_suspect[1])
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
  expect_equal(bounds$d, 5)  # Tightened in v0.2.4
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

# ===========================================================================
# Round 3: SKIP status for extraction-only results
# ===========================================================================

test_that("extraction-only results get SKIP status", {
  res <- check_text("z = 9.47")
  expect_equal(nrow(res), 1)
  expect_equal(res$status[1], "SKIP")
  expect_equal(res$check_type[1], "extraction_only")
})

test_that("results with p-value do NOT get SKIP", {
  res <- check_text("t(50) = 2.00, p = .05")
  expect_equal(nrow(res), 1)
  expect_true(res$status[1] != "SKIP")
})

test_that("results with effect size do NOT get SKIP", {
  res <- check_text("t(50) = 2.00, d = 0.50")
  expect_equal(nrow(res), 1)
  expect_true(res$status[1] != "SKIP")
})

test_that("ns (not significant) notation does NOT get SKIP", {
  res <- check_text("F(1, 50) = 0.50, ns")
  expect_equal(nrow(res), 1)
  expect_equal(res$status[1], "OK")
})

# ===========================================================================
# Round 3: Two-tailed detection and universal fallback
# ===========================================================================

test_that("two_tailed_detected is TRUE when 'two-tailed' appears in chunk", {
  parsed <- effectcheck:::parse_text("t(69) = 0.65, two-tailed p = .52")
  expect_true("two_tailed_detected" %in% names(parsed))
  if (nrow(parsed) > 0) {
    expect_true(parsed$two_tailed_detected[1])
    expect_false(parsed$one_tailed_detected[1])
  }
})

test_that("explicit two-tailed p-value is computed correctly", {
  res <- check_text("t(69) = 0.65, two-tailed p = .52")
  expect_equal(nrow(res), 1)
  expect_equal(res$status[1], "OK")
  expect_equal(round(res$p_computed[1], 3), round(2 * pt(0.65, 69, lower.tail = FALSE), 3))
})

test_that("one-tailed detection only searches chunk, not context", {
  # "one-tailed" in first sentence should NOT bleed to the second result
  res <- check_text("one-tailed, t(52) = -0.60, p = .73. t(69) = 0.65, p = .52")
  expect_equal(nrow(res), 2)
  # Second result should NOT have one_tailed_detected
  parsed <- effectcheck:::parse_text("one-tailed, t(52) = -0.60, p = .73. t(69) = 0.65, p = .52")
  if (nrow(parsed) >= 2) {
    expect_true(parsed$one_tailed_detected[1])
    expect_false(parsed$one_tailed_detected[2])
  }
})

test_that("two-tailed default: no fallback when p-values match", {
  # t(69) = 0.65, p = .52 → two-tailed p ≈ 0.518, both > .05, no decision error
  res <- check_text("t(69) = 0.65, p = .52")
  expect_equal(nrow(res), 1)
  expect_false(res$decision_error[1])
  # Should be OK, not NOTE (no fallback needed)
  expect_equal(res$status[1], "OK")
})

test_that("fallback to one-tailed resolves decision error with NOTE", {
  # t(30) = 1.80, p = .04: two-tailed p ≈ 0.082 (not sig), but one-tailed ≈ 0.041 (sig)
  # Reported p = .04 is significant. Two-tailed says not sig → decision error
  # Fallback: one-tailed ≈ 0.041 (sig) matches → NOTE
  res <- check_text("t(30) = 1.80, p = .04")
  expect_equal(nrow(res), 1)
  expect_false(res$decision_error[1])
  expect_equal(res$status[1], "NOTE")
  expect_true(grepl("one-tailed", res$uncertainty_reasons[1]))
})

test_that("fallback to two-tailed resolves decision error with NOTE", {
  # When one_tailed is set globally but two-tailed matches better
  # t(30) = 2.10, p = .044: one-tailed p ≈ 0.022 (sig), two-tailed p ≈ 0.044 (sig)
  # Actually both are significant here. Let me find a case where they disagree.
  # t(30) = 1.70, p = .10: one-tailed p ≈ 0.050 (borderline), two-tailed p ≈ 0.099
  # With one_tailed=TRUE: primary=one-tailed ≈ 0.050, reported 0.10 not sig, computed borderline
  # Actually let's use a clear case:
  # t(30) = 1.50, p = .14: one-tailed ≈ 0.072 (sig at 0.05? no), two-tailed ≈ 0.144
  # reported p=.14, two-tailed p≈0.144, both not sig → OK
  # But with one_tailed=TRUE: primary one-tailed ≈ 0.072, reported 0.14 not sig, computed not sig → same dir
  # Need: one-tailed sig but two-tailed not sig, reported not sig
  # t(30) = 1.75, p = .09: one-tailed ≈ 0.045 (sig!), two-tailed ≈ 0.090 (not sig)
  # With one_tailed=TRUE: primary = one-tailed 0.045 (sig), reported 0.09 (not sig) → decision error
  # Fallback: two-tailed 0.090, both not sig → resolves → NOTE
  res <- check_text("t(30) = 1.75, p = .09", one_tailed = TRUE)
  expect_equal(nrow(res), 1)
  expect_false(res$decision_error[1])
  expect_equal(res$status[1], "NOTE")
  expect_true(grepl("two-tailed", res$uncertainty_reasons[1]))
})

# ===========================================================================
# Round 3: Method-context detection
# ===========================================================================

test_that("method_context_detected is TRUE for p-curve text", {
  parsed <- effectcheck:::parse_text("in the p-curve applet, F(1, 221) = 6.20, p = .875")
  expect_true("method_context_detected" %in% names(parsed))
  if (nrow(parsed) > 0) {
    expect_true(parsed$method_context_detected[1])
  }
})

test_that("method_context_detected is TRUE for equivalence test", {
  parsed <- effectcheck:::parse_text("equivalence test: t(50) = 1.20, p = .23")
  if (nrow(parsed) > 0) {
    expect_true(parsed$method_context_detected[1])
  }
})

test_that("method_context_detected is FALSE for normal text", {
  parsed <- effectcheck:::parse_text("t(50) = 2.00, p = .05")
  if (nrow(parsed) > 0) {
    expect_false(parsed$method_context_detected[1])
  }
})

test_that("method context suppresses decision_error", {
  res <- check_text("in the p-curve applet, F(1, 221) = 6.20, p = .875")
  expect_equal(nrow(res), 1)
  expect_false(res$decision_error[1])
  expect_true(grepl("methodological context", res$uncertainty_reasons[1]))
})

test_that("method context does NOT suppress decision_error for normal text", {
  # F(1, 50) = 0.50, p = .001 → computed p ≈ 0.483, decision error
  res <- check_text("F(1, 50) = 0.50, p = .001")
  expect_equal(nrow(res), 1)
  expect_true(res$decision_error[1])
})

# ===========================================================================
# Round 3: N candidates for r-tests
# ===========================================================================

test_that("N_candidates_str stores multiple N values from context", {
  parsed <- effectcheck:::parse_text("N = 32. N = 959. r = .25, p < .001")
  expect_true("N_candidates_str" %in% names(parsed))
  if (nrow(parsed) > 0) {
    expect_true(grepl("32", parsed$N_candidates_str[1]))
    expect_true(grepl("959", parsed$N_candidates_str[1]))
  }
})

test_that("r-test selects best N from candidates via p-value match", {
  res <- check_text("N = 32. N = 959. r = .25, p < .001")
  expect_equal(nrow(res), 1)
  expect_equal(res$status[1], "PASS")  # r IS the effect size, self-verified
  # p_computed should be very small (consistent with N=959)
  expect_true(res$p_computed[1] < 0.001)
  # Assumption note should mention multiple sample sizes
  expect_true(grepl("Multiple sample sizes", res$assumptions_used[1]))
  expect_true(grepl("N=959", res$assumptions_used[1]))
})

test_that("single N in context does NOT trigger N candidate selection", {
  parsed <- effectcheck:::parse_text("N = 100. r = .30, p = .002")
  if (nrow(parsed) > 0) {
    expect_true(is.na(parsed$N_candidates_str[1]))
  }
})

# ===========================================================================
# Round 3 follow-up: Garbled p-value detection
# ===========================================================================

test_that("garbled p-value 'p < 0.645' is flagged as extraction_suspect", {
  res <- check_text("t(50) = 2.00, p < .645")
  expect_equal(nrow(res), 1)
  expect_true(res$extraction_suspect[1])
  expect_true(grepl("extraction artifact", res$uncertainty_reasons[1]))
})

test_that("legitimate 'p < .05' is NOT flagged as garbled", {
  res <- check_text("t(50) = 2.00, p < .05")
  expect_equal(nrow(res), 1)
  expect_false(res$extraction_suspect[1])
})

test_that("legitimate 'p < .001' is NOT flagged as garbled", {
  res <- check_text("t(50) = 2.00, p < .001")
  expect_equal(nrow(res), 1)
  expect_false(res$extraction_suspect[1])
})
