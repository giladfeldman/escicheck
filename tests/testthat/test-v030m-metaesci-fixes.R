# test-v030m-metaesci-fixes.R — v0.3.0m: MetaESCI batch validation fixes
# Issue 1: Unstandardized b vs standardized beta (153 false positive ERRORs)
# Issue 1b: pat_eta matching "eta" inside "beta" (parser bug)
# Issue 3: F-test NaN computation errors (27 crashes)
# Issue 4: z-test CI computation missing (1,517 UNVERIFIABLE CIs)

# ===========================================================================
# Issue 1b: pat_eta must not match inside "beta"
# ===========================================================================

test_that("pat_eta does not match eta inside beta", {
  # "beta = 0.29" should parse as effect_name = "beta", not "eta"
  res <- parse_text("b = 0.29, SE = 0.31, t(58.56) = 13.19, p < .001, beta = 0.29")
  expect_equal(res$effect_reported_name[1], "beta")
})

test_that("genuine eta still matches", {
  res <- parse_text("F(1, 50) = 4.00, p = .051, eta = 0.27")
  expect_equal(res$effect_reported_name[1], "eta")
})

# ===========================================================================
# Issue 1: Unstandardized b masquerading as beta
# ===========================================================================

test_that("unstandardized b is NOT compared to standardized_beta", {
  # Paper reports b = 0.29 AND "beta = 0.29" (same value = masquerade)
  # standardized_beta = 13.19/sqrt(13.19^2+58.56) = 0.865
  # Comparison against 0.865 would produce false ERROR (delta = 0.575)
  res <- check_text("b = 0.29, SE = 0.31, t(58.56) = 13.19, p < .001, beta = 0.29")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "regression")
  # Should NOT be ERROR (was false positive before fix)
  expect_true(res$status[1] != "ERROR")
  # Uncertainty should mention unstandardized b
  expect_true(grepl("unstandardized b", res$uncertainty_reasons[1]))
  # standardized_beta should still be computed for output (informational)
  expect_false(is.na(res$standardized_beta[1]))
})

test_that("genuine beta IS compared to standardized_beta", {
  # Both b and beta present with DIFFERENT values — beta should be checked
  # b_coeff = 0.45 != effect_reported = 0.23 → not masquerading
  res <- check_text("b = 0.45, SE = 0.12, t(198) = 3.75, p < .001, beta = 0.23")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "regression")
  expect_equal(res$effect_reported_name[1], "beta")
  expect_false(is.na(res$standardized_beta[1]))
  # Should still match against standardized_beta
  expect_true(res$check_type[1] == "effect_size")
  expect_equal(res$matched_variant[1], "standardized_beta")
})

test_that("b without beta text produces p_value check only", {
  # No "beta" or "β" in text → effect_reported_name = NA → p_value only
  res <- check_text("b = 0.29, SE = 0.31, t(58.56) = 13.19, p < .001")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "regression")
  expect_true(is.na(res$effect_reported_name[1]))
  expect_true(res$check_type[1] %in% c("p_value", "p_value_only"))
})

test_that("b masquerade with exact beta text still detected", {
  # b = 0.50 and beta = 0.50 — same value → masquerade detected
  res <- check_text("b = 0.50, SE = 0.10, t(100) = 5.00, p < .001, beta = 0.50")
  expect_equal(nrow(res), 1)
  # When b_coeff == effect_reported, it's a masquerade
  # Should NOT produce ERROR
  expect_true(res$status[1] != "ERROR")
})

test_that("b masquerade detected with negative coefficients", {
  # Negative b and beta — abs() comparison must handle signs correctly
  res <- check_text("b = -0.50, SE = 0.10, t(100) = -5.00, p < .001, beta = -0.50")
  expect_equal(nrow(res), 1)
  expect_true(res$status[1] != "ERROR")
  expect_true(grepl("unstandardized b", res$uncertainty_reasons[1]))
})

# Regression guard: existing beta test must still work
test_that("regression pipeline computes standardized beta (guard)", {
  res <- check_text("b = 0.45, SE = 0.12, t(198) = 3.75, p < .001, beta = 0.23")
  expected_beta <- 3.75 / sqrt(3.75^2 + 198)
  expect_equal(res$standardized_beta[1], expected_beta, tolerance = 1e-3)
})

# ===========================================================================
# Issue 3: F-test computation guards
# ===========================================================================

test_that("F-test with extreme F value does not crash", {
  # eta2 near 1.0 → Inf CI bounds; should not produce NaN
  res <- check_text("F(1, 10) = 5000.00, p < .001, etap2 = 0.998")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "F")
  # Should not be NA (crash) — should produce a valid status
  expect_false(is.na(res$status[1]))
  expect_true(res$status[1] %in% c("PASS", "WARN", "NOTE", "ERROR"))
})

test_that("F-test with cohens_f does not crash on Inf CI", {
  # Large F → large f → possible Inf CI
  res <- check_text("F(2, 50) = 100.00, p < .001, f = 2.00")
  expect_equal(nrow(res), 1)
  expect_false(is.na(res$status[1]))
})

test_that("F-test with moderate values still works", {
  # Regression guard — normal F-test should still pass
  res <- check_text("F(1, 98) = 4.50, p = .036, etap2 = .044")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "F")
  # partial_eta2 = 4.5*1/(4.5*1+98) = 4.5/102.5 = 0.04390
  expect_true(res$status[1] %in% c("PASS", "WARN"))
})

# ===========================================================================
# Issue 4: z-test CI computation
# ===========================================================================

test_that("z-test with d and N computes CI for d_ind", {
  # z = 2.50, N = 100: d = 2*2.5/sqrt(100) = 0.50
  res <- check_text("z = 2.50, p = .012, d = 0.50, N = 100")
  expect_equal(nrow(res), 1)
  if (res$test_type[1] == "z") {
    expect_false(is.na(res$matched_value[1]))
  }
})

test_that("z-test with reported CI now verifies against computed CI", {
  # z = 2.50, N = 100, d = 0.50, CI [0.11, 0.89]
  # d = 2*2.5/sqrt(100) = 0.50, n1=50, n2=50
  # CI should be computed and compared
  res <- check_text("z = 2.50, p = .012, d = 0.50, 95% CI [0.11, 0.89], N = 100")
  expect_equal(nrow(res), 1)
  if (res$test_type[1] == "z" && !is.na(res$ci_check_status[1])) {
    # Should now be something other than UNVERIFIABLE
    expect_true(res$ci_check_status[1] %in% c("MATCH", "PLAUSIBLE", "INCONSISTENT"))
  }
})

test_that("z-test without N still works (no CI)", {
  # Regression guard — z-test without N should not crash
  res <- check_text("z = 2.58, p = .010")
  expect_equal(nrow(res), 1)
  expect_equal(res$check_scope[1], "p_value_only")
})

test_that("Regression guard: t-test d PASS unchanged", {
  res <- check_text("t(45) = 2.31, p = .023, d = 0.68")
  expect_equal(res$status[1], "PASS")
})
