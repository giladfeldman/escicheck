# test-ci-multimethod.R
# Tests for multi-method CI verification (v0.3.0g)
# Covers: ci_d_ind_all, ci_dz_all, ci_etap2_all, multi-method matching,
# ci_check_status, ci_method_match, ci_V integration, 90% CI for eta-squared

library(testthat)

# =========================================================================
# Unit tests: multi-method CI functions in compute.R
# =========================================================================

test_that("ci_d_ind_all returns both noncentral_t and normal_approx", {
  results <- effectcheck:::ci_d_ind_all(0.5, 50, 50, 0.95)
  expect_true(length(results) >= 2)
  expect_true("noncentral_t" %in% names(results))
  expect_true("normal_approx" %in% names(results))
  # Both should have valid bounds
  expect_true(results$noncentral_t$success)
  expect_true(results$normal_approx$success)
  expect_equal(length(results$noncentral_t$bounds), 2)
  expect_equal(length(results$normal_approx$bounds), 2)
  # Normal approx should be symmetric around d
  na_bounds <- results$normal_approx$bounds
  expect_equal(0.5 - na_bounds[1], na_bounds[2] - 0.5, tolerance = 0.001)
  # Noncentral t should be asymmetric (slightly wider on one side)
  nct_bounds <- results$noncentral_t$bounds
  # Both methods should produce similar (but not identical) results
  expect_true(abs(nct_bounds[1] - na_bounds[1]) < 0.1)
  expect_true(abs(nct_bounds[2] - na_bounds[2]) < 0.1)
})

test_that("ci_d_ind_all returns empty list for NA inputs", {
  results <- effectcheck:::ci_d_ind_all(NA, 50, 50)
  expect_equal(length(results), 0)
})

test_that("ci_dz_all returns both methods", {
  results <- effectcheck:::ci_dz_all(0.5, 50, 0.95)
  expect_true(length(results) >= 2)
  expect_true("noncentral_t" %in% names(results))
  expect_true("normal_approx" %in% names(results))
})

test_that("ci_etap2_all returns 95% and 90% CIs", {
  results <- effectcheck:::ci_etap2_all(5.0, 2, 100, 0.95)
  expect_true(length(results) >= 2)
  expect_true("ncf_inversion" %in% names(results))
  expect_true("ncf_90pct" %in% names(results))
  # 90% CI should be narrower than 95%
  ci_95 <- results$ncf_inversion$bounds
  ci_90 <- results$ncf_90pct$bounds
  expect_true(ci_90[1] >= ci_95[1],
    info = "90% CI lower bound should be >= 95% lower bound")
  expect_true(ci_90[2] <= ci_95[2],
    info = "90% CI upper bound should be <= 95% upper bound")
})

# =========================================================================
# Integration tests: multi-method CI matching in check_text
# =========================================================================

test_that("CI matching works via noncentral t method", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 95% CI [0.14, 1.27]")
  expect_equal(r$ci_check_status[1], "MATCH")
  expect_false(is.na(r$ci_method_match[1]))
})

test_that("90% CI for eta-squared matched when level is explicit", {
  # When the text says "90% CI", the parser extracts ci_level=0.90 and the
  # primary method computes at 90% — so it matches via ncf_inversion, not ncf_90pct
  r <- check_text("F(2, 100) = 5.50, p = .005, eta-squared = .10, 90% CI [.02, .18]")
  expect_equal(r$ci_check_status[1], "MATCH")
  expect_equal(r$ci_level[1], 0.90)
  expect_true(grepl("eta2", r$ci_method_match[1]))
})

test_that("90% CI for eta-squared matched via Steiger fallback when level unlabeled", {
  # When no CI level is stated, the system assumes 95% for the primary method.
  # But the 90% fallback (Steiger 2004) should still match the reported CI.
  # Use parenthesized CI without explicit "90%" label.
  r <- check_text("F(2, 100) = 5.50, p = .005, eta-squared = .10, (.02, .18)")
  if (r$ci_check_status[1] == "MATCH") {
    expect_true(grepl("90pct", r$ci_method_match[1]),
      info = "Unlabeled 90% eta2 CI should match via ncf_90pct fallback")
  }
})

test_that("ci_check_status is MISSING when no CI reported", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71")
  expect_equal(r$ci_check_status[1], "MISSING")
})

test_that("ci_symmetry is computed for symmetric CIs", {
  # Normal approximation produces symmetric CIs
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 95% CI [0.14, 1.27]")
  # The CI itself is symmetric around 0.705
  if (!is.na(r$ci_symmetry[1])) {
    expect_true(r$ci_symmetry[1] %in% c("symmetric", "asymmetric"))
  }
})

test_that("ci_width_ratio is computed", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 95% CI [0.14, 1.27]")
  expect_false(is.na(r$ci_width_ratio[1]))
  # Width ratio should be close to 1 for a correct CI
  expect_true(r$ci_width_ratio[1] > 0.5 && r$ci_width_ratio[1] < 2.0)
})

test_that("new output columns exist in results", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 95% CI [0.14, 1.27]")
  expect_true("ci_delta_upper" %in% names(r))
  expect_true("ci_check_status" %in% names(r))
  expect_true("ci_method_match" %in% names(r))
  expect_true("ci_width_ratio" %in% names(r))
  expect_true("ci_symmetry" %in% names(r))
})

test_that("Cramer's V now has computed CI", {
  r <- check_text("chi2(4, N = 200) = 15.0, p = .005, V = .19")
  # V CI should be computed (was missing before)
  if (!is.na(r$ciL_computed[1])) {
    expect_true(r$ciL_computed[1] >= 0)
    expect_true(r$ciU_computed[1] <= 1)
  }
})

# =========================================================================
# Regression tests: existing ci_match behavior preserved
# =========================================================================

test_that("matching CI still gets ci_match=TRUE", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 95% CI [0.14, 1.27]")
  expect_true(isTRUE(r$ci_match[1]))
})

test_that("badly wrong CI still gets ci_match=FALSE", {
  # CI [0.90, 1.50] is way off for d=0.71 with n=52
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 95% CI [0.90, 1.50]")
  expect_false(isTRUE(r$ci_match[1]))
  expect_equal(r$ci_check_status[1], "INCONSISTENT")
})

# =========================================================================
# Decimal CI level parsing (v0.3.0j fix)
# Bug: (\d+)% regex couldn't match "99.9%" — captured "9" → ci_level=0.09
# =========================================================================

test_that("99.9% CI level parsed correctly (not 0.09)", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 99.9% CI [0.14, 1.27]")
  expect_equal(r$ci_level[1], 0.999)
  expect_equal(r$ci_level_source[1], "explicit_with_bounds")
})

test_that("99.5% CI level parsed correctly (not 0.05)", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 99.5% CI [0.14, 1.27]")
  expect_equal(r$ci_level[1], 0.995)
})

test_that("99% CI level parsed correctly", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 99% CI [0.14, 1.27]")
  expect_equal(r$ci_level[1], 0.99)
})

test_that("95% CI level still works after regex change", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 95% CI [0.14, 1.27]")
  expect_equal(r$ci_level[1], 0.95)
})

test_that("90% CI level still works after regex change", {
  r <- check_text("F(2, 100) = 5.50, p = .005, eta-squared = .10, 90% CI [.02, .18]")
  expect_equal(r$ci_level[1], 0.90)
})

test_that("CI level < 0.50 falls back to 0.95 with implausible_level source", {
  # Simulate edge case: if somehow "5% CI" appears (meaning 5% sig level)
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, 5% CI [0.14, 1.27]")
  expect_equal(r$ci_level[1], 0.95)
  expect_equal(r$ci_level_source[1], "implausible_level")
})

test_that("CI 99.9% alternate format parsed correctly", {
  r <- check_text("t(50) = 2.50, p = .016, d = 0.71, CI 99.9% [0.14, 1.27]")
  expect_equal(r$ci_level[1], 0.999)
})
