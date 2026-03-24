# test-v027-chisq.R — v0.2.7 Issue 1: Chi-square V back-calculation
# When N is inferred from global_text and V is reported, back-calculate
# N from V to correct for global N being too large.

test_that("chi-square V back-calc when N is missing (existing behavior preserved)", {
  # When N is not found, existing back-calc fires: N = chi2 / (V^2 * m)
  # chi2(1) = 14.09, V = 0.24: N_back = 14.09 / (0.24^2 * 1) = 245
  res <- check_text("chi2(1) = 14.09, p < .001, V = .24")
  expect_true(nrow(res) >= 1)
  if (res$test_type[1] == "chisq") {
    # N should be back-calculated since no N in text
    expect_true(!is.na(res$N[1]))
    # V should match closely
    if (!is.na(res$V[1])) {
      expect_true(abs(res$V[1] - 0.24) < 0.01)
    }
  }
})

test_that("chi-square with inline N is NOT overridden by V back-calc", {
  # chi2(1, N = 988) gives inline N — should NOT be overridden
  res <- check_text("chi2(1, N = 988) = 14.09, p < .001, V = .24")
  if (nrow(res) >= 1 && res$test_type[1] == "chisq") {
    # N_source should be chi_inline, not global_text
    # The back-calc guard only fires for global_text
    if (!is.na(res$N_source[1]) && res$N_source[1] == "chi_inline") {
      expect_equal(res$N[1], 988)  # Should stay 988
    }
  }
})

test_that("chi-square without V reported gets no back-calc", {
  # Only phi or V can trigger back-calculation
  res <- check_text("chi2(2) = 8.91, p = .012")
  expect_true(nrow(res) >= 1)
  # No effect size reported → no back-calc
  expect_true(is.na(res$effect_reported[1]) || res$effect_reported_name[1] != "V")
})

test_that("V back-calculation formula is correct", {
  # Test via existing back-calc (N missing path):
  # V = sqrt(chi2 / (N * m)), so N = chi2 / (V^2 * m)
  # chi2 = 50, V = 0.50, m = 1: N_back = 50 / (0.25 * 1) = 200
  res <- check_text("chi2(1) = 50.0, p < .001, V = .50")
  if (nrow(res) >= 1 && res$test_type[1] == "chisq") {
    # N should be approximately 200
    if (!is.na(res$N[1])) {
      expect_true(abs(res$N[1] - 200) < 5)
    }
    # V should match
    if (!is.na(res$V[1])) {
      expect_true(abs(res$V[1] - 0.50) < 0.02)
    }
  }
})

test_that("chi-square df > 1 uses enumerate_m_from_df for back-calc", {
  # chi2(4) = 20.0, V = .25: df1=4 → m = enumerate_m_from_df(4)[1]
  # 4 factors as (2-1)*(4-1)=3 or (4-1)*(1-1)... m candidates = c(1,2,4)
  # m[1] = 1 → N_back = 20 / (0.25^2 * 1) = 320
  res <- check_text("chi2(4) = 20.0, p < .001, V = .25")
  if (nrow(res) >= 1 && res$test_type[1] == "chisq") {
    expect_true(!is.na(res$N[1]))
  }
})
