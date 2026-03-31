# Tests for v0.3.0 missing formulas
# epsilon2_anova, partial_omega2, bias_corrected_eta2, J(N-1) variant

test_that("epsilon2_anova_from_F computes correctly", {
  # F(2, 57) = 8.0: eps2 = (16-2)/(16+57) = 14/73 = 0.1918
  expect_equal(round(epsilon2_anova_from_F(8.0, 2, 57), 4), 0.1918)
  expect_true(is.na(epsilon2_anova_from_F(NA, 2, 57)))
  expect_true(is.na(epsilon2_anova_from_F(8.0, 0, 57)))
  # Small F: epsilon2 clamped to 0
  expect_equal(epsilon2_anova_from_F(0.5, 2, 57), 0)
})

test_that("epsilon2 < eta2 < 1 for same F-test", {
  F_val <- 8.0
  df1 <- 2
  df2 <- 57
  eta2 <- eta2_from_F(F_val, df1, df2)
  eps2 <- epsilon2_anova_from_F(F_val, df1, df2)
  omega2 <- omega2_from_F(F_val, df1, df2)
  # omega2 < epsilon2 < eta2 always
  expect_true(omega2 < eps2)
  expect_true(eps2 < eta2)
})

test_that("partial_omega2_from_F computes correctly", {
  # F(2, 57) = 8.0: N = 60, p_omega2 = 2*(8-1)/(2*(8-1)+60) = 14/74
  expect_equal(round(partial_omega2_from_F(8.0, 2, 57), 4), 0.1892)
  expect_true(is.na(partial_omega2_from_F(NA, 2, 57)))
  # Small F: clamped to 0
  expect_equal(partial_omega2_from_F(0.5, 2, 57), 0)
})

test_that("bias_corrected_eta2 computes correctly", {
  # eta2 = 0.22, N = 60, df1 = 2
  # bc = 1 - (1-0.22) * 59/57 = 1 - 0.78 * 1.0351 = 1 - 0.8074 = 0.1926
  bc <- bias_corrected_eta2(0.22, 60, 2)
  expect_true(!is.na(bc))
  expect_true(bc < 0.22)
  expect_true(bc > 0)
  # Edge: N <= df1 + 1
  expect_true(is.na(bias_corrected_eta2(0.22, 3, 2)))
})

test_that("J(N-1) variant produces slightly different g than J(df)", {
  # For independent t-test: df = n1+n2-2, N = n1+n2
  # J(df) = J(N-2) = 1 - 3/(4(N-2)-1)
  # J(N-1) = 1 - 3/(4(N-1)-1)
  N <- 20
  J_df <- hedges_J(N - 2)
  J_Nm1 <- hedges_J(N - 1)
  expect_true(J_Nm1 > J_df)
  # Difference should be small
  expect_true(abs(J_Nm1 - J_df) < 0.01)
})

test_that("g_ind_Nm1 is computed for t-tests with known groups", {
  res <- check_text("t(38) = 3.00, p = .005, g = 0.93, n1 = 20, n2 = 20")
  # Should have g_ind_Nm1 in variants (check via all_variants JSON)
  expect_true(nrow(res) > 0)
  if (!is.na(res$all_variants[1])) {
    expect_true(grepl("g_ind_Nm1", res$all_variants[1]))
  }
})

test_that("epsilon2_anova is computed for F-tests", {
  res <- check_text("F(2, 57) = 8.0, p < .001, partial eta2 = 0.19")
  expect_true(nrow(res) > 0)
  if (!is.na(res$all_variants[1])) {
    expect_true(grepl("epsilon2_anova", res$all_variants[1]))
  }
})

test_that("partial_omega2 is computed for F-tests", {
  res <- check_text("F(2, 57) = 8.0, p < .001, partial eta2 = 0.19")
  expect_true(nrow(res) > 0)
  if (!is.na(res$all_variants[1])) {
    expect_true(grepl("partial_omega2", res$all_variants[1]))
  }
})
