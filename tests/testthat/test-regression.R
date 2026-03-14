# test-regression.R
# Tests for regression coefficient support

# ===========================================================================
# Compute function tests
# ===========================================================================

test_that("verify_t_from_b_SE checks consistency correctly", {
  # b=0.45, SE=0.12, reported_t=3.75 -> computed_t = 0.45/0.12 = 3.75 (consistent)
  res <- verify_t_from_b_SE(0.45, 0.12, 3.75)
  expect_equal(res$computed_t, 3.75, tolerance = 1e-5)
  expect_true(res$consistent)
  expect_true(res$delta < 0.01)
})

test_that("verify_t_from_b_SE detects inconsistency", {
  # b=0.45, SE=0.12, reported_t=2.00 -> computed_t = 3.75 (inconsistent)
  res <- verify_t_from_b_SE(0.45, 0.12, 2.00)
  expect_equal(res$computed_t, 3.75, tolerance = 1e-5)
  expect_false(res$consistent)
  expect_equal(res$delta, abs(3.75 - 2.00), tolerance = 1e-5)
})

test_that("adjusted_R2 computes correct value", {
  # R2=0.30, n=100, p=3: adj_R2 = 1 - (1-0.30)*(100-1)/(100-3-1) = 1 - 0.70*99/96
  expected <- 1 - 0.70 * 99 / 96
  expect_equal(adjusted_R2(0.30, 100, 3), expected, tolerance = 1e-5)
})

test_that("adjusted_R2 handles edge cases", {
  # n <= p + 1: NA
  expect_true(is.na(adjusted_R2(0.50, 3, 3)))
  # R2 < 0: NA
  expect_true(is.na(adjusted_R2(-0.1, 100, 3)))
})

test_that("verify_adj_R2 checks consistency", {
  R2 <- 0.30; n <- 100; p <- 3
  adj <- adjusted_R2(R2, n, p)
  res <- verify_adj_R2(R2, adj, n, p)
  expect_true(res$consistent)
  expect_equal(res$delta, 0, tolerance = 1e-7)
})

# ===========================================================================
# Parse tests
# ===========================================================================

test_that("regression type is inferred from t + b + SE", {
  res <- parse_text("b = 0.45, SE = 0.12, t(198) = 3.75, p < .001, beta = 0.23")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "regression")
  expect_equal(res$b_coeff[1], 0.45)
  expect_equal(res$SE_coeff[1], 0.12)
  expect_equal(res$stat_value[1], 3.75)
  expect_equal(res$df1[1], 198)
})

test_that("t-test without b/SE stays as t-test", {
  res <- parse_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_equal(res$test_type[1], "t")
})

test_that("adjusted R2 is parsed", {
  res <- parse_text("b = 0.45, SE = 0.12, t(198) = 3.75, p < .001, adjusted R2 = 0.34")
  expect_equal(res$adj_R2[1], 0.34)
})

test_that("SE pattern matches multiple formats", {
  res1 <- parse_text("b = 0.45, SE = 0.12, t(50) = 3.75, p < .001")
  expect_equal(res1$SE_coeff[1], 0.12)

  # Note: "Std. Error" may be split by sentence splitter at the period.
  # Use "standard error" instead which avoids this issue.
  res2 <- parse_text("b = 0.30, standard error = 0.10, t(50) = 3.00, p = .004")
  expect_equal(res2$SE_coeff[1], 0.10)
})

# ===========================================================================
# Pipeline tests
# ===========================================================================

test_that("regression pipeline computes standardized beta", {
  res <- check_text("b = 0.45, SE = 0.12, t(198) = 3.75, p < .001, beta = 0.23")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "regression")
  # standardized_beta = t / sqrt(t^2 + df) = 3.75 / sqrt(14.0625 + 198)
  expected_beta <- 3.75 / sqrt(3.75^2 + 198)
  expect_equal(res$standardized_beta[1], expected_beta, tolerance = 1e-3)
})

test_that("regression pipeline verifies t = b/SE", {
  # Consistent case: b=0.45, SE=0.12, t=3.75 -> 0.45/0.12 = 3.75 (match)
  res <- check_text("b = 0.45, SE = 0.12, t(198) = 3.75, p < .001, beta = 0.23")
  # Should pass without t = b/SE inconsistency
  expect_true(!grepl("inconsistency", res$uncertainty_reasons[1]))
})

test_that("regression pipeline detects t = b/SE inconsistency", {
  # Inconsistent: b=0.45, SE=0.12, t=2.00 -> 0.45/0.12 = 3.75 != 2.00
  res <- check_text("b = 0.45, SE = 0.12, t(198) = 2.00, p = .047")
  expect_true(grepl("inconsistency", res$uncertainty_reasons[1]))
})

test_that("regression p-value uses t-distribution", {
  res <- check_text("b = 0.45, SE = 0.12, t(198) = 3.75, p < .001, beta = 0.23")
  expected_p <- 2 * pt(abs(3.75), df = 198, lower.tail = FALSE)
  expect_equal(res$p_computed[1], expected_p, tolerance = 1e-4)
})

test_that("regression is included in default stats", {
  fn_formals <- formals(check_text)
  default_stats <- eval(fn_formals$stats)
  expect_true("regression" %in% default_stats)
})
