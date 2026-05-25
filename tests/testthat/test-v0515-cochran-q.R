# v0.5.15: Cochran Q meta-analytic heterogeneity test
# Q is chi-square(df) distributed under the homogeneity null. Parse and
# verify the reported p-value against pchisq(Q, df). No standard effect size.

test_that("v0.5.15: Cochran Q_T [df] = val form is parsed", {
  txt <- "(Q_T [40] = 104.65, p < .001, I^2 = 61.8%)."
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "cochran_q")
  expect_equal(res$df1, 40)
  expect_equal(res$stat_value, 104.65)
  expect_true(!is.na(res$p_computed))
  expect_lt(res$p_computed, 0.001)
})

test_that("v0.5.15: Cochran Q with parenthesized df parses", {
  txt <- "Q(12) = 25.4, p = .013"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "cochran_q")
  expect_equal(res$df1, 12)
  expect_equal(res$stat_value, 25.4)
})

test_that("v0.5.15: Cochran Q with subscript and brackets parses", {
  txt <- "Q_M [8] = 15.0, p = .059"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "cochran_q")
  expect_equal(res$df1, 8)
  expect_equal(res$stat_value, 15.0)
})

test_that("v0.5.15: Cochran Q p-value is chi-square-derived", {
  txt <- "Q_T [10] = 18.31, p = .050"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expected_p <- stats::pchisq(18.31, df = 10, lower.tail = FALSE)
  expect_equal(res$p_computed, expected_p, tolerance = 1e-6)
})

test_that("v0.5.15: Cochran Q uncertainty notes no recoverable effect size", {
  txt <- "(Q_T [40] = 104.65, p < .001, I^2 = 61.8%)."
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  # An uncertainty note should mention that no standard effect size is recoverable
  expect_true(grepl("no standard effect size", res$uncertainty_reasons %||% "",
                    ignore.case = TRUE) ||
              grepl("Cochran Q", res$uncertainty_reasons %||% ""))
})
