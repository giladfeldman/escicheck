# test-ci-coercion.R — Verify CI columns are always numeric (never list)
# Bug: bind_rows crashes when ciL_computed is a list from extreme NCP values

test_that("ciL_computed and ciU_computed are always numeric, even with extreme NCP", {
  # Large F with small df triggers extreme NCP in CI computation
  res <- check_text("F(1, 10) = 150.0, p < .001, eta2 = 0.94, 95% CI [0.85, 0.97]")
  expect_true(is.numeric(res$ciL_computed))
  expect_true(is.numeric(res$ciU_computed))
  # Should not be a list

  expect_false(is.list(res$ciL_computed))
  expect_false(is.list(res$ciU_computed))
})

test_that("CI columns are numeric for large t-test effect sizes", {
  # Large d value → large NCP for MBESS::ci.smd
  res <- check_text("t(8) = 12.0, p < .001, d = 4.24, 95% CI [2.50, 5.98]")
  expect_true(is.numeric(res$ciL_computed))
  expect_true(is.numeric(res$ciU_computed))
  expect_false(is.list(res$ciL_computed))
  expect_false(is.list(res$ciU_computed))
})

test_that("bind_rows succeeds with mixed normal and extreme NCP results", {
  # Simulate batch processing: one normal, one extreme
  res1 <- check_text("t(50) = 2.10, p = .041, d = 0.59, 95% CI [0.02, 1.16]")
  res2 <- check_text("F(1, 10) = 150.0, p < .001, eta2 = 0.94, 95% CI [0.85, 0.97]")
  # This is the operation that crashed before the fix
  combined <- dplyr::bind_rows(res1, res2)
  expect_equal(nrow(combined), 2)
  expect_true(is.numeric(combined$ciL_computed))
  expect_true(is.numeric(combined$ciU_computed))
})

test_that("CI columns are numeric when no CI is reported", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.83")
  expect_true(is.numeric(res$ciL_computed))
  expect_true(is.numeric(res$ciU_computed))
})
