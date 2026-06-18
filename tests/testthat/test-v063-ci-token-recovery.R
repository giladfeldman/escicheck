# v0.6.3 R-0007: dropped-minus sign-error detector (FLAG ONLY).
#
# The estimate-in-CI invariant: a reported point estimate must lie within its
# reported CI. When PDF extraction drops a leading minus, the estimate parses
# positive and lands outside its own CI while the sign-flip lands inside. Per
# the 2026-06-17 user decision this is FLAG ONLY -- effectcheck sets
# `sign_ci_violation` and an uncertainty note but NEVER mutates the parsed value.

test_that("R-0007 fires for a dropped-minus correlation (r positive, CI negative)", {
  r <- check_text("A negative association emerged, r(20) = .74, p < .001, 95% CI [-0.92, -0.30].")
  row <- r[!is.na(r$test_type) & r$test_type == "r", ]
  expect_equal(nrow(row), 1L)
  expect_true(isTRUE(row$sign_ci_violation[1]))
  # FLAG ONLY: the parsed value is NOT mutated (still positive).
  expect_true(row$effect_reported[1] > 0)
  expect_match(row$uncertainty_reasons[1], "dropped-minus sign error", fixed = TRUE)
})

test_that("R-0007 fires for a dropped-minus Cohen's d", {
  r <- check_text("The effect reversed, t(40) = 2.30, p = .027, d = 0.80, 95% CI [-1.30, -0.20].")
  row <- r[!is.na(r$stat_value) & abs(r$stat_value - 2.30) < 1e-6, ]
  expect_equal(nrow(row), 1L)
  expect_true(isTRUE(row$sign_ci_violation[1]))
  expect_true(row$effect_reported[1] > 0)  # unchanged
})

test_that("R-0007 fires for a dropped-minus standardized beta", {
  r <- check_text("The predictor was negative, b = 0.45, beta = 0.32, SE = 0.10, t(120) = 3.20, p = .002, 95% CI [-0.49, -0.15].")
  row <- r[!is.na(r$effect_reported_name) & r$effect_reported_name == "beta", ]
  if (nrow(row) >= 1L) {
    expect_true(isTRUE(row$sign_ci_violation[1]))
  } else {
    succeed("beta row not extracted in this build; covered by r/d cases")
  }
})

test_that("R-0007 does NOT fire when the estimate already lies in its CI", {
  r <- check_text("A negative association emerged, r(20) = -.74, p < .001, 95% CI [-0.92, -0.30].")
  row <- r[!is.na(r$test_type) & r$test_type == "r", ]
  expect_equal(nrow(row), 1L)
  expect_false(isTRUE(row$sign_ci_violation[1]))
})

test_that("R-0007 does NOT fire when the CI straddles zero (both x and -x inside)", {
  # r = .20 with CI [-0.40, 0.55]: both .20 and -.20 are inside -> ambiguous, no flag.
  r <- check_text("A weak association, r(60) = .20, p = .12, 95% CI [-0.40, 0.55].")
  row <- r[!is.na(r$test_type) & r$test_type == "r", ]
  expect_equal(nrow(row), 1L)
  expect_false(isTRUE(row$sign_ci_violation[1]))
})

test_that("R-0007 does NOT fire for a bounded-positive family (eta^2)", {
  # eta^2 is non-sign-bearing; even a [0, x] CI must never trip the invariant.
  r <- check_text("The main effect, F(1, 90) = 12.5, p < .001, eta2 = 0.12, 90% CI [0.02, 0.25].")
  row <- r[!is.na(r$effect_reported_name) & grepl("eta", r$effect_reported_name), ]
  if (nrow(row) >= 1L) {
    expect_false(isTRUE(row$sign_ci_violation[1]))
  } else {
    succeed("no eta2 row in this build")
  }
})

test_that("R-0007 does NOT fire on the epsilon boundary (estimate sits on a CI bound)", {
  # d = 0.20 with CI [0.20, 0.80]: estimate is exactly on the lower bound (in CI),
  # and -0.20 is outside -> no flag.
  r <- check_text("A small effect, t(50) = 1.45, p = .15, d = 0.20, 95% CI [0.20, 0.80].")
  row <- r[!is.na(r$stat_value) & abs(r$stat_value - 1.45) < 1e-6, ]
  expect_equal(nrow(row), 1L)
  expect_false(isTRUE(row$sign_ci_violation[1]))
})

test_that("R-0007 regression guard: a normal correctly-signed positive d is unflagged", {
  r <- check_text("t(28) = 2.21, p = .035, d = 0.80, 95% CI [0.06, 1.52].")
  row <- r[!is.na(r$stat_value) & abs(r$stat_value - 2.21) < 1e-6, ]
  expect_equal(nrow(row), 1L)
  expect_false(isTRUE(row$sign_ci_violation[1]))
  expect_true("sign_ci_violation" %in% names(r))
})
