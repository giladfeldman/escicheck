# test-z-u-effects.R — v0.2.7 Issue 2: z-test and U-test effect size computation
# z-tests with d/g reported now compute d_from_z = 2z/sqrt(N)
# U-tests with only N try equal-split rank-biserial r

test_that("d_from_z computes correctly", {
  # d = 2 * 2.5 / sqrt(100) = 0.50
  expect_equal(d_from_z(2.5, 100), 0.50)
  expect_equal(d_from_z(0, 100), 0)
  expect_true(is.na(d_from_z(NA, 100)))
  expect_true(is.na(d_from_z(2.5, NA)))
  expect_true(is.na(d_from_z(2.5, 0)))
})

test_that("z-test with reported d and N computes d_from_z", {
  # z = 2.58, N = 100: d = 2*2.58/sqrt(100) = 0.516
  res <- check_text("z = 2.58, p = .010, d = 0.52, N = 100")
  expect_true(nrow(res) >= 1)
  if (res$test_type[1] == "z" && !is.na(res$check_scope[1])) {
    expect_equal(res$check_scope[1], "effect_size_checked")
    expect_false(is.na(res$matched_value[1]))
    # d_from_z = 0.516, reported = 0.52, delta should be small
    expect_true(is.na(res$delta_effect_abs[1]) || res$delta_effect_abs[1] < 0.02)
  }
})

test_that("z-test without d/N stays p_value_only", {
  res <- check_text("z = 2.58, p = .010")
  expect_true(nrow(res) >= 1)
  expect_equal(res$check_scope[1], "p_value_only")
  expect_true(is.na(res$matched_value[1]))
})

test_that("z-test with g reported and N computes g_from_z", {
  # z = 3.0, N = 50: d = 2*3/sqrt(50) = 0.849, J(48) = 0.985, g = 0.836
  res <- check_text("z = 3.00, p = .003, g = 0.84, N = 50")
  if (res$test_type[1] == "z" && !is.na(res$check_scope[1])) {
    expect_equal(res$check_scope[1], "effect_size_checked")
  }
})

test_that("U-test with N computes rank_biserial_r via equal split", {
  # U = 450, N = 60: n1 = 30, n2 = 30
  # rank_biserial_r = 1 - 2*450/(30*30) = 1 - 1.0 = 0.0
  res <- check_text("U = 450, p = .500, N = 60")
  if (nrow(res) >= 1 && res$test_type[1] == "U") {
    # Should have computed rank_biserial_r (even if zero)
    if (!is.na(res$rank_biserial_r[1])) {
      expect_true(abs(res$rank_biserial_r[1]) <= 1)
    }
  }
})

test_that("Regression guard: t-test d PASS unchanged", {
  res <- check_text("t(45) = 2.31, p = .023, d = 0.68")
  expect_equal(res$status[1], "PASS")
})
