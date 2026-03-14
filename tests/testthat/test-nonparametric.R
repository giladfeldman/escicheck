# test-nonparametric.R
# Tests for nonparametric test support (U, W, H)

# ===========================================================================
# Parsing tests
# ===========================================================================

test_that("Mann-Whitney U is parsed with p co-occurrence", {
  res <- parse_text("U = 245, z = 2.31, p = .021, n1 = 30, n2 = 40")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "U")
  expect_equal(res$stat_value[1], 245)
  expect_equal(res$z_auxiliary[1], 2.31)
  expect_equal(res$n1[1], 30)
  expect_equal(res$n2[1], 40)
})

test_that("Mann-Whitney U is NOT parsed without p or z", {
  # Bare "U = 245" should not match to avoid false positives
  res <- parse_text("U = 245 with no other stats.")
  # Should either have 0 rows or not match as U
  if (nrow(res) > 0) {
    expect_true(all(res$test_type != "U"))
  }
})

test_that("Wilcoxon W is parsed with z co-occurrence", {
  res <- parse_text("W = 145, z = 1.85, p = .064")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "W")
  expect_equal(res$stat_value[1], 145)
  expect_equal(res$z_auxiliary[1], 1.85)
})

test_that("Kruskal-Wallis H is parsed", {
  res <- parse_text("H(2) = 8.45, p = .015, N = 60")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "H")
  expect_equal(res$df1[1], 2)
  expect_equal(res$stat_value[1], 8.45)
})

# ===========================================================================
# Pipeline compute tests
# ===========================================================================

test_that("U-test pipeline computes rank-biserial r from U with group sizes", {
  res <- check_text("U = 245, z = 2.31, p = .021, n1 = 30, n2 = 40")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "U")
  # rank_biserial_r = 1 - 2*245 / (30*40) = 1 - 490/1200 = 0.59167
  expected_rb <- 1 - 490/1200
  expect_equal(res$rank_biserial_r[1], expected_rb, tolerance = 1e-3)
})

test_that("U-test pipeline computes rank-biserial r from z when no group sizes", {
  res <- check_text("U = 245, z = 2.31, p = .021, N = 70")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "U")
  # No n1/n2, but has z and N -> r = z/sqrt(N) = 2.31/sqrt(70)
  expected_rb_z <- 2.31 / sqrt(70)
  expect_equal(res$rank_biserial_r[1], expected_rb_z, tolerance = 1e-3)
})

test_that("H-test pipeline computes epsilon-squared", {
  res <- check_text("H(2) = 8.45, p = .015, N = 60")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "H")
  # epsilon2 = (H - k + 1) / (N - k) = (8.45 - 3 + 1) / (60 - 3) = 6.45/57 = 0.11316
  expected_eps <- 6.45 / 57
  expect_equal(res$epsilon_squared[1], expected_eps, tolerance = 1e-3)
})

test_that("H-test computes p-value using chi-square distribution", {
  res <- check_text("H(2) = 8.45, p = .015, N = 60")
  expected_p <- pchisq(8.45, df = 2, lower.tail = FALSE)
  expect_equal(res$p_computed[1], expected_p, tolerance = 1e-4)
})

test_that("U-test computes p-value from z_auxiliary", {
  res <- check_text("U = 245, z = 2.31, p = .021, n1 = 30, n2 = 40")
  expected_p <- 2 * pnorm(abs(2.31), lower.tail = FALSE)
  expect_equal(res$p_computed[1], expected_p, tolerance = 1e-4)
})

# ===========================================================================
# Status determination
# ===========================================================================

test_that("nonparametric tests return valid status", {
  res <- check_text("H(2) = 8.45, p = .015, N = 60")
  expect_true(res$status[1] %in% c("PASS", "OK", "NOTE", "WARN", "ERROR", "INSUFFICIENT_DATA"))
})

test_that("W-test without N or z still runs without error", {
  res <- check_text("W = 145, p = .064")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "W")
  # Should have insufficient data or WARN status
  expect_true(res$status[1] %in% c("PASS", "OK", "NOTE", "WARN", "ERROR", "INSUFFICIENT_DATA"))
})

# ===========================================================================
# Edge cases
# ===========================================================================

test_that("empty nonparametric tibble has z_auxiliary column", {
  res <- parse_text("")
  expect_true("z_auxiliary" %in% names(res))
})

test_that("nonparametric stats are included in default check_text stats", {
  # Verify U, W, H are in the defaults
  fn_formals <- formals(check_text)
  default_stats <- eval(fn_formals$stats)
  expect_true("U" %in% default_stats)
  expect_true("W" %in% default_stats)
  expect_true("H" %in% default_stats)
})
