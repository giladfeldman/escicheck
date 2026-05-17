# Stage 1 / P2 -- Spearman rho and Kendall tau as first-class rank correlations.
#
# Before Stage 1, rho/tau had no test pattern; if written r(df)= they were
# routed through the Pearson path (parametric t-from-r p-value, plain Fisher-z
# CI) -- silently wrong for rank correlations. Computation functions are
# validated against base-R cor.test(exact = FALSE), which uses exactly the
# large-sample approximations effectcheck implements.

test_that("spearman_pvalue matches cor.test large-sample p", {
  set.seed(1); x <- rnorm(40); y <- x + rnorm(40)
  ref <- cor.test(x, y, method = "spearman", exact = FALSE)
  rho <- unname(ref$estimate); n <- 40
  expect_equal(spearman_pvalue(rho, n), unname(ref$p.value), tolerance = 1e-6)
})

test_that("kendall_pvalue matches cor.test normal-approx p", {
  set.seed(2); x <- rnorm(45); y <- x + rnorm(45)
  ref <- cor.test(x, y, method = "kendall", exact = FALSE)
  tau <- unname(ref$estimate); n <- 45
  expect_equal(kendall_pvalue(tau, n), unname(ref$p.value), tolerance = 1e-6)
})

test_that("ci_spearman differs from a plain Fisher-z (Pearson) CI", {
  rho <- 0.45; n <- 40
  rank_ci <- ci_spearman(rho, n)
  z <- atanh(rho); se_pearson <- 1 / sqrt(n - 3)
  pearson_low <- tanh(z - 1.959964 * se_pearson)
  expect_true(rank_ci$ci_low > -1 && rank_ci$ci_high < 1)
  expect_false(isTRUE(all.equal(rank_ci$ci_low, pearson_low)))
})

test_that("ci_kendall produces an ordered interval containing tau", {
  tau <- 0.30; n <- 50
  ci <- ci_kendall(tau, n)
  expect_true(ci$ci_low < tau && tau < ci$ci_high)
})

# --- Task 3: parse patterns ---------------------------------------------------

test_that("Spearman rho is parsed as its own test type", {
  res <- check_text("A Spearman correlation was significant, rho(38) = .45, p = .004.")
  expect_true("spearman" %in% res$test_type)
})

test_that("Kendall tau is parsed as its own test type", {
  res <- check_text("Kendall's tau showed an association, tau(48) = .30, p = .002.")
  expect_true("kendall" %in% res$test_type)
})

test_that("an r(df) written in a Spearman context is retagged as spearman", {
  res <- check_text("Using a Spearman rank correlation, r(38) = .45, p = .004.")
  expect_true("spearman" %in% res$test_type)
  expect_false("r" %in% res$test_type)
})

# --- Task 4: routing ----------------------------------------------------------

test_that("a Spearman result recomputes a rank-appropriate p and CI", {
  res <- check_text("A Spearman correlation, rho(37) = .45, p = .005, 95% CI [.14, .68].")
  row <- res[res$test_type == "spearman", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$p_computed[1], spearman_pvalue(0.45, 39), tolerance = 1e-6)
  expect_false(row$status[1] == "ERROR")
})

test_that("a Kendall result recomputes a rank-appropriate p", {
  res <- check_text("Kendall's tau, tau(48) = .30, p = .002.")
  row <- res[res$test_type == "kendall", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$p_computed[1], kendall_pvalue(0.30, 50), tolerance = 1e-6)
})
