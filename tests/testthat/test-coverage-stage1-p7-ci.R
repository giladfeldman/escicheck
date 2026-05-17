# Stage 1 / P7 -- confidence intervals for analytic effect-size families.
#
# omega2 / partial omega2 / epsilon2 / f2 / adjusted R2 / Cohen's w had point
# estimates but no CI function, leaving the v0.3.5 CI-audit columns blank.
# Each new CI reuses effectcheck's own tested machinery (the noncentral-F
# inversion behind ci_etap2; the R2 CI; a noncentral-chi-square inversion for
# w) and is verified by bracketing its point estimate.

test_that("ci_omega2 brackets the omega2 point estimate", {
  F_val <- 8.5; df1 <- 2; df2 <- 87
  ci <- ci_omega2(F_val, df1, df2)
  pt <- omega2_from_F(F_val, df1, df2)
  expect_true(isTRUE(ci$success))
  expect_true(ci$bounds[1] <= pt && pt <= ci$bounds[2])
  expect_true(ci$bounds[1] >= 0)
})

test_that("ci_partial_omega2 and ci_epsilon2 bracket their point estimates", {
  F_val <- 8.5; df1 <- 2; df2 <- 87
  po <- ci_partial_omega2(F_val, df1, df2)
  ep <- ci_epsilon2(F_val, df1, df2)
  expect_true(po$bounds[1] <= partial_omega2_from_F(F_val, df1, df2) &&
                partial_omega2_from_F(F_val, df1, df2) <= po$bounds[2])
  expect_true(ep$bounds[1] <= epsilon2_anova_from_F(F_val, df1, df2) &&
                epsilon2_anova_from_F(F_val, df1, df2) <= ep$bounds[2])
})

test_that("ci_f2 brackets the f-squared point estimate", {
  F_val <- 8.5; df1 <- 2; df2 <- 87
  ci <- ci_f2(F_val, df1, df2)
  expect_true(ci$bounds[1] <= cohens_f2_from_F(F_val, df1, df2) &&
                cohens_f2_from_F(F_val, df1, df2) <= ci$bounds[2])
})

test_that("ci_adjusted_R2 brackets the adjusted R2 point estimate", {
  R2 <- 0.30; N <- 120; k <- 4
  ci <- ci_adjusted_R2(R2, N, k, df1 = k, df2 = N - k - 1)
  pt <- adjusted_R2_from_R2(R2, N, k)
  expect_true(isTRUE(ci$success))
  expect_true(ci$bounds[1] <= pt && pt <= ci$bounds[2])
})

test_that("ci_cohens_w brackets the w point estimate", {
  chisq <- 12.0; N <- 100; w <- sqrt(chisq / N)
  ci <- ci_cohens_w(chisq, df = 2, N = N)
  expect_true(ci$bounds[1] <= w && w <= ci$bounds[2])
  expect_true(ci$bounds[1] >= 0)
})

# --- Task 10: wiring into check_text ------------------------------------------

test_that("an omega2 result carries a populated computed CI", {
  res <- check_text("An ANOVA showed an effect, F(2, 87) = 8.50, p < .001, omega2 = .14.")
  row <- res[!is.na(res$reported_type) & res$reported_type == "omega2", ]
  expect_equal(nrow(row), 1)
  expect_false(is.na(row$ciL_computed[1]))
  expect_false(is.na(row$ciU_computed[1]))
  expect_true(row$ciL_computed[1] <= row$ciU_computed[1])
})

test_that("an epsilon-squared result carries a populated computed CI", {
  res <- check_text("A one-way ANOVA, F(3, 96) = 5.20, p = .002, epsilon2 = .11.")
  row <- res[!is.na(res$reported_type) & res$reported_type == "epsilon_squared", ]
  expect_equal(nrow(row), 1)
  expect_false(is.na(row$ciL_computed[1]))
  expect_true(row$ciL_computed[1] <= row$ciU_computed[1])
})
