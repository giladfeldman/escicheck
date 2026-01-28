# Test suite for computation functions

test_that("d_ind_from_t computes correctly", {
  # Known example: t=2.0, n1=20, n2=20
  result <- effectcheck:::d_ind_from_t(2.0, 20, 20)
  expect_true(!is.na(result))
  expect_true(result > 0)
})

test_that("g_ind_from_t applies Hedges correction", {
  t <- 2.0
  n1 <- 20
  n2 <- 20
  d <- effectcheck:::d_ind_from_t(t, n1, n2)
  g <- effectcheck:::g_ind_from_t(t, n1, n2)
  expect_true(g < d)  # Hedges' g should be smaller than d
})

test_that("dz_from_t computes correctly", {
  result <- effectcheck:::dz_from_t(2.0, 30)
  expect_true(!is.na(result))
  expect_true(result > 0)
})

test_that("dav_from_dz computes correctly", {
  dz <- 0.5
  r <- 0.5
  dav <- effectcheck:::dav_from_dz(dz, r)
  expect_true(!is.na(dav))
  expect_true(dav > dz)  # dav should be larger than dz
})

test_that("fisher_ci_r computes CI for correlation", {
  r <- 0.5
  n <- 100
  ci <- effectcheck:::fisher_ci_r(r, n, 0.95)
  expect_equal(length(ci), 2)
  expect_true(ci[1] < r)
  expect_true(ci[2] > r)
  expect_true(ci[1] > -1 && ci[1] < 1)
  expect_true(ci[2] > -1 && ci[2] < 1)
})

test_that("eta2_from_F computes correctly", {
  F_val <- 4.0
  df1 <- 2
  df2 <- 27
  eta2 <- effectcheck:::eta2_from_F(F_val, df1, df2)
  expect_true(!is.na(eta2))
  expect_true(eta2 > 0 && eta2 < 1)
})

test_that("partial_eta2_from_F computes correctly", {
  F_val <- 4.0
  df1 <- 2
  df2 <- 27
  partial_eta2 <- effectcheck:::partial_eta2_from_F(F_val, df1, df2)
  expect_true(!is.na(partial_eta2))
  expect_true(partial_eta2 > 0 && partial_eta2 < 1)
})

test_that("omega2_from_F computes correctly", {
  F_val <- 4.0
  df1 <- 2
  df2 <- 27
  omega2 <- effectcheck:::omega2_from_F(F_val, df1, df2)
  expect_true(!is.na(omega2))
  expect_true(omega2 >= 0 && omega2 < 1)
  # Omega² should be smaller than eta²
  eta2 <- effectcheck:::eta2_from_F(F_val, df1, df2)
  expect_true(omega2 < eta2)
})

test_that("cohens_f_from_F computes correctly", {
  F_val <- 4.0
  df1 <- 2
  df2 <- 27
  f <- effectcheck:::cohens_f_from_F(F_val, df1, df2)
  expect_true(!is.na(f))
  expect_true(f > 0)
})

test_that("standardized_beta_from_t computes correctly", {
  t <- 2.0
  df <- 30
  beta <- effectcheck:::standardized_beta_from_t(t, df)
  expect_true(!is.na(beta))
  expect_true(beta > 0 && beta < 1)
})

test_that("cohens_f2_from_R2 computes correctly", {
  R2 <- 0.25
  f2 <- effectcheck:::cohens_f2_from_R2(R2)
  expect_true(!is.na(f2))
  expect_true(f2 > 0)
  # f² should increase with R²
  f2_larger <- effectcheck:::cohens_f2_from_R2(0.5)
  expect_true(f2_larger > f2)
})

test_that("phi_from_chisq computes correctly", {
  chisq <- 7.2
  N <- 120
  phi <- effectcheck:::phi_from_chisq(chisq, N)
  expect_true(!is.na(phi))
  expect_true(phi > 0 && phi < 1)
})

test_that("V_from_chisq computes correctly", {
  chisq <- 18.0
  N <- 300
  m <- 2
  V <- effectcheck:::V_from_chisq(chisq, N, m)
  expect_true(!is.na(V))
  expect_true(V > 0 && V < 1)
})

test_that("compute_dav_range returns correct structure", {
  dz <- 0.5
  r_grid <- seq(0.1, 0.9, by = 0.1)
  result <- effectcheck:::compute_dav_range(dz, r_grid)
  expect_true(is.list(result))
  expect_true(!is.na(result$min))
  expect_true(!is.na(result$max))
  expect_true(!is.na(result$median))
  expect_true(result$min < result$max)
  expect_true(result$median >= result$min && result$median <= result$max)
})

test_that("compute_all_anova_effects returns all variants", {
  F_val <- 4.0
  df1 <- 2
  df2 <- 27
  result <- effectcheck:::compute_all_anova_effects(F_val, df1, df2, "between")
  expect_true(is.list(result))
  expect_true(!is.na(result$eta2))
  expect_true(!is.na(result$partial_eta2))
  expect_true(!is.na(result$omega2))
  expect_true(!is.na(result$cohens_f))
})
