## v0.3.5 — R-squared CI (MetaESCI request 1B)
## --------------------------------------------
## ci_R2_all() routes through ci_etap2_all() (R^2 = partial eta^2 in
## one-predictor / single-omnibus regression), retagging methods.

test_that("ci_R2_all returns empty list for missing inputs", {
  expect_length(ci_R2_all(NA_real_, 1, 50), 0L)
  expect_length(ci_R2_all(0.1, NA_real_, 50), 0L)
  expect_length(ci_R2_all(0.1, 1, NA_real_), 0L)
  expect_length(ci_R2_all(-0.1, 1, 50), 0L)
  expect_length(ci_R2_all(1, 1, 50), 0L)  # R2 = 1 is degenerate
})

test_that("ci_R2_all matches ci_etap2_all numerically (via_etap2 retagging)", {
  R2 <- 0.147
  df1 <- 3
  df2 <- 96
  F_val <- (R2 / df1) / ((1 - R2) / df2)
  r2 <- ci_R2_all(R2, df1, df2, F_val = F_val, level = 0.95)
  e2 <- ci_etap2_all(F_val, df1, df2, level = 0.95)
  expect_setequal(sub("_via_etap2$", "", names(r2)), names(e2))
  for (m in names(e2)) {
    tag <- paste0(m, "_via_etap2")
    expect_equal(r2[[tag]]$bounds, e2[[m]]$bounds, tolerance = 1e-9)
    expect_match(r2[[tag]]$method, "_via_etap2$")
  }
})

test_that("ci_R2_all back-computes F from R2 + df when F_val omitted", {
  R2 <- 0.147; df1 <- 3; df2 <- 96
  with_F <- ci_R2_all(R2, df1, df2, F_val = (R2/df1) / ((1-R2)/df2))
  without_F <- ci_R2_all(R2, df1, df2)  # F_val defaults to NA -> recomputed
  expect_equal(names(with_F), names(without_F))
  for (m in names(with_F)) {
    expect_equal(with_F[[m]]$bounds, without_F[[m]]$bounds, tolerance = 1e-9)
  }
})

test_that("check_text dispatches R^2 CI for F-test rows", {
  res <- check_text("F(3, 96) = 5.50, p = .002, R2 = 0.147, 95% CI [0.04, 0.25]")
  expect_equal(nrow(res), 1)
  expect_equal(res$effect_reported_name[1], "R2")
  expect_match(res$ci_method_match[1], "^R2:.*via_etap2$")
  expect_true(res$ci_check_status[1] %in% c("MATCH", "PLAUSIBLE", "INCONSISTENT"))
})
