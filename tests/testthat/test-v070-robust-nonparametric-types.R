# v0.7.0 -- the modern nonparametric / robust family: Brunner-Munzel, ATS, WTS,
# and Yuen's trimmed-mean t.
#
# Raised by the same methodologist as v0.6.21/0.6.22, who asked whether the
# permutation work was "extendable to other tests (e.g. Brunner-Munzel, ATS,
# WTS etc)" and assumed that without raw data none could be reproduced.
#
# It is extendable, and further than the question supposed: all four report a
# statistic against a KNOWN reference distribution, so the reported p IS
# independently verifiable from the statistic and df alone. What is not
# recoverable is the effect size. That is the same shape as the existing
# cochran_q branch (v0.5.15), so these follow the NOTE-only test_type template
# with a p-check attached.
#
# Reference distributions:
#   WTS  ~ chi-square(df)                     -> pchisq(WTS, df, lower=FALSE)
#   ATS  ~ F(df1, df2), df1 usually NON-INTEGER; df2 may be Inf, in which case
#          pf(F, df1, Inf) reduces exactly to pchisq(df1*F, df1, lower=FALSE)
#   BM   ~ t(Satterthwaite df)                -> 2*pt(-abs(W), df)
#   Yuen ~ t(trimmed df)                      -> 2*pt(-abs(t), df)
#
# Every one must be EXEMPTED by the v0.6.21 resampling machinery when its own
# clause says permuted/bootstrapped: GFD reports both an asymptotic and a
# permuted WTS precisely because the asymptotic one is liberal in small
# samples, and bootstrap Yuen is standard in WRS2.

test_that("v0.7.0: WTS is extracted and its p verified against pchisq", {
  txt <- "The Wald-type statistic was significant, WTS(2) = 12.34, p = .002."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "wts", ]
  expect_gte(nrow(row), 1L)
  expect_equal(row$stat_value[1], 12.34)
  expect_equal(row$df1[1], 2)
  expect_equal(row$p_computed[1], pchisq(12.34, df = 2, lower.tail = FALSE),
               tolerance = 1e-8)
  # No effect size is recoverable from a WTS.
  expect_true(is.na(row$matched_value[1]))
  expect_match(row$uncertainty_reasons[1], "Wald-type", ignore.case = TRUE)
  # An honest NOTE, never a silent SKIP.
  expect_false(identical(row$status[1], "SKIP"))
})

test_that("v0.7.0: ATS accepts non-integer df and verifies against pf", {
  txt <- "The ANOVA-type statistic was ATS(1.87, 45.30) = 3.45, p = .041."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "ats", ]
  expect_gte(nrow(row), 1L)
  expect_equal(row$stat_value[1], 3.45)
  expect_equal(row$df1[1], 1.87)
  expect_equal(row$df2[1], 45.30)
  expect_equal(row$p_computed[1], pf(3.45, 1.87, 45.30, lower.tail = FALSE),
               tolerance = 1e-8)
})

test_that("v0.7.0: ATS with df2 = Inf reduces exactly to the chi-square form", {
  # F(d1, Inf) =d chisq(d1)/d1, so pf(F, d1, Inf) == pchisq(d1*F, d1).
  txt <- "The ANOVA-type statistic was ATS(1.87, Inf) = 3.45, p = .041."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "ats", ]
  expect_gte(nrow(row), 1L)
  expect_true(is.infinite(row$df2[1]))
  expect_equal(row$p_computed[1], pchisq(1.87 * 3.45, 1.87, lower.tail = FALSE),
               tolerance = 1e-8)
  # ...and that the identity itself holds, so the reduction is not a guess.
  expect_equal(pf(3.45, 1.87, Inf, lower.tail = FALSE),
               pchisq(1.87 * 3.45, 1.87, lower.tail = FALSE), tolerance = 1e-12)
})

test_that("v0.7.0: Brunner-Munzel is extracted and not mistaken for Wilcoxon W", {
  txt <- "A Brunner-Munzel test was used, W = 2.34, df = 45.60, p = .024."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "brunner_munzel", ]
  expect_gte(nrow(row), 1L)
  expect_equal(row$stat_value[1], 2.34)
  expect_equal(row$df1[1], 45.60)
  expect_equal(row$p_computed[1], 2 * pt(-abs(2.34), df = 45.60), tolerance = 1e-8)
  # Must NOT route to the Wilcoxon W branch, which would attach a
  # rank-biserial r that the BM statistic does not imply.
  expect_false(identical(row$test_type[1], "W"))
  expect_true(is.na(row$matched_value[1]))
})

test_that("v0.7.0: Yuen's trimmed-mean t is recognised and p-verified", {
  txt <- "Yuen's trimmed-mean test gave Ty(23.40) = 2.51, p = .019."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "yuen", ]
  expect_gte(nrow(row), 1L)
  expect_equal(row$stat_value[1], 2.51)
  expect_equal(row$df1[1], 23.40)
  expect_equal(row$p_computed[1], 2 * pt(-abs(2.51), df = 23.40), tolerance = 1e-8)
})

test_that("v0.7.0: a PERMUTED WTS is exempted from the parametric p-check", {
  # GFD reports both an asymptotic and a permuted WTS because the asymptotic
  # one is liberal in small samples. The permuted variant's p is NOT
  # chi-square, so grading it against pchisq would be the v0.6.21 defect
  # reintroduced through a new door.
  txt <- "A permuted Wald-type statistic, WTS(2) = 12.34, p = .038."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "wts", ]
  expect_gte(nrow(row), 1L)
  expect_true(isTRUE(row$resampling_inference[1]))
  expect_false(isTRUE(row$decision_error[1]))
})

test_that("v0.7.0: the new types are in the default stats whitelist", {
  # A type absent from the default `stats` vector is filtered out of the
  # result entirely, so the parse work would be invisible.
  fml <- formals(check_text)$stats
  defaults <- eval(fml)
  for (tt in c("wts", "ats", "brunner_munzel", "yuen")) {
    expect_true(tt %in% defaults, info = tt)
  }
})

test_that("v0.7.0: every test type has a friendly display name", {
  # Pre-existing gap: .friendly_test_name covered only 12 of 25 types, so the
  # rest fell through to the raw slug in reports.
  defaults <- eval(formals(check_text)$stats)
  for (tt in c(defaults, "table_estimate")) {
    nm <- effectcheck:::.friendly_test_name(tt)
    expect_false(identical(nm, tt),
                 info = paste("no friendly name for test_type:", tt))
  }
})

test_that("v0.7.0: ordinary t/F/W rows are unaffected by the new patterns", {
  # The new dispatch branches sit before generic ones and must not swallow them.
  t_res <- check_text("An independent t-test, t(58) = 2.31, p = .024, d = 0.61.")
  expect_equal(t_res$test_type[1], "t")

  f_res <- check_text("The main effect was significant, F(2, 45) = 3.45, p = .041.")
  expect_equal(f_res$test_type[1], "F")

  w_res <- check_text("A Wilcoxon signed-rank test, W = 234, z = -2.10, p = .036, N = 30.")
  expect_true(w_res$test_type[1] %in% c("W", "dscf", "kendall_w"))
  expect_false(identical(w_res$test_type[1], "brunner_munzel"))
})
