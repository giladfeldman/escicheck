# v0.7.1 -- state what a rank test actually estimates.
#
# The methodological point behind the whole 0.6.21-0.7.1 line of work, made
# checkable. The reviewer's argument was that switching from a t-test to
# Mann-Whitney when assumptions look shaky "can completely change the
# hypotheses answered and therefore answer to a question that was never asked".
#
# That is correct as stated: the Mann-Whitney-Wilcoxon procedure targets
# stochastic superiority, P(X>Y) + 0.5*P(X=Y), not a difference in means, and
# not generally a difference in MEDIANS either -- that reading requires a
# location-shift / equal-shape assumption most papers never state.
# (Fay & Proschan 2010, Statistics Surveys 4:1-39; Divine, Norton, Baron &
# Juarez-Colunga 2018, The American Statistician 72:278-286, "The
# Wilcoxon-Mann-Whitney Procedure Fails as a Test of Medians".)
#
# Concrete counterexample, exact: X uniform on {1,5,6}, Y uniform on {4,5,9}.
# Both have median 5, yet P(X<Y) + 0.5*P(X=Y) = 11/18 = .611, far from the .5
# null -- so the procedure has power against distributions with IDENTICAL
# medians.
#
# Deliberately a NOTE and never an error, and attached to the TEST ROW only.
# We do not scan surrounding prose for mean/median language: we cannot reliably
# link an interpretation sentence to a specific test, and a false accusation
# there would be worse than silence.

test_that("v0.7.1: a Mann-Whitney U row states its estimand", {
  res <- check_text("A Mann-Whitney test was significant, U = 210, z = -2.34, p = .019, N = 60.")
  row <- res[!is.na(res$test_type) & res$test_type == "U", ]
  expect_gte(nrow(row), 1L)
  expect_match(row$uncertainty_reasons[1], "stochastic superiority",
               ignore.case = TRUE)
  expect_match(row$uncertainty_reasons[1], "median", ignore.case = TRUE)
  # A note only -- the row's verdict must not be worsened by it.
  expect_false(identical(row$status[1], "ERROR"))
})

test_that("v0.7.1: a Wilcoxon W row states its estimand", {
  res <- check_text("A Wilcoxon rank-sum test, W = 234, z = -2.10, p = .036, N = 40.")
  row <- res[!is.na(res$test_type) & res$test_type == "W", ]
  expect_gte(nrow(row), 1L)
  expect_match(row$uncertainty_reasons[1], "stochastic superiority",
               ignore.case = TRUE)
})

test_that("v0.7.1: the note does NOT appear on parametric or other rank rows", {
  # A t-test estimates a mean difference; the note would be nonsense there.
  t_res <- check_text("An independent t-test, t(58) = 2.31, p = .024, d = 0.61.")
  expect_false(grepl("stochastic superiority", t_res$uncertainty_reasons[1],
                     ignore.case = TRUE))

  # Kruskal-Wallis is an omnibus test across k groups -- the two-sample
  # stochastic-superiority statement does not apply verbatim.
  h_res <- check_text("A Kruskal-Wallis test, H(2) = 8.45, p = .015, N = 60.")
  expect_false(grepl("stochastic superiority", h_res$uncertainty_reasons[1],
                     ignore.case = TRUE))
})

test_that("v0.7.1: the estimand claim itself is arithmetically true", {
  # Guards the claim in the message, not just its presence. Equal medians,
  # relative effect far from .5 -- so "not a test of medians" is not rhetoric.
  x <- c(1, 5, 6); y <- c(4, 5, 9)
  expect_equal(median(x), median(y))
  s <- sign(outer(x, y, "-"))
  p_lt <- mean(s == -1) + 0.5 * mean(s == 0)   # P(X<Y) + .5 P(X=Y)
  expect_equal(p_lt, 11 / 18, tolerance = 1e-12)
  expect_gt(abs(p_lt - 0.5), 0.1)
})
