# v0.7.14 (d-cf56d6, d-4efdc1): INCONSISTENT is a claim about the PAPER, so it
# needs an affirmative premise: that the intervals we computed are comparable
# with the one the paper printed -- same tail, same allocation, same quantity.
#
# v0.7.9 withdrew the status escalation (0 of 3 precision on the corpus) but
# `ci_check_status` kept publishing INCONSISTENT on the same false premises.
# Three classes, reproduced at 0.7.13 on correctly reported numbers:
#
#   one-sided interval   r(198) = .34, one-sided 95% CI [0.23, 1.00]
#                        -> the two-sided candidates cannot reproduce it; the
#                           true one-sided lower bound is 0.2326.
#   unstated allocation  t(98) = 0.30, d = 0.075, 95% CI [-0.415, 0.565]
#                        -> exact at n1 = 20, n2 = 80; graded only at 50/50.
#   different referent   d = 1.03 with "the unstandardized contrast had a 95%
#                        CI [9.99, 30.01]" -> not an interval on d at all.
#
# Each is now UNVERIFIABLE with a machine-readable `ci_unverifiable_reason`,
# and `ci_match` is NA (not FALSE) so no other field republishes the verdict.
# The controls matter as much as the fixes: a genuinely wrong interval of each
# shape must STAY INCONSISTENT, or this becomes a way to launder real errors.

row1 <- function(txt) {
  r <- check_text(txt)
  expect_equal(nrow(r), 1L, info = txt)
  r
}

test_that("one-sided interval: UNVERIFIABLE, not INCONSISTENT", {
  r <- row1("r(198) = .34, p < .001, one-sided 95% CI [0.23, 1.00]")
  expect_equal(r$ci_check_status, "UNVERIFIABLE")
  expect_equal(r$ci_unverifiable_reason, "one_sided_interval")
  expect_true(is.na(r$ci_match))
  # a bound at the parameter limit alone is also the one-sided signature
  r2 <- row1("r(198) = .34, p < .001, 95% CI [0.23, 1.00]")
  expect_equal(r2$ci_check_status, "UNVERIFIABLE")
  expect_equal(r2$ci_unverifiable_reason, "one_sided_interval")
})

test_that("one-sided controls: a correct two-sided r CI still MATCHes, a wrong one stays INCONSISTENT", {
  ok <- row1("r(198) = .34, p < .001, 95% CI [0.21, 0.46]")
  expect_equal(ok$ci_check_status, "MATCH")
  expect_true(is.na(ok$ci_unverifiable_reason))
  bad <- row1("r(198) = .34, p < .001, 95% CI [0.30, 0.38]")
  expect_equal(bad$ci_check_status, "INCONSISTENT")
  expect_true(is.na(bad$ci_unverifiable_reason))
})

test_that("unstated allocation: an unequal split that reproduces the CI makes it UNVERIFIABLE", {
  r <- row1("t(98) = 0.30, p = .765, d = 0.075, 95% CI [-0.415, 0.565]")
  expect_equal(r$ci_check_status, "UNVERIFIABLE")
  expect_equal(r$ci_unverifiable_reason, "unstated_allocation")
  expect_true(is.na(r$ci_match))
  u <- paste(unlist(r$uncertainty_reasons), collapse = " | ")
  expect_true(grepl("n1 = 20, n2 = 80", u, fixed = TRUE))
})

test_that("allocation controls: 50/50 interval MATCHes; one narrower than ANY split stays INCONSISTENT", {
  ok <- row1("t(98) = 0.30, p = .765, d = 0.075, 95% CI [-0.317, 0.467]")
  expect_equal(ok$ci_check_status, "MATCH")
  # The equal split gives the NARROWEST interval, so a narrower one fits no split.
  bad <- row1("t(98) = 0.30, p = .765, d = 0.06, 95% CI [-0.10, 0.22]")
  expect_equal(bad$ci_check_status, "INCONSISTENT")
  expect_true(is.na(bad$ci_unverifiable_reason))
  # Stated group sizes remove the allocation question entirely.
  st <- row1("With n1 = 50 and n2 = 50, t(98) = 0.30, p = .765, d = 0.075, 95% CI [-0.415, 0.565]")
  expect_false(identical(st$ci_unverifiable_reason, "unstated_allocation"))
})

test_that("different referent: an interval that cannot be on d is UNVERIFIABLE, and says why", {
  r <- row1("t(58) = 4.00, p < .001, d = 1.03; the unstandardized contrast had a 95% CI [9.99, 30.01]")
  expect_equal(r$ci_check_status, "UNVERIFIABLE")
  expect_equal(r$ci_unverifiable_reason, "referent_not_effect")
  expect_equal(r$ci_referent, "not_effect_reported")
  # the impossible-value finding is NOT laundered: it stays on its own field
  expect_true(isTRUE(r$estimate_outside_ci))
})

test_that("d-family referent by midpoint: a d interval is centred on d (item 9 widening)", {
  # Contains d = 1.03 but is centred on 1.75: no d-interval method is that skewed.
  r <- row1("t(58) = 4.00, p < .001, d = 1.03, 95% CI [0.50, 3.00]")
  expect_equal(r$ci_referent, "not_effect_reported")
  expect_equal(r$ci_check_status, "UNVERIFIABLE")
  # control: the correct d interval is untouched and MATCHes
  ok <- row1("t(58) = 4.00, p < .001, d = 1.03, 95% CI [0.49, 1.57]")
  expect_true(is.na(ok$ci_referent))
  expect_true(ok$ci_check_status %in% c("MATCH", "PLAUSIBLE"))
})

test_that("ci_unverifiable_reason names the pre-existing UNVERIFIABLE causes too", {
  r <- row1("A paired t-test showed t(742) = 12.24, p < .001, d = 0.55, 95% CI [0.47, 0.62]")
  if (identical(r$ci_check_status, "UNVERIFIABLE")) {
    expect_false(is.na(r$ci_unverifiable_reason))
  }
  # MISSING / MATCH rows carry no reason
  m <- row1("t(58) = 4.00, p < .001, d = 1.03")
  expect_true(is.na(m$ci_unverifiable_reason))
})

test_that("an interval with no reported estimate is not graded as a standardized one", {
  # Verbatim corpus shape (10.1038/s41562-024-01961-1): a raw mean difference.
  r <- row1("t(181) = 2.571, P = 0.011, 95%CI difference [0.102, 0.776]")
  expect_equal(r$ci_check_status, "UNVERIFIABLE")
  expect_equal(r$ci_unverifiable_reason, "no_estimate_parsed")
  # a correlation is exempt: its statistic IS its estimate
  bad_r <- row1("r(198) = .34, p < .001, 95% CI [0.30, 0.38]")
  expect_equal(bad_r$ci_check_status, "INCONSISTENT")
})

test_that("an allocation fit must also reproduce the reported d (Sonnet consult 2026-09-28)", {
  # A split that reproduces the interval but implies a DIFFERENT d than the one
  # printed does not explain the row; it is a coincidence on a 1-D family of
  # intervals. d = 0.20 (centred interval, so the referent rule does not fire):
  # [-0.553, 0.753] is the n1 = 10, n2 = 90 interval for d = 0.10, not 0.20.
  r <- row1("t(98) = 0.30, p = .765, d = 0.20, 95% CI [-0.553, 0.753]")
  expect_false(identical(r$ci_unverifiable_reason, "unstated_allocation"))
})
