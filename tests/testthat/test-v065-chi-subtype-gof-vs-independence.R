# v0.6.5: chi-square sub-type classification distinguishes goodness-of-fit (one
# variable vs a 50-50 / chance baseline) from a test of independence (two
# categorical variables), and resists context-window keyword bleed by reading the
# chi-square's OWN sentence (raw_text) first.
#
# Surfaced by the 2026-06-21 escicheck-iterate re-audit of collabra.37122:
# reversal "deviation from 50-50" goodness-of-fit tests were tagged
# chisq_subtype = "contingency", and "chi-square test of independence" rows were
# tagged "gof" (the word "goodness" bled in from an adjacent sentence).
# gof and contingency compute an identical V for a 1-df table, so the reported-V
# verification is unaffected by these tests -- only the sub-type label.

get_chi <- function(res, stat) {
  res[!is.na(res$test_type) & res$test_type == "chisq" &
        !is.na(res$stat_value) & abs(res$stat_value - stat) < 1e-6, ]
}

test_that("a 'test of independence' row is contingency even when a neighbour sentence says 'goodness of fit'", {
  txt <- paste0(
    "First, a deviation from 50-50 was tested by a goodness of fit, ",
    "chi2 (1, N = 100) = 4.00, p = .046, V = 0.20. ",
    "We conducted a chi-square test of independence and found support for the ",
    "association between temporal distance and regret, ",
    "chi2 (1, N = 495) = 14.68, p < .001, V = 0.17."
  )
  res <- effectcheck::check_text(txt)
  indep <- get_chi(res, 14.68)
  expect_true(nrow(indep) >= 1)
  expect_true(any(indep$chisq_subtype == "contingency", na.rm = TRUE))
  expect_false(any(indep$chisq_subtype == "gof", na.rm = TRUE))
  # and the neighbouring goodness-of-fit row stays gof
  gof <- get_chi(res, 4.00)
  expect_true(nrow(gof) >= 1)
  expect_true(any(gof$chisq_subtype == "gof", na.rm = TRUE))
})

test_that("a reversal 'deviation from 50-50' test is goodness-of-fit, not contingency", {
  txt <- paste0(
    "More participants chose action in the short term and inaction in the long ",
    "term, a deviation from 50-50, chi2 (1, N = 133) = 65.03, p < .001, ",
    "V = 0.70, 95% CI [0.56, 0.80]."
  )
  res <- effectcheck::check_text(txt)
  g <- get_chi(res, 65.03)
  expect_true(nrow(g) >= 1)
  expect_true(any(g$chisq_subtype == "gof", na.rm = TRUE))
  # V = sqrt(65.03/133) = 0.699 -> the reported V verifies regardless of label
  expect_true(any(!is.na(g$matched_value), na.rm = TRUE))
})

test_that("an explicit goodness-of-fit chi-square is gof", {
  txt <- "A chi-square goodness of fit test, chi2 (2, N = 90) = 6.10, p = .047."
  res <- effectcheck::check_text(txt)
  g <- get_chi(res, 6.10)
  expect_true(nrow(g) >= 1)
  expect_true(any(g$chisq_subtype == "gof", na.rm = TRUE))
})
