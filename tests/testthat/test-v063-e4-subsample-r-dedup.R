# v0.6.3 E4: a correlation reported in the abstract (no df) and again in the
# body with an inline df must collapse to ONE row bound to its OWN CI.
#
# Paper: 10.1525/collabra.57785. The abstract sentence co-reports a d clause
# "(d=0.39[0.25, 0.54])" immediately before "(r=-.34[-.43, -.24])"; the
# pre-0.6.3 first-bracket CI binder gave the r the d clause's [0.25, 0.54]
# (spurious INCONSISTENT), and the abstract r also inherited a global-N df1,
# leaving a duplicate of the body "r(348) = -0.34, 95% CI [-0.43, -0.24]".
# Fix: (a) bind the CI nearest the r statistic; (b) collapse same-r/same-CI
# rows that differ only in df1, keeping the inline-df (parenthesized) row.

test_that("E4: subsample r collapses to one row with its own CI and inline df", {
  txt <- paste(
    paste0("Self-concept was more strongly associated with experiential ",
           "purchases than with material purchases (d=0.39[0.25, 0.54]), and ",
           "that there was a negative association between experiential purchase ",
           "satisfaction and the willingness to exchange memory ",
           "(r=-.34[-.43, -.24]) (all effects above were p<.001)."),
    paste0("First, a Pearson's r correlation analysis between the exchange ",
           "index of experiential purchases and satisfaction of experience ",
           "purchases revealed that the exchange index was negatively ",
           "correlated with satisfaction, r(348) = -0.34, p < .001, ",
           "95% CI [-0.43, -0.24]."),
    sep = " ")

  r <- check_text(txt)

  r34 <- r[!is.na(r$stat_value) & abs(r$stat_value + 0.34) < 1e-6 &
             !is.na(r$test_type) & r$test_type == "r", ]
  expect_equal(nrow(r34), 1L)
  expect_equal(r34$df1[1], 348)
  expect_equal(r34$ciL_reported[1], -0.43, tolerance = 1e-6)
  expect_equal(r34$ciU_reported[1], -0.24, tolerance = 1e-6)
  # The bled d-clause CI must NOT win.
  expect_false(isTRUE(r34$ciL_reported[1] == 0.25))
  if ("ci_check_status" %in% names(r)) {
    expect_false(isTRUE(r34$ci_check_status[1] == "INCONSISTENT"))
  }
})

test_that("E4 guard: two genuinely distinct correlations with different CIs are NOT collapsed", {
  # Same r value, DIFFERENT reported CIs -> distinct results, must stay separate
  # (protects against over-eager dedup; mirrors the v0.5.14a regression guard).
  txt <- paste(
    "For hypothesis 1a, r(261) = 0.45, p < .001, 95% CI [0.35, 0.55].",
    "For hypothesis 2a, r(261) = 0.45, p < .001, 95% CI [0.34, 0.54].",
    sep = " ")
  r <- check_text(txt)
  r45 <- r[!is.na(r$stat_value) & abs(r$stat_value - 0.45) < 1e-6 &
             !is.na(r$test_type) & r$test_type == "r", ]
  expect_equal(nrow(r45), 2L)
})
