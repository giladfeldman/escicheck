# v0.5.6 -- a bare regression coefficient line ("b = .., SE = .., p = ..") with
# no test statistic of its own
#
# escicheck-iterate cycle 5 (2026-05-17): the standard APA regression-line form
# "b = 0.45, SE = 0.12, p = .001" -- a coefficient with its SE and p but no
# t-statistic written out -- returned 0 stats. parse.R's regression-type
# promotion fired only when a t-test had ALREADY been detected; a bare b + SE
# had no test_type to promote. Surfaced by the escicheck-review Phase-3 smoke
# test (no single gold paper carries this exact form -- the harness regression
# papers report b with a co-printed t). Fix: when b, SE and a reported p all
# co-occur and no test statistic was parsed, create a regression result and
# synthesise the coefficient t = b / SE. All three are required so an incidental
# b/SE co-occurrence cannot spuriously create a result; df is unknown, so the
# row is an honest NOTE.

reg_row <- function(res) {
  if (!"test_type" %in% names(res)) return(res[0, , drop = FALSE])
  res[!is.na(res$test_type) & res$test_type == "regression", , drop = FALSE]
}

test_that("v0.5.6: a bare b/SE/p regression line is detected", {
  res <- check_text("The predictor was significant, b = 0.45, SE = 0.12, p = .001.")
  row <- reg_row(res)
  expect_equal(nrow(row), 1)
  expect_equal(row$stat_value[1], 0.45 / 0.12, tolerance = 1e-6)  # synthesised t = b/SE
  expect_equal(row$p_reported[1], 0.001, tolerance = 1e-9)
})

test_that("v0.5.6: a bare regression line with no df is an honest NOTE", {
  res <- check_text("b = -0.30, SE = 0.10, p = .003")
  row <- reg_row(res)
  expect_equal(nrow(row), 1)
  expect_equal(row$status[1], "NOTE")
})

test_that("v0.5.6 guard: b + SE with a co-reported t-statistic still works", {
  res <- check_text("The coefficient, b = 0.45, SE = 0.12, t(98) = 3.75, p = .001")
  row <- reg_row(res)
  expect_equal(nrow(row), 1)
  expect_equal(row$df1[1], 98)
})

test_that("v0.5.6 guard: b + SE without a reported p does NOT create a result", {
  # the p requirement prevents an incidental b/SE co-occurrence from spuriously
  # creating a regression row.
  res <- check_text("Each box had b = 5 items and SE = 2 issues recorded.")
  expect_equal(nrow(reg_row(res)), 0)
})
