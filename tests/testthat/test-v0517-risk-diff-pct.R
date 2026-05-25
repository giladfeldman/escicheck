# v0.5.17: risk-difference percent with CI (clinical-trial noninferiority)
# Form: "risk difference <val>%; 95% [confidence interval (CI)|CI] <lo> to
# <hi>; ... P = <pval>". From PLOS Medicine PROSECCO trial. Full
# Farrington-Manning noninferiority verification deferred to v0.6.x.

test_that("v0.5.17: risk-difference with full CI phrase + P=", {
  txt <- "risk difference -1.01%; 95% confidence interval (CI) -10.36 to 8.34; noninferiority, P = 0.09"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "rdpct")
  expect_equal(res$stat_value, -1.01)
  expect_equal(res$p_reported, 0.09)
})

test_that("v0.5.17: risk-difference with short CI phrase", {
  txt <- "(risk difference -1.01%; 95% CI -10.36 to 8.34)"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "rdpct")
  expect_equal(res$stat_value, -1.01)
})

test_that("v0.5.17: risk-difference positive value", {
  txt <- "The risk difference was 5.25%; 95% CI 1.2 to 9.3 (p-value 0.013)"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "rdpct")
  expect_equal(res$stat_value, 5.25)
})

test_that("v0.5.17: rdpct row carries NOTE-level uncertainty", {
  txt <- "risk difference 2.0%; 95% CI -3.0 to 7.0; p = 0.50"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_true(grepl("risk[- ]?difference|noninferiority|v0\\.6\\.x",
                    res$uncertainty_reasons %||% "", ignore.case = TRUE))
})
