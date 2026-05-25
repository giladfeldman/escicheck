# v0.5.18: median-difference (Hodges-Lehmann) with IQR + CI
# Form: "median difference <val>; 95% CI <lo> to <hi>; p[-value]? = <pval>"
# From PLOS Medicine PROSECCO trial. HL estimate verification is out of
# scope for sentence-level extraction.

test_that("v0.5.18: median-difference with p-value", {
  txt <- "2.0 (0.0 to 4.0) versus 0.0 (0.0 to 2.0); median difference 0.0; 95% CI 0.0 to 1.0; p-value 0.027"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "md_hl")
  expect_equal(res$stat_value, 0.0)
  expect_equal(res$p_reported, 0.027)
})

test_that("v0.5.18: median-difference negative value + p<", {
  txt <- "240.5 (185.0 to 335.0) minutes versus 386 (320.0 to 447.0) minutes; median difference -140.0; 95% CI -169.0 to -109.0; p < 0.0001"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "md_hl")
  expect_equal(res$stat_value, -140.0)
})

test_that("v0.5.18: md_hl row carries NOTE-level uncertainty", {
  txt <- "median difference 1.5; 95% CI -0.3 to 3.5; p = 0.08"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_true(grepl("Hodges-Lehmann|median[- ]?difference|rank data",
                    res$uncertainty_reasons %||% "", ignore.case = TRUE))
})
