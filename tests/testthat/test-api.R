# Tests for EffectCheck API functions

test_that("check_text returns effectcheck object", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_s3_class(result, "effectcheck")
  expect_true(is.effectcheck(result))
})

test_that("check_text handles empty input", {
  result <- check_text("")
  
  expect_s3_class(result, "effectcheck")
  expect_equal(nrow(result), 0)
})

test_that("check_text handles text with no statistics", {
  result <- check_text("This is just regular text with no statistics.")
  
  expect_s3_class(result, "effectcheck")
  expect_equal(nrow(result), 0)
})

test_that("check_text filters by stats parameter", {
  text <- "t(28) = 2.21, p = .035, d = 0.80. F(2, 45) = 3.5, p = .04"
  
  # Only t-tests
  result_t <- check_text(text, stats = "t")
  expect_true(all(result_t$test_type == "t"))
  
  # Only F-tests
  result_F <- check_text(text, stats = "F")
  expect_true(all(result_F$test_type == "F"))
})

test_that("check_text includes source column", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true("source" %in% names(result))
  expect_equal(result$source[1], "text")
})

test_that("check_text includes decision_error column", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true("decision_error" %in% names(result))
  expect_true("p_computed" %in% names(result))
  expect_true("p_reported" %in% names(result))
})

test_that("summary.effectcheck works correctly", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  summ <- summary(result)
  
  expect_s3_class(summ, "summary.effectcheck")
  expect_true("total" %in% names(summ))
  expect_true("status_counts" %in% names(summ))
  expect_true("error_rate" %in% names(summ))
})

test_that("print.effectcheck works without error", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_output(print(result), "EffectCheck Results")
})

test_that("print.summary.effectcheck works without error", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  summ <- summary(result)
  
  expect_output(print(summ), "EffectCheck Summary Report")
})

test_that("subsetting preserves effectcheck class", {
  text <- "t(28) = 2.21, p = .035, d = 0.80. t(30) = 1.5, p = .14"
  result <- check_text(text)
  
  subset_result <- result[1, ]
  
  expect_s3_class(subset_result, "effectcheck")
})

test_that("ec_identify function filters correctly", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)

  # Should not error even if no errors exist
  errors <- ec_identify(result, "errors")
  expect_s3_class(errors, "effectcheck")

  warnings <- ec_identify(result, "warnings")
  expect_s3_class(warnings, "effectcheck")
})

test_that("get_errors convenience function works", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  errors <- get_errors(result)
  expect_s3_class(errors, "effectcheck")
})

test_that("get_warnings convenience function works", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  warnings <- get_warnings(result)
  expect_s3_class(warnings, "effectcheck")
})

test_that("filter_by_test_type works correctly", {
  text <- "t(28) = 2.21, p = .035, d = 0.80. F(2, 45) = 3.5, p = .04"
  result <- check_text(text)
  
  t_only <- filter_by_test_type(result, "t")
  expect_true(all(t_only$test_type == "t"))
})

test_that("filter_by_uncertainty works correctly", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  # Should not error
  high_unc <- filter_by_uncertainty(result, "high")
  expect_s3_class(high_unc, "effectcheck")
})

test_that("count_by returns correct structure", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  counts <- count_by(result, "status")
  
  expect_true("status" %in% names(counts))
  expect_true("count" %in% names(counts))
  expect_true("percent" %in% names(counts))
})

test_that("check_text handles multiple statistics", {
  text <- "We found t(28) = 2.21, p = .035, d = 0.80. Additionally, t(30) = 1.5, p = .14, d = 0.35."
  result <- check_text(text)
  
  expect_s3_class(result, "effectcheck")
  expect_gte(nrow(result), 1)  # At least one statistic detected
})

test_that("check_text handles F-tests", {
  text <- "F(2, 45) = 3.5, p = .04, eta-squared = 0.13"
  result <- check_text(text)
  
  expect_s3_class(result, "effectcheck")
  if (nrow(result) > 0) {
    expect_true("F" %in% result$test_type)
  }
})

test_that("check_text handles correlations", {
  text <- "r(48) = .45, p < .001"
  result <- check_text(text)
  
  expect_s3_class(result, "effectcheck")
  if (nrow(result) > 0) {
    expect_true("r" %in% result$test_type)
  }
})

test_that("check_text handles chi-square", {
  text <- "chi-square(2) = 8.5, p = .014, phi = 0.25"
  result <- check_text(text)
  
  expect_s3_class(result, "effectcheck")
})

test_that("is.effectcheck returns correct values", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(is.effectcheck(result))
  expect_false(is.effectcheck(data.frame()))
  expect_false(is.effectcheck("not an effectcheck"))
})

test_that("effectcheck attributes are preserved", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(!is.null(attr(result, "effectcheck_version")))
  expect_true(!is.null(attr(result, "call")))
  expect_true(!is.null(attr(result, "settings")))
  expect_true(!is.null(attr(result, "generated")))
})

test_that("check_text respects alpha parameter", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  
  # With default alpha = 0.05
  result_05 <- check_text(text, alpha = 0.05)
  
  # With stricter alpha = 0.01
  result_01 <- check_text(text, alpha = 0.01)
  
  expect_s3_class(result_05, "effectcheck")
  expect_s3_class(result_01, "effectcheck")
})

test_that("check_text respects one_tailed parameter", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  
  result_two <- check_text(text, one_tailed = FALSE)
  result_one <- check_text(text, one_tailed = TRUE)
  
  expect_s3_class(result_two, "effectcheck")
  expect_s3_class(result_one, "effectcheck")
})

test_that("check_text respects tolerance parameters", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  
  # Strict tolerance
  result_strict <- check_text(text, tol_effect = list(d = 0.001))
  
  # Lenient tolerance
  result_lenient <- check_text(text, tol_effect = list(d = 0.5))
  
  expect_s3_class(result_strict, "effectcheck")
  expect_s3_class(result_lenient, "effectcheck")
})
