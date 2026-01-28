# Golden examples regression tests

test_that("Golden example 1: Basic t-test", {
  text <- "t(28) = 2.21, p = .035, d = 0.80, 95% CI [0.12, 1.48], N = 30, n1 = 15, n2 = 15"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "t")
  expect_equal(result$df1[1], 28)
  expect_equal(result$stat_value[1], 2.21)
})

test_that("Golden example 2: Correlation", {
  text <- "r(198) = .34, p < .001, 95% CI [.21, .45]"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "r")
  expect_equal(result$ci_level[1], 0.95)
})

test_that("Golden example 3: Chi-square", {
  text <- "χ²(1) = 7.2, N = 120, phi = .10"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "chisq")
})

test_that("Golden example 4: F-test / ANOVA", {
  text <- "F(2, 27) = 4.56, p < .05, η² = 0.25, ηp² = 0.30"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "F")
  expect_equal(result$df1[1], 2)
  expect_equal(result$df2[1], 27)
})

test_that("Golden example 5: Paired t-test", {
  text <- "A paired-samples comparison yielded t(39) = 3.0, p = .004, d = 0.45."
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "t")
  # Should infer paired design
  if (!is.na(result$design_inferred[1])) {
    expect_true(result$design_inferred[1] %in% c("paired", "unclear"))
  }
})

test_that("Golden example 6: Mixed decimal styles", {
  text <- "t(28) = 2,21, d = 0,45, CI [0,12; 0,78]"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  # Should handle decimal commas and semicolon in CI
  expect_equal(result$stat_value[1], 2.21)
})

test_that("Golden example 7: CI without level", {
  text <- "d = 0.80, CI [0.12, 1.48]"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true(!is.na(result$ciL_reported[1]))
    expect_true(!is.na(result$ciU_reported[1]))
  }
})

test_that("Golden example 8: Multiple stats", {
  text <- "t(28) = 2.21, p = .035, d = 0.80, 95% CI [0.12, 1.48]. Another test: F(2, 27) = 4.56."
  result <- check_text(text)
  expect_true(nrow(result) >= 2)
})

test_that("Golden example 9: z-test", {
  text <- "z = 2.33, p < .05"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "z")
})

test_that("Golden example 10: Regression", {
  text <- "The regression model was significant, F(3, 96) = 8.45, p < .001, R² = 0.21."
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "F")
})

test_that("Golden example 11: df but no N", {
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  # Should flag uncertainty about sample size
  if (nrow(result) > 0 && !is.na(result$uncertainty_level[1])) {
    expect_true(result$uncertainty_level[1] %in% c("medium", "high"))
  }
})

test_that("Golden example 12: CI before effect", {
  text <- "95% CI [0.12, 1.48], d = 0.80, t(28) = 2.21"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$ci_level[1], 0.95)
})

test_that("Golden example 13: Negative values", {
  text <- "t(28) = -2.21, CI [-0.3, 1.2]"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$stat_value[1], -2.21)
  expect_equal(result$ciL_reported[1], -0.3)
})

test_that("Golden example 14: Leading zeros", {
  text <- "d = .45, CI [.12, .78]"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_equal(result$effect_reported[1], 0.45)
    expect_equal(result$ciL_reported[1], 0.12)
  }
})

test_that("Golden example 15: Unicode minus", {
  text <- "t(28) = −2.21, d = 0.80"  # Unicode minus U+2212
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$stat_value[1], -2.21)
})
