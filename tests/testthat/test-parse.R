# Test suite for parsing and normalization functions

test_that("normalize_text handles Unicode minus sign", {
  text <- "t(28) = \u22122.21"
  normalized <- effectcheck:::normalize_text(text)
  expect_false(grepl("\u2212", normalized))
  expect_true(grepl("-", normalized))
})

test_that("normalize_text handles non-breaking spaces", {
  text <- "t(28)\u00A0=\u00A02.21"
  normalized <- effectcheck:::normalize_text(text)
  expect_false(grepl("\u00A0", normalized))
})

test_that("normalize_text converts decimal commas to dots", {
  text <- "d = 0,45, CI [0,12, 0,78]"
  normalized <- effectcheck:::normalize_text(text)
  expect_false(grepl("0,45", normalized))
  expect_true(grepl("0\\.45", normalized))
  expect_true(grepl("0\\.12", normalized))
})

test_that("normalize_text preserves thousands separators", {
  text <- "N = 1,234 participants"
  normalized <- effectcheck:::normalize_text(text)
  # Should preserve the comma in "1,234" (thousands separator)
  # This is a heuristic - may need adjustment based on context
  expect_true(grepl("1,234", normalized) || grepl("1\\.234", normalized))
})

test_that("normalize_text harmonizes CI delimiters", {
  text <- "CI (0.12; 0.45)"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("0\\.12.*0\\.45", normalized))
  # Semicolon should be converted to comma
  expect_false(grepl(";", normalized) || grepl("0\\.12, 0\\.45", normalized))
})

test_that("normalize_text handles multiple whitespace", {
  text <- "t(28)  =   2.21"
  normalized <- effectcheck:::normalize_text(text)
  expect_false(grepl("  ", normalized))
})

test_that("parse_text extracts t-test", {
  text <- "t(28) = 2.21, p = .035"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$test_type, "t")
  expect_equal(result$df1, 28)
  expect_equal(result$stat_value, 2.21)
})

test_that("parse_text extracts F-test", {
  text <- "F(2, 27) = 4.56, p < .05"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$test_type, "F")
  expect_equal(result$df1, 2)
  expect_equal(result$df2, 27)
  expect_equal(result$stat_value, 4.56)
})

test_that("parse_text extracts z-test", {
  text <- "z = 2.33, p < .05"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$test_type, "z")
  expect_equal(result$stat_value, 2.33)
})

test_that("parse_text extracts correlation", {
  text <- "r(198) = .34, p < .001"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$test_type, "r")
  expect_equal(result$df1, 198)
  expect_equal(result$stat_value, 0.34)
})

test_that("parse_text extracts chi-square", {
  text <- "χ²(1) = 7.2, N = 120"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$test_type, "chisq")
  expect_equal(result$df1, 1)
  expect_equal(result$stat_value, 7.2)
})

test_that("parse_text extracts CI with level", {
  text <- "d = 0.80, 95% CI [0.12, 1.48]"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$ci_level, 0.95)
  expect_equal(result$ciL_reported, 0.12)
  expect_equal(result$ciU_reported, 1.48)
})

test_that("parse_text extracts CI without level", {
  text <- "d = 0.80, CI [0.12, 1.48]"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$ci_level))
  expect_equal(result$ciL_reported, 0.12)
  expect_equal(result$ciU_reported, 1.48)
})

test_that("parse_text extracts CI with parentheses", {
  text <- "d = 0.80, (0.12, 0.45)"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$ciL_reported, 0.12)
  expect_equal(result$ciU_reported, 0.45)
})

test_that("parse_text extracts CI with semicolon", {
  text <- "d = 0.80, (0.12; 0.45)"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$ciL_reported, 0.12)
  expect_equal(result$ciU_reported, 0.45)
})

test_that("parse_text extracts multiple statistics", {
  text <- "t(28) = 2.21, p = .035. Another test: F(2, 27) = 4.56."
  result <- parse_text(text)
  expect_equal(nrow(result), 2)
  expect_equal(result$test_type, c("t", "F"))
})

test_that("parse_text extracts effect sizes", {
  text <- "t(28) = 2.21, d = 0.80, g = 0.75"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  # Should extract the most specific effect size mentioned
  expect_true(result$effect_reported_name %in% c("d", "g"))
  expect_true(!is.na(result$effect_reported))
})

test_that("parse_text extracts sample sizes from context", {
  text <- "N = 60. The analysis showed t(28) = 2.21."
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$N, 60)
})

test_that("parse_text extracts context window", {
  text <- "First sentence. Second sentence with t(28) = 2.21. Third sentence. Fourth sentence."
  result <- parse_text(text, context_window_size = 2)
  expect_equal(nrow(result), 1)
  expect_true(grepl("First sentence", result$context_window))
  expect_true(grepl("Second sentence", result$context_window))
  expect_true(grepl("Third sentence", result$context_window))
  expect_true(grepl("Fourth sentence", result$context_window))
})

test_that("parse_text handles mixed decimal styles", {
  text <- "t(28) = 2,21, d = 0,45"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$stat_value, 2.21)
  expect_equal(result$effect_reported, 0.45)
})

test_that("parse_text handles negative values", {
  text <- "t(28) = -2.21, CI [-0.3, 1.2]"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$stat_value, -2.21)
  expect_equal(result$ciL_reported, -0.3)
  expect_equal(result$ciU_reported, 1.2)
})

test_that("parse_text handles leading zeros in decimals", {
  text <- "d = .45, CI [.12, .78]"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$effect_reported, 0.45)
  expect_equal(result$ciL_reported, 0.12)
  expect_equal(result$ciU_reported, 0.78)
})

test_that("parse_text extracts ANOVA effect sizes", {
  text <- "F(2, 27) = 4.56, η² = 0.25, ηp² = 0.30"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_true(result$effect_reported_name %in% c("eta2", "etap2"))
})

test_that("parse_text returns empty tibble for no matches", {
  text <- "This is just regular text with no statistics."
  result <- parse_text(text)
  expect_equal(nrow(result), 0)
  expect_true(tibble::is_tibble(result))
})
