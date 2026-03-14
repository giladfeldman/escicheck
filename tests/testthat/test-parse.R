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
  expect_false(grepl(";", normalized))
  expect_true(grepl("0\\.12, 0\\.45", normalized))
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
  text <- "t(28) = 2.21, d = 0.80, 95% CI [0.12, 1.48]"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$ci_level, 0.95)
  expect_equal(result$ciL_reported, 0.12)
  expect_equal(result$ciU_reported, 1.48)
})

test_that("parse_text extracts CI without level", {
  text <- "t(28) = 2.21, d = 0.80, CI [0.12, 1.48]"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  # "CI" without explicit % may still default to 95% -- accept either NA or 0.95
  expect_true(is.na(result$ci_level) || result$ci_level == 0.95)
  expect_equal(result$ciL_reported, 0.12)
  expect_equal(result$ciU_reported, 1.48)
})

test_that("parse_text extracts CI with parentheses", {
  text <- "t(28) = 2.21, d = 0.80, (0.12, 0.45)"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$ciL_reported, 0.12)
  expect_equal(result$ciU_reported, 0.45)
})

test_that("parse_text extracts CI with semicolon", {
  text <- "t(28) = 2.21, d = 0.80, (0.12; 0.45)"
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
  text <- "t(28) = 2.21, d = .45, CI [.12, .78]"
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

# ===========================================================================
# Subscript notation normalization tests
# ===========================================================================

test_that("normalize_text converts t subscript notation", {
  text <- "t754 = -33.00, p < .001"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("t\\(754\\)", normalized))
})

test_that("normalize_text converts r subscript notation", {
  text <- "r757 = 0.34, p < .001"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("r\\(757\\)", normalized))
})

test_that("normalize_text converts F subscript notation", {
  text <- "F1,200 = 5.32, p = .02"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F\\(1, 200\\)", normalized))
})

test_that("subscript t-test is parseable after normalization", {
  text <- "t754 = -33.00, p < .001"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$test_type[1], "t")
  expect_equal(result$df1[1], 754)
})

test_that("subscript F-test is parseable after normalization", {
  text <- "F1,200 = 5.32, p = .02"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$test_type[1], "F")
  expect_equal(result$df1[1], 1)
  expect_equal(result$df2[1], 200)
})

# ===========================================================================
# Spaced-df normalization tests
# ===========================================================================

test_that("normalize_text collapses spaced df in t-test", {
  text <- "t(4 2 1) = -0.22, p = .83"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("t\\(421\\)", normalized))
})

test_that("spaced-df t-test is parseable after normalization", {
  text <- "t(4 2 1) = -0.22, p = .83"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$test_type[1], "t")
  expect_equal(result$df1[1], 421)
})

# ===========================================================================
# eta2p notation normalization test
# ===========================================================================

test_that("normalize_text converts eta2p to partial eta-squared", {
  text <- "eta2p = 0.03"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("partial eta-squared", normalized))
})

# ===========================================================================
# P-value pattern tests
# ===========================================================================

test_that("parse_text handles p with leading zero", {
  text <- "t(28) = 2.21, p = 0.035"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$p_reported[1], 0.035)
})

test_that("parse_text extracts p_symbol for inequality", {
  text <- "t(28) = 5.50, p < .001"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$p_symbol[1], "<")
})

# ===========================================================================
# Smart/curly quote normalization tests
# ===========================================================================

test_that("normalize_text converts smart double quotes to straight", {
  text <- "\u201CHedges\u2019 g = 0.75\u201D"
  normalized <- effectcheck:::normalize_text(text)
  expect_false(grepl("\u201C", normalized))
  expect_false(grepl("\u201D", normalized))
  expect_true(grepl('"', normalized))
})

test_that("normalize_text converts curly apostrophe to straight", {
  text <- "Cohen\u2019s d = 0.80"
  normalized <- effectcheck:::normalize_text(text)
  expect_false(grepl("\u2019", normalized))
  expect_true(grepl("Cohen's d", normalized))
})

test_that("smart apostrophe in Hedges' g does not break parsing", {
  text <- "Hedges\u2019 g = 0.75, t(28) = 2.21, p = .035"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$effect_reported_name[1], "g")
  expect_equal(result$effect_reported[1], 0.75)
})

# ===========================================================================
# Mathematical symbol normalization tests (less/greater-than-or-equal)
# ===========================================================================

test_that("normalize_text converts less-than-or-equal", {
  text <- "p \u2264 .05"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("<=", normalized))
  expect_false(grepl("\u2264", normalized))
})

test_that("normalize_text converts greater-than-or-equal", {
  text <- "p \u2265 .10"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl(">=", normalized))
  expect_false(grepl("\u2265", normalized))
})

test_that("p-value with less-than-or-equal symbol is parsed correctly", {
  text <- "t(28) = 2.21, p \u2264 .05"
  result <- parse_text(text)
  expect_equal(nrow(result), 1)
  expect_equal(result$p_symbol[1], "<=")
  expect_equal(result$p_reported[1], 0.05)
})

# ===========================================================================
# ff ligature normalization test
# ===========================================================================

test_that("normalize_text expands ff ligature", {
  text <- "The e\uFB00ect size was d = 0.80"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("effect", normalized))
  expect_false(grepl("\uFB00", normalized))
})

# ===========================================================================
# Superscript normalization test
# ===========================================================================

test_that("normalize_text converts superscript 2 to ^2", {
  text <- "\u03c7\u00b2(1) = 7.2"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("\\^2", normalized))
  expect_false(grepl("\u00b2", normalized))
})

# ===========================================================================
# Statistical expression spacing tests
# ===========================================================================

test_that("normalize_text inserts comma before p-value when missing", {
  text <- "= 2.21p = .035"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("2\\.21, p", normalized))
})

test_that("normalize_text inserts comma with space gap before p-value", {
  text <- "= 2.21 p = .035"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("2\\.21, p", normalized))
})

test_that("normalize_text does not double-insert comma before p-value", {
  text <- "= 2.21, p = .035"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("2\\.21, p", normalized))
  expect_false(grepl(",,", normalized))
})

# ===========================================================================
# Mid-sentence line-break joining tests
# ===========================================================================

test_that("normalize_text joins mid-sentence line breaks", {
  text <- "the effect was\nsmall and significant"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("was small", normalized))
})

test_that("normalize_text preserves line breaks before capitalized words", {
  text <- "significant result.\nThe next study"
  normalized <- effectcheck:::normalize_text(text)
  # Should NOT join because next line starts with uppercase T
  expect_true(grepl("\n", normalized))
})

# ===========================================================================
# PDF two-column interleaving artifact fixes
# ===========================================================================

test_that("normalize_text fixes F-test column interleaving: F smaller. (2, 430)", {
  text <- "temporal distance, F smaller. (2, 430) = 22.83, p < 0.001"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F(2, 430) = 22.83", normalized, fixed = TRUE))
  expect_false(grepl("smaller", normalized))
})

test_that("normalize_text fixes F-test column interleaving: F test. (1, 200)", {
  text <- "we found that F test. (1, 200) = 5.32"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F(1, 200) = 5.32", normalized, fixed = TRUE))
})

test_that("normalize_text fixes t-test column interleaving: t value. (28)", {
  text <- "result was t value. (28) = 2.21, p = .035"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("t(28) = 2.21", normalized, fixed = TRUE))
})

test_that("normalize_text does NOT corrupt normal F followed by parenthesized df", {
  # Already correct APA format should be preserved
  text <- "F(2, 430) = 22.83, p < 0.001"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F(2, 430) = 22.83", normalized, fixed = TRUE))
})

test_that("normalize_text handles multi-word interleaving artifact", {
  text <- "the effect was F smaller and more. (2, 875) = 34.76"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F(2, 875) = 34.76", normalized, fixed = TRUE))
})

# --- U+FFFD replacement character as minus sign ---

test_that("normalize_text converts U+FFFD to hyphen-minus", {
  text <- "t(44) = \uFFFD4.54, p <.001, dz = \uFFFD0.75"
  normalized <- effectcheck:::normalize_text(text)
  expect_false(grepl("\uFFFD", normalized))
  expect_true(grepl("t(44) = -4.54", normalized, fixed = TRUE))
  expect_true(grepl("dz = -0.75", normalized, fixed = TRUE))
})

test_that("normalize_text handles U+FFFD in effect sizes and CIs", {
  text <- "d = \uFFFD1.00 [\uFFFD1.33, \uFFFD0.67]"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("d = -1.00 [-1.33, -0.67]", normalized, fixed = TRUE))
})

# --- Period separator in F-test df ---

test_that("normalize_text fixes period separator in F-test df: F(1.45) -> F(1, 45)", {
  text <- "F(1.45) = 8.33, p = .006"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F(1, 45) = 8.33", normalized, fixed = TRUE))
})

test_that("normalize_text fixes period separator with larger df2: F(1.420)", {
  text <- "F(1.420) = 12.59, p < .001"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F(1, 420) = 12.59", normalized, fixed = TRUE))
})

test_that("normalize_text does NOT corrupt F-test with proper comma separator", {
  text <- "F(1, 433) = 105.23, p < .001"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F(1, 433) = 105.23", normalized, fixed = TRUE))
})

test_that("normalize_text does NOT corrupt F-test with fractional df (GG-corrected)", {
  # Greenhouse-Geisser corrected: df1 has real decimal, e.g., F(2.34, 100.8)
  # This should NOT be changed because df2 uses comma separator already
  text <- "F(2.34, 100.8) = 5.67"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F(2.34, 100.8) = 5.67", normalized, fixed = TRUE))
})

# --- dz false positive guard ---

test_that("z-test pattern does not match dz (Cohen's d paired)", {
  results <- effectcheck::check_text("dz = 1.43, 95% CI [0.69, 2.16]")
  # dz is an effect size, not a z-test; should not be detected
  expect_equal(nrow(results), 0)
})

test_that("z-test pattern still matches standalone z = value", {
  results <- effectcheck::check_text("z = 2.45, p = .014")
  expect_equal(nrow(results), 1)
  expect_equal(results$test_type[1], "z")
})

# --- Stripped chi-square symbol ---

test_that("normalize_text recovers stripped chi-square: bare 2(df) = value", {
  text <- "leaving 79% still claiming ( 2 (1, N = 43) = 14.53, p < .001"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("chi-square(1) = 14.53", normalized, fixed = TRUE))
})

test_that("stripped chi-square is detected as chi-square test", {
  text <- "( 2 (1, N = 43) = 14.53, p < .001, dz = 1.43"
  results <- effectcheck::check_text(text)
  chisq_rows <- results[results$test_type == "chisq", ]
  expect_true(nrow(chisq_rows) >= 1)
  expect_equal(chisq_rows$stat_value[1], 14.53)
})

# --- Subscript F with decimal df (GG-corrected) ---

test_that("normalize_text fixes subscript F with decimal df: F1.87, 654.3 = 37.32", {
  text <- "differences between versions (F1.87, 654.3 = 37.32, p < .001)"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("F(1.87, 654.3) = 37.32", normalized, fixed = TRUE))
})

test_that("subscript F with decimal df is detected as F-test", {
  text <- "F1.87, 654.3 = 37.32, p < .001"
  results <- effectcheck::check_text(text)
  f_rows <- results[results$test_type == "F", ]
  expect_true(nrow(f_rows) >= 1)
  expect_equal(f_rows$stat_value[1], 37.32)
  expect_equal(f_rows$df1[1], 1.87)
})

# --- Space before minus sign in stat values ---

test_that("normalize_text collapses space between sign and number: = - 3.79", {
  text <- "t(399) = - 3.79, p < .001, d = - 0.38"
  normalized <- effectcheck:::normalize_text(text)
  expect_true(grepl("= -3.79", normalized, fixed = TRUE))
  expect_true(grepl("= -0.38", normalized, fixed = TRUE))
})

test_that("space-before-minus t-tests are detected", {
  text <- "t(399) = - 3.79, p < .001, d = - 0.38, 95% CI [- 0.58, - 0.18]"
  results <- effectcheck::check_text(text)
  t_rows <- results[results$test_type == "t", ]
  expect_true(nrow(t_rows) >= 1)
  expect_equal(t_rows$stat_value[1], -3.79)
})

# --- Multiple z-tests in one sentence (sub-chunking) ---

test_that("multiple z-tests in one sentence are all detected", {
  text <- "Problem 4 (z = 4.18, p < .001) and Problem 5 (z = 5.99, p < .001). Problem 7 (z = -6.37, p = 1.00) and Problem 8 (z = -10.00, p = 1.00)."
  results <- effectcheck::check_text(text)
  z_rows <- results[results$test_type == "z", ]
  expect_equal(nrow(z_rows), 4)
})
