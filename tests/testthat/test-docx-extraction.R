# =============================================================================
# DOCX extraction tests
# Tests for DOCX-specific extraction via pandoc and downstream parsing
# =============================================================================

# =============================================================================
# SECTION 1: Pandoc availability
# =============================================================================

test_that("pandoc is available on PATH", {
  pandoc <- Sys.which("pandoc")
  expect_true(nchar(pandoc) > 0, info = "pandoc must be installed for DOCX extraction")
})

# =============================================================================
# SECTION 2: DOCX inline statistical text
# =============================================================================

test_that("DOCX-style inline text with eta-squared parses correctly", {
  # Simulates pandoc plain text output with preserved Unicode
  text <- paste0(
    "We found support for a main effect of fame ",
    "(F(1, 1180) = 55.90, p < .001, \u03b7\u00b2 = 0.04, 90% CI [0.02, 0.06]) ",
    "and valence (F(2, 1180) = 181.2, p < .001, \u03b7\u00b2 = 0.22, 90% CI [0.19, 0.26]).")
  res <- check_text(text)
  expect_equal(nrow(res), 2)
  expect_true(all(res$test_type == "F"))
  expect_true(all(res$effect_reported_name == "eta2"))
  expect_equal(res$effect_reported[1], 0.04)
  expect_equal(res$effect_reported[2], 0.22)
})

test_that("DOCX-style t-test with Cohen's d parses correctly", {
  text <- "Participants valued items more (t(1180) = 7.47, p < .001, d = 0.43, 95% CI [0.31, 0.55])."
  res <- check_text(text)
  expect_equal(res$test_type[1], "t")
  expect_equal(res$stat_value[1], 7.47)
  expect_equal(res$effect_reported[1], 0.43)
})

test_that("DOCX-style chi-square with Cramer's V parses correctly", {
  text <- "\u03c7\u00b2(1) = 12.484, p < .001, Cramer's V = .31"
  res <- check_text(text)
  expect_equal(res$test_type[1], "chisq")
  expect_equal(res$stat_value[1], 12.484)
})

# =============================================================================
# SECTION 3: DOCX table pipe formatting
# =============================================================================

test_that("pipe-delimited stats from DOCX tables are parsed", {
  # Pandoc outputs DOCX tables with | delimiters
  text <- "| Study | Statistics | Effect |\n| Original | \u03c7\u00b2(1) = 5.23, p = .022 | V = .18 |"
  res <- check_text(text)
  expect_true(any(res$test_type == "chisq"))
})

test_that("pipe-delimited F-test with effect size from DOCX table", {
  text <- "| ANOVA | F(2, 150) = 8.34, p < .001 | \u03b7\u00b2 = 0.10 |"
  res <- check_text(text)
  expect_equal(res$test_type[1], "F")
  expect_equal(res$stat_value[1], 8.34)
})

test_that("multi-row pipe-delimited table preserves stats", {
  text <- paste(
    "| Condition | F-test | p |",
    "| Group A | F(1, 80) = 4.50, p = .037 | sig |",
    "| Group B | F(1, 80) = 12.10, p < .001 | sig |",
    sep = "\n")
  res <- check_text(text)
  expect_equal(nrow(res), 2)
  expect_equal(res$stat_value[1], 4.50)
  expect_equal(res$stat_value[2], 12.10)
})

# =============================================================================
# SECTION 4: DOCX track changes (pandoc strips these by default)
# =============================================================================

test_that("track changes don't corrupt statistical text", {
  # Pandoc --wrap=none strips track changes; simulated clean output
  text <- "The result was significant, F(1, 200) = 15.30, p < .001, d = 0.55."
  res <- check_text(text)
  expect_equal(res$test_type[1], "F")
  expect_equal(res$stat_value[1], 15.30)
})

# =============================================================================
# SECTION 5: DOCX-specific Unicode preservation
# =============================================================================

test_that("DOCX preserves partial eta-squared Unicode correctly", {
  text <- "F(2, 300) = 9.87, p < .001, \u03b7p\u00b2 = 0.06, 90% CI [0.02, 0.10]"
  res <- check_text(text)
  expect_equal(res$test_type[1], "F")
  expect_equal(res$effect_reported_name[1], "etap2")
  expect_equal(res$effect_reported[1], 0.06)
})

test_that("DOCX preserves omega-squared Unicode correctly", {
  text <- "F(1, 150) = 7.20, p = .008, \u03c9\u00b2 = 0.04"
  res <- check_text(text)
  expect_equal(res$test_type[1], "F")
  expect_equal(res$effect_reported_name[1], "omega2")
  expect_equal(res$effect_reported[1], 0.04)
})
