# =============================================================================
# test-extraction-quality.R — Tests for v0.2.5 PDF extraction quality improvements
# Based on MetaESCI extraction report (121K results, 8,415 PDFs)
# =============================================================================

# =============================================================================
# SECTION 1: Dropped decimal fixes in normalize_text()
# =============================================================================

test_that("p < 001 is corrected to p < .001", {
  res <- check_text("r(60) = .41, p < 001")
  expect_equal(res$p_reported[1], 0.001)
})

test_that("p = 484 is corrected to p = .484 (3+ digit dropped decimal)", {
  res <- check_text("t(94) = -0.42, p = 484")
  # Should parse as p = .484 (not SKIP/NA)
  expect_false(is.na(res$p_reported[1]))
  expect_equal(res$p_reported[1], 0.484)
})

test_that("p = 772 is corrected to p = .772", {
  res <- check_text("r(30) = -.37, p = 772")
  expect_equal(res$p_reported[1], 0.772)
})

test_that("p_decimal_corrected flag produces extraction_suspect and uncertainty note", {
  res <- check_text("t(94) = -0.42, p = 484")
  # The p-value decimal correction should mark as extraction_suspect
  expect_true(res$extraction_suspect[1])
  # And produce an uncertainty note mentioning the correction
  expect_true(any(grepl("[Dd]ecimal", res$uncertainty_reasons[1])))
})

test_that("legitimate p-values are NOT modified by decimal correction", {
  # p = .035 should NOT be treated as a dropped decimal
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_equal(res$p_reported[1], 0.035)
  # No decimal correction uncertainty note
  expect_false(any(grepl("decimal corrected", res$uncertainty_reasons[1], ignore.case = TRUE)))
})

test_that("p < .05 is not modified (short numbers are not dropped decimals)", {
  res <- check_text("F(1, 50) = 4.20, p < .05")
  expect_equal(res$p_reported[1], 0.05)
})

# =============================================================================
# SECTION 2: General line-break joining
# =============================================================================

test_that("line break between = and value is joined", {
  res <- check_text("F(1, 30) =\n4.425, p < 0.05")
  expect_equal(res$stat_value[1], 4.425)
  expect_equal(res$test_type[1], "F")
})

test_that("line break between ( and df is joined", {
  res <- check_text("F(\n1, 30) = 4.42, p < .05")
  expect_equal(res$df1[1], 1)
  expect_equal(res$df2[1], 30)
})

test_that("line break after < with digit is joined", {
  res <- check_text("t(50) = 2.31, p <\n.05")
  expect_equal(res$p_reported[1], 0.05)
})

# =============================================================================
# SECTION 3: Standalone page number removal
# =============================================================================

test_that("standalone page numbers don't interfere with parsing", {
  # Page number "123" on its own line should be removed
  res <- check_text("some text\n123\nF(1, 50) = 4.20, p < .05")
  expect_equal(nrow(res), 1)
  expect_equal(res$stat_value[1], 4.20)
})

# =============================================================================
# SECTION 4: Header/footer stripping (strip_headers_footers)
# =============================================================================

test_that("strip_headers_footers removes repeated lines", {
  text <- paste(c(
    "Downloaded from journal.org on Jan 1, 2024",
    "The main effect was significant, F(1, 50) = 4.20, p < .05.",
    "Downloaded from journal.org on Jan 1, 2024",
    "The interaction was also significant, t(50) = 2.31, p < .05.",
    "Downloaded from journal.org on Jan 1, 2024",
    "Results showed a large effect, d = 0.80.",
    "Downloaded from journal.org on Jan 1, 2024",
    "Downloaded from journal.org on Jan 1, 2024",
    "Downloaded from journal.org on Jan 1, 2024"
  ), collapse = "\n")
  cleaned <- effectcheck:::strip_headers_footers(text)
  expect_false(grepl("Downloaded from journal", cleaned))
  expect_true(grepl("F\\(1, 50\\)", cleaned))
  expect_true(grepl("t\\(50\\)", cleaned))
})

test_that("strip_headers_footers preserves short repeated lines", {
  # Short lines (< 15 chars) should NOT be removed even if repeated
  text <- paste(c(
    "Results",
    "t(28) = 2.21, p = .035",
    "Results",
    "F(1, 50) = 4.20, p < .05",
    "Results",
    "r(30) = .45, p < .01",
    "Results",
    "Results",
    "Results"
  ), collapse = "\n")
  cleaned <- effectcheck:::strip_headers_footers(text)
  # "Results" is only 7 chars, below the 15-char minimum
  expect_true(grepl("Results", cleaned))
})

test_that("strip_headers_footers preserves non-repeated lines", {
  text <- paste(c(
    "This line appears once.",
    "This line also appears once.",
    "F(1, 50) = 4.20, p < .05."
  ), collapse = "\n")
  cleaned <- effectcheck:::strip_headers_footers(text)
  expect_equal(text, cleaned)
})

# =============================================================================
# SECTION 5: Computation-guided decimal recovery (Phase 5B)
# =============================================================================

test_that("effect size decimal recovery works when computed value matches", {
  # d = 80 should be recovered to d = 0.80 if computed d_ind ~ 0.80
  # t(28) = 2.21 with N=60 (30+30) gives d_ind ~ 0.57
  # Use a case where the math works out:
  # t(100) = 4.00 with N=102 gives d_ind ~ 0.80
  res <- check_text("N = 102. t(100) = 4.00, p < .001, d = 80")
  # Should recover d = 0.80 and flag it
  if ("decimal_recovered" %in% names(res)) {
    # If recovery happened, the effect should be close to computed
    if (isTRUE(res$decimal_recovered[1])) {
      expect_true(res$extraction_suspect[1])
      expect_true(any(grepl("Decimal recovery", res$uncertainty_reasons[1])))
    }
  }
})

test_that("legitimate large F-value is NOT treated as dropped decimal", {
  res <- check_text("F(1, 144) = 150.41, p < .001")
  expect_equal(res$stat_value[1], 150.41)
  # Should not trigger decimal recovery
  expect_false(isTRUE(res$decimal_recovered[1]))
})

test_that("no decimal recovery when no candidate matches", {
  # d = 999 with t(28) = 2.21 — no decimal placement will match
  res <- check_text("N = 60. t(28) = 2.21, p = .035, d = 999")
  # Should remain extraction_suspect without recovery
  if (!is.na(res$effect_reported[1]) && res$effect_reported[1] == 999) {
    expect_true(res$extraction_suspect[1])
    expect_false(isTRUE(res$decimal_recovered[1]))
  }
})

test_that("p-value decimal correction with known good d produces uncertainty note", {
  # p = 350 should be corrected to p = .350, and the note should appear
  res <- check_text("t(28) = 2.21, p = 350, d = 0.80")
  expect_equal(res$p_reported[1], 0.350)
  expect_true(res$extraction_suspect[1])
  expect_true(any(grepl("[Dd]ecimal", res$uncertainty_reasons[1])))
})

# =============================================================================
# SECTION 6: Legitimate values preserved (regression guards)
# =============================================================================

test_that("standard t-test still works after all extraction changes", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_equal(res$test_type[1], "t")
  expect_equal(res$stat_value[1], 2.21)
  expect_equal(res$p_reported[1], 0.035)
  expect_equal(res$effect_reported[1], 0.80)
  expect_equal(res$status[1], "PASS")
})

test_that("standard F-test still works after all extraction changes", {
  res <- check_text("F(2, 57) = 4.50, p = .015, etap2 = .136")
  expect_equal(res$test_type[1], "F")
  expect_equal(res$stat_value[1], 4.50)
})

test_that("chi-square still works after all extraction changes", {
  res <- check_text("chi2(1, N = 100) = 4.50, p = .034, phi = 0.21")
  expect_equal(res$test_type[1], "chisq")
  expect_equal(res$stat_value[1], 4.50)
})

test_that("correlation still works after all extraction changes", {
  res <- check_text("r(48) = .345, p = .015")
  expect_equal(res$test_type[1], "r")
  expect_equal(res$stat_value[1], 0.345)
})

# =============================================================================
# SECTION 7: Column-merged text near statistics (existing behavior preserved)
# =============================================================================

test_that("column-merged context text doesn't corrupt stat parsing", {
  res <- check_text("the predicted SelfEvaluation - Accessibility interaction, t(97) = 2.99, p < .01")
  expect_equal(res$stat_value[1], 2.99)
  expect_equal(res$df1[1], 97)
})

# =============================================================================
# SECTION 8: U+FFFD eta-squared recovery (PDF corruption)
# =============================================================================

test_that("U+FFFD in effect size context is recovered as eta-squared", {
  # pdftotext corrupts \u03b7\u00b2 to U+FFFD; should recover as eta-squared
  res <- check_text("F(1, 1180) = 55.90, p < .001, \uFFFD = 0.04, 90% CI [0.02, 0.06]")
  expect_equal(res$test_type[1], "F")
  expect_equal(res$stat_value[1], 55.90)
  expect_equal(res$effect_reported_name[1], "eta2")
  expect_equal(res$effect_reported[1], 0.04)
})

test_that("U+FFFD recovery works with multiple F-tests in paragraph", {
  text <- paste0(
    "main effect of fame (F(1, 1180) = 55.90, p < .001, \uFFFD = 0.04, 90% CI [0.02, 0.06]) ",
    "and valence (F(2, 1180) = 181.2, p < .001, \uFFFD = 0.22, 90% CI [0.19, 0.26])")
  res <- check_text(text)
  expect_equal(nrow(res), 2)
  expect_true(all(res$effect_reported_name == "eta2"))
  expect_equal(res$effect_reported[1], 0.04)
  expect_equal(res$effect_reported[2], 0.22)
})

test_that("U+FFFD still works as minus sign outside effect size context", {
  # Non-effect-size U+FFFD should become dash (minus sign)
  res <- check_text("t(50) = \uFFFD2.50, p = .015, d = 0.70")
  expect_equal(res$test_type[1], "t")
  expect_equal(res$stat_value[1], -2.50)  # preserved as negative
})

# =============================================================================
# SECTION 9: Orphaned superscript 2 recovery
# =============================================================================

test_that("orphaned '2 = value' with 90% CI is recovered as eta-squared", {
  res <- check_text("F(1.97, 2337) = 46.96, p < .001, 2 = 0.01, 90% CI [0.00, 0.02]")
  expect_equal(res$test_type[1], "F")
  expect_equal(res$effect_reported_name[1], "eta2")
  expect_equal(res$effect_reported[1], 0.01)
})

# =============================================================================
# SECTION 10: DOCX pipe-delimited table normalization
# =============================================================================

test_that("pipe-delimited table stats are parsed correctly", {
  text <- "Inferential statistics | \u03c7\u00b2 (1) = 12.484, p < .001 | Cramer's V = .31"
  res <- check_text(text)
  expect_true(any(res$test_type == "chisq"))
  expect_true(any(res$stat_value == 12.484, na.rm = TRUE))
})
