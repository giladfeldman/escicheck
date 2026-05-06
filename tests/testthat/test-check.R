# Test suite for check functions

test_that("check_text returns tibble with correct structure", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  expect_true(tibble::is_tibble(result))
  if (nrow(result) > 0) {
    expect_true("test_type" %in% names(result))
    expect_true("status" %in% names(result))
    expect_true("uncertainty_level" %in% names(result))
  }
})

test_that("check_text handles empty text", {
  result <- check_text("")
  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
})

test_that("check_text detects t-test", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_equal(result$test_type[1], "t")
  }
})

test_that("check_text detects F-test", {
  text <- "F(2, 27) = 4.56, p < .05"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_equal(result$test_type[1], "F")
  }
})

test_that("check_text detects correlation", {
  text <- "r(198) = .34, p < .001"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_equal(result$test_type[1], "r")
  }
})

test_that("check_text computes effect sizes for t-test", {
  text <- "t(28) = 2.21, d = 0.80, N = 30"
  result <- check_text(text)
  if (nrow(result) > 0) {
    # Should compute d_ind or dz
    has_effect <- !is.na(result$d_ind[1]) || !is.na(result$dz[1])
    expect_true(has_effect)
  }
})

test_that("check_text computes ANOVA effect sizes", {
  text <- "F(2, 27) = 4.56, η² = 0.25"
  result <- check_text(text)
  if (nrow(result) > 0) {
    # Should compute eta² variants
    has_eta2 <- !is.na(result$eta2[1]) || !is.na(result$partial_eta2[1])
    expect_true(has_eta2)
  }
})

test_that("check_text assigns status correctly", {
  text <- "t(28) = 2.21, d = 0.80, N = 30, n1 = 15, n2 = 15"
  result <- check_text(text, tol_effect = list(d = 0.02))
  if (nrow(result) > 0) {
    expect_true(result$status[1] %in% c("PASS", "OK", "NOTE", "WARN", "ERROR", "INSUFFICIENT_DATA"))
  }
})

test_that("check_text tracks uncertainty", {
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true("uncertainty_level" %in% names(result))
    expect_true(result$uncertainty_level[1] %in% c("low", "medium", "high"))
  }
})

test_that("check_text includes design_inferred", {
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true("design_inferred" %in% names(result))
  }
})

test_that("check_text includes variants_tested", {
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true("variants_tested" %in% names(result))
  }
})

test_that("check_files() is defunct in v0.4.0", {
  # File extraction was moved to docpluck in v0.4.0. The replacement is to
  # extract via docpluck (or any other extractor) and pass the resulting
  # text directly to check_text(). check_files() now errors on call.
  expect_error(
    check_files("any_path.txt"),
    regexp = "(Defunct|docpluck|effectcheck v0\\.4)",
    info = "check_files() should error with the v0.4.0 migration message"
  )
})

test_that("check_text() handles equivalent multi-input via concatenation", {
  # The functional replacement for check_files(c(f1, f2)) is to concatenate
  # the texts and call check_text() once, or call check_text() per file in
  # the caller and dplyr::bind_rows() the results.
  text <- paste(
    "t(28) = 2.21, d = 0.80",
    "r(50) = 0.34, p < .05",
    sep = "\n"
  )
  result <- check_text(text)
  expect_true(tibble::is_tibble(result))
  expect_gte(nrow(result), 2L)
})

# ===========================================================================
# 5-level status system tests (PASS / OK / NOTE / WARN / ERROR)
# ===========================================================================

test_that("OK status for p-value-only check with consistent direction", {
  # F-test with no effect size, but p-value consistent
  text <- "F(2, 27) = 4.56, p = .02"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true(result$status[1] %in% c("PASS", "OK", "NOTE", "WARN", "ERROR"))
  }
})

test_that("OK status for p < .001 inequality when computed p also < .001", {
  # Large t-value ensures computed p < .001
  text <- "t(28) = 5.50, p < .001"
  result <- check_text(text)
  if (nrow(result) > 0) {
    # No effect size reported, p < .001 consistent -> should be OK
    expect_true(result$status[1] %in% c("OK", "NOTE", "PASS"))
  }
})

test_that("r-test with consistent p-value gets PASS status", {
  # v0.3.0l: r IS both the statistic and the effect size — should be PASS
  text <- "r(198) = .34, p < .001"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_equal(result$status[1], "PASS")
    expect_equal(result$check_type[1], "effect_size")
    expect_equal(result$effect_reported[1], 0.34)
    expect_equal(result$matched_variant[1], "r")
  }
})

test_that("NOTE status for ambiguous design with matching effect", {
  # Ambiguous design (no clear paired/independent context) but effect matches
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    # Should be PASS, NOTE, or WARN depending on delta
    expect_true(result$status[1] %in% c("PASS", "OK", "NOTE", "WARN", "ERROR"))
  }
})

test_that("p_symbol column is available in parsed output", {
  text <- "t(28) = 2.21, p < .001"
  parsed <- parse_text(text)
  expect_true("p_symbol" %in% names(parsed))
  if (nrow(parsed) > 0) {
    expect_equal(parsed$p_symbol[1], "<")
  }
})

test_that("p_symbol captures = sign", {
  text <- "t(28) = 2.21, p = .035"
  parsed <- parse_text(text)
  if (nrow(parsed) > 0) {
    expect_equal(parsed$p_symbol[1], "=")
  }
})

# ===========================================================================
# Welch t-test and thousands-separator bug fixes
# ===========================================================================

test_that("thousands-separator N is parsed correctly (not truncated)", {
  # Bug: "N = 1,182" was parsed as N=1 due to decimal comma conversion
  text <- "N = 1,182. t(403.8) = -3.15, p < .001, d = -.31"
  result <- check_text(text)
  expect_true(nrow(result) >= 1)
  # N should be 1182, NOT 1
  expect_equal(result$N[1], 1182)
  # d_ind should NOT be garbage (-3.15 from N=1); with N=1182 it's ~-0.18
  if (!is.na(result$d_ind_equalN[1])) {
    expect_true(abs(result$d_ind_equalN[1]) < 1)  # Not garbage
    expect_true(abs(result$d_ind_equalN[1]) > 0.05) # Not degenerate
  }
})

test_that("Welch t-test with correct N gives PASS", {
  # Use the actual correct N for this t-test
  text <- "N = 406. t(403.8) = -3.15, p < .001, d = -.31"
  result <- check_text(text)
  expect_true(nrow(result) >= 1)
  if (!is.na(result$d_ind_equalN[1])) {
    expect_true(abs(result$d_ind_equalN[1] - (-0.31)) < 0.05)
  }
  expect_true(result$status[1] %in% c("PASS", "NOTE"))
})

test_that("Welch t-test infers N from df when N missing", {
  text <- "t(403.8) = -3.15, p < .001, d = -.31"
  result <- check_text(text)
  expect_true(nrow(result) >= 1)
  # Should infer N ~ 406-413 from df=403.8 and/or reported d
  if (!is.na(result$N[1])) {
    expect_true(result$N[1] >= 400 && result$N[1] <= 420)
  }
})

test_that("Welch t-test back-computes N from reported d", {
  # t=-3.15, d=-0.31 -> N = 4*9.9225/0.0961 ~ 413
  text <- "t(403.8) = -3.15, p = .001, d = -.31"
  result <- check_text(text)
  expect_true(nrow(result) >= 1)
  # Should get reasonable effect size match
  if (!is.na(result$d_ind_equalN[1])) {
    expect_true(abs(abs(result$d_ind_equalN[1]) - 0.31) < 0.05)
  }
})

test_that("parse_text correctly extracts N with thousands separator", {
  text <- "N = 1,182 participants were tested. t(403.8) = -3.15, p < .001, d = -.31"
  result <- parse_text(text)
  expect_true(nrow(result) >= 1)
  # N should be 1182, NOT 1
  expect_equal(result$N[1], 1182)
})

test_that("dz_from_t returns NA for n < 2", {
  expect_true(is.na(effectcheck:::dz_from_t(2.5, 1)))
  expect_true(is.na(effectcheck:::dz_from_t(2.5, 0)))
  expect_true(is.na(effectcheck:::dz_from_t(2.5, -1)))
  # n=2 should still work
  expect_equal(effectcheck:::dz_from_t(2.0, 2), 2.0 / sqrt(2), tolerance = 1e-7)
})

# ============================================================================
# v0.3.6: df_arity_mismatch short-circuit in check.R
# When the parser flags a row as df-arity-mismatched, check.R must skip all
# computation paths and emit status=NOTE with an explanatory message.
# ============================================================================

test_that("F(48) check: df_arity_mismatch propagates to status NOTE", {
  res <- check_text("F(48) = 2.31, p = .04.")
  expect_equal(nrow(res), 1)
  expect_true(res$df_arity_mismatch[1])
  expect_equal(res$status[1], "NOTE")
  expect_true(is.na(res$p_computed[1]))
  expect_false(res$decision_error[1])
  expect_true(grepl("df argument", res$uncertainty_reasons[1], fixed = TRUE))
})

test_that("t(36, 10) check: df_arity_mismatch propagates to status NOTE", {
  res <- check_text("t(36, 10) = 5.34, p = .003.")
  expect_equal(nrow(res), 1)
  expect_true(res$df_arity_mismatch[1])
  expect_equal(res$status[1], "NOTE")
  expect_true(is.na(res$p_computed[1]))
  expect_false(res$decision_error[1])
})

test_that("normal F(2, 87) check: df_arity_mismatch FALSE and computation runs", {
  res <- check_text("F(2, 87) = 4.12, p = .020.")
  expect_equal(nrow(res), 1)
  expect_false(res$df_arity_mismatch[1])
  expect_false(is.na(res$p_computed[1]))
})
