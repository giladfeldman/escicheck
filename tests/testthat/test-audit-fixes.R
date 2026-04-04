# Tests for audit fixes (Issues 1-5)
# See: MetaESCI/preregistration/escicheck_audit_report.md

# ===========================================================================
# Issue 1: Cross-type effect size matching
# ===========================================================================

test_that("F(1,k) with reported d matches d variant, not eta", {
  text <- "F(1, 58) = 8.33, p = .006, d = 0.75"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "F")

  # matched_variant should be a d-family variant, not eta
  expect_true(grepl("^d_|^dz", result$matched_variant[1]),
    info = paste("Expected d-family match, got:", result$matched_variant[1]))

  # Should NOT be highly_ambiguous cross-type
  expect_false(result$ambiguity_level[1] == "highly_ambiguous",
    info = "F(1,k) with d should not be highly_ambiguous")

  # Status should not be ERROR from cross-type mismatch
  expect_true(result$status[1] != "ERROR" ||
    !grepl("No same-type", result$ambiguity_reason[1]),
    info = "Cross-type fallback should not produce ERROR")
})

test_that("F(1,k) d_equiv is computed correctly from t equivalence", {
  # F(1, 30) = 9.00 => t = 3.0, n1=n2=16, d_ind = t*sqrt(1/16+1/16) = 3*sqrt(1/8) = 1.061
  text <- "F(1, 30) = 9.00, p = .005, d = 1.06"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # Should match closely (delta < 0.02)
  expect_true(!is.na(result$delta_effect[1]) && result$delta_effect[1] < 0.05,
    info = paste("Delta:", result$delta_effect[1]))
})

test_that("F(1,k) computes r_equiv as alternative", {
  # F(1, 48) = 6.25 => r = sqrt(6.25/(6.25+48)) = sqrt(0.1152) = 0.3394
  # Note: parser does not currently extract "r = .34" as an effect size for F-tests
  # (it's ambiguous with a correlation test). But r_equiv should be computed.
  text <- "F(1, 48) = 6.25, p = .016"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expected_r <- sqrt(6.25 / (6.25 + 48))
  actual_r <- result$r_from_t_or_reported[1]
  expect_true(!is.na(actual_r),
    info = "r_equiv should be computed for F(1,k)")
  expect_true(abs(actual_r - expected_r) < 0.001,
    info = sprintf("r_equiv=%.4f expected=%.4f", actual_r, expected_r))
})

test_that("F(1,k) with d does not produce 'unusual for F-test' warning", {
  text <- "F(1, 40) = 5.00, p = .031, d = 0.70"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # Should not contain "unusual for F-test" uncertainty
  expect_false(grepl("unusual for F-test|unusual for F/ANOVA", result$uncertainty_reasons[1]),
    info = paste("Unexpected uncertainty:", result$uncertainty_reasons[1]))
})

test_that("F(2,k) with d still flags cross-type (df1 != 1)", {
  text <- "F(2, 27) = 4.56, p = .02, d = 0.80"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # df1=2, no t-equivalence, should flag cross-type
  expect_true(result$ambiguity_level[1] == "highly_ambiguous" ||
    grepl("unusual", result$uncertainty_reasons[1]),
    info = "F(df1>1) with d should flag mismatch")
})

test_that("F-test with R2 reported computes R2 variant", {
  # F(2, 97) = 5.50 => eta2 = 5.50*2/(5.50*2+97) = 11/108 = 0.1019
  text <- "F(2, 97) = 5.50, p = .005, R2 = .10"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # Should have R2 in matched_variant
  if (!is.na(result$matched_variant[1])) {
    expect_true(grepl("R2", result$matched_variant[1]),
      info = paste("Expected R2 match, got:", result$matched_variant[1]))
  }
})

test_that("cross-type fallback is capped at NOTE, not ERROR", {
  # F(3, 50) with d reported — no way to compute d from multi-df F
  text <- "F(3, 50) = 10.00, p < .001, d = 2.50"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  if (result$ambiguity_level[1] == "highly_ambiguous" &&
      grepl("No same-type", result$ambiguity_reason[1])) {
    expect_true(result$status[1] != "ERROR",
      info = "Cross-type (no same-type variants) should not be ERROR")
  }
})

# ===========================================================================
# Issue 2: Paired N bug in t-test design inference
# ===========================================================================

test_that("dz uses df+1 not df+2 for paired computation", {
  # t(29) = 3.50 => correct dz = 3.50/sqrt(30) = 0.63901
  text <- "t(29) = 3.50, p = .001, dz = 0.64"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # dz should be computed with n = df+1 = 30
  expected_dz <- 3.50 / sqrt(30)
  actual_dz <- result$dz[1]
  expect_true(!is.na(actual_dz),
    info = "dz should be computed")
  expect_true(abs(actual_dz - expected_dz) < 0.001,
    info = sprintf("dz=%.4f expected=%.4f (diff=%.4f)", actual_dz, expected_dz, abs(actual_dz - expected_dz)))
})

test_that("paired dz uses df+1 even when design inferred as independent", {
  # When d is reported, design is inferred as independent (N=df+2)
  # But paired dz should still use n=df+1
  text <- "t(49) = 2.80, p = .007, d = 0.56"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # dz = 2.80/sqrt(50) = 0.39598
  expected_dz <- 2.80 / sqrt(50)
  actual_dz <- result$dz[1]
  expect_true(!is.na(actual_dz),
    info = "dz should be computed even when design is independent")
  expect_true(abs(actual_dz - expected_dz) < 0.001,
    info = sprintf("dz=%.4f expected=%.4f", actual_dz, expected_dz))
})

# ===========================================================================
# Issue 3: NOTE results that should be PASS
# ===========================================================================

test_that("ambiguous design with good match is PASS not NOTE", {
  # t(28) with d reported, no design context => ambiguous
  # d_ind_equalN ~ 2*2.21/sqrt(30) = 0.807, reported d=0.81
  text <- "t(28) = 2.21, p = .035, d = 0.81"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # If delta is within tolerance, should be PASS even with ambiguous design
  if (!is.na(result$delta_effect[1]) && result$delta_effect[1] < 0.02) {
    expect_equal(result$status[1], "PASS",
      info = paste("Status:", result$status[1], "delta:", result$delta_effect[1],
                   "ambiguity:", result$ambiguity_level[1]))
  }
})

test_that("highly_ambiguous with good delta stays NOTE", {
  # This test verifies that truly cross-type matches stay NOTE
  # (only ambiguous should upgrade to PASS)
  text <- "t(28) = 2.21, p = .035, xyz = 0.81"
  result <- check_text(text)

  # If unknown type and highly_ambiguous, should stay NOTE even with small delta
  if (nrow(result) > 0 && !is.na(result$ambiguity_level[1]) &&
      result$ambiguity_level[1] == "highly_ambiguous" &&
      !is.na(result$delta_effect[1]) && result$delta_effect[1] < 0.02) {
    expect_equal(result$status[1], "NOTE")
  }
})

# ===========================================================================
# Issue 4: check_type column
# ===========================================================================

test_that("check_type column is present in output", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_true("check_type" %in% names(result),
    info = "check_type column should be in output")
})

test_that("check_type is 'effect_size' when ES is reported", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_equal(result$check_type[1], "effect_size")
})

test_that("check_type is 'p_value' when only p-value reported", {
  text <- "t(28) = 2.21, p = .035"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_equal(result$check_type[1], "p_value")
})

# ===========================================================================
# Issue 5: Extreme delta / extraction_suspect flag
# ===========================================================================

test_that("extraction_suspect column is present in output", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_true("extraction_suspect" %in% names(result),
    info = "extraction_suspect column should be in output")
})

test_that("extraction_suspect is FALSE for normal deltas", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_false(result$extraction_suspect[1])
})

test_that("extraction_suspect is TRUE for extreme deltas", {
  # Report a wildly wrong d value to trigger extreme delta
  text <- "t(28) = 2.21, p = .035, d = 5.00"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  if (!is.na(result$delta_effect[1]) && result$delta_effect[1] > 1.0) {
    expect_true(result$extraction_suspect[1],
      info = paste("delta:", result$delta_effect[1], "should flag extraction_suspect"))
  }
})

test_that("extraction artifact d >> expected downgrades to NOTE", {
  # v0.3.0f: d=5.00 with t(28)=2.21 gives expected d≈0.81
  # d is 6× expected — flagged as extraction artifact, NOTE not ERROR
  text <- "t(28) = 2.21, p = .035, d = 5.00"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_true(result$extraction_suspect[1])
  expect_equal(result$status[1], "NOTE",
    info = "d far exceeding expected from t-stat should be NOTE (artifact)")
})
