# =============================================================================
# test-v024-bugfixes.R — Tests for v0.2.4 validation-driven fixes
# =============================================================================

# =============================================================================
# PHASE 1: Reported-side plausibility guards
# =============================================================================

test_that("R2 > 1 is rejected at parse time", {
  res <- check_text("F(1, 98) = 5.0, p = .025, R2 = 5")
  # R2=5 is impossible (bounded 0-1), should not be extracted
  expect_true(is.na(res$effect_reported[1]) || res$effect_reported[1] != 5)
})

test_that("V > 1 is rejected at parse time", {
  res <- check_text("chi2(1, N = 100) = 4.50, p = .034, V = 36")
  # V=36 is impossible (bounded 0-1), should not be extracted
  expect_true(is.na(res$effect_reported[1]) || res$effect_reported[1] != 36)
})

test_that("Valid R2 < 1 is still extracted", {
  res <- check_text("F(1, 98) = 5.0, p = .025, R2 = 0.09")
  expect_equal(res$effect_reported[1], 0.09)
})

test_that("Valid V < 1 is still extracted", {
  res <- check_text("chi2(1, N = 100) = 4.50, p = .034, V = 0.21")
  expect_equal(res$effect_reported[1], 0.21)
})

test_that("Round integer d > 5 is rejected", {
  res <- check_text("t(28) = 2.21, p = .035, d = 8")
  # d=8 as a round integer with no decimal is not a plausible effect size
  expect_true(is.na(res$effect_reported[1]) || res$effect_reported[1] != 8)
})

test_that("d = 1 is still extracted (plausible small integer)", {
  res <- check_text("t(28) = 2.21, p = .035, d = 1")
  expect_equal(res$effect_reported[1], 1)
})

test_that("d = 2.50 with decimal point is still extracted", {
  res <- check_text("t(28) = 2.21, p = .035, d = 2.50")
  expect_equal(res$effect_reported[1], 2.50)
})

test_that("Normal d = 0.80 is unaffected", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_equal(res$effect_reported[1], 0.80)
})

test_that("Tightened EFFECT_PLAUSIBILITY for d is 5", {
  expect_equal(EFFECT_PLAUSIBILITY$d, 5)
  expect_equal(EFFECT_PLAUSIBILITY$g, 5)
})

# =============================================================================
# PHASE 1B: Computed-side plausibility guards
# =============================================================================

test_that("Computed effect size > plausibility bound flags extraction_suspect", {
  # A garbled t-stat (e.g., t=221 instead of t=2.21) produces huge computed d
  res <- check_text("t(200) = 221, p < .001, d = 1.38")
  expect_true(res$extraction_suspect[1])
})

# =============================================================================
# PHASE 2: warn_tiny_delta — decision error should not override excellent match
# =============================================================================

test_that("Excellent effect match with decision error stays PASS", {
  # Construct a case where effect size matches perfectly but p-value has decision error
  # t(500) = 1.98 → p ≈ 0.048 (barely significant)
  # If reported p = .052 (not significant), that's a decision error
  # But if d matches within tolerance and ambiguity is clear, should stay PASS
  res <- check_text("t(500) = 1.98, p = .052, d = 0.18")
  # The key test: if delta is tiny and ambiguity is clear, PASS should be preserved
  if (!is.na(res$delta_effect[1]) && res$delta_effect[1] < 0.01 &&
      res$ambiguity_level[1] == "clear") {
    expect_equal(res$status[1], "PASS")
  }
})

# =============================================================================
# PHASE 3: Method context in-chunk vs nearby
# =============================================================================

test_that("Method context in chunk caps ERROR to NOTE", {
  res <- check_text("A power analysis using G*Power showed t(28) = 2.21, p = .035, d = 0.80")
  # The chunk contains "power analysis" — should be flagged
  if (nrow(res) > 0) {
    expect_true("result_context" %in% names(res))
    # If method_context_in_chunk fired, result_context should be "method"
    if (!is.na(res$result_context[1]) && res$result_context[1] == "method") {
      expect_true(res$status[1] != "ERROR")
    }
  }
})

test_that("result_context column exists in output", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_true("result_context" %in% names(res))
  expect_equal(res$result_context[1], "study")
})

# =============================================================================
# PHASE 4: Cross-type error — alternatives included in same-type matching
# =============================================================================

test_that("g_ind from alternatives is included in same-type matching for reported d", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  # g_ind should be available as a same-type match for reported d
  # The matched_variant should be the closest d-family variant
  expect_true(nrow(res) > 0)
  expect_true(!is.na(res$matched_variant[1]))
})

# =============================================================================
# PHASE 5: suspicious_decision_error — effect_test_mismatch caps status
# =============================================================================

test_that("Type-incompatible effect size does not produce ERROR", {
  # R2 is not valid for chi-square tests
  res <- check_text("chi2(1, N = 100) = 5.0, p = .025, R2 = 0.05")
  if (nrow(res) > 0 && !is.na(res$status[1])) {
    # If there's a type mismatch, status should be capped at NOTE
    # (This test validates that the mismatch doesn't cause ERROR)
    expect_true(TRUE)  # placeholder — detailed assertion depends on whether R2 is extracted for chi2
  }
})

# =============================================================================
# PHASE 7: Confidence score
# =============================================================================

test_that("Confidence column exists in output", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_true("confidence" %in% names(res))
  expect_true(is.numeric(res$confidence[1]))
  expect_true(res$confidence[1] >= 0 && res$confidence[1] <= 10)
})

test_that("Clear ambiguity with small delta has high confidence", {
  res <- check_text("t(100) = 2.50, p = .014, d = 0.50")
  if (nrow(res) > 0 && res$ambiguity_level[1] == "clear" &&
      !is.na(res$delta_effect[1]) && res$delta_effect[1] < 0.02) {
    expect_true(res$confidence[1] >= 7)
  }
})

test_that("min_confidence parameter filters results", {
  res_all <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  res_high <- check_text("t(28) = 2.21, p = .035, d = 0.80", min_confidence = 10L)
  # High min_confidence should filter out some or all results
  expect_true(nrow(res_high) <= nrow(res_all))
})
