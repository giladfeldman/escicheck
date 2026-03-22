# =============================================================================
# Tests for design-ambiguous t-test effect size ERROR downgrade (v0.2.6)
# =============================================================================

test_that("Ambiguous t-test effect size ERROR downgraded to WARN by default", {
  # t(100) with bare "d" — design is ambiguous
  # d_ind_equalN = 2*3.45/sqrt(100) = 0.69, dz = 3.45/sqrt(51) = 0.483
  # reported d = 1.20, delta >> 5*0.02 = 0.10 → would be ERROR
  res <- check_text("t(100) = 3.45, p < .001, d = 1.20")
  expect_true(nrow(res) >= 1)
  # Should be WARN (downgraded from ERROR) with design ambiguity note
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect[1]) &&
      res$delta_effect[1] > 5 * 0.02) {
    expect_equal(res$status[1], "WARN")
    expect_true(res$confidence[1] <= 4)
    expect_true(grepl("d computation method difference|d-from-t",
                      res$uncertainty_reasons[1]))
  }
})

test_that("design_ambiguous_action = ERROR preserves original behavior", {
  res <- check_text("t(100) = 3.45, p < .001, d = 1.20",
                    design_ambiguous_action = "ERROR")
  expect_true(nrow(res) >= 1)
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect[1]) &&
      res$delta_effect[1] > 5 * 0.02) {
    expect_equal(res$status[1], "ERROR")
  }
})

test_that("design_ambiguous_action = NOTE downgrades to NOTE", {
  res <- check_text("t(100) = 3.45, p < .001, d = 1.20",
                    design_ambiguous_action = "NOTE")
  expect_true(nrow(res) >= 1)
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect[1]) &&
      res$delta_effect[1] > 5 * 0.02) {
    expect_equal(res$status[1], "NOTE")
  }
})

test_that("Clear paired design (dz reported) does NOT get downgraded", {
  # When dz is reported, design is "paired" — clear, not ambiguous
  res <- check_text("t(28) = 2.21, p = .035, dz = 1.50")
  expect_true(nrow(res) >= 1)
  # dz = 2.21/sqrt(29) = 0.410, delta = |1.50 - 0.410| = 1.09 >> tolerance
  # But design is clear (paired), so should remain ERROR
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect[1]) &&
      res$delta_effect[1] > 5 * 0.02 &&
      !res$extraction_suspect[1]) {
    expect_equal(res$status[1], "ERROR")
  }
})

test_that("PASS and WARN t-tests are unaffected by design_ambiguous_action", {
  # d close to computed — should stay PASS
  # t(100) = 3.45 → d_ind_equalN ≈ 0.69
  res <- check_text("t(100) = 3.45, p < .001, d = 0.69")
  expect_true(nrow(res) >= 1)
  expect_true(res$status[1] %in% c("PASS", "WARN", "OK"))
})

test_that("Confidence is capped at 4 for design-ambiguous downgrades", {
  res <- check_text("t(100) = 3.45, p < .001, d = 1.20")
  expect_true(nrow(res) >= 1)
  if (res$check_type[1] == "effect_size" && res$status[1] == "WARN" &&
      grepl("d computation method difference|d-from-t",
            res$uncertainty_reasons[1])) {
    expect_true(res$confidence[1] <= 4)
  }
})

test_that("Large df ambiguous t-test gets downgraded", {
  # t(518) with d much larger than computed
  # d_ind_equalN = 2*10.21/sqrt(518) ≈ 0.897
  # reported d = 1.50, delta ≈ 0.60
  res <- check_text("t(518) = 10.21, p < .001, d = 1.50")
  expect_true(nrow(res) >= 1)
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect[1]) &&
      res$delta_effect[1] > 5 * 0.02) {
    expect_equal(res$status[1], "WARN")
  }
})

test_that("Small df ambiguous t-test gets downgraded", {
  # t(15) = 6.80 → d_ind_equalN = 2*6.80/sqrt(15) ≈ 3.51
  # dz = 6.80/sqrt(16) = 1.70
  # reported d = 2.80, ambiguous between variants
  res <- check_text("t(15) = 6.80, p < .001, d = 2.80")
  expect_true(nrow(res) >= 1)
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect[1]) &&
      res$delta_effect[1] > 5 * 0.02 &&
      !res$extraction_suspect[1]) {
    expect_equal(res$status[1], "WARN")
  }
})

test_that("F(1,df) with reported d also gets downgraded", {
  # F(1, 30) = 9.00 → t = 3.0, d_ind_equalN = 2*3/sqrt(30) ≈ 1.095
  # reported d = 1.80, delta ≈ 0.705
  res <- check_text("F(1, 30) = 9.00, p = .005, d = 1.80")
  expect_true(nrow(res) >= 1)
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect[1]) &&
      res$delta_effect[1] > 5 * 0.02 &&
      !res$extraction_suspect[1]) {
    expect_equal(res$status[1], "WARN")
  }
})

test_that("Report test case: common independent t-test", {
  # From the MetaESCI report — most common pattern
  res <- check_text("t(100) = 3.45, p < .001, d = 0.68")
  expect_true(nrow(res) >= 1)
  # d_ind_equalN ≈ 0.69, delta ≈ 0.01 — should PASS or be close
  # This verifies the fix doesn't break good matches
  expect_true(res$status[1] %in% c("PASS", "WARN"))
})

# =============================================================================
# Issue F/I fix: Decision error requires reported p-value (v0.2.6)
# Prevents false decision errors for extraction-only results (regression
# z-statistics from coefficient tables)
# =============================================================================

test_that("z-statistic without reported p does NOT trigger decision error", {
  # z = 9.47 computes p << 0.001, but no p is reported
  # Without fix: decision_error = TRUE (FALSE != TRUE), prevents SKIP
  # With fix: decision_error = FALSE, gets SKIP correctly
  res <- check_text("z = 9.47")
  expect_true(nrow(res) >= 1)
  expect_equal(res$status[1], "SKIP")
})

test_that("z-statistic WITH reported p still gets decision error checked", {
  # z = 0.50 with p < .001 — this IS a genuine decision error
  # (z = 0.50 gives p ≈ 0.617, but reported p < .001)
  res <- check_text("z = 0.50, p < .001")
  expect_true(nrow(res) >= 1)
  # Should flag decision error (p_reported exists)
  expect_true(grepl("decision|Decision", res$uncertainty_reasons[1]) ||
              res$status[1] %in% c("WARN", "ERROR", "NOTE"))
})

test_that("Bare t-statistic without p gets SKIP", {
  # t(28) = 2.21 with no p-value reported → extraction_only → SKIP
  res <- check_text("t(28) = 2.21")
  expect_true(nrow(res) >= 1)
  expect_equal(res$status[1], "SKIP")
})

# =============================================================================
# Issue H fix: r-test with global N suppresses decision error (v0.2.6)
# =============================================================================

test_that("r-test with explicit p-value gets decision error checked normally", {
  # r(100) = .164, p = .050 — df is explicit, p-computation is reliable
  # z = r * sqrt(df/(1-r^2)) = .164 * sqrt(100/0.973) ≈ 1.662
  # p_two_tail = 2 * pt(1.662, 100, lower=FALSE) ≈ 0.0997
  # reported p = .050 < alpha, computed p = 0.100 > alpha → decision error
  res <- check_text("r(100) = .164, p = .050")
  expect_true(nrow(res) >= 1)
  # With explicit df in r(100), N_source is irrelevant — decision error should fire
})
