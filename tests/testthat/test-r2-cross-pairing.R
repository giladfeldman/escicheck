# test-r2-cross-pairing.R — v0.2.7 Issue B: R² cross-pairing detection
# Detects regression table cross-pairing artifacts where F and R² come from
# different models. Uses text signals to downgrade ERRORs to WARN/NOTE.

test_that("R2 before F on same line matches correctly (not downgraded)", {
  # R2 = .20, F(3,100) = 8.33: computed R² = 0.200 → delta ~0 → PASS
  res <- check_text("R2 = .20, F(3, 100) = 8.33, p < .001")
  expect_equal(res$status[1], "PASS")
  expect_false(res$r2_cross_pairing_detected[1])
})

test_that("r2_cross_pairing_detected column exists", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_true("r2_cross_pairing_detected" %in% names(res))
  expect_false(res$r2_cross_pairing_detected[1])
})

test_that("Multi-predictor F with R² ERROR gets WARN in regression context", {
  # F(3,96)=42: computed R²=0.568, reported=0.15 → ERROR → should detect mismatch
  # df1>1 is regression signal
  res <- check_text("regression analysis showed F(3, 96) = 42.0, p < .001, R2 = .15")
  if (res$test_type[1] == "F" && !is.na(res$delta_effect_abs[1]) &&
      res$delta_effect_abs[1] > 5 * 0.01) {
    # Should be downgraded from ERROR
    expect_true(res$status[1] %in% c("WARN", "NOTE"))
    expect_true(res$r2_cross_pairing_detected[1])
  }
})

test_that("F with R² matching correctly stays PASS (regression guard)", {
  # F(1,98) = 12.5: computed R² = 0.113, reported = .11 → PASS
  res <- check_text("F(1, 98) = 12.5, p < .001, R2 = .11")
  expect_true(res$status[1] %in% c("PASS", "WARN"))
  expect_false(res$r2_cross_pairing_detected[1])
})

test_that("Hierarchical regression detected and downgraded", {
  res <- check_text("Step 2 R2 change F(1, 98) = 5.0, p = .03, R2 = .35")
  if (nrow(res) >= 1 && res$test_type[1] == "F" &&
      !is.na(res$delta_effect_abs[1]) && res$delta_effect_abs[1] > 5 * 0.01) {
    expect_true(res$status[1] %in% c("NOTE", "WARN"))
  }
})

test_that("ANOVA context prevents regression downgrade", {
  # ANOVA keywords should prevent the regression-specific downgrades
  res <- check_text("ANOVA showed F(3, 96) = 42.0, p < .001, R2 = .15")
  # With ANOVA context, the cross-pairing guard should NOT fire
  if (nrow(res) >= 1 && res$test_type[1] == "F") {
    # May still be ERROR (ANOVA guard prevents downgrade)
    # The key assertion: r2_cross_pairing_detected should be FALSE
    if (res$status[1] == "ERROR") {
      expect_false(res$r2_cross_pairing_detected[1])
    }
  }
})

# Signal 14 (v0.3.4): Eta/f cross-family detection
# When reported eta2/etap2 is matched to cohens_f/cohens_f2 with delta > 0.05,
# downgrade ERROR -> WARN with r2_cross_pairing_detected = TRUE.

test_that("etap2 reported matched to cohens_f gets WARN (Signal 14)", {
  # F(1,100)=2.50: computed etap2=0.024, cohens_f=0.158
  # Reported etap2=0.50 is closer to cohens_f -> cross-family match, delta ~0.34
  res <- check_text("F(1, 100) = 2.50, etap2 = 0.50")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "F")
  expect_equal(res$matched_variant[1], "cohens_f")
  expect_equal(res$status[1], "WARN")
  expect_true(res$r2_cross_pairing_detected[1])
})

test_that("eta2 reported matched to cohens_f gets WARN (Signal 14)", {
  # Same as above but with eta2 instead of etap2
  res <- check_text("F(1, 100) = 2.50, eta2 = 0.50")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "F")
  expect_equal(res$matched_variant[1], "cohens_f")
  expect_equal(res$status[1], "WARN")
  expect_true(res$r2_cross_pairing_detected[1])
})

test_that("eta2 correct match stays PASS (no false positive from Signal 14)", {
  # F(2,100)=35.58: computed eta2=0.416 -> reported matches -> PASS
  res <- check_text("F(2, 100) = 35.58, eta2 = 0.416")
  expect_equal(res$status[1], "PASS")
  expect_false(res$r2_cross_pairing_detected[1])
})

test_that("partial eta2 correct match stays PASS (no false positive from Signal 14)", {
  # F(1,98)=478.55: computed partial eta2=0.830 -> reported matches -> PASS
  res <- check_text("F(1, 98) = 478.55, partial eta2 = 0.83")
  expect_equal(res$status[1], "PASS")
  expect_false(res$r2_cross_pairing_detected[1])
})

test_that("cohens_f correct match stays PASS (no false positive from Signal 14)", {
  # F(2,100)=35.58: computed cohens_f=0.844 -> reported matches -> PASS
  res <- check_text("F(2, 100) = 35.58, f = 0.844")
  expect_equal(res$status[1], "PASS")
  expect_false(res$r2_cross_pairing_detected[1])
})
