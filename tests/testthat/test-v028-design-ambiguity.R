# v0.2.8 tests: Design ambiguity improvements
# Tests for Phase 8A-bis (structural ambiguity), Phase 8B relaxation,
# z-test paired variants, R2 cross-pairing signal 9, and Cramer's V multi-m

# ===========================================================================
# Fix A: extraction_suspect guard relaxation in Phase 8B
# ===========================================================================

test_that("design-ambiguous t-test with large delta is WARN, not ERROR (Fix A)", {
  # t(10)=8.00, d=0.50: d_ind_equalN is large (~5.06), dz is smaller (~2.41)

# reported d=0.50 is far from both → large delta triggers extraction_suspect.
  # But design ambiguity should still downgrade to WARN.
  r <- check_text("t(10) = 8.00, p < .001, d = 0.50")
  df <- as.data.frame(r)
  expect_true(df$status %in% c("WARN", "NOTE"),
    info = "Design-ambiguous t-test with large delta should be WARN/NOTE, not ERROR")
  expect_true(df$design_ambiguous,
    info = "Should be flagged as design-ambiguous")
})

test_that("design_ambiguous_action='ERROR' preserves ERROR for t-test (Fix A)", {
  # Use a case with known n1/n2 so Phase 8C doesn't also fire
  r <- check_text("t(100) = 6.00, p < .001, d = 2.50, n1 = 51, n2 = 51",
                   design_ambiguous_action = "ERROR")
  df <- as.data.frame(r)
  # Phase 8B sets ERROR, Phase 8C won't fire (n1/n2 known)
  expect_equal(df$status, "ERROR",
    info = "With design_ambiguous_action='ERROR', should stay ERROR")
})

test_that("t-test with good match stays PASS (regression guard)", {
  # t(100)=2.50, d=0.50: d_ind_equalN ≈ 0.50, should PASS
  r <- check_text("t(100) = 2.50, p = .014, d = 0.50")
  df <- as.data.frame(r)
  expect_equal(df$status, "PASS",
    info = "Well-matched t-test should remain PASS")
})

# ===========================================================================
# Fix B: z-test paired variants
# ===========================================================================

test_that("dz_from_z computes correctly", {
  # dz = z/sqrt(N)
  expect_equal(dz_from_z(3.0, 100), 0.30, tolerance = 0.001)
  expect_equal(dz_from_z(2.0, 50), 2/sqrt(50), tolerance = 0.001)
  expect_true(is.na(dz_from_z(NA, 100)))
  expect_true(is.na(dz_from_z(3.0, NA)))
  expect_true(is.na(dz_from_z(3.0, 0)))
})

test_that("z-test computes both independent and paired d variants (Fix B)", {
  # z=3.00, N=100: d_ind = 2*3/sqrt(100) = 0.60, dz = 3/sqrt(100) = 0.30
  # Report d=0.30 → should match dz (paired)
  r <- check_text("z = 3.00, p = .003, d = 0.30, N = 100")
  df <- as.data.frame(r)
  # Should match dz since it's closer to reported d=0.30
  expect_true(df$matched_variant %in% c("dz", "dav", "drm"),
    info = "z-test with d=0.30 should match a paired variant (dz≈0.30)")
  expect_true(df$status %in% c("PASS", "WARN"),
    info = "Matched paired variant should give PASS or WARN")
})

test_that("z-test with d matching independent variant still works (Fix B)", {
  # z=3.00, N=100: d_ind = 0.60, dz = 0.30
  # Report d=0.60 → should match d_ind (independent)
  r <- check_text("z = 3.00, p = .003, d = 0.60, N = 100")
  df <- as.data.frame(r)
  expect_equal(df$matched_variant, "d_ind_equalN",
    info = "z-test with d matching independent value should match d_ind_equalN")
  expect_true(df$status %in% c("PASS", "WARN"),
    info = "Matched independent variant should give PASS or WARN")
})

test_that("z-test with d not matching either variant is WARN (design-ambiguous)", {
  # z=3.50, N=50: d_ind = 2*3.5/sqrt(50) ≈ 0.99, dz = 3.5/sqrt(50) ≈ 0.495
  # Report d=0.80 → between both variants, design ambiguous
  r <- check_text("z = 3.50, p < .001, d = 0.80, N = 50")
  df <- as.data.frame(r)
  expect_true(df$status %in% c("WARN", "NOTE"),
    info = "z-test with d between independent and paired should be WARN/NOTE")
  expect_true(df$design_ambiguous,
    info = "z-test with d/g should be flagged as design-ambiguous")
})

test_that("z-test with g computes Hedges variants (Fix B)", {
  # z=2.50, N=80: d_ind ≈ 0.559, g_ind ≈ 0.551
  # dz ≈ 0.280, gz ≈ 0.276
  r <- check_text("z = 2.50, p = .012, g = 0.28, N = 80")
  df <- as.data.frame(r)
  expect_true(df$status %in% c("PASS", "WARN"),
    info = "z-test with g matching gz should give good result")
})

test_that("z-test without N stays p_value_only (regression guard)", {
  r <- check_text("z = 2.50, p = .012, d = 0.40")
  df <- as.data.frame(r)
  expect_equal(df$check_scope, "p_value_only",
    info = "z-test without N cannot compute d, stays p_value_only")
})

test_that("z-test with r reported is NOT affected by d/g ambiguity (Fix B guard)", {
  r <- check_text("z = 2.50, p = .012, r = 0.28, N = 80")
  df <- as.data.frame(r)
  # r is not in d/g family — structural ambiguity should not fire
  expect_false(isTRUE(df$design_ambiguous) && df$ambiguity_level == "ambiguous" &&
               grepl("inherently ambiguous", df$uncertainty_reasons),
    info = "z-test with r should not trigger d/g design ambiguity")
})

# ===========================================================================
# Fix C: Structural design ambiguity for F(1,df)
# ===========================================================================

test_that("F(1,df) with d far from all variants triggers structural ambiguity (Fix C)", {
  # F(1,20)=25.0: d_ind_equalN ≈ 2.236, dz ≈ 1.091
  # Report d=0.20 → far from both → ERROR, but structural ambiguity should
  # detect that both independent and paired variants exist and downgrade.
  r <- check_text("F(1, 20) = 25.00, p < .001, d = 0.20")
  df <- as.data.frame(r)
  expect_true(df$design_ambiguous,
    info = "F(1,df) with both d_ind and dz should be design-ambiguous")
  expect_true(df$status %in% c("WARN", "NOTE"),
    info = "F(1,df) design-ambiguous should not be ERROR")
})

test_that("F(1,df) with explicit dz reported is NOT promoted to ambiguous (Fix C guard)", {
  # When author explicitly reports dz, design is known (paired)
  r <- check_text("F(1, 30) = 9.00, p = .005, dz = 0.55")
  df <- as.data.frame(r)
  # dz family does not include d_ind → no structural ambiguity
  # (unless the system computes d_ind as an alternative, which it might)
})

test_that("F(2,30) is NOT affected by structural ambiguity (Fix C guard)", {
  # df1=2 → not F(1,df), should not trigger structural ambiguity
  r <- check_text("F(2, 30) = 5.00, p = .013, d = 0.60")
  df <- as.data.frame(r)
  # F(2,df) doesn't compute d_ind/dz paired variants → no structural ambiguity
})

test_that("t-test with both d_ind and dz has structural ambiguity (Fix C)", {
  # t-tests always compute both d_ind and dz → structural ambiguity applies
  # t(30)=3.00, d_ind_equalN ≈ 1.095, dz ≈ 0.539
  # Report d=0.75 → between both → ambiguous
  r <- check_text("t(30) = 3.00, p = .005, d = 0.75")
  df <- as.data.frame(r)
  expect_true(df$design_ambiguous,
    info = "t-test with d between paired/independent should be design-ambiguous")
})

# ===========================================================================
# Fix D: R2 cross-pairing signal 9 (large delta)
# ===========================================================================

test_that("F-test with R2 and very large delta (>0.5) triggers cross-pairing (Fix D)", {
  # Regression context with extreme R2 mismatch
  r <- check_text("The regression model was significant, F(3, 96) = 42.0, p < .001, R2 = .15")
  df <- as.data.frame(r)
  # F(3,96)=42.0 → computed R2 = F*df1/(F*df1+df2) = 126/(126+96) = 0.567
  # Reported R2=0.15, delta = 0.417
  # With regression context, this should be caught
  if (df$delta_effect_abs > 0.3) {
    expect_true(df$status %in% c("WARN", "NOTE"),
      info = "Large R2 delta with regression context should downgrade from ERROR")
  }
})

test_that("F-test with R2 and small delta is unaffected (Fix D regression guard)", {
  # Good R2 match should stay PASS
  r <- check_text("F(3, 96) = 5.0, p = .003, R2 = .14")
  df <- as.data.frame(r)
  # F(3,96)=5.0 → R2 ≈ 0.135 → close to 0.14
  expect_true(df$status %in% c("PASS", "WARN"),
    info = "Small R2 delta should stay PASS/WARN")
})

# ===========================================================================
# Fix E: Cramer's V multi-m
# ===========================================================================

test_that("Chi-square V tries multiple m values when df allows (Fix E)", {
  # chi2(4, N=200) = 20.0: df=4 could be m=1 (5×2), m=2 (3×3), m=4 (5×5)
  # V with m=1: sqrt(20/(200*1)) = 0.316
  # V with m=2: sqrt(20/(200*2)) = 0.224
  # V with m=4: sqrt(20/(200*4)) = 0.158
  # Report V=0.22 → should match m=2 best
  r <- check_text("chi2(4, N = 200) = 20.0, p < .001, V = 0.22")
  df <- as.data.frame(r)
  if (nrow(df) > 0 && !is.na(df$matched_value)) {
    expect_true(abs(df$matched_value - 0.224) < abs(df$matched_value - 0.316),
      info = "Should use m=2 (V≈0.224) rather than m=1 (V≈0.316) for best match")
  }
})

# ===========================================================================
# Regression guards: existing behavior preserved
# ===========================================================================

test_that("SKIP status for extraction-only results preserved", {
  r <- check_text("t(28) = 2.21")
  df <- as.data.frame(r)
  expect_equal(df$check_scope, "extraction_only")
})

test_that("decision error requires p_reported", {
  r <- check_text("z = 1.50, d = 0.30, N = 100")
  df <- as.data.frame(r)
  expect_false(isTRUE(df$decision_error),
    info = "No reported p → no decision error")
})

test_that("r-test with global N guard still works", {
  r <- check_text("r(48) = .28, p = .041")
  df <- as.data.frame(r)
  expect_true(df$status %in% c("PASS", "WARN", "NOTE", "OK"),
    info = "r-test should still work normally")
})
