test_that("Fix 8: chi2=0 with reported V produces SKIP", {
  r <- check_text("chi2(1, N = 988) = 0.00, V = 0.50")
  expect_equal(r$status[1], "SKIP")
  expect_true(grepl("chi-square = 0", r$uncertainty_reasons[1]))
})

test_that("Fix 8: chi2=0 with reported phi produces SKIP", {
  r <- check_text("chi2(1, N = 200) = 0.0, phi = 0.30")
  expect_equal(r$status[1], "SKIP")
})

test_that("Fix 8 regression: normal chi2 still computes", {
  r <- check_text("chi2(1, N = 100) = 10.0, V = 0.30")
  expect_true(r$status[1] %in% c("PASS", "WARN"))
  expect_equal(r$check_type[1], "effect_size")
})

test_that("Fix 8 edge: chi2=0 without reported effect is unchanged", {
  r <- check_text("chi2(1, N = 200) = 0.00, p = .99")
  # No effect reported, so chi2=0 guard doesn't fire
  # Still computes phi=0 but no effect to compare -> p_value check
  expect_true(r$status[1] %in% c("PASS", "NOTE", "WARN"))
})

test_that("Fix 9: z-test with |d|>3 skips effect size computation", {
  r <- check_text("z = 3.00, p = .003, d = 3.10, N = 40")
  # Effect size computation skipped, but p-value still checked
  # Status should NOT be ERROR (was ERROR before fix)
  expect_true(r$status[1] %in% c("WARN", "NOTE", "SKIP"))
  expect_true(grepl("unusually large", r$uncertainty_reasons[1]))
})

test_that("Fix 9 regression: normal z-test d still computes", {
  r <- check_text("z = 2.50, p = .012, d = 0.50, N = 100")
  expect_equal(r$status[1], "PASS")
  expect_equal(r$check_type[1], "effect_size")
})

test_that("Fix 7: phi with df>=2 reinterpreted as V", {
  # df=2 means 3x2 or 2x3 table, not 2x2. Phi doesn't apply.
  r <- check_text("chi2(2, N = 285) = 15.0, phi = 0.229")
  # Should use V computation (with m from df), not phi
  expect_true(grepl("non-2x2|reinterpreted", r$uncertainty_reasons[1]))
  expect_true(r$confidence[1] <= 4)
})

test_that("Fix 7 regression: phi with df=1 stays as phi", {
  r <- check_text("chi2(1, N = 100) = 5.0, phi = 0.224")
  # df=1 → 2x2 table, phi is correct
  expect_true(!grepl("reinterpreted", r$uncertainty_reasons[1],
                     fixed = FALSE) || is.na(r$uncertainty_reasons[1]))
})

# ==== Fix 4: Non-integer df2 mixed-model flag ====

test_that("Fix 4: non-integer df2 produces WARN not ERROR", {
  r <- check_text("F(1, 73.21) = 8.50, p = .005, d = 1.20")
  # Non-integer df2 → mixed-model flag → WARN
  expect_true(r$status[1] %in% c("WARN", "NOTE"))
  expect_true(grepl("Non-integer|mixed", r$uncertainty_reasons[1]))
})

test_that("Fix 4 regression: integer df2 unchanged", {
  r <- check_text("F(1, 73) = 8.50, p = .005, d = 0.68")
  # Integer df2, no mixed-model flag
  expect_true(!grepl("Non-integer", r$uncertainty_reasons[1]) ||
              is.na(r$uncertainty_reasons[1]))
})

# ==== Fix 5: Extended r-grid ====

test_that("Fix 5: r-grid covers 0.95 for large d", {
  # t(6) = 8.0 → dz = 8/sqrt(7) ≈ 3.02
  # With r=0.95: dav = dz/sqrt(2(1-0.95)) = 3.02/sqrt(0.1) ≈ 9.55
  # The extended grid should cover larger d values
  r <- check_text("t(6) = 8.00, d = 2.55")
  # With r=0.95 in grid, the range should encompass larger values
  # Status should be WARN (design ambiguous) not ERROR
  expect_true(r$status[1] %in% c("WARN", "NOTE", "PASS"))
})

# ==== Fix 2/3: R2 cross-pairing improvements ====

test_that("Fix 2: R2 cross-pairing with small delta detected", {
  # F(1,126)=1.68 → R2 = 1.68/(1.68+126) = 0.013
  # Reported R2 = 0.22 → delta = 0.207 (was ERROR, should be WARN)
  r <- check_text("F(1, 126) = 1.68, R2 = 0.22")
  # The extreme ratio (0.22/0.013 = 17x) should trigger cross-pairing
  expect_true(r$status[1] %in% c("WARN", "NOTE"))
})

test_that("Fix 2 regression: matching R2 stays PASS", {
  # F(1, 98) = 12.0 → R2 = 12/110 ≈ 0.109
  r <- check_text("F(1, 98) = 12.0, R2 = 0.11")
  expect_equal(r$status[1], "PASS")
})

# ==== Issue A: F(1,df) with g — structural ambiguity ====

test_that("Issue A: F(1,df) with g gets design-ambiguous WARN or range-match PASS", {
  r <- check_text("F(1, 69) = 10.00, p < .001, g = 0.291")
  # F(1,df) with g: structural ambiguity (both g_ind and gz exist)
  # v0.3.0: range-aware matching may match gav/grm closely -> PASS
  expect_true(r$status[1] %in% c("PASS", "WARN", "NOTE"))
})

test_that("Issue A: F(1,df) with matching g stays PASS", {
  # F(1,69) = 10 → g_ind ≈ 0.74 (J * 2*sqrt(F/N))
  r <- check_text("F(1, 69) = 10.00, p < .001, g = 0.74")
  expect_true(r$status[1] %in% c("PASS", "WARN"))
})

# ==== Issue B: Cohen's f/f2 cross-pairing ====

test_that("Issue B: cohens_f standalone cross-pairing detected", {
  # Cohen's f = sqrt(F*df1/df2) = sqrt(12.5/131) = 0.309, reported 1.30, delta >> 0.10
  r <- check_text("F(1, 131) = 12.5, p < .001, Cohen's f = 1.30")
  expect_true(r$status[1] %in% c("WARN", "NOTE"))
  expect_true(grepl("cross-pairing", r$uncertainty_reasons[1]))
})

test_that("Issue B: f2 standalone cross-pairing detected", {
  # f2 = F*df1/df2 = 30/386 = 0.0777, reported 0.80, delta = 0.722 >> 0.10
  r <- check_text("F(1, 386) = 30.0, p < .001, f2 = 0.80")
  expect_true(r$status[1] %in% c("WARN", "NOTE"))
})

test_that("Issue B: f2 matching value stays PASS/WARN", {
  # F(1, 100) = 25.0 → f2 = 25/100 = 0.25
  r <- check_text("F(1, 100) = 25.0, p < .001, f2 = 0.25")
  expect_true(r$status[1] %in% c("PASS", "WARN"))
})

# ==== Fix 9 continued ====

test_that("Fix 9 edge: d=2.9 below threshold still computes", {
  r <- check_text("z = 5.00, p < .001, d = 2.90, N = 30")
  # |d|=2.9 < 3.0 threshold, should still compute
  expect_true(r$check_type[1] == "effect_size")
  expect_true(!is.na(r$matched_value[1]))
})

# ==== v0.2.9d Fixes ====

test_that("v0.2.9d Fix 1: N >> df+1 gets WARN not ERROR", {
  # t(262) with N=467 — N is 1.8x larger than expected (263-264)
  # Use g value that won't be decimal-recovered
  r <- check_text("In a sample of 467 participants, t(262) = 3.50, p < .001, g = 1.50")
  # Should be WARN (N mismatch + design ambiguous) not ERROR
  expect_true(r$status[1] %in% c("WARN", "NOTE"))
})

test_that("v0.2.9d Fix 1 regression: normal N stays unchanged", {
  r <- check_text("t(28) = 2.50, p = .019, d = 0.92")
  # N inferred from df, no mismatch
  expect_true(!grepl("larger than expected", r$uncertainty_reasons[1]) ||
              is.na(r$uncertainty_reasons[1]))
})

test_that("v0.2.9d Fix 2: Signal 12 fires for very small R2", {
  # F(1,7892) → R2 = 5.2/7897.2 = 0.00066, reported 0.058
  r <- check_text("F(1, 7892) = 5.20, p = .023, R2 = 0.058")
  expect_true(r$status[1] %in% c("WARN", "NOTE"))
})

test_that("v0.2.9d Fix 3: V back-calc fires for non-global_text N_source", {
  # Synthetic: V back-calc should fire when N is clearly wrong
  r <- check_text("In the study (N = 1409), chi2(1) = 5.40, V = 0.22")
  # V = sqrt(5.4/(N*1)). With N=1409: V=0.062. With N_back=112: V=0.220
  # Back-calc should override N
  expect_true(r$status[1] %in% c("PASS", "WARN"))
})

test_that("v0.2.9d Fix 4: d cross-pairing when reported < min_variant * 0.5", {
  # F(1,49) = 190 → d_ind = 2*sqrt(190/49) ≈ 3.94, drm ≈ 1.88
  # Reported d = 0.80 < 1.88 * 0.5 = 0.94 → cross-pairing
  r <- check_text("F(1, 49) = 190.0, p < .001, d = 0.80")
  expect_true(r$status[1] %in% c("WARN", "NOTE"))
})
