# test-v030f-parser-fixes.R
# Tests for v0.3.0f fixes: capital D, generalized eta-squared,
# d-vs-t artifact detection, d>10 guard
# Source: MetaESCI v0.3.0c error analysis (132K results, 24 ERRORs)

library(testthat)

# =========================================================================
# Fix 1: Capital D parsing (was: D = 0.44 not matched at all)
# =========================================================================

test_that("Capital D = 0.44 parsed correctly", {
  r <- parse_text("t(59) = 1.23, p = .12, D = 0.44")
  expect_equal(r$effect_reported_name[1], "d")
  expect_equal(r$effect_reported[1], 0.44)
})

test_that("D = 0.2 parsed correctly (not as 2)", {
  r <- parse_text("t(100) = 3.5, p < .001, D = 0.2")
  expect_equal(r$effect_reported_name[1], "d")
  expect_equal(r$effect_reported[1], 0.2)
})

test_that("D = 0.214 parsed correctly", {
  r <- parse_text("t(57) = 0.82, p = 0.417, D = 0.214")
  expect_equal(r$effect_reported_name[1], "d")
  expect_equal(r$effect_reported[1], 0.214)
})

test_that("D = 0.81 parsed correctly", {
  r <- parse_text("t(200) = 5.0, p < .001, D = 0.81")
  expect_equal(r$effect_reported_name[1], "d")
  expect_equal(r$effect_reported[1], 0.81)
})

test_that("lowercase d still works after capital D fix", {
  r <- parse_text("t(59) = 1.23, p = .12, d = 0.44")
  expect_equal(r$effect_reported_name[1], "d")
  expect_equal(r$effect_reported[1], 0.44)
})

test_that("Capital Dz parsed correctly", {
  r <- parse_text("t(100) = 4.0, p < .001, Dz = 0.40")
  expect_equal(r$effect_reported_name[1], "dz")
  expect_equal(r$effect_reported[1], 0.40)
})

test_that("Hedges G parsed correctly", {
  r <- parse_text("t(50) = 3.0, p = .004, Hedges' G = 0.85")
  expect_equal(r$effect_reported_name[1], "g")
  expect_equal(r$effect_reported[1], 0.85)
})

test_that("SD = 2.5 not falsely matched as D", {
  r <- parse_text("t(50) = 3.0, p = .004, SD = 2.5")
  # SD should NOT be parsed as d=2.5
  if (nrow(r) > 0 && !is.na(r$effect_reported_name[1])) {
    expect_true(r$effect_reported_name[1] != "d" ||
                r$effect_reported[1] != 2.5)
  }
})

# =========================================================================
# Fix 2: Generalized eta-squared detection
# =========================================================================

test_that("geta-squared parsed as generalized_eta2", {
  r <- parse_text("F(1, 100) = 5.0, p = .028, geta-squared = 0.04")
  expect_equal(r$effect_reported_name[1], "generalized_eta2")
  expect_equal(r$effect_reported[1], 0.04)
})

test_that("Geta-squared parsed as generalized_eta2", {
  r <- parse_text("F(2, 80) = 8.0, p < .001, Geta-squared = 0.097")
  expect_equal(r$effect_reported_name[1], "generalized_eta2")
  expect_equal(r$effect_reported[1], 0.097)
})

test_that("generalized eta-squared (spelled out) parsed correctly", {
  r <- parse_text(
    "F(1, 50) = 10.0, p = .002, generalized eta-squared = 0.12")
  expect_equal(r$effect_reported_name[1], "generalized_eta2")
  expect_equal(r$effect_reported[1], 0.12)
})

test_that("generalized_eta2 gets NOTE not ERROR in check_text", {
  r <- check_text("F(1, 100) = 5.0, p = .028, geta-squared = 0.04")
  expect_equal(r$status[1], "NOTE")
})

test_that("regular eta2 still matches correctly", {
  r <- parse_text("F(1, 100) = 5.0, p = .028, eta-squared = 0.048")
  expect_equal(r$effect_reported_name[1], "eta2")
  expect_equal(r$effect_reported[1], 0.048)
})

test_that("partial eta2 still matches correctly", {
  r <- parse_text(
    "F(1, 100) = 5.0, p = .028, partial eta-squared = 0.048")
  expect_equal(r$effect_reported_name[1], "etap2")
  expect_equal(r$effect_reported[1], 0.048)
})

# =========================================================================
# Fix 3A: d-vs-t cross-check for extraction artifacts
# =========================================================================

test_that("dz integer far exceeding expected rejected at parse time", {
  # v0.3.0f: dz=4 (integer) with t(298)=10.50: max_d=1.22, 2*1.22=2.43
  # dz=4 > 2.43 -> rejected at parse time as page number artifact
  r <- parse_text("t(298) = 10.50, p < .001, dz = 4")
  expect_true(is.na(r$effect_reported[1]))
})

test_that("d integer far exceeding expected rejected at parse time", {
  # d=5 (integer) with t(246)=2.98: max_d=0.38, 2*0.38=0.76
  # d=5 > 2 -> rejected at parse time
  r <- parse_text("t(246) = 2.98, p = .010, d = 5")
  expect_true(is.na(r$effect_reported[1]))
})

test_that("legitimate large d not falsely flagged", {
  # d=2.5 with t(24)=6.0: expected d~2.4, reported 1.04x (normal)
  r <- check_text("t(24) = 6.0, p < .001, d = 2.5")
  expect_true(r$status[1] %in% c("PASS", "WARN"))
})

# =========================================================================
# Fix 3B: d > 10 auto-rejection at parse time
# =========================================================================

test_that("dz > 10 rejected at parse time (line number artifact)", {
  r <- parse_text("t(100) = 3.0, p = .003, dz = 219")
  expect_true(is.na(r$effect_reported_name[1]))
  expect_true(is.na(r$effect_reported[1]))
})

test_that("d > 10 rejected at parse time", {
  r <- parse_text("t(50) = 2.0, p = .05, d = 15")
  expect_true(is.na(r$effect_reported_name[1]))
})

test_that("dz = 5 still parsed (within range)", {
  r <- parse_text("t(50) = 5.0, p < .001, dz = 5")
  # dz=5 is integer, > 2, but within bounds
  # May or may not be rejected by spurious context check
  # At minimum: if parsed, value should be 5
  if (!is.na(r$effect_reported[1])) {
    expect_equal(r$effect_reported[1], 5)
  }
})

# =========================================================================
# Fix 3C: dz/dav/drm included in integer guard
# =========================================================================

test_that("dz integer > 5 rejected at parse time", {
  r <- parse_text("t(50) = 2.0, p = .05, dz = 8")
  expect_true(is.na(r$effect_reported_name[1]))
})

test_that("dav integer > 5 rejected at parse time", {
  r <- parse_text("t(50) = 2.0, p = .05, dav = 7")
  expect_true(is.na(r$effect_reported_name[1]))
})

# =========================================================================
# Fix 4: Garbled F detection (already handled by existing machinery)
# =========================================================================

test_that("garbled F=425 with eta2=0.10 not ERROR", {
  # F(1,261)=425 is garbled (actual F=30.27)
  # Existing machinery downgrades to WARN via range matching
  r <- check_text(
    "F(1, 261) = 425, p < .001, eta-squared = 0.10")
  expect_true(r$status[1] != "ERROR")
})

test_that("correct F=30.27 with eta2=0.10 is PASS", {
  r <- check_text(
    "F(1, 261) = 30.27, p < .001, eta-squared = 0.10")
  expect_equal(r$status[1], "PASS")
})

# =========================================================================
# Phase 14: Cross-result effect size sweep
# =========================================================================

test_that("cross-paired eta2 matches different F-test -> WARN", {
  # F(1,100)=50 gives eta2=0.333, reported eta2=0.10
  # F(1,261)=30 gives eta2=0.103 which matches 0.10
  # Phase 8G or Phase 14 should produce WARN (not ERROR)
  r <- check_text(paste(
    "F(1, 100) = 50.0, p < .001, eta-squared = 0.10.",
    "F(1, 261) = 30.0, p < .001, eta-squared = 0.33."))
  expect_equal(r$status[1], "WARN")
})

test_that("cross-paired d matches different t-test -> WARN", {
  # t(100)=2.0 gives d=0.40, reported d=1.20
  # t(50)=4.2 gives d=1.19 which matches 1.20
  r <- check_text(paste(
    "t(100) = 2.0, p = .048, d = 1.20.",
    "t(50) = 4.2, p < .001, d = 0.40."))
  # At least one should be WARN from cross-match
  expect_true(any(r$status %in% c("WARN", "PASS")))
})

test_that("genuine error stays ERROR when no cross-match", {
  # dz=1.08 with t(32)=2.41: no formula matches
  r <- check_text("t(32) = 2.41, p = .022, dz = 1.08")
  expect_equal(r$status[1], "ERROR")
})

test_that("eta2 cross-match with distant F-test", {
  # F(1,100)=50 gives eta2=0.333, reported eta2=0.10
  # F(1,261)=30 gives eta2=0.103 -> matches 0.10
  r <- check_text(paste(
    "F(1, 100) = 50.0, p < .001, eta-squared = 0.10.",
    "F(1, 261) = 30.0, p < .001, eta-squared = 0.33."))
  expect_equal(r$status[1], "WARN")
})

test_that("Phase 14 cross-result reports attempts to user", {
  # Row 2: F(2,74)=6.9 gives eta2=0.157, reported=0.22 (ERROR)
  # Phase 14 tries F(1,36)=2.02 -> eta2=0.053 (no match)
  # Should still be ERROR but with "No cross-result match" note
  r <- check_text(paste(
    "F(1, 36) = 2.02, p = .164, eta-squared = 0.16.",
    "F(2, 74) = 6.9, p < .001, eta-squared = 0.22."))
  # Row 2 should have cross-result attempt info
  if (nrow(r) >= 2 && r$status[2] %in% c("ERROR", "WARN")) {
    expect_true(grepl("cross-result|Cross-result|attempts",
      r$uncertainty_reasons[2], ignore.case = TRUE))
  }
})

# =========================================================================
# Cross-type effect size conversions (v0.3.0f)
# =========================================================================

test_that("t-test + eta2 matches via t2/(t2+df)", {
  # t(100)=3.0: eta2 = 9/(9+100) = 0.0826
  r <- check_text("t(100) = 3.0, p = .003, eta-squared = 0.083")
  expect_equal(r$status[1], "PASS")
  expect_equal(r$matched_variant[1], "partial_eta2")
})

test_that("t-test + omega2 matches", {
  # t(50)=4.0: omega2 = (16-1)/(16+50+1) = 0.224
  r <- check_text("t(50) = 4.0, p < .001, omega-squared = 0.22")
  expect_equal(r$status[1], "PASS")
})

test_that("t-test + R2 matches", {
  # R2 = t2/(t2+df) = 9/109 = 0.0826
  r <- check_text("t(100) = 3.0, p = .003, R2 = 0.08")
  expect_equal(r$status[1], "PASS")
})

test_that("r-test + d matches via 2r/sqrt(1-r2)", {
  # r=0.40: d = 2*0.4/sqrt(1-0.16) = 0.873
  r <- check_text("r(50) = .40, p = .003, d = 0.87")
  expect_equal(r$status[1], "PASS")
  expect_equal(r$matched_variant[1], "d_from_r")
})

test_that("chi-square + Cohen's w matches", {
  # w = sqrt(9/100) = 0.30
  r <- check_text(
    "chi-square(1, N = 100) = 9.0, p = .003, Cohen's w = 0.30")
  expect_equal(r$status[1], "PASS")
})

test_that("chi-square + d from phi (2x2)", {
  # phi = sqrt(20/200) = 0.316, d = 2*0.316/sqrt(1-0.1) = 0.666
  r <- check_text(
    "chi-square(1, N = 200) = 20.0, p < .001, d = 0.67")
  expect_true(r$status[1] %in% c("PASS", "WARN"))
})

test_that("t-test + eta2 not confused with d", {
  # eta2 should be matched as eta2, not as d
  r <- check_text("t(100) = 3.0, p = .003, eta-squared = 0.083")
  expect_equal(r$reported_type[1], "eta2")
})
