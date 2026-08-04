# v0.6.16 (E3 / E-ci-sign-align) -- CI comparison must follow the magnitude
# convention of the value match.
#
# Surfaced by the 2026-08-03 Sonnet canary audit of collabra.23443 (S1-R13),
# escicheck-iterate cycle 5; fixed cycle 7 (2026-08-04). The value match is
# magnitude-based, so a reported positive effect (d = 0.19) matches a computed
# value whose sign follows the arbitrary direction of the t statistic
# (t = -7.67). The CI check, however, compared the reported POSITIVE CI
# [0.14, 0.24] against the computed r-scale CI carrying the negative t sign
# ([-0.235, -0.141]), yielding a spurious ci_check_status = "INCONSISTENT"
# with fabricated deltas ~0.38. Authored against the UNFIXED code and watched
# fail (INCONSISTENT) before the sign-alignment landed.

test_that("v0.6.16: reported positive CI matches sign-flipped computed CI (collabra.23443 S1-R13)", {
  txt <- paste0(
    "Price sensitivity predicted higher estimates of self-interest in others, ",
    "t(1596) = -7.67, p < .0001, d = 0.19 [0.14, 0.24]."
  )
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$stat_value[1], -7.67)
  expect_equal(res$effect_reported[1], 0.19)
  # The CI verdict must not be a spurious INCONSISTENT: magnitude-aligned, the
  # computed CI [0.141, 0.235] sits within rounding of the reported [0.14, 0.24].
  expect_true(res$ci_check_status[1] %in% c("MATCH", "PLAUSIBLE"),
              info = paste("ci_check_status was", res$ci_check_status[1]))
  # The alignment must be visible, never silent (No-pretending rule).
  expect_true(grepl(":sign-aligned$", res$ci_method_match[1]),
              info = paste("ci_method_match was", res$ci_method_match[1]))
  # Deltas are computed on the aligned bounds -- small, not the fabricated ~0.38.
  expect_lt(res$ci_delta_lower[1], 0.05)
  expect_lt(res$ci_delta_upper[1], 0.05)
})

test_that("v0.6.16: mirror case -- reported negative CI, positive t", {
  txt <- paste0(
    "Price sensitivity predicted lower estimates, ",
    "t(1596) = 7.67, p < .0001, d = -0.19 [-0.24, -0.14]."
  )
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_true(res$ci_check_status[1] %in% c("MATCH", "PLAUSIBLE"),
              info = paste("ci_check_status was", res$ci_check_status[1]))
  expect_true(grepl(":sign-aligned$", res$ci_method_match[1]))
})

test_that("v0.6.16: genuinely wrong CI magnitudes stay INCONSISTENT despite opposite signs", {
  # Same statistic, but the reported CI magnitudes are wrong (no computed
  # candidate is near [0.45, 0.60] on any scale) -- the sign-alignment must not
  # convert a real inconsistency into a pass.
  txt <- paste0(
    "Price sensitivity predicted higher estimates of self-interest in others, ",
    "t(1596) = -7.67, p < .0001, d = 0.19 [0.45, 0.60]."
  )
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$ci_check_status[1], "INCONSISTENT")
})

test_that("v0.6.16: dropped-minus contradiction is never sign-aligned into a MATCH", {
  # Codex CLI review finding, 2026-08-04 (reproduced locally before fixing):
  # the reported estimate (r = -0.50) lies OUTSIDE its own reported CI
  # [0.34, 0.63] -- a genuine within-paper sign contradiction (the v0.6.3
  # dropped-minus signature). The sign-alignment must not fire here: the
  # direction disagreement is the paper's own, and the CI verdict must stay
  # INCONSISTENT (an unguarded flip yielded MATCH + status PASS).
  txt <- "The correlation was significant, r(98) = -0.50, p < .001, 95% CI [0.34, 0.63]."
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_true(isTRUE(res$sign_ci_violation[1]))
  expect_equal(res$ci_check_status[1], "INCONSISTENT")
  expect_false(grepl("sign-aligned", res$ci_method_match[1]))
})

test_that("v0.6.16: same-sign comparisons never carry the sign-aligned marker", {
  # Positive t, positive reported effect and CI -- the pre-v0.6.16 path,
  # unchanged: no flip, no marker.
  txt <- "The effect was significant, t(1596) = 7.67, p < .0001, d = 0.19 [0.14, 0.24]."
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_true(res$ci_check_status[1] %in% c("MATCH", "PLAUSIBLE"))
  expect_false(grepl("sign-aligned", res$ci_method_match[1]))
})
