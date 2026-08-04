# v0.6.16 (E4 / E-delta-vs-matched-value) -- the emitted delta and the emitted
# matched_value must describe the SAME number.
#
# Found by the 2026-08-04 Sonnet canary audit of collabra.57785 and reproduced
# minimally before fixing. For a correlation-dependent variant (drm / dav: the
# value depends on the unknown within-pair correlation and is computed as a grid
# with $range + $grid_values), `delta_effect_abs` was the distance to the nearest
# GRID POINT while `matched_value` was the r-midpoint estimate. The row published
# three mutually contradictory numbers -- reported 0.15, matched 0.1095, delta
# 0.0038 -- where the true gap to the midpoint is 0.0405, a 10x understatement in
# the field that drives the PASS/WARN/ERROR threshold and that users read as the
# reported-vs-computed gap.
#
# The verdict threshold is deliberately unchanged (the grid distance is the
# statistically honest question for a correlation-dependent effect: "is the
# reported value achievable under some plausible within-pair r?"). What is fixed
# is that the row now PUBLISHES the grid point it measured against.
#
# Authored against the UNFIXED code and watched fail (matched_value came back
# 0.1094622 against a delta of 0.0038) before the fix landed.

test_that("v0.6.16 E4: delta_effect equals |effect_reported| - |matched_value| (grid variant)", {
  txt <- paste0(
    "To replicate Study 3A examining satisfaction of purchases based on the recalls of both ",
    "experiential and material purchases, we ran a two-tailed paired t-test and found that ",
    "participants were more satisfied with experiential purchases (N = 743, M = 8.10, SD = 1.31) ",
    "than material purchases (M = 7.92, SD = 1.28), t(742) = 3.15, p = .002, d = 0.15, ",
    "95% CI [0.07, 0.22]."
  )
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$matched_variant[1], "drm")
  eff <- res$effect_reported[1]
  mv  <- res$matched_value[1]
  de  <- res$delta_effect[1]
  expect_false(is.na(mv))
  expect_false(is.na(de))
  # The invariant: the published delta is the published gap.
  expect_equal(de, abs(abs(eff) - abs(mv)), tolerance = 1e-6)
  # And the delta must not be the pre-fix midpoint-vs-gridpoint mixture.
  expect_lt(abs(mv - 0.146176), 1e-4)
  # The grid provenance must be stated, never silent (No-pretending rule).
  expect_true(grepl("grid point", res$assumptions_used[1], fixed = TRUE),
              info = paste("assumptions_used was", res$assumptions_used[1]))
})

test_that("v0.6.16 E4: the delta/matched_value invariant holds for a non-grid variant", {
  # An independent-samples d has no correlation grid; the invariant must hold
  # there too (this path was already correct -- pinned so it stays correct).
  txt <- paste0(
    "An independent-samples t-test showed a difference between conditions, ",
    "t(198) = 2.50, p = .013, d = 0.35, 95% CI [0.07, 0.63]."
  )
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  eff <- res$effect_reported[1]
  mv  <- res$matched_value[1]
  de  <- res$delta_effect[1]
  if (!is.na(mv) && !is.na(de)) {
    expect_equal(de, abs(abs(eff) - abs(mv)), tolerance = 1e-6)
  }
})
