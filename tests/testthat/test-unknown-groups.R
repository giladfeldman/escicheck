# test-unknown-groups.R — v0.2.7 Issue 3: Unknown group sizes d/g downgrade
# When n1/n2 are not available, d/g results should NOT be ERROR.
# Multiple mechanisms may prevent ERROR: Phase 8B (design_ambiguous),
# Phase 8C (unknown_groups), or gz/gav/grm matching (Issue A).

test_that("Hedges' g with unknown n1/n2 is not ERROR (default)", {
  res <- check_text("t(100) = 3.45, p < .001, g = 1.50")
  expect_true(nrow(res) >= 1)
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect_abs[1]) &&
      res$delta_effect_abs[1] > 5 * 0.02) {
    # Should be WARN from design_ambiguous (8B) or unknown_groups (8C), not ERROR
    expect_true(res$status[1] %in% c("WARN", "NOTE", "PASS"))
    expect_true(res$confidence[1] <= 4)
  }
})

test_that("Cohen's d with unknown n1/n2 is not ERROR", {
  res <- check_text("t(20) = 4.50, p < .001, d = 2.50")
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect_abs[1]) &&
      res$delta_effect_abs[1] > 5 * 0.02) {
    expect_true(res$status[1] %in% c("WARN", "NOTE"))
    expect_true(res$design_ambiguous[1] || res$unknown_groups_downgraded[1])
  }
})

test_that("unknown_groups_action = 'ERROR' with design_ambiguous_action = 'ERROR' preserves ERROR", {
  # Both mechanisms must be set to ERROR to preserve it
  res <- check_text("t(100) = 3.45, p < .001, g = 1.50",
                     unknown_groups_action = "ERROR",
                     design_ambiguous_action = "ERROR")
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect_abs[1]) &&
      res$delta_effect_abs[1] > 5 * 0.02) {
    expect_equal(res$status[1], "ERROR")
  }
})

test_that("PASS result is not affected by unknown_groups_action", {
  res <- check_text("t(45) = 2.31, p = .023, d = 0.68")
  expect_equal(res$status[1], "PASS")
  expect_false(res$unknown_groups_downgraded[1])
})

test_that("unknown_groups_downgraded column exists", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_true("unknown_groups_downgraded" %in% names(res))
  expect_false(res$unknown_groups_downgraded[1])
})

test_that("F(1,df) with d and unknown groups is not ERROR", {
  res <- check_text("F(1, 98) = 12.5, p < .001, d = 1.30")
  expect_true(res$status[1] %in% c("WARN", "NOTE"))
})
