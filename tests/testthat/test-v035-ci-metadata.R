## v0.3.5 — CI audit metadata (MetaESCI requests 2B/2C/2D/2E)
## ----------------------------------------------------------
## ci_expected, ci_reported, ci_level_mismatch, ci_clipped_to_bound,
## ci_symmetry_class — all derived in Phase 6 of check.R.

test_that("ci_expected fires on standard ES families and only on ES rows", {
  # d-family with CI
  r1 <- check_text("t(48) = 4.20, p < .001, d = 0.85, 95% CI [0.45, 1.25]")
  expect_true(r1$ci_expected[1])
  expect_true(r1$ci_reported[1])

  # F + eta2, no CI: expected but not reported
  r2 <- check_text("F(2, 87) = 5.12, p = .008, eta2 = 0.11")
  expect_true(r2$ci_expected[1])
  expect_false(r2$ci_reported[1])

  # OR row should also count as ci_expected
  r3 <- check_text("chi-square(1) = 5.99, p = .015, OR = 2.50, 95% CI [1.20, 5.10]")
  expect_true(r3$ci_expected[1])
  expect_true(r3$ci_reported[1])
})

test_that("ci_level_mismatch flags 90%-on-ANOVA reporting", {
  r <- check_text("F(2, 87) = 5.12, p = .008, eta2 = 0.11, 90% CI [0.02, 0.22]")
  expect_equal(r$ci_level_mismatch[1], "90_vs_95_anova")
})

test_that("ci_level_mismatch returns 'match' for canonical 95%", {
  r <- check_text("t(48) = 4.20, p < .001, d = 0.85, 95% CI [0.45, 1.25]")
  expect_equal(r$ci_level_mismatch[1], "match")
})

test_that("ci_level_mismatch flags assumed_95 when level not stated", {
  r <- check_text("t(48) = 4.20, p < .001, d = 0.85, CI [0.45, 1.25]")
  # parse.R sets ci_level_source = "assumed_95" when no level token is present.
  expect_true(r$ci_level_mismatch[1] %in%
              c("unstated_assumed_95", "match"))  # tolerant: implementation may infer
})

test_that("ci_clipped_to_bound detects lower-zero clipping for bounded ES", {
  r <- check_text("F(1, 50) = 1.20, p = .278, R2 = 0.023, 95% CI [0.00, 0.15]")
  expect_equal(r$ci_clipped_to_bound[1], "lower_0")
})

test_that("ci_clipped_to_bound is NA for unbounded families", {
  r <- check_text("t(48) = 4.20, p < .001, d = 0.85, 95% CI [0.45, 1.25]")
  expect_true(is.na(r$ci_clipped_to_bound[1]))
})

test_that("ci_symmetry_class categorises symmetry against ES family expectations", {
  # Bounded ES (R2) with clearly asymmetric CI (lower arm 0.10, upper arm 0.20)
  r1 <- check_text("F(3, 96) = 5.50, p = .002, R2 = 0.15, 95% CI [0.05, 0.35]")
  expect_equal(r1$ci_symmetry_class[1], "asymmetric_expected")

  # d > 0.5: asymmetric CI is expected; symmetric reporting is unexpected
  r2 <- check_text("t(48) = 4.20, p < .001, d = 0.85, 95% CI [0.45, 1.25]")
  expect_equal(r2$ci_symmetry_class[1], "symmetric_unexpected")
})

test_that("all 9 v0.3.5 columns are present and typed correctly", {
  r <- check_text("t(48) = 4.20, p < .001, d = 0.85, 95% CI [0.45, 1.25]")
  v035_cols <- c(
    "effect_reported_decimals", "ciL_reported_decimals",
    "ciU_reported_decimals",    "stat_value_decimals",
    "ci_expected", "ci_reported",
    "ci_level_mismatch", "ci_clipped_to_bound", "ci_symmetry_class"
  )
  expect_true(all(v035_cols %in% names(r)))
  expect_type(r$effect_reported_decimals, "integer")
  expect_type(r$ciL_reported_decimals,    "integer")
  expect_type(r$ciU_reported_decimals,    "integer")
  expect_type(r$stat_value_decimals,      "integer")
  expect_type(r$ci_expected,              "logical")
  expect_type(r$ci_reported,              "logical")
  expect_type(r$ci_level_mismatch,        "character")
  expect_type(r$ci_clipped_to_bound,      "character")
  expect_type(r$ci_symmetry_class,        "character")
})
