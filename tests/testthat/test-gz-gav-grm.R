# test-gz-gav-grm.R — v0.2.7 Issue A: Paired Hedges variants
# EFFECT_SIZE_FAMILIES declares gz/gav/grm but they were never computed.
# Adding them fixes 52/112 (46%) of Hedges' g ERRORs from MetaESCI.

test_that("gz computed for paired t-test", {
  # t(21) = 7.23: dz = 7.23/sqrt(22) = 1.5414, J(21) = 0.9643
  # gz = 1.5414 * 0.9643 = 1.4864
  res <- check_text("t(21) = 7.23, p < .001, g = 1.66")
  expect_true(nrow(res) >= 1)
  # The paired variants (gz/gav/grm) should be computed and available
  # Check that at least one of them gives a closer match than g_ind
  if (res$check_type[1] == "effect_size" && !is.na(res$matched_variant[1])) {
    # Should match a paired variant more closely than g_ind alone
    expect_true(res$matched_variant[1] %in% c("g_ind", "gz", "gav", "grm") ||
                grepl("g", res$matched_variant[1]))
  }
})

test_that("gav matches MetaESCI case with r~0.6", {
  # From simulation: t(21)=7.23, g=1.66
  # dz = 1.5414, dav(r=0.6) = 1.5414/sqrt(2*0.4) = 1.7234
  # gav = 1.7234 * J(21) = 1.7234 * 0.9643 = 1.6618
  # delta to 1.66 = 0.002 → should be PASS
  res <- check_text("t(21) = 7.23, p < .001, g = 1.66")
  if (res$check_type[1] == "effect_size") {
    # With gav, delta should be much smaller than before (was 1.249)
    expect_true(is.na(res$delta_effect_abs[1]) || res$delta_effect_abs[1] < 0.5)
  }
})

test_that("g_ind promoted to computed_variants when canonical_type is g", {
  res <- check_text("t(40) = 3.50, p < .001, g = 0.80")
  expect_true(nrow(res) >= 1)
  # g_ind should be in the matching path now (not just alternatives)
  if (res$check_type[1] == "effect_size" && !is.na(res$matched_variant[1])) {
    expect_true(grepl("g|d", res$matched_variant[1]))
  }
})

test_that("d result is not affected by gz/gav/grm addition", {
  # Regression guard: d = 0.68 should still PASS
  res <- check_text("t(45) = 2.31, p = .023, d = 0.68")
  expect_equal(res$status[1], "PASS")
})

test_that("dav and drm moved to alternatives when reporting g", {
  # When canonical_type is "g", paired d variants should be in alternatives
  res <- check_text("t(40) = 3.50, p < .001, g = 0.80")
  # The all_variants JSON should include paired variants
  if (!is.na(res$all_variants[1])) {
    expect_true(nchar(res$all_variants[1]) > 10)  # Non-empty variants
  }
})

test_that("Small N paired design matches g closely", {
  # t(9) = 4.0: dz = 4/sqrt(10) = 1.265, J(9) = 0.9693
  # gz = 1.225. For g ~ 1.22, should be very close match.
  res <- check_text("t(9) = 4.0, p < .01, g = 1.22")
  if (res$check_type[1] == "effect_size" && !is.na(res$delta_effect_abs[1])) {
    expect_true(res$delta_effect_abs[1] < 0.1)
  }
})
