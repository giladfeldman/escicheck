# v0.6.6 (E-B): a correlation reported as `r(df) = value, 95% CI [..]` with NO
# co-located p-value must adopt the r as its own effect_reported (effect IS the
# r) and verify via the reported CI — not route to SKIP with effect_reported
# null. The r-adopts-itself-as-effect block in check.R only fired when
# check_type == "p_value" (i.e. a p was present); a CI-only row had check_type
# "extraction_only", so the adoption was skipped.
#
# Surfaced by the 2026-06-25 escicheck-iterate canary audit (cycle 1) against
# collabra.57785 (Experiential vs Material): the Discussion summary sentence
# "r(741) = -0.43, 95% CI [-0.49, -0.37]" and "r(741) = -0.44, 95% CI
# [-0.50, -0.38]" (both with NO co-located p) were dropped to status SKIP with
# effect_reported = NA. docpluck delivers the text; this is an effectcheck
# binding gap (same family as the v0.5.10 bare-r-with-CI feature).

test_that("a bare r(df) with a CI and no p adopts r as its effect and does not SKIP", {
  txt <- paste0(
    "Similarly, the within-subjects analysis showed a negative association, ",
    "r(741) = -0.43, 95% CI [-0.49, -0.37], as did the partial correlation, ",
    "r(741) = -0.44, 95% CI [-0.50, -0.38]."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r" &
              !is.na(res$stat_value) &
              (abs(res$stat_value - (-0.43)) < 1e-6 | abs(res$stat_value - (-0.44)) < 1e-6), ]
  expect_true(nrow(rr) >= 2)
  # effect_reported adopted from the r value (not NA)
  expect_true(all(!is.na(rr$effect_reported)))
  expect_true(all(rr$effect_reported_name == "r", na.rm = TRUE))
  # no longer SKIP
  expect_false(any(rr$status == "SKIP", na.rm = TRUE))
})

test_that("a bare r(df) with NEITHER p nor CI still routes to extraction-only (unchanged)", {
  # Guard: the adoption must require a CI; a truly unverifiable bare r(df) with
  # no p and no CI stays extraction-only (NOTE/SKIP), not a false PASS.
  txt <- "The correlation was negative, r(741) = -0.43."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r" &
              !is.na(res$stat_value) & abs(res$stat_value - (-0.43)) < 1e-6, ]
  expect_true(nrow(rr) >= 1)
  # With no p and no CI, this is not a verified effect_size match.
  expect_false(any(rr$status == "PASS", na.rm = TRUE))
})

test_that("a normal r(df) with a p still verifies (no regression)", {
  txt <- "Variables were correlated, r(48) = 0.42, p = .003."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r" &
              !is.na(res$stat_value) & abs(res$stat_value - 0.42) < 1e-6, ]
  expect_true(nrow(rr) >= 1)
  expect_true(all(rr$effect_reported_name == "r", na.rm = TRUE))
  expect_true(any(rr$status == "PASS", na.rm = TRUE))
})
