# v0.6.5: an explicitly INDEPENDENT (Welch / independent-samples / two-sample)
# t-test must not be mislabeled design_inferred = "paired" just because a
# "paired"/"within-subjects" keyword appears elsewhere in the (multi-sentence)
# context window.
#
# Surfaced by the 2026-06-21 escicheck-iterate canary audit against
# collabra.57785 (Experiential vs Material): "We ran an independent Welch's
# t-test ... t(741) = 5.36, ... d = 0.39" was tagged design_inferred = "paired"
# because the surrounding text also discussed within-subjects analyses, and the
# paired keyword is checked before the independent keyword. df = 741 is an
# integer, so the v0.6.3 E2 (fractional-df) reclassification did not fire.
# Discriminator: a Welch's t-test is by definition unequal-variance
# independent-samples; there is no paired Welch test.

test_that("independent Welch t-test in a within-subjects-mentioning context is labeled independent", {
  txt <- paste0(
    "In an earlier within-subjects pilot we used repeated measures on the same participants. ",
    "We ran an independent Welch's t-test (two-tailed) comparing the conditions: ",
    "participants in the material condition (M = 4.75, SD = 1.36, N = 393) were more ",
    "willing to exchange than participants in the experiential condition ",
    "(M = 4.22, SD = 1.33, N = 350), t(741) = 5.36, p < .001, d = 0.39, 95% CI [0.25, 0.54]."
  )
  res <- effectcheck::check_text(txt)
  tt <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 5.36) < 1e-6, ]
  expect_true(nrow(tt) >= 1)
  expect_true(any(tt$design_inferred == "independent", na.rm = TRUE))
  expect_false(any(tt$design_inferred == "paired", na.rm = TRUE))
})

test_that("a genuine paired t-test (no Welch / independent keyword) stays paired", {
  txt <- paste0(
    "Using a paired-samples t-test (repeated measures on the same participants), ",
    "scores rose from pre to post, t(99) = 3.20, p = .002, dz = 0.32, 95% CI [0.12, 0.52]."
  )
  res <- effectcheck::check_text(txt)
  tt <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 3.20) < 1e-6, ]
  expect_true(nrow(tt) >= 1)
  expect_true(any(tt$design_inferred == "paired", na.rm = TRUE))
})
