# Stage 1 / Gap 3 -- Cohen's h reported as the effect size of a one-proportion
# / goodness-of-fit chi-square.
#
# Stage 1 P6 built only the two-proportion-z -> h path. When a chi-square row
# reports h as its effect size, effectcheck used to compute phi and match the
# reported h against it (matched_variant = "phi", matched_value ~ 0.59 vs a
# reported h of -0.26) -- a meaningless cross-family comparison, plus a spurious
# CI mismatch (the h CI compared against a phi CI) that pushed status to WARN.
# h depends on two specific proportions that this chi-square does not pin down,
# so the honest outcome is "recognised, cannot verify".
#
# 2026-08-05 (triple-verification audit): the assertion below used to grep for
# the literal phrase "not recoverable from the chi-square". That phrasing was
# retracted as OVERBROAD -- for df = 1 against an explicit 50-50 null with N
# known, h IS determined up to sign (chi2 = N*(2*phat-1)^2). The fixture here is
# df = 2 (three categories), where no such recovery exists, so the BEHAVIOUR is
# unchanged and correct. The test now pins the behaviour (unmatched + NOTE +
# a surfaced reason) rather than a specific sentence, so a wording correction
# cannot masquerade as a regression -- and a real regression still fails.

test_that("Gap 3: Cohen's h on a chi-square is not mis-matched to phi", {
  # The one-proportion result from 10.1098/rsos.250367 (Study 1). The chi-square
  # symbol and Unicode minus are given here in ASCII (effectcheck normalises
  # both before parsing); the routing under test is independent of that.
  txt <- paste0(
    "We conducted a one-proportion test among the three choices and found ",
    "support for the hypothesis that people think Mr. Fry (no movie bundle ",
    "possible; bought single tickets; count = 245, proportion = 37%) is more ",
    "likely to purchase the third ticket than Mr. Munn (three-movie bundle ",
    "possible; bought single tickets; count = 51, proportion = 8%), ",
    "chi2(2, N = 659) = 225.95, p < 0.001, h = -0.26, ",
    "95% CI [-0.42, -0.10]."
  )
  res <- check_text(txt)
  row <- res[res$test_type == "chisq", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$reported_type[1], "h")
  expect_equal(row$effect_reported[1], -0.26, tolerance = 1e-6)
  # The defect: reported h was matched against the computed contingency phi.
  expect_true(is.na(row$matched_variant[1]))
  expect_true(is.na(row$matched_value[1]))
  # Recognised but unverifiable -> NOTE, never WARN/ERROR.
  expect_equal(row$status[1], "NOTE")
  # The honest "cannot verify" reason must be surfaced. Pinned on the stable
  # semantic content (it names Cohen's h and the underlying proportions), not on
  # the exact sentence -- see the header note on the 2026-08-05 wording fix.
  reasons <- paste(row$uncertainty_reasons, collapse = " ")
  expect_true(grepl("Cohen's h", reasons, fixed = TRUE))
  expect_true(grepl("proportions", reasons, fixed = TRUE))
  expect_true(grepl("not independently verified", reasons, fixed = TRUE))
  # And it must NOT re-assert the retracted impossibility claim.
  expect_false(grepl("not recoverable from the chi-square", reasons, fixed = TRUE))
})

test_that("Gap 3 guard: a genuine phi on a contingency chi-square still checks", {
  # A real phi reported on a chi-square must still be matched and verified --
  # the Gap 3 guard is scoped to reported_type == "h" only.
  res <- check_text("A contingency analysis, chi2(1, N = 200) = 8.0, p = .005, phi = .20.")
  row <- res[res$test_type == "chisq", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$matched_variant[1], "phi")
  expect_equal(row$status[1], "PASS")
})
