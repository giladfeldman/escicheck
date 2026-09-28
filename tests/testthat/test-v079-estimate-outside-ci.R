# v0.7.9 — the estimate-in-CI invariant must REPORT every violation, not only
# the dropped-minus shape.
#
# FOUND 2026-09-02 on a real paper, by rasterizing the page rather than reading
# any extractor: Chan & Feldman (2025), Cognition and Emotion 39(6), page 13,
# Table 9 row 2a prints
#
#     target article:  r = .70,  95% CI [0.73, 0.76]
#
# The interval does not contain its own point estimate. That is impossible as
# printed and is a genuine published error — exactly what this package exists
# to surface.
#
# effectcheck 0.7.8 said NOTHING about it, while its own message asserted
#
#     "... the estimate-in-CI invariant was checked ..."
#
# The invariant IS evaluated (check.R ~5291), but it only FIRES on the
# dropped-minus signature `!pos_in && neg_in` — estimate outside, its negation
# inside. Its comment says so: "both-in / both-out is a different defect, left
# alone". So a both-out violation produced output BYTE-IDENTICAL to a row whose
# estimate sits comfortably inside its interval:
#
#     r = .70, CI [0.73, 0.76]   ->  NOTE / UNVERIFIABLE / sign_ci_violation FALSE
#     r = .70, CI [0.64, 0.76]   ->  NOTE / UNVERIFIABLE / sign_ci_violation FALSE
#
# A message that claims a check whose only possible outcome for this input is
# silence is the "No pretending" class: the check ran, found a violation, and
# said nothing while advertising that it had looked.
#
# These tests were written against the UNFIXED code and watched to FAIL first.

test_that("v0.7.9: an estimate outside its own CI is reported, not silently passed", {
  # The real published row. No df and no N, so nothing can be recomputed —
  # which is precisely why the structural invariant is the only check available
  # and must not stay silent.
  res <- check_text("For the target article, r = .70, 95% CI [0.73, 0.76], p < .001.")

  expect_gt(nrow(res), 0)
  expect_true("estimate_outside_ci" %in% names(res))
  expect_true(isTRUE(res$estimate_outside_ci[1]))

  # And it must SAY so. A boolean nobody surfaces is the same defect one layer
  # down.
  expect_match(paste(res$uncertainty_reasons, collapse = " "),
               "outside its reported CI", fixed = TRUE)
})

test_that("v0.7.9: control — an estimate INSIDE its CI is not flagged", {
  # Same shape, same absence of df/N, estimate inside the interval. Without this
  # the test above passes for a flag that fires on everything.
  res <- check_text("For the target article, r = .70, 95% CI [0.64, 0.76], p < .001.")

  expect_gt(nrow(res), 0)
  expect_false(isTRUE(res$estimate_outside_ci[1]))
  expect_no_match(paste(res$uncertainty_reasons, collapse = " "),
                  "outside its reported CI", fixed = TRUE)
})

test_that("v0.7.9: the dropped-minus shape keeps its OWN distinct diagnosis", {
  # r = .74 reported with CI [-0.92, -0.30]: the estimate is outside and its
  # NEGATION is inside. That is the pre-existing sign_ci_violation case and its
  # message names the likely cause. The new flag must not swallow or relabel it
  # — a sign error and an unexplained impossibility need different remedies.
  res <- check_text("The association was r = .74, 95% CI [-0.92, -0.30], p = .004.")

  expect_gt(nrow(res), 0)
  expect_true(isTRUE(res$sign_ci_violation[1]))
  expect_match(paste(res$uncertainty_reasons, collapse = " "),
               "dropped-minus", fixed = TRUE)
})

test_that("v0.7.9: the flag does not fire when a bound is missing or degenerate", {
  # Conservative guards, in the spirit of CRAN design principle 5 (never emit
  # garbage): no CI at all, and a reported interval that is reversed, must not
  # be reported as an estimate-outside-CI violation. The reversed interval has
  # its own IMPOSSIBLE VALUE message already.
  no_ci <- check_text("The association was r = .70, p < .001, n = 120.")
  expect_false(any(isTRUE(no_ci$estimate_outside_ci)))

  reversed <- check_text("The correlation was r = .195, p = .240, 95% CI [0.485, 0.132].")
  expect_false(any(isTRUE(reversed$estimate_outside_ci)))
})
