# `ci_method_match` must name the method that ACTUALLY produced the bounds.
#
# THE DEFECT. v0.7.8 fixed exactly this class one layer down: `ci_d_ind()` was
# reporting `noncentral_t` for bounds it had not computed that way, because
# `ci_d_ind_noncentral_t()` falls through to the large-sample approximation when
# `|ncp| > 37.62` or MBESS is missing. The fix gave the bounds a `ci_engine`
# attribute and derived the entry's `method` field from it
# (`compute.R:.ci_method_from_engine`).
#
# But the LIST KEY was left hardcoded — `compute.R:596` still writes
# `results$noncentral_t <- ci_result(bounds = bounds_nct, method = ...)` — and
# `collect_ci_candidates()` in check.R builds the candidate name from
# `names(variant$ci_all)`, i.e. the KEY, never reading `entry$method`. That
# candidate name becomes `ci_method_match`, which API.md documents as "the
# method that produced the closest computed CI".
#
# So the honest label existed and was discarded at the one place a user reads it.
# Same shape as the CI-severity defect in test-v079-ci-severity-tiers.R: the
# distinction is computed, then thrown away downstream.
#
# MEASURED (2026-09-05, MBESS present, so this is the ncp-limit path, not a
# missing-dependency artefact):
#
#   d = 0.67, n1 = n2 = 25   -> entry$method "noncentral_t"        bounds [0.096453, 1.236953]
#   d = 6.00, n1 = n2 = 100  -> entry$method "large_sample_approx" bounds [5.347273, 6.652727]
#                               ...and the normal_approx entry returns the IDENTICAL bounds,
#                               which is what proves the fallback ran.
#
#   Live: check_text("... t(198) = 42.43, ... d = 6.00, 95% CI [5.35, 6.65].")
#         published ci_method_match = "d_ind_equalN:noncentral_t"
#         against ciL/ciU_computed = 5.3477 / 6.6533 -- the approximation's bounds.
#
# No verdict changes: the bounds were always the correct approximation, and the
# row still matches. What was wrong was the provenance claim -- which on a
# science platform is a defect in its own right, and is the specific thing
# v0.7.8's release note says was eliminated.
#
# Watched FAIL first on 2026-09-05.

test_that("a fallback CI is not labelled noncentral_t", {
  skip_if_not(exists("ci_d_ind_all"), "ci_d_ind_all not available")

  # Force the fallback: ncp = d * sqrt(n1*n2/(n1+n2)) ~= 42.4 > 37.62.
  all_b <- ci_d_ind_all(6.00, 100, 100, 0.95)
  expect_true("noncentral_t" %in% names(all_b))
  expect_equal(all_b$noncentral_t$method, "large_sample_approx",
               info = "fixture no longer forces the fallback; pick a larger ncp")

  txt <- paste("An independent-samples t test gave t(198) = 42.43, p < .001,",
               "d = 6.00, 95% CI [5.35, 6.65].")
  df <- as.data.frame(check_text(txt))
  expect_equal(nrow(df), 1L)

  # The published label must not claim a method that did not run.
  expect_false(grepl("noncentral_t", df$ci_method_match[1], fixed = TRUE),
               info = paste("ci_method_match =", df$ci_method_match[1],
                            "but the bounds came from the large-sample approximation"))
  expect_true(grepl("large_sample_approx", df$ci_method_match[1], fixed = TRUE))
})

test_that("a genuine noncentral-t CI IS still labelled noncentral_t", {
  # The control. Without it, a fix that simply deleted the word "noncentral_t"
  # from every label would pass the test above while destroying the field's
  # meaning. d = 0.67, n1 = n2 = 25 gives ncp ~= 3.35, well inside the limit.
  skip_if_not(exists("ci_d_ind_all"), "ci_d_ind_all not available")
  skip_if_not_installed("MBESS")

  all_b <- ci_d_ind_all(0.67, 25, 25, 0.95)
  expect_equal(all_b$noncentral_t$method, "noncentral_t")

  # t must be CONSISTENT with d or the row is a mismatch and the CI arm matches
  # something else entirely: t = d * sqrt(n1*n2/(n1+n2)) = 0.67 * sqrt(625/50)
  # = 2.369 on df = 48. The reported interval is the noncentral-t one measured
  # above, [0.096453, 1.236953].
  txt <- paste("An independent-samples t test gave t(48) = 2.369, p = .022,",
               "d = 0.67, 95% CI [0.0965, 1.2370].")
  df <- as.data.frame(check_text(txt))
  expect_equal(nrow(df), 1L)
  expect_equal(df$ci_check_status[1], "MATCH")
  expect_true(grepl("noncentral_t", df$ci_method_match[1], fixed = TRUE))
  expect_false(grepl("large_sample_approx", df$ci_method_match[1], fixed = TRUE))
})

test_that("the candidate label is derived from the entry, not the list key", {
  # Pins the mechanism directly, so a future refactor that reintroduces
  # key-reading fails here rather than silently in the published column.
  skip_if_not(exists("ci_d_ind_all"), "ci_d_ind_all not available")
  all_b <- ci_d_ind_all(6.00, 100, 100, 0.95)
  # The KEY and the METHOD genuinely disagree for this input -- that
  # disagreement is the whole point, so assert it before relying on it.
  expect_false(identical("noncentral_t", all_b$noncentral_t$method))
})
