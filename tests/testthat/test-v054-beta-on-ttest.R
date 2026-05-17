# v0.5.4 -- standardized regression coefficient (beta) reported on a plain t-test
#
# escicheck-iterate cycle 3 (2026-05-17): a standardized regression coefficient
# (beta) reported as the effect size of a t-test -- "(β = 0.83, t(261) = 5.82,
# p < .001)" -- was cross-matched against the t-test's Cohen's d variants
# (matched_variant = d_ind_equalN / gav / drm). A beta is not a Cohen's d; a
# multi-predictor / mediation beta is not recoverable from the t-statistic
# alone, so matching it to a computed d is "garbage statistics" (design
# principle 5) -- the PASS/NOTE verdict depended on whether the beta value
# coincidentally resembled the computed d. Fix: when canonical_type == "beta"
# and tt == "t", leave it unmatched and report an honest NOTE (mirrors Gap 3).
# The verbatims are the real gold strings from 10.1080/02699931.2024.2434156.

test_that("v0.5.4: a beta on a t-test is not cross-matched to a Cohen's d", {
  cases <- c(
    "we found an association between perceived apology and forgiveness (β = 0.83, t(261) = 5.82, p < .001)",
    "between empathy and forgiveness (β = 0.74, t(260) = 11.32, p < .001)",
    "and between apology and empathy (β = 0.91, t(261) = 8.24, p < .001)"
  )
  for (vq in cases) {
    res <- check_text(vq)
    row <- res[!is.na(res$test_type) & res$test_type == "t", , drop = FALSE]
    expect_equal(nrow(row), 1, info = vq)
    expect_equal(row$reported_type[1], "beta", info = vq)
    # the defect: the reported beta was matched to a d-family variant
    expect_true(is.na(row$matched_variant[1]), info = vq)
    expect_true(is.na(row$matched_value[1]), info = vq)
    # recognised but unverifiable -> NOTE, never a coincidental PASS
    expect_equal(row$status[1], "NOTE", info = vq)
    expect_true(grepl("not recoverable",
                      paste(row$uncertainty_reasons, collapse = " ")), info = vq)
  }
})

test_that("v0.5.4 guard: a genuine Cohen's d on a t-test still matches", {
  res <- check_text("an independent-samples t-test, t(98) = 2.10, p = .038, d = 0.42")
  row <- res[!is.na(res$test_type) & res$test_type == "t", , drop = FALSE]
  expect_equal(nrow(row), 1)
  expect_false(is.na(row$matched_variant[1]))
  expect_equal(row$status[1], "PASS")
})

test_that("v0.5.4 guard: a plain t-test with no effect size is unaffected (NA-safe)", {
  # canonical_type is NA here -- the beta/t guard condition must be NA-safe so
  # it does not error on the common "t(df) = x, p = y" no-effect-size case.
  res <- check_text("t(45) = 2.31, p = .023")
  row <- res[!is.na(res$test_type) & res$test_type == "t", , drop = FALSE]
  expect_equal(nrow(row), 1)
})
