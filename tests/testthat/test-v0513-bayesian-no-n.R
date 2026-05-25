# Regression test for v0.5.13 — Bayesian model-averaged r/d/etc. should not
# inherit a global-text N attribution. Caught by the 2026-05-24
# escicheck-iterate cycle-4 validation against the Collabra Identifiable-Victim
# stats gold: a `r = 0.002 (95% CI [0; 0.004])` from a RoBMA / Bayesian model
# average was extracted with df1=1002, N=1004 borrowed from a much later
# sentence about a frequentist r(1002) correlation.

test_that("RoBMA r=value with CI does not inherit global-text N", {
  # Realistic Bayesian-context paragraph followed by an unrelated frequentist
  # correlation that establishes a global N — the bug was the first sentence
  # picking up the second sentence's N.
  txt <- paste(
    "When applying RoBMA to the data by Lee and Freely (2016), we found",
    "moderate evidence for publication bias (BF_01 = 0.11) and strong",
    "evidence for the absence of the average effect, with a model-averaged",
    "mean effect size estimate of r = 0.002 (95% CI [0; 0.004]).",
    "",
    "Separately, we tested whether trait X predicted Y in a Pearson",
    "correlation, r(1002) = 0.54, p < .001."
  )
  res <- effectcheck::check_text(txt)
  # Find the row with stat_value ~ 0.002
  robma_row <- res[!is.na(res$stat_value) & abs(res$stat_value - 0.002) < 1e-6, ]
  expect_gt(nrow(robma_row), 0L)
  expect_true(is.na(robma_row$N[1L]) || robma_row$N_source[1L] == "bayesian_model_no_n")
})

test_that("Bayesian model-averaging context (no RoBMA keyword) also suppresses global-N", {
  txt <- paste(
    "We applied Bayesian model-averaging to the meta-analytic dataset",
    "and obtained r = 0.05 (95% CI [-0.01, 0.10]).",
    "",
    "An unrelated study reported r(500) = 0.40, p < .001."
  )
  res <- effectcheck::check_text(txt)
  bma_row <- res[!is.na(res$stat_value) & abs(res$stat_value - 0.05) < 1e-6, ]
  expect_gt(nrow(bma_row), 0L)
  expect_true(is.na(bma_row$N[1L]) || bma_row$N_source[1L] == "bayesian_model_no_n")
})

test_that("Regular bare-r with no Bayesian context still inherits global-N", {
  # Sanity check: the guard must be specific to Bayesian context, not break
  # the existing bare-r-with-global-N behavior for non-Bayesian text.
  txt <- paste(
    "Our sample comprised N = 200 participants.",
    "",
    "Trait X was correlated with Y, r = 0.30, p < .001, 95% CI [0.16, 0.42]."
  )
  res <- effectcheck::check_text(txt)
  r_row <- res[!is.na(res$stat_value) & abs(res$stat_value - 0.30) < 1e-6, ]
  expect_gt(nrow(r_row), 0L)
  # N should be picked up here (no Bayesian context to suppress it)
  expect_false(is.na(r_row$N[1L]))
})
