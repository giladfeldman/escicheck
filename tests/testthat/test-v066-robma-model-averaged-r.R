# v0.6.6 (E-C1): a Bayesian MODEL-AVERAGED effect (RoBMA / Bayesian
# model-averaging) reported as `r = value` is NOT a frequentist Pearson
# correlation — it is a posterior model-averaged estimate accompanied by a Bayes
# factor (BF01), not a recomputable r or a p-value. effectcheck must not flatten
# it to a plain `r` and (via the r-adopts-itself block) mark it a verified PASS;
# it routes to an honest NOTE recording its Bayesian nature.
#
# Surfaced by the 2026-06-25 escicheck-iterate canary audit against
# collabra.90203 (Identifiable Victim Effect): "When applying RoBMA ... strong
# evidence for the absence of the average effect (BF01 = 14.93), with a
# model-averaged mean effect size estimate of r = 0.002 (95% CI [0, 0.004])".
# The signal is read from the r's OWN clause only (not the wider context), so a
# stray "BF01" elsewhere in a Bayes-using paper cannot reclassify an ordinary r.

test_that("a RoBMA model-averaged r routes to NOTE, not a verified PASS", {
  txt <- paste0(
    "When applying RoBMA to the meta-analytic data, we found strong evidence for ",
    "the absence of the average effect (BF01 = 14.93), with a model-averaged mean ",
    "effect size estimate of r = 0.002, 95% CI [0.00, 0.004]."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r" &
              !is.na(res$stat_value) & abs(res$stat_value - 0.002) < 1e-9, ]
  expect_true(nrow(rr) >= 1)
  # Not a verified PASS — it is a Bayesian model-averaged estimate (NOTE/SKIP).
  expect_false(any(rr$status == "PASS", na.rm = TRUE))
  # The Bayesian nature is recorded (effect name flags it model-averaged).
  expect_true(any(rr$effect_reported_name == "r_model_averaged", na.rm = TRUE))
})

test_that("an ordinary frequentist r in a paper that also uses BF01 stays a normal r", {
  # A plain Pearson r whose OWN clause has no model-averaging phrase must NOT be
  # reclassified just because a different sentence mentions a Bayes factor.
  txt <- paste0(
    "A complementary Bayesian analysis returned BF01 = 3.2 for the null. ",
    "The two scales were positively correlated, r(98) = 0.42, p = .002."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r" &
              !is.na(res$stat_value) & abs(res$stat_value - 0.42) < 1e-9, ]
  expect_true(nrow(rr) >= 1)
  expect_true(any(rr$effect_reported_name == "r", na.rm = TRUE))
  expect_false(any(rr$effect_reported_name == "r_model_averaged", na.rm = TRUE))
  expect_true(any(rr$status == "PASS", na.rm = TRUE))
})
