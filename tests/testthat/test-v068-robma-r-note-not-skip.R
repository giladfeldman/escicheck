# v0.6.8 (E-C1-regress): a Bayesian model-averaged r (RoBMA) must ship status
# NOTE, not SKIP. The v0.6.6 block sets effect_reported_name = "r_model_averaged"
# and intends an honest NOTE (surfacing the BF01 / model-averaging provenance),
# but the status reassignment guard omitted "SKIP" AND the Phase-9 extraction-only
# SKIP downgrade re-overrode the NOTE back to SKIP. A model-averaged r carries no
# p and no adopted effect, so it reaches both rules with status == "SKIP". The fix
# (a) includes "SKIP" in the v0.6.6 guard and (b) excludes a model-averaged r from
# the Phase-9 SKIP downgrade (via a bayes_model_avg_surfaced flag, mirroring
# r_ci_surfaced). NOTE is the correct status: the BF01/CI provenance IS surfaced
# and the estimate-in-CI invariant is evaluated, so SKIP ("nothing was checked")
# is wrong.
#
# Surfaced by the 2026-06-29 escicheck-iterate cycle-1 canary audit against
# collabra.90203: the rendered RoBMA r = 0.002 (95% CI [0, 0.004], BF01 = 14.93)
# row shipped status == "SKIP".

test_that("a RoBMA model-averaged r reported with a CI routes to NOTE, not SKIP", {
  txt <- paste0(
    "When applying RoBMA to the meta-analytic data, we found strong evidence for ",
    "the absence of the average effect (BF01 = 14.93), with a model-averaged mean ",
    "effect size estimate of r = 0.002, 95% CI [0.00, 0.004]."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r" &
              !is.na(res$stat_value) & abs(res$stat_value - 0.002) < 1e-9, ]
  expect_true(nrow(rr) >= 1)
  expect_true(any(rr$effect_reported_name == "r_model_averaged", na.rm = TRUE))
  # The defining assertion: status is NOTE, not SKIP (and not PASS).
  expect_true(any(rr$status == "NOTE", na.rm = TRUE))
  expect_false(any(rr$status == "SKIP", na.rm = TRUE))
  expect_false(any(rr$status == "PASS", na.rm = TRUE))
})

test_that("the NOTE routing is not value-specific (a different model-averaged r + CI)", {
  # A different reported value with the same RoBMA provenance + CI must also land
  # at NOTE, confirming the fix is general, not pinned to r = 0.002.
  txt <- paste0(
    "Using Bayesian model-averaging across the candidate models, the model-averaged ",
    "mean effect size estimate was r = 0.05, 95% CI [-0.02, 0.12] (BF01 = 6.10)."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r" &
              !is.na(res$stat_value) & abs(res$stat_value - 0.05) < 1e-9, ]
  expect_true(nrow(rr) >= 1)
  expect_true(any(rr$effect_reported_name == "r_model_averaged", na.rm = TRUE))
  expect_true(any(rr$status == "NOTE", na.rm = TRUE))
  expect_false(any(rr$status == "SKIP", na.rm = TRUE))
})
