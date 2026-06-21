# v0.6.5: chi-square sub-type classification must not mislabel a goodness-of-fit
# / contingency chi-square as McNemar merely because a SEPARATE, OR-based
# McNemar test is mentioned in the same sentence.
#
# Surfaced by the 2026-06-21 escicheck-iterate canary audit against
# collabra.37122 (Regret action/inaction): four reversal goodness-of-fit
# chi-squares, each reported "chi2(1, N) = X, p, V = Y" and immediately
# followed by "We also conducted a McNemar test, ... OR = Z", were all tagged
# chisq_subtype = "mcnemar". That suppressed the verifiable Cramer's V (routed
# to the "McNemar effect size not recoverable" NOTE) for a test that is not a
# McNemar at all. Discriminator: a McNemar test yields an odds ratio from
# discordant pairs, never a Cramer's V.

test_that("gof chi-square reporting a Cramer's V is not mislabeled McNemar", {
  # Mirrors the collabra.37122 form: a one-df proportion (goodness-of-fit)
  # chi-square WITH a reported V, followed by a separate McNemar-OR clause.
  txt <- paste0(
    "The proportions were not significantly different from 50-50, ",
    "chi2 (1, N = 265) = 0.00, p = .951, V = 0.00, 95% CI [0.00, 0.14]. ",
    "We also conducted a McNemar test, OR = 0.99, 95% CI [0.77, 1.27], p = 1."
  )
  res <- effectcheck::check_text(txt)
  chi <- res[!is.na(res$test_type) & res$test_type == "chisq", ]
  expect_true(nrow(chi) >= 1)
  # The chi-square carries a reported V, so it must NOT be classified McNemar.
  expect_false(any(chi$chisq_subtype == "mcnemar", na.rm = TRUE))
  # And the reported V should still be present on the row (not dropped).
  expect_true(any(chi$effect_reported_name == "V", na.rm = TRUE))
})

test_that("a non-trivial gof chi-square with V verifies its V instead of 'not recoverable'", {
  # chi2 = 65.03, N = 133  ->  V = sqrt(65.03/133) = 0.699 ~ reported .70
  txt <- paste0(
    "More participants chose action in the short term, ",
    "chi2 (1, N = 133) = 65.03, p < .001, V = 0.70, 95% CI [0.56, 0.80]. ",
    "We also conducted a McNemar test, OR = 0.18, 95% CI [0.10, 0.29], p < .001."
  )
  res <- effectcheck::check_text(txt)
  chi <- res[!is.na(res$test_type) & res$test_type == "chisq", ]
  expect_true(nrow(chi) >= 1)
  expect_false(any(chi$chisq_subtype == "mcnemar", na.rm = TRUE))
  # The computed V should be populated (the row is verifiable, not a McNemar NOTE).
  expect_true(any(!is.na(chi$matched_value), na.rm = TRUE))
})

test_that("a genuine McNemar chi-square (no Cramer's V) is still labeled mcnemar", {
  # Positive control: McNemar reported AS a chi-square, with no V -> stays mcnemar.
  txt <- "We ran a McNemar test of symmetry, chi2 (1, N = 100) = 8.20, p = .004."
  res <- effectcheck::check_text(txt)
  chi <- res[!is.na(res$test_type) & res$test_type == "chisq", ]
  expect_true(nrow(chi) >= 1)
  expect_true(any(chi$chisq_subtype == "mcnemar", na.rm = TRUE))
})
