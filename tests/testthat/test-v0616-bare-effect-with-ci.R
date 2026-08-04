# v0.6.16 (E10 + E11) -- two PARSE-MISS classes: a result reported as a bare
# effect + CI (+ p) with NO test statistic of its own.
#
# Both found by the 2026-08-04 Sonnet canary audit of cog_emo
# (10.1080/02699931.2024.2434156) and both reproduced before fixing. Critically,
# the audit verified the sentences WERE delivered by the extractor (they appear
# verbatim in the render's own context_window), so these are effectcheck parse
# defects, not docpluck gaps -- the triage boundary was checked, not assumed.
#
# E10: a BOOTSTRAPPED mediation effect (ACME / ADE) reports a CI, never a Sobel
#      Z, so the v0.6.10 Sobel-anchored pattern never fired.
# E11: a Scheffe / Games-Howell post-hoc contrast reports "d = X, 95% CI [L, U]"
#      with no statistic; six such contrasts produced zero rows.
#
# Neither is independently verifiable (no statistic, df, or N to recompute
# from), so both route to NOTE with the reason stated. The point of the fix is
# that the reported values are SURFACED rather than silently dropped -- a
# checker that drops a result reports "nothing to see here" about a finding it
# never examined.

test_that("v0.6.16 E10: a bootstrapped mediation effect with a CI is extracted", {
  txt <- paste0(
    "The average direct effect was 0.15, 95% CI [-0.13 to 0.45], p = .3, ",
    "whereas the bootstrapped unstandardised indirect effect ",
    "(Average Causal Mediation Effect, ACME) was 0.67, 95% CI [0.47-0.89], p < .001."
  )
  res <- check_text(txt)
  expect_gte(nrow(res), 1L)
  row <- res[!is.na(res$test_type) & res$test_type == "mediation_indirect", ]
  expect_gte(nrow(row), 1L)
  expect_equal(row$effect_reported[1], 0.15)
  # The CI must bind -- it is the only verification handle a bootstrapped
  # mediation effect has, and the "to"/"-" separators are not in the generic
  # pat_CI* set.
  expect_equal(row$ciL_reported[1], -0.13)
  expect_equal(row$ciU_reported[1], 0.45)
  expect_equal(row$p_reported[1], 0.3)
  # Not independently recomputable -> NOTE, never a verification claim.
  expect_equal(row$status[1], "NOTE")
})

test_that("v0.6.16 E11: a bare Cohen's d post-hoc contrast with a CI is extracted", {
  txt <- paste0(
    "Scheffe post-hoc contrasts showed high empathy differed from control, ",
    "Md = 3.80, 95% CI [2.46, 5.15], p < .001; d = 0.60, 95% CI [0.43, 0.77]."
  )
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "d_reported_only", ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$effect_reported[1], 0.60)
  expect_equal(row$ciL_reported[1], 0.43)
  expect_equal(row$ciU_reported[1], 0.77)
  expect_equal(row$status[1], "NOTE")
  # The honesty requirement: the row must SAY it was not verified.
  expect_true(grepl("not independently verified", row$uncertainty_reasons[1], fixed = TRUE))
})

test_that("v0.6.16 E11: the bare-d branch never pre-empts a real test statistic", {
  # A normally-reported t-test whose d also carries a CI must still be a t row
  # (the bare-d branch is last-resort, gated on is.na(test_type)). If this
  # regressed, every d-with-CI in the corpus would collapse to an unverifiable
  # NOTE -- a catastrophic loss of verification coverage.
  txt <- "The groups differed, t(198) = 2.50, p = .013, d = 0.35, 95% CI [0.07, 0.63]."
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type[1], "t")
  expect_false(res$test_type[1] == "d_reported_only")
  # And it must still be genuinely checked, not downgraded.
  expect_false(is.na(res$matched_variant[1]))
})

test_that("v0.6.16 E10: a Sobel-Z mediation report still uses the v0.6.10 path", {
  # Guard: the CI-anchored branch must not shadow the original Sobel branch,
  # which binds the Sobel Z as the test statistic.
  txt <- paste0(
    "The indirect effect was .05, 95% CI [-.04, .12], Sobel Z = 1.42, p = .16."
  )
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "mediation_indirect", ]
  expect_gte(nrow(row), 1L)
  # The Sobel path binds the Z as stat_value; the CI-only path leaves it NA.
  expect_equal(row$stat_value[1], 1.42)
})

test_that("v0.6.16 E11: a two-effect sentence pairs each d with its OWN CI", {
  # Caught by the whole-corpus render diff after the E11 fix landed
  # (collabra.57785 discussion): the generic effect-size scan takes the FIRST
  # "d =" in the chunk, so the row emitted d = 0.39 carrying [0.47, 0.62] --
  # which is the OTHER d's (0.55) interval. Pairing one finding's effect with
  # another's CI is a fabricated result, worse than dropping the row.
  txt <- paste0(
    "The effect size of between-subjects design on willingness to exchange memories, ",
    "d = 0.39, was smaller and below the range of confidence intervals of that of ",
    "within-subjects design, d = 0.55, 95% CI [0.47, 0.62]."
  )
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "d_reported_only", ]
  expect_equal(nrow(row), 1L)
  # The d that OWNS the adjacent CI must be the one reported.
  expect_equal(row$effect_reported[1], 0.55)
  expect_equal(row$ciL_reported[1], 0.47)
  expect_equal(row$ciU_reported[1], 0.62)
  # The cross-paired combination must never appear.
  expect_false(isTRUE(row$effect_reported[1] == 0.39 &&
                      row$ciL_reported[1] == 0.47))
})
