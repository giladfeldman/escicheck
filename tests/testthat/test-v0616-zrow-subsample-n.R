# v0.6.16 (E7 / E-zrow-subsample-n) -- a clause that states its OWN denominator
# as "<k>/<N>" must bind that N, not a parent total from the surrounding window.
#
# Found by the 2026-08-04 Sonnet canary audit of collabra.37122 (6 z rows) and
# reproduced before fixing. The reversal-subsample rows read
# "(113/133, the total number of participants who showed reversal ..., 84.96%),
# versus ... (20/133, 15.04%), ... z = 7.98" yet bound N = 493 -- the PARENT
# study total -- from the surrounding context window.
#
# This is NOT cosmetic despite the row's SKIP status: the emitted `all_variants`
# values are surfaced to the reader regardless of status, so the wrong N
# published r_from_z = 0.3382 where N = 133 gives ~0.57, and d_ind = 0.7188
# where the correct value is 1.3839 -- nearly double.
#
# Same "prefer the signal inside the row's own clause" discriminator as the
# v0.6.8 t-test fix, which was gated to t-tests and so never covered z rows.
#
# Authored against the UNFIXED code and watched fail (N came back 493) before
# the fix landed.

test_that("v0.6.16 E7: a z row binds its own slash denominator, not the parent N", {
  txt <- paste0(
    "Study 3 included 493 participants who completed both time points. ",
    "Comparing the proportion of participants choosing action Jim in the short term and ",
    "choosing inaction Dave in the long term (113/133, the total number of participants who ",
    "showed reversal in answers, 84.96%), versus the proportion of participants choosing ",
    "inaction Dave in the short term and choosing action Jim in the long term (20/133, 15.04%), ",
    "we found support for a deviation from 50-50, z = 7.98, chi2 (1, N = 133) = 65.03, p < .001."
  )
  res <- check_text(txt)
  z <- res[!is.na(res$test_type) & res$test_type == "z", ]
  expect_equal(nrow(z), 1L)
  expect_equal(z$stat_value[1], 7.98)
  # The subsample denominator, NOT the parent study total.
  expect_equal(z$N[1], 133)
  expect_false(z$N[1] == 493)
  # Provenance must name the own-clause source, never a silent rebind.
  expect_equal(z$N_source[1], "own_clause_denominator")
})

test_that("v0.6.16 E7: a clause with DISAGREEING slash denominators stays ambiguous", {
  # "113/133 ... 20/140" names two different samples -- genuinely ambiguous, so
  # the own-clause denominator must NOT be bound (the guard requires agreement).
  txt <- paste0(
    "Across the 493 participants, comparing (113/133, 84.96%) versus (20/140, 14.3%), ",
    "we found a deviation from 50-50, z = 7.98, p < .001."
  )
  res <- check_text(txt)
  z <- res[!is.na(res$test_type) & res$test_type == "z", ]
  expect_equal(nrow(z), 1L)
  expect_false(isTRUE(z$N_source[1] == "own_clause_denominator"))
})

test_that("v0.6.16 E7: the r-test multi-N candidate path is NOT short-circuited", {
  # Guard against the regression this fix caused when first written without an
  # r-test exclusion: a correlation with no explicit df runs a
  # best-N-by-p-value-fit selection over ALL candidates and must keep emitting
  # its "Multiple sample sizes" note. Binding the first own-clause N silently
  # dropped that note (test-metaesci-v023.R:530 and
  # test-v0612-ownclause-n-and-repcol-dedup.R:188 both went red).
  res <- check_text("N = 32. N = 959. r = .25, p < .001")
  row <- res[!is.na(res$stat_value), ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$N[1], 959)
  expect_true(grepl("Multiple sample sizes", row$assumptions_used[1]))
})
