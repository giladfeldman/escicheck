# v0.6.22 -- checks that ARE valid on a resampling p-value without raw data.
#
# v0.6.21 established that a resampling p cannot be recomputed and stopped
# grading it against the parametric reference. That left the honest question
# the reviewer actually asked: what CAN be checked? Three things, none of which
# need the data:
#
# 1. MINIMUM ATTAINABLE p. A Monte Carlo permutation p sits on the lattice
#    (r+1)/(B+1) (Phipson & Smyth 2010), so with B stated, p < 1/(B+1) is below
#    the floor. "1,000 permutations, p < .0001" cannot be produced by counting.
#
# 2. EXACT-PERMUTATION FLOOR. With n1 and n2 known the reference set has
#    C(n1+n2, n1) members, so no exact two-sided p below 1/M is reachable.
#    Enumeration (2026-08-07) shows the true floor is 2/M for every n1,n2 tried
#    -- including unequal n -- and that ties raise it further, so shipping 1/M
#    is deliberately conservative: it flags only the genuinely unreachable.
#
# 3. MONTE CARLO FRAGILITY. SE(p_hat) = sqrt(p(1-p)/B), so a p adjacent to
#    alpha with small B has an unstable significance decision.
#
# NONE of these may be a hard ERROR. Legitimate methods go below the floor:
# GPD tail approximation, sequential Monte Carlo, per-stratum p-combination,
# mid-p (0.5/M) and randomized p (no positive floor at all). They are NOTEs.

test_that("v0.6.22: B (number of resamples) is parsed from the clause", {
  for (phrase in c("with 10,000 permutations",
                   "using 10000 permutations",
                   "based on 5,000 bootstrap resamples",
                   "with 2,000 replicates",
                   "using 1,000 Monte Carlo samples")) {
    res <- check_text(paste0("A permutation test ", phrase, ", t(58) = 2.31, p = .062."))
    row <- res[!is.na(res$test_type) & res$test_type == "t", ]
    expect_gte(nrow(row), 1L)
    expect_false(is.na(row$resampling_B[1]), info = phrase)
  }
})

test_that("v0.6.22: a p below the Monte Carlo floor 1/(B+1) is flagged", {
  # 1,000 permutations -> smallest attainable p is 1/1001 = .000999.
  txt <- "A permutation test with 1,000 permutations, t(58) = 2.31, p < .0001."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_true(isTRUE(row$resampling_p_below_floor[1]))
  expect_match(row$uncertainty_reasons[1], "1,001|minimum attainable|below the",
               ignore.case = TRUE)
  # Never a hard ERROR -- legitimate methods reach below the counting floor.
  expect_false(identical(row$status[1], "ERROR"))
})

test_that("v0.6.22: a p ON or ABOVE the floor is not flagged", {
  # 1/(B+1) = 1/10001 = 9.999e-05, so p = .001 is comfortably attainable.
  txt <- "A permutation test with 10,000 permutations, t(58) = 2.31, p = .001."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_false(isTRUE(row$resampling_p_below_floor[1]))
})

test_that("v0.6.22: the exact-permutation floor uses C(n1+n2, n1)", {
  # n1 = n2 = 5 -> M = 252. Enumerated floor is 2/252 = .00794; we ship the
  # conservative 1/252 = .003968, so a reported .001 is still unreachable.
  expect_equal(effectcheck:::perm_min_p_exact(5, 5), 1 / 252, tolerance = 1e-12)
  expect_equal(effectcheck:::perm_min_p_exact(4, 6), 1 / 210, tolerance = 1e-12)
  expect_true(0.001 < effectcheck:::perm_min_p_exact(5, 5))
  # Large samples: the floor becomes vanishingly small and must not false-fire.
  expect_lt(effectcheck:::perm_min_p_exact(50, 50), 1e-25)
  # Guard rails.
  expect_true(is.na(effectcheck:::perm_min_p_exact(NA, 5)))
  expect_true(is.na(effectcheck:::perm_min_p_exact(1, 0)))
})

test_that("v0.6.22: Monte Carlo floor helper matches 1/(B+1)", {
  expect_equal(effectcheck:::perm_min_p_mc(1000), 1 / 1001, tolerance = 1e-12)
  expect_equal(effectcheck:::perm_min_p_mc(9999), 1 / 10000, tolerance = 1e-12)
  expect_true(is.na(effectcheck:::perm_min_p_mc(NA)))
  expect_true(is.na(effectcheck:::perm_min_p_mc(0)))
})

test_that("v0.6.22: a resampling result that never reports B is noted", {
  # Without B the p-value is not reproducible even WITH the raw data.
  res <- check_text("A permutation test showed t(58) = 2.31, p = .062.")
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_true(is.na(row$resampling_B[1]))
  expect_match(row$uncertainty_reasons[1],
               "number of (?:permutations|resamples)|not reported",
               ignore.case = TRUE)
})

test_that("v0.6.22: Monte Carlo fragility is surfaced when p is near alpha", {
  # p = .048 with B = 1,000 has SE ~ .0068; the .05 decision is unstable.
  txt <- "A permutation test with 1,000 permutations, t(58) = 2.31, p = .048."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_match(row$uncertainty_reasons[1], "Monte Carlo|resampling uncertainty",
               ignore.case = TRUE)
  # A p far from alpha with a large B must NOT get the note.
  far <- check_text("A permutation test with 100,000 permutations, t(58) = 2.31, p = .6.")
  frow <- far[!is.na(far$test_type) & far$test_type == "t", ]
  expect_false(grepl("Monte Carlo uncertainty", frow$uncertainty_reasons[1], fixed = TRUE))
})

test_that("v0.6.22: none of this fires on a non-resampling result", {
  res <- check_text("An independent t-test with 1,000 participants, t(58) = 2.31, p = .0001.")
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_false(isTRUE(row$resampling_inference[1]))
  expect_true(is.na(row$resampling_B[1]))
  expect_false(isTRUE(row$resampling_p_below_floor[1]))
})

# ---------------------------------------------------------------------------
# Cross-model review round 2 (2026-08-07), against v0.6.22/v0.7.0/v0.7.1.
# Eight further defects raised, ALL EIGHT reproduced locally before being
# acted on. Four of them wrote a WRONG NUMBER rather than merely a wrong flag.
# ---------------------------------------------------------------------------

test_that("v0.7.1 R9: B is not scraped from a neighbouring count", {
  # Reproduced: "Across 500 samples, a permutation test with 10,000
  # permutations ..." bound B = 500 and then FALSE-FLAGGED the p as below
  # "1/(B+1) = 0.002" -- a wrong accusation from a count in the wrong clause.
  # A bare "<n> samples/draws/iterations" no longer counts without a
  # resampling qualifier.
  txt <- paste0("Across 500 samples, a permutation test with 10,000 ",
                "permutations yielded t(58) = 2.31, p = .0005.")
  row <- check_text(txt)[1, ]
  expect_equal(as.numeric(row$resampling_B[1]), 10000)
  expect_false(isTRUE(row$resampling_p_below_floor[1]))
})

test_that("v0.7.1 R10: a bare 'B = 10,000' is recognised", {
  row <- check_text("A permutation test (B = 10,000) yielded t(58) = 2.31, p = .0005.")[1, ]
  expect_equal(as.numeric(row$resampling_B[1]), 10000)
})

test_that("v0.7.1 R11: the EXACT floor is a permutation bound, not a bootstrap one", {
  # A bootstrap resamples WITH replacement, so it has no choose(n1+n2, n1)
  # reference set. Reproduced: a legitimate bootstrap p was flagged against
  # 1/choose(10,5) = .00397, a bound that never constrained it.
  txt <- "Using 10,000 bootstrap resamples with n1 = 5 and n2 = 5, t(8) = 2.31, p = .001."
  row <- check_text(txt)[1, ]
  expect_true(isTRUE(row$resampling_inference[1]))
  expect_false(isTRUE(row$resampling_p_below_floor[1]))

  # ...but a PERMUTATION at the same n is still bounded.
  perm <- check_text(
    "Using an exact permutation test with n1 = 5 and n2 = 5, t(8) = 2.31, p = .001.")[1, ]
  expect_true(isTRUE(perm$resampling_p_below_floor[1]))
})

test_that("v0.7.1 R12: a strict inequality AT the floor is unreachable", {
  # 1/(999+1) = .001 exactly. "p < .001" asserts strictly below that, which
  # counting cannot produce -- but `.001 < .001` is FALSE, so it was missed.
  row <- check_text("Using 999 permutations, t(58) = 2.31, p < .001.")[1, ]
  expect_true(isTRUE(row$resampling_p_below_floor[1]))
})

test_that("v0.7.1 R13: no Monte Carlo SE is printed for an inequality p", {
  # Reproduced: "p < .05" produced "SE = 0.00218 (approx. 95% interval 0.0457
  # to 0.0543)" -- concrete numbers attached to a value the paper never gave.
  row <- check_text("Using 10,000 permutations, t(58) = 2.31, p < .05.")[1, ]
  expect_false(grepl("Monte Carlo uncertainty", row$uncertainty_reasons[1], fixed = TRUE))

  # The note still fires for a genuine point estimate near alpha.
  pt_row <- check_text("Using 1,000 permutations, t(58) = 2.31, p = .048.")[1, ]
  expect_match(pt_row$uncertainty_reasons[1], "Monte Carlo uncertainty")
})

test_that("v0.7.1 R14: Yuen and Brunner-Munzel written with a plain t(df)", {
  # The COMMONEST form. The generic t branch claimed it first, so these were
  # typed `t` and had ordinary Cohen's d variants computed -- effect sizes
  # those statistics do not imply.
  y <- check_text("Yuen's trimmed-mean test, t(18.5) = 2.31, p = .033.")[1, ]
  expect_equal(y$test_type[1], "yuen")
  expect_equal(y$df1[1], 18.5)

  b <- check_text("A Brunner-Munzel test, t(18.5) = 2.31, p = .033.")[1, ]
  expect_equal(b$test_type[1], "brunner_munzel")

  # An ordinary t-test is untouched.
  t_row <- check_text("An independent t-test, t(18.5) = 2.31, p = .033, d = 0.61.")[1, ]
  expect_equal(t_row$test_type[1], "t")
})

test_that("v0.7.1 R15: two robust statistics in one sentence split correctly", {
  # Reproduced: the sentence stayed one chunk, the ATS row carried the WTS's
  # p = .002, and the WTS row was dropped entirely.
  res <- check_text("WTS(2) = 12.34, p = .002; ATS(1.87, Inf) = 3.45, p = .061.")
  expect_equal(nrow(res), 2L)
  w <- res[res$test_type == "wts", ]
  a <- res[res$test_type == "ats", ]
  expect_equal(nrow(w), 1L)
  expect_equal(nrow(a), 1L)
  expect_equal(w$p_reported[1], 0.002)
  expect_equal(a$p_reported[1], 0.061)   # its OWN p, not the WTS's
})

test_that("v0.7.1 R16: a Brunner-Munzel mention does not claim another test's W", {
  # Reproduced: this bound the Wilcoxon's W = 123 as a Brunner-Munzel
  # statistic -- wrong type AND wrong value. A competing test name inside the
  # gap now refuses the match rather than guessing.
  txt <- paste0("The Brunner-Munzel alternative was considered, but the reported ",
                "Wilcoxon result was W = 123, p = .04, N = 30.")
  row <- check_text(txt)[1, ]
  expect_false(identical(row$test_type[1], "brunner_munzel"))
  expect_equal(row$p_reported[1], 0.04)
})
