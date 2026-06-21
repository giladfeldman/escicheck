# v0.6.2: exact binomial test reported with Cohen's h
#
# Form: "<n> out of <N> ... binomial p [op] X, Cohen's h = Y, [95% CI [...]]"
# Common in decoy-effect / binary-choice replication papers (CRSP, JESP).
#
# Before v0.6.2 these returned 0 rows. v0.6.2 adds pat_binom_h with two
# anchors (binomial p AND Cohen's h within ~80 non-period chars) plus a
# NOTE-only check.R branch that recomputes the binomial p via binom.test()
# when n / N are recoverable from a "<n> out of <N>" clause in the same
# verbatim. p_null defaults to 0.5; a stated null is a v0.6.3 follow-up.

test_that("v0.6.2: full CRSP binomial+h+N verbatim parses and N is recovered", {
  v <- "Regret-Salient: 185 out of 326 participants chose the target (56.7%, 95% CI [51.2%, 62.2%]), exact binomial p = .017, Cohen's h = 0.14, 95% CI [0.03, 0.25]"
  r <- check_text(v)
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type[1], "binomial")
  expect_equal(r$stat_value[1], 0.14)
  expect_equal(r$N[1], 326)
  expect_equal(r$effect_reported[1], 0.14)
})

test_that("v0.6.2: bare 'binomial p + Cohen h' (no n out of N) extracts with N=NA, status NOTE", {
  v <- "exact binomial p = .017, Cohen's h = 0.14"
  r <- check_text(v)
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type[1], "binomial")
  expect_true(is.na(r$N[1]))
  expect_equal(r$status[1], "NOTE")
})

test_that("v0.6.2: 80-char-lookahead rejects far-apart binomial-p and Cohen-h", {
  v <- paste0(
    "many many words go here ranging well over eighty non-period characters ",
    "before the cohens h equals statement Cohen's h = 0.14 ... but the ",
    "binomial p = .020 is at the start"
  )
  r <- check_text(v)
  # The binomial-p and the (wrong-order, >80-char-distant) Cohen's h must not be
  # bound together. v0.6.5 added a bare-binomial path, but it is guarded to fire
  # ONLY when no Cohen's h co-occurs in the chunk -- so this ambiguous case still
  # extracts nothing (a stray h must never be associated to a far-away binomial).
  expect_equal(nrow(r), 0L)
})

test_that("v0.6.2: 'h' without 'binomial p' anchor does NOT trigger binomial branch", {
  v <- "the height is 5.2, h = 0.50"
  r <- check_text(v)
  expect_equal(nrow(r), 0L)
})

test_that("v0.6.2: chi-square verbatim with Cohen's h still routes to chisq (not binomial)", {
  v <- "chi2(1, N=200) = 5.3, p = .021, Cohen's h = 0.16"
  r <- check_text(v)
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type[1], "chisq")
})

test_that("v0.6.2: lowercase 'cohen h' (no apostrophe) also matches the binomial branch", {
  v <- "binomial p < .001, Cohen h = 0.30"
  r <- check_text(v)
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type[1], "binomial")
  expect_equal(r$stat_value[1], 0.30)
})

test_that("v0.6.2: when n / N are recoverable, the uncertainty message names the recomputed binomial p", {
  v <- "185 out of 326 participants chose the target. exact binomial p = .017, Cohen's h = 0.14"
  r <- check_text(v)
  expect_equal(nrow(r), 1L)
  expect_match(as.character(r$uncertainty_reasons[1]), "Exact binomial test verified")
  expect_match(as.character(r$uncertainty_reasons[1]), "p_null=0.50")
})
