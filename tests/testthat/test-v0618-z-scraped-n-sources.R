# v0.6.18 -- the z-branch scraped-N disclosure must cover EVERY scraped source,
# not just `global_text`.
#
# Source: cross-model review (Codex) of the final v0.6.18 pre-push diff,
# 2026-08-05. Reproduced at HEAD before fixing.
#
# WHAT WENT WRONG
#
# v0.6.17 added a disclosure to the z branch because a z-test has no df, so
# none of the df-keyed N-plausibility guards that protect t-rows can fire --
# whatever N is bound is used to compute d = 2z/sqrt(N), dz = z/sqrt(N) and
# r = z/sqrt(z^2 + N) with nothing to contradict it. But that disclosure was
# written as `N_source == "global_text"`, the one provenance its reproduction
# exercised. `local_context` / `extended_context` / `subgroup_sum` are the same
# kind of evidence -- a number the statistic's own clause never claimed -- and
# were left silent.
#
# This is the SAME defect class this release fixed on the t-branch by
# introducing `.SCRAPED_N_SOURCES`; the z branch simply was not re-keyed on it.
#
# Reproduced verbatim from the reviewer's counterexample:
#
#   "The calibration sample (N = 100) was used first. In the target subsample
#    (n = 25), z = 2.00, p = .046, r = .20."
#
# binds N = 100 (`N_source = "local_context"`), publishes `status = "OK"` with an
# entirely EMPTY `uncertainty_reasons`, and computes r = 2/sqrt(4 + 100) = 0.196
# against the reported .20 -- an apparent match. The clause's own n = 25 gives
# r = 2/sqrt(4 + 25) = 0.371. A wrong N and a wrong verified effect size, both
# silent.
#
# THE RULE: every effect size on a z row scales with N, and a z row has no df to
# check N against. So ANY scraped N must be announced -- the source class, not
# one member of it.

test_that("a local_context N on a z row is disclosed", {
  txt <- paste0(
    "The calibration sample (N = 100) was used first. In the target subsample ",
    "(n = 25), z = 2.00, p = .046, r = .20."
  )
  res <- effectcheck::check_text(txt)
  z <- res[!is.na(res$test_type) & res$test_type == "z", ]
  expect_gt(nrow(z), 0)
  row <- z[1, ]
  # Precondition: this is the shape under test.
  expect_true(as.character(row$N_source[1]) %in%
                c("local_context", "extended_context", "global_text"))
  # The row must NOT publish effect sizes from a scraped N in silence.
  expect_match(as.character(row$uncertainty_reasons[1]),
               "sample size|Sample size", ignore.case = TRUE)
})

test_that("the existing global_text disclosure still fires (v0.6.17 intact)", {
  filler <- paste(rep("Filler describing the procedure at length. ", 40),
                  collapse = "")
  txt <- paste0(
    "A total of N = 76 participants took part. ", filler,
    "The indirect effect was significant, z = 2.41, p = .016."
  )
  res <- effectcheck::check_text(txt)
  z <- res[!is.na(res$test_type) & res$test_type == "z", ]
  expect_gt(nrow(z), 0)
  expect_match(as.character(z$uncertainty_reasons[1]),
               "sample size|Sample size", ignore.case = TRUE)
})

test_that("a z row whose own clause states its N is NOT given the scraped warning", {
  # `own_clause_denominator` is an observation about THIS test, not a scrape --
  # it must not be lumped in with the scraped sources.
  txt <- "In the reversal analysis, 113/133 participants switched, z = 7.98, p < .001."
  res <- effectcheck::check_text(txt)
  z <- res[!is.na(res$test_type) & res$test_type == "z", ]
  expect_gt(nrow(z), 0)
  src <- as.character(z$N_source[1])
  if (!is.na(src) && grepl("own_clause", src)) {
    expect_false(grepl("found distant from statistic",
                       as.character(z$uncertainty_reasons[1])))
  }
})
