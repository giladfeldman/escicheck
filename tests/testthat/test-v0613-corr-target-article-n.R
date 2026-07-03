# v0.6.13 (E-corr-target-article-N): a correlation explicitly attributed to the
# TARGET / ORIGINAL article -- a value a replication reproduces from the paper it
# replicates (a "Table 2. Target article" intercorrelation block, or prose "the
# weakest effect in the target article ... r = 0.36") -- must carry the TARGET
# article's own sample size, not the current study's (global) N. For a bare `r`
# with no co-located N, the current study's global N is bound by default, which is
# wrong for a target-article statistic. When BOTH (a) the row's context names "the
# target/original article/study" AND (b) the context states that article's sample
# size ("target article's sample size of 239"), N is rebound to that value.
#
# Surfaced by the 2026-07-02 escicheck-iterate cycle-3 canary re-audit of cog_emo
# (Chan & Feldman replication of McCullough et al. 1997): loc 124 r = 0.36 quoted
# from "the target article", whose Method states "we followed the target article's
# sample size of 239 participants" -- N had been bound to the study's global 794.

test_that("a target-article r with the target-article N in context rebinds N", {
  filler <- paste(rep("Unrelated methods prose about the current study procedures.",
                      10), collapse = " ")
  txt <- paste0(
    "Our current study recruited a total of N = 794 participants. ", filler,
    " Thus, we followed the target article's sample size of 239 participants. ",
    "This is weaker than the lower bound of the weakest effect in the target article ",
    "(apology vs. empathy: r = 0.36, 95% CI [0.24, 0.47])."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_equal(nrow(rr), 1L)
  # N rebound to the target article's 239 (df = N - 2 = 237), not the study's 794.
  expect_equal(rr$N[1], 239)
  expect_equal(rr$df1[1], 237)
  expect_true(grepl("target", rr$uncertainty_reasons[1], ignore.case = TRUE))
})

test_that("a current-study r is NOT given a target-article N", {
  # No target-article attribution: the r keeps the current study's own N.
  txt <- paste0(
    "In our study of N = 794 participants, empathy correlated with forgiveness, ",
    "r(792) = 0.36, 95% CI [0.24, 0.47]."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_equal(nrow(rr), 1L)
  expect_true(rr$N[1] > 700)   # ~794, not 239
})

test_that("a target-article mention WITHOUT a stated target-article N does not rebind", {
  # Only one of the two required cues -> no override (avoids guessing an N).
  filler <- paste(rep("Unrelated methods prose about the current study procedures.",
                      10), collapse = " ")
  txt <- paste0(
    "Our current study recruited a total of N = 794 participants. ", filler,
    " The pattern resembled the weakest effect in the target article ",
    "(apology vs. empathy: r = 0.36, 95% CI [0.24, 0.47])."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_equal(nrow(rr), 1L)
  # No "sample size of N" statement -> N is NOT forced to some target value.
  expect_false(identical(as.numeric(rr$N[1]), 239))
})
