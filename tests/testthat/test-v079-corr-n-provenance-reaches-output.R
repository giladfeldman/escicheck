# v0.7.9 -- two correlation N-rebinding rules relabelled the provenance of the N
# they had just replaced, and the label never reached the output. Every such row
# published a corrected N under the provenance of the number it discarded.
#
# Source: 2026-09-02 escicheck-iterate, found while tracing where the r branch's
# `N_source` assignments go. Answer: nowhere.
#
# WHAT WENT WRONG
#
# The output tibble builds provenance from the PARSED column:
#
#     N_source_out <- if ("N_source" %in% names(row)) row$N_source else NA
#
# with two later overrides (`df_inferred`, `omnibus_df_contrast`). The local
# `N_source` variable is not consulted. So these two assignments were dead:
#
#     check.R  v0.6.12  N_source <- "corr_df_plus_2"      -- r(348) vs a bled N
#     check.R  v0.6.13  N_source <- "target_article_n"    -- a target-article r
#
# Both strings appear exactly once in R/ -- as the assignment. Never read.
#
# The blocks that came before and after them got this right: the md_hl (v0.6.10)
# and cochran_q blocks each mutate `row$N_source` and say in a comment why --
# "the output tibble reads N_source from row$N_source ... clear it too, or N is
# NA while the provenance still misleadingly reads global_text". The r branch
# was written without that step.
#
# MEASURED ON A REAL PAPER, 2026-09-02. Chan & Feldman (2025), doi
# 10.1080/02699931.2024.2434156: eight prose correlations state `r(261)`, the
# v0.6.12 rule correctly rebinds each from the document's N = 794 to df + 2 =
# 263, and all eight publish `N_source = "global_text"` -- naming a document-
# level scrape as the origin of a number that is in fact a df derivation. A
# ninth row is rebound by v0.6.13 to the target article's N = 239 and publishes
# "global_text" too.
#
# WHY THE EXISTING TESTS PASSED: test-v0613-corr-target-article-n.R asserts the
# N (239), the df (237) and the uncertainty text, and never asserts N_source.
# The value was right; only the label lied, and nothing looked at the label.
#
# This matters beyond tidiness. N_source is the field a consumer uses to decide
# how much to trust an N -- downstream consumers read it -- and "global_text" is the LEAST
# trustworthy provenance the vocabulary has. These rows were published under it
# while actually carrying the MOST defensible kind of N available.

test_that("an N rebound from the correlation's own df publishes corr_df_plus_2", {
  # v0.6.12's case: the r prints its own df, and the N bound from context
  # disagrees with it. df wins; the label must say df is where the N came from.
  txt <- paste0(
    "Our current study recruited a total of N = 794 participants. ",
    paste(rep("Unrelated methods prose about the current study procedures. ", 12),
          collapse = ""),
    "Forgiveness correlated with conciliation, r(261) = 0.45, ",
    "95% CI [0.35, 0.54], p < .001."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_equal(nrow(rr), 1L)

  # The value was always right ...
  expect_equal(as.numeric(rr$N[1]), 263)
  expect_equal(as.numeric(rr$df1[1]), 261)
  # ... the label was not.
  expect_equal(as.character(rr$N_source[1]), "corr_df_plus_2",
    info = "N was replaced by df + 2, so publishing 'global_text' names the provenance of the number that was DISCARDED")
})

test_that("an N rebound to the target article's sample size publishes target_article_n", {
  # v0.6.13's case, using the fixture from test-v0613-corr-target-article-n.R.
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

  expect_equal(as.numeric(rr$N[1]), 239)
  expect_equal(as.character(rr$N_source[1]), "target_article_n",
    info = "N came from the TARGET article's stated sample size; 'global_text' points at the host study's N, which this rule exists to reject")
})

test_that("CONTROL: a correlation whose N was never rebound keeps its parsed provenance", {
  # The rebinding rules did not fire here, so nothing about the provenance may
  # change. This is the half that fails if the fix relabels indiscriminately.
  txt <- paste0(
    "In our study of N = 794 participants, empathy correlated with forgiveness, ",
    "r(792) = 0.36, 95% CI [0.24, 0.47]."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_equal(nrow(rr), 1L)
  # df 792 and N 794 agree (N = df + 2), so v0.6.12 does not fire.
  expect_equal(as.numeric(rr$N[1]), 794)
  expect_false(as.character(rr$N_source[1]) %in%
                 c("corr_df_plus_2", "target_article_n"),
    info = "no rebinding happened, so neither rebinding label may appear")
})

test_that("CONTROL: a non-correlation row's provenance is untouched", {
  # The two labels are correlation-only. A t-test with the same shape of
  # document must not acquire either of them.
  txt <- paste0(
    "In our study of N = 794 participants the groups differed, ",
    "t(261) = 2.31, p = .022, d = 0.28."
  )
  res <- effectcheck::check_text(txt)
  tt <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(tt), 0)
  expect_false(any(as.character(tt$N_source) %in%
                     c("corr_df_plus_2", "target_article_n")))
})
