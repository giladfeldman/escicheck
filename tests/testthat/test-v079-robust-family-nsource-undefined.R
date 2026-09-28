# v0.7.9 -- the whole v0.7.0 robust / modern-nonparametric family (WTS, ATS,
# Brunner-Munzel, Yuen) CRASHED on any document that states a sample size.
#
# Source: 2026-09-02 escicheck-iterate, found while tracing where the r branch's
# `N_source` assignments go.
#
# WHAT WENT WRONG
#
# `compute_and_compare_one()` never initialises a local `N_source`. It assigns
# one in ten places, and the output tibble reads the provenance from
# `row$N_source` instead, so the local is write-only -- except at one site. The
# robust-family branch guards its N-clearing rule with
#
#     if (!is.na(N) && !is.na(N_source) && N_source %in% .SCRAPED_N_SOURCES)
#
# and that is a READ of a variable that does not exist. R evaluates it only when
# the first conjunct is TRUE, i.e. only when the row actually has an N. So:
#
#     no N in the document  -> `!is.na(N)` is FALSE, short-circuit, no crash
#     an N in the document  -> "object 'N_source' not found"
#
# The error is caught by the per-row `tryCatch` in `check_text()`, so it does not
# abort the run. It publishes `status = "ERROR"`, `check_type = NA`, and a
# warning -- one row silently converted from a real verdict into an error, in a
# document that otherwise looks healthy.
#
# WHY NOTHING CAUGHT IT. Measured 2026-09-02:
#   - The nine tests in test-v070-robust-nonparametric-types.R never place an
#     `N = ...` in a WTS/ATS/Brunner-Munzel/Yuen document. The only `N =` in that
#     file is on a Wilcoxon row, which takes a different branch. The suite was
#     green (3779 passing) with every one of these four types broken.
#   - The 30-document validation corpus renders 512 rows with ZERO ERROR rows --
#     no corpus paper reports a WRS2 / nparLD statistic alongside a sample size.
#     The corpus diff, the release gate's usual net, could not see this either.
#
# Both nets missed it for the same reason: the crash needs a condition
# (a stated N) that every fixture happened to omit -- and a paper reporting a
# robust test without ever stating its sample size is the unusual case, not the
# usual one.

robust_doc <- function(clause, with_n = TRUE) {
  paste0(
    if (with_n) "Participants. We recruited N = 120 participants in total. "
    else "Participants were recruited from a university subject pool. ",
    paste(rep("Filler about the procedure and the measures used here. ", 20),
          collapse = ""),
    clause
  )
}

robust_clauses <- list(
  brunner_munzel = "A Brunner-Munzel test showed a difference, W(112.4) = 2.41, p = .017.",
  wts            = "The Wald-type statistic was significant, WTS(2) = 9.41, p = .009.",
  ats            = "The ANOVA-type statistic was significant, ATS(1.87, 45.30) = 3.45, p = .041.",
  yuen           = "Yuen's trimmed-mean test was significant, Ty(38.2) = 2.66, p = .011."
)

test_that("a robust-family statistic in a document that states an N does not error", {
  for (nm in names(robust_clauses)) {
    res <- effectcheck::check_text(robust_doc(robust_clauses[[nm]], with_n = TRUE))
    expect_equal(nrow(res), 1L, info = nm)
    expect_false(identical(as.character(res$status[1]), "ERROR"),
      info = paste0(nm, ": a stated sample size made the row crash and publish ",
                    "status = ERROR with no check_type"))
    expect_equal(as.character(res$check_type[1]), "p_value", info = nm)
  }
})

test_that("the robust family still refuses a sample size it only found in distant context", {
  # The guard that crashed exists for a reason: a WTS / ATS / Brunner-Munzel /
  # Yuen statistic has no recoverable N, so an N scraped from elsewhere in the
  # document is not this test's N and must not be published beside it. Once the
  # guard can actually run, it must still DO that -- fixing the crash by deleting
  # the rule would be the easy way to make this file green while losing the
  # protection.
  for (nm in names(robust_clauses)) {
    res <- effectcheck::check_text(robust_doc(robust_clauses[[nm]], with_n = TRUE))
    expect_true(is.na(res$N[1]),
      info = paste0(nm, ": N = 120 came from the Participants section, not from ",
                    "this statistic's own clause, and must be dropped"))
    expect_true(is.na(res$N_source[1]),
      info = paste0(nm, ": an N that was dropped must not leave its provenance ",
                    "behind advertising global_text"))
  }
})

test_that("CONTROL: a robust-family statistic with no N in the document is unchanged", {
  # This is the shape every pre-existing fixture happened to use, and the shape
  # that stayed green throughout. It must keep behaving exactly as it did.
  for (nm in names(robust_clauses)) {
    res <- effectcheck::check_text(robust_doc(robust_clauses[[nm]], with_n = FALSE))
    expect_equal(nrow(res), 1L, info = nm)
    expect_equal(as.character(res$status[1]), "OK", info = nm)
    expect_equal(as.character(res$check_type[1]), "p_value", info = nm)
    expect_true(is.na(res$N[1]), info = nm)
    expect_equal(as.character(res$N_source[1]), "not_found", info = nm)
  }
})
