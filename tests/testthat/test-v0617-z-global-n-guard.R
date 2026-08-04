# v0.6.17 -- the document-level `global_N` fallback must not silently resolve to
# the SMALLEST N in the paper, and a z-test row must not publish effect sizes
# from a scraped global N in silence.
#
# Source: 2026-08-04 escicheck-iterate cycle 1, Sonnet canary audit of
# 10.1016/j.jesp.2009.12.010 (Study 2 Sobel mediation z-tests), root-caused and
# reproduced against the real docpluck text before any code change.
#
# WHAT WENT WRONG (two compounding defects)
#
# 1. TIE-BREAK. parse.R's global-N fallback takes the MODE of every
#    "N = <int>" in the document, falling back to max() only when all values
#    are unique:
#
#        n_counts <- table(ns)
#        if (max(n_counts) > 1) as.numeric(names(n_counts)[which.max(n_counts)])
#        else max(ns)
#
#    `table()` orders counts by ascending numeric name and `which.max` returns
#    the FIRST maximum, so whenever the top frequency is SHARED the "mode"
#    silently resolves to the SMALLEST N in the document. In the real paper
#    every candidate tied at frequency 2 (7, 13, 25, 31, 38 -- each appearing
#    twice, all cells of one accepters/rejecters subgroup table), so global_N
#    became 7: the smallest subgroup cell in the paper.
#
# 2. BIASED CANDIDATE POOL. `pat_N` matches only "N = <int>" / "nobs = <int>".
#    This paper states its real sample size in prose -- "A total of 76 Israeli
#    students, 53 males and 23 females ... participated" -- which the pattern
#    never sees, while the subgroup TABLE cells ("N=7") match freely. The
#    candidate pool therefore systematically excludes study totals and includes
#    subgroup cells, which is exactly backwards for a document-level fallback.
#
# The consequence: the Study 2 mediation z rows bound N = 7 and published
# r_from_z = 0.7341 and d = 2.162. With the true N = 76 those are 0.312 and
# 0.328 -- the published values are more than DOUBLE the truth, and
# uncertainty_reasons was entirely EMPTY on both rows.
#
# The t branch escapes this because it has df-keyed N-plausibility guards
# (minimum-N, Welch floor, global-N override). A z-test has no df, so not one
# of them applies and the z branch had no guard at all.
#
# NOTE: values in `all_variants` reach the user regardless of the row's status
# (2026-08-04 handoff: "status = SKIP does not make a wrong number safe"), so
# publishing d = 2.162 is a user-visible defect, not a cosmetic one.

test_that("a frequency tie resolves to the largest TIED value, not the smallest", {
  # 10.1016/j.jesp.2009.12.010's candidate multiset: five values, each twice.
  # The old rule returned 7 (smallest); the largest tied value is 38.
  ns <- c(7, 7, 13, 13, 25, 25, 31, 31, 38, 38)
  expect_equal(effectcheck:::global_n_from_candidates(ns), 38)
})

test_that("a tie never escapes to a once-mentioned outlier", {
  # 10.1525/collabra.32572: a tight cluster of candidates plus one 3302 outlier.
  # 274 and 275 tie at the top; the answer must come from THEM, not from 3302.
  # An intermediate version of this fix returned max(ns) on a tie and handed
  # every F row on that paper N = 3302 against a true 999.
  ns <- c(rep(273, 3), rep(274, 4), rep(275, 4), rep(276, 2), rep(277, 2), 279, 3302)
  expect_equal(effectcheck:::global_n_from_candidates(ns), 275)
})

test_that("a multi-study tie returns the pooled N (documented trade-off)", {
  # Cross-model review (codex, 2026-08-04), reproduced: Study 1 N=40 x2,
  # Study 2 N=60 x2, pooled N=100 x2. The old rule returned 40, this one
  # returns 100. Neither is right for every row -- each scores 1 of 3 -- and
  # the tie-break is chosen on the DIRECTION of its error (a too-small N
  # inflates effects and manufactures false discrepancies; a too-large N
  # attenuates toward agreement and is additionally caught by the df-authority
  # override on t rows). Pinned so the choice is deliberate, not accidental.
  ns <- c(40, 40, 60, 60, 100, 100)
  expect_equal(effectcheck:::global_n_from_candidates(ns), 100)
})

test_that("a genuine mode still wins over a larger one-off value", {
  # 76 is the repeated study total; 300 a one-off (e.g. a cited prior study).
  ns <- c(76, 76, 76, 7, 300)
  expect_equal(effectcheck:::global_n_from_candidates(ns), 76)
})

test_that("all-unique candidates still fall back to the largest", {
  ns <- c(13, 25, 38, 76)
  expect_equal(effectcheck:::global_n_from_candidates(ns), 76)
})

test_that("a z row never publishes effect sizes from a global-text N in silence", {
  # Subgroup cells far from the z clause -> the z row can only reach them via
  # the document-level fallback, which must announce itself.
  txt <- paste0(
    "A total of 76 Israeli students participated in the negotiation study. ",
    paste(rep("Filler sentence describing the procedure in detail. ", 40),
          collapse = ""),
    "Accepters only. Positive Expectations N = 38. Neutral Expectations N = 38. ",
    "Rejecters only. Positive Expectations N = 7. Neutral Expectations N = 7. ",
    paste(rep("Further discussion of the manipulation and its effects. ", 40),
          collapse = ""),
    "However, the strength of this direct link was significantly reduced, ",
    "z = 2.86, p = .004."
  )

  res <- effectcheck::check_text(txt)
  z_rows <- res[!is.na(res$test_type) & res$test_type == "z", ]
  expect_gt(nrow(z_rows), 0)

  row <- z_rows[1, ]
  n_source <- as.character(row$N_source[1])
  reasons <- paste(unlist(row$uncertainty_reasons), collapse = " ")

  # The t branch has warned "Sample size may not apply to this specific
  # comparison" for a global_text N since v0.2.9; the z branch must not stay
  # silent about the same provenance.
  if (!is.na(n_source) && n_source == "global_text") {
    expect_true(
      nzchar(trimws(reasons)),
      info = paste0(
        "z row bound N = ", row$N[1],
        " from global_text and published effect sizes with an EMPTY ",
        "uncertainty_reasons -- the user gets a computed d/r with no signal ",
        "that its N was scraped from elsewhere in the document."
      )
    )
  }
})
