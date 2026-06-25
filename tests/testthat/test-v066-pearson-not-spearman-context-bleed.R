# v0.6.6: a bare r(df) defaults to Pearson and is reclassified to Spearman ONLY
# on a cue in the IMMEDIATE clause (the sub-chunk containing the r) -- never on a
# "spearman" keyword that merely appears somewhere in the wider context window.
#
# Surfaced by the 2026-06-25 escicheck-iterate canary audit (cycle 1) against
# cog_emo (Chan & Feldman 2024, Cognition & Emotion). The body-text Pearson
# correlation "r(261) = -0.43, 95% CI [-0.52, -0.33], p < .001" was mislabeled
# test_type = "spearman" because a DISTANT table note read "Format: Pearson's
# correlations [confidence interval] (Spearman's rho)" -- the note describes a
# parenthetical alternative column, not this r. The reclassification signal had
# been computed over `paste(s, context)` (the wide window); it now reads only `s`
# (the immediate sub-chunk), matching the documented "cue near the statistic"
# intent of the Stage-1/Gap-4 design.

test_that("a body Pearson r stays r when 'Spearman' is only in a distant table note", {
  txt <- paste0(
    "We found support for forgiveness as negatively associated with revenge ",
    "motivation, r(261) = -0.43, 95% CI [-0.52, -0.33], p < .001. ",
    "We provided the summary scatterplots in Figures 4-6. ",
    "Table 2 reports the intercorrelations. ",
    "Note: Correlations in the control condition for the replication (n = 263). ",
    "Format: Pearson's correlations [confidence interval] (Spearman's rho)."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type %in% c("r", "spearman") &
              !is.na(res$stat_value) & abs(res$stat_value - (-0.43)) < 1e-6, ]
  expect_true(nrow(rr) >= 1)
  expect_true(any(rr$test_type == "r", na.rm = TRUE))
  expect_false(any(rr$test_type == "spearman", na.rm = TRUE))
})

test_that("an explicit near-statistic Spearman cue still reclassifies to spearman", {
  # The Stage-1/P2 near-statistic reclassification must be untouched.
  res <- effectcheck::check_text("A Spearman correlation was computed, r(20) = 0.50, p = .018.")
  row <- res[!is.na(res$test_type), ]
  expect_true(any(row$test_type == "spearman", na.rm = TRUE))
})

test_that("a Spearman cue in the SAME clause as the r reclassifies even amid other sentences", {
  txt <- paste0(
    "Means and SDs are reported in Table 1. ",
    "Using Spearman's rank correlation, r(48) = 0.31, p = .029. ",
    "We then ran a regression."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & !is.na(res$stat_value) &
              abs(res$stat_value - 0.31) < 1e-6, ]
  expect_true(nrow(rr) >= 1)
  expect_true(any(rr$test_type == "spearman", na.rm = TRUE))
})
