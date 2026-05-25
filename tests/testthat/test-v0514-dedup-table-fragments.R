# Regression test for v0.5.14 — collapse table-fragment duplicates of
# body-text statistics. Replication papers commonly print a summary table
# that lists the same correlations / effects already reported in the body;
# the parser legitimately catches both, but they are the same result.
# Caught by the 2026-05-23 escicheck-iterate cycle-1 validation against
# collabra_57785 (Experiential-vs-Material).

test_that("body-text r-with-df + table-fragment r-without-df dedupes to one row", {
  txt <- paste(
    "Our sample comprised N = 743 participants.",
    "Trait X correlated with Y, r(741) = -0.43, p < .001, 95% CI [-0.49, -0.37].",
    "",
    "## Table 8.",
    "",
    "Study 5 - Pearson r correlation analysis",
    "    Association of X and Y     r = -.43 [-.49, -.37]   Not reported   Signal"
  )
  res <- effectcheck::check_text(txt)
  # Count r-rows with stat_value ~ -0.43
  match_rows <- res[!is.na(res$stat_value) & res$test_type == "r" & abs(res$stat_value - (-0.43)) < 0.005, ]
  expect_equal(nrow(match_rows), 1L)
  # The kept row should be the parenthesized form (body text)
  expect_true(grepl("r\\(741\\)", match_rows$raw_text[1L]))
})

test_that("different df1 values are NOT collapsed even with same stat", {
  # Some papers report the same r value in two distinct contexts (subgroup vs
  # full sample). The dedup must NOT merge them.
  txt <- paste(
    "Our sample comprised N = 743 participants.",
    "Subgroup analysis: r(348) = -0.34, p < .001.",
    "Full-sample: r(741) = -0.34, p < .001."
  )
  res <- effectcheck::check_text(txt)
  match_rows <- res[!is.na(res$stat_value) & res$test_type == "r" & abs(res$stat_value - (-0.34)) < 0.005, ]
  expect_equal(nrow(match_rows), 2L)
})

test_that("same stat + same df but different CI bounds are NOT collapsed", {
  # Caught by cycle-4 verifier on chan_feldman: H1a r(261)=0.45 with CI
  # [.35,.55] and H2a r(261)=0.45 with CI [.35,.54] are distinct hypothesis
  # tests that happen to share a stat value; the dedup must treat the CI
  # bound difference as a discriminating signal.
  txt <- paste(
    "Our sample comprised N = 263 participants.",
    "H1a empathy-to-apology: r(261) = 0.45, p < .001, 95% CI [0.35, 0.55].",
    "H2a forgiveness-to-conciliation: r(261) = 0.45, p < .001, 95% CI [0.35, 0.54]."
  )
  res <- effectcheck::check_text(txt)
  match_rows <- res[!is.na(res$stat_value) & res$test_type == "r" & abs(res$stat_value - 0.45) < 0.005, ]
  expect_equal(nrow(match_rows), 2L)
})

test_that("same stat + same df but different effect_reported are NOT collapsed", {
  # Caught by cycle-4 verifier on chan_feldman: a figure-header restatement
  # carrying d = 1.09 should not be collapsed into a body-text t-row that
  # has no d binding, even if t and df match.
  txt <- paste(
    "Our sample comprised N = 263 participants.",
    "Body: t(261) = 8.24, p < .001.",
    "",
    "Figure 2. t_Student(261) = 8.24, p < .001, d = 1.09."
  )
  res <- effectcheck::check_text(txt)
  match_rows <- res[!is.na(res$stat_value) & res$test_type == "t" & abs(res$stat_value - 8.24) < 0.005, ]
  expect_equal(nrow(match_rows), 2L)
})

test_that("non-duplicates with different stat values survive", {
  txt <- paste(
    "r(741) = -0.20, p < .001, 95% CI [-0.27, -0.13]; ",
    "r(741) = -0.22, p < .001, 95% CI [-0.29, -0.15]."
  )
  res <- effectcheck::check_text(txt)
  match_rows <- res[!is.na(res$stat_value) & res$test_type == "r", ]
  expect_equal(nrow(match_rows), 2L)
  expect_setequal(round(match_rows$stat_value, 2), c(-0.20, -0.22))
})
