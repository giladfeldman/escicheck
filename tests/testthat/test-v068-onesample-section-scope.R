# v0.6.8 (E-A1): section-scoped one-sample carry-forward. A "one-sample t-test
# against the {scale midpoint|chance|N}" declaration scopes the t-tests that follow
# it within its study/section, even when those tests sit many sentences later (past
# an interleaved table the PDF flattened between body paragraphs) and so are out of
# the per-row +-2-sentence context window. Two-tier carry distance: a plain
# declaration reaches only the next few chunks; a multi-scope ("for each ...")
# declaration reaches far. A PROSE contradicting design declaration ("we ran a
# paired / independent / Welch t-test") cancels the carry, and a one-sample signal
# living ONLY in a trailing "Table N ..." caption does not relabel a body test that
# precedes the table.
#
# Surfaced by the 2026-06-29 escicheck-iterate cycle-1 canary audit against
# collabra.57785 Study 3B/3C (4 one-sample t(742) tests mislabeled "independent").
# The false-positive guards were derived from rsos.250908, whose paired
# condition-comparison t-tests (gold: dependent t-test) must NOT be one-sampled.

# Helper: build a multi-sentence body where a declaration precedes a t-test by a
# given number of filler sentences.
filler <- function(n) paste(rep("This sentence adds neutral filler context to the section.", n), collapse = " ")

test_that("a FAR one-sample t-test is one-sample when the declaration says 'for each sub-question'", {
  txt <- paste0(
    "We conducted a one-sample t-test against the scale midpoint of 50 for each of ",
    "the sub-questions in this study. ",
    filler(10),
    "We found that experience gave greater insight, t(742) = 30.74, p < .001, d = 1.13, ",
    "95% CI [1.04, 1.22]."
  )
  res <- effectcheck::check_text(txt)
  tr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 30.74) < 1e-9, ]
  expect_true(nrow(tr) >= 1)
  expect_true(any(tr$design_inferred == "one-sample", na.rm = TRUE))
})

test_that("a NEAR one-sample t-test is one-sample even without 'for each'", {
  txt <- paste0(
    "We conducted a one-sample t-test (two-tailed) against the scale midpoint of 50. ",
    "We found support that knowledge of experiences gave greater insight, ",
    "t(742) = 24.00, p < .001, d = 0.88, 95% CI [0.80, 0.96]."
  )
  res <- effectcheck::check_text(txt)
  tr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 24.00) < 1e-9, ]
  expect_true(nrow(tr) >= 1)
  expect_true(any(tr$design_inferred == "one-sample", na.rm = TRUE))
})

test_that("a FAR test is NOT one-sample when the plain declaration lacks 'for each' (rsos FP guard)", {
  # A plain one-sample declaration (no multi-scope phrasing) must not reach a test
  # 10 sentences away describing a DIFFERENT (paired) comparison.
  txt <- paste0(
    "We conducted a one-sample t-test against the midpoint as a manipulation check. ",
    filler(10),
    "Comparing morally good and neutral changes, surface-self differed, ",
    "t(801) = -7.17, p < .001."
  )
  res <- effectcheck::check_text(txt)
  tr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - (-7.17)) < 1e-9, ]
  expect_true(nrow(tr) >= 1)
  expect_false(any(tr$design_inferred == "one-sample", na.rm = TRUE))
})

test_that("a one-sample signal in a TRAILING table caption does not relabel a preceding paired test", {
  # rsos.250908: "..., t(801) = 8.73, p < .001). Table 14. ... (one-sample t-test
  # against midpoint 0)." The paired comparison precedes the table; the caption
  # describes the table, not this sentence.
  txt <- paste0(
    "When compared with neutral change, true-self was more revealed in morally bad ",
    "change, t(801) = 8.73, p < .001. ",
    "Table 14. Study 1: morality valence manipulation check (one-sample t-test ",
    "against midpoint 0)."
  )
  res <- effectcheck::check_text(txt)
  tr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 8.73) < 1e-9, ]
  expect_true(nrow(tr) >= 1)
  expect_false(any(tr$design_inferred == "one-sample", na.rm = TRUE))
})

test_that("a one-sample signal in a FOLLOWING sentence does not relabel a preceding paired test", {
  # collabra.23443: paired estimate-comparisons "..., t(798) = 23.7, ..." are
  # FOLLOWED by "Participants underestimated ... based on one-sample t-tests: ...
  # t(798) = 11.84 ...". The trailing "one-sample" must not relabel the 23.7 row
  # (gold: within-subjects paired); the genuine one-sample 11.84 row stays one-sample.
  txt <- paste0(
    "Being paid increased willingness based on paired-sample t-tests, ",
    "t(798) = 4.17, p < .001, d = 0.13 [0.07, 0.19]. ",
    "As expected, participants estimated others would be more willing when paid, ",
    "t(798) = 23.7, p < .001, d = 0.64 [0.58, 0.70]. ",
    "Participants underestimated donation rates based on one-sample t-tests: ",
    "willingness was higher than estimated, t(798) = 11.84, p < .001, d = 0.48 [0.38, 0.58]."
  )
  res <- effectcheck::check_text(txt)
  paired_row <- res[!is.na(res$test_type) & res$test_type == "t" &
                      !is.na(res$stat_value) & abs(res$stat_value - 23.7) < 1e-9, ]
  expect_true(nrow(paired_row) >= 1)
  expect_false(any(paired_row$design_inferred == "one-sample", na.rm = TRUE))
  # The genuine one-sample row (its OWN clause names one-sample) is one-sample.
  os_row <- res[!is.na(res$test_type) & res$test_type == "t" &
                  !is.na(res$stat_value) & abs(res$stat_value - 11.84) < 1e-9, ]
  expect_true(nrow(os_row) >= 1)
  expect_true(any(os_row$design_inferred == "one-sample", na.rm = TRUE))
})

test_that("a one-sample test against mu=0 with an inline table reference stays one-sample", {
  # collabra.23443: "Overall, participants overestimated ... (see Table 7 and Figure
  # 4 ...). The one-sample t-test against mu = 0 for overestimation ... t(604) = 19.9
  # ...". The inline "see Table 7" must not strip the genuine one-sample declaration.
  txt <- paste0(
    "Overall, participants overestimated the self-interest of smokers (see Table 7 ",
    "and Figure 4 for the results by sample). ",
    "The one-sample t-test against mu = 0 for overestimation in both samples combined ",
    "was t(604) = 19.9, p < .0001, d = 0.81 [0.72, 0.90]."
  )
  res <- effectcheck::check_text(txt)
  tr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 19.9) < 1e-9, ]
  expect_true(nrow(tr) >= 1)
  expect_true(any(tr$design_inferred == "one-sample", na.rm = TRUE))
})

test_that("a one-sample t-test's repro_code emits the one-sample formula, not the independent one", {
  # The PASS verdict for a one-sample t uses d = t / sqrt(N) (= d_onesample), so
  # the emitted reproduction code must compute the SAME thing -- not the
  # independent-samples d ~ 2t/sqrt(df), which is ~2x larger.
  txt <- paste0(
    "We conducted a one-sample t-test against the scale midpoint of 50. ",
    "We found greater insight, t(742) = 24.00, p < .001, d = 0.88, 95% CI [0.80, 0.96]."
  )
  res <- effectcheck::check_text(txt)
  os <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 24.00) < 1e-9, ]
  expect_true(nrow(os) >= 1)
  expect_true(any(os$design_inferred == "one-sample", na.rm = TRUE))
  rc <- as.character(os$repro_code[1])
  expect_true(grepl("d_onesample <- stat / sqrt(N)", rc, fixed = TRUE))
  expect_false(grepl("d_ind <- 2 * stat / sqrt(df1)", rc, fixed = TRUE))
  # The reproduced output value matches the reported one-sample d (~0.88).
  ro <- as.character(os$repro_output[1])
  expect_true(grepl("d_onesample", ro, fixed = TRUE))
})

test_that("a PROSE paired declaration cancels a preceding one-sample carry", {
  # A one-sample declaration, then a prose "we ran a paired t-test", then the test:
  # the paired declaration is nearest and wins (the test is paired, not one-sample).
  txt <- paste0(
    "We conducted a one-sample t-test against the scale midpoint for each sub-question. ",
    filler(3),
    "We then ran a two-tailed paired t-test on the recall measure. ",
    "Participants were more satisfied with experiential purchases, ",
    "t(742) = 3.15, p = .002, d = 0.15, 95% CI [0.07, 0.22]."
  )
  res <- effectcheck::check_text(txt)
  tr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 3.15) < 1e-9, ]
  expect_true(nrow(tr) >= 1)
  expect_false(any(tr$design_inferred == "one-sample", na.rm = TRUE))
})
