# v0.6.6: an F-test whose context window directly names a within-subjects /
# repeated-measures ANOVA must be labeled design_inferred = "within", even when
# the (multi-sentence) window ALSO contains a "between-subjects" keyword
# elsewhere. The old F-test design block keyed on the bare word "between" first
# (first-keyword-wins), so a within-subjects ANOVA discussed alongside a
# between-subjects comparison was mislabeled "between".
#
# Surfaced by the 2026-06-25 escicheck-iterate canary audit (cycle 1) against
# collabra.57785 (Experiential vs Material): the "2 (purchase type) x 2 (feeling
# time) within-subjects two-way ANOVA" rows F(1,742) = 101.10 / 54.70 were
# labeled "between" because the same window discussed "both a between-subjects
# design and within-subjects design were performed". The gold confirms these are
# repeated-measures 2x2 ANOVAs.
#
# Fix: a DEFINITIVE design signal (the keyword directly modifying "ANOVA")
# wins over a stray opposite keyword; the bare fallback requires design-specific
# phrasing ("between-subjects" / "within-subjects" / "repeated measures"), NOT
# the bare preposition "between" (which appears in "interaction between X and Y").

test_that("a within-subjects ANOVA in a between-subjects-mentioning context is labeled within", {
  txt <- paste0(
    "For Study 5 both a between-subjects design and within-subjects design were performed ",
    "as a comparison to the original article. ",
    "We conducted a 2 (purchase type: material vs. experiential) x 2 (feeling time: ",
    "current vs. past) within-subjects two-way ANOVA and found support for the main ",
    "effect of feeling time, F(1, 742) = 101.10, p < .001, partial eta-squared = 0.12, ",
    "CI [0.08, 0.17]."
  )
  res <- effectcheck::check_text(txt)
  ff <- res[!is.na(res$test_type) & res$test_type == "F" &
              !is.na(res$stat_value) & abs(res$stat_value - 101.10) < 1e-6, ]
  expect_true(nrow(ff) >= 1)
  expect_true(any(ff$design_inferred == "within", na.rm = TRUE))
  expect_false(any(ff$design_inferred == "between", na.rm = TRUE))
})

test_that("a genuine between-subjects ANOVA stays between", {
  txt <- paste0(
    "We conducted a one-way between-subjects ANOVA comparing the three conditions, ",
    "F(2, 297) = 8.41, p < .001, partial eta-squared = 0.05, CI [0.01, 0.10]."
  )
  res <- effectcheck::check_text(txt)
  ff <- res[!is.na(res$test_type) & res$test_type == "F" &
              !is.na(res$stat_value) & abs(res$stat_value - 8.41) < 1e-6, ]
  expect_true(nrow(ff) >= 1)
  expect_true(any(ff$design_inferred == "between", na.rm = TRUE))
})

test_that("a within-subjects ANOVA interaction row is within even when the sentence says 'interaction between X and Y'", {
  # The real collabra.57785 F(1,742)=5.54 interaction row: its multi-sentence
  # window establishes the within-subjects ANOVA design, and the interaction
  # sentence itself uses the preposition "between purchase type and feeling
  # type". A within-subjects DESIGN PHRASE in the window must outrank the bare
  # preposition "between" so the row is labeled within (the gold says within).
  txt <- paste0(
    "We conducted a 2 (purchase type) x 2 (feeling time) within-subjects two-way ANOVA. ",
    "We found support for an interaction between purchase type and feeling type, ",
    "F(1, 742) = 5.54, p = .019, partial eta-squared = 0.01, CI [0.00, 0.02]."
  )
  res <- effectcheck::check_text(txt)
  ff <- res[!is.na(res$test_type) & res$test_type == "F" &
              !is.na(res$stat_value) & abs(res$stat_value - 5.54) < 1e-6, ]
  expect_true(nrow(ff) >= 1)
  expect_true(any(ff$design_inferred == "within", na.rm = TRUE))
  expect_false(any(ff$design_inferred == "between", na.rm = TRUE))
})

test_that("a repeated-measures ANOVA is labeled within", {
  txt <- paste0(
    "A repeated-measures ANOVA on the four timepoints revealed a main effect of time, ",
    "F(3, 117) = 12.30, p < .001, partial eta-squared = 0.24, CI [0.11, 0.35]."
  )
  res <- effectcheck::check_text(txt)
  ff <- res[!is.na(res$test_type) & res$test_type == "F" &
              !is.na(res$stat_value) & abs(res$stat_value - 12.30) < 1e-6, ]
  expect_true(nrow(ff) >= 1)
  expect_true(any(ff$design_inferred == "within", na.rm = TRUE))
})
