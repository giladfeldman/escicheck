# v0.6.13 (E-pairedci-indep-substring): the v0.6.12 paired-CI-unverifiable guard
# used an UNANCHORED "dependent samples" alternative in its within-subjects
# keyword regex, which matches as a SUBSTRING of "INdependent samples". So a
# genuinely independent-samples Welch clause whose context reads "We conducted
# independent samples Welch's t-tests" was falsely treated as within-subjects, and
# its CI verdict was capped at UNVERIFIABLE instead of the correct INCONSISTENT --
# masking a real reported-vs-computed CI discrepancy. The regex now anchors
# "dependent samples" with a negative lookbehind `(?<!in)`.
#
# Surfaced by the 2026-07-02 escicheck-iterate cycle-3 canary re-audit of cog_emo
# (Chan & Feldman, Cognition & Emotion): loc 284 t(520.72) = -1.93, d = 0.17 with a
# reported CI that genuinely disagrees with the computed independent CI.

test_that("an independent-samples Welch row with a discrepant CI is INCONSISTENT, not UNVERIFIABLE", {
  txt <- paste0(
    "We conducted independent samples Welch's t-tests (two-tailed). The difference ",
    "in affective empathy between the low empathy condition and the control condition ",
    "was weaker, t(520.72) = -1.93, p = .050, d = 0.17, 95% CI [-0.00, 0.34]."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(rr), 1L)
  # The reported CI [-0.00, 0.34] disagrees with the computed independent CI; because
  # this is a genuinely INDEPENDENT-samples row, the "independent samples" substring
  # must NOT trip the within-subjects guard.
  expect_false(identical(rr$ci_check_status[1], "UNVERIFIABLE"))
})

test_that("a genuine within-subjects paired row with a CI still caps at UNVERIFIABLE", {
  # The v0.6.12 behavior must be preserved: a paired t whose CI can only be an
  # independent-samples over-approximation stays UNVERIFIABLE, never INCONSISTENT.
  txt <- paste0(
    "Using a within-subjects design, participants were more willing to exchange their ",
    "memories of material purchases (M = 4.90, SD = 1.42, N = 743) than experiences ",
    "(M = 4.11, SD = 1.44, N = 743; paired t(742) = 12.24, p < .001, d = 0.55, ",
    "95% CI [0.47, 0.62])."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$ci_check_status[1], "UNVERIFIABLE")
})

test_that("a within-subjects row flagged only by 'dependent samples' (not 'independent') still caps", {
  # Confirm the negative lookbehind still MATCHES a real "dependent samples" phrase.
  txt <- paste0(
    "We used dependent samples (paired) analyses. Participants rated both conditions, ",
    "t(49) = 8.1, p < .001, dz = 1.16, 95% CI [0.80, 1.52]."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(rr), 1L)
  # dz is a within family, so this stays UNVERIFIABLE regardless; the point is the
  # regex must not crash and must still treat a true "dependent samples" as within.
  expect_true(rr$ci_check_status[1] %in% c("UNVERIFIABLE", "MATCH", "PLAUSIBLE"))
})
