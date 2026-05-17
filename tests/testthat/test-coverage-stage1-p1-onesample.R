# Stage 1 / P1 -- one-sample t-test design label.
#
# A one-sample t-test was mislabelled design_inferred = "independent" and
# matched_variant = "dz" because Phase 8 design inference (check.R) had no
# one-sample pattern (effectcheck v0.4.2 and earlier). The recomputed value was
# already correct -- one-sample d = t/sqrt(N) coincides with the dz formula --
# so the defect was invisible to value-only tests. This fixture asserts the
# LABELS, which is the class of bug the Stage 1 test-strategy hardening targets.

test_that("one-sample t-test is labelled one-sample, not paired/independent", {
  txt <- paste(
    "We conducted a one-sample t-test (two-tailed) against the scale midpoint.",
    "Participants rated the item above the midpoint, t(99) = 4.20, p < .001, d = 0.42.",
    "There were N = 100 participants."
  )
  res <- check_text(txt)
  row <- res[res$test_type == "t", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$design_inferred[1], "one-sample")
  expect_equal(row$matched_variant[1], "d_onesample")
  expect_equal(row$matched_value[1], 0.42, tolerance = 0.02)
  expect_equal(row$status[1], "PASS")
})

test_that("an explicit independent-samples t-test is still labelled independent", {
  # Guards against the one-sample patterns over-matching a between-subjects test.
  res <- check_text(paste(
    "An independent-samples t-test showed the groups differed,",
    "t(98) = 2.40, p = .018, d = 0.48. There were N = 100 participants."
  ))
  row <- res[res$test_type == "t", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$design_inferred[1], "independent")
})

test_that("Gap 1: 'higher than chance' is detected as one-sample", {
  # Real verbatim from 10.1016/j.jesp.2021.104154 (Study 3, S3-R1). The Stage 1
  # one_sample_patterns matched "against chance" but NOT "than chance", so this
  # explicit one-sample t-test against a 50% chance baseline was mislabelled
  # design_inferred = "independent", matched_variant = "dz".
  txt <- paste0(
    "We found that the probability estimates for a successful replication ",
    "(MeanProb = 65.36%, S.D.Prob = 18.08%) were higher than chance (50%), ",
    "t(153) = 10.55, p < .001, d = 0.85."
  )
  res <- check_text(txt)
  row <- res[res$test_type == "t", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$design_inferred[1], "one-sample")
  expect_equal(row$matched_variant[1], "d_onesample")
  expect_equal(row$matched_value[1], 0.85, tolerance = 0.02)
})

test_that("Gap 1: 'differed from the scale midpoint' is detected as one-sample", {
  # The "from the ... midpoint" family is a mean-vs-constant comparison and
  # must route to one-sample alongside the "against the midpoint" phrasing.
  res <- check_text(paste(
    "Ratings differed from the scale midpoint,",
    "t(60) = 3.10, p = .003, d = 0.40. There were N = 61 participants."
  ))
  row <- res[res$test_type == "t", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$design_inferred[1], "one-sample")
  expect_equal(row$matched_variant[1], "d_onesample")
})

test_that("Gap 1 guard: 'higher than the control group' is NOT one-sample", {
  # "than" + a group reference (no constant) must stay independent -- the
  # over-match the broadened patterns were explicitly designed to avoid.
  res <- check_text(paste(
    "The treatment group scored higher than the control group,",
    "t(98) = 2.40, p = .018, d = 0.48. There were N = 100 participants."
  ))
  row <- res[res$test_type == "t", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$design_inferred[1], "independent")
})
