# Regression test for v0.4.1 -- t-test global-text N override.
#
# Defect found by the escicheck-iterate pilot (2026-05-16) on the real
# docpluck extraction of Chen et al. (2021, JESP) hindsight-bias replication.
# In that paper Study 3 reports three independent-samples t-tests in body prose
# (t(364) = 9.84, t(330) = 3.95, t(340) = -5.84). effectcheck attached a
# document-wide N (Study 2's "N = 608") to each, even though an independent
# t-test with df = 364 has N = df + 2 = 366 -- N = 608 is structurally
# impossible. The wrong N produced a wrong recomputed d and a spurious WARN,
# although each reported d is in fact consistent. check.R now overrides a
# global_text N that is incompatible with the t-test df with the df-based N.
#
# The strings below reproduce the real N_source = "global_text" path: an
# explicit document-wide "N = 608" far from the statistic, and a t-test whose
# local/extended context carries no N of its own.

make_global_n_text <- function(stat_sentence) {
  filler <- paste(rep(paste(
    "The study used a between-subject design and participants were randomly",
    "assigned to one of three conditions in the replication of the original",
    "experiment, with comprehension checks administered before the main task."
  ), 8), collapse = " ")
  paste0(
    "In Study 2 (N = 608), we replicated the original experiment with a larger sample. ",
    filler,
    " We conducted independent samples t-tests to test the hindsight effect. ",
    stat_sentence
  )
}

test_that("v0.4.1: global-text N incompatible with t-test df is overridden (Chen 2021 JESP S3)", {
  cases <- list(
    list(s = paste("Participants informed of Outcome Success estimated a successful",
                    "replication to be more probable than those informed of Outcome",
                    "Fail, t(364) = 9.84, p < .001, Cohen's d = 1.03, 95% CI [0.80, 1.26]."),
         df = 364),
    list(s = paste("Participants informed of Outcome Success estimated a successful",
                    "replication to be more probable than those who did not know the",
                    "outcome, t(330) = 3.95, p < .001, Cohen's d = 0.43, 95% CI [0.21, 0.65]."),
         df = 330),
    list(s = paste("Participants informed of Outcome Fail estimated a successful",
                    "replication to be less probable than those who did not know the",
                    "outcome, t(340) = -5.84, p < .001, Cohen's d = -0.64, 95% CI [-0.86, -0.41]."),
         df = 340)
  )
  for (cs in cases) {
    r <- check_text(make_global_n_text(cs$s))
    row <- r[r$test_type == "t" & !is.na(r$stat_value), ]
    expect_equal(nrow(row), 1)
    # N was sourced from the document-wide statement, not local context
    expect_identical(row$N_source[1], "global_text")
    # ... and is replaced by the df-based N (df + 2), not the global N = 608
    expect_lt(abs(row$N[1] - (cs$df + 2)), 1.5)
    # with the structurally-correct N the reported d is consistent -> PASS
    expect_identical(row$status[1], "PASS")
  }
})

test_that("v0.4.1: a non-global-text large N is still NOT overridden", {
  # Guards the else-branch: when N is found in local context (not the global
  # fallback), the pre-existing flag-only behaviour is preserved -- the fix
  # must not silently rewrite an N that came from a trusted adjacent source.
  r <- check_text("In a sample of 467 participants, t(262) = 3.50, p < .001, g = 1.50")
  expect_true(r$status[1] %in% c("WARN", "NOTE", "ERROR"))
  expect_false(identical(r$N_source[1], "global_text"))
})
