# v0.7.9 -- the chi-square family must not verify a reported effect against a
# sample size derived from that same effect.
#
# Gilad's ruling, 2026-09-04: "We should never just correct things, the aim is
# highest transparency and accuracy." Reported to him as a major bug and fixed on
# his instruction ("yes, fix them as well").
#
# THE BUG. `V_from_chisq(chisq, N, m) = sqrt(chisq/(N*m))` and the back-calculation
# `N = chisq/(V^2 * m)` are exact algebraic inverses. Recomputing V from a
# back-solved N therefore returns the reported V identically: delta 0, PASS, for
# ANY reported value. The same holds for phi.
#
# WATCHED RED FIRST. Measured on the unfixed code, 2026-09-04, with a document
# stating N = 500 and chi-square(4) = 20.00 -- which fix V at 0.10 and nothing
# else. All four reported values passed, three of them after the stated N was
# silently replaced:
#
#     reported V = 0.20 -> PASS, delta 0     , N kept 500
#     reported V = 0.35 -> PASS, delta 0.0003, N replaced by 163
#     reported V = 0.60 -> PASS, delta 0.0024, N replaced by  56
#     reported V = 0.90 -> PASS, delta 0.0056, N replaced by  25
#
# A published N of 25 for a study reporting 500, with `N_source` still crediting
# the document's own text. The check could not fail.
#
# THE FIX has two halves, and the CONTROLS below are what keep them apart:
#   * a STATED N that disagrees with the reported effect is EVIDENCE -- the
#     comparison is independent, so a real inconsistency must still be reported;
#   * an N DERIVED FROM the reported effect is CIRCULAR -- no verdict may rest
#     on it.
# An earlier draft conflated the two and note-washed a genuine ERROR. The df=1
# controls exist to catch exactly that regression.

chisq_row <- function(res) {
  k <- !is.na(res$test_type) & res$test_type == "chisq"
  res[k, , drop = FALSE]
}

# df = 1 is a 2x2 table: m = 1 unambiguously, so V is fully determined and the
# tool has everything it needs to grade honestly.
UNAMBIGUOUS <- function(v) sprintf(paste(
  "A total of N = 500 participants were surveyed. The association was",
  "significant, chi-square(1) = 20.00, p < .001, Cramer's V = %s."), v)

test_that("CONTROL: a correctly reported effect on an unambiguous table still PASSES", {
  # chi2(1)=20, N=500 -> V = sqrt(20/500) = 0.2000 exactly.
  # If this ever stops passing, the fix has over-corrected into note-washing
  # everything, which is as bad as the bug it replaced.
  row <- chisq_row(check_text(UNAMBIGUOUS(".20")))
  expect_equal(nrow(row), 1L)
  expect_identical(as.character(row$status[1]), "PASS")
  expect_lt(as.numeric(row$delta_effect_abs[1]), 0.01)
})

test_that("CONTROL: a WRONG effect on an unambiguous table is still reported as ERROR", {
  # The document's own numbers are mutually impossible. That is a true finding and
  # must NOT be softened -- an earlier draft of this fix returned NOTE here.
  row <- chisq_row(check_text(UNAMBIGUOUS(".90")))
  expect_equal(nrow(row), 1L)
  expect_identical(as.character(row$status[1]), "ERROR")
  expect_gt(as.numeric(row$delta_effect_abs[1]), 0.5)
})

test_that("a stated N is NEVER replaced by one back-solved from the reported effect", {
  # This is the headline defect. Pre-fix, N came back as 25.
  row <- chisq_row(check_text(UNAMBIGUOUS(".90")))
  expect_equal(as.numeric(row$N[1]), 500,
    info = "the published N must be the one the document states, not one inferred from its effect size")
})

test_that("the conflict between the stated N and the reported effect is reported to the reader", {
  row <- chisq_row(check_text(UNAMBIGUOUS(".90")))
  reasons <- as.character(row$uncertainty_reasons[1])
  expect_true(grepl("CONFLICTING SAMPLE SIZE", reasons, fixed = TRUE))
  expect_true(grepl("N=500", reasons, fixed = TRUE),
    info = "the sample size the document states must be named")
  expect_true(grepl("N=25", reasons, fixed = TRUE),
    info = "the sample size the reported effect implies must ALSO be named -- both readings")
})

test_that("the delta responds to the reported value instead of being 0 by construction", {
  # THE decisive property. Pre-fix every delta was ~0 whatever was reported,
  # because the recomputation was the inverse of the back-calculation. A check
  # whose output does not move when its input moves is not a check.
  deltas <- vapply(c(".20", ".35", ".60", ".90"), function(v) {
    as.numeric(chisq_row(check_text(UNAMBIGUOUS(v)))$delta_effect_abs[1])
  }, numeric(1))
  expect_true(all(diff(deltas) > 0),
    info = "delta must grow as the reported effect moves further from the truth")
  expect_gt(deltas[[4]] - deltas[[1]], 0.5)   # .90 vs .20, by position
})

test_that("a table shape that df does not determine is reported, not chosen by fit", {
  # df = 4 is a 5x2 table (m=1, V=0.200) or a 3x3 table (m=2, V=0.141). Until
  # v0.7.8 the tool picked whichever m best matched the REPORTED V and then graded
  # the paper against that choice. Now both readings are reported and no confident
  # verdict is published.
  txt <- paste("A total of N = 500 participants were surveyed. The association was",
               "significant, chi-square(4) = 20.00, p < .001, Cramer's V = .20.")
  row <- chisq_row(check_text(txt))
  expect_equal(nrow(row), 1L)

  expect_identical(as.character(row$status[1]), "NOTE",
    info = "an effect computed on an undetermined table shape must not publish PASS")
  reasons <- as.character(row$uncertainty_reasons[1])
  expect_true(grepl("AMBIGUOUS TABLE SHAPE", reasons, fixed = TRUE))
  expect_true(grepl("m=1", reasons, fixed = TRUE))
  expect_true(grepl("m=2", reasons, fixed = TRUE),
    info = "every candidate shape must be listed, with the V it implies")
})

test_that("when no N is stated at all, the back-solved N is used but claims nothing", {
  # With no N in the document the back-calculation is the only way to get one, so
  # it stays -- but the effect cannot be verified against it, and the row must say
  # so rather than publishing PASS.
  txt <- paste("The association between the two categorical measures was significant,",
               "chi-square(1) = 20.00, p < .001, phi = .20.")
  row <- chisq_row(check_text(txt))
  expect_equal(nrow(row), 1L)

  expect_false(identical(as.character(row$status[1]), "PASS"),
    info = "an effect size compared against an N derived from itself must never publish PASS")
  expect_true(grepl("SAMPLE SIZE DERIVED FROM THE REPORTED EFFECT",
                    as.character(row$uncertainty_reasons[1]), fixed = TRUE))
})
