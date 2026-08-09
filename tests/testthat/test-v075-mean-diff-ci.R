# v0.7.5 -- handoff Issue A residue: an unstandardized mean difference reported
# with a confidence interval and a p-value, and NO test statistic.
#
#   "Alzheimer's disease: Mean difference of 457.66 articles,
#    p-value = 7.171e-11, confidence interval (320.98, 594.35)."
#
# ieee_access_alt prints three of these and produced ZERO rows: every existing
# pattern anchors on a test statistic or a standardized effect size and this
# clause has neither. The triple is nonetheless mutually checkable --
# SE = (594.35 - 320.98)/(2 x 1.96) = 69.74, z = 457.66/69.74 = 6.56,
# p ~ 5.3e-11 against a reported 7.171e-11 -- and `md_hl` already establishes
# that a row may carry CI-symmetry and p-CI-consistency checks while claiming no
# effect size. Scope confirmed with the user before implementing.
#
# NO STANDARDIZED EFFECT SIZE IS EVER CLAIMED. A mean difference "in articles"
# has no standardizer in the clause, and inventing one would be the cross-scale
# defect `ci_referent` exists to prevent.
#
# Whole-corpus diff over 48 texts: 0 rows lost, 0 changed, 3 GAINED -- all three
# in this previously zero-row paper.

alz <- paste("Alzheimer's disease: Mean difference of 457.66 articles,",
             "p-value = 7.171e-11, confidence interval (320.98, 594.35).")

test_that("v0.7.5: an estimate + CI + p triple is extracted and typed", {
  r <- check_text(alz)
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type[1], "mean_diff_ci")
  expect_equal(r$stat_value[1], 457.66)
  expect_equal(r$ciL_reported[1], 320.98)
  expect_equal(r$ciU_reported[1], 594.35)
  expect_equal(r$p_reported[1], 7.171e-11)
  # Extraction-only for the effect size: NOTE, never a PASS that would imply a
  # standardized effect was verified.
  expect_equal(r$status[1], "NOTE")
  expect_match(paste(unlist(r$uncertainty_reasons), collapse = " "),
               "no standard effect size is recoverable", fixed = TRUE)
})

test_that("v0.7.5: a correctly reported triple is NOT flagged", {
  # The implied p is 5.291e-11 against a reported 7.171e-11 -- a factor of 1.36.
  # A normal approximation against whatever the authors actually ran will differ
  # by that much routinely, so the tolerance is a RATIO and it is generous. A
  # checker that flags correct papers is worse than one that flags nothing.
  msg <- paste(unlist(check_text(alz)$uncertainty_reasons), collapse = " ")
  expect_false(grepl("disagrees with the p implied", msg, fixed = TRUE))
  expect_false(grepl("not the midpoint", msg, fixed = TRUE))
  expect_false(grepl("p-CI inconsistency", msg, fixed = TRUE))
})

test_that("v0.7.5: a mis-stated estimate is caught by the midpoint check", {
  # Unlike a Hodges-Lehmann interval, a normal-approximation CI on a mean
  # difference is symmetric BY CONSTRUCTION, so the estimate must be the
  # midpoint. 400.00 against a midpoint of 457.665 cannot both be right.
  r <- check_text(paste("Mean difference of 400.00 articles, p-value = 7.171e-11,",
                        "confidence interval (320.98, 594.35)."))
  expect_match(paste(unlist(r$uncertainty_reasons), collapse = " "),
               "not the midpoint of its own interval", fixed = TRUE)
})

test_that("v0.7.5: a mis-stated p is caught by the implied-p check", {
  r <- check_text(paste("Mean difference of 457.66 articles, p-value = 0.42,",
                        "confidence interval (320.98, 594.35)."))
  msg <- paste(unlist(r$uncertainty_reasons), collapse = " ")
  expect_match(msg, "disagrees with the p implied", fixed = TRUE)
  # ... and the interval excludes 0 while the p says non-significant.
  expect_match(msg, "p-CI inconsistency", fixed = TRUE)
})

test_that("v0.7.5: 'p-value = <e-notation>' parses for every test type", {
  # pat_p_enote required the operator directly after `p`, so "p-value = 7.171e-11"
  # matched nothing and the row published p_reported = NA with "not a valid
  # probability (outside [0,1] or unparseable)" -- about an ordinary probability
  # written the way IEEE and the clinical journals write it. Fixed in the SHARED
  # pattern, so it is not a fourth per-branch workaround.
  r <- check_text("t(45) = 6.31, p-value = 2.572e-08, d = 1.88")
  expect_equal(r$p_reported[1], 2.572e-08)
  # The pre-existing "p = <e-notation>" spelling must keep working.
  expect_equal(check_text("t(45) = 6.31, p = 2.572e-08, d = 1.88")$p_reported[1],
               2.572e-08)
})

test_that("v0.7.5: consecutive bulleted results are separate chunks", {
  # The chunk splitter needed a capital immediately after the whitespace, and a
  # list marker sits in between -- so three bullets stayed ONE chunk and only the
  # first statistic in the list was ever extracted.
  #
  # `-` IS the load-bearing member of the marker set, which is not obvious:
  # `normalize_text()` maps the U+FFFD the extractor delivers for this paper to a
  # HYPHEN (codepoint 45) before chunking runs, so the real text arrives as
  # "- Childhood cancer: ...". An earlier revision dropped `-` on the theory that
  # U+FFFD was doing the work, and silently reverted the paper from 3 rows to 1.
  # Every marker is asserted here so that cannot recur unnoticed.
  for (marker in c(intToUtf8(0x2022), intToUtf8(0xFFFD), intToUtf8(0x00B7), "-", "*")) {
    txt <- paste0(
      "Alzheimer's disease: Mean difference of 457.66 articles, p-value = 7.171e-11, confidence interval (320.98, 594.35). ",
      marker,
      " Childhood cancer: Mean difference of 260.73 articles, p-value = 1.756e-07, confidence interval (163.30, 358.17).")
    r <- check_text(txt)
    expect_equal(nrow(r), 2L, info = paste("marker", utf8ToInt(marker)))
    expect_equal(sort(r$stat_value), c(260.73, 457.66), info = paste("marker", utf8ToInt(marker)))
  }
})

test_that("v0.7.5: the bullet rule cannot split a statistic from its own values", {
  # Invariant 6 (v0.7.4): a chunk boundary may never fall inside a reported
  # quantity, or between a test statistic and its own effect size, CI, p-value
  # or N. The bullet alternative keeps the `(?<=[.!?])` anchor, so a marker that
  # is NOT preceded by sentence-ending punctuation must not split. A minus sign
  # between a statistic and its CI is the dangerous case.
  r <- check_text("The effect held, t(58) = 2.31, p = .025, d = 0.61 - 95% CI [0.09, 1.13].")
  expect_equal(nrow(r), 1L)
  expect_equal(r$stat_value[1], 2.31)
  expect_equal(r$effect_reported[1], 0.61)

  # ... and the harder case, where the marker DOES follow sentence-ending
  # punctuation. Cross-model review (Claude Sonnet, 2026-08-09) REPRODUCED this
  # against the first version of the bullet rule: the row degraded from
  # `mcnemar_or` carrying OR = 0.99 [0.77, 1.27] to a bare `chisq` with both NA.
  # That is exactly how collabra.37122 lost an odds ratio in v0.7.4, and it is
  # the shape flattened-table extraction produces.
  #
  # A hyphen is also a dash, a minus and a range, so the marker alone is not
  # evidence. The discriminator is what FOLLOWS it: a new list item is prose,
  # a continued statistic is an assignment (`N = 211`, `OR = 0.99`).
  or_row <- check_text(
    "The association held, chi2(1) = 12.74, p = .013. - N = 211, OR = 0.99, 95% CI [0.77, 1.27].")
  expect_equal(nrow(or_row), 1L)
  expect_equal(or_row$effect_reported[1], 0.99)
  expect_equal(or_row$ciL_reported[1], 0.77)
  expect_equal(or_row$ciU_reported[1], 1.27)
})
