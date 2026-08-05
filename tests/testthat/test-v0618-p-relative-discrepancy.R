# v0.6.18 -- a p-value consistency check on an ABSOLUTE difference is blind at
# small p, where every p-value that matters lives.
#
# Source: 2026-08-04 escicheck-iterate cycle 2, tracing the pci.rr.100726 canary
# finding "reported p = .006 vs computed p = .00269 (2.23x) passes as
# status = OK". Reproduced at HEAD before fixing.
#
# WHAT WENT WRONG
#
# The no-effect-size p-consistency ladder (check.R Phase 9) reads:
#
#     p_diff <- abs(p_reported - p_computed)
#     ...
#     } else if (p_diff < 0.005) { status <- "OK" }        # Case 2
#
# `abs()` is the wrong scale for a p-value. Worked examples, all of which the
# absolute rule waves through SILENTLY:
#
#     reported .006  computed .00269   abs 0.0033    2.2x
#     reported .004  computed .0001    abs 0.0039   40.0x
#     reported .005  computed .00002   abs 0.0050  250.0x
#
# A 250x discrepancy is not a rounding artifact -- at t(868), p = .006 implies
# t = 2.755 where 3.01 was reported. Meanwhile the genuinely harmless case the
# threshold exists for -- .049 vs .045, a 1.1x rounding wobble -- must keep
# passing.
#
# THE RULE: at small p, judge on the RATIO; keep the absolute tolerance for the
# large-p region where a ratio is meaninglessly volatile (.9 vs .8 is 1.1x and
# uninteresting; .0001 vs .00001 is 10x and a real inconsistency). The
# `decision_error` machinery is untouched -- it tests the significance boundary
# and is orthogonal to this magnitude check.

test_that("a large RELATIVE p discrepancy at small p is no longer silently OK", {
  # t(868) = -3.01 computes p = .00269; the text reports p = .006 (2.23x).
  # abs diff = 0.0033 < 0.005, so the old Case 2 returned OK.
  txt <- "The effect was significant, t(868) = -3.01, p = .006."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  row <- rows[1, ]
  expect_false(identical(as.character(row$status[1]), "OK"))
  # The message must state the RATIO -- the whole point is that the absolute
  # gap looked harmless.
  expect_match(as.character(row$uncertainty_reasons[1]), "differ by a factor of")
})

test_that("an extreme relative p discrepancy at tiny p is caught", {
  # Reported p = .004 where the statistic implies a far smaller p.
  txt <- "The effect was significant, t(500) = 6.20, p = .004."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_false(identical(as.character(rows$status[1]), "OK"))
})

test_that("ordinary rounding at larger p still passes cleanly", {
  # .045 vs ~.049 is a 1.1x wobble -- exactly what the absolute tolerance is
  # for. It must NOT become a false positive now that a ratio rule exists.
  txt <- "The effect was significant, t(100) = 2.02, p = .046."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_true(as.character(rows$status[1]) %in% c("OK", "PASS"))
})

test_that("an exactly-consistent p stays clean", {
  # t(28) = 2.21 gives p = .0354; reporting .035 is correct to the digit shown.
  txt <- "The effect was significant, t(28) = 2.21, p = .035."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_true(as.character(rows$status[1]) %in% c("OK", "PASS"))
})

test_that("a p reported as an inequality is unaffected", {
  # `p < .001` with a computed p far below .001 is consistent by construction;
  # the ratio rule must not fire on an inequality.
  txt <- "The effect was significant, t(500) = 6.20, p < .001."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_true(as.character(rows$status[1]) %in% c("OK", "PASS"))
})
