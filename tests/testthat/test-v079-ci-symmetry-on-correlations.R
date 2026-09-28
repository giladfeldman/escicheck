# v0.7.9 -- ci_symmetry and ci_symmetry_class were NA on EVERY correlation that
# reports a confidence interval, which is exactly the row class they exist to
# protect.
#
# Source: 2026-09-02 escicheck-iterate, item 4 of the fix queue. Found on the
# malformed interval in Chan & Feldman (2025), doi 10.1080/02699931.2024.2434156
# -- `r = -.43` published with `[-0.52, 0.33]`, an interval whose two arms differ
# by a factor of eight around the estimate. The diagnostic designed to notice
# precisely that shape reported nothing.
#
# WHAT WENT WRONG
#
# Both diagnostics read `effect_reported` (and, for the class, `canonical_type`):
#
#     if (!is.na(effect_reported) && !is.na(ciL_rep) && !is.na(ciU_rep)) {
#       lower_arm <- abs(effect_reported - ciL_rep) ...
#
# For a correlation the point estimate IS the test statistic, and the r branch
# adopts `stat` into `effect_reported` -- about four hundred lines BELOW this
# block, at the "adopt r as its own effect when there is either a p or a reported
# CI" gate. So at the moment the symmetry test runs, `effect_reported` is NA on
# every r row and the test silently skips. `canonical_type` is NA there too, so
# the `canonical_type == "r"` arm of `expects_asym` was unreachable as well.
#
# THE TWO-SIDED MEASUREMENT that pins it, taken at the unfixed code 2026-09-02:
#
#   r = -.43, CI [-0.52, 0.33]   ci_symmetry NA   class NA   ci_width_ratio 0.243
#   r =  .63, CI [ 0.56, 0.70]   ci_symmetry NA   class NA   ci_width_ratio 1.048
#   t(118), d = 0.43, [0.06, 0.79]  ci_symmetry "symmetric"  class
#                                   "symmetric_expected"     ci_width_ratio 0.991
#
# `ci_width_ratio` sits in the same block and populated on all three, because it
# needs no point estimate. So the block was entered, the surrounding machinery
# worked, and only the two fields that need the estimate were inert -- on the one
# test type whose estimate arrives late. Not a parse failure and not a gating
# failure: an ordering failure, invisible from anywhere except a row that has a
# malformed interval and no other flag to catch it.
#
# FIX: read the correlation's estimate and type from `stat` / `tt` at this point,
# where the later adoption has not happened yet. Nothing else in the block
# changes, and no other test type is touched.

sym_doc <- function(clause) {
  paste0(
    "We recruited N = 794 participants. ",
    paste(rep("Filler about the procedure. ", 20), collapse = ""),
    clause
  )
}

test_that("a malformed correlation interval is now reported as asymmetric", {
  # Chan & Feldman Table 9 row 2bii: a dropped minus in the upper bound, the
  # authors' own typo (confirmed by rasterizing page 13). Arms around the
  # estimate: 0.09 below, 0.76 above -- 8x.
  res <- effectcheck::check_text(sym_doc(
    "Forgiveness was associated with revenge, r(261) = -0.43, 95% CI [-0.52, 0.33], p < .001."))
  expect_equal(nrow(res), 1L)
  expect_equal(as.character(res$ci_symmetry[1]), "asymmetric",
    info = "an interval 8x wider on one side than the other must not report NA symmetry")
  expect_equal(as.character(res$ci_symmetry_class[1]), "asymmetric_unexpected",
    info = "|r| = .43 is below the 0.5 threshold at which Fisher-z asymmetry is expected, so this asymmetry is unexpected and must be labelled so")
})

test_that("a well-formed correlation interval is now reported as symmetric", {
  # The other half of the signal. If the fix reported "asymmetric" for everything
  # it would be as useless as reporting NA -- this is the row that must come back
  # the other way.
  res <- effectcheck::check_text(sym_doc(
    "Forgiveness was associated with conciliation, r(261) = 0.63, 95% CI [0.56, 0.70], p < .001."))
  expect_equal(nrow(res), 1L)
  expect_equal(as.character(res$ci_symmetry[1]), "symmetric")
  expect_equal(as.character(res$ci_symmetry_class[1]), "symmetric_unexpected",
    info = "|r| = .63 is above 0.5, where a Fisher-z interval IS asymmetric, so a perfectly symmetric printed interval is the unexpected case -- the canonical_type == 'r' arm of expects_asym was unreachable before this fix")
})

test_that("CONTROL: a correlation with no reported CI keeps both fields NA", {
  res <- effectcheck::check_text(sym_doc(
    "Forgiveness was associated with revenge, r(261) = -0.43, p < .001."))
  expect_equal(nrow(res), 1L)
  expect_true(is.na(res$ci_symmetry[1]))
  expect_true(is.na(res$ci_symmetry_class[1]))
  expect_true(is.na(res$ci_width_ratio[1]))
})

test_that("CONTROL: a t-test reporting a named d is byte-identical to before", {
  # This row already worked, because `d` is a NAMED effect size present in
  # effect_reported from parse time. Measured at the unfixed code: "symmetric" /
  # "symmetric_expected" / 0.991. The fix must not move it.
  res <- effectcheck::check_text(sym_doc(
    "The groups differed, t(118) = 2.31, p = .022, d = 0.43, 95% CI [0.06, 0.79]."))
  tt <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(tt), 1L)
  expect_equal(as.character(tt$ci_symmetry[1]), "symmetric")
  expect_equal(as.character(tt$ci_symmetry_class[1]), "symmetric_expected")
  expect_equal(as.numeric(tt$ci_width_ratio[1]), 0.991)
})

test_that("CONTROL: the correlation's other reported fields do not move", {
  # The fix reads `stat` for two diagnostics only. It must not leak into the
  # published estimate, the verdict, or the width ratio.
  res <- effectcheck::check_text(sym_doc(
    "Forgiveness was associated with revenge, r(261) = -0.43, 95% CI [-0.52, 0.33], p < .001."))
  expect_equal(as.numeric(res$effect_reported[1]), -0.43)
  expect_equal(as.numeric(res$ci_width_ratio[1]), 0.243)
  expect_equal(as.character(res$ci_check_status[1]), "INCONSISTENT")
})
