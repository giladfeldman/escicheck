# v0.7.11: a row for which NO effect-size variant could be computed must not
# claim a cross-family fallback.
#
# Found by the escicheck-iterate canary audit of 10.1525/collabra.90203
# (2026-09-23) and confirmed by Opus + Sonnet 5 + Fable 5 in triage. Table 8 of
# that paper prints F, p, BF01, eta2p and a CI but NO degrees of freedom, so
# docpluck delivers e.g. `F = 0.01, eta2 = .00` with df1/df2 absent. Nothing can
# be recomputed from an F without its df, so `computed_variants` is EMPTY.
# check.R's "no same-type variants" branch nevertheless wrote
#   "No same-type variants available for 'etap2' - using all computed variants
#    [category: cross-family]"
# -- describing a fallback to "all computed variants" that did not exist, and
# tagging the row with the category API.md defines as "the matcher cross-falls
# to the closest computed variant in a different family". Neither happened.
#
# The fix is TEXT-ONLY by design (Fable 5's review): ambiguity_level stays
# "highly_ambiguous", so design_ambiguous, confidence and uncertainty_level are
# unchanged and the flag still never under-reports. Only the reason and its
# category tag stop describing an action that never ran.

tr <- function(label, row_label, fields, group = NULL, row_idx = 0L) {
  if (!is.null(group)) fields$group <- group
  list(table_id = "t1", label = label, row_label = row_label,
       row_idx = row_idx, fields = fields)
}

test_that("an F table row with no df does not claim a cross-family fallback", {
  # collabra.90203 Table 8 H1a Replication, exactly as docpluck 2.4.143 flattens it
  r <- as.data.frame(check_text("", table_rows = list(
    tr("Table 8", "Replication",
       list(F = 0.01, eta2 = 0.0, BF01 = 11.57, p = 0.923, p_op = "=",
            CI_lower = 0.0, CI_upper = 0.003))
  )))
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type, "F")
  expect_true(is.na(r$df1))
  expect_equal(r$effect_reported_name, "etap2")
  # nothing was computed ...
  expect_true(is.na(r$matched_variant))
  # ... so the reason must not say a fallback to computed variants happened
  expect_false(grepl("using all computed variants", r$ambiguity_reason, fixed = TRUE))
  expect_false(grepl("[category: cross-family]", r$ambiguity_reason, fixed = TRUE))
  expect_true(grepl("[category: not-computed]", r$ambiguity_reason, fixed = TRUE))
  # unchanged, deliberately: the flag still never under-reports
  expect_equal(r$ambiguity_level, "highly_ambiguous")
  expect_true(r$design_ambiguous)
  expect_equal(r$status, "NOTE")
})

test_that("a genuine cross-family fallback keeps its cross-family tag", {
  # control: variants WERE computed (F(2,30)), the reported d has no same-type
  # variant, so the matcher really does cross-fall -- tag must survive.
  r <- as.data.frame(check_text("F(2, 30) = 5.00, p = .013, d = 0.60"))
  expect_equal(nrow(r), 1L)
  expect_true(grepl("[category: cross-family]", r$ambiguity_reason, fixed = TRUE))
  expect_true(grepl("No same-type", r$ambiguity_reason, fixed = TRUE))
  expect_false(grepl("[category: not-computed]", r$ambiguity_reason, fixed = TRUE))
})
