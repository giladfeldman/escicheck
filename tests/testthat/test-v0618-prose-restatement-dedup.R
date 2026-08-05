# v0.6.18 -- prose-vs-prose restatement dedup: ATTEMPTED, NOT SHIPPED.
#
# Source: 2026-08-05 escicheck-iterate cycle-2 Sonnet canary audit of
# pci.rr.100726. The finding is REAL and stays OPEN; what is recorded here is
# why the obvious fix cannot be shipped safely, so a later cycle does not
# re-derive the same dead end.
#
# THE FINDING
#
# pci.rr.100726 is a peer-review letter whose Reviewer #2 comment quotes ONE
# t-test twice to illustrate APA comma placement:
#
#     "... and t(868) = -3.01, p = .006 for the gender ..."   should be
#     "... and, t(868) = -3.01, p = .006, for the gender ..."
#
# effectcheck emits TWO fully scored rows. The gold states plainly: "The same
# statistic appears twice in the comment (incorrect vs. corrected comma
# example); it is a single reported test." Any per-paper count built on that
# render is inflated 2x for this statistic.
#
# WHY IT IS NOT FIXED HERE
#
# Three successive rules were built and each was disproved by an existing test
# or by the real corpus:
#
# 1. **Full reported signature** (type + stat + df1 + df2 + p + effect +
#    effect_name + ciL + ciU + N). Broke 8 blocks across test-v0514, test-v063
#    and test-v0614: v0.6.14 documents collabra.23443 reporting FOUR
#    correlations in two sentences where H2A-paid r(797) = .16 and H2C-unpaid
#    r(797) = .16 are DIFFERENT correlations sharing every reported number and
#    carrying no CI. Collapsing them DROPS a real result.
#
# 2. **Signature + positional adjacency** (gap <= 2 parse locations). Still
#    collapsed v0.6.14's minimal reproducer, which puts both distinct
#    correlations in ONE sentence.
#
# 3. **Signature + adjacency + "the duplicate is not independently anchored by
#    its own parenthesized stat(df) form"**. This satisfied every prior test --
#    and then failed on the real paper, because the pci.rr.100726 echo IS
#    independently anchored (`t(868) = -3.01, p = .006, for the gender`). The
#    guard that protects the correlations disables the fix it was written for.
#
# The two cases are not separable from row content: a genuine restatement and
# two genuinely distinct results can present identically. Separating them needs
# a signal this layer does not have -- e.g. docpluck marking a span as quoted
# material, or a section classifier that knows a reviewer-comment block is not
# a results section.
#
# DECISION: keep the duplicate rather than risk dropping a real finding. A
# duplicate row is a counting error the reader can see; a dropped row is a lost
# result they cannot. The finding stays OPEN in run-meta and in
# docs/TRIAGE_iterate_2026-08-04.md.
#
# The tests below pin the INVARIANTS that any future fix must not break. They
# pass against current behaviour (no dedup) and would also pass against a
# correct future fix -- they are the guard rails, not a spec for the missing
# feature.

test_that("two genuinely different t-tests are both kept", {
  txt <- paste0(
    "The first contrast was significant, t(100) = 2.51, p = .014, ",
    "and the second was not, t(100) = 0.87, p = .386."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(rows), 2)
})

test_that("the same statistic on different df is kept", {
  txt <- paste0(
    "Study 1 showed an effect, t(48) = 2.31, p = .025, ",
    "and Study 2 replicated it, t(96) = 2.31, p = .023."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(rows), 2)
})

test_that("the same statistic reported with different p-values is kept", {
  # Differing p on an identical statistic is itself a discrepancy worth
  # surfacing -- any future dedup must NOT merge it away.
  txt <- paste0(
    "The analysis gave t(100) = 2.51, p = .014 in the text, ",
    "while the abstract reported t(100) = 2.51, p = .002."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(rows), 2)
})

test_that("two distinct same-valued correlations in one sentence are both kept", {
  # The v0.6.14 invariant that killed attempts 1 and 2 above. Pinned here so a
  # future dedup attempt meets it immediately rather than after a full suite run.
  txt <- paste0(
    "communal in the paid, r(797) = .16, p < .001, and unpaid conditions, ",
    "r(797) = .16, p < .001 (H2C; Pearson's r)."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_equal(nrow(rr), 2L)
})
