# v0.7.5 -- `infer_numeric_locale()` returns three outcomes and the rules that
# gate on it only ever tested for two.
#
# The function sets `decimal_mark = NA` for BOTH "none" (no evidence either way)
# and "conflict" (both conventions attested, neither dominant). Three separate
# rules asked `identical(decimal_mark, ",")`, so a conflicted document was
# normalized as if it were decisively US:
#
#   .apply_thousands_protect()                      parse.R -- rule T1
#   the test-statistic paren pre-strip loop         parse.R -- the E8 repair
#   D1's `.amb_shape` exclusion                     parse.R -- rule D1
#
# The third is the one the v0.7.3 cross-model audit had already fixed for the
# decisively-European case -- and it was fixed with a fourth hand-written copy of
# the same test, which is why the conflict state slipped past all three. They now
# share `.locale_comma_unresolved()`.
#
# REPRODUCED AT HEAD before the fix. Both halves matter: with T1 alone stepping
# aside, D1 would still convert and "leave it alone" becomes "call it a European
# decimal", which is just the other unfounded guess.
#
# 0 of the 48 validation-corpus papers infer `conflict`, and the whole-corpus
# diff for this change is empty (measured, 764 rows, 0 lost / 0 gained /
# 0 changed). That emptiness is the evidence the change is safe -- it is not a
# reason the change was skippable. A computed signal nothing reads is a trap:
# it reads as handled at every site that mentions the locale.

conflict_doc <- paste(
  "In Study 1 the effect was reliable, t(58) = 2.41, p = .035, d = 0.80.",
  "In Study 2 Welch's correction gave t(2,758) = 3,21, d = 0,45.",
  sep = "\n")

test_that("v0.7.5: a two-sided document is reported as conflict", {
  loc <- effectcheck:::infer_numeric_locale(conflict_doc)
  expect_equal(loc$confidence, "conflict")
  expect_true(is.na(loc$decimal_mark))
})

test_that("v0.7.5: the three locale gates agree via one predicate", {
  expect_true(effectcheck:::.locale_comma_unresolved(
    list(decimal_mark = NA_character_, confidence = "conflict")))
  expect_true(effectcheck:::.locale_comma_unresolved(
    list(decimal_mark = ",", confidence = "decisive")))
  # "none" is NOT unresolved: with no evidence at all, the US-style thousands
  # default is the right prior and must keep applying.
  expect_false(effectcheck:::.locale_comma_unresolved(
    list(decimal_mark = NA_character_, confidence = "none")))
  expect_false(effectcheck:::.locale_comma_unresolved(
    list(decimal_mark = ".", confidence = "decisive")))
  expect_false(effectcheck:::.locale_comma_unresolved(NULL))
})

test_that("v0.7.5: a conflicted document does not fabricate a computed effect", {
  # BEFORE: df1 = 2758, N = 2760, matched_value = 0.1222 against a reported
  # 0.45, status WARN -- a false accusation carrying a fabricated effect size.
  # AFTER: the ambiguous token survives verbatim, so nothing is computed and the
  # row says so. This is the package's stated "graceful failure" principle:
  # report "cannot verify" rather than a meaningless computed value.
  r <- check_text(conflict_doc)
  welch <- r[!is.na(r$stat_value) & abs(r$stat_value - 3.21) < 1e-9, ]
  expect_equal(nrow(welch), 1L)
  expect_true(is.na(welch$matched_value[1]))
  expect_equal(welch$status[1], "NOTE")
  expect_false(identical(welch$df1[1], 2758))
})

test_that("v0.7.5: conflict lands on the SAME outcome as decisive-European", {
  # The equivalence is the point: "unknown" must behave like the branch that
  # knows the comma is a decimal mark, not like the branch that knows it is a
  # thousands separator.
  eu_doc <- paste(
    "In Studie 1 war der Effekt zuverlassig, t(58) = 2,41, p = ,035, d = 0,80.",
    "In Studie 2 ergab die Welch-Korrektur t(2,758) = 3,21, d = 0,45.",
    sep = "\n")
  eu <- check_text(eu_doc)
  cf <- check_text(conflict_doc)
  eu_row <- eu[!is.na(eu$stat_value) & abs(eu$stat_value - 3.21) < 1e-9, ]
  cf_row <- cf[!is.na(cf$stat_value) & abs(cf$stat_value - 3.21) < 1e-9, ]
  expect_equal(cf_row$status[1], eu_row$status[1])
  expect_equal(cf_row$df1[1], eu_row$df1[1])
  expect_equal(is.na(cf_row$matched_value[1]), is.na(eu_row$matched_value[1]))
})

test_that("v0.7.5: a decisively US document is untouched", {
  # The guard must cost nothing where the locale IS established. "N = 2,758" in
  # a US document is a grouped integer and must keep being stripped.
  us_doc <- paste(
    "In Study 1 the effect was reliable, t(58) = 2.41, p = .035, d = 0.80.",
    "The full sample was N = 2,758 and the second effect was d = 0.45.",
    sep = "\n")
  expect_match(effectcheck:::normalize_text(us_doc), "N = 2758", fixed = TRUE)
  # ... and a document with no separator evidence at all keeps the same default.
  expect_match(effectcheck:::normalize_text("The sample was N = 1,182 overall."),
               "N = 1182", fixed = TRUE)
})
