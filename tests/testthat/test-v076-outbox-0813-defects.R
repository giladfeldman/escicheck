# v0.7.6 -- the six defects docpluck filed against effectcheck 0.7.5 in
# OUTBOX_TO_ESCIMATE_2026-08-13.md, plus the false negative in the guard they
# were about to adopt from us.
#
# ALL SEVEN WERE REPRODUCED AT HEAD on 2026-08-21 before any fix was written;
# each block records the unfixed output. docpluck reproduced them against our
# source, but a reviewer finding is a hypothesis until it is reproduced locally,
# and every one of these was.

nt <- function(s) effectcheck:::normalize_text(s)

# --- D1 -------------------------------------------------------------------
# Unfixed: "Patients with AF(6, 7), were excluded".
# An abbreviation ending in F plus a CITATION bracket, rewritten into F-test
# notation. A consumer can then report an F(6, 7) that is in no paper.
test_that("v0.7.6: a word ending in F does not turn a citation bracket into an F-test", {
  expect_equal(nt("Patients with AF [6, 7], were excluded from the analysis."),
               "Patients with AF [6, 7], were excluded from the analysis.")
  expect_false(grepl("AF(6, 7)", nt("Patients with AF [6, 7], were excluded."), fixed = TRUE))
})

test_that("v0.7.6: a genuine bracketed F-test still normalises", {
  # The guard must cost no coverage -- this is the shape the rule exists for.
  expect_match(nt("The effect was reliable, F[1,30] = 8.33, p = .007."),
               "F(1, 30) = 8.33", fixed = TRUE)
  expect_match(nt("Results showed F [2, 42] = 3.10, p = .05."),
               "F(2, 42) = 3.10", fixed = TRUE)
})

# --- D2 -------------------------------------------------------------------
# Unfixed: "Third-plus generation had lower rates ..." -- the leading 90.6 was
# DELETED WITH NO RESIDUE. docpluck reports 0.01/0.00/0.03 lost the same way in
# that paper. Our own LESSONS calls this the worst defect class we have.
test_that("v0.7.6: the outline stripper does not delete a table value", {
  out <- nt("90.6 Third-plus generation had lower rates of reported discrimination.")
  expect_match(out, "90.6", fixed = TRUE)
  # A leading-zero value can never be a section number.
  expect_match(nt("0.01 Third-plus generation had lower rates of discrimination."),
               "0.01", fixed = TRUE)
})

test_that("v0.7.6: a real section heading is still stripped", {
  # The rule exists for "d =\n3.3 Discussion"; that must keep working, or the
  # fix has traded one silent fabrication for another.
  expect_false(grepl("3.3", nt("The effect was large, d =\n3.3 Discussion"), fixed = TRUE))
  expect_false(grepl("4.2", nt("Text ended here.\n4.2 Results\nNext paragraph."), fixed = TRUE))
})

# --- D3 -------------------------------------------------------------------
# Unfixed: "v2.1451.52." -- the full-notation rule read "1.451,52" as a grouped
# European number and rewrote a version identifier into garbage.
test_that("v0.7.6: a software version string is not fused with a citation run", {
  expect_equal(nt("Analyses used software v2.1.451,52."),
               "Analyses used software v2.1.451,52.")
})

# --- D4 -------------------------------------------------------------------
# Unfixed: "9999999.1". The SPEC calls one digit before the comma its most
# important constraint; the code allowed `(\d+)`. Neither extreme is right --
# see the note at the D1 pattern.
test_that("v0.7.6: an implausibly long integer part is not read as a decimal comma", {
  expect_equal(nt("The value was 9999999,1 in that condition."),
               "The value was 9999999,1 in that condition.")
})

test_that("v0.7.6: real continental decimals with multi-digit integer parts still convert", {
  # The one-digit SPEC rule would have broken all of these, which is why the
  # cap is four digits rather than one.
  expect_match(nt("The mean was 12,34 overall."), "12.34", fixed = TRUE)
  expect_match(nt("The statistic was 1234,56 overall."), "1234.56", fixed = TRUE)
  expect_match(nt("The effect was 0,45 overall."), "0.45", fixed = TRUE)
})

# --- D5 -------------------------------------------------------------------
# Unfixed: "Frank 1.2". A two-element affiliation run separated from the name
# by a space fell through both the glued-form lookbehind and the 3+-chain rule.
test_that("v0.7.6: a space-separated author affiliation run is not read as a decimal", {
  expect_equal(nt("Erik. T. Frank 1,2 , Lucie Kesner 3 and others."),
               "Erik. T. Frank 1,2 , Lucie Kesner 3 and others.")
})

test_that("v0.7.6: the affiliation guard does not swallow a real decimal", {
  # The space before the separating comma is load-bearing: a first draft used
  # `\s*,` and therefore also protected "Median 0,45, SD 0,12", suppressing a
  # genuine European decimal. Found by testing, not by reading the pattern.
  expect_match(nt("Values were Median 0,45, SD 0,12."), "0.45", fixed = TRUE)
  expect_match(nt("Values were Median 0,45, SD 0,12."), "0.12", fixed = TRUE)
  expect_match(nt("The Median 0,45 and the mean were reported."), "0.45", fixed = TRUE)
})

# --- D6 -------------------------------------------------------------------
# Unfixed: "%6.28" -- a percentage followed by citation superscripts read as a
# decimal, because `%` was missing from D1's lookbehind exclusions.
test_that("v0.7.6: a citation run after a percent sign is not read as a decimal", {
  expect_equal(nt("The rate was ~25%6,28 in that sample."),
               "The rate was ~25%6,28 in that sample.")
})

# --- D7 -------------------------------------------------------------------
# Unfixed: "Participants scored 0,87" was PRESERVED while "Participants had
# 0,87" converted -- docpluck read this as a false negative in the
# coded/dummy/scored vocabulary and was about to adopt that vocabulary because
# of it. The vocabulary was never the cause: each element of the protected run
# is a single `\d`, so the pattern matched the PREFIX "0,8".
test_that("v0.7.6: a continental score after 'scored' converts like any other", {
  expect_match(nt("Participants scored 0,87 on average."), "0.87", fixed = TRUE)
  # The control docpluck contrasted it against, which always worked.
  expect_match(nt("Participants had 0,87 on average."), "0.87", fixed = TRUE)
})

test_that("v0.7.6: a genuine dummy-coding list is still protected", {
  # This is what the vocabulary is FOR -- "coded 0,1" is two levels, not 0.1.
  expect_match(nt("Gender was coded 0,1 in the model."), "coded 0,1", fixed = TRUE)
  expect_match(nt("Condition was dummy coded 0,1,2 throughout."), "0,1,2", fixed = TRUE)
})
