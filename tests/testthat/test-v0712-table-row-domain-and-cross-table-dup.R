# v0.7.12: two defects in how docpluck's flattened table rows became results.
#
# Measured 2026-09-24/25 against a clean local docpluck 2.4.143 and 2.4.144 (the
# version production serves) on worker/tests/fixtures/docpluck-contract/
# contract-structure.pdf. Its Table 2 is [Contrast, t, df, p, d] with rows
# `Sleep vs Control 2.41 98 .018 0.15` and `Sleep vs Nap -3.07 98 .003 -0.31`.
# docpluck attached those rows to Table 1's ruled box, under Table 1's
# [F, df1, df2] header, and effectcheck 0.7.11 published:
#   * F(0.018, 0.15) = 98 and F(0.003, -0.31) = 98 as SKIP rows -- and SKIP does
#     not suppress values, so both impossible statistics reached the user;
#   * Table 1's rows twice, once under the label "Table 2".
# The flattened rows below are the ones docpluck returned, field for field.

fr <- function(table_id, label, row_label, fields, page = 1L) {
  list(table_id = table_id, page = page, label = label, row_idx = 0L,
       row_label = row_label, fields = fields)
}
f_row <- function(F, df1, df2, p, p_op = "=") {
  list(F = F, df1 = df1, df2 = df2, p = p, p_op = p_op)
}

docpluck_2_4_144_rows <- list(
  fr("t1", "Table 1", "Condition (main effect)", f_row(5.2, 1, 98, 0.025)),
  fr("t1", "Table 1", "Time (main effect)", f_row(12.5, 2, 196, 0.001, "<")),
  fr("t1", "Table 1", "Condition x Time", f_row(0.45, 2, 196, 0.64)),
  fr("camelot_t0", "Table 2", "Condition (main effect)", f_row(5.2, 1, 98, 0.025)),
  fr("camelot_t0", "Table 2", "Time (main effect)", f_row(12.5, 2, 196, 0.001, "<")),
  fr("camelot_t0", "Table 2", "Condition x Time", f_row(0.45, 2, 196, 0.64)),
  fr("camelot_t0", "Table 2", "Contrastt", list()),
  fr("camelot_t0", "Table 2", "Sleep vs Control 2.41", list(F = 98, df1 = 0.018, df2 = 0.15)),
  fr("camelot_t0", "Table 2", "Sleep vs Nap-3.07", list(F = 98, df1 = 0.003, df2 = -0.31))
)

text_only <- "Participants completed the task."

test_that("a table row typed as an impossible F publishes no numbers", {
  res <- as.data.frame(check_text(text_only, table_rows = docpluck_2_4_144_rows))
  bad <- res[grepl("Sleep vs", res$raw_text), ]
  expect_equal(nrow(bad), 2L)                 # kept, so the gap is visible
  expect_true(all(bad$df_guard_rejected))
  expect_true(all(bad$extraction_suspect))
  expect_true(all(bad$status == "NOTE"))
  expect_true(all(bad$check_scope == "extraction_only"))
  expect_true(all(is.na(bad$stat_value)))
  expect_true(all(is.na(bad$df1)))
  expect_true(all(is.na(bad$df2)))
  expect_true(all(is.na(bad$p_reported)))
  expect_true(all(bad$all_variants == "{}"))
  expect_true(all(grepl("TABLE ROW REFUSED", bad$df_guard_reason)))
  expect_true(all(bad$uncertainty_reasons == bad$df_guard_reason))
  expect_match(bad$df_guard_reason[1], "numerator df 0.018 is below 1", fixed = TRUE)
  expect_match(bad$df_guard_reason[2], "denominator df -0.31 is not positive", fixed = TRUE)
  # No admissible row is touched.
  ok <- res[!grepl("Sleep vs", res$raw_text), ]
  expect_false(any(ok$df_guard_rejected))
})

test_that("one table detected twice is checked once, under its first label", {
  res <- as.data.frame(check_text(text_only, table_rows = docpluck_2_4_144_rows))
  f_rows <- res[!res$df_guard_rejected, ]
  expect_equal(nrow(f_rows), 3L)
  expect_setequal(f_rows$stat_value, c(5.2, 12.5, 0.45))
  expect_true(all(grepl("^Table 1: ", f_rows$raw_text)))
})

test_that("the domain guard never fires on legitimately fractional df", {
  legit <- list(
    # Greenhouse-Geisser corrected repeated-measures F
    fr("t1", "Table 3", "Time", f_row(8.41, 1.73, 169.5, 0.001, "<")),
    # Kenward-Roger / Satterthwaite mixed-model F
    fr("t1", "Table 3", "Group", f_row(4.02, 1, 45.3, 0.051)),
    # Welch t
    fr("t2", "Table 4", "A vs B", list(t = 2.10, df = 97.3, p = 0.038))
  )
  res <- as.data.frame(check_text(text_only, table_rows = legit))
  expect_equal(nrow(res), 3L)
  expect_false(any(res$df_guard_rejected))
  expect_false(any(is.na(res$stat_value)))
})

test_that("each domain boundary refuses, and only across the line", {
  guard <- effectcheck:::.table_row_domain_violation
  expect_false(is.na(guard("F", 3, 0.99, 20)))
  expect_true(is.na(guard("F", 3, 1, 20)))
  expect_false(is.na(guard("F", 3, 2, 0)))
  expect_true(is.na(guard("F", 3, 2, 0.5)))
  expect_false(is.na(guard("F", -0.2, 2, 20)))
  expect_true(is.na(guard("F", 0, 2, 20)))
  expect_true(is.na(guard("t", 2.1, 0.5, NA)))   # Satterthwaite t may print df < 1
  expect_true(is.na(guard("t", -2.1, 1, NA)))
  expect_true(is.na(guard("r", 0.3, NA, NA)))
  expect_true(is.na(guard("F", 3, NA, NA)))     # missing df is not a violation
})

test_that("cross-table dedup keeps rows unless the whole signature holds", {
  dup <- effectcheck:::.table_row_cross_table_duplicates
  a1 <- fr("t1", "Table 1", "Age", f_row(1.0, 1, 98, 0.32))
  a2 <- fr("t1", "Table 1", "Sex", f_row(2.5, 1, 98, 0.12))
  b1 <- fr("t2", "Table 2", "Age", f_row(1.0, 1, 98, 0.32))
  b2 <- fr("t2", "Table 2", "Sex", f_row(2.5, 1, 98, 0.12))
  # The detected-twice shape: two rows of t2 repeat two rows of t1.
  expect_equal(dup(list(a1, a2, b1, b2)), c(FALSE, FALSE, TRUE, TRUE))
  # Only ONE row repeats -> may be coincidence across two real tables: kept.
  b2x <- fr("t2", "Table 2", "Sex", f_row(3.9, 1, 98, 0.05))
  expect_equal(dup(list(a1, a2, b1, b2x)), rep(FALSE, 4))
  # Same rows on another page -> a restatement, not a double detection: kept.
  b1p <- b1; b1p$page <- 7L
  b2p <- b2; b2p$page <- 7L
  expect_equal(dup(list(a1, a2, b1p, b2p)), rep(FALSE, 4))
  # Same table_id repeating itself -> not this rule's business: kept.
  expect_equal(dup(list(a1, a2, a1, a2)), rep(FALSE, 4))
  # No table_id -> cannot tell tables apart: kept.
  strip <- function(r) { r$table_id <- NULL; r }
  expect_equal(dup(lapply(list(a1, a2, b1, b2), strip)), rep(FALSE, 4))
  # A differing p (or p_op, or group) is a different result: kept.
  b1q <- fr("t2", "Table 2", "Age", f_row(1.0, 1, 98, 0.32, "<"))
  b2q <- fr("t2", "Table 2", "Sex", f_row(2.5, 1, 98, 0.13))
  expect_equal(dup(list(a1, a2, b1q, b2q)), rep(FALSE, 4))
  # A row with a single number cannot match on coincidence.
  e1 <- fr("t1", "Table 1", "x", list(F = 3)); e2 <- fr("t1", "Table 1", "y", list(F = 4))
  e3 <- fr("t2", "Table 2", "x", list(F = 3)); e4 <- fr("t2", "Table 2", "y", list(F = 4))
  expect_equal(dup(list(e1, e2, e3, e4)), rep(FALSE, 4))
})

test_that("single matches on DIFFERENT pages never pool into a >= 2 block", {
  # Found by the Sonnet consult seat, 2026-09-25: the >= 2-row threshold grouped
  # matches by (later table, earlier table) only, so when table ids repeat
  # across pages, one coincidental match on page 1 and another on page 5 --
  # each meant to be kept -- counted as a block of two and were both dropped.
  dup <- effectcheck:::.table_row_cross_table_duplicates
  a1 <- fr("t1", "Table 1", "Age", f_row(1.0, 1, 98, 0.32), page = 1L)
  a2 <- fr("t1", "Table 1", "Sex", f_row(2.5, 1, 98, 0.12), page = 1L)
  b1 <- fr("t2", "Table 2", "Age", f_row(1.0, 1, 98, 0.32), page = 1L)
  b1o <- fr("t2", "Table 2", "Other", f_row(7.7, 1, 98, 0.01), page = 1L)
  c1 <- fr("t1", "Table 5", "Sex", f_row(2.5, 1, 98, 0.12), page = 5L)
  c1o <- fr("t1", "Table 5", "More", f_row(8.8, 1, 98, 0.01), page = 5L)
  c2 <- fr("t2", "Table 6", "Sex", f_row(2.5, 1, 98, 0.12), page = 5L)
  c2o <- fr("t2", "Table 6", "Else", f_row(9.9, 1, 98, 0.01), page = 5L)
  expect_equal(dup(list(a1, a2, b1, b1o, c1, c1o, c2, c2o)), rep(FALSE, 8))
})

test_that("three copies, numeric vs string page, and an NA test type", {
  dup <- effectcheck:::.table_row_cross_table_duplicates
  a1 <- fr("t1", "Table 1", "Age", f_row(1.0, 1, 98, 0.32), page = 2L)
  a2 <- fr("t1", "Table 1", "Sex", f_row(2.5, 1, 98, 0.12), page = 2L)
  b1 <- fr("t2", "Table 2", "Age", f_row(1.0, 1, 98, 0.32), page = "2")
  b2 <- fr("t2", "Table 2", "Sex", f_row(2.5, 1, 98, 0.12), page = "2")
  c1 <- fr("t3", "Table 3", "Age", f_row(1.0, 1, 98, 0.32), page = 2)
  c2 <- fr("t3", "Table 3", "Sex", f_row(2.5, 1, 98, 0.12), page = 2)
  expect_equal(dup(list(a1, a2, b1, b2, c1, c2)), c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
  expect_true(is.na(effectcheck:::.table_row_domain_violation(NA_character_, 98, 0.01, 0.1)))
})

test_that("t: df in (0, 1) is admissible, df <= 0 is refused, table and prose alike", {
  # Sonnet (2026-09-25) found the prose guard covered F but not t; Sol (same
  # day) then showed the t line itself was wrong: a mixed-model Satterthwaite
  # contrast can print a df in (0, 1). So the t domain is df > 0, and prose
  # needs no new branch -- the existing df1 <= 0 flag already is that rule.
  guard <- effectcheck:::.table_row_domain_violation
  expect_true(is.na(guard("t", 2.1, 0.5, NA)))
  expect_false(is.na(guard("t", 2.1, 0, NA)))
  expect_false(is.na(guard("t", 2.1, -3, NA)))
  tbl <- list(fr("t9", "Table 9", "Contrast A", list(t = 2.10, df = 0, p = 0.04)),
              fr("t9", "Table 9", "Contrast B", list(t = 2.10, df = 0.6, p = 0.04)))
  res <- as.data.frame(check_text(text_only, table_rows = tbl))
  expect_equal(res$df_guard_rejected, c(TRUE, FALSE))
  expect_true(is.na(res$stat_value[1]))
  expect_equal(res$stat_value[2], 2.10)
  prose <- as.data.frame(check_text("The difference was t(0.5) = 2.10, p = .04."))
  expect_false(grepl("IMPOSSIBLE DF", prose$uncertainty_reasons, fixed = TRUE))
})

test_that("prose F with a non-positive denominator df is flagged, values visible", {
  res <- suppressWarnings(as.data.frame(check_text("The effect was F(2, 0) = 3.20, p = .04.")))
  expect_equal(nrow(res), 1L)
  expect_true(res$extraction_suspect)
  expect_match(res$uncertainty_reasons, "IMPOSSIBLE DF: an F test's denominator df", fixed = TRUE)
  expect_equal(res$df2, 0)
  expect_false(res$df_guard_rejected)
  # df1 <= 0 keeps the long-standing message, now printing the value as read.
  z <- suppressWarnings(as.data.frame(check_text("The effect was F(0, 20) = 3.20, p = .04.")))
  expect_match(z$uncertainty_reasons, "df1 = 0 appears implausible", fixed = TRUE)
})

test_that("containment: genuine tables that agree on SOME rows are kept", {
  # Sol consult seat, 2026-09-25: two real tables on one page (age- and
  # sex-adjusted models) can agree on two rounded rows. They must not be
  # merged unless the earlier table reappears in full.
  dup <- effectcheck:::.table_row_cross_table_duplicates
  a1 <- fr("t1", "Table 1", "Anxiety", f_row(6.1, 1, 198, 0.014))
  a2 <- fr("t1", "Table 1", "Depression", f_row(4.4, 1, 198, 0.037))
  a3 <- fr("t1", "Table 1", "Stress", f_row(1.2, 1, 198, 0.27))
  b1 <- fr("t2", "Table 2", "Anxiety", f_row(6.1, 1, 198, 0.014))
  b2 <- fr("t2", "Table 2", "Depression", f_row(4.4, 1, 198, 0.037))
  b3 <- fr("t2", "Table 2", "Stress", f_row(2.9, 1, 198, 0.09))
  expect_equal(dup(list(a1, a2, a3, b1, b2, b3)), rep(FALSE, 6))
  # Two copies of ONE earlier row are not two rows of containment.
  c1 <- fr("t3", "Table 3", "Anxiety", f_row(6.1, 1, 198, 0.014))
  expect_equal(dup(list(a1, a2, c1, c1)), rep(FALSE, 4))
  # Missing pages are no evidence of a shared page.
  nopage <- function(r) { r$page <- NULL; r }
  expect_equal(dup(lapply(list(a1, a2, b1, b2), nopage)), rep(FALSE, 4))
})

test_that("containment: a partial third copy is caught through the table it copies", {
  # Sol consult seat, 2026-09-25: A = {Age}; B = {Age, Sex}; C = {Age, Sex}.
  # The first draft assigned C's Age to source A and C's Sex to source B, so
  # neither pair reached two and C was kept.
  dup <- effectcheck:::.table_row_cross_table_duplicates
  a <- fr("tA", "Table 1", "Age", f_row(1.0, 1, 98, 0.32))
  b1 <- fr("tB", "Table 2", "Age", f_row(1.0, 1, 98, 0.32))
  b2 <- fr("tB", "Table 2", "Sex", f_row(2.5, 1, 98, 0.12))
  c1 <- fr("tC", "Table 3", "Age", f_row(1.0, 1, 98, 0.32))
  c2 <- fr("tC", "Table 3", "Sex", f_row(2.5, 1, 98, 0.12))
  expect_equal(dup(list(a, b1, b2, c1, c2)), c(FALSE, FALSE, FALSE, TRUE, TRUE))
})

test_that("an impossible F in PROSE is flagged but stays visible", {
  res <- as.data.frame(check_text("The effect was F(0.5, 20) = 3.20, p = .04."))
  expect_equal(nrow(res), 1L)
  expect_true(res$extraction_suspect)
  expect_equal(res$df1, 0.5)                    # the reader can see it
  expect_match(res$uncertainty_reasons, "IMPOSSIBLE DF", fixed = TRUE)
  expect_false(res$df_guard_rejected)           # not withheld: prose, not table
  ok <- as.data.frame(check_text("The effect was F(1.73, 169.5) = 8.41, p < .001."))
  expect_false(grepl("IMPOSSIBLE DF", ok$uncertainty_reasons, fixed = TRUE))
})
