# v0.6.8: some PDFs encode the "=" glyph such that the text layer emits U+00BC
# ("¼", the fraction one-quarter). Whole papers come through with EVERY equals
# sign as U+00BC and no real "=" at all, so `t ¼ -7.81`, `F (3, 1791) ¼
# 200.12`, `d ¼ 0.57` parse to nothing. normalize_text() folds U+00BC to "="
# ONLY in a statistical-operator position (flanked by whitespace, adjacent to a
# value / sign / bracket / a stat-word), so a genuine one-quarter fraction in prose
# is NOT rewritten.
#
# Surfaced by the 2026-06-29 escicheck-iterate run against
# 10.1177/1948550619900570 (SPPS "Inaction Inertia" replications): 120 U+00BC, zero
# real "=" -> 0 body-prose statistics extracted before the fix.

u00bc <- intToUtf8(0x00BC)  # U+00BC (fraction one-quarter); built from the code point so package R code stays ASCII

test_that("U+00BC standing in for '=' is folded in a statistical context", {
  txt <- paste0("the pairwise comparison was significant (d ", u00bc, " -0.78, 95% CI ",
                u00bc, " [-0.99, -0.58], t ", u00bc, " -7.81, p < .001)")
  res <- effectcheck::check_text(txt)
  tr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - (-7.81)) < 1e-9, ]
  expect_true(nrow(tr) >= 1)
  expect_equal(as.numeric(tr$stat_value[1]), -7.81)
})

test_that("an F-test written with U+00BC for '=' is extracted", {
  txt <- paste0("The scenario main effect was significant (F (3, 1791) ", u00bc,
                " 200.12, p < .001).")
  res <- effectcheck::check_text(txt)
  fr <- res[!is.na(res$test_type) & res$test_type == "F" &
              !is.na(res$stat_value) & abs(res$stat_value - 200.12) < 1e-6, ]
  expect_true(nrow(fr) >= 1)
  expect_equal(as.numeric(fr$df1[1]), 3)
  expect_equal(as.numeric(fr$df2[1]), 1791)
})

test_that("a genuine one-quarter fraction is NOT rewritten to '='", {
  for (txt in c(
    paste0("Add ", u00bc, " cup of sugar to the mix."),
    paste0("About ", u00bc, " of participants declined to continue.")
  )) {
    n <- effectcheck:::normalize_text(txt)
    # The fraction stays; no spurious "=" is introduced.
    expect_false(grepl("=", n, fixed = TRUE))
    expect_true(grepl(u00bc, n, fixed = TRUE))
  }
})
