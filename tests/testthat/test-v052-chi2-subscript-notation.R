# v0.5.2 -- subscripted chi-square notation (JASP-style "chi2gof", "chi2Pearson")
#
# escicheck-iterate cycle 1 (2026-05-17) found that a chi-square written with a
# subscript label glued to the symbol -- the JASP output form chi2gof(df) and
# chi2Pearson(df) -- returned 0 stats. parse.R's pat_chi required the open paren
# to follow the chi token immediately, so a "gof" / "Pearson" word between the
# token and "(" blocked the match. 7 of the 12 chi-square results in the gold
# for 10.1098/rsos.250367 (a JASP-reported PCI-RR paper) were invisible.
#
# Fix: an optional non-capturing subscript group (chi_sub) -- an allowlist of the
# known chi-square subscript labels (gof / Pearson / Yates / LR / MH / Wald) --
# inserted into pat_chi, pat_chi_nodf and pat_chi_two_dfs. The verbatims below
# are the exact gold strings; the guards prove the allowlist stays bounded.

# zero-stat check_text() output is a tibble with no test_type column
chisq_of <- function(res) {
  if (!"test_type" %in% names(res)) return(data.frame())
  res[!is.na(res$test_type) & res$test_type == "chisq", , drop = FALSE]
}

test_that("v0.5.2: JASP-subscripted chi-square is parsed (real rsos.250367 verbatims)", {
  cases <- list(
    list(vq = "χ²gof(2) = 225.954, p = 8.602e-50, ĈPearson = 0.505, CI95% [0.453, 0.551], nobs = 659", stat = 225.954, df = 2),
    list(vq = "χ²Pearson(1) = 1.590, p = 0.207, V̂Cramer = 0.030, CI95% [0.000, 0.119], nobs = 659",   stat = 1.590,   df = 1),
    list(vq = "χ²gof(1) = 31.01, p = 2.572e-08, n = 329", stat = 31.01, df = 1),
    list(vq = "χ²gof(1) = 14.85, p = 1.165e-04, n = 330", stat = 14.85, df = 1),
    list(vq = "χ²Pearson(1) = 6.773, p = 0.009, V̂Cramer = 0.094, CI95% [0.000, 0.174], nobs = 659",   stat = 6.773,   df = 1),
    list(vq = "χ²gof(1) = 49.95, p = 1.576e-12, n = 328", stat = 49.95, df = 1),
    list(vq = "χ²gof(1) = 12.76, p = 3.533e-04, n = 331", stat = 12.76, df = 1)
  )
  for (cs in cases) {
    res <- check_text(cs$vq)
    chisq <- chisq_of(res)
    expect_true(nrow(chisq) >= 1, info = paste("no chisq row for:", cs$vq))
    expect_true(any(abs(chisq$stat_value - cs$stat) < 0.01),
                info = paste("stat_value mismatch for:", cs$vq))
    expect_true(any(chisq$df1 == cs$df, na.rm = TRUE),
                info = paste("df1 mismatch for:", cs$vq))
  }
})

test_that("v0.5.2: multiple subscripted chi-squares in one text each get a row", {
  # The sub-chunk splitter (parse.R) must recognise the subscripted form too --
  # otherwise a paragraph of JASP-style chi-squares collapses into one result
  # row. The one-statement-per-quote verbatim harness is blind to this; a worker
  # /process-text round-trip on the 7 verbatims caught it (cycle 1, Tier 2).
  blob <- paste(c(
    "χ²gof(2) = 225.954, p = 8.602e-50, nobs = 659.",
    "χ²Pearson(1) = 1.590, p = 0.207, nobs = 659.",
    "χ²gof(1) = 31.01, p = 2.572e-08, n = 329.",
    "χ²gof(1) = 14.85, p = 1.165e-04, n = 330.",
    "χ²Pearson(1) = 6.773, p = 0.009, nobs = 659.",
    "χ²gof(1) = 49.95, p = 1.576e-12, n = 328.",
    "χ²gof(1) = 12.76, p = 3.533e-04, n = 331."
  ), collapse = "\n")
  chisq <- chisq_of(check_text(blob))
  expect_equal(nrow(chisq), 7)
})

test_that("v0.5.2 guard: plain chi-square (no subscript) still parses", {
  res <- check_text("chi2(2, N = 659) = 225.95, p < .001, w = 0.50")
  chisq <- chisq_of(res)
  expect_equal(nrow(chisq), 1)
  expect_equal(chisq$stat_value[1], 225.95, tolerance = 1e-6)
  expect_equal(chisq$df1[1], 2)
})

test_that("v0.5.2 guard: an unknown word after the chi token does not falsely match", {
  # "xyz" is not an allowlisted subscript -- the optional group is bounded, not a
  # greedy [A-Za-z]*, so a chi token glued to an unknown word stays unparsed
  # (conservative: do not invent a chi-square from an unrecognised form).
  res <- check_text("The estimate chi2xyz(2) = 5.0, p = .08 was reported.")
  expect_equal(nrow(chisq_of(res)), 0)
})
