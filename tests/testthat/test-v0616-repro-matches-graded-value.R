# v0.6.16 (E8 / E-repro-output-vs-graded-value) -- the emitted repro code and
# output must reproduce the value the row's VERDICT was graded against.
#
# Found by the 2026-08-04 Sonnet canary audit of collabra.77859 and reproduced
# on 7 of 15 t-test rows before fixing. For independent-samples rows the
# repro_output printed a crude `2 * stat / sqrt(df1)` approximation under a flat
# "d_ind" label, while the row had actually been matched and graded against
# d_ind_equalN / g_ind. The printed and graded numbers diverged by up to 0.0256
# (e.g. printed 0.6460 vs graded 0.6204).
#
# This directly defeats Design Principle 3 ("Always return repro_code and
# repro_output for user verification"): a user who follows our own instruction
# to run the code gets a different number than the verdict used, and would
# reasonably conclude the tool is wrong -- or, worse, trust a PASS they just
# failed to reproduce.
#
# Authored against the UNFIXED code and watched fail (repro_output printed
# "> d_ind / [1] 0.3241" against a matched_value of 0.3216) before the fix.

test_that("v0.6.16 E8: repro_output prints the graded value under its own name", {
  txt <- paste0(
    "An independent-samples t-test showed a difference, ",
    "t(223) = 2.42, p = .016, d = 0.32, 95% CI [0.06, 0.58]."
  )
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  mv  <- res$matched_value[1]
  mvar <- res$matched_variant[1]
  expect_false(is.na(mv))
  expect_false(is.na(mvar))
  # The output must name the variant that was graded, not a flat "d_ind".
  expect_true(grepl(paste0("> ", mvar), res$repro_output[1], fixed = TRUE),
              info = paste("repro_output was:", res$repro_output[1]))
  # And the printed number must BE the graded number (4dp as emitted).
  expect_true(grepl(sprintf("[1] %.4f", mv), res$repro_output[1], fixed = TRUE),
              info = paste("expected", sprintf("%.4f", mv), "in:", res$repro_output[1]))
  # The code must state the graded value too, so running it reproduces it.
  expect_true(grepl("graded against", res$repro_code[1], fixed = TRUE))
})

test_that("v0.6.16 E8: the crude approximation is never labelled as the graded value", {
  txt <- "An independent-samples t-test showed a difference, t(223) = 2.42, p = .016, d = 0.32."
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  code <- res$repro_code[1]
  # If the 2t/sqrt(df) approximation is emitted at all, it must be named
  # d_ind_approx and marked -- never a bare `d_ind <- 2 * stat / sqrt(df1)`,
  # which a reader takes for the graded value.
  expect_false(grepl("d_ind <- 2 * stat / sqrt(df1)", code, fixed = TRUE))
  if (grepl("2 * stat / sqrt(df1)", code, fixed = TRUE)) {
    expect_true(grepl("d_ind_approx", code, fixed = TRUE))
    expect_true(grepl("NOT the graded value", code, fixed = TRUE))
  }
})

test_that("v0.6.16 E8: one-sample / paired rows still emit their own d = t/sqrt(N)", {
  # Guard: the v0.6.8 one-sample emission (and its v0.6.16 dz-family extension)
  # must be untouched by the independent-samples branch change.
  txt <- "A one-sample t-test against the midpoint was significant, t(99) = 3.20, p = .002, d = 0.32."
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_true(grepl("stat / sqrt(N)", res$repro_code[1], fixed = TRUE),
              info = paste("repro_code was:", res$repro_code[1]))
})
