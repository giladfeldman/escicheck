# Markdown emphasis around a statistic symbol must not destroy the result.
#
# Reported by a downstream consumer (2026-08-21/22, measured against 0.7.6, still
# true at 0.7.15): APA style italicises statistic symbols, so markdown, DOCX- and
# HTML-converted text carries `*t*(17) = -1.32, *p* = .453`. An italic statistic
# produced ZERO rows; an italic p alone was worse -- the row survived with
# p_reported = NA and status SKIP, a row that looks checked and is not.
# Inputs are synthetic (made-up numbers), not article text.

ref <- function(x) {
  r <- as.data.frame(check_text(x))
  if (nrow(r) == 0L) return("0 rows")
  r[, c("test_type", "stat_value", "df1", "p_reported", "effect_reported", "status")]
}

test_that("italic, bold and underscore emphasis parse exactly like plain text", {
  plain <- ref("t(48) = 2.31, p = .025, d = 0.65.")
  expect_equal(nrow(plain), 1L)
  for (x in c("*t*(48) = 2.31, *p* = .025, *d* = 0.65.",
              "**t**(48) = 2.31, **p** = .025, **d** = 0.65.",
              "_t_(48) = 2.31, _p_ = .025, _d_ = 0.65.",
              "***t***(48) = 2.31, ***p*** = .025, ***d*** = 0.65.",
              "*t*(48) = 2.31, p = .025, d = 0.65.")) {
    expect_equal(ref(x), plain, info = x)
  }
})

test_that("an italic p alone is not silently dropped", {
  r <- check_text("t(17) = -1.32, *p* = .453.")
  expect_equal(nrow(r), 1L)
  expect_equal(r$p_reported, 0.453)
  expect_false(identical(r$status, "SKIP"))
})

test_that("F, chi-square, r and partial eta-squared survive emphasis", {
  expect_equal(ref("*F*(1, 98) = 12.34, *p* = .001, *eta2p* = .11."),
               ref("F(1, 98) = 12.34, p = .001, eta2p = .11."))
  expect_equal(ref("*r*(100) = .30, *p* = .002."), ref("r(100) = .30, p = .002."))
  expect_equal(ref("*chi2*(1, N = 200) = 5.10, *p* = .024."),
               ref("chi2(1, N = 200) = 5.10, p = .024."))
})

test_that("asterisks that are not emphasis are left alone", {
  # significance stars and a multiplication-style asterisk must not merge tokens
  expect_true(grepl("\\.32\\*\\*", normalize_text("r = .32** and r = .10*")))
  expect_true(grepl("2\\*3\\*4", normalize_text("a 2*3*4 design")))
  # an identifier with internal underscores keeps them
  expect_true(grepl("model_t_value", normalize_text("model_t_value = 3"), fixed = TRUE))
})
