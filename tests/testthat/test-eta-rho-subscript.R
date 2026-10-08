# Partial eta-squared typeset with a Greek rho as its subscript.
#
# Some APA journal typesetting (Linotype MathematicalPi fonts, e.g. JEP:G 2015,
# DOI 10.1037/xge0000057) draws the "p" of partial eta-squared with the rho glyph.
# docpluck 2.4.150 decodes that font and emits the glyphs in printed order, so the
# extracted text reads `ηρ2 = .06`. Before this was recognised, every such row
# published effect_reported = NA: the eta value was silently dropped.
# Inputs are synthetic (made-up numbers), not article text.

ref_es <- function(x) {
  r <- as.data.frame(check_text(x))
  if (nrow(r) == 0L) return("0 rows")
  r[, c("test_type", "stat_value", "df1", "df2", "p_reported",
        "effect_reported", "effect_reported_name", "status")]
}

test_that("eta with a rho subscript parses exactly like eta with a p subscript", {
  latin <- ref_es("F(2, 120) = 5.00, p = .008, ηp2 = .08.")
  expect_equal(nrow(latin), 1L)
  expect_equal(latin$effect_reported, 0.08)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, ηρ2 = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, ηρ 2 = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, η2ρ = .08."), latin)
})

test_that("superscript, caret, underscore and rho-variant spellings also bind", {
  # Sonnet consult 2026-10-06 (round-eta-rho): these all fell through to NA at OK
  latin <- ref_es("F(2, 120) = 5.00, p = .008, ηp2 = .08.")
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, ηρ² = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, η²ρ = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, ηρ^2 = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, η_ρ2 = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, η2_ρ = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, ηϱ2 = .08."), latin)
})

test_that("docpluck's ASCII spelling of the rho subscript binds (what the worker receives)", {
  # Measured 2026-10-06: docpluck 2.4.150's normalized text (normalize=academic, the
  # worker's request) transliterates the glyphs, so 10.1037/xge0000057 arrives as
  # `eta2rho = .06`, never as the Greek letters.
  latin <- ref_es("F(2, 120) = 5.00, p = .008, eta2p = .08.")
  expect_equal(latin$effect_reported, 0.08)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, eta2rho = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, eta2_rho = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, etarho2 = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, eta_rho2 = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, eta_rho^2 = .08."), latin)
  # not a token boundary: a separate rho after eta-squared stays separate
  expect_false(grepl("partial eta-squared = .31", normalize_text("eta2 rho = .31"), fixed = TRUE))
})

test_that("ASCII form: superscript two and wide spaces bind, identifiers do not", {
  # Sonnet consult round 2, 2026-10-06 (round-eta-rho-ascii)
  latin <- ref_es("F(2, 120) = 5.00, p = .008, eta2p = .08.")
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, eta²rho = .08."), latin)
  expect_equal(ref_es("F(2, 120) = 5.00, p = .008, eta2rho = .08."), latin)
  expect_false(grepl("partial eta-squared", normalize_text("my_eta2rho = .2"), fixed = TRUE))
  expect_false(grepl("partial eta-squared", normalize_text("x1eta2rho = .2"), fixed = TRUE))
})

test_that("an eta-squared followed by a separate rho is not merged into one token", {
  # Sonnet consult 2026-10-06: `2\s*rho` let a line break join eta-squared to a
  # following, unrelated rho statistic -> a wrong partial eta-squared value.
  expect_false(grepl("partial eta-squared = .31",
                     normalize_text("η2 = .05; η2\nρ = .31"), fixed = TRUE))
  expect_false(grepl("partial eta-squared = .31",
                     normalize_text("η2 ρ = .31"), fixed = TRUE))
})

test_that("a rho subscript is not an independent rho effect size", {
  r <- check_text("F(2, 120) = 5.00, p = .008, ηρ2 = .08.")
  expect_false(identical(r$effect_reported_name, "rho"))
  # a standalone rho with no eta in front keeps its own meaning
  expect_true(grepl("ρ = .30", normalize_text("ρ = .30"), fixed = TRUE))
})
