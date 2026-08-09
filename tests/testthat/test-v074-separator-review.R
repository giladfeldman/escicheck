# v0.7.4 -- two defects in the v0.7.3 separator logic, found by the cross-model
# review that the v0.7.3 release never ran (Codex/gpt-5.5, 2026-08-09).
#
# The reviewer produced 8 findings. All 8 were run against the working tree
# before anything was changed. THREE reproduced as stated or worse, TWO
# reproduced with a different outcome than claimed, and THREE were refuted:
#
#   REFUTED  "F (1,234) fuses the df pair"     -- normalizes to "F (1, 234)" and
#            parses correctly as df1 = 1, df2 = 234.
#   REFUTED  "Indian grouping 12,34,567 is partially stripped" -- left verbatim.
#   REFUTED  "a 1:5 lopsided locale contradiction returns US as decisive, so
#            d = 0,80 reads as 0" -- it converts to 0.80.
#   NOT FIXED (recorded) "F = 1,234 with no locale evidence reads as 1234
#            rather than 1.234" -- reproduced, but genuinely undecidable from
#            shape and it publishes no effect size (status NOTE, no reported
#            effect to grade). Left as the documented ambiguity.
#   NOT FIXED (recorded) "RGB = 120,120,120 fuses to 120120120" -- reproduced,
#            and worse than claimed (full fusion, not partial), but the fused
#            token is not read as any statistic: the neighbouring t-test row is
#            byte-identical with and without it. This is the known undecidable
#            width-3 chain case; the corpus scan found 71 such chains across 48
#            real papers and every one is a citation superscript already
#            protected upstream.
#
# The two below DO reach a published number, so they are fixed here. Both tests
# were watched to FAIL against v0.7.3.

test_that("a European decimal with no integer part is converted, not dropped", {
  # Rule D1 required at least one digit BEFORE the comma -- `(\d+),(\d+)`. A
  # European paper omits the leading zero exactly as APA does: "p = ,025" is
  # the continental spelling of "p = .025". It matched nothing, so the p-value
  # was SILENTLY LOST while the same clause's t and d converted normally.
  #
  # At v0.7.3: p_reported = NA on this row, and the full sentence form with
  # "p < ,001" returned ZERO rows.
  res <- effectcheck::check_text(
    "Der Unterschied war signifikant, t(48) = 2,31, p = ,025, d = 0,74.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$stat_value[1], 2.31)
  expect_equal(res$p_reported[1], 0.025)
  expect_equal(res$effect_reported[1], 0.74)
})

test_that("a European p reported with an operator is converted", {
  res <- effectcheck::check_text(
    "Die Wirkung war gross, t(58) = 4,12, p < ,001, d = 1,08.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$p_reported[1], 0.001)
  expect_equal(res$effect_reported[1], 1.08)
})

test_that("the leading-comma repair does not fire on a list or a CI pair", {
  # The repair is scoped to a VALUE POSITION -- a comma immediately after `=`,
  # `<` or `>` with no space. A list comma or a CI pair never has that shape,
  # and admitting them would fuse or corrupt real numbers.
  res <- effectcheck::check_text(
    "The effect was reliable, t(48) = 2.31, p = .025, d = 0.74, 95% CI [0.45, 0.89].")
  expect_equal(nrow(res), 1L)
  expect_equal(res$ciL_reported[1], 0.45)
  expect_equal(res$ciU_reported[1], 0.89)
  expect_equal(res$effect_reported[1], 0.74)
})

test_that("an OCR-spaced sample size joins every group, not just the first", {
  # The sample-size repair ran in a `repeat` loop whose anchor was `\d{1,3}`.
  # After the first join the prefix is four digits, so the loop could not
  # continue: "nobs = 1, 234, 567" became "nobs = 1234, 567" and the row
  # published N = 1234 -- a wrong sample size, not a missing one.
  res <- effectcheck::check_text(
    "A register chi-square, chi2(1) = 8.42, nobs = 1, 234, 567, was significant, p = .004.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$N[1], 1234567)
})

test_that("a tight CI pair is not rebuilt as European full notation", {
  # THE WORST OF THE SET, and the only one found on REAL PUBLISHED TEXT.
  # nathumbeh_replication_2025.txt writes its intervals with no space after the
  # comma -- "95%CI=[7.944,11.984]" -- 11 times. `.apply_full_notation` matched
  # "7.944,11" as European full notation (period groups, comma decimates) and
  # rebuilt it as "7944.11", so the normalized text read
  #   95%CI=[7944.11.984]
  # and the interval was DESTROYED: ciL and ciU both NA, status still OK. The
  # identical clause written with a space parses correctly, which is what makes
  # this invisible -- the same paper's other rows look fine.
  #
  # The discriminator is structural, not locale-based: in genuine full notation
  # the part after the comma is a TERMINAL fraction. It cannot be followed by
  # another decimal point and more digits. "7.944,11.984" therefore cannot be
  # one number, whatever the document's locale.
  res <- effectcheck::check_text("t(183)=9.733, P<0.001, 95%CI=[7.944,11.984]")
  expect_equal(nrow(res), 1L)
  expect_equal(res$stat_value[1], 9.733)
  expect_equal(res$ciL_reported[1], 7.944)
  expect_equal(res$ciU_reported[1], 11.984)

  res2 <- effectcheck::check_text("t(181)=12.607, P<0.0001, 95%CI=[2.320,3.182]")
  expect_equal(res2$ciL_reported[1], 2.320)
  expect_equal(res2$ciU_reported[1], 3.182)
})

test_that("a tight effect-then-CI-level comma is not rebuilt as full notation", {
  # The same class as above, second shape, found in the SAME real paper by
  # inspecting what the first fix left behind:
  #   "=-0.008,95%CI=[-0.023,0.007]"  ->  "=-0008.95%CI=[...]"
  # The rule read "0.008,95" as European full notation and fused the reported
  # coefficient with the CI's confidence LEVEL, destroying the effect size.
  # `(?!\.\d)` does not catch it because what follows the fraction here is `%`,
  # not a decimal point.
  txt <- "t(1988.3245)=-1.044, P=0.297, =-0.008,95%CI=[-0.023,0.007]"
  norm <- effectcheck:::normalize_text(txt)
  expect_false(grepl("0008.95", norm, fixed = TRUE))
  expect_true(grepl("-0.008", norm, fixed = TRUE))
})

test_that("genuine European full notation still converts", {
  # The rule must keep doing its job: "1.234,56" is self-identifying, and a
  # sentence-final period after the fraction is not another decimal.
  expect_equal(effectcheck:::normalize_text("The total was 1.234,56 euros."),
               "The total was 1234.56 euros.")
  expect_equal(effectcheck:::normalize_text("Der Wert betrug 1.234,56."),
               "Der Wert betrug 1234.56.")
  expect_equal(effectcheck:::normalize_text("A total of 1.234.567,89 records."),
               "A total of 1234567.89 records.")
})

test_that("a four-digit N followed by a separate count is not fused", {
  # Self-review of the fix above. The obvious repair -- widening the anchor to
  # `\d{1,3}(?:\d{3})*` so the loop could continue -- also admits a run whose
  # FIRST group is already four digits, fusing "N = 1234, 567 of whom were
  # female" into N = 1234567. v0.7.3 correctly refused that, so the start anchor
  # must stay exactly as strict as it was. Watched to FAIL against the
  # widened-anchor draft.
  expect_equal(
    effectcheck:::normalize_text("The sample was N = 1234, 567 of whom were female."),
    "The sample was N = 1234, 567 of whom were female.")
})

test_that("the single-group OCR-spaced sample size still works", {
  # The v0.7.2 case the loop was written for must not regress.
  res <- effectcheck::check_text(
    "A chi-square on the register, chi2(1) = 8.42, N = 1, 182, was significant, p = .004.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$N[1], 1182)
})
