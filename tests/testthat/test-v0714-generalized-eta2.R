# v0.7.14 (d-b1c193): generalized eta-squared is not computable from F and df.
#
# eta_G^2 = SS_effect / (SS_effect + SS_error + SS of every MEASURED factor and
# of subjects) (Olejnik & Algina, 2003; Bakeman, 2005). Which terms enter the
# denominator depends on the design, which F and df do not carry. Until 0.7.14
# compute_all_anova_effects() fell through to the between-subjects formula for
# an UNCLEAR design, so the published `generalized_eta2` column and
# all_variants$same_type$generalized_eta2 were partial eta-squared under a
# different name -- on the very row whose uncertainty text said generalized
# eta-squared "cannot be verified from summary statistics". Measured on
# collabra.126266: equal to partial_eta2 on all 15 F rows.
#
# The same row also carried two false warnings (generalized eta-squared called
# "unusual for F-test", and its explicit label called an "unclear symbol") and
# collapsed to SKIP although its p-value WAS checked.

gen_row <- function(txt) check_text(txt)

test_that("generalized_eta2 is NA when the design is not known, never partial eta2", {
  r <- gen_row("F(1, 98) = 5.00, p = .028, generalized eta-squared = .05")
  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$generalized_eta2))
  av <- jsonlite::fromJSON(r$all_variants[[1]], simplifyVector = FALSE)
  expect_null(av$same_type$generalized_eta2)
})

test_that("an unreported-type F row does not publish a generalized eta2 either", {
  r <- gen_row("F(2, 97) = 5.00, p = .009, partial eta-squared = .09")
  expect_true(is.na(r$generalized_eta2))
  # partial eta2 itself is unchanged
  expect_equal(r$partial_eta2, 10 / (10 + 97), tolerance = 1e-9)
  expect_equal(r$status, "PASS")
})

test_that("compute_all_anova_effects: generalized eta2 NA for every design it cannot know", {
  for (d in c("unclear", "within", "mixed", "between")) {
    expect_true(is.na(compute_all_anova_effects(5, 1, 98, d)$generalized_eta2),
                info = d)
  }
})

test_that("generalized eta2 is not called unusual for an F-test, nor an unclear symbol", {
  r <- gen_row("F(1, 98) = 5.00, p = .028, generalized eta-squared = .05")
  u <- paste(unlist(r$uncertainty_reasons), collapse = " | ")
  expect_false(grepl("unusual", u, fixed = TRUE))
  expect_false(grepl("unclear - may be OCR", u, fixed = TRUE))
  # the honest limitation stays
  expect_true(grepl("Generalized eta-squared cannot be verified", u, fixed = TRUE))
})

test_that("a corrupted generalized-eta2 form IS still flagged as an uncertain symbol", {
  r <- gen_row("F(1, 98) = 5.00, p = .028, n2G = .05")
  u <- paste(unlist(r$uncertainty_reasons), collapse = " | ")
  expect_true(grepl("unclear - may be OCR", u, fixed = TRUE))
})

test_that("a generalized-eta2 row whose p-value was checked is not SKIP", {
  r <- gen_row("F(1, 98) = 5.00, p = .028, generalized eta-squared = .05")
  expect_equal(r$check_type, "p_value")
  expect_false(r$status == "SKIP")
  # and a p that is wrong is still caught
  r2 <- gen_row("F(1, 98) = 5.00, p = .60, generalized eta-squared = .05")
  expect_true(r2$status %in% c("WARN", "ERROR"))
})

test_that("a generalized-eta2 CI is not graded against partial-eta2 intervals", {
  # collabra.126266 at 0.7.13: 8 INCONSISTENT / 5 PLAUSIBLE / 1 MATCH, every one
  # a comparison against a different estimand.
  r <- gen_row("F(1, 265) = 26.51, p < .001, generalized eta-squared = .037, 95% CI [.01, .08]")
  expect_equal(r$ci_check_status, "UNVERIFIABLE")
  expect_equal(r$ci_unverifiable_reason, "estimand_not_computable")
  expect_true(is.na(r$ci_match))
  # control: a partial-eta2 interval is still graded
  p <- gen_row("F(1, 265) = 26.51, p < .001, partial eta-squared = .09, 95% CI [.04, .16]")
  expect_true(p$ci_check_status %in% c("MATCH", "PLAUSIBLE", "INCONSISTENT"))
})
