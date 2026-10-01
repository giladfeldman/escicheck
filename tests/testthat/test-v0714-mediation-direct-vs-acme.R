# v0.7.14 (d-4fe38e): a sentence reporting the direct effect AND the ACME yields
# both, each under its own name.
#
# Chan & Feldman (2025), Cognition and Emotion, doi 10.1080/02699931.2024.2434156:
#
#   "The average direct effect was 0.15, 95% CI [-0.13 to 0.45], p = .3, whereas
#    the bootstrapped unstandardised indirect effect (Average Causal Mediation
#    Effect, ACME) was 0.67, 95% CI [0.47-0.89], p < .001."
#
# v0.6.16 added pat_mediation_ci for exactly this sentence and its changelog says
# both effects were recovered. Re-tested at 0.7.13: ONE row, carrying the DIRECT
# effect (0.15, [-0.13, 0.45], p = .3) under effect_reported_name
# "indirect_effect", and the ACME (the paper's actual mediation finding) absent.
# A reader of that row sees a non-significant "indirect effect" the paper never
# reported.

MED <- paste(
  "The average direct effect was 0.15, 95% CI [-0.13 to 0.45], p = .3, whereas",
  "the bootstrapped unstandardised indirect effect (Average Causal Mediation",
  "Effect, ACME) was 0.67, 95% CI [0.47-0.89], p < .001.")

med_rows <- function(txt) {
  r <- as.data.frame(check_text(txt))
  r[!is.na(r$test_type) & r$test_type == "mediation_indirect", , drop = FALSE]
}

test_that("both mediation effects are extracted, each under its own name", {
  r <- med_rows(MED)
  expect_equal(nrow(r), 2L)
  ade <- r[r$effect_reported_name == "direct_effect", ]
  acme <- r[r$effect_reported_name == "indirect_effect", ]
  expect_equal(nrow(ade), 1L)
  expect_equal(nrow(acme), 1L)
  expect_equal(ade$effect_reported, 0.15)
  expect_equal(c(ade$ciL_reported, ade$ciU_reported), c(-0.13, 0.45))
  expect_equal(ade$p_reported, 0.3)
  expect_equal(acme$effect_reported, 0.67)
  expect_equal(c(acme$ciL_reported, acme$ciU_reported), c(0.47, 0.89))
  expect_equal(acme$p_reported, 0.001)
})

test_that("the direct effect is never labelled indirect, and the note says which", {
  r <- med_rows(MED)
  ade <- r[r$effect_reported == 0.15, ]
  expect_equal(ade$effect_reported_name, "direct_effect")
  u <- paste(unlist(ade$uncertainty_reasons), collapse = " | ")
  expect_true(grepl("direct effect", u, fixed = TRUE))
  expect_false(grepl("Sobel Z", u, fixed = TRUE))
})

test_that("a lone ACME sentence still yields one indirect-effect row (control)", {
  r <- med_rows("The indirect effect (ACME) was 0.67, 95% CI [0.47, 0.89], p < .001.")
  expect_equal(nrow(r), 1L)
  expect_equal(r$effect_reported_name, "indirect_effect")
})

test_that("the Sobel-Z form is unchanged (control)", {
  r <- med_rows(paste("The bootstrapped indirect effect of X on Y was .05, 95% CI",
                      "[-.04, .12], Sobel Z = 0.84, p = .40."))
  expect_equal(nrow(r), 1L)
  expect_equal(r$effect_reported_name, "indirect_effect")
  expect_equal(r$stat_value, 0.84)
})
