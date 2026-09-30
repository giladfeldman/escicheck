# v0.7.14 (d-43c69d): a post-hoc contrast whose effect size cannot decide
# between the two sample-size readings must SAY both.
#
# collabra.90203 reprints the omnibus error df F(2, 998) on its Bonferroni
# post-hoc contrasts. v0.6.18 adopts the balanced contrast N (~667) when the
# row's own d decisively favours it -- t(998) = 2.46, d = 0.19 does, and was
# re-tested MATCH at 0.7.13. But t(998) = 0.097, d = 0.01 cannot decide: at a d
# that small both N = 1000 and N ~ 667 reproduce it. The row kept N = 1000 and
# said nothing about the contrast reading, although the announcement it sits
# under says it is a post-hoc contrast (the paper's cells are 335 + 335).
# Publishing the df + 2 reading is the package's convention; publishing it as
# the only reading is not. (Owner ruling 2026-09-04: report, never silently
# correct.)

OMNI <- paste(
  "There was a main effect of condition, F(2, 998) = 3.91, p = .02.",
  "Post-hoc comparisons with Bonferroni correction showed no difference between",
  "statistical and identifiable victims, t(998) = 0.097, p = 1.00, d = 0.01, 95% CI [-0.16, 0.14],",
  "and a difference between statistical and joint, t(998) = 2.46, p = .041, d = 0.19 [0.04, 0.34].")

t_by_stat <- function(res, v) {
  res <- as.data.frame(res)
  res[!is.na(res$test_type) & res$test_type == "t" & abs(res$stat_value - v) < 1e-9, , drop = FALSE]
}

test_that("an undecidable post-hoc contrast names the contrast reading", {
  r <- t_by_stat(check_text(OMNI), 0.097)
  expect_equal(nrow(r), 1L)
  expect_equal(r$N, 1000)            # the incumbent reading is still published
  u <- paste(unlist(r$uncertainty_reasons), collapse = " | ")
  expect_true(grepl("post-hoc contrast", u, fixed = TRUE))
  expect_true(grepl("N~667", u, fixed = TRUE))
})

test_that("the decisive contrast is unchanged: adopted, labelled, MATCH", {
  r <- t_by_stat(check_text(OMNI), 2.46)
  expect_equal(r$N, 667)
  expect_equal(r$N_source, "omnibus_df_contrast")
  expect_equal(r$status, "PASS")
})

test_that("no post-hoc announcement -> no contrast note (control)", {
  txt <- paste(
    "There was a main effect of condition, F(2, 998) = 3.91, p = .02.",
    "Separately, the two sexes did not differ, t(998) = 0.097, p = .92, d = 0.01.")
  r <- t_by_stat(check_text(txt), 0.097)
  u <- paste(unlist(r$uncertainty_reasons), collapse = " | ")
  expect_false(grepl("post-hoc contrast", u, fixed = TRUE))
})
