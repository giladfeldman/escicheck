# v0.5.9 -- chi-square "chi^2" caret token (escicheck-iterate run 2026-05-17b)
#
# A chi-square written as the word "chi" + a caret-superscript -- "chi^2(1) =
# 3.74" -- returned 0 stats. The chi-square token alternation was duplicated in
# four places (the sub-chunk splitter and pat_chi / pat_chi_nodf /
# pat_chi_two_dfs); the symbol forms already allowed an optional caret
# ("chi^2" via the Greek/Latin symbol, "X^2") but the word form "chi" only
# matched "chi2" with no caret. The four copies had also drifted -- the
# splitter copy lacked the precomposed superscript forms. Fix: the alternation
# is hoisted to one shared chi_tok with the optional caret on the word form.
# Real verbatims: CRSP decoy-effect replication (10.1080/23743603.2021.1878340,
# "chi^2" body-text chi-squares, p.18).

chisq_rows <- function(res) {
  if (!"test_type" %in% names(res)) return(res[0, , drop = FALSE])
  res[!is.na(res$test_type) & res$test_type == "chisq", , drop = FALSE]
}

test_that("v0.5.9: 'chi^2(df)' (word chi + caret) parses as a chi-square (real CRSP verbatims)", {
  cases <- list(
    list(vq = paste0("A Fisher's exact test between the Control and the Regret-Salient ",
                     "conditions revealed weak support for a difference in the expected ",
                     "direction, chi^2(1) = 3.74, p = .057, phi = 0.075, 95% CI [0.000, 0.151]."),
         stat = 3.74),
    list(vq = paste0("The difference between the Control and the Low-Reversibility ",
                     "conditions was smaller, chi^2(1) = 2.26, p = .151, phi = 0.058, ",
                     "95% CI [0.000, 0.134]"),
         stat = 2.26),
    list(vq = paste0("and even smaller was the difference between the Regret-Salient ",
                     "and the Low-Reversibility conditions, chi^2(1) = 0.18, p = .693, ",
                     "phi = 0.017, 95% CI [0.000, 0.092]."),
         stat = 0.18)
  )
  for (cs in cases) {
    row <- chisq_rows(check_text(cs$vq))
    expect_equal(nrow(row), 1L, info = cs$vq)
    expect_equal(row$stat_value[1], cs$stat, info = cs$vq)
    expect_equal(row$df1[1], 1, info = cs$vq)
  }
})

test_that("v0.5.9: the splitter recognises 'chi^2(' so a multi-statement sentence is not collapsed", {
  # Three chi^2 statistics in one sentence -- the sub-chunk splitter must split
  # on each "chi^2(" or str_match silently drops the 2nd and 3rd (the cycle-1
  # splitter lesson: a chi-token fix is incomplete until the splitter agrees).
  multi <- paste0("chi^2(1) = 3.74, p = .057, phi = 0.075; ",
                  "chi^2(1) = 2.26, p = .151, phi = 0.058; ",
                  "chi^2(1) = 0.18, p = .693, phi = 0.017.")
  rows <- chisq_rows(check_text(multi))
  expect_equal(nrow(rows), 3L)
  expect_setequal(rows$stat_value, c(3.74, 2.26, 0.18))
})

test_that("v0.5.9 guard: existing chi-square notations still parse (chi2, X2, chi-square)", {
  for (vq in c("chi2(1) = 3.74, p = .057", "X2(1) = 3.74, p = .057",
               "chi-square(1) = 3.74, p = .057")) {
    row <- chisq_rows(check_text(vq))
    expect_equal(nrow(row), 1L, info = vq)
    expect_equal(row$stat_value[1], 3.74, info = vq)
  }
})

test_that("v0.5.9: 'chi^2' composes with a JASP subscript label (chi^2gof)", {
  # chi_tok must compose with chi_sub: caret word form + a glued gof label.
  row <- chisq_rows(check_text("chi^2gof(1) = 31.01, p = 2.572e-08, n = 329"))
  expect_equal(nrow(row), 1L)
  expect_equal(row$stat_value[1], 31.01)
})
