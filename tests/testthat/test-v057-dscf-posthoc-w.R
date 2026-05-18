# v0.5.7 -- DSCF (Dwass-Steel-Critchlow-Fligner) post-hoc W recognised
#
# escicheck-iterate cycle 1 (run 2026-05-17): the DSCF pairwise-comparison W
# of a Kruskal-Wallis post-hoc (Ziano & Feldman, J. Econ. Psych. 2021,
# 10.1016/j.joep.2020.102349) was mishandled. A negative DSCF W -- "W = -3.84,
# p = .018" -- returned 0 stats (pat_W and the sub-chunk splitter both rejected
# the leading minus). A positive DSCF W -- "W = 5.99" -- parsed but was
# mislabelled Wilcoxon's W. Fix: pat_W and the splitter accept a leading sign;
# the W block routes a negative W, or a W in an explicit DSCF / Dwass /
# Kruskal-pairwise context, to a new "dscf" test type. DSCF is an honest
# "cannot verify" NOTE -- no standard effect size is recoverable from the W
# alone (mirrors the kendall_w handling).

dscf_rows <- function(res) {
  if (!"test_type" %in% names(res)) return(res[0, , drop = FALSE])
  res[!is.na(res$test_type) & res$test_type == "dscf", , drop = FALSE]
}

test_that("v0.5.7: a negative DSCF W is extracted as test_type 'dscf' (real joep verbatims)", {
  # Real verbatim quotes from Ziano & Feldman (2021), Kruskal-Wallis DSCF
  # post-hoc pairwise comparisons. pat_W previously rejected the minus sign,
  # so each of these returned 0 stats.
  cases <- list(
    list(vq = "for a difference between the economic terms presentation and the job attractiveness presentation (W = -3.84, p = .018)", w = -3.84),
    list(vq = "for a difference between the happiness presentation and the job attractiveness presentation (W = -9.71, p < .001).", w = -9.71),
    list(vq = "but no support for a difference between the happiness presentation and the job attractiveness presentation (W = -0.95, p = .78).", w = -0.95)
  )
  for (cs in cases) {
    row <- dscf_rows(check_text(cs$vq))
    expect_equal(nrow(row), 1L, info = cs$vq)
    expect_equal(row$stat_value[1], cs$w, info = cs$vq)
    expect_equal(row$status[1], "NOTE", info = cs$vq)
  }
})

test_that("v0.5.7: an explicit DSCF context routes a W to 'dscf'", {
  # The Kruskal-Wallis DSCF context is in the sentence -- the W is DSCF, not
  # Wilcoxon's W. (Real joep verbatim.)
  vq <- "We conducted DSCF comparisons in a Kruskal-Wallis ANOVA, which showed that choices in the real terms condition were significantly different from choices in the nominal condition (W = -4.02, p = .012)"
  row <- dscf_rows(check_text(vq))
  expect_equal(nrow(row), 1L)
  expect_equal(row$stat_value[1], -4.02)
  expect_equal(row$status[1], "NOTE")
})

test_that("v0.5.7: the sub-chunk splitter handles multiple DSCF W's incl. negatives", {
  # A multi-statement DSCF paragraph: the splitter must split on a negative W
  # too, and the DSCF context carries to each comparison -- so even the
  # positive W = 5.99 is classified DSCF, not Wilcoxon's W.
  txt <- paste0("We conducted DSCF comparisons in a Kruskal-Wallis ANOVA. ",
                "We found support for a difference between economic terms and ",
                "happiness (W = 5.99, p < .001), for a difference between ",
                "economic terms and job attractiveness (W = -3.84, p = .018), ",
                "and for a difference between happiness and job attractiveness ",
                "(W = -9.71, p < .001).")
  rows <- dscf_rows(check_text(txt))
  expect_equal(nrow(rows), 3L)
  expect_setequal(rows$stat_value, c(5.99, -3.84, -9.71))
  expect_true(all(rows$status == "NOTE"))
})

test_that("v0.5.7 guard: a plain positive W (no DSCF context) stays Wilcoxon's W", {
  # Without a DSCF / Dwass / Kruskal-pairwise context and with a non-negative
  # value, a bare "W =" is still Wilcoxon's W -- the fix must not over-route.
  res <- check_text("A Wilcoxon signed-rank test was significant (W = 210, z = 2.4, p = .016)")
  tt <- res$test_type[!is.na(res$test_type)]
  expect_true("W" %in% tt)
  expect_false("dscf" %in% tt)
})

test_that("v0.5.7 guard: Kendall's W is still recognised, not re-routed to dscf", {
  res <- check_text("There was strong agreement among raters (Kendall's W = 0.82, p = .003)")
  tt <- res$test_type[!is.na(res$test_type)]
  expect_true("kendall_w" %in% tt)
  expect_false("dscf" %in% tt)
})
