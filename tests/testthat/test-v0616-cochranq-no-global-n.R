# v0.6.16 (E5 / E-cochranq-global-n) -- a Cochran Q heterogeneity row must not
# adopt a sample size that came only from distant document context.
#
# Found by the 2026-08-04 Sonnet canary audit of collabra.90203 and reproduced
# in the render before fixing: the row "Q_T [40] = 104.65, p < .001, I2 = 61.8%"
# carried N = 1004 with N_source = "global_text" -- the HOST paper's participant
# count. Cochran Q is a heterogeneity test over the meta-analysis's effects (k),
# so the host paper's participant N is an unrelated quantity and its provenance
# is fabricated. Same defect class the v0.5.14 Bayesian-model-averaged guard and
# the v0.5.18 md_hl guard already close for their test types.
#
# Authored against the UNFIXED code and watched fail (N came back 1004 with
# N_source "global_text") before the guard landed.

test_that("v0.6.16 E5: cochran_q does not adopt a global-text N", {
  # Mirrors the collabra.90203 structure: a study-level "N = ..." declared
  # elsewhere in the document (so it binds as global_text), and a Cochran Q
  # reported later about a DIFFERENT body of evidence (another team's
  # meta-analysis). Pre-fix, the Q row inherited the host paper's N.
  txt <- paste0(
    "Participants. A total of N = 1004 participants completed the replication ",
    "study after exclusions. All measures are reported in the supplement.\n\n",
    "However, this approach does not perform well under high heterogeneity, ",
    "which is present in Lee and Freely's meta-analysis (QT [40] = 104.65, ",
    "p < .001, I2 = 61.8%)."
  )
  res <- check_text(txt)
  cq <- res[!is.na(res$test_type) & res$test_type == "cochran_q", ]
  expect_equal(nrow(cq), 1L)
  expect_equal(cq$stat_value[1], 104.65)
  # The host paper's participant count must NOT be attached to a heterogeneity
  # test over meta-analytic effects.
  expect_true(is.na(cq$N[1]),
              info = paste("N was", cq$N[1], "from", cq$N_source[1]))
  # Provenance must be cleared too -- an NA N with N_source "global_text" still
  # misleadingly claims the row had a document-derived sample size.
  expect_true(is.na(cq$N_source[1]) || cq$N_source[1] == "not_found",
              info = paste("N_source was", cq$N_source[1]))
})

test_that("v0.6.16 E5: the p-value consistency check still runs on cochran_q", {
  # The guard clears only N -- the row's actual verification (reported p vs
  # pchisq(Q, df, lower.tail = FALSE)) must be unaffected.
  txt <- "Heterogeneity was significant, Q_T [40] = 104.65, p < .001, I2 = 61.8%."
  res <- check_text(txt)
  cq <- res[!is.na(res$test_type) & res$test_type == "cochran_q", ]
  expect_equal(nrow(cq), 1L)
  expect_equal(cq$df1[1], 40)
  # pchisq(104.65, 40, lower.tail = FALSE) is ~2.6e-08 -- consistent with p<.001,
  # so the row must not be flagged as a decision error.
  expect_false(isTRUE(cq$decision_error[1]))
})
