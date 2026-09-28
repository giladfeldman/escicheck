# v0.7.9 -- a bare `r` printed in a table, with no df and no n of its own, bound
# the DOCUMENT-GLOBAL N and computed its interval from it.
#
# Source: 2026-09-02 escicheck-iterate. The defect was first filed on 2026-08-04
# as item 5a.4 of the N-binding family ("cog_emo 2434156 -- table-local N not
# preferred ... 3 false CI-mismatch flags") and left open through v0.6.17,
# v0.6.18, v0.7.x. It is the SAME class as v0.6.17 / v0.6.18: a scraped
# document-level N is a last-resort guess and must lose to structural evidence
# about which sample the statistic is on.
#
# WHAT WENT WRONG
#
# Chan & Feldman (2025), Cognition and Emotion 39(6), doi
# 10.1080/02699931.2024.2434156, reports N = 794 across both waves, and a
# control-condition subsample of n = 263. Its Table 9 prints bare correlations
# (`r = .45` in one cell, `[0.34, 0.54]` in the next) with no df and no n. Those
# rows bound N = 794, derived df = 792, and recomputed intervals from it.
#
# The published intervals decide which N is right, and they are not close:
#
#     reported        published CI        at N=794              at N=263
#     r =  .63    [0.560, 0.700]   [0.586, 0.670] err .056   [0.551, 0.698] err .011
#     r =  .45    [0.340, 0.540]   [0.393, 0.504] err .089   [0.348, 0.541] err .010
#     r = -.43    [-0.520, -0.330] [-0.485, -0.372] err .077 [-0.524, -0.326] err .008
#
# An order of magnitude. The N=794 rows were published as PLAUSIBLE -- a
# CI-mismatch verdict manufactured entirely by binding the wrong sample.
#
# THE EVIDENCE THE PARSER WAS NOT USING, and the trap in reaching it: the
# `n = 263` note belongs to Table 8; the rows in question are Table 9, whose own
# note says only "Effects are Pearson's correlations". So "read the adjacent
# table note" does NOT reach them. What does reach them is the PROSE, which
# states the same correlations with their df: `r(261) = -0.43, 95% CI
# [-0.52, -0.33]`. Exact value match against a stated `r(df)` is the evidence
# gate -- the same shape as the v0.6.18 df-compatible table-N rule, where two
# independently-sourced numbers agreeing exactly is the thing that licenses the
# binding.
#
# UNIQUENESS IS LOAD-BEARING. Two correlations of equal value computed on
# different samples must not cross-contaminate, so the adoption fires only when
# every stated `r(df)` carrying this value agrees on one df.
#
# WHAT THIS DOES NOT FIX, stated so nobody records it as fixed: Table 9's
# `r = .63` has no stated `r(df) = .63` anywhere (the prose rounds it to .64), so
# no match exists and that row keeps the global N. 3 of the paper's 4 affected
# rows are corrected; the fourth is out of reach of any value-match rule.

test_that("a bare table r with no df adopts the df of a uniquely-matching stated r(df)", {
  txt <- paste0(
    "Participants. We recruited N = 794 participants across both waves. ",
    paste(rep("Filler sentence describing the procedure in some detail. ", 30),
          collapse = ""),
    "In the control condition we found that empathy was positively associated ",
    "with conciliation, r(261) = 0.45, 95% CI [0.35, 0.54], p < .001. ",
    paste(rep("More filler about the analysis plan and the measures used. ", 30),
          collapse = ""),
    "\n\nTable 9. Replication results.\n\nr = .45\n\n[0.34, 0.54]\n\n",
    "Signal, consistent\n\n<.001\n\n",
    "Note: Effects are Pearson's correlations.\n"
  )

  res <- effectcheck::check_text(txt)
  r <- res[!is.na(res$test_type) & res$test_type == "r", ]

  # The bare table row is the one whose reported lower bound is 0.34; the prose
  # row (already correct via the v0.6.12 r(df) rule) reports 0.35.
  bare <- r[!is.na(r$ciL_reported) & abs(r$ciL_reported - 0.34) < 1e-9, ]
  expect_equal(nrow(bare), 1L)

  # EXPECTATIONS CHANGED v0.7.9. Gilad's ruling, 2026-09-04: "We should never just
  # correct things, the aim is highest transparency and accuracy." The rule no
  # longer ADOPTS the matched df -- adopting it could move a published verdict in
  # both directions (measured: a correct independent-sample CI graded
  # INCONSISTENT, and a real reporting error graded MATCH). It now reports both
  # readings and publishes neither.
  expect_equal(as.numeric(bare$N[1]), 794,
    info = "the bound N must be left exactly as the evidence had it -- nothing is rebound")
  expect_false(identical(as.character(bare$N_source[1]), "corr_matched_r_df"),
    info = "no provenance is claimed for a sample size that was not adopted")

  # Here the two candidate readings DISAGREE (at N=263 the published [0.34, 0.54]
  # reproduces; at N=794 it does not), so no verdict may be published.
  expect_equal(as.character(bare$ci_check_status[1]), "UNVERIFIABLE")
  expect_true(grepl("AMBIGUOUS SAMPLE", as.character(bare$uncertainty_reasons[1]), fixed = TRUE))
  expect_true(grepl("N=263", as.character(bare$uncertainty_reasons[1]), fixed = TRUE),
    info = "the alternative reading must still be shown to the reader")

  # The prose row is untouched -- it already had its own df.
  prose <- r[!is.na(r$ciL_reported) & abs(r$ciL_reported - 0.35) < 1e-9, ]
  expect_equal(nrow(prose), 1L)
  expect_equal(as.numeric(prose$N[1]), 263)
})

test_that("CONTROL: a bare r with no matching stated r(df) keeps the global N", {
  # Same document shape, but the table prints .63 while the prose states .64 --
  # the real Chan & Feldman case. No match exists, so nothing is adopted and the
  # row keeps exactly the behaviour it had before this rule.
  txt <- paste0(
    "Participants. We recruited N = 794 participants across both waves. ",
    paste(rep("Filler sentence describing the procedure in some detail. ", 30),
          collapse = ""),
    "Forgiveness was associated with conciliation, r(261) = 0.64, ",
    "95% CI [0.56, 0.70], p < .001. ",
    paste(rep("More filler about the analysis plan and the measures used. ", 30),
          collapse = ""),
    "\n\nTable 9. Replication results.\n\nr = .63\n\n[0.56, 0.70]\n\n",
    "Signal, consistent\n\n<.001\n\n",
    "Note: Effects are Pearson's correlations.\n"
  )

  res <- effectcheck::check_text(txt)
  r <- res[!is.na(res$test_type) & res$test_type == "r", ]
  bare <- r[!is.na(r$effect_reported) & abs(r$effect_reported - 0.63) < 1e-9, ]
  expect_equal(nrow(bare), 1L)

  expect_equal(as.numeric(bare$N[1]), 794,
    info = "no stated r(df) carries .63, so there is no evidence to adopt and the row must be left alone")
  expect_false(identical(as.character(bare$N_source[1]), "corr_matched_r_df"))
})

test_that("CONTROL: an r value stated with TWO different dfs is ambiguous and adopts neither", {
  # Two studies, both reporting r = 0.45, on different samples. A value match
  # alone is not evidence about WHICH sample the bare table row is on.
  txt <- paste0(
    "Participants. We recruited N = 794 participants across both waves. ",
    paste(rep("Filler sentence describing the procedure in some detail. ", 25),
          collapse = ""),
    "In Study 1 the association was r(261) = 0.45, 95% CI [0.35, 0.54], p < .001. ",
    paste(rep("More filler about the analysis plan and the measures used. ", 25),
          collapse = ""),
    "In Study 2 the same association was r(118) = 0.45, 95% CI [0.29, 0.59], ",
    "p < .001. ",
    paste(rep("Yet more filler separating the studies from the table. ", 25),
          collapse = ""),
    "\n\nTable 9. Pooled results.\n\nr = .45\n\n[0.34, 0.54]\n\n",
    "Signal, consistent\n\n<.001\n\n",
    "Note: Effects are Pearson's correlations.\n"
  )

  res <- effectcheck::check_text(txt)
  r <- res[!is.na(res$test_type) & res$test_type == "r", ]
  bare <- r[!is.na(r$ciL_reported) & abs(r$ciL_reported - 0.34) < 1e-9, ]
  expect_equal(nrow(bare), 1L)

  expect_false(identical(as.character(bare$N_source[1]), "corr_matched_r_df"),
    info = "0.45 is stated with df 261 AND df 118; adopting either would be a coin flip dressed as evidence")
  expect_false(as.numeric(bare$N[1]) %in% c(263, 120))
})

test_that("CONTROL: the rule does not silence a genuinely wrong published interval", {
  # Chan & Feldman Table 9 row 2bii prints r = -.43 with [-0.52, 0.33] -- the
  # authors' own typo, a dropped minus, confirmed by rasterizing page 13 on
  # 2026-09-02. ESCImate flags it INCONSISTENT and that flag is CORRECT.
  #
  # This is the failure mode a value-matching rule must not have: rebinding N so
  # that intervals reproduce could, if it selected N by CI agreement, make a real
  # published error disappear. It selects on the r VALUE only, never on the CI,
  # so the interval stays independent evidence -- and the error survives.
  txt <- paste0(
    "Participants. We recruited N = 794 participants across both waves. ",
    paste(rep("Filler sentence describing the procedure in some detail. ", 30),
          collapse = ""),
    "Forgiveness was negatively associated with revenge motivation, ",
    "r(261) = -0.43, 95% CI [-0.52, -0.33], p < .001. ",
    paste(rep("More filler about the analysis plan and the measures used. ", 30),
          collapse = ""),
    "\n\nTable 9. Replication results.\n\nr = -.43\n\n[-0.52, 0.33]\n\n",
    "Signal\n\n<.001\n\n",
    "Note: Effects are Pearson's correlations.\n"
  )

  res <- effectcheck::check_text(txt)
  r <- res[!is.na(res$test_type) & res$test_type == "r", ]
  bare <- r[!is.na(r$ciU_reported) & abs(r$ciU_reported - 0.33) < 1e-9, ]
  expect_equal(nrow(bare), 1L)

  # v0.7.9: the N is NOT corrected -- it stays as the evidence had it ...
  expect_equal(as.numeric(bare$N[1]), 794,
    info = "nothing is rebound; the ambiguity is reported instead")
  # ... and the authors' error is STILL reported.
  #
  # THIS TEST IS WHY THE CAP IS CONDITIONAL. A first draft of the v0.7.9 fix
  # capped every ambiguous row at UNVERIFIABLE, and this control went red: it
  # suppressed a REAL published error. Note the reported [-0.52, 0.33] still
  # CONTAINS -0.43, so `estimate_outside_ci` does not catch it either (verified
  # FALSE) -- the CI recomputation is the only check that sees this typo.
  # Both candidate sample sizes (794 and 263) call it INCONSISTENT, so the
  # ambiguity does not change the answer and the answer is published.
  expect_equal(as.character(bare$ci_check_status[1]), "INCONSISTENT",
    info = "the published [-0.52, 0.33] is a real error; an ambiguous N must not suppress it when BOTH readings agree it is wrong")
  expect_true(grepl("both candidate readings", as.character(bare$uncertainty_reasons[1]), fixed = TRUE),
    info = "the reader is told the verdict survives the ambiguity, and why")
})

test_that("CONTROL: a correlation carrying its own r(df) is not touched by this rule", {
  # v0.6.12 already owns this row. The new rule must not relabel its provenance.
  txt <- paste0(
    "Participants. We recruited N = 794 participants across both waves. ",
    paste(rep("Filler sentence describing the procedure in some detail. ", 30),
          collapse = ""),
    "The association was r(261) = 0.45, 95% CI [0.35, 0.54], p < .001."
  )

  res <- effectcheck::check_text(txt)
  r <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_gt(nrow(r), 0)
  expect_false(any(as.character(r$N_source) %in% "corr_matched_r_df"),
    info = "a row with a printed df has stronger evidence than a value match and must keep its own provenance")
})
