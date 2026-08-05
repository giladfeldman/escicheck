# v0.6.18 -- a post-hoc pairwise t-test that reprints the OMNIBUS ANOVA error
# df must not be scored against N = df + 2.
#
# Source: 2026-08-04 escicheck-iterate cycle 2, Sonnet canary audit of
# collabra.90203; reproduced at HEAD before fixing. Design reviewed by Codex
# (2026-08-04), which REFUTED the first draft of the rule -- see below.
#
# WHAT WENT WRONG
#
# The paper runs a 3-level omnibus `F(2, 998)` (so N = 2 + 998 + 1 = 1001,
# ~334/cell) and then reports pairwise contrasts between TWO of those three
# levels. The stats package reprints the omnibus error df:
#
#     t(998) = 2.46, p = .041, d = 0.19 [0.04, 0.35]
#
# A two-level contrast cannot have 998 error df -- it uses ~2/3 of the sample.
# Binding N = df + 2 = 1000 recomputes d = 0.1556 against a reported 0.19
# (delta 0.0344) and fires WARN + an INCONSISTENT CI flag. Both are FALSE
# POSITIVES: at the true contrast N = 669 (335 + 334, per the gold's own notes)
# d = 0.1902 (delta 0.0002) and the computed CI [0.038, 0.342] reproduces the
# reported [0.04, 0.35] almost exactly. The paper is self-consistent; the tool
# was wrong.
#
# WHY THE OBVIOUS RULE IS WRONG (cross-model review, reproduced locally)
#
# The first draft fired whenever a t-row's df equalled some omnibus F's df2 in
# the same document. Codex flagged a false-fire case; it REPRODUCED at HEAD:
# a 3-arm paper that ALSO reports a legitimate two-group comparison of the full
# sample (n1 = n2 = 500) has a genuine t(998) for which N = 1000 is correct.
# Same-document df equality is therefore only a HYPOTHESIS.
#
# THE RULE AS SHIPPED: omnibus-df matching PROPOSES a candidate N; the row's
# OWN reported effect size decides. The omnibus-contrast N is adopted only when
# it explains the reported effect BETTER than df + 2 does. When neither variant
# explains the reported effect, the row stays flagged -- suppressing the WARN
# unconditionally would mask a genuine reporting error (Codex Q3).

test_that("post-hoc t reusing the omnibus error df is scored against the contrast N", {
  txt <- paste0(
    "A total of N = 1001 participants were randomly assigned to one of three ",
    "conditions. We found some support for a main effect of Identifiability, ",
    "F(2, 998) = 3.91, p = .02. Post hoc pairwise comparisons showed that we ",
    "found support for differences between the statistical and the joint ",
    "condition, t(998) = 2.46, p = .041, d = 0.19 [0.04, 0.35]."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]

  # The contrast N (2 * 1001/3 = 667) must beat the naive df + 2 = 1000.
  n_bound <- suppressWarnings(as.numeric(row$N[1]))
  expect_lt(n_bound, 1000)
  expect_equal(as.character(row$N_source[1]), "omnibus_df_contrast")

  # The delta collapses, so the false WARN is gone.
  delta <- suppressWarnings(as.numeric(row$delta_effect[1]))
  expect_lt(delta, 0.02)
  expect_false(identical(as.character(row$status[1]), "WARN"))

  # The inference must be disclosed, never published as observed fact.
  expect_match(as.character(row$uncertainty_reasons[1]), "omnibus")
})

test_that("a genuine two-group t with the same df is NOT rewritten (Codex false-fire case)", {
  # REPRODUCED at HEAD before the fix: a 3-arm ANOVA plus a legitimate
  # full-sample two-group comparison whose df really is 998.
  txt <- paste0(
    "A total of N = 1001 participants were randomized to three conditions. ",
    "The omnibus test was significant, F(2, 998) = 3.91, p = .02. ",
    "In a separate two-group demographic comparison of the full sample ",
    "(n1 = 500, n2 = 500), age differed, t(998) = 2.46, p = .014, d = 0.156."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]

  # Explicit group sizes are an OBSERVATION and outrank any df hypothesis.
  # (N here is the stated study total 1001, not n1 + n2 -- what matters for
  # this guard is that the omnibus-contrast inference did NOT fire and the
  # row still verifies cleanly off its explicit group sizes.)
  expect_equal(suppressWarnings(as.numeric(row$n1[1])), 500)
  expect_equal(suppressWarnings(as.numeric(row$n2[1])), 500)
  expect_false(identical(as.character(row$N_source[1]), "omnibus_df_contrast"))
  expect_lt(suppressWarnings(as.numeric(row$delta_effect[1])), 0.02)
})

test_that("a reported effect matching NEITHER variant stays flagged (no blanket suppression)", {
  # Codex Q3: suppressing the WARN whenever an omnibus df matches would mask a
  # genuine reporting error. Here d = 0.30 fits neither N = 1000 (d = 0.1556)
  # nor the contrast N = 667 (d = 0.1905), so the row must NOT be quietly
  # downgraded to a clean verdict.
  txt <- paste0(
    "A total of N = 1001 participants were randomly assigned to one of three ",
    "conditions. The omnibus effect was significant, F(2, 998) = 3.91, p = .02. ",
    "Post hoc, the statistical and joint conditions differed, ",
    "t(998) = 2.46, p = .041, d = 0.30."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]

  delta <- suppressWarnings(as.numeric(row$delta_effect[1]))
  expect_gt(delta, 0.05)
  expect_true(as.character(row$status[1]) %in% c("WARN", "ERROR"))
})

test_that("an honest pairwise df in the same paper is untouched", {
  # collabra.90203 also reports genuine pairwise df -- t(667) / t(668) -- which
  # PASS with MATCH CIs in the real corpus render. The omnibus rule must not
  # disturb them: 667 != 998, so no hypothesis is even raised.
  txt <- paste0(
    "A total of N = 1001 participants were randomly assigned to one of three ",
    "conditions. We found a main effect, F(2, 998) = 3.91, p = .02. ",
    "Perceived impact was higher in the identifiable victim condition, ",
    "t(667) = 3.67, p < .001, d = 0.28, 95% CI [0.13, 0.44]."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]
  expect_false(identical(as.character(row$N_source[1]), "omnibus_df_contrast"))
})

test_that("a study-total N in local context does not outrank the row's own df", {
  # FOUND 2026-08-04c while writing the guard above -- a SEPARATE defect of the
  # same class v0.6.17 fixed for `global_text`, still open for `local_context`.
  #
  # A paper states its total in the participants sentence and reports a
  # pairwise contrast a couple of sentences later. df = 667 fixes N at 669
  # (independent) or 668 (paired); the scraped study total 1001 is impossible
  # for THIS test. Binding it computes d = 0.2320 against a reported 0.28
  # (delta 0.048) -> FALSE WARN plus a false CI mismatch. The row's own
  # uncertainty text even said "Reported N (1001) is larger than expected
  # (668-669) for df=667" -- and then used 1001 anyway. df is structurally
  # authoritative; a scraped document total is not.
  txt <- paste0(
    "Participants (N = 1001) were recruited online and randomly assigned. ",
    "They completed the measures in a fixed order and were then debriefed. ",
    "Perceived impact was higher in the identifiable victim condition ",
    "(M = 3.39, SD = 1.24) than in the statistical condition (M = 3.10, ",
    "SD = 1.31), t(667) = 3.67, p < .001, d = 0.28, 95% CI [0.13, 0.44]."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]
  n_bound <- suppressWarnings(as.numeric(row$N[1]))
  expect_lt(n_bound, 1001)
  expect_lt(suppressWarnings(as.numeric(row$delta_effect[1])), 0.02)
  expect_false(identical(as.character(row$status[1]), "WARN"))
})

test_that("a 2-group omnibus F(1, df) never triggers the contrast inference", {
  # An F with df1 = 1 has only TWO groups, so a pairwise contrast uses the FULL
  # sample and df + 2 is correct. The rule requires df1 >= 2 (3+ groups).
  txt <- paste0(
    "A total of N = 1001 participants took part. The main effect was ",
    "significant, F(1, 999) = 4.10, p = .04. The two conditions differed, ",
    "t(999) = 2.02, p = .043, d = 0.128."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]
  expect_false(identical(as.character(row$N_source[1]), "omnibus_df_contrast"))
})
