# v0.6.11: two fixes from the 2026-07-01 escicheck-iterate cycle-2 canary audit.
#
# E-origcol (collabra.57785 Table 8) — the comparison/original-study column is not
# always labeled "Original study/article". Table 8 labels its two columns
# "Replication Effect and CI" / "Original Effect and CI", so the v0.6.6 filter regex
# (which required original + article|study|paper) missed it and every Table-8 finding
# was emitted TWICE (the Original-column duplicate leaked as a spurious own-result).
# The widened regex also matches "Original Effect/Result/Finding/Value/Cohen-d/r/F"
# and an "Original … CI/[stat]" column header, while still keeping the paper's own
# "Replication …" column and any substantive condition label.
#
# E-mdhl-N (PROSECCO md_hl) — a Hodges-Lehmann median difference has no recoverable N
# from a sentence; the v0.6.3 fix that stopped RR/rdpct rows inheriting an unrelated
# global N was never extended to md_hl, so a md_hl row attached a bled
# global_text/extended_context N (N=106 shown on two distinct outcomes the source
# never quantifies). md_hl now clears a non-co-located N.

tr <- function(label, row_label, fields, group = NULL, row_idx = 0L) {
  if (!is.null(group)) fields$group <- group
  list(table_id = "t1", label = label, row_label = row_label,
       row_idx = row_idx, fields = fields)
}

test_that("E-origcol: an 'Original Effect and CI' comparison column is dropped", {
  p <- effectcheck:::flattened_rows_to_parsed(list(
    tr("Table 8", "Importance", list(t = 3.93, d = 0.29, df = 200, p = 0.001, p_op = "<"),
       group = "Original Effect and CI"),
    tr("Table 8", "Importance", list(t = 6.79, d = 0.25, df = 200, p = 0.001, p_op = "<"),
       group = "Replication Effect and CI")
  ))
  # Only the Replication-column row survives.
  expect_equal(nrow(p), 1L)
  expect_true(abs(p$stat_value[1] - 6.79) < 1e-9)
  expect_false(any(abs(as.numeric(p$stat_value) - 3.93) < 1e-9, na.rm = TRUE))
})

test_that("E-origcol: 'Original Effect' as the ROW_LABEL is also dropped", {
  p <- effectcheck:::flattened_rows_to_parsed(list(
    tr("Table 8", "Original Effect", list(F = 6.75, est = 0.04, p = 0.001, p_op = "<")),
    tr("Table 8", "Replication Effect", list(F = 3.91, est = 0.008, p = 0.02, p_op = "="))
  ))
  expect_equal(nrow(p), 1L)
  expect_true(abs(p$stat_value[1] - 3.91) < 1e-9)
})

test_that("E-origcol: a substantive label containing the word 'original' is NOT dropped", {
  # Guard against over-matching: a real condition label that merely contains
  # "original" (with no stat-column word) must survive.
  p <- effectcheck:::flattened_rows_to_parsed(list(
    tr("Table 3", "Original Authentic Self condition", list(t = 2.10, d = 0.30, df = 100, p = 0.038, p_op = "="))
  ))
  expect_equal(nrow(p), 1L)
  expect_true(abs(p$stat_value[1] - 2.10) < 1e-9)
})

test_that("E-mcnemar-OR: a McNemar test reported as an odds ratio is extracted as NOTE", {
  # collabra.37122: "We also conducted a McNemar test ... OR = 0.18, 95% CI
  # [0.10, 0.29], p < .001". No chi-square value, so the generic path produced NO
  # row and the OR was silently dropped (all 4 of the paper's McNemar tests).
  # Now a `mcnemar_or` extraction-only NOTE surfaces the OR + CI + p.
  txt <- paste0(
    "We also conducted a McNemar test, and found support for the association ",
    "between temporal distance and action-inaction regret, OR = 0.18, ",
    "95% CI [0.10, 0.29], p < .001."
  )
  res <- effectcheck::check_text(txt)
  m <- res[!is.na(res$test_type) & res$test_type == "mcnemar_or", ]
  expect_equal(nrow(m), 1L)
  expect_equal(m$effect_reported[1], 0.18)
  expect_equal(m$effect_reported_name[1], "OR")
  expect_equal(m$ciL_reported[1], 0.10)
  expect_equal(m$ciU_reported[1], 0.29)
  expect_equal(m$status[1], "NOTE")     # extraction-only, never SKIP/PASS/WARN
})

test_that("E-mcnemar-OR: a plain OR with no McNemar mention is NOT mcnemar_or", {
  # Guard: an OR reported without "McNemar" stays its normal classification
  # (e.g. an OR on a chi-square or a bare OR), not mcnemar_or.
  res <- effectcheck::check_text("A logistic regression gave OR = 2.10, 95% CI [1.3, 3.4], p = .002.")
  tt <- if ("test_type" %in% names(res)) res$test_type else character(0)
  expect_false(any(tt == "mcnemar_or", na.rm = TRUE))
})

test_that("E-subgroupN: two per-group 'N = ' sizes in a split bind as n1/n2", {
  # collabra.74820: "we divided the sample in high CA (score >= 3, N = 223) and low
  # CA (score <= 2, N = 19) ... t(240) = -2.32, p = .029". The two unsubscripted
  # subgroup N's must bind as n1/n2 (total = sum = 242), NOT leave the first (223)
  # bound as the total (which fired a bogus "N=223 implausibly small for df=240"
  # warning and forced an equal-split Cohen's d).
  txt <- paste0(
    "To check contingency awareness, we divided the sample in high CA ",
    "(score >= 3, N = 223) and low CA (score <= 2, N = 19). An independent-samples ",
    "t-test showed a significant difference, t(240) = -2.32, p = .029."
  )
  res <- effectcheck::check_text(txt)
  tr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_true(nrow(tr) >= 1)
  expect_equal(sort(c(tr$n1[1], tr$n2[1])), c(19, 223))
  expect_equal(tr$N[1], 242)            # total = sum of the two subgroups
  # The bogus "implausibly small N" warning must NOT fire (223 is a subgroup, not N).
  expect_false(any(grepl("implausibly small", tr$uncertainty_reasons), na.rm = TRUE))
})

test_that("E-subgroupN: a single total 'N = ' is NOT split into subgroups", {
  # Guard against over-firing: one N value (no second group N) stays the total.
  txt <- "An independent-samples t-test on the full sample (N = 200) gave t(198) = 2.10, p = .037."
  res <- effectcheck::check_text(txt)
  tr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_true(nrow(tr) >= 1)
  expect_true(is.na(tr$n1[1]) || is.na(tr$n2[1]))   # not split
  expect_equal(tr$N[1], 200)
})

test_that("E-mdhl-N: a md_hl row does not inherit an unrelated global N", {
  # A Hodges-Lehmann median difference reported with an IQR + CI but no per-arm n in
  # its own clause; a global "N = 402" elsewhere must NOT be attached to it.
  txt <- paste0(
    "Overall, 402 participants were randomized. ",
    "Median duration of hospital admission was significantly shorter in the PSA ",
    "group compared to the GA group; median difference -140.0; 95% CI -169.0 to ",
    "-109.0; p < .001."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "md_hl", ]
  expect_true(nrow(rr) >= 1)
  # N must be NA (not the bled 402) — the source does not report a per-arm n here.
  expect_true(all(is.na(rr$N)))
})
