# v0.6.12: two fixes from the 2026-07-02 escicheck-iterate cycle-1 canary re-audit
# (independent Sonnet-watches-Opus over the v0.6.11 canary set).
#
# E-ownclause-N (collabra.57785 loc 170) — a t-test row's N must be taken from the
# row's OWN sub-chunk when its clause states one, not from the first N in the wider
# +/-2-sentence context window. The clause "(M = 4.90, SD = 1.42, N = 743) ...
# (M = 4.11, SD = 1.44, N = 743; t(742) = 12.24, ...)" states N = 743 twice, yet the
# parser bound N = 350 from the PRECEDING sentence (loc 167 "N = 350 ... N = 393"),
# check.R rejected it as implausibly small for df=742, and fell back to the
# independent default N = df + 2 = 744 — a fabricated N. Now the own-clause N wins
# (N_source = "own_clause").
#
# E-repcol-dedup (collabra.57785 Table 8 "Replication" column) — a test-statistic-
# bearing table row that carries NO CI of its own (only the statistic + df) is not
# matched by any of the three CI-based table-vs-prose dedup passes, so it leaked as a
# spurious duplicate of the identical prose finding. The v0.6.11 E-origcol fix dropped
# the "(Original)" column but left the kept "(Replication)" rows undeduplicated against
# their prose twins (8 double-counts). A CI-less table F/t/r row now collapses when its
# (test_type + statistic + df1) matches a prose row's.

tr <- function(label, row_label, fields, group = NULL, row_idx = 0L) {
  if (!is.null(group)) fields$group <- group
  list(table_id = "t1", label = label, row_label = row_label,
       row_idx = row_idx, fields = fields)
}

test_that("E-ownclause-N: a t-test binds the N in its own clause, not a neighbor's", {
  # Two sentences. The FIRST reports a between-subjects contrast with N = 350 / N = 393.
  # The SECOND (the row under test) is a paired within-subjects t whose own clause
  # states N = 743 twice, line-broken and positioned after the M/SD. The generic
  # "first N in the context window" scan would grab 350 from the previous sentence;
  # the own-clause scan must bind 743.
  txt <- paste0(
    "In addition, participants in the experiential condition were more satisfied ",
    "with their purchase (M = 8.21, SD = 1.21, N = 350), than participants in the ",
    "material condition (M = 8.00, SD = 1.11, N = 393), t(741) = 2.51, p = .01, ",
    "d = 0.18, 95% CI [0.04, 0.33]. ",
    "We conducted two-tailed paired t-tests based on a within-subjects design. ",
    "Participants were more willing to exchange their memories of material purchases ",
    "(M = 4.90, SD = 1.42, N = 743) than their memories of experiences ",
    "(M = 4.11, SD = 1.44, N = 743; t(742) = 12.24, p < .001, d = 0.55, ",
    "95% CI [0.47, 0.62])."
  )
  res <- effectcheck::check_text(txt)
  row <- res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) - 12.24) < 1e-6, ]
  expect_equal(nrow(row), 1L)
  # The own-clause N = 743 is bound (NOT 350 from the neighbor, NOT the df+2=744 default).
  expect_equal(row$N[1], 743)
  expect_equal(row$N_source[1], "own_clause")
  # The spurious "implausibly small N" note must NOT fire (it fired when N=350 bled in).
  expect_false(any(grepl("implausibly small", row$uncertainty_reasons), na.rm = TRUE))
})

test_that("E-ownclause-N: a row with no N in its own clause still falls back to context", {
  # Guard: when the row's own sub-chunk has NO N, the +/-2-sentence context window is
  # still used (unchanged behavior). Here only the surrounding text carries N = 88.
  txt <- paste0(
    "The study enrolled N = 88 participants across both conditions. ",
    "Reaction times were faster after training, t(87) = 3.10, p = .003, d = 0.33."
  )
  res <- effectcheck::check_text(txt)
  row <- res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) - 3.10) < 1e-6, ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$N[1], 88)
  # N_source is a context source, not own_clause (the clause itself had no N).
  expect_true(row$N_source[1] %in% c("local_context", "extended_context"))
})

test_that("E-repcol-dedup: a CI-less table row restating a prose t-test is dropped", {
  # A prose paired t-test WITH a full report (t, df, d, CI) plus the same finding as a
  # bare "Replication" table cell (t + df only, no CI, no effect size). The table row
  # must collapse into the prose row.
  prose <- paste0(
    "To replicate Study 3A, we ran a paired t-test and found that participants had ",
    "greater insight into self after experiential purchases (M = 8.10, SD = 1.31, ",
    "N = 744) than material purchases (M = 7.20, SD = 1.40), t(742) = 17.61, ",
    "p < .001, d = 0.65, 95% CI [0.57, 0.73]."
  )
  # The docpluck "Replication" table cell for the SAME finding: statistic + df only.
  rows <- list(
    tr("Table 8", "3A: Insight into self", list(t = 17.61, df = 742, p = 0.001, p_op = "<"),
       group = "Replication Effect and CI")
  )
  res <- effectcheck::check_text(prose, table_rows = rows)
  # Exactly ONE row for t = 17.61 (the prose row); the table restatement is deduped.
  hits <- res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) - 17.61) < 1e-6, ]
  expect_equal(nrow(hits), 1L)
  # The surviving row is the richer prose row (has the effect size + CI).
  expect_false(is.na(hits$effect_reported[1]))
  expect_equal(hits$effect_reported[1], 0.65)
})

test_that("E-repcol-dedup: a genuinely table-only row (no prose twin) is KEPT", {
  # Guard: a CI-less table t-row with NO matching prose finding must survive — the
  # dedup must not drop a real table-only result. Here the prose reports t = 17.61 but
  # the table also has a distinct t = 6.79 / df = 742 that appears nowhere in prose.
  prose <- paste0(
    "We ran a paired t-test on insight, t(742) = 17.61, p < .001, d = 0.65, ",
    "95% CI [0.57, 0.73]."
  )
  rows <- list(
    tr("Table 8", "3A: Insight into self", list(t = 17.61, df = 742, p = 0.001, p_op = "<"),
       group = "Replication Effect and CI"),
    tr("Table 8", "Importance of memory exchange", list(t = 6.79, df = 742, p = 0.001, p_op = "<"),
       group = "Replication Effect and CI")
  )
  res <- effectcheck::check_text(prose, table_rows = rows)
  # t = 17.61 deduped to one; t = 6.79 (table-only) survives.
  expect_equal(nrow(res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) - 17.61) < 1e-6, ]), 1L)
  expect_equal(nrow(res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) - 6.79) < 1e-6, ]), 1L)
})

test_that("E-pairedci-unverifiable: a within-subjects d reported with a CI is UNVERIFIABLE, not INCONSISTENT", {
  # collabra.57785 loc 170: a paired (within-subjects) t reports d = 0.55 with a CI
  # [0.47, 0.62] the authors computed from the raw paired data. effectcheck cannot
  # reproduce that paired CI from t + df alone (no per-arm SDs / within-pair
  # correlation), so its computed CI is an independent-samples over-approximation.
  # The reported CI must therefore be UNVERIFIABLE, never a hard INCONSISTENT that
  # falsely implies the reported values are wrong.
  txt <- paste0(
    "We conducted two-tailed paired t-tests based on a within-subjects design. ",
    "Participants were more willing to exchange their memories of material purchases ",
    "(M = 4.90, SD = 1.42, N = 743) than their memories of experiences ",
    "(M = 4.11, SD = 1.44, N = 743; t(742) = 12.24, p < .001, d = 0.55, ",
    "95% CI [0.47, 0.62])."
  )
  res <- effectcheck::check_text(txt)
  row <- res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) - 12.24) < 1e-6, ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$design_inferred[1], "paired")
  expect_equal(row$ci_check_status[1], "UNVERIFIABLE")
  expect_false(identical(row$ci_check_status[1], "INCONSISTENT"))
})

test_that("E-pairedci-unverifiable: an INDEPENDENT-samples row can still be INCONSISTENT", {
  # Guard: the UNVERIFIABLE cap is scoped to within-subjects rows. A genuine
  # independent-samples t whose reported CI disagrees with the (correctly computable)
  # independent CI must still be able to surface as INCONSISTENT -- the fix must not
  # blanket-suppress CI mismatches.
  txt <- paste0(
    "An independent-samples t-test comparing the two groups, t(200) = 5.00, ",
    "p < .001, d = 0.70, 95% CI [0.15, 0.45]."
  )
  res <- effectcheck::check_text(txt)
  row <- res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) - 5.00) < 1e-6, ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$design_inferred[1], "independent")
  # Not forced to UNVERIFIABLE by the within-subjects cap (it is a real independent row).
  expect_false(identical(row$ci_check_status[1], "UNVERIFIABLE"))
})

test_that("E-corr-N-from-df: a Pearson r(df) with a bled N shows N = df + 2, not the bled N", {
  # collabra.57785 loc 173/175: a between-subjects subsample correlation r(348) = -0.36
  # is reported in a paragraph that also mentions the whole-study N = 743. df = 348
  # implies a 350-person subsample (Pearson: N = df + 2), so displaying N = 743 is
  # internally inconsistent with the very df printed next to the r. The N field must
  # reconcile to df + 2 = 350 (the CI/p were already computed from df, so PASS stands).
  txt <- paste0(
    "The full analysis sample was N = 743. Within the experiential condition, ",
    "the result showed a negative correlation, r(348) = -0.36, p < .001, ",
    "95% CI [-0.45, -0.26]."
  )
  res <- effectcheck::check_text(txt)
  row <- res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) + 0.36) < 1e-6, ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$df1[1], 348)
  expect_equal(row$N[1], 350)                 # df + 2, NOT the bled 743
  expect_equal(row$status[1], "PASS")         # verdict unaffected (computed from df)
})

test_that("E-corr-N-from-df: a consistent r(df) + N (N == df + 2) is left unchanged", {
  # Guard: no spurious correction / note when the reported N already equals df + 2.
  res <- effectcheck::check_text("A correlation, r(98) = .30, N = 100, p = .002.")
  row <- res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) - 0.30) < 1e-6, ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$N[1], 100)                 # unchanged
  expect_false(any(grepl("inconsistent with the correlation's own df",
                         row$uncertainty_reasons), na.rm = TRUE))
})

test_that("E-corr-N-from-df: the r-test multi-N candidate path (df inferred from N) is unaffected", {
  # Guard: when r has NO explicit df, N -> df derivation + best-N-by-p-value selection
  # must still run (the fix only fires for an EXPLICIT r(df) whose N disagrees).
  res <- effectcheck::check_text("N = 32. N = 959. r = .25, p < .001")
  row <- res[!is.na(res$stat_value), ]
  expect_equal(nrow(row), 1L)
  expect_equal(row$N[1], 959)
  expect_true(grepl("Multiple sample sizes", row$assumptions_used[1]))
})

test_that("E-repcol-dedup: a CI-BEARING table row is NOT collapsed on stat+df alone", {
  # Guard: the new stat+df pass is scoped to CI-LESS table rows. A table row that DOES
  # carry a CI is handled by the stricter stat+CI+p pass, and a distinct finding that
  # merely shares a statistic value with a prose row (but has its own CI) must not be
  # dropped by the looser stat+df key. Different df here keeps them distinct anyway.
  prose <- "A paired t-test gave t(100) = 2.50, p = .014, d = 0.25, 95% CI [0.05, 0.45]."
  rows <- list(
    tr("Table 2", "Other outcome", list(t = 2.50, df = 200, p = 0.013, p_op = "=",
                                          CI_lower = 0.10, CI_upper = 0.40))
  )
  res <- effectcheck::check_text(prose, table_rows = rows)
  # Both survive: the table row has a CI + a different df, so it is not a restatement.
  expect_true(nrow(res[!is.na(res$stat_value) & abs(as.numeric(res$stat_value) - 2.50) < 1e-6, ]) >= 2)
})
