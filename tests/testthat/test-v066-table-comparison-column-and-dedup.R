# v0.6.6 (E-D1 + E-D2): two fixes to the Mode B docpluck table-row consumer,
# surfaced by the 2026-06-25 escicheck-iterate canary audit (cycle 1) against
# collabra.90203 (Identifiable Victim Effect replication, which reproduces Small
# et al. 2007's original statistics in a "Target article" column).
#
# E-D1: a flattened table row whose row_label / group marks it as the comparison
# / original-study column ("Target article", "Original study", ...) is NOT one
# of THIS paper's results and must be dropped — not extracted + checked + counted
# as the audited paper's finding (the Target-article F = 6.75 / 5.32 surfaced as
# spurious own-result rows).
#
# E-D2: a `table_estimate` row (typed `est` cell, no test statistic) that
# restates a body result whose full numeric signature differs (the body row
# carries the test statistic + a stripped effect, the table cell carries the
# effect + CI) is collapsed when its EXACT reported CI pair matches a prose row's.

tr <- function(label, row_label, fields, group = NULL, row_idx = 0L) {
  if (!is.null(group)) fields$group <- group
  list(table_id = "t1", label = label, row_label = row_label,
       row_idx = row_idx, fields = fields)
}

test_that("E-D1: a 'Target article' comparison-column row is not extracted", {
  p <- effectcheck:::flattened_rows_to_parsed(list(
    tr("Table 8", "Target article", list(F = 6.75, est = 0.04, p = 0.001, p_op = "<",
                                          CI_lower = 0.01, CI_upper = 0.07)),
    tr("Table 8", "Replication",    list(F = 3.91, est = 0.008, p = 0.02, p_op = "=",
                                          CI_lower = 0.0, CI_upper = 0.021))
  ))
  # Only the Replication row survives.
  expect_equal(nrow(p), 1L)
  expect_true(abs(p$stat_value[1] - 3.91) < 1e-9)
  expect_false(any(abs(as.numeric(p$stat_value) - 6.75) < 1e-9, na.rm = TRUE))
})

test_that("E-D1: 'Target article' as the GROUP field is also dropped", {
  p <- effectcheck:::flattened_rows_to_parsed(list(
    tr("Table 10", "Identifiable/No explicit", list(r = 0.55, n = 161), group = "Target article"),
    tr("Table 10", "Identifiable/No explicit", list(r = 0.58, n = 161, CI_lower = 0.58, CI_upper = 0.80), group = "Replication")
  ))
  expect_equal(nrow(p), 1L)
  expect_true(abs(p$stat_value[1] - 0.58) < 1e-9)
})

test_that("E-D1: a substantive (non-comparison) row is NOT dropped", {
  p <- effectcheck:::flattened_rows_to_parsed(list(
    tr("Table 8", "Replication", list(F = 3.91, est = 0.008, p = 0.02, p_op = "=",
                                       CI_lower = 0.0, CI_upper = 0.021)),
    tr("Table 9", "H5a", list(F = 0.09, df1 = 1, df2 = 666, p = 0.764, p_op = "="))
  ))
  expect_equal(nrow(p), 2L)
})

test_that("E-D2: a table_estimate restating a body CI is collapsed", {
  # Body H1b: F(2,998) = 3.91, eta^2p stripped (no effect), CI [.000, .021].
  prose <- "We found some support for a main effect of Identifiability (H1b), F(2, 998) = 3.91, p = .02, 95% CI [.000, .021]."
  # Table 8 estimate cell for the SAME result: eta^2p = .01, CI [0, .021] (rounds .008 -> .01).
  tbl <- list(tr("Table 8", "Replication",
                 list(est = 0.01, CI_lower = 0.0, CI_upper = 0.021, p = 0.02, p_op = "=")))
  combined <- as.data.frame(effectcheck::check_text(prose, table_rows = tbl))
  # The table_estimate row sharing the body CI [0, .021] is dropped; the body F row remains.
  te <- combined[!is.na(combined$test_type) & combined$test_type == "table_estimate", ]
  expect_equal(nrow(te), 0L)
  expect_true(any(!is.na(combined$stat_value) & abs(combined$stat_value - 3.91) < 1e-9))
})

test_that("E-D2 guard: a table_estimate with a DIFFERENT CI is kept", {
  prose <- "We found a main effect, F(2, 998) = 3.91, p = .02, 95% CI [.000, .021]."
  # A genuinely different estimate (different CI) must NOT be merged away.
  tbl <- list(tr("Table 8", "Other row",
                 list(est = 0.05, CI_lower = 0.02, CI_upper = 0.10, p = 0.01, p_op = "=")))
  combined <- as.data.frame(effectcheck::check_text(prose, table_rows = tbl))
  te <- combined[!is.na(combined$test_type) & combined$test_type == "table_estimate", ]
  expect_equal(nrow(te), 1L)
})
