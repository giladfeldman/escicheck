# v0.6.7: consume docpluck v2.4.98 typed table fields.
#
#   DP-3: docpluck now TYPES the partial-eta^2 column as `fields.eta2` on a
#   structurally-identified F-test/ANOVA table (the body-text glyph stays
#   stripped -- no ToUnicode CMap -- so the table is the only recoverable source).
#   The consumer binds it as `etap2`: verified from F+df1+df2 when present, else
#   surfaced as an honest NOTE carrying the eta2 + CI.
#
#   DP-5: a typed `fields.r` correlation cell with a reported CI but no df/N
#   (docpluck mis-binds the per-row n to the comparison column) now surfaces as
#   a NOTE with the r + CI (the estimate-in-CI invariant is checked) instead of
#   collapsing to a bare SKIP.
#
# Field shapes mirror live docpluck v2.4.98 `flattened_rows[]` for
# collabra.90203 Tables 8/9/10 (captured 2026-06-26).

tr <- function(label, row_label, fields, group = NULL, row_idx = 0L) {
  if (!is.null(group)) fields$group <- group
  list(table_id = "t1", label = label, row_label = row_label,
       row_idx = row_idx, fields = fields)
}

# ---- DP-3: eta2 binding in flattened_rows_to_parsed ------------------------

test_that("typed fields.eta2 on an F row binds as etap2 (parse level)", {
  p <- effectcheck:::flattened_rows_to_parsed(list(
    # F + df + eta2  (collabra.90203 Table 9 H5d: F(2,998)=0.792, etap2=.002)
    tr("Table 9", "Replication",
       list(F = 0.792, df1 = 2, df2 = 998, eta2 = 0.002,
            p = 0.453, p_op = "=", CI_lower = 0, CI_upper = 0.009))
  ))
  expect_equal(nrow(p), 1L)
  expect_equal(p$test_type, "F")
  expect_equal(p$df1, 2); expect_equal(p$df2, 998)
  expect_equal(p$stat_value, 0.792)
  expect_equal(p$effect_reported_name, "etap2")
  expect_equal(p$effect_reported, 0.002)
})

test_that("an untyped est on an F row is STILL left unbound (no regression)", {
  # docpluck's old untyped-`est` form must not be misread as an effect.
  p <- effectcheck:::flattened_rows_to_parsed(list(
    tr("Table 9", "Rep", list(F = 0.01, df1 = 1, df2 = 114, est = 0.0,
                              p = 0.94, p_op = "=", CI_lower = 0, CI_upper = 0.002))
  ))
  expect_equal(p$test_type, "F")
  expect_true(is.na(p$effect_reported))
})

test_that("an eta2-only cell (no usable F) becomes a table_estimate naming etap2", {
  # collabra.90203 Table 8 H2a: docpluck delivers eta2 + CI but a blank F cell.
  p <- effectcheck:::flattened_rows_to_parsed(list(
    tr("Table 8", "Replication",
       list(eta2 = 0.001, p = 0.419, p_op = "=", CI_lower = 0, CI_upper = 0.011))
  ))
  expect_equal(nrow(p), 1L)
  expect_equal(p$test_type, "table_estimate")
  expect_equal(p$effect_reported_name, "etap2")
  expect_equal(p$effect_reported, 0.001)
  expect_equal(p$ciU_reported, 0.011)
})

# ---- DP-3: end-to-end verdicts --------------------------------------------

test_that("F + df + eta2 table row is independently VERIFIED (PASS), not NOTE", {
  r <- as.data.frame(check_text("", table_rows = list(
    tr("Table 9", "Replication",
       list(F = 0.792, df1 = 2, df2 = 998, eta2 = 0.002,
            p = 0.453, p_op = "=", CI_lower = 0, CI_upper = 0.009))
  )))
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type, "F")
  expect_equal(r$result_context, "table")
  expect_equal(r$effect_reported_name, "etap2")
  expect_equal(r$effect_reported, 0.002)
  # partial_eta2 recomputed from F(2,998) ~= 0.0016 -> rounds to .002 reported
  expect_true(r$status %in% c("PASS", "OK"))
  expect_false(r$status %in% c("SKIP", "ERROR"))
})

test_that("eta2 table row WITHOUT df routes to NOTE surfacing the eta2 + CI", {
  r <- as.data.frame(check_text("", table_rows = list(
    tr("Table 8", "Replication",
       list(eta2 = 0.003, p = 0.228, p_op = "=", CI_lower = 0, CI_upper = 0.012))
  )))
  expect_equal(nrow(r), 1L)
  expect_equal(r$status, "NOTE")
  expect_equal(r$effect_reported_name, "etap2")
  expect_equal(r$effect_reported, 0.003)
  expect_equal(r$result_context, "table")
})

# ---- DP-5: r-with-CI surfaces as NOTE, not SKIP ---------------------------

test_that("r cell with a CI but no df/N is a NOTE (not a bare SKIP)", {
  # collabra.90203 Table 10 Replication: r=.63, CI [.53,.72], n mis-bound by
  # docpluck to the comparison column so the Replication cell has no n.
  r <- as.data.frame(check_text("", table_rows = list(
    tr("Table 10", "Identifiable/ Explicit learning",
       list(r = 0.63, p = 0.001, p_op = "<", CI_lower = 0.53, CI_upper = 0.72),
       group = "Replication")
  )))
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type, "r")
  expect_equal(r$status, "NOTE")
  expect_false(r$status == "SKIP")
  # the r is adopted as its own effect and surfaced
  expect_equal(r$effect_reported_name, "r")
  expect_equal(r$effect_reported, 0.63)
  expect_equal(r$ciL_reported, 0.53)
})

test_that("r cell with NO CI and no df/N stays the conservative no-CI NOTE", {
  # the surfacing exclusion is gated on a reported CI: a bare r with neither a
  # CI nor df/N must still route through the original 'without df or N' guard.
  r <- as.data.frame(check_text("", table_rows = list(
    tr("Table X", "row", list(r = 0.42, p = 0.01, p_op = "="))
  )))
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type, "r")
  # no CI to check, nothing recomputable -> extraction-only SKIP (unchanged)
  expect_true(r$status %in% c("SKIP", "NOTE"))
})

# ---- DP-3 follow-on: F + CI dedup against a glyph-stripped prose row -------

test_that("a table F+eta2 row that restates a prose F+CI row is deduped", {
  # The prose body reports the same H-test but docpluck stripped its eta^2
  # glyph, so the prose row has only {F, CI}. The table row carries
  # {F, eta2, CI}. They must collapse on the (F + CI) signature so the same
  # finding is not double-counted.
  prose <- "We found no support for the interaction, F(1, 666) = 0.04, p = .842, 95% CI [0.00, 0.01]."
  r_prose <- as.data.frame(check_text(prose))
  r_full  <- as.data.frame(check_text(prose, table_rows = list(
    tr("Table 9", "Replication",
       list(F = 0.04, df1 = 1, df2 = 114, eta2 = 0,
            p = 0.842, p_op = "=", CI_lower = 0, CI_upper = 0.01))
  )))
  # the table row collapses into the prose row: same count
  expect_equal(nrow(r_full), nrow(r_prose))
})

test_that("a table F+eta2 row with a DISTINCT CI is NOT deduped (kept)", {
  # Same F value but a different CI = a different test -> must survive.
  prose <- "Main effect, F(1, 666) = 0.01, p = .940, 95% CI [0.00, 0.002]."
  r_prose <- as.data.frame(check_text(prose))
  r_full  <- as.data.frame(check_text(prose, table_rows = list(
    tr("Table 8", "Replication",
       list(F = 0.01, eta2 = 0, p = 0.923, p_op = "=",
            CI_lower = 0, CI_upper = 0.003))   # CI .003 != prose .002
  )))
  expect_equal(nrow(r_full), nrow(r_prose) + 1L)
})
