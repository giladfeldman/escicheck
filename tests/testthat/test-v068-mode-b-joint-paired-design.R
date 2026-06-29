# v0.6.8 (E-A3): a docpluck Mode B flattened table row whose column label marks a
# JOINT-evaluation (within-subjects) condition must be classified
# design_inferred = "paired"/"within", not "independent". The design lives in the
# table NOTE ("Paired-samples t for joint"), which docpluck does NOT carry onto the
# flattened row -- the row carries only its `group`/`row_label` column label. So the
# Mode B consumer (flattened_rows_to_parsed) injects a design phrase derived from the
# column label into the row's context_window, and check.R's existing t-test design
# detector does the rest. The mirror case: a SEPARATE-evaluation column label is a
# between-subjects / independent signal.
#
# Surfaced by the 2026-06-29 escicheck-iterate cycle-1 canary audit against
# collabra.77859 (Less Is Better) + collabra.57785: Table-3 joint rows t(131)=0.95 /
# t(131)=1.27 were tagged design_inferred = "independent" though the gold marks them
# within-subjects (paired-samples t for joint).

mk_row <- function(group, t, df, d, p, ciL, ciU, table_id = "camelot_t10", row_label = "Attractive") {
  list(
    table_id = table_id,
    row_label = row_label,
    fields = list(
      group = list(group),
      t = list(t), df = list(df), d = list(d),
      p = list(p), p_op = list("="),
      CI_lower = list(ciL), CI_upper = list(ciU)
    )
  )
}

test_that("a Mode B joint-evaluation t-row is classified paired/within, not independent", {
  rows <- list(mk_row("Joint", 0.95, 131, 0.08, 0.344, -0.09, 0.25))
  res <- effectcheck::check_text("", table_rows = rows)
  jr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 0.95) < 1e-9, ]
  expect_true(nrow(jr) >= 1)
  expect_true(any(jr$design_inferred %in% c("paired", "within"), na.rm = TRUE))
  expect_false(any(jr$design_inferred == "independent", na.rm = TRUE))
  # The joint row's effect, typed generically as `d` by docpluck, is named `dz`
  # to match the within-subjects design (table note: "d_z for paired").
  expect_true(any(jr$effect_reported_name == "dz", na.rm = TRUE))
  expect_false(any(jr$effect_reported_name == "d", na.rm = TRUE))
})

test_that("a Mode B separate-evaluation t-row stays independent/between", {
  rows <- list(mk_row("Separate", 5.65, 259.58, 0.69, 0.001, 0.44, 0.94))
  res <- effectcheck::check_text("", table_rows = rows)
  sr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 5.65) < 1e-9, ]
  expect_true(nrow(sr) >= 1)
  expect_true(any(sr$design_inferred %in% c("independent", "between"), na.rm = TRUE))
  expect_false(any(sr$design_inferred == "paired", na.rm = TRUE))
  # A between/separate row keeps the plain `d` effect name (not relabeled dz).
  expect_true(any(sr$effect_reported_name == "d", na.rm = TRUE))
  expect_false(any(sr$effect_reported_name == "dz", na.rm = TRUE))
})

test_that("a PROSE t-test reporting dz is paired even when a neighboring sentence names Welch", {
  # collabra.77859 Study 2: the joint-condition t(131)=6.92 reports dz=0.60 (a
  # within-subjects effect) but its context window bleeds "Welch's t(198.52)" from
  # the PRECEDING separate-evaluation sentence. A definitive-independent signal that
  # bled from a neighbor must not override a row whose OWN clause reports a paired
  # effect family. (The mirror guard: a row reporting a plain d with a same-clause
  # Welch is still independent -- covered by the 57785 t(741)=5.36 production path.)
  txt <- paste0(
    "In the separate evaluation condition we found no support for willingness to pay ",
    "more for the smaller cup than the larger cup, Welch's t(198.52) = 0.47, p = .640, ",
    "(d = 0.06, 95% CI [-0.18, 0.30]). ",
    "In the joint condition, as predicted, participants were willing to pay more for ",
    "the larger amount than the smaller amount, t(131) = 6.92, p < .001, ",
    "(dz = 0.60, 95% CI [0.42, 0.79])."
  )
  res <- effectcheck::check_text(txt)
  jr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 6.92) < 1e-9, ]
  expect_true(nrow(jr) >= 1)
  expect_true(any(jr$design_inferred == "paired", na.rm = TRUE))
  expect_false(any(jr$design_inferred == "independent", na.rm = TRUE))
  # And the neighboring Welch row stays independent.
  wr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 0.47) < 1e-9, ]
  expect_true(nrow(wr) >= 1)
  expect_true(any(wr$design_inferred == "independent", na.rm = TRUE))
})

test_that("a plain-d Welch t-test stays independent even if a paired t is discussed nearby", {
  # The 57785 t(741)=5.36 production case in synthetic form: a Welch t reporting a
  # plain d must stay independent. The own_reports_paired_es guard must NOT fire
  # (no dz/dav/drm in this row's clause), so the Welch signal still wins.
  txt <- paste0(
    "We also ran a paired t-test on the within-subjects measure. ",
    "We ran an independent Welch's t-test comparing the conditions, ",
    "t(741) = 5.36, p < .001, d = 0.39, 95% CI [0.25, 0.54]."
  )
  res <- effectcheck::check_text(txt)
  wr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 5.36) < 1e-9, ]
  expect_true(nrow(wr) >= 1)
  expect_true(any(wr$design_inferred == "independent", na.rm = TRUE))
  expect_false(any(wr$design_inferred == "paired", na.rm = TRUE))
})

test_that("a Mode B row with a neutral column label is not forced to a design", {
  # No within/between marker in the label -> the design hint is empty and the row
  # falls back to check.R's effect-type inference (a plain `d` -> independent by
  # default). The point of this test is that the hint does not over-trigger on an
  # unrelated label.
  rows <- list(mk_row("Overall", 3.10, 200, 0.30, 0.002, 0.10, 0.50,
                      table_id = "camelot_t99", row_label = "Manipulation check"))
  res <- effectcheck::check_text("", table_rows = rows)
  rr <- res[!is.na(res$test_type) & res$test_type == "t" &
              !is.na(res$stat_value) & abs(res$stat_value - 3.10) < 1e-9, ]
  expect_true(nrow(rr) >= 1)
  # No paired signal was injected (label has no joint/within marker).
  expect_false(any(grepl("joint condition", as.character(rr$context_window), fixed = TRUE)))
})
