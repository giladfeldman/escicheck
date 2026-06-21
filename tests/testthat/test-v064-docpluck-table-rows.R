# v0.6.4: Mode B docpluck table-row consumer (REQUEST_11 / docpluck v2.4.95).
# check_text(table_rows=) ingests structured flattened_rows[].fields, verifies
# verifiable ones and routes the rest to a conservative NOTE, tags
# result_context="table", and dedups table rows that restate a prose result.

tr <- function(label, row_label, fields, group = NULL, row_idx = 0L) {
  if (!is.null(group)) fields$group <- group
  list(table_id = "t1", label = label, row_label = row_label,
       row_idx = row_idx, fields = fields)
}

test_that("flattened_rows_to_parsed maps typed keys to test_type", {
  p <- effectcheck:::flattened_rows_to_parsed(list(
    tr("Table 5", "Separate", list(t = 6.23, df = 257, d = 0.76,
                                    p = 0.001, p_op = "<",
                                    CI_lower = 0.5, CI_upper = 1.02)),
    tr("Table 9", "Rep", list(F = 0.01, df1 = 1, df2 = 114, est = 0.0,
                              p = 0.94, p_op = "=", CI_lower = 0, CI_upper = 0.002)),
    tr("Table 10", "Joint", list(r = 0.63, n = 173, p = 0.001, p_op = "<",
                                 CI_lower = 0.53, CI_upper = 0.71)),
    tr("Table 2", "adjusted", list(est = -1.83, CI_lower = -11.2, CI_upper = 7.5),
       group = "ITT")
  ))
  expect_equal(nrow(p), 4L)
  expect_equal(p$test_type, c("t", "F", "r", "table_estimate"))
  expect_true(all(p$from_table))
  # t row: df + d bound
  expect_equal(p$stat_value[1], 6.23)
  expect_equal(p$df1[1], 257)
  expect_equal(p$effect_reported_name[1], "d")
  expect_equal(p$effect_reported[1], 0.76)
  # F row: df1/df2 bound, est (partial eta^2) intentionally NOT bound as effect
  expect_equal(p$df1[2], 1); expect_equal(p$df2[2], 114)
  expect_true(is.na(p$effect_reported[2]))
  # r row: N from n
  expect_equal(p$stat_value[3], 0.63); expect_equal(p$N[3], 173)
  # est-only row: estimate carried, group preserved
  expect_equal(p$effect_reported[4], -1.83)
  expect_equal(p$table_group[4], "ITT")
})

test_that("rows with empty / no-stat fields are skipped", {
  expect_null(effectcheck:::flattened_rows_to_parsed(list(
    tr("T", "label-only", list()),
    tr("T", "group-only", list(group = "PP"))
  )))
  expect_null(effectcheck:::flattened_rows_to_parsed(NULL))
  expect_null(effectcheck:::flattened_rows_to_parsed(list()))
})

test_that("est-only table row routes to extraction-only NOTE, result_context table", {
  r <- as.data.frame(check_text("", table_rows = list(
    tr("Table 2", "adjusted for stratification", list(
      est = -1.83, CI_lower = -11.2, CI_upper = 7.5), group = "ITT")
  )))
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type, "table_estimate")
  expect_equal(r$status, "NOTE")
  expect_equal(r$check_scope, "extraction_only")
  expect_equal(r$result_context, "table")
  expect_equal(r$effect_reported, -1.83)
  expect_equal(r$ciL_reported, -11.2)
  expect_equal(r$ciU_reported, 7.5)
})

test_that("verifiable t table row (t+df+d) is checked, not forced to NOTE", {
  r <- as.data.frame(check_text("", table_rows = list(
    tr("Table 5", "Joint", list(t = 1.26, df = 133, d = 0.11,
                                p = 0.21, p_op = "=",
                                CI_lower = -0.06, CI_upper = 0.28))
  )))
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type, "t")
  expect_equal(r$result_context, "table")
  # an independently-verifiable row reaches a real verdict (not extraction_only)
  expect_true(r$status %in% c("PASS", "WARN", "ERROR", "NOTE"))
  expect_false(identical(r$check_scope, "extraction_only"))
})

test_that("r table row CI is verified (PASS)", {
  r <- as.data.frame(check_text("", table_rows = list(
    tr("Table 10", "Statistical", list(r = 0.64, n = 159, p = 0.001, p_op = "<",
                                       CI_lower = 0.54, CI_upper = 0.73))
  )))
  expect_equal(r$test_type, "r")
  expect_equal(r$result_context, "table")
  expect_equal(r$status, "PASS")
})

test_that("table row duplicating a prose result is deduped (prose kept)", {
  prose <- "A correlation was significant, r(157) = 0.64, 95% CI [0.54, 0.73], p < .001."
  dup <- list(tr("Table 10", "Statistical",
                 list(r = 0.64, n = 159, CI_lower = 0.54, CI_upper = 0.73,
                      p = 0.001, p_op = "<")))
  base <- as.data.frame(check_text(prose))
  comb <- as.data.frame(check_text(prose, table_rows = dup))
  expect_equal(nrow(comb), nrow(base))           # table dup collapsed
  expect_false(any(comb$result_context == "table", na.rm = TRUE))  # prose kept
})

test_that("table_rows = NULL leaves text-only output unchanged", {
  txt <- "An independent t-test, t(48) = 2.10, p = .041, d = 0.59."
  a <- as.data.frame(check_text(txt))
  b <- as.data.frame(check_text(txt, table_rows = NULL))
  # ignore_attr: the effectcheck object carries a per-call `call` + `generated`
  # timestamp; only the result content must be identical.
  expect_equal(a, b, ignore_attr = TRUE)
})

test_that("fixture smoke: v2.4.95 flattened rows ingest without error", {
  fpath <- test_path("fixtures", "docpluck_flattened_rows_v2495.json")
  skip_if_not(file.exists(fpath), "fixture not present")
  fx <- jsonlite::fromJSON(fpath, simplifyVector = FALSE)
  # PROSECCO: 6 est-only rows -> all table_estimate NOTE
  pro <- as.data.frame(check_text("", table_rows = fx$prosecco))
  expect_equal(nrow(pro), 6L)
  expect_true(all(pro$test_type == "table_estimate"))
  expect_true(all(pro$status == "NOTE"))
  # 77859: Table 5 Separate/Joint carry t + d
  c8 <- as.data.frame(check_text("", table_rows = fx$c77859))
  expect_true(any(c8$test_type == "t" & !is.na(c8$df1)))
  # 90203: r rows verify (PASS), F rows present
  c2 <- as.data.frame(check_text("", table_rows = fx$c90203))
  expect_true(any(c2$test_type == "r" & c2$status == "PASS"))
  expect_true(any(c2$test_type == "F"))
  expect_true(all(c2$result_context == "table"))
})
