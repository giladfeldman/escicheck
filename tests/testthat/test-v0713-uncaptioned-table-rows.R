# v0.7.13: rows from a table no caption claimed are not checked.
#
# docpluck 2.4.145 stamps every flattened row with its table's `caption_status`
# ("matched" | "none_found" | "uncaptioned_candidate") and, unlike 2.4.144, now
# INCLUDES rows from grids no caption claimed. docpluck measured roughly half of
# those grids as page furniture (flow-diagram boxes, author blocks).
#
# Measured 2026-09-27, effectcheck 0.7.12, all 27 seam papers, local docpluck
# 2.4.144 vs the 2.4.145 candidate (deb69ae): 85 rows gained, 0 lost, PASS
# 377 -> 377 and OK 268 -> 268 -- the new rows verified nothing -- and two new
# WARNs on 10.1016/j.jesp.2022.104372, both from a questionnaire-item grid read
# as t-tests (`Item2`: t = 1.83 on df = 2, d = -0.10 [-0.67, 0.48]), each with an
# INCONSISTENT CI. On that paper 343 of 393 table rows were uncaptioned
# candidates; dropping them restores the 2.4.144 output exactly.
#
# docpluck's contract says to filter on `caption_status`, never on the
# `table_id` prefix (an implementation detail), so this test uses a `table_id`
# that does NOT start with "u".

fr <- function(table_id, label, row_label, fields, caption_status = NULL) {
  r <- list(table_id = table_id, page = 1L, label = label, row_idx = 0L,
            row_label = row_label, fields = fields)
  if (!is.null(caption_status)) r$caption_status <- caption_status
  r
}
item_row <- list(t = 1.83, df = 2, d = -0.10, CI_lower = -0.67, CI_upper = 0.48)
real_row <- list(t = 2.41, df = 98, d = 0.48, p = 0.018, p_op = "=")

text_only <- "Participants completed the task."

test_that("an uncaptioned-candidate row publishes nothing", {
  rows <- list(
    fr("t1", "Table 1", "Sleep vs Control", real_row, "matched"),
    fr("camelot_t3", "", "Item2", item_row, "uncaptioned_candidate")
  )
  res <- as.data.frame(check_text(text_only, table_rows = rows))
  expect_false(any(grepl("Item2", res$raw_text)))
  expect_false(any(res$stat_value %in% 1.83))
  # The captioned row is untouched.
  expect_equal(sum(res$stat_value %in% 2.41), 1L)
})

test_that("matched, none_found and pre-2.4.145 rows (no field) are all kept", {
  rows <- list(
    fr("t1", "Table 1", "A", real_row, "matched"),
    fr("t2", "Table 2", "B", modifyList(real_row, list(t = 3.10)), "none_found"),
    fr("t3", "Table 3", "C", modifyList(real_row, list(t = 2.05)))
  )
  res <- as.data.frame(check_text(text_only, table_rows = rows))
  expect_setequal(res$stat_value[!is.na(res$stat_value)], c(2.41, 3.10, 2.05))
})

test_that("an uncaptioned duplicate cannot suppress the captioned table", {
  # The cross-table dedup drops a LATER table repeating an EARLIER one. If an
  # uncaptioned grid came first and was filtered only afterwards, the captioned
  # table's row would be lost with it.
  rows <- list(
    fr("camelot_t0", "", "Sleep vs Control", real_row, "uncaptioned_candidate"),
    fr("t1", "Table 1", "Sleep vs Control", real_row, "matched")
  )
  res <- as.data.frame(check_text(text_only, table_rows = rows))
  expect_equal(sum(res$stat_value %in% 2.41), 1L)
})

test_that("only uncaptioned rows is the same as no table rows", {
  rows <- list(fr("camelot_t3", "", "Item2", item_row, "uncaptioned_candidate"))
  a <- as.data.frame(check_text(text_only, table_rows = rows))
  b <- as.data.frame(check_text(text_only))
  expect_equal(nrow(a), nrow(b))
})

test_that("the settings record says how many table rows were set aside", {
  # Sonnet consult 2026-09-27: `n_table_rows` counted every row received, so a
  # paper with 343 of 393 rows dropped recorded "393" with no trace of the drop.
  rows <- list(
    fr("t1", "Table 1", "Sleep vs Control", real_row, "matched"),
    fr("camelot_t3", "", "Item2", item_row, "uncaptioned_candidate"),
    fr("camelot_t4", "", "Item3", item_row, "uncaptioned_candidate")
  )
  s <- attr(check_text(text_only, table_rows = rows), "settings")
  expect_equal(s$n_table_rows, 3L)
  expect_equal(s$n_table_rows_uncaptioned_dropped, 2L)
  s0 <- attr(check_text(text_only), "settings")
  expect_equal(s0$n_table_rows_uncaptioned_dropped, 0L)
})
