# v0.6.15 (E-modeb-t-n): Mode B (flattened_rows) must bind a typed `n` field on
# a t-test table row. docpluck types a per-sample `n` column on table rows that
# print n but NOT df — collabra.23443 Table 5 delivers
# `{t: 16.6, d: 0.59, SD: 20.8, n: 799, CI_lower: 0.51, CI_upper: 0.66}` — but
# the t branch of flattened_rows_to_parsed only consumed t/df/d, so the row
# carried no N at all and fell to SKIP/insufficient_data even though the sample
# size was delivered typed (the r branch has always bound `fields.n`).
# Surfaced by the 2026-08-03 Sonnet canary audit of collabra.23443
# (S1-R15/S1-R16: rendered rows had df1/N both null, status SKIP).

test_that("a t table row with typed n but no df binds N", {
  p <- effectcheck:::flattened_rows_to_parsed(list(
    list(table_id = "camelot_t11", label = "Table 5", row_label = "MTurk",
         row_idx = 1, fields = list(t = 16.6, d = 0.59, SD = 20.8, n = 799,
                                    CI_lower = 0.51, CI_upper = 0.66))
  ))
  expect_equal(nrow(p), 1L)
  expect_equal(p$test_type[1], "t")
  expect_equal(as.numeric(p$N[1]), 799)
  expect_true(is.na(p$df1[1]))  # the table does not print df; N alone is bound
})

test_that("the full collabra.23443 Table-5 row is checkable, not SKIP", {
  r <- as.data.frame(effectcheck::check_text("", table_rows = list(
    list(table_id = "camelot_t11", label = "Table 5", row_label = "MTurk",
         row_idx = 1, fields = list(t = 16.6, d = 0.59, SD = 20.8, n = 799,
                                    CI_lower = 0.51, CI_upper = 0.66))
  )))
  tt <- r[!is.na(r$test_type) & r$test_type == "t", ]
  expect_equal(nrow(tt), 1L)
  expect_equal(as.numeric(tt$N[1]), 799)
  # With N bound, the reported d = 0.59 is verifiable (dz = 16.6/sqrt(799) =
  # 0.587): the row must no longer be SKIP / INSUFFICIENT_DATA.
  expect_false(tt$status[1] %in% c("SKIP", "INSUFFICIENT_DATA"))
})

test_that("a t row with BOTH df and n binds both (df untouched)", {
  p <- effectcheck:::flattened_rows_to_parsed(list(
    list(table_id = "t1", label = "Table 7", row_label = "Prolific",
         row_idx = 1, fields = list(t = 16.8, df = 398, d = 0.84, n = 400,
                                    CI_lower = 0.73, CI_upper = 0.96))
  ))
  expect_equal(as.numeric(p$df1[1]), 398)
  expect_equal(as.numeric(p$N[1]), 400)
})
