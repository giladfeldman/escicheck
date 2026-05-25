# v0.6.1: bare "t = X, p [op] Y" (no df) extraction
#
# Lee Newman 2014 RR replication (Lee-Feldman 2025 RSOS, gold v0.6.0 cycle 11)
# reports dozens of one-sample t-tests in tables 10-15 in the compact form
# `<label> M = m (sd), t = X, p < .001`. The df lives only in the table header,
# not in the immediate sentence. Before v0.6.1 the parser required either
# "t(df) = X" or "t = X, df = Y" — bare "t = X, p < Y" returned 0 rows.
#
# v0.6.1 adds pat_t_p_nodf with an 80-char lookahead anchor on a p-clause to
# distinguish a genuine t-test report from an unrelated "t = value" elsewhere
# in prose. df1 stays NA; check.R routes to status NOTE (extracted, p-check
# needs df). Word-boundary lookbehind keeps `dt = ...` etc. from matching.

test_that("v0.6.1: bare t with p but no df extracts as t with status NOTE", {
  r <- check_text("t = 37.7, p < 0.001")
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type[1], "t")
  expect_equal(r$stat_value[1], 37.7)
  expect_true(is.na(r$df1[1]))
  expect_equal(r$status[1], "NOTE")
})

test_that("v0.6.1: real Lee-Newman compact one-sample t-test extracts", {
  v <- "alcoholism good change M = 67.1 (35.9), t = 37.7, p < 0.001"
  r <- check_text(v)
  expect_equal(nrow(r), 1L)
  expect_equal(r$test_type[1], "t")
  expect_equal(r$stat_value[1], 37.7)
})

test_that("v0.6.1: negative t with p extracts", {
  r <- check_text("t = -25.7, p = .034")
  expect_equal(nrow(r), 1L)
  expect_equal(r$stat_value[1], -25.7)
})

test_that("v0.6.1: decimal-mantissa t with p extracts", {
  r <- check_text("t = .35, p = .728")
  expect_equal(nrow(r), 1L)
  expect_equal(r$stat_value[1], 0.35)
})

test_that("v0.6.1: word-boundary lookbehind prevents `dt = ...` false positive", {
  r <- check_text("the dt = 0.5, p = .03")
  expect_equal(nrow(r), 0L)
})

test_that("v0.6.1: p-clause too far away does NOT match (>80 chars between t= and p)", {
  v <- paste0(
    "this t = 37.7 result was reported elsewhere and the manuscript ",
    "also reports p = .001 on a different topic various other items ",
    "here that span over eighty characters before any p clause"
  )
  r <- check_text(v)
  expect_equal(nrow(r), 0L)
})

test_that("v0.6.1: with-df form (pat_t_nodf) still wins over bare-with-p form", {
  # When both df and p are present, pat_t_nodf should match first and produce
  # OK status (full verification possible).
  r <- check_text("t = 37.7, df = 100, p < 0.001")
  expect_equal(nrow(r), 1L)
  expect_equal(r$df1[1], 100)
  expect_equal(r$status[1], "OK")
})
