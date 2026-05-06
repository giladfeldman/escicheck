## v0.3.5 — Decimal-place precision tracking (MetaESCI request 2A)
## ----------------------------------------------------------------
## count_decimal_places() must return the trailing-digit count from
## the raw matched string (preserving trailing zeros), and the four
## *_decimals output columns must populate from it.

test_that("count_decimal_places handles canonical inputs", {
  expect_equal(count_decimal_places("0.0400"), 4L)
  expect_equal(count_decimal_places("0.04"), 2L)
  expect_equal(count_decimal_places("2"), 0L)
  expect_equal(count_decimal_places("-1.500"), 3L)
  expect_equal(count_decimal_places(".05"), 2L)
  expect_equal(count_decimal_places("+12.3"), 1L)
})

test_that("count_decimal_places handles edge cases", {
  expect_true(is.na(count_decimal_places(NA)))
  expect_true(is.na(count_decimal_places(NA_character_)))
  expect_true(is.na(count_decimal_places("")))
  expect_true(is.na(count_decimal_places(NULL)))
  expect_equal(count_decimal_places(c("0.10", "ignored")), 2L)
})

test_that("check_text() populates *_decimals columns from raw matches", {
  res <- check_text("t(48) = 2.34, p = .023, d = 0.670, 95% CI [0.090, 1.250]")
  expect_equal(nrow(res), 1)
  expect_equal(res$stat_value_decimals[1], 2L)
  expect_equal(res$effect_reported_decimals[1], 3L)
  expect_equal(res$ciL_reported_decimals[1], 3L)
  expect_equal(res$ciU_reported_decimals[1], 3L)
})

test_that("trailing zeros are preserved (numify would lose them)", {
  res <- check_text("F(2, 87) = 5.12, p = .008, eta2 = 0.1100, 90% CI [0.020, 0.220]")
  expect_equal(res$effect_reported_decimals[1], 4L)
  expect_equal(res$stat_value_decimals[1], 2L)
  expect_equal(res$ciL_reported_decimals[1], 3L)
  expect_equal(res$ciU_reported_decimals[1], 3L)
})

test_that("missing CI leaves bound-decimal columns NA", {
  res <- check_text("F(2, 87) = 5.12, p = .008, eta2 = 0.11")
  expect_true(is.na(res$ciL_reported_decimals[1]))
  expect_true(is.na(res$ciU_reported_decimals[1]))
  expect_equal(res$effect_reported_decimals[1], 2L)
})
