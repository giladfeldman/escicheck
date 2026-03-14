# test-compare.R
# Tests for statcheck comparison mode

test_that("compare_with_statcheck returns effectcheck_comparison class", {
  res <- compare_with_statcheck("t(28) = 2.21, p = .035, d = 0.80")
  expect_true("effectcheck_comparison" %in% class(res))
})

test_that("compare_with_statcheck has source column", {
  res <- compare_with_statcheck("t(28) = 2.21, p = .035, d = 0.80")
  expect_true("source" %in% names(res))
  expect_true(all(res$source %in% c("both", "effectcheck_only", "statcheck_only")))
})

test_that("compare_with_statcheck has statcheck_error column", {
  res <- compare_with_statcheck("t(28) = 2.21, p = .035, d = 0.80")
  expect_true("statcheck_error" %in% names(res))
})

test_that("compare_with_statcheck works without statcheck installed", {
  # This test verifies graceful fallback — statcheck may or may not be installed
  res <- compare_with_statcheck("t(28) = 2.21, p = .035, d = 0.80")
  expect_true(nrow(res) >= 1)
  expect_true("source" %in% names(res))
})

test_that("compare_with_statcheck finds at least one result", {
  res <- compare_with_statcheck("t(28) = 2.21, p = .035, d = 0.80")
  expect_true(nrow(res) >= 1)
  expect_equal(res$test_type[1], "t")
})

test_that("compare_file_with_statcheck works on text file", {
  tmp <- tempfile(fileext = ".txt")
  writeLines("t(28) = 2.21, p = .035, d = 0.80", tmp)
  res <- compare_file_with_statcheck(tmp)
  expect_true("effectcheck_comparison" %in% class(res))
  expect_true("source" %in% names(res))
  unlink(tmp)
})

test_that("print method works for comparison", {
  res <- compare_with_statcheck("t(28) = 2.21, p = .035, d = 0.80")
  output <- capture.output(print(res))
  expect_true(any(grepl("EffectCheck vs statcheck", output)))
  expect_true(any(grepl("Total rows", output)))
})

test_that("compare_with_statcheck handles multiple stats", {
  text <- "t(28) = 2.21, p = .035; F(1, 58) = 4.12, p = .047"
  res <- compare_with_statcheck(text)
  expect_true(nrow(res) >= 2)
})
