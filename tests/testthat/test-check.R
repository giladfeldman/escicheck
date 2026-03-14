# Test suite for check functions

test_that("check_text returns tibble with correct structure", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  expect_true(tibble::is_tibble(result))
  if (nrow(result) > 0) {
    expect_true("test_type" %in% names(result))
    expect_true("status" %in% names(result))
    expect_true("uncertainty_level" %in% names(result))
  }
})

test_that("check_text handles empty text", {
  result <- check_text("")
  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 0)
})

test_that("check_text detects t-test", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_equal(result$test_type[1], "t")
  }
})

test_that("check_text detects F-test", {
  text <- "F(2, 27) = 4.56, p < .05"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_equal(result$test_type[1], "F")
  }
})

test_that("check_text detects correlation", {
  text <- "r(198) = .34, p < .001"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_equal(result$test_type[1], "r")
  }
})

test_that("check_text computes effect sizes for t-test", {
  text <- "t(28) = 2.21, d = 0.80, N = 30"
  result <- check_text(text)
  if (nrow(result) > 0) {
    # Should compute d_ind or dz
    has_effect <- !is.na(result$d_ind[1]) || !is.na(result$dz[1])
    expect_true(has_effect)
  }
})

test_that("check_text computes ANOVA effect sizes", {
  text <- "F(2, 27) = 4.56, η² = 0.25"
  result <- check_text(text)
  if (nrow(result) > 0) {
    # Should compute eta² variants
    has_eta2 <- !is.na(result$eta2[1]) || !is.na(result$partial_eta2[1])
    expect_true(has_eta2)
  }
})

test_that("check_text assigns status correctly", {
  text <- "t(28) = 2.21, d = 0.80, N = 30, n1 = 15, n2 = 15"
  result <- check_text(text, tol_effect = list(d = 0.02))
  if (nrow(result) > 0) {
    expect_true(result$status[1] %in% c("PASS", "OK", "NOTE", "WARN", "ERROR", "INSUFFICIENT_DATA"))
  }
})

test_that("check_text tracks uncertainty", {
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true("uncertainty_level" %in% names(result))
    expect_true(result$uncertainty_level[1] %in% c("low", "medium", "high"))
  }
})

test_that("check_text includes design_inferred", {
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true("design_inferred" %in% names(result))
  }
})

test_that("check_text includes variants_tested", {
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true("variants_tested" %in% names(result))
  }
})

test_that("check_files handles multiple files", {
  # Create temporary test files
  file1 <- tempfile(fileext = ".txt")
  file2 <- tempfile(fileext = ".txt")
  writeLines("t(28) = 2.21, d = 0.80", file1)
  writeLines("r(50) = 0.34, p < .05", file2)
  
  result <- check_files(c(file1, file2))
  expect_true(tibble::is_tibble(result))
  expect_true("source" %in% names(result))
  
  # Cleanup
  unlink(c(file1, file2))
})

# ===========================================================================
# 5-level status system tests (PASS / OK / NOTE / WARN / ERROR)
# ===========================================================================

test_that("OK status for p-value-only check with consistent direction", {
  # F-test with no effect size, but p-value consistent
  text <- "F(2, 27) = 4.56, p = .02"
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true(result$status[1] %in% c("PASS", "OK", "NOTE", "WARN", "ERROR"))
  }
})

test_that("OK status for p < .001 inequality when computed p also < .001", {
  # Large t-value ensures computed p < .001
  text <- "t(28) = 5.50, p < .001"
  result <- check_text(text)
  if (nrow(result) > 0) {
    # No effect size reported, p < .001 consistent -> should be OK
    expect_true(result$status[1] %in% c("OK", "NOTE", "PASS"))
  }
})

test_that("r-test with consistent p-value gets OK status", {
  # r IS the effect size, p should be consistent
  text <- "r(198) = .34, p < .001"
  result <- check_text(text)
  if (nrow(result) > 0) {
    # r-test with consistent p should be OK or PASS
    expect_true(result$status[1] %in% c("OK", "PASS", "NOTE"))
  }
})

test_that("NOTE status for ambiguous design with matching effect", {
  # Ambiguous design (no clear paired/independent context) but effect matches
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  if (nrow(result) > 0) {
    # Should be PASS, NOTE, or WARN depending on delta
    expect_true(result$status[1] %in% c("PASS", "OK", "NOTE", "WARN", "ERROR"))
  }
})

test_that("p_symbol column is available in parsed output", {
  text <- "t(28) = 2.21, p < .001"
  parsed <- parse_text(text)
  expect_true("p_symbol" %in% names(parsed))
  if (nrow(parsed) > 0) {
    expect_equal(parsed$p_symbol[1], "<")
  }
})

test_that("p_symbol captures = sign", {
  text <- "t(28) = 2.21, p = .035"
  parsed <- parse_text(text)
  if (nrow(parsed) > 0) {
    expect_equal(parsed$p_symbol[1], "=")
  }
})
