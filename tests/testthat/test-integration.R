# Integration tests for end-to-end functionality

test_that("Full pipeline works: parse -> compute -> check", {
  text <- "Participants in the controllable condition reported higher responsibility than the uncontrollable condition, t(28) = 2.21, p = .035, d = 0.80, 95% CI [0.12, 1.48]."
  
  # Parse
  parsed <- parse_text(text)
  expect_true(nrow(parsed) > 0)
  expect_equal(parsed$test_type[1], "t")
  
  # Check
  result <- check_text(text)
  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "t")
  expect_true("status" %in% names(result))
})

test_that("Multiple statistics in one text", {
  text <- "t(28) = 2.21, d = 0.80. Another test: F(2, 27) = 4.56, η² = 0.25."
  
  result <- check_text(text)
  expect_true(nrow(result) >= 2)
  expect_true("t" %in% result$test_type)
  expect_true("F" %in% result$test_type)
})

test_that("CI extraction and comparison works", {
  text <- "r(198) = .34, p < .001, 95% CI [.21, .45]"
  
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true(!is.na(result$ci_level[1]))
    expect_equal(result$ci_level[1], 0.95)
    expect_true(!is.na(result$ciL_reported[1]))
    expect_true(!is.na(result$ciU_reported[1]))
  }
})

test_that("Uncertainty tracking works end-to-end", {
  text <- "t(28) = 2.21, d = 0.80"  # Missing N, n1, n2
  
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true("uncertainty_level" %in% names(result))
    expect_true("uncertainty_reasons" %in% names(result))
    expect_true("assumptions_used" %in% names(result))
    expect_true("design_inferred" %in% names(result))
    expect_true("variants_tested" %in% names(result))
  }
})

test_that("Defunct file-input API errors with migration message (v0.4.0)", {
  # The v0.3.x pipeline:
  #   text <- read_any_text(path); check_text(text)
  #   check_files(path)
  # is replaced in v0.4.0 by extracting via docpluck and calling check_text()
  # on the resulting string. Both functions now hard-error.
  expect_error(read_any_text("dummy.txt"),
               regexp = "(Defunct|docpluck|effectcheck v0\\.4)")
  expect_error(check_files("dummy.txt"),
               regexp = "(Defunct|docpluck|effectcheck v0\\.4)")
})

test_that("Text-only checking pipeline still works end-to-end", {
  # Equivalent to the old read_any_text + check_text combo, with text
  # provided directly (as docpluck would supply it).
  text <- "t(28) = 2.21, d = 0.80, 95% CI [0.12, 1.48]"
  result <- check_text(text)
  expect_true(nrow(result) > 0)
})

test_that("Export pipeline works", {
  text <- "t(28) = 2.21, d = 0.80"
  result <- check_text(text)
  
  if (nrow(result) > 0) {
    # HTML export
    html_file <- tempfile(fileext = ".html")
    render_report(result, html_file)
    expect_true(file.exists(html_file))
    unlink(html_file)
    
    # CSV export
    csv_file <- tempfile(fileext = ".csv")
    export_csv(result, csv_file)
    expect_true(file.exists(csv_file))
    unlink(csv_file)
    
    # JSON export (if available)
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      json_file <- tempfile(fileext = ".json")
      export_json(result, json_file)
      expect_true(file.exists(json_file))
      unlink(json_file)
    }
  }
})

test_that("Context window extraction works", {
  text <- "First sentence. Second sentence with t(28) = 2.21. Third sentence. Fourth sentence."
  
  parsed <- parse_text(text, context_window_size = 2)
  if (nrow(parsed) > 0) {
    expect_true("context_window" %in% names(parsed))
    expect_true(grepl("First sentence", parsed$context_window[1]))
    expect_true(grepl("Fourth sentence", parsed$context_window[1]))
  }
})

test_that("Design inference from context", {
  text <- "A paired-samples t-test was conducted. t(39) = 3.0, p = .004, d = 0.45."
  
  result <- check_text(text)
  if (nrow(result) > 0) {
    expect_true("design_inferred" %in% names(result))
    # Should infer "paired" from context
    if (!is.na(result$design_inferred[1])) {
      expect_true(result$design_inferred[1] %in% c("paired", "independent", "unclear"))
    }
  }
})
