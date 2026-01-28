# Test suite for report functions

test_that("render_report creates HTML file", {
  # Create sample data
  res <- tibble::tibble(
    location = 1,
    raw_text = "t(28) = 2.21",
    test_type = "t",
    stat_value = 2.21,
    df1 = 28,
    status = "PASS",
    uncertainty_level = "low"
  )
  
  out_file <- tempfile(fileext = ".html")
  result <- render_report(res, out_file)
  
  expect_true(file.exists(out_file))
  expect_true(file.size(out_file) > 0)
  
  # Check it's valid HTML
  content <- readLines(out_file, warn = FALSE)
  expect_true(any(grepl("<!doctype html>", content, ignore.case = TRUE)))
  
  unlink(out_file)
})

test_that("export_csv creates CSV file", {
  res <- tibble::tibble(
    location = 1,
    test_type = "t",
    status = "PASS"
  )
  
  out_file <- tempfile(fileext = ".csv")
  result <- export_csv(res, out_file)
  
  expect_true(file.exists(out_file))
  expect_true(file.size(out_file) > 0)
  
  # Check it can be read back
  read_back <- utils::read.csv(out_file)
  expect_equal(nrow(read_back), 1)
  
  unlink(out_file)
})

test_that("export_json creates JSON file", {
  # Skip if jsonlite not available
  skip_if_not_installed("jsonlite")
  
  res <- tibble::tibble(
    location = 1,
    test_type = "t",
    status = "PASS"
  )
  
  out_file <- tempfile(fileext = ".json")
  result <- export_json(res, out_file)
  
  expect_true(file.exists(out_file))
  expect_true(file.size(out_file) > 0)
  
  # Check it's valid JSON
  json_data <- jsonlite::fromJSON(out_file)
  expect_true("metadata" %in% names(json_data))
  expect_true("results" %in% names(json_data))
  
  unlink(out_file)
})

test_that("render_report handles empty results", {
  res <- tibble::tibble()
  
  out_file <- tempfile(fileext = ".html")
  result <- render_report(res, out_file)
  
  expect_true(file.exists(out_file))
  unlink(out_file)
})

test_that("export_csv handles NA values", {
  res <- tibble::tibble(
    location = 1,
    test_type = "t",
    d_ind = NA_real_,
    status = "PASS"
  )
  
  out_file <- tempfile(fileext = ".csv")
  result <- export_csv(res, out_file, na = "")
  
  expect_true(file.exists(out_file))
  unlink(out_file)
})
