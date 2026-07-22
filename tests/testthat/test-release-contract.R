test_that("the versioned result contract matches check_text output", {
  fixture_path <- system.file("contracts", "analysis-result.fixture.json", package = "effectcheck")
  expect_true(nzchar(fixture_path))
  fixture <- jsonlite::fromJSON(fixture_path)
  result <- check_text("The effect was significant, t(48) = 2.31, p = .025, d = 0.66.")

  expect_true(all(fixture$requiredFields %in% names(result)),
              info = "check_text() dropped a required worker/frontend contract field")
  expect_true(all(unique(result$status) %in% fixture$statuses))
  expect_true(all(!is.na(result$test_type) & result$test_type %in% fixture$testTypes))
})
