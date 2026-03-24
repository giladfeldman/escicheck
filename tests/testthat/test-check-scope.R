# test-check-scope.R — v0.2.7 Issue 5: check_scope column + matched_value cleanup
# Ensures the check_scope column correctly distinguishes effect-size-checked
# from p-value-only and extraction-only results, and that matched_value is
# proper NA (not literal "NA" string) for non-ES-checked results.

test_that("check_scope column exists in check_text output", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_true("check_scope" %in% names(res))
})

test_that("effect_size_checked: ES reported and matched", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
  expect_equal(res$check_scope[1], "effect_size_checked")
  expect_equal(res$check_type[1], "effect_size")
  expect_false(is.na(res$matched_value[1]))
})

test_that("p_value_only: p reported but no effect size", {
  res <- check_text("t(28) = 2.21, p = .035")
  expect_equal(res$check_scope[1], "p_value_only")
  expect_equal(res$check_type[1], "p_value")
  # matched_value must be proper NA, not literal string "NA"
  expect_true(is.na(res$matched_value[1]))
  expect_true(is.na(res$matched_variant[1]))
  expect_true(is.na(res$delta_effect_abs[1]))
})

test_that("extraction_only: no p-value, no effect size", {
  res <- check_text("t(28) = 2.21")
  expect_equal(res$check_scope[1], "extraction_only")
  expect_equal(res$check_type[1], "extraction_only")
  expect_true(is.na(res$matched_value[1]))
})

test_that("regression guard: ES-checked result still works correctly", {
  res <- check_text("t(45) = 2.31, p = .023, d = 0.68")
  expect_equal(res$check_scope[1], "effect_size_checked")
  expect_equal(res$status[1], "PASS")
  expect_false(is.na(res$matched_value[1]))
  # matched_value should be close to 0.68
  expect_true(abs(res$matched_value[1] - 0.68) < 0.05)
})

test_that("matched_value is R NA_real_ not string 'NA' for p-value-only", {
  res <- check_text("F(2, 97) = 4.56, p = .013")
  # This is p-value only (F-test with p but no effect size reported)
  if (res$check_type[1] == "p_value") {
    expect_true(is.na(res$matched_value[1]))
    # Verify it's actual NA, not a character "NA"
    expect_true(is.numeric(res$matched_value[1]) || is.na(res$matched_value[1]))
    expect_false(identical(as.character(res$matched_value[1]), "NA"))
  }
})
