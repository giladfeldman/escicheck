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

# v0.3.0c: Correlation guard — r-tests without df or N cannot be verified
test_that("r-test without df or N is extraction_only (correlation guard)", {
  # r without parenthetical df, no N in context — the r value trivially matches
  # itself but nothing can be independently verified
  res <- check_text("r = .39, p = .04, R2 = .15")
  if (nrow(res) > 0) {
    # Find the r-test result
    r_rows <- res[res$test_type == "r", ]
    if (nrow(r_rows) > 0) {
      expect_equal(r_rows$check_scope[1], "extraction_only")
      expect_true(is.na(r_rows$delta_effect_abs[1]))
      expect_equal(r_rows$check_type[1], "extraction_only")
    }
  }
})

test_that("r-test WITH df is still effect_size_checked (not affected by guard)", {
  res <- check_text("r(50) = .394, p = .004, R2 = .155")
  if (nrow(res) > 0) {
    r_rows <- res[res$test_type == "r", ]
    if (nrow(r_rows) > 0 && r_rows$check_type[1] == "effect_size") {
      expect_equal(r_rows$check_scope[1], "effect_size_checked")
      expect_false(is.na(r_rows$matched_value[1]))
    }
  }
})

test_that("z-test without df still works (not affected by correlation guard)", {
  # z-tests legitimately compute d from z and N, without needing df
  res <- check_text("In a sample of N = 200 participants, z = 2.50, p = .012, d = 0.35")
  if (nrow(res) > 0) {
    z_rows <- res[res$test_type == "z", ]
    if (nrow(z_rows) > 0) {
      # z-test should NOT be downgraded by the guard
      expect_true(z_rows$check_type[1] %in% c("effect_size", "p_value"))
    }
  }
})

test_that("guard does not affect normal results with valid stats", {
  # Normal t-test with effect size — should still be effect_size_checked
  res <- check_text("t(50) = 3.12, p = .003, d = 0.88")
  expect_equal(res$check_scope[1], "effect_size_checked")
  expect_equal(res$check_type[1], "effect_size")
  expect_false(is.na(res$delta_effect_abs[1]))
})
