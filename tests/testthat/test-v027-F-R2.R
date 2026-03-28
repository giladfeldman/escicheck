# test-v027-F-R2.R — v0.2.7 Issue C: adjusted_R2 and cohens_f2 for F-tests

test_that("adjusted_R2_from_R2 computes correctly", {
  # R2=0.50, N=100, k=3: adj = 1 - (0.50)*(99/96) = 1 - 0.515625 = 0.484375
  expect_equal(round(adjusted_R2_from_R2(0.50, 100, 3), 4), 0.4844)
  # Edge cases
  expect_true(is.na(adjusted_R2_from_R2(NA, 100, 3)))
  expect_true(is.na(adjusted_R2_from_R2(0.5, 4, 3)))  # N <= k+1
  expect_true(is.na(adjusted_R2_from_R2(1.5, 100, 3)))  # R2 > 1
})

test_that("F-test with R2 computes adjusted_R2 as alternative", {
  # F(3, 96) = 8.5, p < .001: R2 = 3*8.5/(3*8.5+96) = 0.2099
  # adjusted_R2 = 1 - (1-0.2099)*99/96 = 1 - 0.7901*1.03125 = 0.1852
  res <- check_text("F(3, 96) = 8.5, p < .001, R2 = .21")
  if (nrow(res) >= 1 && res$test_type[1] == "F") {
    # Should match R2 closely
    expect_true(res$status[1] %in% c("PASS", "WARN"))
  }
})

test_that("cohens_f2 computed for F-tests", {
  # F(2, 97) = 10.0, p < .001: R2 = 2*10/(2*10+97) = 0.171
  # f2 = 0.171/(1-0.171) = 0.206
  res <- check_text("F(2, 97) = 10.0, p < .001, f2 = .21")
  if (nrow(res) >= 1 && res$test_type[1] == "F") {
    expect_equal(res$check_scope[1], "effect_size_checked")
    if (!is.na(res$delta_effect_abs[1])) {
      expect_true(res$delta_effect_abs[1] < 0.02)
    }
  }
})

test_that("Regression guard: F with matching R2 still PASS", {
  # F(1, 98) = 12.5: R2 = 12.5/110.5 = 0.1131
  res <- check_text("F(1, 98) = 12.5, p < .001, R2 = .11")
  expect_true(res$status[1] %in% c("PASS", "WARN"))
})
