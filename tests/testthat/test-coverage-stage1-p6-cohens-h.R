# Stage 1 / P6 -- Cohen's h from two proportions.
#
# Cohen's h was parsed but never computable (variants = character(0), no
# computation path). Stage 1 adds h_from_proportions / ci_h and routes a
# two-proportion z-test to compute and verify h.

test_that("h_from_proportions matches the arcsine-difference definition", {
  expect_equal(h_from_proportions(0.5, 0.5), 0, tolerance = 1e-9)
  manual <- abs(2 * asin(sqrt(0.6)) - 2 * asin(sqrt(0.4)))
  expect_equal(h_from_proportions(0.6, 0.4), manual, tolerance = 1e-9)
})

test_that("h_from_proportions returns NA for out-of-range input", {
  expect_true(is.na(h_from_proportions(1.4, 0.4)))
  expect_true(is.na(h_from_proportions(0.6, NA)))
})

test_that("ci_h gives an ordered interval containing h", {
  ci <- ci_h(0.6, 0.4, n1 = 80, n2 = 80)
  h  <- h_from_proportions(0.6, 0.4)
  expect_true(ci$ci_low <= h && h <= ci$ci_high)
  expect_true(ci$ci_low >= 0)
})

# --- Task 8: routing ----------------------------------------------------------

test_that("a two-proportion z-test computes and verifies Cohen's h", {
  txt <- paste("A two-proportion z-test compared success rates (p1 = .60, n1 = 80;",
               "p2 = .40, n2 = 80), z = 2.53, p = .011, h = 0.40.")
  res <- check_text(txt)
  row <- res[res$test_type == "z", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$matched_variant[1], "h")
  expect_equal(row$matched_value[1], h_from_proportions(0.6, 0.4), tolerance = 0.02)
  expect_equal(row$status[1], "PASS")
})

test_that("a plain z-test without proportions does not compute Cohen's h", {
  res <- check_text("A z-test was significant, z = 2.10, p = .036.")
  row <- res[res$test_type == "z", ]
  expect_false(isTRUE(row$matched_variant[1] == "h"))
})
