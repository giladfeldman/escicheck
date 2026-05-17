# Stage 1 / P3 -- McNemar / Friedman / goodness-of-fit chi-square sub-types.
#
# All three are chi-square-shaped statistics that were silently routed to the
# contingency-table phi/V path, producing a confidently-computed but meaningless
# effect size. Stage 1 tags the sub-type from context and routes each correctly:
# Friedman -> Kendall's W, goodness-of-fit -> Cohen's w, McNemar -> honest NOTE.

test_that("chi-square sub-types are tagged from context", {
  mcnemar  <- check_text("A McNemar test was significant, chi2(1) = 6.40, p = .011.")
  friedman <- check_text("A Friedman test, chi2(3) = 9.20, p = .027, N = 30.")
  gof      <- check_text("A goodness-of-fit test, chi2(2) = 5.10, p = .078, N = 90.")
  expect_equal(mcnemar$chisq_subtype[mcnemar$test_type == "chisq"][1],  "mcnemar")
  expect_equal(friedman$chisq_subtype[friedman$test_type == "chisq"][1], "friedman")
  expect_equal(gof$chisq_subtype[gof$test_type == "chisq"][1],          "gof")
})

test_that("a plain contingency chi-square is tagged contingency", {
  res <- check_text("A chi-square test of association, chi2(1) = 8.20, p = .004, N = 120.")
  expect_equal(res$chisq_subtype[res$test_type == "chisq"][1], "contingency")
})

# --- Task 6: routing ----------------------------------------------------------

test_that("Friedman chi-square routes to Kendall's W, not phi/V", {
  res <- check_text("A Friedman test on the rankings, chi2(3) = 9.20, p = .027, N = 30.")
  row <- res[res$test_type == "chisq", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$kendalls_W[1], 9.20 / (30 * 3), tolerance = 0.001)
  expect_true(is.na(row$phi[1]))
  expect_true(is.na(row$V[1]))
})

test_that("goodness-of-fit chi-square is not given a phi/V association effect", {
  res <- check_text("A goodness-of-fit test, chi2(2) = 5.10, p = .078, N = 90.")
  row <- res[res$test_type == "chisq", ]
  expect_equal(nrow(row), 1)
  expect_true(is.na(row$phi[1]))
  expect_true(is.na(row$V[1]))
  expect_false(row$status[1] == "ERROR")
})

test_that("McNemar chi-square is NOT given a phi/V effect size", {
  res <- check_text("A McNemar test was significant, chi2(1) = 6.40, p = .011.")
  row <- res[res$test_type == "chisq", ]
  expect_equal(nrow(row), 1)
  expect_true(is.na(row$phi[1]))
  expect_true(is.na(row$V[1]))
  expect_false(row$status[1] == "ERROR")
})

test_that("a plain contingency chi-square still computes phi/V (no regression)", {
  res <- check_text("A chi-square test of association, chi2(1) = 8.20, p = .004, N = 120.")
  row <- res[res$test_type == "chisq", ]
  expect_false(is.na(row$phi[1]))
})
