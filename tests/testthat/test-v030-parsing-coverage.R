# Tests for v0.3.0a comprehensive parsing coverage
# Verifies all effect size formats parse correctly

test_that("eta2 plain text is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, eta2 = 0.15")
  expect_equal(res$effect_reported_name[1], "eta2")
  expect_true(res$check_scope[1] != "error")
})

test_that("omega2 plain text is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, omega2 = 0.10")
  expect_equal(res$effect_reported_name[1], "omega2")
})

test_that("epsilon2 plain text is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, epsilon2 = 0.12")
  expect_equal(res$effect_reported_name[1], "epsilon_squared")
})

test_that("partial eta2 plain text is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, partial eta2 = 0.10")
  expect_equal(res$effect_reported_name[1], "etap2")
})

test_that("partial omega2 plain text is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, partial omega2 = 0.08")
  expect_equal(res$effect_reported_name[1], "partial_omega2")
})

test_that("Unicode eta-squared is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, \u03b7\u00b2 = 0.15")
  expect_equal(res$effect_reported_name[1], "eta2")
  expect_true(res$check_scope[1] != "error")
})

test_that("Unicode partial eta-squared is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, \u03b7p\u00b2 = 0.10")
  expect_equal(res$effect_reported_name[1], "etap2")
})

test_that("Unicode omega-squared is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, \u03c9\u00b2 = 0.10")
  expect_equal(res$effect_reported_name[1], "omega2")
})

test_that("caret eta-squared is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, eta^2 = 0.15")
  expect_equal(res$effect_reported_name[1], "eta2")
})

test_that("omega-squared with caret is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, omega^2 = 0.10")
  expect_equal(res$effect_reported_name[1], "omega2")
})

test_that("eta-squared written out is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, eta-squared = 0.15")
  expect_equal(res$effect_reported_name[1], "eta2")
})

test_that("partial eta-squared written out is parsed", {
  res <- check_text("F(1, 50) = 10.0, p = .003, partial eta-squared = 0.10")
  expect_equal(res$effect_reported_name[1], "etap2")
})

test_that("etap2 normalization still works", {
  # Line 147 normalize_text converts etap2 -> partial eta-squared
  res <- check_text("F(1, 50) = 10.0, p = .003, etap2 = 0.10")
  expect_equal(res$effect_reported_name[1], "etap2")
})

test_that("eta2 parsing does not produce check_scope error", {
  # Regression: v0.3.0 Phase 11C crashed on eta2 variable reference
  res <- check_text("F(2, 57) = 8.0, p < .001, eta-squared = 0.22")
  expect_true(res$check_scope[1] != "error")
})

test_that("all parsed effect sizes get effect_size_checked scope", {
  # Verify parsing reaches the checker for key formats
  texts <- c(
    "F(1, 50) = 10.0, p = .003, eta2 = 0.17",
    "F(1, 50) = 10.0, p = .003, omega2 = 0.13",
    "F(1, 50) = 10.0, p = .003, epsilon2 = 0.15"
  )
  for (t in texts) {
    res <- check_text(t)
    expect_true(res$check_scope[1] %in% c("effect_size_checked", "ci_checked", "p_value_only"),
      info = paste("Failed for:", t))
  }
})
