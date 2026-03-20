# Tests for bugs reported by Innsbruck Credibility Hackathon (2026-03-19)

# ===========================================================================
# Bug 1: Section numbers parsed as p-values
# Text like "p = .548).\n\n3.3. Further analyses" should NOT extract p = 3.3
# ===========================================================================

test_that("Section numbers are not parsed as p-values (Bug 1)", {
  text <- "F(6, 1913) = 0.83, p = .548).\n\n3.3. Further analyses showed"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "F")
  # p_reported should be 0.548, NOT 3.3
  expect_true(!is.na(result$p_reported[1]),
    info = "p_reported should not be NA")
  expect_equal(result$p_reported[1], 0.548, tolerance = 0.001,
    info = paste("Expected p=0.548, got p=", result$p_reported[1]))
})

test_that("Section numbers don't corrupt p-values across line breaks (Bug 1 variant)", {
  text <- "t(45) = 2.10, p = .041.\n\n3.1. Discussion of results"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_equal(result$p_reported[1], 0.041, tolerance = 0.001)
})

test_that("Legitimate p-value line breaks still work after Bug 1 fix", {
  # This is the case the original regex was designed for
  text <- "t(28) = 2.21, p =\n0.035, d = 0.80"
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_equal(result$p_reported[1], 0.035, tolerance = 0.001,
    info = "p-value across line break should still be joined")
})

# ===========================================================================
# Bug 2: Wrong N used for phi in chi-square
# Chi-square with inline N should use that N, not a global/context N
# ===========================================================================

test_that("Chi-square uses inline N for phi computation (Bug 2)", {
  text <- "Study 1 involved N = 572 participants.\n\nStudy 2 Results: chi2(1, N = 204) = 4.36, p = .037, phi = .15."
  result <- check_text(text)

  # Should find at least the chi-square test

  chi_rows <- result[result$test_type == "chisq", ]
  expect_true(nrow(chi_rows) > 0, info = "Should detect chi-square test")

  # N should be 204 (from inline), NOT 572 (from Study 1)
  expect_equal(chi_rows$N[1], 204,
    info = paste("Expected N=204 (inline), got N=", chi_rows$N[1]))

  # phi = sqrt(chi2/N) = sqrt(4.36/204) = 0.1462
  # Reported phi = 0.15, so delta should be small
  expect_true(!is.na(chi_rows$phi[1]),
    info = "phi should be computed")
  expected_phi <- sqrt(4.36 / 204)
  expect_equal(chi_rows$phi[1], expected_phi, tolerance = 0.01,
    info = paste("phi should be computed with N=204, got:", chi_rows$phi[1]))
})

test_that("Chi-square without inline N still falls back to context (Bug 2 non-regression)", {
  text <- "With N = 100 participants, chi2(2) = 8.73, p = .013"
  result <- check_text(text)

  chi_rows <- result[result$test_type == "chisq", ]
  expect_true(nrow(chi_rows) > 0)
  expect_equal(chi_rows$N[1], 100,
    info = "Should fall back to context N when no inline N")
})

test_that("Chi-square inline N with thousands separator (Bug 2 edge case)", {
  text <- "chi2(1, N = 1,204) = 5.12, p = .024"
  result <- check_text(text)

  chi_rows <- result[result$test_type == "chisq", ]
  expect_true(nrow(chi_rows) > 0)
  expect_equal(chi_rows$N[1], 1204,
    info = paste("Expected N=1204, got N=", chi_rows$N[1]))
})

# ===========================================================================
# Bug 3: "ns" triggers WARN instead of OK
# "ns" / "n.s." should be recognized as "not significant"
# ===========================================================================

test_that("'ns' is recognized as not significant and gives OK (Bug 3)", {
  text <- "F(1, 99) = .86, ns."
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "F")

  # p_computed for F(1,99) = 0.86 should be ~0.356 (not significant)
  expect_true(!is.na(result$p_computed[1]))
  expect_true(result$p_computed[1] > 0.05,
    info = paste("p_computed should be > 0.05, got:", result$p_computed[1]))

  # Status should be OK (both agree: not significant)
  expect_equal(result$status[1], "OK",
    info = paste("Expected OK for ns with p > .05, got:", result$status[1]))
})

test_that("'n.s.' variant is also recognized (Bug 3)", {
  text <- "t(50) = 1.20, n.s."
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # p_computed for t(50) = 1.20 is ~0.236 (not significant)
  expect_equal(result$status[1], "OK",
    info = paste("Expected OK for n.s., got:", result$status[1]))
})

test_that("'ns' with significant computed p gives WARN (Bug 3 decision error)", {
  text <- "F(1, 99) = 5.50, ns."
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # p_computed for F(1,99) = 5.50 is ~0.021 (significant)
  expect_true(result$p_computed[1] < 0.05,
    info = paste("p_computed should be < 0.05, got:", result$p_computed[1]))

  # Status should be WARN (decision error: paper says ns but p < .05)
  expect_equal(result$status[1], "WARN",
    info = paste("Expected WARN for ns with significant p, got:", result$status[1]))
  expect_true(result$decision_error[1],
    info = "Should flag decision error")
})

test_that("Numeric p-value takes precedence over ns (Bug 3 non-regression)", {
  # When both numeric p and ns are present, numeric should win
  text <- "F(1, 99) = .86, p = .356, ns."
  result <- check_text(text)

  expect_true(nrow(result) > 0)
  # Should use the numeric p-value
  expect_equal(result$p_reported[1], 0.356, tolerance = 0.001,
    info = "Numeric p should take precedence over ns")
})
