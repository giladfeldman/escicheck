# test-v030n-fixes.R
# Regression tests for v0.3.0n fixes (MetaESCI batch deep-dive 2026-04-07)
#   Bug 1: F ~= 0 crash via list-to-double coercion in CI fallback path
#   Bug 2: Beta masquerade — multi-predictor regression (b != beta, both populated)

# ---------------------------------------------------------------------------
# Bug 1: F ~= 0 should not crash the pipeline
# ---------------------------------------------------------------------------

test_that("F near zero does not crash check_text (v0.3.0n)", {
  # F = 0.08 with df1 = 1, df2 = 100 — exercises the noncentral-F CI path
  # that previously stored a list and crashed downstream as.numeric().
  res <- tryCatch(
    check_text("F(1, 100) = 0.08, p = .78, partial eta squared = .001"),
    error = function(e) e
  )
  expect_false(inherits(res, "error"),
               info = sprintf("check_text crashed on F ~= 0: %s",
                              if (inherits(res, "error")) conditionMessage(res) else ""))
  expect_true(nrow(res) >= 1)
  expect_false(is.na(res$status[1]))
})

test_that("Very small F (0.01) does not crash check_text (v0.3.0n)", {
  res <- tryCatch(
    check_text("F(2, 50) = 0.01, p = .99"),
    error = function(e) e
  )
  expect_false(inherits(res, "error"))
  expect_true(nrow(res) >= 1)
})

# ---------------------------------------------------------------------------
# Bug 2a: b == beta masquerade (v0.3.0m case, still guarded in v0.3.0n)
# ---------------------------------------------------------------------------

test_that("b == beta masquerade is detected and does not ERROR (v0.3.0m)", {
  # Parser extracts "b = 0.45" and labels it "beta = 0.45" — these are the
  # same number, meaning the reported "beta" is actually unstandardized b.
  res <- check_text("b = 0.45, SE = 0.12, t(198) = 3.75, p < .001, beta = 0.45")
  expect_equal(nrow(res), 1)
  expect_true(res$status[1] != "ERROR",
              info = sprintf("Expected non-ERROR, got: %s", res$status[1]))
  expect_true(grepl("unstandardized b coefficient",
                    res$uncertainty_reasons[1], fixed = TRUE))
})

# ---------------------------------------------------------------------------
# Bug 2b: Multi-predictor regression (b != beta, both populated) — new v0.3.0n
# ---------------------------------------------------------------------------

test_that("multi-predictor regression ERROR is downgraded to WARN (v0.3.0n)", {
  # Real-world pattern: multi-predictor model reports both unstandardized b
  # and standardized beta, with values that single-predictor
  # standardized_beta_from_t cannot reproduce. With b = 4.12, SE = 1.50,
  # t(50) = 2.747 -> computed beta ~= 0.362 vs reported 0.20 (delta = 0.16,
  # well over 5x tolerance -> ERROR under v0.3.0m). v0.3.0n downgrades to
  # WARN because b_coeff is populated and differs from effect_reported.
  res <- check_text("b = 4.12, SE = 1.50, t(50) = 2.75, p = .008, beta = 0.20")
  expect_equal(nrow(res), 1)
  expect_equal(res$test_type[1], "regression")
  expect_true(res$status[1] != "ERROR",
              info = sprintf("Expected non-ERROR, got: %s", res$status[1]))
  expect_true(grepl("multi-predictor",
                    res$uncertainty_reasons[1], fixed = TRUE))
})
