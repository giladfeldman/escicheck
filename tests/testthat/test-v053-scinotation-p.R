# v0.5.3 -- scientific E-notation reported p-values
#
# escicheck-iterate cycle 2 (2026-05-17): a reported p written in E-notation --
# the form R / JASP / Python emit, e.g. "p = 2.572e-08" -- was not captured.
# pat_p requires a [01].x mantissa (so it rejects "2.572") and pat_p_sci only
# handles the "p < 10^-N" form. 5 of the 7 chi-square results in the gold for
# 10.1098/rsos.250367 carry an E-notation p, so effectcheck silently SKIPped a
# checkable p-value (status SKIP, p_reported NA). Fix: pat_p_enote captures the
# mantissa+exponent whole and converts it to a plain-decimal string.

p_of <- function(res) {
  if (!"p_reported" %in% names(res)) return(NA_real_)
  v <- res$p_reported[!is.na(res$p_reported)]
  if (length(v) == 0) NA_real_ else v[1]
}

test_that("v0.5.3: E-notation reported p is captured (real rsos.250367 verbatims)", {
  cases <- list(
    list(vq = "χ²gof(2) = 225.954, p = 8.602e-50, ĈPearson = 0.505, CI95% [0.453, 0.551], nobs = 659", p = 8.602e-50),
    list(vq = "χ²gof(1) = 31.01, p = 2.572e-08, n = 329", p = 2.572e-08),
    list(vq = "χ²gof(1) = 14.85, p = 1.165e-04, n = 330", p = 1.165e-04),
    list(vq = "χ²gof(1) = 49.95, p = 1.576e-12, n = 328", p = 1.576e-12),
    list(vq = "χ²gof(1) = 12.76, p = 3.533e-04, n = 331", p = 3.533e-04)
  )
  for (cs in cases) {
    pr <- p_of(check_text(cs$vq))
    expect_false(is.na(pr), info = paste("p not captured for:", cs$vq))
    expect_equal(pr, cs$p, tolerance = 1e-6, info = cs$vq)
  }
})

test_that("v0.5.3: E-notation p on a non-chi-square test is captured", {
  expect_equal(p_of(check_text("t(120) = 6.5, p = 3.0e-9")), 3.0e-9, tolerance = 1e-12)
  expect_equal(p_of(check_text("F(2, 88) = 9.1, p = 4.4E-5")),  4.4e-5, tolerance = 1e-10)
})

test_that("v0.5.3 guard: plain-decimal p still parses unchanged", {
  expect_equal(p_of(check_text("t(45) = 2.31, p = .023")), 0.023, tolerance = 1e-9)
  expect_equal(p_of(check_text("chi2(1, N = 100) = 8.0, p = 0.005")), 0.005, tolerance = 1e-9)
})

test_that("v0.5.3 guard: a positive-exponent E-number is not captured as a p", {
  # pat_p_enote requires a negative exponent -- "p = 2.5e3" (= 2500) is not a
  # valid p-value form and must not be captured.
  expect_true(is.na(p_of(check_text("chi2(1, N = 100) = 5.0, p = 2.5e3"))))
})
