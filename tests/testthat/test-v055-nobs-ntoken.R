# v0.5.5 -- "nobs" (the JASP number-of-observations token) recognised as total N
#
# escicheck-iterate cycle 4 (2026-05-17): a chi-square reporting its sample size
# as "nobs = 659" (the JASP token) -- "χ²Pearson(1) = 1.590, ..., nobs = 659" --
# had N come back NA, so the reported effect size (Cohen's w / Cramér's V) could
# not be verified (status NOTE). pat_N matched only capital "N =". Fix: pat_N
# now also accepts "nobs". Bare lowercase "n =" is intentionally NOT matched --
# it is commonly a per-group size and would be mis-read as the total N.

n_of <- function(res) {
  if (!"N" %in% names(res)) return(NA_real_)
  v <- res$N[!is.na(res$N)]
  if (length(v) == 0) NA_real_ else v[1]
}
chisq_row <- function(res) {
  if (!"test_type" %in% names(res)) return(res[0, , drop = FALSE])
  res[!is.na(res$test_type) & res$test_type == "chisq", , drop = FALSE]
}

test_that("v0.5.5: nobs is recognised as total N (real rsos.250367 verbatims)", {
  cases <- c(
    "χ²gof(2) = 225.954, p = 8.602e-50, ĈPearson = 0.505, CI95% [0.453, 0.551], nobs = 659",
    "χ²Pearson(1) = 1.590, p = 0.207, V̂Cramer = 0.030, CI95% [0.000, 0.119], nobs = 659",
    "χ²Pearson(1) = 6.773, p = 0.009, V̂Cramer = 0.094, CI95% [0.000, 0.174], nobs = 659"
  )
  for (vq in cases) {
    expect_equal(n_of(check_text(vq)), 659, info = vq)
  }
})

test_that("v0.5.5: nobs N enables effect-size verification on a chi-square", {
  res <- check_text("chi2(1) = 8.0, p = .005, w = 0.11, nobs = 659")
  row <- chisq_row(res)
  expect_equal(nrow(row), 1)
  expect_equal(row$N[1], 659)
  expect_equal(row$matched_variant[1], "cohens_w")
  expect_equal(row$status[1], "PASS")
})

test_that("v0.5.5 guard: bare lowercase 'n =' is NOT matched as total N", {
  # "n =" is commonly a per-group size; matching it as total N would mis-infer.
  res <- check_text("chi2(1) = 8.0, p = .005, w = 0.11, n = 659")
  row <- chisq_row(res)
  expect_equal(nrow(row), 1)
  expect_true(is.na(row$N[1]))
})

test_that("v0.5.5 guard: capital 'N =' still works", {
  res <- check_text("chi2(1, N = 200) = 8.0, p = .005, w = 0.20")
  expect_equal(chisq_row(res)$N[1], 200)
})
