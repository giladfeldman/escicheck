# v0.5.8 -- chi-square-scoped bare-n N fallback (TRIAGE T3 residual)
#
# escicheck-iterate cycle 2 (run 2026-05-17b): a chi-square reporting its total
# sample size as a bare lowercase "n = 329" (Zhu & Feldman, R. Soc. Open Sci.
# 2025, 10.1098/rsos.250367 -- JASP goodness-of-fit lines) had N come back NA.
# pat_N deliberately matches only "N" / "nobs" because a bare "n =" is commonly
# a per-group size. Fix: a chi-square-scoped fallback -- when test_type is
# chisq, N is still NA, the chunk has no n1/n2, and EXACTLY ONE "n =" appears,
# that single bare n is the total N. Two or more "n =" (per-group counts) must
# not trigger it.

n_of <- function(res) {
  if (!"N" %in% names(res)) return(NA_real_)
  v <- res$N[!is.na(res$N)]
  if (length(v) == 0) NA_real_ else v[1]
}
chisq_row <- function(res) {
  if (!"test_type" %in% names(res)) return(res[0, , drop = FALSE])
  res[!is.na(res$test_type) & res$test_type == "chisq", , drop = FALSE]
}

test_that("v0.5.8: a bare 'n =' is read as total N for a chi-square (real rsos.250367 verbatims)", {
  # Real verbatim quotes from Zhu & Feldman (2025), JASP goodness-of-fit lines.
  cases <- list(
    list(vq = "χ²gof(1) = 31.01, p = 2.572e-08, n = 329", n = 329),
    list(vq = "χ²gof(1) = 14.85, p = 1.165e-04, n = 330", n = 330),
    list(vq = "χ²gof(1) = 49.95, p = 1.576e-12, n = 328", n = 328),
    list(vq = "χ²gof(1) = 12.76, p = 3.533e-04, n = 331", n = 331)
  )
  for (cs in cases) {
    row <- chisq_row(check_text(cs$vq))
    expect_equal(nrow(row), 1L, info = cs$vq)
    expect_equal(row$N[1], cs$n, info = cs$vq)
    expect_equal(row$N_source[1], "chi_bare_n", info = cs$vq)
  }
})

test_that("v0.5.8: the bare-n N enables Cohen's w verification on a chi-square", {
  # With N, a goodness-of-fit chi-square can compute and match Cohen's w.
  # w = sqrt(8 / 329) = 0.156 -- a reported w = 0.16 matches within tolerance.
  res <- check_text("chi2(1) = 8.0, p = .005, w = 0.16, n = 329")
  row <- chisq_row(res)
  expect_equal(nrow(row), 1L)
  expect_equal(row$N[1], 329)
  expect_equal(row$matched_variant[1], "cohens_w")
})

test_that("v0.5.8 guard: two per-group 'n =' do NOT trigger the fallback", {
  # "group A (n = 50) ... group B (n = 48)" -- two bare n = are per-group
  # counts, not a total. The fallback must leave N unresolved, not pick 50.
  res <- check_text("Chi-square test: group A (n = 50) and group B (n = 48), chi2(1) = 3.2, p = .07")
  row <- chisq_row(res)
  expect_equal(nrow(row), 1L)
  expect_true(is.na(row$N[1]) || row$N_source[1] != "chi_bare_n")
})

test_that("v0.5.8 guard: explicit n1/n2 tokens block the bare-n fallback", {
  res <- check_text("chi2(1) = 5.0, p = .03, n1 = 60, n2 = 64")
  row <- chisq_row(res)
  expect_equal(nrow(row), 1L)
  expect_true(is.na(row$N_source[1]) || row$N_source[1] != "chi_bare_n")
})

test_that("v0.5.8 guard: the fallback is chi-square-scoped -- a t-test bare n is untouched", {
  # n on a t-test is ambiguous (often per-group); the fallback must not fire.
  res <- check_text("t(40) = 2.1, p = .04, n = 21")
  trow <- res[!is.na(res$test_type) & res$test_type == "t", , drop = FALSE]
  expect_equal(nrow(trow), 1L)
  expect_true(is.na(trow$N_source[1]) || trow$N_source[1] != "chi_bare_n")
})

test_that("v0.5.8 guard: capital 'N =' is still the primary source, not the fallback", {
  res <- check_text("chi2(1) = 8.0, p = .005, N = 200")
  row <- chisq_row(res)
  expect_equal(row$N[1], 200)
  expect_true(row$N_source[1] != "chi_bare_n")
})
