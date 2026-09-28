# v0.7.8 -- the CI engine label must name the engine that ACTUALLY ran.
#
# WHY THIS EXISTS. `ci_d_ind_noncentral_t()` falls through to
# `ci_d_ind_approx()` -- a large-sample approximation, a genuinely different
# method -- on two paths: when |ncp| exceeds R's noncentral-t accuracy limit
# (~37.62), and when MBESS is not installed. Its caller `ci_d_ind()` labelled
# whatever came back as `method = "noncentral_t"` either way, so the package
# reported a method it had not run.
#
# Measured 2026-09-02 in the production image, two fresh R processes, same
# input d = 0.67, n1 = n2 = 25:
#   MBESS present -> [0.0964533619, 1.2369531589]  method "noncentral_t"
#   MBESS absent  -> [0.0996671786, 1.2403328214]  method "noncentral_t"
# The interval moved and the label did not. MBESS is a Suggests, installed
# through the Docker step that was measured dropping 18 packages silently, so
# this is reachable in a build that reports success.
#
# The |ncp| > 37.62 path reaches the same mislabel deterministically and
# without touching the installed library, which is what these tests use.

test_that("the ncp-overflow fallback is not labelled noncentral_t", {
  # ncp = d * sqrt(n1*n2/(n1+n2)) = 5 * sqrt(250000/1000) = 79.06 > 37.62,
  # so ci_d_ind_noncentral_t() returns the large-sample approximation.
  res <- ci_d_ind(5, 500, 500)
  expect_true(res$success)
  expect_false(
    identical(res$method, "noncentral_t"),
    info = "reported noncentral_t for bounds produced by the approximation"
  )
  expect_identical(res$method, "large_sample_approx")
})

test_that("the approximation and the noncentral-t engine really do differ", {
  # Two-sided control: if these agreed, the label would not matter.
  nct <- ci_d_ind_noncentral_t(0.67, 25, 25)
  apx <- ci_d_ind_approx(0.67, 25, 25)
  expect_false(isTRUE(all.equal(as.numeric(nct), as.numeric(apx))))
})

test_that("a genuine noncentral-t result keeps the noncentral_t label", {
  # The normal production path must be unchanged -- this is what makes the fix
  # invisible downstream except on rows that were mislabelled.
  skip_if_not_installed("MBESS")
  res <- ci_d_ind(0.67, 25, 25)
  expect_identical(res$method, "noncentral_t")
})

test_that("the engine tag is carried on the bounds themselves", {
  expect_identical(attr(ci_d_ind_approx(0.67, 25, 25), "ci_engine"),
                   "large_sample_approx")
  skip_if_not_installed("MBESS")
  expect_identical(attr(ci_d_ind_noncentral_t(0.67, 25, 25), "ci_engine"),
                   "mbess_ci_smd")
})
