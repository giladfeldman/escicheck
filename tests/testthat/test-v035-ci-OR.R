## v0.3.5 — Odds-ratio CI (MetaESCI request 1A)
## ---------------------------------------------
## ci_OR_all() supplies CIs from log-Wald (with SE), Fisher exact (from
## 2x2 cells), or Wald-on-log back-derived from a reported p-value.

test_that("ci_OR_all returns empty list when no information available", {
  expect_length(ci_OR_all(OR = 2.5), 0L)
  expect_length(ci_OR_all(OR = NA_real_), 0L)
  expect_length(ci_OR_all(OR = -1), 0L)
})

test_that("ci_OR_all wald_log uses supplied SE_logOR", {
  # OR = 2, SE_logOR = log(2) / qnorm(0.975) gives bounds [1, 4].
  se <- log(2) / qnorm(0.975)
  res <- ci_OR_all(OR = 2, SE_logOR = se, level = 0.95)
  expect_true("wald_log" %in% names(res))
  expect_equal(res$wald_log$bounds[1], 1, tolerance = 1e-6)
  expect_equal(res$wald_log$bounds[2], 4, tolerance = 1e-6)
})

test_that("ci_OR_all back-derives SE from p-value via Wald inversion", {
  # OR = 2.5 with p = .015 -> z = qnorm(1 - .015/2)
  res <- ci_OR_all(OR = 2.5, level = 0.95, p_value = 0.015)
  expect_true("wald_log_from_p" %in% names(res))
  expect_true(res$wald_log_from_p$bounds[1] > 0)
  expect_true(res$wald_log_from_p$bounds[2] > res$wald_log_from_p$bounds[1])
  # The reported point estimate should sit within the back-derived CI.
  expect_true(res$wald_log_from_p$bounds[1] <= 2.5)
  expect_true(res$wald_log_from_p$bounds[2] >= 2.5)
})

test_that("ci_OR_all Fisher exact branch fires when 2x2 cells supplied", {
  # 2x2 table: rows = group, cols = outcome. Random concrete table.
  cells <- c(20, 10, 5, 25)  # OR ~ (20*25)/(10*5) = 10
  res <- ci_OR_all(OR = 10, level = 0.95, cells = cells)
  expect_true("fisher_exact" %in% names(res))
  expect_true(all(is.finite(res$fisher_exact$bounds)))
})

test_that("check_text dispatches OR CI through Phase 6 multi-method matcher", {
  res <- check_text("chi-square(1) = 5.99, p = .015, OR = 2.50, 95% CI [1.20, 5.10]")
  expect_equal(nrow(res), 1)
  expect_equal(res$effect_reported_name[1], "OR")
  expect_true(!is.na(res$ci_check_status[1]))
  expect_true(grepl("^OR:", res$ci_method_match[1]))
  expect_true(!is.na(res$ciL_computed[1]))
  expect_true(!is.na(res$ciU_computed[1]))
})

test_that("OR variant does not trigger tautological self-match in Phase 5", {
  # Regression: previous version put effect_reported into computed_variants$OR$value,
  # which made cross_type_action ineffective.
  res <- check_text(
    "chi2(1, N = 200) = 5.50, p = .019, OR = 2.30",
    cross_type_action = "WARN"
  )
  expect_equal(res$status[1], "WARN")
})
