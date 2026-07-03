# v0.6.13: hazard ratio (Cox proportional-hazards / survival analysis) reported in
# a clean prose sentence is extracted as an extraction-only NOTE
# (`test_type = "hazard_ratio"`), mirroring the OR / RR / mcnemar_or ratios. A Cox
# HR is not independently recomputable from the reported numbers (it needs the full
# time-to-event data), so the HR + its CI + p are surfaced without a verdict.
#
# Scope note (from the 2026-07-02 corpus audit): s41598-023-50401-z's 58 hazard
# ratios are ALL in a docpluck-column-shredded survival table (univariate +
# multivariate HR/CI/p interleaved and misaligned) with NO clean prose form, so
# they are a docpluck extraction defect (DP-5), not extractable here. This feature
# targets the standard prose sentence form that clinical / epi papers use.
#
# The extraction requires a CO-LOCATED CI (a "95% CI" phrase or a bracketed range),
# so a bare "HR" mention -- or the "HR" abbreviation used for heart rate -- never
# fires.

test_that("a clean prose HR with a bracketed CI is an extraction-only NOTE", {
  txt <- "Older age predicted worse survival, HR = 1.87, 95% CI [1.54, 2.28], p < .01."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "hazard_ratio", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$effect_reported_name[1], "HR")
  expect_equal(rr$effect_reported[1], 1.87)
  expect_equal(rr$ciL_reported[1], 1.54)
  expect_equal(rr$ciU_reported[1], 2.28)
  expect_equal(rr$status[1], "NOTE")
  # The reported CI is surfaced but a Cox HR is not recomputable, so its CI verdict
  # must NOT be a false INCONSISTENT (the reported HR 1.87 lies inside [1.54,2.28]).
  if ("ci_check_status" %in% names(rr)) {
    expect_false(identical(rr$ci_check_status[1], "INCONSISTENT"))
  }
})

test_that("an 'adjusted HR' with a bracketless dash-range CI binds the CI", {
  txt <- "Distant stage predicted mortality (adjusted HR = 2.05, 95% CI 1.40-3.00, p = .002)."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "hazard_ratio", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$effect_reported[1], 2.05)
  expect_equal(rr$ciL_reported[1], 1.40)
  expect_equal(rr$ciU_reported[1], 3.00)
  expect_equal(rr$status[1], "NOTE")
})

test_that("a spelled-out 'hazard ratio' with a colon-comma CI binds the CI", {
  txt <- "Treatment reduced risk, hazard ratio = 0.62, 95% CI: 0.45, 0.85, p = .003."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "hazard_ratio", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$effect_reported[1], 0.62)
  expect_equal(rr$ciL_reported[1], 0.45)
  expect_equal(rr$ciU_reported[1], 0.85)
})

test_that("a 'HR ... to ...' range CI form binds the CI", {
  txt <- "Reginal stage increased mortality, HR = 1.96, 95% CI 1.53 to 2.51, p < .01."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "hazard_ratio", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$ciL_reported[1], 1.53)
  expect_equal(rr$ciU_reported[1], 2.51)
})

# --- guards: no false positives -------------------------------------------

test_that("a bare 'HR' mention with no CI does NOT create a hazard_ratio row", {
  txt <- "We calculated the HR for each subgroup and reported them in Table 3."
  res <- effectcheck::check_text(txt)
  expect_false("test_type" %in% names(res) &&
               any(res$test_type == "hazard_ratio", na.rm = TRUE))
})

test_that("'HR = 1.5' with NO co-located CI does NOT fire", {
  txt <- "The HR = 1.5 was reported without a confidence interval in the abstract."
  res <- effectcheck::check_text(txt)
  expect_false("test_type" %in% names(res) &&
               any(res$test_type == "hazard_ratio", na.rm = TRUE))
})

test_that("'HR' as the heart-rate abbreviation in a t-test sentence does NOT fire", {
  txt <- "Heart rate (HR) differed between groups, t(40) = 2.3, p = .02, d = 0.5."
  res <- effectcheck::check_text(txt)
  expect_false(any(res$test_type == "hazard_ratio", na.rm = TRUE))
  # The t-test is still extracted.
  expect_true(any(res$test_type == "t", na.rm = TRUE))
})

test_that("hazard_ratio is off unless requested in stats", {
  txt <- "Older age predicted worse survival, HR = 1.87, 95% CI [1.54, 2.28], p < .01."
  res <- effectcheck::check_text(txt, stats = c("t", "F", "r"))
  expect_false("test_type" %in% names(res) &&
               any(res$test_type == "hazard_ratio", na.rm = TRUE))
})

test_that("the medical bracketless CI does not disturb a bracketed odds-ratio row", {
  # An OR reported with a normal bracketed CI must still bind that CI (the medical
  # fallback only fires when no bracketed CI was found).
  txt <- "The association held, chi2 (1, N = 200) = 5.4, p = .02, OR = 1.8, 95% CI [1.1, 2.9]."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$effect_reported_name) & res$effect_reported_name == "OR", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$ciL_reported[1], 1.1)
  expect_equal(rr$ciU_reported[1], 2.9)
})
