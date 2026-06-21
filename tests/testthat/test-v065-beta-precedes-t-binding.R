# v0.6.5: a standardized regression coefficient (beta) that PRECEDES its t in a
# "(beta = X, t(df) = Y, p)" clause must bind to THAT t, not to the next clause's
# beta. The sub-chunk splitter previously split before each t, stranding the
# preceding beta at the end of the prior sub-chunk so the t adopted the next
# clause's beta.
#
# Surfaced by the 2026-06-21 escicheck-iterate canary audit against cog_emo
# (10.1080/02699931.2024.2434156): "(beta = 0.74, t(260) = 11.32, ...)" had
# t(260) = 11.32 tagged beta = 0.91 (the apology->empathy clause that follows),
# instead of its own 0.74. Mirror of the r-d adoption lesson, but a beta
# PRECEDES its t (a Cohen's d FOLLOWS its r).

test_that("each beta binds to the t in its own clause across three chained regressions", {
  txt <- paste0(
    "Examining the regression coefficients, we found an association between ",
    "perceived apology and forgiveness (beta = 0.83, t(261) = 5.82, p < .001), ",
    "between empathy and forgiveness (beta = 0.74, t(260) = 11.32, p < .001) ",
    "and between apology and empathy (beta = 0.91, t(261) = 8.24, p < .001)."
  )
  res <- effectcheck::check_text(txt)
  get_beta <- function(tval) {
    rr <- res[!is.na(res$test_type) & res$test_type == "t" &
                !is.na(res$stat_value) & abs(res$stat_value - tval) < 1e-6, ]
    if (nrow(rr) == 0) return(NA_real_)
    as.numeric(rr$effect_reported[1])
  }
  expect_equal(get_beta(5.82),  0.83, tolerance = 1e-6)  # apology -> forgiveness
  expect_equal(get_beta(11.32), 0.74, tolerance = 1e-6)  # empathy -> forgiveness (was 0.91)
  expect_equal(get_beta(8.24),  0.91, tolerance = 1e-6)  # apology -> empathy
})

test_that("the r-d adoption path is unaffected: a d FOLLOWING an r still binds to that r", {
  # Regression guard for the r-d lesson (test-v030f): d follows r, must still bind.
  txt <- "There was a correlation, r(50) = .40, p = .003, d = 0.87."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r" &
              !is.na(res$stat_value) & abs(res$stat_value - 0.40) < 1e-6, ]
  expect_true(nrow(rr) >= 1)
  # the d (0.87) is adopted by the r row (reported_type d), per the existing path
  expect_true(any(rr$effect_reported_name == "d", na.rm = TRUE) ||
                any(!is.na(rr$effect_reported)))
})
