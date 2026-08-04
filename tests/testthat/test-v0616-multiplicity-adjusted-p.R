# v0.6.16 (E6 / E-multiplicity-adjusted-p) -- when the paper states a
# multiple-comparison correction, a "reported ns / computed sig" decision-error
# flag is a false positive: the reported p is ADJUSTED, the computed p is the
# raw per-test p, and the two are different quantities.
#
# Found by the 2026-08-04 Sonnet canary audit of collabra.90203 and verified
# arithmetically before fixing: the context states "post-hoc comparisons ...
# with Bonferroni correction"; t(998) = 2.37 reports p = .053 while the raw
# two-sided p is .017977 -- and .017977 x 3 comparisons = .0539, the reported
# value. The sibling rows corroborate the x3: t = 2.46 -> .0422 (reported .041)
# and t = 0.097 -> 2.77 clamped to the reported p = 1.00, a value only reachable
# under an adjustment.
#
# Authored against the UNFIXED code and watched fail (decision_error TRUE,
# status WARN) before the guard landed.

test_that("v0.6.16 E6: Bonferroni-corrected post-hoc p is not a decision error", {
  txt <- paste0(
    "To better understand the Identifiability main effect, we also examined the ",
    "post-hoc comparisons comparing the different Identifiability conditions with ",
    "Bonferroni correction. We found near threshold support for the comparison ",
    "between identifiable and joint, t(998) = 2.37, p = .053, d = 0.18 [0.03, 0.34] ",
    "with donations slightly lower in the joint condition."
  )
  res <- check_text(txt)
  row <- res[!is.na(res$stat_value) & res$stat_value == 2.37, ]
  expect_equal(nrow(row), 1L)
  expect_false(isTRUE(row$decision_error[1]),
               info = paste("decision_error was", row$decision_error[1],
                            "reason", row$decision_error_reason[1]))
  expect_true(is.na(row$decision_error_reason[1]))
  # Suppressed, never silent: the reason must be stated.
  expect_true(grepl("multiple-comparison correction", row$uncertainty_reasons[1], fixed = TRUE))
  # And the row must not still be a WARN on the strength of the cleared flag.
  expect_false(row$status[1] == "WARN")
})

test_that("v0.6.16 E6: the OPPOSITE direction is still flagged under a stated correction", {
  # A correction only ever makes p LARGER. So "reported significant, computed
  # not significant" cannot be explained by multiplicity adjustment -- that
  # direction must remain a decision error even when a correction is stated,
  # or the guard would launder a real error class.
  txt <- paste0(
    "Post-hoc comparisons used Bonferroni correction. The difference was ",
    "significant, t(20) = 0.42, p = .03, d = 0.09."
  )
  res <- check_text(txt)
  row <- res[!is.na(res$stat_value) & res$stat_value == 0.42, ]
  expect_equal(nrow(row), 1L)
  # raw two-sided p for t(20) = 0.42 is ~0.68 -- reported .03 is NOT explicable
  # by any multiplicity adjustment.
  expect_true(isTRUE(row$decision_error[1]),
              info = paste("decision_error was", row$decision_error[1]))
  expect_equal(row$decision_error_reason[1], "reported_sig_computed_ns")
})

test_that("v0.6.16 E6: without a stated correction the flag still fires", {
  # Same numbers as the first test but no correction mentioned anywhere: the
  # decision error is a genuine finding and must survive.
  txt <- "The comparison was near threshold, t(998) = 2.37, p = .053, d = 0.18 [0.03, 0.34]."
  res <- check_text(txt)
  row <- res[!is.na(res$stat_value) & res$stat_value == 2.37, ]
  expect_equal(nrow(row), 1L)
  expect_true(isTRUE(row$decision_error[1]),
              info = paste("decision_error was", row$decision_error[1]))
  expect_equal(row$decision_error_reason[1], "reported_ns_computed_sig")
})
