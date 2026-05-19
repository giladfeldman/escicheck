# v0.5.10 -- a bare "r = value" is extracted when a CI corroborates it (no p needed)
#
# escicheck-iterate: effectcheck deliberately did not extract a bare "r = X"
# (no df) unless a p-value was nearby -- a guard against casual "r = .3"
# mentions. But a correlation reported with a confidence interval is a genuine
# result even without a p. The m_r_nodf guard now accepts "p OR CI", mirroring
# the chi_nodf ("p OR df") and U ("p OR z") guards. A bare bracketed pair only
# counts as a CI when its bounds bracket the r value (the straddle check), so
# an unrelated bracketed pair is not mistaken for a CI.
# Real verbatim: SPPS affect-heuristic replication (10.1177/19485506211056761).

r_rows <- function(res) {
  if (!"test_type" %in% names(res)) return(res[0, , drop = FALSE])
  res[!is.na(res$test_type) & res$test_type == "r", , drop = FALSE]
}

test_that("v0.5.10: a bare r with a bracketed CI but no p is extracted (real SPPS verbatim)", {
  res <- check_text(paste0("Vice versa, increasing benefits leads people to judge ",
                           "its risks as lower (original: r = -.74 [-0.92, -0.30])."))
  row <- r_rows(res)
  expect_equal(nrow(row), 1L)
  expect_equal(row$stat_value[1], -0.74)
})

test_that("v0.5.10: a bare r with an explicitly-labelled CI is extracted", {
  row <- r_rows(check_text("The association was moderate, r = .42, 95% CI [.21, .58]."))
  expect_equal(nrow(row), 1L)
  expect_equal(row$stat_value[1], 0.42)
})

test_that("v0.5.10: a bare r with a nearby p is still extracted (behaviour unchanged)", {
  row <- r_rows(check_text("The correlation was significant, r = .195, p = .032."))
  expect_equal(nrow(row), 1L)
  expect_equal(row$stat_value[1], 0.195)
})

test_that("v0.5.10 guard: a bare r with neither a p nor a CI is NOT extracted", {
  expect_equal(nrow(r_rows(check_text("one study with 12802 participants (r = 0.004)."))), 0L)
  expect_equal(nrow(r_rows(check_text("The correlation was small (r = .21)."))), 0L)
})

test_that("v0.5.10 guard: an unrelated bracketed pair is not mistaken for a CI", {
  # [12, 15] does not bracket r = .30 -- the straddle check rejects it.
  expect_equal(nrow(r_rows(check_text("items r = .30 across the range [12, 15] of the scale."))), 0L)
})
