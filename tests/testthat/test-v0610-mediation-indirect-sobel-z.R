# v0.6.10 (E-mediation): a bootstrapped MEDIATION indirect effect reported with a
# Sobel Z must be classified `mediation_indirect`, NOT a plain z-test. The reported
# effect is the indirect-effect coefficient (e.g. .05), NOT the sensitivity-analysis
# rho ("ACME robust until rho = 0.7") that the fallback-ES pattern previously grabbed.
# The Sobel Z is the test statistic; the bootstrapped CI is the indirect-effect CI;
# the row routes to an honest NOTE (the indirect effect is not recomputable from the
# reported numbers without the a/b path coefficients).
#
# Surfaced by the 2026-06-29 escicheck-iterate new-corpus pass against
# collabra.126266 (Outcome Bias replication+extension of Gino, Moore & Bazerman
# 2009): H2 + H5 mediation rows were all typed `z` with effect_reported_name="rho",
# effect_reported = the ACME sensitivity bound (0.7/0.5/0.8/0.7), and a spurious WARN.

test_that("a null Sobel-Z mediation indirect effect binds the indirect effect, not rho", {
  txt <- paste0(
    "The bootstrapped indirect effect of unethicality on punishment was .05, ",
    "95% CI [-.04, .12], Sobel Z = 0.84, p = .40, ACME found to be robust until ",
    "ρ = 0.7."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "mediation_indirect", ]
  expect_equal(nrow(rr), 1L)
  # The Sobel Z is the test statistic.
  expect_equal(rr$stat_value[1], 0.84)
  # The reported effect is the indirect effect (.05), NOT the sensitivity rho (0.7).
  expect_equal(rr$effect_reported_name[1], "indirect_effect")
  expect_equal(rr$effect_reported[1], 0.05)
  expect_false(any(res$effect_reported == 0.7, na.rm = TRUE))   # rho never adopted
  expect_false(any(res$effect_reported_name == "rho", na.rm = TRUE))
  # The bootstrapped CI is the indirect-effect CI.
  expect_equal(rr$ciL_reported[1], -0.04)
  expect_equal(rr$ciU_reported[1], 0.12)
  # Extraction-only NOTE -- never a plain-z WARN/SKIP/PASS.
  expect_equal(rr$status[1], "NOTE")
})

test_that("a significant Sobel-Z mediation indirect effect also binds the IE not rho", {
  txt <- paste0(
    "The bootstrapped indirect effect of unethicality on punishment was .27, ",
    "95% CI [.19, .35], Sobel Z = 4.87, p = <.001, ACME found to be robust until ",
    "ρ = 0.8."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "mediation_indirect", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$stat_value[1], 4.87)
  expect_equal(rr$effect_reported_name[1], "indirect_effect")
  expect_equal(rr$effect_reported[1], 0.27)
  expect_false(any(res$effect_reported == 0.8, na.rm = TRUE))   # rho=0.8 never adopted
  expect_equal(rr$ciL_reported[1], 0.19)
  expect_equal(rr$ciU_reported[1], 0.35)
  expect_equal(rr$status[1], "NOTE")
})

test_that("the malformed 'p = <.001' form (spurious = before the operator) parses", {
  # v0.6.10: docpluck delivers the H5 punishment mediation as "Sobel Z = 4.87,
  # p = <.001" (the PDF prints "p < .001"); the spurious "= " before "<" must not
  # block p-value extraction. The real operator is "<", the value .001.
  txt <- paste0(
    "The bootstrapped indirect effect of unethicality on punishment was .27, ",
    "95% CI [.19, .35], Sobel Z = 4.87, p = <.001, ACME found to be robust until ",
    "ρ = 0.8."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "mediation_indirect", ]
  expect_equal(nrow(rr), 1L)
  expect_true(isTRUE(rr$p_valid[1]))
  expect_equal(rr$p_reported[1], 0.001)
  expect_equal(rr$p_symbol[1], "<")
})

test_that("a normal 'p = .40' still captures = as the operator (no over-match)", {
  # Guard against the v0.6.10 'p = <.001' fix over-consuming the '=' in a plain
  # 'p = value' (the lookahead must require a real <,> operator to follow).
  res <- effectcheck::check_text("A simple effect, t(50) = 2.10, p = .40.")
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_true(nrow(rr) >= 1)
  expect_equal(rr$p_reported[1], 0.40)
  expect_equal(rr$p_symbol[1], "=")
})

test_that("the mediation pattern needs BOTH anchors (indirect effect AND Sobel Z)", {
  # A plain z-test with no 'indirect effect ... was' anchor stays a z-test, and a
  # bare 'indirect effect' mention with no Sobel Z does not become mediation_indirect.
  res_plain_z <- effectcheck::check_text("The effect was significant, z = 2.10, p = .036.")
  expect_false(any(res_plain_z$test_type == "mediation_indirect", na.rm = TRUE))

  res_no_sobel <- effectcheck::check_text(
    "The indirect effect of X on Y was small but we report no formal test here."
  )
  # This input may yield no extracted rows at all; guard the column access so a
  # 0-row tibble does not warn "Unknown or uninitialised column".
  ns_types <- if ("test_type" %in% names(res_no_sobel)) res_no_sobel$test_type else character(0)
  expect_false(any(ns_types == "mediation_indirect", na.rm = TRUE))
})
