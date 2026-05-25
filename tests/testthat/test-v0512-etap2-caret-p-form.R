# Regression test for v0.5.12 — pat_etap2 extended to recognize the
# `eta^2p` / `eta^2_p` form (subscript-p AFTER the squared symbol), in
# addition to the previously-supported `etap^2` form (subscript-p BEFORE).
#
# Caught by the 2026-05-23 escicheck-iterate validation cycle against the
# Collabra Identifiable-Victim, Experiential-vs-Material, and Less-Is-Better
# stats gold: 13+ F-rows across two papers dropped their reported
# partial-eta-squared point estimate (CI was captured, name + value null)
# because every Collabra paper writes `η^2p = .008` with the p trailing
# the caret-2.

test_that("etap2 binder catches η^2p form (p after squared)", {
  txt <- "We found some support for main effect of Identifiability (H1b), F(2, 998) = 3.91, p = .02, η^2p = .008. 95% CI [.000, .021]."
  res <- effectcheck::check_text(txt)
  expect_gt(nrow(res), 0L)
  expect_equal(res$effect_reported_name[1], "etap2")
  expect_equal(round(res$effect_reported[1], 3), 0.008)
})

test_that("etap2 binder catches η^2p form even with literal 0.00 value", {
  txt <- "F(1, 666) = 0.09, p = .764, η^2p = 0.00, BF_01 = 11.08, 95% CI [.00, .01]."
  res <- effectcheck::check_text(txt)
  expect_gt(nrow(res), 0L)
  expect_equal(res$effect_reported_name[1], "etap2")
  expect_equal(res$effect_reported[1], 0.00)
})

test_that("etap2 binder catches η^2p form with bracket-CI (no '95% CI' label)", {
  txt <- "F(2, 998) = 3.82, p = .022, η^2p = .008 [.000, .021]."
  res <- effectcheck::check_text(txt)
  expect_gt(nrow(res), 0L)
  expect_equal(res$effect_reported_name[1], "etap2")
  expect_equal(round(res$effect_reported[1], 3), 0.008)
})

test_that("etap2 binder still catches the legacy ηp^2 form (p before squared)", {
  txt <- "F(2, 100) = 5.5, p = .005, ηp^2 = .10, 95% CI [.02, .19]."
  res <- effectcheck::check_text(txt)
  expect_gt(nrow(res), 0L)
  expect_equal(res$effect_reported_name[1], "etap2")
  expect_equal(round(res$effect_reported[1], 2), 0.10)
})

test_that("eta2 (non-partial) is NOT mis-bound by the new etap2 forms", {
  txt <- "F(2, 100) = 5.5, p = .005, η^2 = .15, 95% CI [.05, .25]."
  res <- effectcheck::check_text(txt)
  expect_gt(nrow(res), 0L)
  expect_equal(res$effect_reported_name[1], "eta2")
  expect_equal(round(res$effect_reported[1], 2), 0.15)
})
