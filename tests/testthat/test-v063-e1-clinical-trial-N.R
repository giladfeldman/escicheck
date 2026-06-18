# v0.6.3 (E1): clinical-trial combined N = sum of per-arm totals.
#
# Surfaced by the 2026-06-16 escicheck-iterate canary audit of the PROSECCO
# trial (10.1371/journal.pmed.1004323). Two coupled defects:
#   (1) RR / rdpct rows inherited a SINGLE arm total as N (from
#       global_text / extended_context) instead of summing the two arms the
#       parser already captured -- e.g. an RR with arms 106 + 101 reported
#       N = 106 rather than 207.
#   (2) the primary-outcome risk-difference sentence
#       "86/98 women (87.8%) ... and 79/89 women (88.8%)" bound NO per-arm
#       cells, because pat_two_props_slash required the percent to follow the
#       slash-count immediately and the word "women" sat between them.
# Fix: parse.R relaxes pat_two_props_slash to allow a short alphabetic
# descriptor between the slash-count and the percent; check.R sets
# N <- arm1_total + arm2_total (N_source = "arm_totals_sum") whenever both
# arm totals are present and positive, for test_type %in% {RR, rdpct}.
# Each asserted N below equals the AI-gold n_total for that result.

test_that("v0.6.3: RR combined N is the sum of per-arm totals (RR 0.90, gold N=205)", {
  txt <- "There was no significant difference in surgeon's judgment on completeness of resection in the PSA group compared to the GA group (79/106 (74.5%) versus 82/99 (82.8%), RR 0.90; 95% CI 0.78 to 1.04; p = 0.16)."
  res <- check_text(txt)
  rr <- res[res$test_type == "RR", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$arm1_total, 106)
  expect_equal(rr$arm2_total, 99)
  expect_equal(rr$N, rr$arm1_total + rr$arm2_total)
  expect_equal(rr$N, 205)
  expect_equal(rr$N_source, "arm_totals_sum")
})

test_that("v0.6.3: RR combined N (RR 1.53, gold N=207)", {
  txt <- "Surgical perioperative complications occurred in 8/106 (7.5%) women under PSA and 5/101 (5.0%) women under GA (RR 1.53; 95% CI 0.52 to 4.51; p-value 0.44)."
  res <- check_text(txt)
  rr <- res[res$test_type == "RR", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$N, 207)
  expect_equal(rr$N_source, "arm_totals_sum")
})

test_that("v0.6.3: RR combined N (RR 0.79, gold N=145)", {
  txt <- "There was no significant difference in surgical reinterventions in the PSA group compared to the GA group at 12 months follow-up (8/77 (10.4%) versus 9/68 (13.2%); RR 0.79; 95% CI 0.32 to 1.92; p-value 0.62)."
  res <- check_text(txt)
  rr <- res[res$test_type == "RR", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$N, 145)
  expect_equal(rr$N_source, "arm_totals_sum")
})

test_that("v0.6.3: a descriptor between slash-count and percent binds cells (rdpct, gold N=187)", {
  # The PROSECCO primary-outcome sentence. "women" sits between the
  # slash-count and the percent in BOTH arms; before the v0.6.3 relaxation
  # this bound no cells and N fell back to a single-arm global_text value.
  txt <- "Hysteroscopic resection was complete in 86/98 women (87.8%) in the PSA group and 79/89 women (88.8%) in the GA group (risk difference -1.01%; 95% confidence interval (CI) -10.36 to 8.34; noninferiority, P = 0.09)."
  res <- check_text(txt)
  rd <- res[res$test_type == "rdpct", ]
  expect_equal(nrow(rd), 1L)
  expect_equal(rd$arm1_total, 98)
  expect_equal(rd$arm2_total, 89)
  expect_equal(rd$N, 187)
  expect_equal(rd$N_source, "arm_totals_sum")
})

test_that("v0.6.3: relaxed descriptor is letters-only and cannot bridge a digit into a fraction", {
  # Safety guard: the descriptor class is [A-Za-z ] (no digits), so a numeric
  # token between the slash-count and the parenthesis breaks the cell match
  # rather than being silently swallowed. This sentence must NOT yield a
  # two-proportion RR/rdpct arm capture.
  txt <- "Group sizes were 10/20 cohort 3 (15%) versus 5/25 (20%) across waves."
  res <- check_text(txt)
  arm_tot <- if ("arm1_total" %in% names(res)) res$arm1_total else numeric(0)
  expect_true(all(is.na(arm_tot)))
})

test_that("v0.6.3: a lone slash-count in prose yields no arm-total N override", {
  # Single fraction with a percent but no second comparison cell: the
  # two-proportion pattern needs both arms, so nothing should be captured as
  # RR/rdpct cells and no arm_totals_sum N should appear.
  txt <- "Overall, 7/10 (70%) of participants completed the follow-up survey."
  res <- check_text(txt)
  nsrc <- if ("N_source" %in% names(res)) res$N_source else character(0)
  expect_false(any(!is.na(nsrc) & nsrc == "arm_totals_sum"))
})
