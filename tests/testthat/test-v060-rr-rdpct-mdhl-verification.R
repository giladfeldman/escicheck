# v0.6.0: clinical-trial RR / rdpct / md_hl independent verification.
# v0.5.16/17/18 captured these test types for surface transparency but emitted
# status=NOTE without computing anything. v0.6.0 wires:
#   - RR  : computes (events1/total1)/(events2/total2) when per-arm cells are
#           in the same sentence; surfaces reported-vs-computed delta + a
#           Wald-on-log 95% CI in the uncertainty message.
#   - rdpct: computes 100*(events1/total1 - events2/total2) likewise; surfaces
#           reported-vs-computed delta + a Wald 95% CI.
#   - md_hl: sanity-checks CI symmetry around the point estimate and p-CI
#           consistency (p<.05 iff 0 outside the 95% CI).
# Full per-arm-rank HL recompute, Fisher-exact / chi-square p-value, and
# Farrington-Manning noninferiority MLE remain v0.6.x+ scope.

test_that("v0.6.0: RR computed-vs-reported message surfaces when cells in scope", {
  txt <- "79/106 (74.5%) versus 82/99 (82.8%), RR 0.90; 95% CI 0.78 to 1.04; p = 0.15"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "RR")
  expect_true(grepl("computed RR", res$uncertainty_reasons, fixed = TRUE))
  expect_true(grepl("Wald-on-log", res$uncertainty_reasons, fixed = TRUE))
  # Sanity-check the computed value: 0.745/0.828 = 0.8997...
  expect_true(grepl("0\\.900?|0\\.899|0\\.9(?!\\d)", res$uncertainty_reasons, perl = TRUE))
})

test_that("v0.6.0: RR per-arm cells captured in parse output", {
  txt <- "8/106 (7.5%) women under PSA and 5/101 (5.0%) women under GA (RR 1.53; 95% CI 0.52 to 4.51; p-value 0.44)"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$arm1_events, 8)
  expect_equal(res$arm1_total, 106)
  expect_equal(res$arm2_events, 5)
  expect_equal(res$arm2_total, 101)
})

test_that("v0.6.0: RR without slash-count clause falls back to honest 'cannot verify' NOTE", {
  txt <- "RR 0.90; 95% CI 0.78 to 1.04; p = 0.15"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "RR")
  expect_true(grepl("per-arm cells not in the same sentence", res$uncertainty_reasons,
                    fixed = TRUE))
  expect_true(is.na(res$arm1_events))
})

test_that("v0.6.0: rdpct computed-vs-reported message surfaces when cells in scope", {
  txt <- "26/106 (24.5%) versus 12/99 (12.1%), risk difference 12.4%; 95% CI 1.9 to 23.0; p = 0.024"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "rdpct")
  expect_true(grepl("computed RD", res$uncertainty_reasons, fixed = TRUE))
  expect_true(grepl("Wald", res$uncertainty_reasons, fixed = TRUE))
})

test_that("v0.6.0: rdpct without slash-count clause falls back to honest NOTE", {
  txt <- "risk difference 12.4%; 95% CI 1.9 to 23.0"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "rdpct")
  expect_true(grepl("per-arm cells not in the same sentence", res$uncertainty_reasons,
                    fixed = TRUE))
})

test_that("v0.6.0: md_hl symmetric CI + consistent p passes sanity checks", {
  # Symmetric CI [-2, 2] around 0; p > .05 with 0 inside CI is consistent.
  txt <- "median difference 0; 95% CI -2 to 2; p-value 0.85"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "md_hl")
  expect_true(grepl("sanity checks passed", res$uncertainty_reasons, fixed = TRUE))
})

test_that("v0.6.0: md_hl asymmetric CI is flagged", {
  # CI [0.5, 9.5] around 5: below=4.5, above=4.5 -> symmetric (won't trip).
  # CI [0.1, 9.5] around 5: below=4.9, above=4.5 -> |0.4|/9.4 = 0.043 (won't trip).
  # CI [1.0, 9.5] around 2: below=1.0, above=7.5 -> |6.5|/8.5 = 0.76 -> trips.
  txt <- "median difference 2; 95% CI 1.0 to 9.5; p-value 0.03"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "md_hl")
  expect_true(grepl("CI is asymmetric", res$uncertainty_reasons, fixed = TRUE))
})

test_that("v0.6.0: md_hl p-CI inconsistency is flagged (p<.05 but 0 in CI)", {
  txt <- "median difference 0; 95% CI -2 to 2; p-value 0.01"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "md_hl")
  expect_true(grepl("p-CI inconsistency", res$uncertainty_reasons, fixed = TRUE))
})
