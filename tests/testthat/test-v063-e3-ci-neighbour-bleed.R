# v0.6.3 E3: CI binding must not bleed from an interleaved flattened-table row.
#
# Paper: 10.1525/collabra.77859, Study 4. A docpluck-flattened Table 4 is
# interleaved between body sentences; the t(133)=4.44 row's own CI is the colon
# form "95%CI: [.21, .56]" which the pre-0.6.3 pat_CI1 did not match, so the
# bare-bracket fallback picked the FIRST bracket in the sub-chunk -- the table
# cell "0.76 [.50, 1.02]" -- yielding a spurious INCONSISTENT. The fix: accept
# the colon/equals label form AND bind the CI nearest the effect-size value.

test_that("E3: t-test CI binds its own colon-form 95%CI, not an interleaved table cell", {
  txt <- paste(
    "Table 4. Study 4: Replication analyses df d or dz [95%CI]",
    "Separate Evaluation $23.96 $33.70 6.23 < .001 0.76 [.50, 1.02]",
    "Joint Evaluation $31.67 $30.35 1.26 .210 0.11 [-.06, .28]",
    "Joint evaluations",
    paste0("Against our predictions, participants in the joint evaluation ",
           "condition rated the larger set (M = 3.57, SD = 1.43) as less ",
           "attractive than the smaller set (M = 4.19, SD = 1.15), ",
           "t(133) = 4.44, p < .001, dz = 0.38, 95%CI: [.21, .56]."),
    paste0("They also had less positive feelings about the larger set's price ",
           "(M = 3.66, SD = 1.40) than about the smaller set's price ",
           "(M = 4.22, SD = 1.21), t(133) = 4.59, p < .001, dz = 0.40, ",
           "95%CI: [.22, .57]."),
    sep = " ")

  r <- check_text(txt)

  row44 <- r[!is.na(r$stat_value) & abs(r$stat_value - 4.44) < 1e-6, ]
  expect_equal(nrow(row44), 1L)
  expect_equal(row44$ciL_reported[1], 0.21, tolerance = 1e-6)
  expect_equal(row44$ciU_reported[1], 0.56, tolerance = 1e-6)
  # The bled neighbour value must NOT win.
  expect_false(isTRUE(row44$ciL_reported[1] == 0.50))
  if ("ci_check_status" %in% names(r)) {
    expect_false(isTRUE(row44$ci_check_status[1] == "INCONSISTENT"))
  }

  # Regression guard: the adjacent t=4.59 row keeps its own CI.
  row59 <- r[!is.na(r$stat_value) & abs(r$stat_value - 4.59) < 1e-6, ]
  expect_equal(nrow(row59), 1L)
  expect_equal(row59$ciL_reported[1], 0.22, tolerance = 1e-6)
  expect_equal(row59$ciU_reported[1], 0.57, tolerance = 1e-6)
})

test_that("E3: a labeled 95% CI with a colon/equals separator is recognized as labeled", {
  # The colon form previously fell through to the bare-bracket pattern.
  r1 <- check_text("The effect was reliable, t(48) = 2.50, p = .016, d = 0.72, 95% CI: [0.14, 1.30].")
  row1 <- r1[!is.na(r1$stat_value) & abs(r1$stat_value - 2.50) < 1e-6, ]
  expect_equal(nrow(row1), 1L)
  expect_equal(row1$ciL_reported[1], 0.14, tolerance = 1e-6)
  expect_equal(row1$ciU_reported[1], 1.30, tolerance = 1e-6)

  r2 <- check_text("The effect was reliable, t(48) = 2.50, p = .016, d = 0.72, 95% CI = [0.14, 1.30].")
  row2 <- r2[!is.na(r2$stat_value) & abs(r2$stat_value - 2.50) < 1e-6, ]
  expect_equal(row2$ciL_reported[1], 0.14, tolerance = 1e-6)
  expect_equal(row2$ciU_reported[1], 1.30, tolerance = 1e-6)
})
