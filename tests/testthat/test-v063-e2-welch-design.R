# v0.6.3 (E2): a non-integer df is a definitive Welch / independent-samples
# signal, so design_inferred must never be "paired" when df1 is fractional
# (a paired t has integer df = n - 1). Surfaced by the 2026-06-16 canary audit
# of collabra.77859 (Welch rows with df1 = 257.03 tagged design_inferred=paired).

test_that("v0.6.3: non-integer df with a paired-type effect is NOT tagged paired", {
  txt <- "Welch's t(257.03) = 6.23, p < .001, (dz = 0.76, 95% CI [0.50, 1.02])."
  res <- check_text(txt)
  t_rows <- res[res$test_type == "t" & !is.na(res$df1), , drop = FALSE]
  expect_true(nrow(t_rows) >= 1)
  expect_false(any(abs(t_rows$df1 - round(t_rows$df1)) > 1e-9 &
                   t_rows$design_inferred == "paired"))
})

test_that("v0.6.3: non-integer-df t reclassifies to independent", {
  txt <- "Welch's t(257.03) = 6.23, p < .001, (dz = 0.76, 95% CI [0.50, 1.02])."
  res <- check_text(txt)
  row <- res[res$test_type == "t" & !is.na(res$df1) &
             abs(res$df1 - round(res$df1)) > 1e-9, , drop = FALSE]
  expect_true(nrow(row) >= 1)
  expect_equal(row$design_inferred[1], "independent")
})

test_that("v0.6.3: an integer-df paired t is still allowed to be paired", {
  # Regression guard: the non-integer guard must not touch genuine paired t's.
  txt <- "t(133) = 4.44, p < .001, dz = 0.38, 95% CI [0.21, 0.56]."
  res <- check_text(txt)
  row <- res[res$test_type == "t", , drop = FALSE]
  expect_true(nrow(row) >= 1)
  expect_equal(row$df1[1], 133)
  expect_equal(row$design_inferred[1], "paired")
})
