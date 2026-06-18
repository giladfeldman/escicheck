# v0.6.3 (E5): PDF text extraction flattens the "Q_T" subscript to a glued
# "QT", so pat_cochran_q must accept "QT [40] = 104.65" (no underscore) as well
# as "Q_T [40]" and bare "Q[40]". Surfaced by the 2026-06-16 canary audit of
# collabra.90203, where docpluck delivered "QT [40] = 104.65, p < .001" but the
# pattern required an underscore and missed it.

test_that("v0.6.3: flattened 'QT [df]' Cochran Q is extracted", {
  txt <- "Heterogeneity was substantial, QT [40] = 104.65, p < .001, I2 = 61.8%."
  res <- check_text(txt)
  q <- res[res$test_type == "cochran_q", , drop = FALSE]
  expect_equal(nrow(q), 1L)
  expect_equal(q$df1[1], 40)
  expect_equal(q$stat_value[1], 104.65)
})

test_that("v0.6.3: underscored 'Q_T [df]' still extracted (regression guard)", {
  txt <- "Q_T [40] = 104.65, p < .001"
  res <- check_text(txt)
  q <- res[res$test_type == "cochran_q", , drop = FALSE]
  expect_equal(nrow(q), 1L)
  expect_equal(q$df1[1], 40)
  expect_equal(q$stat_value[1], 104.65)
})

test_that("v0.6.3: a plain capitalized word like 'Quality [40]' is not mistaken for Q", {
  # The optional subscript matches at most one letter then REQUIRES a bracket,
  # so a longer word cannot be swallowed as a Cochran Q.
  txt <- "Quality ratings in cluster [40] were high overall."
  res <- check_text(txt)
  tt <- if ("test_type" %in% names(res)) res$test_type else character(0)
  expect_equal(sum(!is.na(tt) & tt == "cochran_q"), 0L)
})
