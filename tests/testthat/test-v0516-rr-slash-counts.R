# v0.5.16: clinical-trial risk ratio with two-proportion slash counts
# Form: "<n1>/<N1> (<pct>%) versus <n2>/<N2> (<pct>%) ... RR <val>; 95% CI
# <lo> to <hi>; p[-value]? = <val>". From PLOS Medicine PROSECCO trial
# (10.1371/journal.pmed.1004323). Full RR-vs-cells verification deferred
# to v0.6.x; this cycle resolves the PARSE-MISS aspect.

test_that("v0.5.16: RR with semicolons + p = (case 1) parses", {
  txt <- "79/106 (74.5%) versus 82/99 (82.8%), RR 0.90; 95% CI 0.78 to 1.04; p = 0.15"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "RR")
  expect_equal(res$stat_value, 0.90)
  expect_equal(res$p_reported, 0.15)
})

test_that("v0.5.16: RR with 'p-value' (no '=') parses", {
  txt <- "8/106 (7.5%) women under PSA and 5/101 (5.0%) women under GA (RR 1.53; 95% CI 0.52 to 4.51; p-value 0.44)"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "RR")
  expect_equal(res$stat_value, 1.53)
  expect_equal(res$p_reported, 0.44)
})

test_that("v0.5.16: RR with 'versus' separator parses", {
  txt <- "8/77 (10.4%) versus 9/68 (13.2%); RR 0.79; 95% CI 0.32 to 1.92; p-value 0.60"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "RR")
  expect_equal(res$stat_value, 0.79)
  expect_equal(res$p_reported, 0.60)
})

test_that("v0.5.16: RR row carries NOTE-level uncertainty", {
  txt <- "RR 0.90; 95% CI 0.78 to 1.04; p = 0.15"
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_equal(res$test_type, "RR")
  # status NOTE / OK / SKIP all acceptable -- the key is it's not silently 0 rows
  expect_true(res$status %in% c("NOTE", "OK", "SKIP", "PASS"))
  expect_true(grepl("Risk ratio|RR|v0\\.6\\.x|verification", res$uncertainty_reasons %||% "",
                    ignore.case = TRUE))
})
