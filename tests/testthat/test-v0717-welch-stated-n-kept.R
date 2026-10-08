# v0.7.17 (d-99842a): a stated N that already reproduces the reported d is kept.
#
# A Welch df only bounds N from below (N >= df + 2), so a stated N far above
# df + 2 is legitimate whenever variances and group sizes are unequal. The Welch
# branch treated any scraped N > 1.5 * (df + 2) as "likely from a different study"
# and replaced it with a value chosen FROM THE REPORTED d. Measured at 0.7.16 on
# "We recruited N = 300 participants. ... Welch t(150.2) = 0.24, p = .81, d = X.":
#   d = 0.03 (correct: 2 * 0.24 / sqrt(300) = 0.028) -> NOTE, N back-solved to 256
#   d = 0.04 (one unit off)                          -> PASS, N swapped to 152
#   d = 0.05                                         -> WARN, N = 300 kept
# so the correct value graded worse than the wrong one, and which N a row was
# checked against depended on the value being checked. The override exists for a
# stated N that CONTRADICTS the reported d (cog_emo, jesp.2020.104052); when the
# stated N already reproduces d there is no evidence it belongs to another study.

welch_row <- function(txt) {
  r <- as.data.frame(check_text(txt))
  r[!is.na(r$test_type) & r$test_type == "t", , drop = FALSE]
}

stated_n_txt <- function(d) {
  sprintf(paste("We recruited N = 300 participants. The groups did not differ,",
                "Welch t(150.2) = 0.24, p = .81, d = %s."), d)
}

test_that("the correct d is checked against the stated N and passes", {
  r <- welch_row(stated_n_txt("0.03"))
  expect_equal(nrow(r), 1L)
  expect_equal(r$N, 300)
  expect_false(identical(r$N_source, "effect_backsolved"))
  expect_equal(r$check_type, "effect_size")
  expect_equal(r$status, "PASS")
})

test_that("rows differing only in the reported d are checked against the same N", {
  ns <- vapply(c("0.02", "0.03", "0.04", "0.05"),
               function(d) welch_row(stated_n_txt(d))$N, numeric(1))
  expect_true(all(ns == 300))
})

test_that("every d within the d tolerance at the stated N grades PASS, as with N in-clause", {
  # 0.04 is 0.012 from the equal-n 0.028: inside the absolute d tolerance (0.02),
  # the same verdict "Welch t(150.2) = 0.24, d = 0.04, N = 300." gets. The tolerance
  # was audited for tiny d (Sonnet consult 2026-10-07) and left unchanged: Welch
  # vs pooled-SD conventions plus 2-decimal rounding spread printed values over
  # roughly 0.017-0.039 at this t, so a tolerance tight enough to flag 0.04 would
  # flag correct values.
  st <- vapply(c("0.02", "0.03", "0.04"),
               function(d) welch_row(stated_n_txt(d))$status, character(1))
  expect_equal(unname(st), c("PASS", "PASS", "PASS"))
  inclause <- welch_row("The groups did not differ, Welch t(150.2) = 0.24, p = .81, d = 0.04, N = 300.")
  expect_equal(inclause$status, "PASS")
})

test_that("a kept stated N says so in the assumptions", {
  r <- welch_row(stated_n_txt("0.03"))
  a <- paste(unlist(r$assumptions_used), collapse = " | ")
  expect_true(grepl("stated N=300 kept", a, fixed = TRUE))
})

test_that("Hedges' g is put on the d scale before the stated-N comparison", {
  r <- welch_row(paste("We recruited N = 300 participants. The groups did not differ,",
                       "Welch t(150.2) = 0.24, p = .81, g = 0.03."))
  expect_equal(nrow(r), 1L)
  expect_equal(r$N, 300)
  expect_false(identical(r$N_source, "effect_backsolved"))
})

test_that("the correct d never grades worse than a wrong one on the same row", {
  rank <- c(PASS = 1, OK = 1, NOTE = 2, WARN = 3, ERROR = 4)
  ok  <- welch_row(stated_n_txt("0.03"))$status
  bad <- welch_row(stated_n_txt("0.05"))$status
  expect_lte(rank[[ok]], rank[[bad]])
  expect_equal(bad, "WARN")
})

test_that("a stated N that contradicts d is still overridden (control)", {
  # Same shape as the cog_emo case the override was built for: at N = 794 the
  # equal-n d is 0.137, 0.033 away from the printed 0.17, so the stated N is
  # still treated as another study's and replaced.
  txt <- paste("A total of N = 794 people took part. Later, the subgroups differed,",
               "Welch t(513.8) = -1.93, p = .054, d = 0.17.")
  r <- welch_row(txt)
  expect_equal(nrow(r), 1L)
  expect_false(r$N == 794)
})
