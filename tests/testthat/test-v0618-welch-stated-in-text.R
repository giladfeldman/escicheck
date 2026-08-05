# v0.6.18 -- a Welch t-test whose df the AUTHOR rounded to a whole number must
# still take the Welch path.
#
# Source: 2026-08-04 escicheck-iterate canary audit of collabra.77859;
# reproduced at HEAD before fixing. PRE-EXISTING (byte-identical at the v0.6.16
# baseline) -- age is a changelog note, not a disposition.
#
# WHAT WENT WRONG
#
# Welch detection is purely numeric:
#
#     is_welch <- !is.na(df1) && abs(df1 - round(df1)) > 0.01
#
# It infers "this is a Welch test" from a fractional df. But authors routinely
# round: collabra.77859 prints "Welch's t(223) = 8.11, p < .001, d = 0.99"
# where the true df is 222.87. The df is a whole number, so `is_welch` is
# FALSE, the non-Welch path binds N = df + 2 = 225, and the recomputed
# d = 1.0813 lands 0.0913 from the reported 0.99 -> FALSE WARN plus an
# INCONSISTENT CI flag. The paper's real cells are n1 = 131, n2 = 135
# (N = 266), where d = 0.9945 -- delta 0.0045, a clean PASS.
#
# The evidence was in the sentence the whole time: the text SAYS "Welch's".
#
# THE RULE: the row's own text naming a Welch / Satterthwaite / unequal-variance
# test is direct evidence of the design and sets `is_welch` regardless of
# whether the printed df happens to be integral. The Welch path then does what
# it already knows how to do -- back-compute N from the reported effect size
# (N = 4t^2/d^2, validated against the df + 2 floor) rather than asserting
# N = df + 2, which for a Welch test is only a LOWER BOUND.

test_that("an author-rounded Welch df stated in the text takes the Welch path", {
  txt <- paste0(
    "As predicted, participants rated the scarf (M = 5.45, SD = 0.95) as a ",
    "more generous gift than the coat (M = 4.18, SD = 1.55), ",
    "Welch's t(223) = 8.11, p < .001, d = 0.99."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  row <- rows[1, ]

  # N = df + 2 = 225 is a Welch LOWER BOUND, not the sample size. Back-computing
  # from the reported d recovers ~266 (the paper's n1 = 131 + n2 = 135).
  n_bound <- suppressWarnings(as.numeric(row$N[1]))
  expect_gt(n_bound, 225)

  # With the right N the reported d is consistent -- the WARN was a false
  # positive produced entirely by the wrong sample size.
  expect_lt(suppressWarnings(as.numeric(row$delta_effect[1])), 0.02)
  expect_false(identical(as.character(row$status[1]), "WARN"))
})

test_that("a fractional df still takes the Welch path (existing behaviour intact)", {
  txt <- "Groups differed, t(222.87) = 8.11, p < .001, d = 0.99."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_lt(suppressWarnings(as.numeric(rows$delta_effect[1])), 0.02)
})

test_that("a plain integer-df t-test with no Welch wording is unaffected", {
  # The guard that keeps this fix narrow: absent the word, an integer df is an
  # ordinary Student t and N = df + 2 remains correct.
  txt <- "Groups differed, t(48) = 2.31, p = .025, d = 0.66."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_equal(suppressWarnings(as.numeric(rows$N[1])), 50)
})

test_that("a neighbouring sentence's Welch does NOT leak onto a paired row", {
  # REGRESSION, caught by the corpus diff on the very render that verified the
  # fix above: the first draft scanned `context_window`, which bleeds adjacent
  # sentences. On collabra.77859 the next paragraph's "Welch's t(198.52)" leaked
  # onto an unrelated PAIRED t(131) row (dz = 0.60) and pushed its N from 132 to
  # 403. Detection is scoped to the row's OWN clause; this pins that scope.
  txt <- paste0(
    "In the separate evaluation condition, we found no support for willingness ",
    "to pay more for the smaller cup (M = 3.99, SD = 1.49) than for the larger ",
    "cup (M = 3.86, SD = 2.98), Welch's t(198.52) = 0.47, p = .640, ",
    "(d = 0.06, 95% CI [-0.18, 0.30]). In the joint condition, as predicted, ",
    "participants were willing to pay more for the larger amount of ice cream ",
    "(M = 4.28, SD = 1.63) than for the smaller amount (M = 3.76, SD = 1.53), ",
    "t(131) = 6.92, p < .001, (dz = 0.60, 95% CI [.42, .79])."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t" &
                !is.na(res$df1) & res$df1 == 131, ]
  expect_gt(nrow(rows), 0)
  # A paired t(131) reporting dz has N = df + 1 = 132. It must NOT inherit the
  # neighbouring Welch clause's treatment.
  expect_lt(suppressWarnings(as.numeric(rows$N[1])), 200)
})

test_that("Satterthwaite / unequal-variance wording also counts", {
  txt <- paste0(
    "Because variances were unequal we used the Satterthwaite correction, ",
    "t(223) = 8.11, p < .001, d = 0.99."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_gt(suppressWarnings(as.numeric(rows$N[1])), 225)
})
