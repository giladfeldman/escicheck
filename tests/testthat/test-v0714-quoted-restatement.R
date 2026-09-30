# v0.7.14 (d-43ed61): a statistic QUOTED twice is one result, not two.
#
# 10.24072/pci.rr.100726 is a peer-review letter. Reviewer #2 quotes one test
# twice to illustrate APA comma placement:
#
#   (e.g. "... and t(868) = -3.01, p = .006 for the gender..." should be
#    "... and, t(868) = -3.01, p = .006, for the gender...")
#
# and effectcheck emitted two fully scored rows. v0.6.18 built three dedup rules
# and withdrew all three (test-v0618-prose-restatement-dedup.R), because two
# genuinely distinct results can share every printed number -- v0.6.14's
# `r(797) = .16` twice in one sentence, different variables. That file named
# the missing signal: "a span marked as quoted material". The text carries it:
# both copies sit inside quotation marks. Distinct results in running prose are
# not quoted, so the collabra.23443 case is untouched by construction.
#
# The rule: identical printed signature + adjacent + BOTH inside quotation
# marks -> one row, and that row SAYS the statistic was quoted more than once.

t_rows <- function(txt) {
  r <- check_text(txt)
  r[!is.na(r$test_type) & r$test_type == "t", ]
}

PCI <- paste0(
  "(1) I believe that according to the APA style commas should be included ",
  "before and after statistics (e.g. \"... and t(868) = -3.01, p = .006 for the ",
  "gender...\" should be \"... and, t(868) = -3.01, p = .006, for the gender...\"). ",
  "Thank you.")

test_that("a statistic quoted twice in one passage is scored once, and says so", {
  r <- t_rows(PCI)
  expect_equal(nrow(r), 1L)
  u <- paste(unlist(r$uncertainty_reasons), collapse = " | ")
  expect_true(grepl("quoted 2 times", u, fixed = TRUE))
})

test_that("curly quotation marks count as quotation too", {
  txt <- paste0(
    "The reviewer wrote “the effect, t(40) = 2.10, p = .042 was small” and ",
    "later “the effect, t(40) = 2.10, p = .042, was small”.")
  expect_equal(nrow(t_rows(txt)), 1L)
})

test_that("the same numbers twice in UNQUOTED prose are both kept (v0.6.14 invariant)", {
  txt <- paste0(
    "Paid participants showed r(797) = .16, p < .001, and unpaid participants ",
    "also showed r(797) = .16, p < .001.")
  r <- check_text(txt)
  expect_equal(sum(!is.na(r$test_type) & r$test_type == "r"), 2L)
  txt2 <- "The first test was t(40) = 2.10, p = .042, and the second was t(40) = 2.10, p = .042."
  expect_equal(nrow(t_rows(txt2)), 2L)
})

test_that("one quoted copy beside one unquoted result keeps both", {
  txt <- paste0(
    "We found t(40) = 2.10, p = .042. The reviewer asked us to write ",
    "\"t(40) = 2.10, p = .042,\" with a comma.")
  expect_equal(nrow(t_rows(txt)), 2L)
})

test_that("two DIFFERENT quoted statistics are both kept", {
  # Separate sentences: a second quoted statistic in the SAME sentence after a
  # closing quote is currently lost by the parser (TODO d-6ac077), which is a
  # different defect from the one pinned here.
  txt <- paste0(
    "The reviewer wrote \"t(868) = -3.01, p = .006 for the gender\". ",
    "Another reviewer wrote \"t(868) = -2.01, p = .045 for the age\".")
  expect_equal(nrow(t_rows(txt)), 2L)
})
