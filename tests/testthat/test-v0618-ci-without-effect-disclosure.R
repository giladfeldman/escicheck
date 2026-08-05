# v0.6.18 -- a row carrying a reported CI but NO parseable effect size must say
# that the effect size was never checked.
#
# Source: 2026-08-05 escicheck-iterate cycle-2 Sonnet canary audit of
# collabra.90203; reproduced at HEAD before fixing.
#
# WHAT WENT WRONG
#
# On that paper the partial-eta-squared symbol is drawn as filled vector curves
# with no character object in the source PDF (mechanism corrected 2026-08-05 by
# triple-verification re-audit; this comment previously said "has no ToUnicode
# mapping", which is refuted -- it is ink, not badly-encoded text. The test and
# the behaviour it pins are unaffected), so the body text arrives as a nameless "F(2, 998) = 3.91,
# p = .02, = .008. 95% CI [.000, .021]" -- the CI parses, the effect size
# cannot. The row then narrowed to a p-value-only check and published
#
#     status = "OK", ci_check_status = "MATCH", effect_reported = NA
#
# with nothing in `uncertainty_reasons` about the dropped effect size. A reader
# sees a green row and cannot tell the paper reported an effect size the tool
# never verified. (effectcheck already compensates by typing the value from the
# TABLE view, which is why the paper's eta-squareds are not lost outright --
# but that does not make silence on the body-text row honest.)
#
# THE RULE: a confidence interval cannot exist without an estimate, so a
# reported CI with an unparseable effect size is positive evidence that an
# effect size WAS reported. Say so on the row.

test_that("a reported CI with no parseable effect size is disclosed", {
  # The stripped-glyph shape: the eta token is gone, the CI survives.
  txt <- paste0(
    "We found some support for a main effect of Identifiability, ",
    "F(2, 998) = 3.91, p = .02, = .008. 95% CI [.000, .021]."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "F", ]
  expect_gt(nrow(rows), 0)
  row <- rows[1, ]

  # Precondition: this is the shape under test -- CI present, effect absent.
  expect_true(is.na(suppressWarnings(as.numeric(row$effect_reported[1]))))
  expect_false(is.na(suppressWarnings(as.numeric(row$ciL_reported[1]))))

  # The row must not imply a complete check.
  expect_match(as.character(row$uncertainty_reasons[1]),
               "was NOT verified")
})

test_that("a row with BOTH an effect size and a CI is not given the disclosure", {
  txt <- "Groups differed, t(48) = 2.31, p = .025, d = 0.66, 95% CI [0.09, 1.23]."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_false(grepl("was NOT verified",
                     as.character(rows$uncertainty_reasons[1])))
})

test_that("a row with no CI at all is not given the disclosure", {
  txt <- "Groups differed, t(48) = 2.31, p = .025."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_false(grepl("was NOT verified",
                     as.character(rows$uncertainty_reasons[1])))
})

test_that("emitted uncertainty messages are free of mojibake", {
  # Found in the same render: the ANOVA-design message was authored with an
  # escaped em-dash (--), which passed the source ASCII check but reached
  # the user as a corrupted byte. Every emitted message must be plain ASCII.
  txt <- "The interaction was significant, F(2, 100) = 4.10, p = .019."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "F", ]
  expect_gt(nrow(rows), 0)
  msg <- as.character(rows$uncertainty_reasons[1])
  if (!is.na(msg) && nzchar(msg)) {
    expect_false(grepl("[^\x01-\x7F]", msg, perl = TRUE))
  }
})
