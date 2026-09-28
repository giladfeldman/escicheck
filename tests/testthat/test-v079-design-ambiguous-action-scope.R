# v0.7.9 — `design_ambiguous_action` has an EXCEPTION, and it must be pinned.
#
# Raised by a downstream consumer 2026-09-02: two rows in 148,984 sit at status ERROR with
# design_ambiguous = TRUE while the caller passed design_ambiguous_action =
# "WARN". Its question was the right one — a policy parameter that silently
# does not apply stays invisible for exactly as long as nobody probes it.
#
# The answer is that the parameter IS honoured, but only where design ambiguity
# is a candidate explanation for the discrepancy. check.R's guard says so in
# its own comment:
#
#     "Guard: reported value must be plausibly between the independent and
#      paired variants (with 50% margin). If it's far from ALL variants, it's
#      genuinely wrong."
#
# A reported effect that matches NEITHER the independent nor the paired variant
# is not explained by not knowing which design was used. Same principle as the
# v0.6.18 omnibus-df rule: an effect matching NEITHER N keeps its flag.
#
# That behaviour was correct and undocumented, which is why it read as a bypass
# from outside. These tests pin BOTH SIDES so the exception can never be
# silently widened, narrowed, or removed — and so a future reader finds the
# reasoning attached to a failing test rather than to a comment.
#
# The fixture reconstructs the consumer's psychological_science row from its own
# numbers: F(1, 125) with d_ind_equalN = 0.952606, i.e. F = (0.952606 *
# sqrt(125) / 2)^2 = 28.358. It reproduces delta = 2.0549 against their
# 2.04739, confidence 4, extraction_suspect TRUE, ambiguity_reason
# "[category: structural-design]", and both uncertainty reasons verbatim.

F_MATCHING_CONSUMER_ROW <- (0.952606 * sqrt(125) / 2)^2   # 28.358

test_that("v0.7.9: the action is NOT applied when the reported effect matches NEITHER design", {
  # d = 3.00 against computed variants spanning roughly 0.47..0.95. The
  # reported value is far outside the range plus its 50% margin, so design
  # ambiguity cannot account for it and the ERROR stands.
  txt <- sprintf("There was an effect, F(1, 125) = %.2f, p < .001, d = 3.00.",
                 F_MATCHING_CONSUMER_ROW)

  warn_r <- check_text(txt, design_ambiguous_action = "WARN")
  note_r <- check_text(txt, design_ambiguous_action = "NOTE")

  expect_equal(nrow(warn_r), 1L)
  expect_true(isTRUE(warn_r$design_ambiguous[1]))
  expect_match(warn_r$ambiguity_reason[1], "structural-design", fixed = TRUE)

  # The exception: BOTH settings give ERROR. The parameter is inert here by
  # design, and that is the whole point of the test.
  expect_equal(warn_r$status[1], "ERROR")
  expect_equal(note_r$status[1], "ERROR")

  # And the row must SAY why, rather than leaving the caller to infer that its
  # policy was overridden.
  expect_match(paste(warn_r$uncertainty_reasons, collapse = " "),
               "Extreme discrepancy", fixed = TRUE)
})

test_that("v0.7.9: the action IS applied when design ambiguity can explain the gap", {
  # Same F, same df, same structural-design ambiguity — but a reported d inside
  # the variant range. Without this control the test above passes for a
  # parameter that never works at all, which is the failure it is meant to
  # exclude.
  txt <- sprintf("There was an effect, F(1, 125) = %.2f, p < .001, d = 1.30.",
                 F_MATCHING_CONSUMER_ROW)

  expect_equal(check_text(txt, design_ambiguous_action = "WARN")$status[1], "WARN")
  expect_equal(check_text(txt, design_ambiguous_action = "NOTE")$status[1], "NOTE")
  expect_equal(check_text(txt, design_ambiguous_action = "ERROR")$status[1], "ERROR")
})

test_that("v0.7.9: both rows are STRUCTURAL-DESIGN, not cross-family", {
  # The two rows differ only in the reported effect size, so they must carry
  # the same v0.5.11 category tag. This pins the tag against the reading that
  # sent the first answer to the consumer off course: the phrase "ANOVA design
  # unclear (between/within/mixed)" in uncertainty_reasons is GENERIC ANOVA
  # wording that appears on both, NOT a cross-family marker. The machine
  # readable tag is the one to key on, which is exactly why v0.5.11 added it.
  base <- "There was an effect, F(1, 125) = %.2f, p < .001, d = %s."
  extreme  <- check_text(sprintf(base, F_MATCHING_CONSUMER_ROW, "3.00"))
  in_range <- check_text(sprintf(base, F_MATCHING_CONSUMER_ROW, "1.30"))

  for (r in list(extreme, in_range)) {
    expect_match(r$ambiguity_reason[1], "[category: structural-design]", fixed = TRUE)
    expect_no_match(r$ambiguity_reason[1], "cross-family", fixed = TRUE)
    expect_match(paste(r$uncertainty_reasons, collapse = " "),
                 "ANOVA design unclear", fixed = TRUE)
  }
})
