# v0.6.20 -- MetaESCI O-1 / O-2 and the sweep they triggered.
#
# THE DEFECT CLASS: a normalization rule that DELETES source text.
#
# normalize_text() repairs PDF text-layer artifacts before anything is parsed.
# Several of its rules "bridge" a line wrap by skipping over a span and adopting
# a number from the next line. None of the three that did so checked whether the
# span it skipped contained a value -- so a REPORTED STATISTIC was destroyed and
# replaced by whatever number happened to open the next line.
#
# MetaESCI filed this as two unrelated defects with two different diagnoses:
#   O-1: "the effect-size capture window bleeds across the newline and takes the
#         leading digit of a wrapped Table/Figure/Study/Experiment label"
#   O-2: "the chi-square continuation guard is missing a newline terminator"
# Neither diagnosis was right. There is ONE root cause, in normalize_text, and
# it fires on any `<lowercase word> = <prose>\n<number>` -- the label vocabulary
# is incidental, and the capture patterns were never involved.
#
# Two invariants now govern every bridging rule (see R/parse.R):
#   1. The skipped span must be DIGIT-FREE -- a bridge may never discard a
#      number that is already present.
#   2. When prose is skipped, the ADOPTED number must carry a DECIMAL POINT --
#      a bare integer opening a line is a page/section number, not a statistic.
#
# Invariant 2 came from a cross-model review of the fix for invariant 1, which
# found three surviving paths; two are pinned below. Every test here was
# authored against the UNFIXED code and watched to fail first.

# ---------------------------------------------------------------------------
# O-1: a wrapped label replaces or erases a reported effect size
# ---------------------------------------------------------------------------

test_that("a wrapped experiment label does not replace the reported effect size", {
  # At 0.6.19 the captured value tracked the LABEL DIGIT and ignored the article:
  #   label 1b -> etap2 = 1.00, ERROR     (substitution)
  #   label 2b -> etap2 = NA,   "OK"      (silent LOSS -- the dangerous one)
  #   label 0b -> etap2 = 0.00, ERROR     (substitution)
  #   label Bb -> etap2 = 0.86, PASS      (alphabetic label parsed cleanly)
  # The alphabetic control is what proves the digit is being adopted.
  mk <- function(label) sprintf(paste0(
    "F(2,16) = 49.05, p < .001, etap2 = .86, and Experiment\n%s, ",
    "F(2,38) = 90.62, p < .001, etap2 = .83"), label)
  for (label in c("1b", "2b", "0b", "Bb")) {
    res <- effectcheck::check_text(mk(label))
    expect_equal(res$effect_reported[1], 0.86,
                 info = paste("label", label))
    expect_equal(as.character(res$status[1]), "PASS",
                 info = paste("label", label))
  }
})

test_that("the captured effect size tracks the ARTICLE, not the label digit", {
  # The sharpest form of the defect: at 0.6.19 all three of these returned 1.00.
  mk <- function(val) sprintf(paste0(
    "F(2,16) = 49.05, p < .001, etap2 = %s, and Experiment\n1b, ",
    "F(2,38) = 90.62, p < .001, etap2 = .83"), val)
  expect_equal(effectcheck::check_text(mk(".86"))$effect_reported[1], 0.86)
  expect_equal(effectcheck::check_text(mk(".42"))$effect_reported[1], 0.42)
  expect_equal(effectcheck::check_text(mk(".10"))$effect_reported[1], 0.10)
})

test_that("ordinary Table / Figure / Study cross-references do not corrupt a row", {
  # MetaESCI's corpus reconciliation found the commonest trigger is not an
  # experiment label at all but a wrapped cross-reference, which appears in APA
  # prose constantly. Each of these was verified article-text <-> shipped CSV.
  res <- effectcheck::check_text("t(48) = 2.31, p = .025, d = 0.74 (see Table\n2)")
  expect_equal(res$effect_reported[1], 0.74)   # shipped d = 2, WARN
  expect_equal(res$p_reported[1], 0.025)       # the p was destroyed too

  res <- effectcheck::check_text("F(1, 34) = 8.11, p = .007, eta2p = .08 (See Figure\n3-b)")
  expect_equal(res$effect_reported[1], 0.08)   # shipped NA with status OK

  res <- effectcheck::check_text(
    "F(1, 34) = 8.11, p = .007, eta2p = .44 (see \"1. Study\n1 for details\")")
  expect_equal(res$effect_reported[1], 0.44)   # shipped etap2 = 1, WARN
})

test_that("the trigger needs no label word at all", {
  # This is why MetaESCI's incidence figure (42 rows / 39 articles) is a floor:
  # their locator only searched a fixed label vocabulary, but the rule keys on
  # `[a-z]+ =` plus a wrapped digit. Any prose triggers it.
  res <- effectcheck::check_text(
    "t(48) = 2.31, p = .025, d = 0.74, which was larger than\n2 of the other contrasts")
  expect_equal(res$effect_reported[1], 0.74)
})

# ---------------------------------------------------------------------------
# O-2: same root cause, presenting as a dropped chi-square
# ---------------------------------------------------------------------------

test_that("a chi-square followed by a newline and a digit is not dropped", {
  # Filed as a chi-square-specific continuation-guard defect. In fact the SAME
  # bridging rule matched "n = " inside the chi-square's own parentheses and
  # deleted "211) = 12.74, p = .013", destroying the chi-square token itself --
  # hence zero rows. Observed live in 10.1525/collabra.194 (16 -> 15 rows).
  for (txt in c(
    "chi2 (4, n = 211) = 12.74, p = .013\n\n10 negative affect items",
    "chi2 (4, n = 211) = 12.74, p = .013\n10 negative affect items",
    "chi2 (4, n = 211) = 12.74, p = .013\n\nnegative affect items"
  )) {
    res <- effectcheck::check_text(txt)
    expect_equal(nrow(res), 1L, info = txt)
  }
  res <- effectcheck::check_text(
    "chi2 (4, n = 211) = 12.74, p = .013\n\n10 negative affect items")
  expect_equal(res$stat_value[1], 12.74)
  expect_equal(res$p_reported[1], 0.013)
})

test_that("the t and r rows filed as O-2 'controls' keep their real values", {
  # These were reported as UNAFFECTED because they still return one row. They
  # do -- carrying a FABRICATED p-value. Row count is not a sufficient probe for
  # this class, which is why the defect went unnoticed on those two branches.
  res <- effectcheck::check_text("t(48) = 2.31, p = .025, d = 0.65\n\n10 negative affect items")
  expect_equal(res$effect_reported[1], 0.65)   # was NA
  expect_equal(res$p_reported[1], 0.025)       # was 1 (from a fabricated "p = 10")

  res <- effectcheck::check_text("r(351) = .164, p = .050\n\n10 negative affect items")
  expect_equal(res$p_reported[1], 0.05)        # was 1

  # F genuinely was unaffected: it has no lowercase `x =` clause to match.
  expect_equal(nrow(effectcheck::check_text(
    "F(1, 222) = 17.62, p < .001\n\n10 negative affect items")), 1L)
})

# ---------------------------------------------------------------------------
# The section-number stripper: the same class, from the opposite direction
# ---------------------------------------------------------------------------

test_that("a line-wrapped value at the start of a line is not stripped as a section number", {
  # `\d+(\.\d+)+\.?[ \t]+` at line start matched "0.86 " exactly as it matched
  # "3.3.1 ", so a wrapped value was deleted and the row shipped
  # effect_reported = NA with status OK -- the silent-loss shape again, from a
  # rule MetaESCI never looked at.
  res <- effectcheck::check_text(
    "The effect was large, t(30) = 2.55, p = .016, d =\n0.86 in the treatment group")
  expect_equal(res$effect_reported[1], 0.86)

  res <- effectcheck::check_text("F(1, 30) = 4.42, p =\n0.037 for the interaction")
  expect_equal(res$p_reported[1], 0.037)
})

test_that("a wrapped value that ENDS A SENTENCE is not stripped either", {
  # Found by neither MetaESCI nor the cross-model review: the sentence period
  # supplies the trailing dot, so "0.86. " is shaped exactly like "3.3. ".
  # A dangling assignment operator on the previous line is the discriminator --
  # nothing numbers a section immediately after "d =".
  res <- effectcheck::check_text(paste0(
    "The effect was significant, t(30) = 2.55, p = .016, d =\n",
    "0.86. In Study 2 we replicated it"))
  expect_equal(res$effect_reported[1], 0.86)

  res <- effectcheck::check_text(
    "F(1, 30) = 4.42, p =\n0.037. The interaction was not significant")
  expect_equal(res$p_reported[1], 0.037)
})

test_that("genuine section numbers are still stripped", {
  # The fix must not simply disable the rule.
  expect_true(grepl("^Results", effectcheck:::normalize_text("3.3.1 Results\nF(1, 30) = 4.42")))
  expect_true(grepl("^Results", effectcheck:::normalize_text("3.3. Results\nF(1, 30) = 4.42")))
})

# ---------------------------------------------------------------------------
# Invariant 2: a prose-skipping bridge must adopt a DECIMAL number
# (both cases from the cross-model review of the invariant-1 fix, reproduced
#  locally at HEAD before being acted on)
# ---------------------------------------------------------------------------

test_that("a bridge across prose does not adopt a bare integer as an effect size", {
  # Digit-freeness alone still let the rule reach across prose and adopt a line
  # number: "d = see Table\n1 for means" normalized to "d = 1", publishing
  # effect_reported = 1 -- plausible enough to sail past the guard.
  res <- effectcheck::check_text("t(20) = 2.09, p = .049, d = see Table\n1 for means")
  expect_true(is.na(res$effect_reported[1]))
})

test_that("a bridge across prose does not fabricate a p-value from a section number", {
  # "p = ns\n1 Results" normalized to "p = 1" and shipped p_reported = 1 with
  # p_valid = TRUE.
  res <- effectcheck::check_text("t(20) = 1.21, p = ns\n1 Results")
  expect_true(is.na(res$p_reported[1]))
})

test_that("genuine line-wrap repair still works, including a wrapped integer N", {
  # The bare case -- an assignment whose line simply ends at the "=" -- is
  # handled upstream by the whitespace-only joiner, which deletes nothing. That
  # is what keeps a wrapped integer sample size working after invariant 2.
  expect_equal(effectcheck::check_text("t(30) = 2.55, p = .016, d =\n0.80")$effect_reported[1], 0.80)
  expect_equal(effectcheck::check_text("F(1, 30) = 4.42, p =\n0.037")$p_reported[1], 0.037)
  expect_equal(effectcheck::check_text(
    "F(1, 30) = 4.42, p = on social distance\n0.837")$p_reported[1], 0.837)
  expect_true(grepl("n = 120", effectcheck:::normalize_text("chi2(1) = 4.2, p = .04, n =\n120")))
})
