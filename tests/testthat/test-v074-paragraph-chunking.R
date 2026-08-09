# v0.7.4 -- the sentence splitter never broke on a paragraph boundary.
#
# THE DEFECT: a two-column PDF whose columns the text extractor merges produces
# "<end of column A>.\n\n<continuation of column B>". The splitter broke only on
# `[.!?]` followed by whitespace and an UPPERCASE letter, so a column that
# resumes with a DIGIT stayed glued to the previous column -- and the parser
# then paired an effect size from one column with a test statistic from another
# and graded them against each other.
#
# Reproduced in the wild, tmp/iterate/dptext/spps.txt location 216 at v0.7.3:
#
#   "The overall effect size was d = 0.33, 95% CI [0.09, 0.57].
#
#    0.75, 95% CI = [0.54, 0.95], t = 7.47, p < .001)."
#
# ONE row: test_type "t", stat_value 7.47 (column B), effect_reported 0.33
# (column A), graded g_ind = 0.379 against N = 1555, status WARN. The `0.75`
# that actually belongs with t = 7.47 was dropped, and d = 0.33 was charged
# against a statistic it was never reported with. Neither number is wrong in
# the article; the PAIRING is fabricated.
#
# THE FIX (R/parse.R, the chunk splitter): the sentence rule's LOOKAHEAD now
# also accepts a digit, when the boundary is a blank line.
#
# WHAT IS DELIBERATELY NOT FIXED: a column truncated WITHOUT sentence-ending
# punctuation still merges. The first version of this fix split on any blank
# line followed by a capital or a digit, which does cover that case -- and two
# independent cross-model reviews plus the whole-corpus diff produced seven
# counterexamples where it severed a test statistic from its own effect size,
# CI, p-value or N. Six reproduced locally and are pinned below as guards; each
# was watched to FAIL against that first version. Being unable to break a
# genuine merge costs one un-checked row; breaking a real statement costs a
# reported value, which is strictly worse on a science tool.
#
# The defect tests were authored against v0.7.3 and watched to fail there; the
# guards were authored against the over-broad first fix and five of the seven
# were watched to fail against it (see the note above the guard block).

# ---------------------------------------------------------------------------
# The defect itself
# ---------------------------------------------------------------------------

test_that("a digit-initial paragraph is not glued to the previous one", {
  # At v0.7.3 this returned ONE row pairing d = 0.33 with t = 7.47.
  txt <- paste0(
    "The overall effect size was d = 0.33, 95% CI [0.09, 0.57].\n\n",
    "0.75, 95% CI = [0.54, 0.95], t = 7.47, p < .001).")
  res <- effectcheck::check_text(txt)

  expect_equal(nrow(res), 2L)

  # Column A survives on its own as a bare reported effect (v0.6.16
  # `d_reported_only`) -- the split must not LOSE the orphaned effect, which
  # would be worse than the mis-pairing it removes.
  a <- res[res$test_type == "d_reported_only", ]
  expect_equal(nrow(a), 1L)
  expect_equal(a$effect_reported[1], 0.33)
  expect_equal(a$ciL_reported[1], 0.09)
  expect_equal(a$ciU_reported[1], 0.57)

  # Column B keeps its own statistic and its own p-value, and carries NO effect
  # size -- the `d = 0.33` label belongs to the other column.
  b <- res[res$test_type == "t", ]
  expect_equal(nrow(b), 1L)
  expect_equal(b$stat_value[1], 7.47)
  expect_equal(b$p_reported[1], 0.001)
  expect_true(is.na(b$effect_reported[1]))

  # And no row anywhere may still carry the fabricated pairing.
  expect_false(any(!is.na(res$stat_value) & res$stat_value == 7.47 &
                   !is.na(res$effect_reported) & res$effect_reported == 0.33))
})

test_that("the digit boundary requires a blank line, not merely a space", {
  # "sentence. 30% of trials" is ordinary prose on one line, not a column
  # merge. Only a BLANK LINE carries the paragraph signal, so the digit
  # lookahead must not fire on a plain space.
  res <- effectcheck::check_text(
    "The manipulation worked. 30% of trials were excluded, t(48) = 2.31, p = .025, d = 0.74.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$stat_value[1], 2.31)
  expect_equal(res$effect_reported[1], 0.74)
})

# ---------------------------------------------------------------------------
# Guards: a chunk boundary may never fall inside a single reported quantity, or
# between a test statistic and its own effect size / CI / p-value / N.
#
# All seven were produced by cross-model review of the over-broad first fix
# (Codex/gpt-5.5 and Claude Sonnet 5, 2026-08-09, independently converging on
# the same class) and every one was run against that version before being
# pinned. FIVE reproduced as row-level defects and were watched RED here
# (t-vs-effect, effect-vs-CI, percent-vs-CI, digit-terminated period, and the
# p-value break). TWO did not reproduce -- the two sample-size cases, where the
# implausible truncated N is rejected and the document-level scan or df + 2
# supplies the value regardless. Those two are marked in place as documentation
# of intent rather than tests that have ever been red; a reviewer finding that
# does not reproduce is recorded, not silently promoted.
# ---------------------------------------------------------------------------

test_that("a blank line does not sever a t-test from its own effect size", {
  # Codex finding 2. The over-broad rule split before "Cohen's" and the t row
  # lost d = 0.75 entirely -- the reported-vs-computed check silently stopped
  # running while the row still reported status OK.
  res <- effectcheck::check_text(
    "The contrast was significant, t(38) = 2.31, p = .026\n\nCohen's d = 0.75, 95% CI [0.09, 1.41].")
  expect_equal(nrow(res), 1L)
  expect_equal(res$stat_value[1], 2.31)
  expect_equal(res$effect_reported[1], 0.75)
  expect_equal(res$ciL_reported[1], 0.09)
})

test_that("a blank line does not sever an effect size from its own CI", {
  # Codex finding 1: the effect value and its interval are separated by a line
  # break with no comma, so the CI lands in a chunk of its own.
  res <- effectcheck::check_text(
    "The contrast was significant, t(38) = 2.31, p = .026, d = 0.75\n\n95% CI [0.09, 1.41].")
  expect_equal(nrow(res), 1L)
  expect_equal(res$effect_reported[1], 0.75)
  expect_equal(res$ciL_reported[1], 0.09)
  expect_equal(res$ciU_reported[1], 1.41)
})

test_that("a blank line between a percent sign and CI does not split", {
  # Sonnet finding 4: the break falls inside the token "95% CI". The over-broad
  # rule left the t row with a CI and no effect, and the d = 0.65 was lost.
  res <- effectcheck::check_text(
    "The effect was medium to large, d = 0.65, 95%\n\nCI [0.40, 0.90], t(58) = 3.45, p = .003).")
  expect_equal(nrow(res), 1L)
  expect_equal(res$effect_reported[1], 0.65)
  expect_equal(res$ciL_reported[1], 0.40)
})

test_that("a blank line after a digit-terminated period does not split", {
  # Sonnet finding 1. `0.` ends with a period that satisfies the sentence
  # anchor, so the anchor alone is not enough: without `(?<!\d\.)` the chunk
  # breaks between "d = 0." and "65, 95% CI [...]", stranding the CI in a chunk
  # of its own and leaving the t row without one.
  #
  # NOT asserted here: that `d` reads 0.65. It reads 0 -- v0.7.3 truncates the
  # value at the line break, and this change is neutral on that (verified
  # against HEAD: 1 row, d = 0, WARN, identical before and after). Rejoining
  # "<digits>.\n<digits>" was considered and REFUSED on corpus evidence: across
  # the 48 real-article texts the shape `<label> = <digits>.` at end of line
  # occurs 22 times and every one is a sentence-final period (`p < .001.`),
  # while `<digit>.\n<digit>` occurs 221 times and is dominated by reference
  # numbering and section headings (`...962-967.\n\n30.\n\nSingh H, ...`, which
  # a joiner would fuse into `967.30`). A bridge with no observed benefit and a
  # demonstrated corruption path is exactly the v0.6.20 defect class.
  res <- effectcheck::check_text(
    "The effect was medium, d = 0.\n\n65, 95% CI [0.40, 0.90], t(58) = 3.45, p = .001).")
  expect_equal(nrow(res), 1L)
  expect_equal(res$stat_value[1], 3.45)
  expect_equal(res$ciL_reported[1], 0.40)
  expect_equal(res$ciU_reported[1], 0.90)
  # The discriminating assertion: the effect label stays in the statistic's own
  # chunk. Under the over-broad rule this was NA (severed); it is now non-NA.
  expect_false(is.na(res$effect_reported[1]))
})

test_that("a blank line inside a p-value does not split", {
  # Sonnet finding 2: ".03\n\n1" is one p-value, .031, broken across a column.
  # Splitting publishes p = .03 -- a wrong number, not a missing one.
  res <- effectcheck::check_text(
    "t(58) = 3.45, p = .03\n\n1, d = 0.65, 95% CI [0.40, 0.90]).")
  expect_equal(nrow(res), 1L)
  expect_equal(res$effect_reported[1], 0.65)
})

test_that("a blank line inside a sample size does not split", {
  # Sonnet finding 3: "N = 1\n\n204" is N = 1204. Like Codex finding 3 below,
  # this did NOT reproduce as a row-level defect even against the over-broad
  # rule -- the implausible N = 1 is rejected either way and the row falls back
  # to df + 2 -- so it is documentation of intent, not a test that has been red.
  res <- effectcheck::check_text(
    "The final sample consisted of N = 1\n\n204 participants, t(58) = 3.45, p = .003, d = 0.65.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$stat_value[1], 3.45)
  expect_equal(res$effect_reported[1], 0.65)
})

test_that("a blank line does not sever a correlation from its sample size", {
  # Codex finding 3. This one did NOT reproduce as a row-level defect even
  # against the over-broad rule -- N = 54 is recovered by the document-level
  # scan -- but it is pinned so the recovery path stays load-bearing rather
  # than incidental.
  res <- effectcheck::check_text(
    "The association was significant, r = .32, p = .018\n\nN = 54.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$N[1], 54)
})

# ---------------------------------------------------------------------------
# Guards on behaviour that must not change
# ---------------------------------------------------------------------------

test_that("a value wrapped across a blank line is still joined, never split", {
  # "d =\n\n0.80" is a PDF wrap, not a paragraph. normalize_text joins it before
  # the splitter sees it; splitting instead would strand the 0.80.
  res <- effectcheck::check_text(
    "The effect was large, t(48) = 2.31, p = .025, d =\n\n0.80 overall.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$stat_value[1], 2.31)
  expect_equal(res$effect_reported[1], 0.80)
})

test_that("a lowercase mid-sentence wrap across a blank line is still joined", {
  res <- effectcheck::check_text(
    "The result was reliable and\n\nthe effect held, t(48) = 2.31, p = .025, d = 0.74.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$stat_value[1], 2.31)
  expect_equal(res$effect_reported[1], 0.74)
})

test_that("the v0.6.20 bridging invariants still hold across the new split", {
  # Invariant 2 of v0.6.20: a bounded prose bridge adopts the next line's number
  # only when it carries a decimal point. "(see Table\n2)" must leave d = 0.74
  # and p = .025 intact.
  res <- effectcheck::check_text("t(48) = 2.31, p = .025, d = 0.74 (see Table\n2)")
  expect_equal(nrow(res), 1L)
  expect_equal(res$effect_reported[1], 0.74)
  expect_equal(res$p_reported[1], 0.025)
})

test_that("an uppercase next sentence still splits exactly as before", {
  res <- effectcheck::check_text(paste0(
    "The first test was t(48) = 2.31, p = .025, d = 0.74.\n\n",
    "The second was F(1, 30) = 4.42, p = .044."))
  expect_equal(nrow(res), 2L)
  expect_equal(res$stat_value[res$test_type == "t"], 2.31)
  expect_equal(res$stat_value[res$test_type == "F"], 4.42)
})

test_that("a single newline inside a paragraph does not split", {
  res <- effectcheck::check_text(
    "Performance on the Stroop\nTask improved, t(48) = 2.31, p = .025, d = 0.74.")
  expect_equal(nrow(res), 1L)
  expect_equal(res$effect_reported[1], 0.74)
})
