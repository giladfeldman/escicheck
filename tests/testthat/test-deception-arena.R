# ============================================================================
# Tier-5 verification fixture — ScienceArena `stats-extraction-v1` contract
#
# The arena adapter reads each row of effectcheck::check_text() output and
# computes flagged_suspicious as the OR of six row-level signals. This file
# pins that contract: every row corresponding to a deceptive stat in the
# tier-5 inputs MUST be flagged by at least one of those signals.
#
# Source spec: C:/Users/filin/Dropbox/Vibe/MetaScienceProjects/ScienceArena/
#              docs/deception-detection-spec-for-escimate.md
# ============================================================================

# Helper: a row is flagged-suspicious if any of the six signals fires.
is_flagged <- function(row) {
  isTRUE(row$decision_error) ||
    isTRUE(row$extraction_suspect) ||
    isTRUE(row$insufficient_data) ||
    isTRUE(row$df_arity_mismatch) ||
    (!is.na(row$ambiguity_level) && row$ambiguity_level == "highly_ambiguous") ||
    (!is.na(row$status) && row$status %in% c("WARN", "ERROR"))
}

# ============================================================================
# Tier-5 deterministic inputs (from arena seed=0). Each test_that asserts:
#   1. Suspicion disjunction over every emitted row.
#   2. Non-extraction of the spec's labelled impostor numbers.
#   3. Targeted-flag check where the correspondence is clean.
# ============================================================================

test_that("t-tier5-d1-0-s0: section-marker impostor — d=0.27 with no test stat", {
  input <- "Section 95.6 discusses Cohen's d = 0.27 more carefully."
  res <- check_text(input)
  # 95.6 must NOT appear as stat_value on any row
  expect_false(95.6 %in% res$stat_value)
  # The d=0.27 extraction (if emitted) must be flagged: insufficient_data fires
  # because no test statistic accompanies the effect size.
  if (nrow(res) > 0) {
    for (i in seq_len(nrow(res))) {
      expect_true(is_flagged(res[i, ]))
    }
  }
})

test_that("t-tier5-d1-1-s0: internal_inconsistency — t(194)=27.43 vs p=.544", {
  input <- "The result was significant, t(194) = 27.43, p = 0.544."
  res <- check_text(input)
  expect_equal(nrow(res), 1)
  expect_true(is_flagged(res[1, ]))
  # Targeted flag: the math inconsistency must fire either `decision_error`
  # OR `extraction_suspect`. ESCImate suppresses `decision_error` when the
  # computed effect size is implausible (e.g., t=27.43 with df=194 yields
  # d~3.94, exceeding plausibility bounds), favouring the conservative
  # extraction-suspect interpretation. Either flag is a valid expression of
  # "math does not match", so the arena adapter's disjunction maps it to
  # flagged_suspicious correctly. See TODO.md for whether decision_error
  # should also be allowed to fire alongside extraction_suspect (out of
  # v0.3.6 scope to avoid regression risk).
  expect_true(isTRUE(res$decision_error[1]) || isTRUE(res$extraction_suspect[1]))
})

test_that("t-tier5-d2-0-s0: fabricated_value + internal_inconsistency", {
  input <- "We found t(101) = 37.44, p = 0.081; this contradicts the table where the value is 38.44. The result was significant, t(20) = 4.93, p = 0.597."
  res <- check_text(input)
  # Numbers labelled as impostors must not be extracted as stat_value.
  expect_false(38.44 %in% res$stat_value)
  # Both extracted t-rows must be flagged.
  expect_true(nrow(res) >= 2)
  for (i in seq_len(nrow(res))) {
    expect_true(is_flagged(res[i, ]),
                info = sprintf("row %d not flagged: %s", i, res$raw_text[i]))
  }
})

test_that("t-tier5-d2-1-s0: fabricated_value + statistic_impostor", {
  input <- "We found t(73) = 3.21, p = 0.055; this contradicts the table where the value is 4.21. Section 46.12 discusses Cohen's d = 0.78 more carefully."
  res <- check_text(input)
  expect_false(4.21 %in% res$stat_value)
  expect_false(46.12 %in% res$stat_value)
  for (i in seq_len(nrow(res))) {
    expect_true(is_flagged(res[i, ]),
                info = sprintf("row %d not flagged", i))
  }
})

test_that("t-tier5-d3-0-s0: triple — internal_inconsistency, missing_info, internal_inconsistency", {
  input <- "The result was significant, t(78) = 723.87, p = 0.709. We confirmed the effect, t = 904.91, p = 0.143. The result was significant, t(93) = 17.05, p = 0.602."
  res <- check_text(input)
  expect_true(nrow(res) >= 2)
  for (i in seq_len(nrow(res))) {
    expect_true(is_flagged(res[i, ]),
                info = sprintf("row %d not flagged", i))
  }
  # Targeted flag: at least one row corresponds to the missing-df t (904.91).
  # That row must have insufficient_data == TRUE because no df was supplied.
  missing_df_rows <- res[is.na(res$df1) & res$test_type == "t", ]
  if (nrow(missing_df_rows) > 0) {
    expect_true(any(missing_df_rows$insufficient_data))
  }
})

test_that("t-tier5-d3-1-s0: statistic_impostor + double fabricated_value", {
  input <- "Section 131.10 discusses Cohen's d = 0.73 more carefully. We found t(189) = 601.39, p = 0.313; this contradicts the table where the value is 602.39. We found t(75) = 179.42, p = 0.163; this contradicts the table where the value is 180.42."
  res <- check_text(input)
  expect_false(131.10 %in% res$stat_value)
  expect_false(602.39 %in% res$stat_value)
  expect_false(180.42 %in% res$stat_value)
  for (i in seq_len(nrow(res))) {
    expect_true(is_flagged(res[i, ]),
                info = sprintf("row %d not flagged", i))
  }
})

# ============================================================================
# df_arity_mismatch synthetic inputs (NOT in the arena tier-5 list) — these
# pin the new flag's behaviour on the four malformed-arity shapes.
# ============================================================================

test_that("synthetic F(48): df_arity_mismatch fires, status NOTE", {
  res <- check_text("F(48) = 2.31, p = .04.")
  expect_equal(nrow(res), 1)
  expect_true(res$df_arity_mismatch[1])
  expect_equal(res$status[1], "NOTE")
  expect_true(is.na(res$p_computed[1]))
})

test_that("synthetic t(36, 10): df_arity_mismatch fires, status NOTE", {
  res <- check_text("t(36, 10) = 5.34, p = .003.")
  expect_equal(nrow(res), 1)
  expect_true(res$df_arity_mismatch[1])
  expect_equal(res$status[1], "NOTE")
})

test_that("synthetic chi2(48, 14): df_arity_mismatch fires, status NOTE", {
  res <- check_text("chi2(48, 14) = 12.4, p = .002.")
  expect_equal(nrow(res), 1)
  expect_true(res$df_arity_mismatch[1])
  expect_equal(res$status[1], "NOTE")
})

test_that("synthetic r(50, 30): df_arity_mismatch fires, status NOTE", {
  res <- check_text("r(50, 30) = 0.31, p = .03.")
  expect_equal(nrow(res), 1)
  expect_true(res$df_arity_mismatch[1])
  expect_equal(res$status[1], "NOTE")
})
