# v0.7.6 -- adaptation to docpluck v2.4.130 ... v2.4.136.
#
# Sources: docpluck OUTBOX_2026-08-13b_symbol_contract_v2_ACTION_REQUIRED.md
# (revised 2026-08-14) and OUTBOX_TO_ESCIMATE_2026-08-21_v2.4.136.md.
#
# Every expectation below was REPRODUCED against the unfixed package on
# 2026-08-21 before the fix was written, and each block records what the
# unfixed code actually did. Two of these were shipping wrong verdicts to
# users at the time they were written.
#
# Cross-model review of the plan behind these tests: codex (gpt-5.5) and
# Sonnet 5, 2026-08-21. Both independently confirmed the symbol-contract, SE
# and flattened-rows findings. Disagreements were settled by reproduction, not
# by authority -- see the note above the form-feed block.

# ---------------------------------------------------------------------------
# V1 -- symbol contract v1.0 -> v2.0 (docpluck v2.4.130, live ~2026-08-14)
#
# docpluck now `_`-joins every subscript run, so partial eta-squared arrives as
# `eta2_p` and partial omega-squared as `omega2_p`. Verified against the live
# docpluck 2.4.136 library:
#     "F(1, 98) = 12.34, p = .001, <eta><sup>2</sup><sub>p</sub> = .11"
#         -> "F(1, 98) = 12.34, p = .001, eta2_p = .11"
#     "F(1, 98) = 4.2, p = .04, <omega><sup>2</sup><sub>p</sub> = .09"
#         -> "F(1, 98) = 4.2, p = .04, omega2_p = .09"
#
# MEASURED AT HEAD BEFORE THE FIX:
#     eta2_p   = .11  ->  effect_reported = NA,   status = OK      <- WRONG
#     eta2p    = .11  ->  effect_reported = 0.11, status = ERROR
#     omega2_p = .09  ->  effect_reported = NA,   status = OK      <- WRONG
#     omega2p  = .09  ->  effect_reported = 0.09, status = WARN
#
# The effect size is dropped AND the verdict flips from ERROR/WARN to a clean
# OK -- a green row for a result that was never checked. A green result from an
# empty input is a false green.
#
# TWO sites need the fix, not one: the `=`-anchored rewrite at parse.R:576/580,
# and the direct extractors pat_etap2 (parse.R:2278) / pat_partial_omega2
# (parse.R:2289), which have no bare-token fallback of their own. Sonnet's
# review noted pat_partial_omega2 carries no underscore variant at all, so the
# omega case is strictly worse than the eta case.

test_that("v0.7.6: docpluck symbol contract v2.0 partial-eta-squared parses", {
  r <- check_text("A main effect emerged, F(1, 98) = 12.34, p = .001, eta2_p = .11.")
  expect_equal(nrow(r), 1L)
  expect_equal(r$effect_reported[1], 0.11)
})

test_that("v0.7.6: contract v2.0 `eta2_p` grades identically to v1.0 `eta2p`", {
  # The point of the fix: the two spellings are the same statistic, so they
  # must produce the same verdict. Before the fix the v2.0 spelling scored OK
  # (unchecked) where the v1.0 spelling scored ERROR (checked, inconsistent).
  new_form <- check_text("A main effect emerged, F(1, 98) = 12.34, p = .001, eta2_p = .11.")
  old_form <- check_text("A main effect emerged, F(1, 98) = 12.34, p = .001, eta2p = .11.")
  expect_equal(new_form$effect_reported[1], old_form$effect_reported[1])
  expect_equal(new_form$status[1], old_form$status[1])
})

test_that("v0.7.6: docpluck symbol contract v2.0 partial-omega-squared parses", {
  new_form <- check_text("F(1, 98) = 4.2, p = .04, omega2_p = .09.")
  old_form <- check_text("F(1, 98) = 4.2, p = .04, omega2p = .09.")
  expect_equal(new_form$effect_reported[1], 0.09)
  expect_equal(new_form$status[1], old_form$status[1])
})

test_that("v0.7.6: contract v2.0 tokens that already worked keep working", {
  # Measured at HEAD: these all parsed correctly before the fix. Pinned so the
  # eta2_p / omega2_p change cannot regress a sibling token.
  expect_equal(check_text("F(1, 98) = 4.2, p = .04, eta2 = .12.")$effect_reported[1], 0.12)
  expect_equal(check_text("F(1, 98) = 4.2, p = .04, omega2 = .06.")$effect_reported[1], 0.06)
  expect_equal(check_text("F(1, 98) = 4.2, p = .04, epsilon2 = .05.")$effect_reported[1], 0.05)
  expect_equal(check_text("F(1, 98) = 4.2, p = .04, eta2G = .07.")$effect_reported[1], 0.07)
  expect_equal(check_text("r(98) = .31, p = .002, R2 = .096.")$effect_reported[1], 0.096)
  # Superscript-after-digit is a caret under contract v2.0; the baseline form
  # was what v1.0 delivered. Both must read as 1e-08.
  expect_equal(check_text("t(98) = 2.1, p < 10^-8.")$p_reported[1], 1e-08)
  expect_equal(check_text("t(98) = 2.1, p < 10-8.")$p_reported[1], 1e-08)
})

# ---------------------------------------------------------------------------
# V2 -- restored form feeds (docpluck v2.4.136)
#
# docpluck's `_normalize_text` stripped standalone page numbers with
# `^\s*\d{1,3}\s*$` under MULTILINE; the whitespace class matches the FORM
# FEED, so the rule consumed the page boundary standing beside the number it
# deleted. v2.4.136 captures and restores the form feed, so page breaks now
# appear in text where they never used to (+1..+17 per paper, 46% of documents).
#
# effectcheck had ZERO form-feed awareness -- no "\f", "\x0c" or "[:cntrl:]"
# anywhere in the package.
#
# MEASURED AT HEAD BEFORE THE FIX, through check_text():
#     separator "\n\n"   -> 2 rows: d_reported_only (d = 0.33) + t (7.47)
#     separator "\n\f\n" -> 1 row : t = 7.47 carrying effect_reported = 0.33
#
# That single row is a FABRICATED PAIRING -- both numbers are in the text, but
# the pairing is not. It is the exact defect v0.7.4 shipped to eliminate (the
# spps.txt location-216 incident), reverted by one form feed.
#
# WHY DELETION AND NOT TRANSLATION. Three strategies were measured through
# check_text() before choosing:
#     A: delete "\f"       -> blank-line shape 2 rows; single-newline shape 1 row
#                             (both identical to pre-2.4.136 behaviour)
#     B: "\f" -> "\n"      -> blank-line shape 2 rows; single-newline shape 2 rows
#                             -- a chunk split that never existed before
#     C: "\f" -> "\n\n"    -> manufactures a hard page boundary; not evaluated,
#                             and deliberately not shipped, because severing a
#                             statistic from its own effect size across a page
#                             break is the counterexample class that killed the
#                             v0.7.4 general blank-line rule.
# Codex independently warned that B creates both new chunk boundaries and new
# cross-page bridge opportunities at parse.R:893. Sonnet recommended B on the
# reasoning that it "does not synthesize a new \n\n" -- that reasoning does not
# hold when the form feed is adjacent to an existing newline, which is the
# corpus-majority shape, and the single-newline row above is that refutation.
# Deletion is the only option that restores measured prior behaviour exactly.

.v076_two_results <- function(sep) {
  paste0("The difference was d = 0.33, 95% CI [0.09, 0.57].", sep,
         "0.75, 95% CI = [0.55, 0.95], t = 7.47, p < .001.")
}

test_that("v0.7.6: a form feed at a paragraph break does not fuse two results", {
  ff   <- check_text(.v076_two_results("\n\f\n"))
  base <- check_text(.v076_two_results("\n\n"))
  # The page break must not change how many results the text contains.
  expect_equal(nrow(ff), nrow(base))
  expect_equal(nrow(ff), 2L)
})

test_that("v0.7.6: a form feed cannot attach one result's effect to another's statistic", {
  ff <- check_text(.v076_two_results("\n\f\n"))
  # The t = 7.47 row must NOT carry the OTHER result's d = 0.33.
  t_row <- ff[!is.na(ff$stat_value) & abs(ff$stat_value - 7.47) < 1e-9, ]
  expect_equal(nrow(t_row), 1L)
  expect_true(is.na(t_row$effect_reported[1]) || t_row$effect_reported[1] != 0.33)
})

test_that("v0.7.6: a form feed does not change how a text chunks", {
  # Strategy A must be behaviour-identical to pre-2.4.136 text in BOTH shapes.
  # The single-newline shape is where strategy B diverged.
  one_nl    <- check_text(.v076_two_results("\n"))
  one_nl_ff <- check_text(.v076_two_results("\n\f"))
  expect_equal(nrow(one_nl_ff), nrow(one_nl))
})

test_that("v0.7.6: a form feed does not resurrect a stripped standalone page number", {
  # normalize_text() strips a standalone page number so a bridging rule cannot
  # adopt it as a value (parse.R:887, whose own comment says it MUST run before
  # the joiner at :893 -- the one bridging rule with no decimal-point guard).
  # Its "[ \t]" class cannot match a form feed, so 63 lines in the 38-paper
  # corpus escaped it once docpluck stopped deleting page boundaries.
  out <- effectcheck:::normalize_text(
    "Effect was large, dz =\n\n\f3\n\nAmerican Sociological Review 89(4)")
  expect_false(grepl("dz = 3", out, fixed = TRUE))
})

test_that("v0.7.6: a form feed does not preserve a section number", {
  # parse.R:857/859/880 anchor on "(^|\n)([ \t]*)", so a section number at the
  # top of a page survived. Six such headings already survive in the corpus
  # file rsos250908.txt.
  out <- effectcheck:::normalize_text(
    "Body text ended here.\n\f2.4. Design and procedure\nNext paragraph.")
  expect_false(grepl("2.4. Design and procedure", out, fixed = TRUE))
})

test_that("v0.7.6: a form-feed-only chunk does not survive as a result location", {
  # R's trimws() default whitespace class is "[ \t\r\n]" and does NOT strip a
  # form feed (verified: nchar(trimws("\f")) == 1), so a form-feed-only chunk
  # passed the nchar(trimws(chunk)) > 0 filters at parse.R:1667/1770/1772 and
  # shifted every subsequent `location` ordinal. MetaESCI joins on `location`.
  with_ff <- check_text("First, t(50) = 2.10, p = .04.\n\f\nSecond, t(60) = 3.10, p = .003.")
  no_ff   <- check_text("First, t(50) = 2.10, p = .04.\n\nSecond, t(60) = 3.10, p = .003.")
  expect_equal(nrow(with_ff), nrow(no_ff))
  expect_equal(with_ff$location, no_ff$location)
})

# ---------------------------------------------------------------------------
# V3 -- docpluck rule W0g fabricates a negative standard error
#
# `recover_dropped_minus_via_ci_pairing` (docpluck normalize.py:3921, called
# UNGATED at :5267, so it fires on the HTTP API path ESCImate uses) infers a
# missing minus from arithmetic: "this value must be negative, because
# otherwise it falls outside its confidence interval".
#
# REPRODUCED against docpluck 2.4.136 on ESCImate's own corpus by diffing with
# the rule monkeypatched out:
#   frontiers_music_mood_2024:  SE=0.199 -> SE=-0.199  AND  p=0.069 -> p=-0.069
#   efendic_2022_affect:        [0.22, 0.75] -> [-0.22, 0.75]
#                               [0.04, 0.25] -> [0.04, -0.25]   (reversed CI)
#   direct probe: "b = 0.25, SE = 0.11, 95% CI [-0.38, 0.04]"
#                               -> b = -0.25, SE = -0.11        (the ESTIMATE too)
#
# A standard error is non-negative BY DEFINITION, so this guard is correct
# permanently -- independent of W0g, which docpluck intends to delete. The
# codebase already knows the right test: check.R:4958 gates the b-referenced CI
# recompute on `SE_val_ci > 0`. It was simply never propagated upstream of the
# synthesis that consumes the same value.
#
# MEASURED AT HEAD BEFORE THE FIX, on
#   "In the regression model the predictor was significant, b = 0.45, SE = -0.11, p = .001."
#     stat_value          = -4.0909...  (sign-flipped: parse.R:3584 gates on != 0, not > 0)
#     SE_coeff            = -0.11       (published verbatim)
#     extraction_suspect  = FALSE       (no SE guard exists anywhere in the package)

.v076_neg_se <- paste("In the regression model the predictor was significant,",
                      "b = 0.45, SE = -0.11, p = .001.")

test_that("v0.7.6: a negative standard error is refused, not computed with", {
  r <- check_text(.v076_neg_se)
  expect_equal(nrow(r), 1L)
  # An impossible value must never be published as though it were the paper's.
  expect_true(is.na(r$SE_coeff[1]))
})

test_that("v0.7.6: a negative SE never sign-flips the synthesized t = b/SE", {
  neg <- check_text(.v076_neg_se)
  pos <- check_text(paste("In the regression model the predictor was significant,",
                          "b = 0.45, SE = 0.11, p = .001."))
  # b/SE with a fabricated minus yields t = -4.09 where the paper implies +4.09.
  # verify_t_from_b_SE (compute.R:1145) absolutises BOTH sides, so it is
  # structurally blind to this -- the guard has to sit upstream of it.
  expect_false(isTRUE(!is.na(neg$stat_value[1]) &&
                      !is.na(pos$stat_value[1]) &&
                      neg$stat_value[1] == -pos$stat_value[1]))
})

test_that("v0.7.6: a negative SE marks the row extraction_suspect with a named reason", {
  r <- check_text(.v076_neg_se)
  expect_true(isTRUE(r$extraction_suspect[1]))
  expect_match(paste(r$uncertainty_reasons[1], collapse = " "),
               "standard error", ignore.case = TRUE)
})

test_that("v0.7.6: a positive standard error is untouched by the guard", {
  # The guard must not cost coverage on the normal case.
  r <- check_text(paste("In the regression model the predictor was significant,",
                        "b = 0.45, SE = 0.12, p = .001."))
  expect_equal(r$SE_coeff[1], 0.12)
  expect_false(isTRUE(r$extraction_suspect[1]))
})

# ---------------------------------------------------------------------------
# V4 -- the docpluck flattened-rows path validates nothing
#
# flattened_rows_to_parsed() (parse.R:5008-5014) reads a typed table field and
# then HARDCODES `p_valid = !is.na(p_val)` and `p_out_of_range = FALSE`. That
# is a field the code never populates: check.R has no lower bound on
# p_reported, and check.R:7035 `p_reported < alpha` reads a negative p as
# highly significant, which can manufacture a false decision error.
#
# HONEST SCOPE: this is LATENT, not live. docpluck's own table channel already
# drops an out-of-domain p (tables/flatten.py:1210 gates 0.0 <= p <= 1.0),
# along with out-of-domain r and non-positive n/N. The guard is defence in
# depth -- effectcheck's own claim about its own column must not rest on
# someone else's invariant, and a hardcoded FALSE is a claim the code cannot
# back. The prose path already does exactly this check at parse.R:4195; this
# brings the table path to parity rather than inventing new semantics.

.v076_flat_row <- function(p) {
  list(list(sentence = "Group A vs Group B",
            fields = list(t = 2.41, df = 98, p = p)))
}

test_that("v0.7.6: the flattened-rows path rejects an out-of-range p", {
  r <- check_text("Table 1 reports the contrasts.", table_rows = .v076_flat_row(-0.90))
  expect_equal(nrow(r), 1L)
  expect_true(is.na(r$p_reported[1]))
  expect_false(isTRUE(r$p_valid[1]))
  expect_true(isTRUE(r$p_out_of_range[1]))
})

test_that("v0.7.6: the flattened-rows path keeps a valid p", {
  r <- check_text("Table 1 reports the contrasts.", table_rows = .v076_flat_row(0.018))
  expect_equal(nrow(r), 1L)
  expect_equal(r$p_reported[1], 0.018)
  expect_true(isTRUE(r$p_valid[1]))
  expect_false(isTRUE(r$p_out_of_range[1]))
})

# ---------------------------------------------------------------------------
# V3b -- a REVERSED reported interval
#
# Raised by the Grok 4.6 review of the v0.7.6 plan (2026-08-21) while reviewing
# the SE guard, and the corpus evidence for it was already in hand: docpluck's
# W0g rewrote the printed `[0.22, 0.75]` into `[-0.22, 0.75]` and the printed
# `[0.04, 0.25]` into `[0.04, -0.25]` in efendic_2022_affect.
#
# The second of those is invisible to every other guard in this release. Both
# bounds are individually plausible numbers; only their ORDER is impossible. A
# negative-SE check cannot see it, a p-range check cannot see it, and
# `sign_ci_violation` cannot either -- that fires on an estimate lying outside
# its own interval, which is a different shape.

test_that("v0.7.6: a reversed reported interval is flagged as impossible", {
  r <- check_text("The contrast was reliable, t(98) = 2.41, p = .018, d = 0.15, 95% CI [0.04, -0.25].")
  expect_equal(nrow(r), 1L)
  expect_true(isTRUE(r$extraction_suspect[1]))
  expect_match(paste(r$uncertainty_reasons[1], collapse = " "), "reversed")
})

test_that("v0.7.6: a correctly ordered interval is not flagged", {
  # The guard must cost nothing on the normal case, including an interval that
  # is entirely negative (lower bound more negative than the upper).
  ok  <- check_text("The contrast was reliable, t(98) = 2.41, p = .018, d = 0.15, 95% CI [0.04, 0.25].")
  neg <- check_text("The contrast was reliable, t(98) = -2.41, p = .018, d = -0.15, 95% CI [-0.25, -0.04].")
  expect_false(grepl("reversed", paste(ok$uncertainty_reasons[1], collapse = " ")))
  expect_false(grepl("reversed", paste(neg$uncertainty_reasons[1], collapse = " ")))
})

# ---------------------------------------------------------------------------
# V5 -- document-level upstream provenance
#
# docpluck's HTTP response has always carried its own `normalization` report,
# and the worker has always received it (worker/docpluck_client.R) -- but it
# was never forwarded to check_text(), so a document whose statistics the
# extractor had REWRITTEN was indistinguishable from one it had merely
# canonicalised. Until v0.7.6, `steps_changed` had zero occurrences anywhere in
# effectcheck/R/ or worker/.
#
# Keyed on docpluck's METRIC KEY (`dropped_minus_signs_recovered`), never on a
# rule name: docpluck has said it will delete the rule that dominates that
# count, and a name-keyed branch would then go silently dark.
#
# Deliberately NOT wired to `extraction_suspect`. That flag gates effect-size
# decimal REWRITING (check.R) and two ERROR-path downgrades, so raising it on
# every row of a paper because one span was rewritten would demote unrelated
# genuine inconsistencies. Raised by the Grok 4.6 review and confirmed by
# reading those three call sites.

.v076_prov <- function(n_sign_rewrites, version = "1.9.57") {
  list(version = version,
       steps_changed = list("W0g_dropped_minus_ci_pairing"),
       changes_made = list(dropped_minus_signs_recovered = n_sign_rewrites))
}

.v076_plain <- "The difference was significant, t(48) = 2.34, p = .023, d = 0.67, 95% CI [0.09, 1.25]."

test_that("v0.7.6: upstream sign rewrites are surfaced on every row", {
  r <- check_text(.v076_plain, extraction_provenance = .v076_prov(3L))
  expect_true("upstream_sign_rewrites" %in% names(r))
  expect_equal(r$upstream_sign_rewrites[1], 3L)
  expect_equal(r$upstream_normalization_version[1], "1.9.57")
})

test_that("v0.7.6: upstream provenance is NA, not 0, when none was supplied", {
  # "the extractor reported nothing" and "the extractor reported no rewrites"
  # are different facts, and only the second is an all-clear.
  r <- check_text(.v076_plain)
  expect_true(is.na(r$upstream_sign_rewrites[1]))
  expect_true(is.na(r$upstream_normalization_version[1]))
})

test_that("v0.7.6: a clean upstream report reads as zero rewrites", {
  r <- check_text(.v076_plain, extraction_provenance = .v076_prov(0L))
  expect_equal(r$upstream_sign_rewrites[1], 0L)
})

test_that("v0.7.6: upstream provenance does NOT mark rows suspect or change status", {
  # The whole point of keeping this document-level. A rewritten span elsewhere
  # in the paper must not demote this row's verdict.
  with_prov <- check_text(.v076_plain, extraction_provenance = .v076_prov(3L))
  without   <- check_text(.v076_plain)
  expect_equal(with_prov$status[1], without$status[1])
  expect_equal(isTRUE(with_prov$extraction_suspect[1]),
               isTRUE(without$extraction_suspect[1]))
  expect_equal(with_prov$uncertainty_reasons[1], without$uncertainty_reasons[1])
})

test_that("v0.7.6: an absent changes_made KEY means zero, not unknown", {
  # Measured against a live docpluck response on 2026-08-21: `changes_made`
  # carries a key only for a step that actually FIRED, so a clean document has
  # no `dropped_minus_signs_recovered` key rather than a key set to 0. Reading
  # that absence as "unknown" would make the common case indistinguishable from
  # "we were told nothing" -- and an unknown that is really a zero is the kind
  # of permanent NA nobody ever investigates.
  r <- check_text(.v076_plain, extraction_provenance = list(
    version = "1.9.57", changes_made = list(), steps_changed = list("S6_whitespace")))
  expect_equal(r$upstream_sign_rewrites[1], 0L)
  expect_equal(r$upstream_normalization_version[1], "1.9.57")
})

test_that("v0.7.6: a malformed or partial provenance block never errors", {
  # The worker passes this straight through from an external service, so it
  # must tolerate anything that service does. A provenance reader that can
  # crash the audit is worse than no provenance at all.
  expect_silent(check_text(.v076_plain, extraction_provenance = list()))
  expect_silent(check_text(.v076_plain, extraction_provenance = list(changes_made = list())))
  expect_silent(check_text(.v076_plain, extraction_provenance = "not a list"))
  expect_silent(check_text(.v076_plain, extraction_provenance = list(changes_made = "junk")))
  # No `changes_made` at all is genuinely unknown, and stays NA.
  r <- check_text(.v076_plain, extraction_provenance = list(version = "1.9.57"))
  expect_true(is.na(r$upstream_sign_rewrites[1]))
})

# ---------------------------------------------------------------------------
# Column read-back. Surfaced by the v0.7.6 release review, which found the SE
# guard's BEHAVIOUR tested (value nulled, row suspect, reason surfaced) but its
# two new COLUMNS asserted nowhere.
#
# That is the write-only-column shape: every unit is individually correct, the
# feature does nothing observable, and every test still passes. A column this
# package declares must be one this package populates.

test_that("v0.7.6: the SE guard's columns are populated, not merely declared", {
  bad <- check_text(paste("In the regression model the predictor was significant,",
                          "b = 0.45, SE = -0.11, p = .001."))
  expect_true("SE_guard_rejected" %in% names(bad))
  expect_true("SE_guard_reason" %in% names(bad))
  expect_true(isTRUE(bad$SE_guard_rejected[1]))
  # The reason must QUOTE the refused value -- a reason that does not say what
  # was rejected leaves the reader unable to check the source document.
  expect_match(bad$SE_guard_reason[1], "-0.11", fixed = TRUE)

  good <- check_text(paste("In the regression model the predictor was significant,",
                           "b = 0.45, SE = 0.12, p = .001."))
  expect_false(isTRUE(good$SE_guard_rejected[1]))
  expect_true(is.na(good$SE_guard_reason[1]))
})

test_that("v0.7.6: provenance columns are present on every row, populated or NA", {
  r <- check_text("The difference was significant, t(48) = 2.34, p = .023, d = 0.67.")
  expect_true(all(c("upstream_sign_rewrites", "upstream_normalization_version") %in% names(r)))
  expect_equal(nrow(r), 1L)
})
