# The CI arm computes three tiers and publishes only two.
#
# WHAT THE CODE ALREADY DOES RIGHT (check.R:5040-5137). It enumerates EVERY CI
# candidate -- from every computed variant and every alternative -- de-duplicates
# them, scans all of them for the best match, records the winner in
# `ci_method_match`, and grades three tiers:
#
#     MATCH         delta <= tol_ci
#     PLAUSIBLE     delta <= 3 * tol_ci
#     INCONSISTENT  otherwise            <- matched NOTHING in the universe
#
# That is exactly the "compute the whole universe, then compare" design the
# effect-size arm uses, and INCONSISTENT already carries a strong, earned claim:
# the reported interval matches no method we know how to compute.
#
# WHAT WENT WRONG (check.R:5468, before this change):
#
#     if (!is.na(ci_match) && !ci_match) {
#       if (ci_affects_status && status == "PASS") status <- "NOTE"
#
# `ci_match` is a SINGLE BOOLEAN (delta <= tol_ci), so PLAUSIBLE and INCONSISTENT
# were indistinguishable to the published `status`, and the only movement was a
# PASS -> NOTE downgrade -- never an escalation. Measured on 0.7.8, four rows with
# identical statistics (d = 0.55 against t(58) = 2.10, computed d_ind = 0.5422) so
# that only the reported interval varied:
#
#     reported CI        ci_check_status   status
#     [0.03, 1.07] true  MATCH             PASS
#     [0.10, 1.00]       INCONSISTENT      NOTE
#     [0.54, 0.56]       INCONSISTENT      NOTE
#     [8.00, 9.00]       INCONSISTENT      NOTE
#
# v0.7.14: the blocks below that used [8.00, 9.00] as their "absurd" interval now
# use [0.40, 0.70] -- centred on the estimate, containing it, and far too narrow,
# so it is wrong ON THE EFFECT'S OWN SCALE. [8.00, 9.00] excludes its own
# estimate, and an interval that excludes its estimate cannot be graded as that
# estimate's interval: since 0.7.14 it is UNVERIFIABLE with ci_referent =
# "not_effect_reported", while `estimate_outside_ci` (and status NOTE) carry the
# impossible-value finding. That case is pinned in test-v0714-ci-verdict-premise.R
# and at the end of this file. The rows that still use [8.00, 9.00] assert
# something else (bounded r range, a stated level of 263.95%, cross-family).
#
# A confidence interval wrong by 0.07 and one wrong by 8.0 published the IDENTICAL
# verdict, while the effect-size arm grades PASS / WARN (3x) / ERROR (5x) on the
# same input. Downstream, a consumer maps NOTE -> severity "info" by explicit design,
# so an interval that contradicts every computable method reached the author as a
# neutral chip that turned no badge and counted toward no issue.
#
# WHY IT WAS BUILT THIS WAY, and why the caution turned out to be RIGHT. The CI
# arm exists to say "we could not confirm this", not "this is wrong" --
# check.R:5169 deliberately reports UNVERIFIABLE rather than a hard INCONSISTENT
# when every candidate is an independent-samples approximation of a within-subjects
# design, and the sign-alignment block at 5075 exists because a spurious
# INCONSISTENT was measured in the wild.
#
# ##########################################################################
# ## READ THIS BEFORE CHANGING ANYTHING IN THIS FILE.                     ##
# ##                                                                      ##
# ## The fix described above -- escalate INCONSISTENT to WARN -- WAS      ##
# ## WRITTEN, SHIPPED TO A BRANCH, AND THEN WITHDRAWN ON 2026-09-06,      ##
# ## because it was measured at PRECISION 0 OF 3 on the real corpus: it   ##
# ## fired on three published rows and every one was a CORRECT paper.     ##
# ## Two of the three had been PASS.                                      ##
# ##                                                                      ##
# ## The full measurement, the five independent ways its premise failed,  ##
# ## and the three providers that found them are in the block comment at  ##
# ## the END of this file. The complaint at the top of this file was      ##
# ## real; the fix was not. What replaced it: the three tiers are already ##
# ## published in `ci_check_status`, so nothing was lost by leaving       ##
# ## `status` alone -- and the one genuine catch (an interval outside a   ##
# ## bounded family's mathematical range) moved to `impossible_value`,    ##
# ## where it compares against a bound rather than against anything we    ##
# ## computed and therefore cannot fire on a correct paper.               ##
# ##                                                                      ##
# ## Do not re-wire any tier to `status` without a NEW corpus measurement ##
# ## showing precision better than 0 of 3.                                ##
# ##########################################################################
#
# The tests below were watched FAIL first on 2026-09-05 against unmodified 0.7.8
# and then REWRITTEN on 2026-09-06 when the measurement came in. Both facts are
# kept deliberately: a test watched red is evidence the fixture reaches the case,
# and it says nothing about whether the behaviour it pins is the right one.

# Identical statistics in every case: t(58) = 2.10 gives d_ind = 2*2.10/sqrt(58)
# = 0.5515, so a reported d = 0.55 matches and the effect-size arm cannot be the
# source of any status movement. Only the interval changes.
ci_row <- function(ci_text, ...) {
  txt <- paste0(
    "Groups were compared with an independent-samples t test. ",
    "The effect was reliable, t(58) = 2.10, p = .040, d = 0.55, ", ci_text, "."
  )
  as.data.frame(check_text(txt, ...))
}

test_that("an interval matching NO computed candidate does NOT escalate status", {
  # WITHDRAWN 2026-09-06. This test asserted WARN. The escalation it pinned was
  # measured at precision 0 of 3 on the corpus -- see the block comment at the
  # end of this file -- and now leaves `status` alone. The TIER is still
  # computed and still published; it is `ci_check_status` that carries it.
  df <- ci_row("95% CI [0.40, 0.70]")
  expect_equal(nrow(df), 1L)
  expect_equal(df$ci_check_status[1], "INCONSISTENT")
  expect_equal(df$status[1], "NOTE")
})

test_that("a correct interval is untouched -- the control", {
  # Without this, a rule that returned WARN unconditionally would pass the test
  # above while destroying every clean row in the corpus.
  df <- ci_row("95% CI [0.03, 1.07]")
  expect_equal(nrow(df), 1L)
  expect_equal(df$ci_check_status[1], "MATCH")
  expect_equal(df$status[1], "PASS")
})

test_that("PLAUSIBLE stays NOTE -- escalation is reserved for matching nothing", {
  # tol_ci default is 0.02; PLAUSIBLE is delta <= 3*tol_ci = 0.06. Widen tol_ci so
  # a near-miss lands in the PLAUSIBLE band rather than past it, and assert the
  # middle tier does NOT escalate. This is the assertion that keeps the change
  # conservative: only the exhausted-universe case moves.
  df <- ci_row("95% CI [0.10, 1.00]", tol_ci = 0.05)
  expect_equal(nrow(df), 1L)
  expect_equal(df$ci_check_status[1], "PLAUSIBLE")
  expect_equal(df$status[1], "NOTE")
})

test_that("the three tiers are genuinely distinguishable IN THE PUBLISHED ROW", {
  # REWRITTEN 2026-09-06, and the rewrite is the whole lesson of this file.
  #
  # The original complaint was real: a CI wrong by 0.07 and one wrong by 8.0
  # published the same NOTE. The original FIX -- escalate the second to WARN --
  # was measured at 0 of 3 precision on the corpus and is withdrawn.
  #
  # But the complaint was never that the tiers are indistinguishable in the ROW.
  # They never were. `ci_check_status` has carried MATCH / PLAUSIBLE /
  # INCONSISTENT all along and is a documented output column. The actual defect
  # was downstream: a consumer mapping `status` and ignoring that column.
  #
  # So this asserts what is both TRUE and SUFFICIENT -- three distinct tiers
  # reach the consumer on every row -- without moving a field three projects
  # parse, and without any way to accuse a correct paper.
  exact  <- ci_row("95% CI [0.03, 1.07]", tol_ci = 0.05)
  near   <- ci_row("95% CI [0.10, 1.00]", tol_ci = 0.05)
  absurd <- ci_row("95% CI [0.40, 0.70]", tol_ci = 0.05)

  tiers <- c(exact$ci_check_status[1], near$ci_check_status[1], absurd$ci_check_status[1])
  expect_equal(tiers, c("MATCH", "PLAUSIBLE", "INCONSISTENT"))
  expect_equal(length(unique(tiers)), 3L)

  # And the closest method is named on every one, so a consumer can see WHAT the
  # interval was compared against -- which is exactly what would have revealed
  # the corpus false alarms (a Pearson r graded against a Spearman interval).
  expect_true(all(!is.na(c(exact$ci_method_match[1], near$ci_method_match[1],
                           absurd$ci_method_match[1]))))

  # `status` deliberately does NOT separate the last two. Pinned so that a
  # future session re-proposing the escalation has to confront the measurement
  # rather than rediscover the complaint.
  statuses <- c(exact$status[1], near$status[1], absurd$status[1])
  expect_equal(statuses, c("PASS", "NOTE", "NOTE"))
})

test_that("ci_affects_status = FALSE still suppresses the CI's effect entirely", {
  # The escalation must honour the existing opt-out, or a consumer that has
  # deliberately turned the CI arm off would start seeing new WARNs.
  df <- ci_row("95% CI [0.40, 0.70]", ci_affects_status = FALSE)
  expect_equal(df$ci_check_status[1], "INCONSISTENT")
  expect_equal(df$status[1], "PASS")
})

test_that("an INCONSISTENT CI never DOWNGRADES an existing ERROR", {
  # The escalation is one-directional. A row already failing its effect-size
  # check must not be softened to WARN because the CI arm also fired -- that
  # would mute a stronger finding with a weaker one.
  #
  # Stated as the invariant that actually matters -- adding an INCONSISTENT CI
  # must NEVER LOWER a row's severity -- rather than as an expectation about one
  # hand-picked status. Two earlier drafts of this test asserted a specific
  # status and were both wrong about the input: d = 3.90 trips the
  # extraction-suspect guard (delta 1.78 > EXTREME_DELTA_THRESHOLD) so it is
  # NOTE, not ERROR; and d = 1.10 is downgraded ERROR -> WARN by the
  # design-ambiguity rule because a bare t-test cannot establish paired vs
  # independent. Pinning the ORDERING is both more honest and more general: it
  # holds whatever those neighbouring rules do next.
  rank_of <- function(s) match(s, c("SKIP", "OK", "PASS", "NOTE", "WARN", "ERROR"))

  base_txt <- paste(
    "Groups were compared with an independent-samples t test.",
    "The effect was reliable, t(58) = 2.10, p = .040, d = 1.10."
  )
  txt <- paste(
    "Groups were compared with an independent-samples t test.",
    "The effect was reliable, t(58) = 2.10, p = .040, d = 1.10, 95% CI [1.00, 1.20]."
  )
  base <- as.data.frame(check_text(base_txt))
  df   <- as.data.frame(check_text(txt))

  expect_equal(nrow(base), 1L)
  expect_equal(nrow(df), 1L)
  expect_equal(df$ci_check_status[1], "INCONSISTENT")
  # The row must already be flagged on its effect size, or this proves nothing
  # about softening -- a PASS row has nothing to soften.
  expect_true(base$status[1] %in% c("WARN", "ERROR"))
  expect_gte(rank_of(df$status[1]), rank_of(base$status[1]))
})

test_that("an ASSUMED confidence level is not escalated -- the universe was not exhausted", {
  # Raised by a downstream consumer, 2026-09-05, with a measurement: across their
  # stored results, 22 of 53 INCONSISTENT rows (42%) carry an assumed level
  # against 14 of 76 MATCH rows (18%). Assumed-level rows are more than twice as
  # enriched among mismatches, which is what a level assumption MANUFACTURING
  # mismatches looks like.
  #
  # It is also a hole in this file's own rationale. The escalation is justified
  # by "the candidate universe has been exhausted, so the conservative reading
  # is spent". When the paper does not state its confidence level,
  # `ci_level_used` falls back to the 0.95 default (check.R:1013-1017) and every
  # candidate is computed at THAT ONE LEVEL. A paper reporting a 90% or 99%
  # interval then mismatches every candidate -- and the paper is not wrong, our
  # assumption is. The universe was explored along the method axis only, never
  # along the level axis, so it was never exhausted and the escalation's premise
  # does not hold.
  #
  # An INCONSISTENT verdict on an assumed level is therefore partly a statement
  # about OUR inference, not purely about the paper. It stays NOTE.
  df <- ci_row("CI [0.40, 0.70]")   # no stated level -> ci_level_source assumed_95
  expect_equal(nrow(df), 1L)
  expect_equal(as.character(df$ci_level_source[1]), "assumed_95")
  expect_equal(df$ci_check_status[1], "INCONSISTENT")
  expect_equal(df$status[1], "NOTE")
})

test_that("a STATED level reaches the same status -- level no longer changes status", {
  # This was the escalation's control: a stated level was supposed to escalate
  # where an assumed one did not. With the escalation withdrawn both land on
  # NOTE. The DISTINCTION still exists and is still published -- `ci_level_source`
  # tells a consumer whether the level was the paper's or ours -- it simply no
  # longer moves `status`, which is a contract read by three projects.
  df <- ci_row("95% CI [0.40, 0.70]")
  expect_equal(nrow(df), 1L)
  expect_equal(as.character(df$ci_level_source[1]), "explicit_with_bounds")
  expect_equal(df$ci_check_status[1], "INCONSISTENT")
  expect_equal(df$status[1], "NOTE")
})

test_that("a CORRELATION with an impossible interval does not publish PASS", {
  # Grok 4.6, 2026-09-05, reproduced here before fixing. The CI escalation is
  # computed in Phase 7, but the r-as-stat upgrade adopts r as the effect and
  # REASSIGNS status to PASS afterwards (check.R ~5640-5652), silently
  # overwriting it. Exactly the failure the v0.7.3 `impossible_value` block
  # documents ("status is assigned and reassigned after that point, so the
  # escalation has to be applied here").
  #
  # Measured at the time: r(198) = .34 reporting 95% CI [8.00, 9.00] published
  # PASS with ci_check_status = INCONSISTENT -- indistinguishable from the same
  # row with a correct interval. A correlation is bounded [-1, 1], so [8, 9]
  # cannot be its confidence interval under any method; this was a muted verdict
  # worse than the one this file was opened to fix.
  df <- as.data.frame(check_text(
    "The correlation was significant, r(198) = .34, p < .001, 95% CI [8.00, 9.00]."))
  expect_equal(nrow(df), 1L)
  expect_equal(df$ci_check_status[1], "UNVERIFIABLE")
  expect_equal(df$ci_unverifiable_reason[1], "referent_not_effect")  # v0.7.14, see header
  expect_false(identical(df$status[1], "PASS"))
  expect_equal(df$status[1], "WARN")
})

test_that("a correlation with a CORRECT interval still passes -- the control", {
  df <- as.data.frame(check_text(
    "The correlation was significant, r(198) = .34, p < .001, 95% CI [0.21, 0.46]."))
  expect_equal(nrow(df), 1L)
  expect_equal(df$ci_check_status[1], "MATCH")
  expect_equal(df$status[1], "PASS")
})

test_that("an IMPLAUSIBLE stated level is treated like an assumed one", {
  # Grok 4.6, 2026-09-05, reproduced. parse.R rewrites an out-of-range level
  # (`263.95% CI`) to 0.95 and records ci_level_source = "implausible_level".
  # That is OUR substitution, exactly like assumed_95 -- the candidate universe
  # is again explored at one level we chose, so the escalation's premise fails
  # and the row must not carry a WARN. The original exclusion list named only
  # assumed_95 and inferred_from_context and let this through.
  df <- as.data.frame(check_text(paste(
    "An independent-samples t test gave t(58) = 2.10, p = .040, d = 0.55,",
    "263.95% CI [8.00, 9.00].")))
  expect_equal(nrow(df), 1L)
  expect_equal(as.character(df$ci_level_source[1]), "implausible_level")
  expect_equal(df$ci_check_status[1], "UNVERIFIABLE")
  expect_equal(df$ci_unverifiable_reason[1], "referent_not_effect")  # v0.7.14, see header
  expect_equal(df$status[1], "NOTE")
})

test_that("a row whose EXTRACTION is suspect is not escalated by the CI arm", {
  # A deliberate non-escalation, and the reason matters: when delta exceeds
  # EXTREME_DELTA_THRESHOLD the extraction itself is in doubt, and the existing
  # guard suppresses ERROR to NOTE because we cannot trust the numbers being
  # compared. The reported CI is drawn from that same untrusted extraction, so
  # raising an alarm on it would assert confidence the row does not have.
  # d = 3.90 vs computed 0.5515 -> delta 1.78, suspect.
  txt <- paste(
    "Groups were compared with an independent-samples t test.",
    "The effect was reliable, t(58) = 2.10, p = .040, d = 3.90, 95% CI [8.00, 9.00]."
  )
  df <- as.data.frame(check_text(txt))
  expect_equal(nrow(df), 1L)
  expect_true(isTRUE(df$extraction_suspect[1]))
  expect_equal(df$ci_check_status[1], "UNVERIFIABLE")
  expect_equal(df$ci_unverifiable_reason[1], "referent_not_effect")  # v0.7.14, see header
  expect_equal(df$status[1], "NOTE")
})

# ---------------------------------------------------------------------------
# A CROSS-FAMILY row's candidate universe is on the WRONG SCALE.
#
# Raised by Grok 4.6 (2026-09-05) as "a cross_type_action NOTE may be promoted by
# the CI escalation", logged as NOT REPRODUCED, and REPRODUCED here on 2026-09-06
# with a two-sided control against the pre-change tree (444fe39):
#
#   fixture                                   before   after
#   F(2,57) omnibus + d + 95% CI [8.00, 9.00]  NOTE  -> WARN
#   F(2,57) omnibus + d + 95% CI [0.02, 1.08]  NOTE  -> WARN     <- FALSE ALARM
#
# The second interval is right to within ordinary method variation:
# ci_d_ind(0.55, 30, 30, .95) = [0.0319, 1.0635], so [0.02, 1.08] is off by 0.012
# and 0.017 -- a slightly different n split or SMD engine, not an error. (Precision
# owed to an independent reproduction, which reproduced this independently and pointed out that
# "exactly right" overstates it. The scale error is the defect and it holds however
# the last decimal resolves, which is why the assertions below key on
# `ci_method_match` naming a different-estimand method rather than on the bounds.)
# So a defensible paper now draws a WARN, because `ambiguity_level` is
# "highly_ambiguous" -- the reported d has NO same-family computed variant on an
# F(2, 57) omnibus (check.R:4810), so `computed_variants` holds eta / Cohen's f,
# and `collect_ci_candidates()` builds the candidate universe FROM those variants
# (check.R:4897-4922). Measured: `ci_method_match = "cohens_f:primary"` and
# `matched_variant = "eta"` against a reported d.
#
# So the escalation's premise -- "the candidate universe was exhausted" -- is
# false in exactly the way the assumed-level exclusion is false: the universe was
# exhausted on a scale the paper never used. This is the SAME structural
# exemption, not a new kind of caution.
crossfam_row <- function(ci_text) {
  txt <- paste0(
    "The omnibus effect was significant, F(2, 57) = 4.20, p = .020, d = 0.55, ",
    ci_text, "."
  )
  as.data.frame(check_text(txt))
}

test_that("a CROSS-FAMILY row is not escalated -- its candidates are the wrong scale", {
  r <- crossfam_row("95% CI [0.02, 1.08]")
  expect_equal(nrow(r), 1L)
  # The premise of the fixture, asserted rather than assumed.
  expect_equal(as.character(r$ambiguity_level[1]), "highly_ambiguous")
  expect_equal(as.character(r$ci_level_source[1]), "explicit_with_bounds")
  expect_true(grepl("^cohens_f", as.character(r$ci_method_match[1])))
  expect_equal(r$ci_check_status[1], "INCONSISTENT")
  # ... and the reported interval is the CORRECT one for the reported d.
  b <- as.numeric(ci_d_ind(0.55, 30, 30, 0.95)$bounds)
  expect_true(all(abs(b - c(0.02, 1.08)) < 0.05))
  # Therefore no alarm.
  expect_false(identical(as.character(r$status[1]), "WARN"))
  expect_equal(as.character(r$status[1]), "NOTE")
})

test_that("a cross-family row with an ABSURD interval also stays NOTE", {
  # Same exemption, opposite input: we still cannot say the interval is wrong,
  # because we never computed one on its scale. Conservative, and honest.
  r <- crossfam_row("95% CI [8.00, 9.00]")
  expect_equal(nrow(r), 1L)
  expect_equal(as.character(r$ambiguity_level[1]), "highly_ambiguous")
  expect_equal(r$ci_check_status[1], "UNVERIFIABLE")
  expect_equal(r$ci_unverifiable_reason[1], "referent_not_effect")  # v0.7.14, see header
  expect_equal(as.character(r$status[1]), "NOTE")
})

test_that("the SAME-family row reaches the same status -- the exemption was subsumed", {
  # This was the control proving my cross-family exemption was not a blanket:
  # a same-scale row DID still escalate. With the escalation withdrawn entirely
  # the distinction no longer reaches `status`, so the exemption is moot for
  # status purposes -- it survives only as `.ci_scale_comparable` feeding the
  # honest wording of the uncertainty message.
  #
  # Keeping the fixture is the point. The cross-family measurement above is why
  # anyone knew to look, and it is the first of the five premise failures that
  # together justified the withdrawal.
  r <- ci_row("95% CI [8.00, 9.00]")
  expect_equal(as.character(r$ambiguity_level[1]), "clear")
  expect_equal(r$ci_check_status[1], "UNVERIFIABLE")
  expect_equal(r$ci_unverifiable_reason[1], "referent_not_effect")  # v0.7.14, see header
  expect_equal(as.character(r$status[1]), "NOTE")
})

# ---------------------------------------------------------------------------
# `inferred_from_context` STAYS EXCLUDED -- and this is a MEASUREMENT, not a
# preference. Grok 4.6 (2026-09-05) argued the opposite: the level token came
# from the paper's own sentence, so the assumed-95 rationale does not apply and
# excluding it mutes a genuinely stated mismatch. That argument is reasonable
# and the corpus refutes it.
#
# Measured 2026-09-06 over the 49 corpus texts in article-finder custody
# (41 papers produced rows, 923 rows total), `ci_level_source` x `ci_check_status`:
#
#                          INCONSISTENT  MATCH  PLAUSIBLE  UNVERIFIABLE
#   explicit_with_bounds             40     50         12            41
#   assumed_95                        2      3          3             6
#   inferred_from_context            15      0          0             0
#
# **15 of 15 -- 100% INCONSISTENT, against 28% for `explicit_with_bounds`.** A
# source class that NEVER matches is not detecting mismatches, it is manufacturing
# them; a legitimate binding would match sometimes. Escalating it would have turned
# all 15 into WARN.
#
# The raw text says why. Eleven of the fifteen are one paper
# (10.1038/s41562-024-01961-1) in this shape:
#
#   "t(181) = 2.571, P = 0.011, 95%CI difference [0.102, 0.776]"
#
# The word "difference" sits between the level and the bracket, so the
# level-with-bounds patterns miss and the bounds parse bare -- and the interval is
# for the RAW MEAN DIFFERENCE, which is then graded against standardized d
# intervals. Same scale error as the cross-family case above, reached by a
# different route, and NOT caught by `ambiguity_level` (these rows are "clear").
# A twelfth is a table CAPTION whose "95% CI" is a column header, not an interval.
#
# So exclusion 2 is load-bearing on real papers today. Revisit only with a new
# corpus measurement, never on the argument alone.

test_that("inferred_from_context is NOT escalated -- 15/15 corpus rows say it never matches", {
  # Synthetic, and deliberately so: it carries a non-NA matched_value, so the
  # extraction-only exclusion is NOT what protects it. This fixture isolates
  # exclusion 2. (The real corpus rows are additionally protected by exclusion 1.)
  txt <- paste("Participants differed, t(58) = 2.10, p = .040, d = 0.55 [8.00, 9.00],",
               "reported as a 95% confidence interval.")
  r <- as.data.frame(check_text(txt))
  expect_equal(nrow(r), 1L)
  expect_equal(as.character(r$ci_level_source[1]), "inferred_from_context")
  expect_equal(r$ci_check_status[1], "UNVERIFIABLE")
  expect_equal(r$ci_unverifiable_reason[1], "referent_not_effect")  # v0.7.14, see header
  expect_false(is.na(r$matched_value[1]))   # exclusion 1 is NOT doing the work here
  expect_equal(as.character(r$status[1]), "NOTE")
})

test_that("the real corpus shape -- a raw-difference interval -- is not escalated", {
  # Verbatim from 10.1038/s41562-024-01961-1, the paper contributing 11 of the 15.
  txt <- paste("t(181) = 2.571, P = 0.011, 95%CI difference [0.102, 0.776]",
               "Technical expertise: statistics (self-reported) 2.808 (0.738);",
               "md = 3 (n = 99) 1.686 (0.997); md = 1 (n = 86)")
  r <- as.data.frame(check_text(txt))
  expect_equal(nrow(r), 1L)
  expect_equal(as.character(r$ci_level_source[1]), "inferred_from_context")
  expect_equal(r$ci_check_status[1], "UNVERIFIABLE")
  expect_equal(r$ci_unverifiable_reason[1], "no_estimate_parsed")  # v0.7.14, see header
  expect_true(as.character(r$status[1]) %in% c("OK", "NOTE"))
  expect_false(identical(as.character(r$status[1]), "WARN"))
})

# ===========================================================================
# THE ESCALATION IS WITHDRAWN. This block is the evidence, kept so that the
# next session to notice the original complaint confronts the measurement
# rather than rediscovering the complaint and re-shipping the fix.
#
# WHAT WAS MEASURED, 2026-09-06. Whole-corpus diff, 0.7.8 (444fe39) against the
# escalating build, over the 49 corpus texts in article-finder custody; 704 of
# 923 rows join uniquely between the arms (219 duplicate keys dropped
# identically from BOTH, so the join is symmetric).
#
#   the escalation fired on 3 rows.  ALL THREE WERE FALSE ALARMS.
#   precision 0 of 3. No true findings.
#
#   10.1080/02699931.2024.2434156  NOTE -> WARN  graded against dz:noncentral_t
#       -- a PAIRED interval, for a between-groups Welch d
#   10.3389/fpsyg.2024.1303262     PASS -> WARN  graded against
#       r:spearman_bonett_wright -- a SPEARMAN interval, for a paper whose own
#       sentence says "A Pearson's correlation was computed"
#   10.3389/fpsyg.2024.1303262     PASS -> WARN  same
#
# Each interval was recomputed INDEPENDENTLY of this package from the paper's
# own reported numbers, recovering n by inverting the t test where unstated:
#
#   paper reports     recomputed
#   [-0.00, 0.34]     [-0.0018, 0.3418]
#   [-0.060, 0.539]   [-0.0597, 0.5390]   (n = 38 from r = .265, p = .108)
#   [-0.033, 0.545]   [-0.0324, 0.5456]   (n = 40 from r = .282, p = .078)
#
# Exact to three decimals. Every accused author was right. Two were PASS.
#
# FIVE INDEPENDENT WAYS THE PREMISE FAILED, found by THREE providers:
#   cross-family scale        (Grok 4.6; reproduced here, fixture above)
#   an assumed level          (a downstream consumer)
#   an assumed equal-N split  (Sonnet 5 AND Sol, independently of each other)
#   a one-sided interval      (Sol; it was PASS at 0.7.8 and became WARN)
#   a CI whose referent is not the reported effect  (Sol)
# Sol's verdict was REQUEST CHANGES. Enumerating exemptions was losing: the ways
# "we computed a comparable interval" can be false are open-ended, which is the
# signature of an inverted default rather than of missing special cases.
#
# A check that fires on correct input is worse than no check. An author who
# follows a flag and finds nothing behind it learns to ignore the next one.

test_that("NONE of the five measured premise failures escalates any more", {
  # Each fixture is a CORRECT paper that the escalation accused. All must sit at
  # or below NOTE. Sources named so a future reader can retrace them.
  cases <- list(
    # Sonnet Q1 + Sol #2, independently. True groups n1=20, n2=80; the reported
    # interval is exactly right for that split (SE = 0.2501 -> [-0.4151, 0.5651]).
    unequal_split = "An independent-samples test gave t(98) = 0.30, p = .765, d = 0.075, 95% CI [-0.415, 0.565].",
    # Sol #1. A correct one-sided interval. This one was PASS at 0.7.8.
    one_sided     = "A one-sided correlation test gave r(198) = .34, p < .001, one-sided 95% CI [0.23, 1.00].",
    # Sol #3. The interval belongs to the unstandardized contrast, not to d.
    wrong_referent = "Groups differed by 20.00 scale points, t(58) = 4.00, p < .001, d = 1.03; the unstandardized contrast had a 95% CI [9.99, 30.01]."
  )
  for (nm in names(cases)) {
    r <- as.data.frame(check_text(cases[[nm]]))
    expect_equal(nrow(r), 1L, info = nm)
    expect_false(identical(as.character(r$status[1]), "WARN"), info = nm)
    expect_false(identical(as.character(r$status[1]), "ERROR"), info = nm)
  }
})

test_that("the controls still discriminate -- these fixtures are not always-NOTE", {
  # Without this, a rule that returned NOTE unconditionally would satisfy the
  # test above while destroying every clean row in the corpus.
  ok_split <- as.data.frame(check_text(
    "An independent-samples test gave t(98) = 0.30, p = .765, d = 0.075, 95% CI [-0.332, 0.452]."))
  ok_r <- as.data.frame(check_text(
    "A correlation test gave r(198) = .34, p < .001, 95% CI [0.21, 0.46]."))
  expect_equal(ok_split$ci_check_status[1], "MATCH")
  expect_equal(as.character(ok_split$status[1]), "PASS")
  expect_equal(ok_r$ci_check_status[1], "MATCH")
  expect_equal(as.character(ok_r$status[1]), "PASS")
})

# ---------------------------------------------------------------------------
# THE ONE GENUINE CATCH IS SALVAGED, in a form that cannot fire on a correct
# paper. A correlation is bounded [-1, 1], so an interval outside that range is
# impossible under EVERY design, allocation, tail and estimand. This compares
# the paper's interval against a mathematical bound rather than against anything
# we computed, so it is immune to all five premise failures above. It lives in
# the `impossible_value` family beside the reversed-interval check, and reuses
# the same `.bounded` table as the computed-value check so the two cannot drift.

impossible_ci <- function(df) {
  grepl("IMPOSSIBLE VALUE: the reported interval",
        as.character(df$uncertainty_reasons[1]), fixed = TRUE)
}

test_that("a correlation interval outside [-1, 1] is flagged as impossible", {
  # Watched FAIL first against 0.7.8: status PASS, flag FALSE. This is the case
  # the withdrawn escalation was originally written to catch, and it is the only
  # one of its motivating cases that survives -- because it is the only one that
  # does not require us to have computed a comparable interval.
  df <- as.data.frame(check_text(
    "The correlation was significant, r(198) = .34, p < .001, 95% CI [8.00, 9.00]."))
  expect_equal(nrow(df), 1L)
  expect_true(impossible_ci(df))
  # NOT `df$impossible_value` -- that flag is INTERNAL and reaches no output
  # column (0 occurrences as a tibble field, 0 mentions in API.md, checked
  # 2026-09-06). A consumer sees only `extraction_suspect` and the message text,
  # which is the same computed-then-discarded shape this file's own history is
  # about. Logged in TODO.md rather than fixed here: adding a column is a
  # contract change across three consumers and is not mine to make.
  expect_true(isTRUE(df$extraction_suspect[1]))
  expect_equal(as.character(df$status[1]), "WARN")
})

test_that("a WRONG BUT POSSIBLE correlation interval is NOT flagged", {
  # THE control that separates this check from the withdrawn one. [0.90, 0.99]
  # is nowhere near the Fisher interval for r = .34 and would have been
  # INCONSISTENT -> WARN under the escalation. It is not impossible, so we say
  # nothing -- because "it does not match what we computed" is exactly the
  # inference that measured 0 of 3.
  df <- as.data.frame(check_text(
    "The correlation was significant, r(198) = .34, p < .001, 95% CI [0.90, 0.99]."))
  expect_equal(nrow(df), 1L)
  # v0.7.14: the interval excludes its own estimate, so it is not graded as the
  # estimate's interval (see header); `estimate_outside_ci` carries the finding.
  expect_equal(df$ci_check_status[1], "UNVERIFIABLE")
  expect_equal(df$ci_unverifiable_reason[1], "referent_not_effect")
  expect_false(impossible_ci(df))
  expect_equal(as.character(df$status[1]), "PASS")
})

test_that("a bounded non-negative family is checked at its own upper bound", {
  bad <- as.data.frame(check_text(
    "The effect was large, F(1, 50) = 4.00, p = .051, eta2 = 0.07, 95% CI [0.00, 1.50]."))
  good <- as.data.frame(check_text(
    "The effect was large, F(1, 50) = 4.00, p = .051, eta2 = 0.07, 95% CI [0.00, 0.25]."))
  expect_true(impossible_ci(bad))
  expect_false(impossible_ci(good))
  expect_equal(as.character(good$status[1]), "PASS")
})

test_that("an UNBOUNDED family is never touched by the bounded-CI check", {
  # d has no mathematical ceiling; d = 2.10 with [1.45, 2.75] is perfectly
  # legitimate and must not trip a bound that does not apply to it.
  df <- as.data.frame(check_text(
    "Groups differed, t(58) = 8.00, p < .001, d = 2.10, 95% CI [1.45, 2.75]."))
  expect_equal(nrow(df), 1L)
  expect_false(impossible_ci(df))
})

test_that("v0.7.14: an interval excluding its own estimate is UNVERIFIABLE, never PASS", {
  # The old "absurd" fixture, pinned under the 0.7.14 semantics (see header).
  df <- ci_row("95% CI [8.00, 9.00]")
  expect_equal(df$ci_check_status[1], "UNVERIFIABLE")
  expect_equal(df$ci_referent[1], "not_effect_reported")
  expect_true(isTRUE(df$estimate_outside_ci[1]))
  expect_equal(df$status[1], "NOTE")
  # and the opt-out still suppresses the CI arm's effect on status entirely
  off <- ci_row("95% CI [8.00, 9.00]", ci_affects_status = FALSE)
  expect_equal(off$status[1], "PASS")
})
