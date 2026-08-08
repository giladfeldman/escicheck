# v0.6.21 -- a p-value produced by a RESAMPLING procedure (permutation /
# randomization / bootstrap / Monte Carlo) is not recomputable from the test
# statistic, because it comes from a different reference distribution.
#
# Raised by a methodologist reviewing ESCImate (2026-08-07), who asked whether a
# "perm Welch t" could be approximately checked. Investigating it surfaced TWO
# defects, both reproduced at v0.6.20 HEAD before any fix was written:
#
# D1 (false decision error): "A permutation Welch t-test with 10,000
#     permutations showed no significant difference, t(58) = 2.31, p = .062"
#     -> status=WARN, decision_error=TRUE, reason=reported_ns_computed_sig.
#     The paper is CORRECT; the parametric p for t(58)=2.31 is .0245, but the
#     reported .062 came from the permutation distribution. Note the existing
#     method_context_in_chunk cap does NOT rescue this: "permutation" is absent
#     from method_kw, and that cap only fires on status == "ERROR".
#
# D2 (wrong published CI): ci_OR_all() back-derives an interval from the
#     reported p via Wald inversion, SE = |log(OR)| / qnorm(1 - p/2)
#     (compute.R wald_log_from_p) -- an inversion that assumes a NORMAL
#     reference distribution. Fed a permutation p, a McNemar row reporting
#     "OR = 2.50, 95% CI [1.05, 5.95], p = .062" produced a computed CI of
#     [0.9551, 6.5441] -- which CROSSES 1 where the paper's does not -- and
#     declared the paper's own correctly-reported interval INCONSISTENT.
#
# The fix is deliberately scoped to the P-VALUE, not the row: a permutation
# changes only the reference distribution, so the t statistic itself -- and
# therefore d = 2t/sqrt(df) -- is computed identically and MUST still be
# checked. Blanket-capping the row at NOTE would discard a real, correct check.

test_that("v0.6.21 D1: a permutation p does not raise a false decision error", {
  txt <- paste0(
    "A permutation Welch t-test with 10,000 permutations showed no ",
    "significant difference, t(58) = 2.31, p = .062, d = 0.61."
  )
  res <- check_text(txt)
  expect_gte(nrow(res), 1L)
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)

  # The row must be RECOGNISED as resampling-based.
  expect_true(isTRUE(row$resampling_inference[1]))
  expect_match(row$resampling_method[1], "permut", ignore.case = TRUE)

  # The defect: reported .062 (ns) vs parametric .0245 (sig) fired a decision
  # error against a correctly-reported result.
  expect_false(isTRUE(row$decision_error[1]))
  expect_true(is.na(row$decision_error_reason[1]))

  # The reason must be stated honestly, and must NOT be the method-context
  # message (which claims the statistic is from a power analysis / meta-analysis).
  expect_match(row$uncertainty_reasons[1], "resampling|permutation", ignore.case = TRUE)
  expect_false(grepl("power analysis", row$uncertainty_reasons[1], fixed = TRUE))
})

test_that("v0.6.21 D1b: the effect-size check SURVIVES the p-value exemption", {
  # d = 2t/sqrt(df) does not depend on how the p-value was obtained, so
  # suppressing the p check must not suppress the effect check.
  txt <- paste0(
    "A permutation Welch t-test with 10,000 permutations, ",
    "t(58) = 2.31, p = .062, d = 0.61."
  )
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)

  # The reported d must still be matched against a computed variant.
  expect_false(is.na(row$matched_value[1]))
  expect_false(is.na(row$delta_effect[1]))
  expect_equal(row$effect_reported[1], 0.61)
  # Still an effect-size check, not downgraded to extraction-only.
  expect_equal(row$check_type[1], "effect_size")
})

test_that("v0.6.21 D2: a permutation p does not back-derive an OR interval", {
  txt <- paste0(
    "A permutation test with 10,000 permutations of the paired data, ",
    "McNemar chi2(1) = 5.20, OR = 2.50, 95% CI [1.05, 5.95], p = .062."
  )
  res <- check_text(txt)
  expect_gte(nrow(res), 1L)
  row <- res[1, ]

  expect_true(isTRUE(row$resampling_inference[1]))
  # The Wald-on-log inversion assumes a normal reference distribution, so it
  # must not run on a resampling p. No computed interval, hence no verdict
  # against the paper's own correctly-reported one.
  expect_false(identical(row$ci_method_match[1], "OR:wald_log_from_p"))
  expect_true(is.na(row$ciL_computed[1]))
  expect_true(is.na(row$ciU_computed[1]))
  expect_false(identical(row$ci_check_status[1], "INCONSISTENT"))
})

test_that("v0.6.21: the change is INERT on a non-resampling result", {
  # Same numbers, no resampling language: every prior behaviour must be intact,
  # including the decision error that is now genuinely correct.
  txt <- "An independent-samples t-test showed t(58) = 2.31, p = .062, d = 0.61."
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)

  expect_false(isTRUE(row$resampling_inference[1]))
  expect_true(isTRUE(row$decision_error[1]))
  expect_equal(row$decision_error_reason[1], "reported_ns_computed_sig")
})

test_that("v0.6.21: Fisher's exact test is NOT treated as a resampling method", {
  # "exact test" must not be a bare keyword -- Fisher's exact is a closed-form
  # conditional test whose p IS computable, so matching it would suppress a
  # legitimate check.
  txt <- "Fisher's exact test was used, chi2(1) = 5.20, p = .062, OR = 2.50."
  res <- check_text(txt)
  expect_gte(nrow(res), 1L)
  expect_false(isTRUE(res$resampling_inference[1]))
})

test_that("v0.6.21: resampling keywords are read from the row's OWN clause", {
  # The v0.6.18 Welch precedent: reading context_window leaked a neighbouring
  # sentence's modifier onto an adjacent row (N 132 -> 403). A permutation test
  # described in ONE sentence must not exempt a DIFFERENT test in the next.
  txt <- paste0(
    "Group differences were assessed with a permutation test using 10,000 ",
    "permutations. A separate parametric analysis showed t(58) = 2.31, p = .062."
  )
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  # The t row's own clause says "parametric", not "permutation".
  expect_false(isTRUE(row$resampling_inference[1]))
})

# ---------------------------------------------------------------------------
# Cross-model review round (2026-08-07). Eight further paths were raised
# against the first draft of this fix; ALL EIGHT were reproduced locally
# before being acted on, and each is pinned below. Three were over-suppression
# (the fix hiding a valid check), four were under-application (a resampling p
# still driving a verdict or a computed value), one was a regex gap.
# ---------------------------------------------------------------------------

test_that("v0.6.21 R1: a bootstrapped CI does not exempt a parametric p", {
  # Over-suppression. "bootstrapped 95% CI" says the INTERVAL was resampled,
  # not the p-value; treating the row as resampling-based hid a real decision
  # error. Reproduced: this input lost its (correct) reported_ns_computed_sig.
  txt <- paste0("An independent t-test, t(58) = 2.31, p = .50, d = 0.61, ",
                "bootstrapped 95% CI [0.10, 1.10].")
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_false(isTRUE(row$resampling_inference[1]))
  expect_true(isTRUE(row$decision_error[1]))
  expect_equal(row$decision_error_reason[1], "reported_ns_computed_sig")
})

test_that("v0.6.21 R2: an 'n.s.' label on a resampling row is not a decision error", {
  # Under-application. This branch keys on p_ns, not p_reported, so it bypassed
  # the guard. An "n.s." label is a claim about the resampling reference
  # distribution exactly as a numeric p would be.
  res <- check_text("A permutation Welch t-test, t(58) = 2.31, n.s.")
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_true(isTRUE(row$resampling_inference[1]))
  expect_false(isTRUE(row$decision_error[1]))

  # ...and the control still fires.
  ctl <- check_text("An independent t-test, t(58) = 2.31, n.s.")
  crow <- ctl[!is.na(ctl$test_type) & ctl$test_type == "t", ]
  expect_true(isTRUE(crow$decision_error[1]))
  expect_equal(crow$decision_error_reason[1], "ns_label_vs_computed_sig")
})

test_that("v0.6.21 R3: a resampling p is not used to pick among N candidates", {
  # Under-application, and the most dangerous of the set: the correlation
  # branch chooses N by minimising |p_try - p_reported| where p_try is computed
  # PARAMETRICALLY. N then drives df, the effect size and its CI. Reproduced:
  # with candidates 42 and 380 a permutation "r = .30, p = .001" moved N to 380.
  base <- "Study 1 had N = 42 participants. Study 2 had N = 380 participants. "
  res <- check_text(paste0(base, "Using a permutation test, r = .30, p = .001."))
  row <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_gte(nrow(row), 1L)
  expect_true(isTRUE(row$resampling_inference[1]))
  expect_false(identical(as.numeric(row$N[1]), 380))

  # The parametric control still performs the selection.
  ctl <- check_text(paste0(base, "r = .30, p = .001."))
  crow <- ctl[!is.na(ctl$test_type) & ctl$test_type == "r", ]
  expect_equal(as.numeric(crow$N[1]), 380)
})

test_that("v0.6.21 R4: a resampling p is not marked an extraction artifact", {
  # Under-application. The p > .5 suspect rule is a |p_reported - p_computed|
  # MAGNITUDE test -- exactly the comparison a resampling p cannot support.
  res <- check_text("A permutation Welch t-test, t(58) = 2.31, p = .62, d = 0.61.")
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_false(isTRUE(row$extraction_suspect[1]))

  ctl <- check_text("An independent t-test, t(58) = 2.31, p = .62, d = 0.61.")
  crow <- ctl[!is.na(ctl$test_type) & ctl$test_type == "t", ]
  expect_true(isTRUE(crow$extraction_suspect[1]))
})

test_that("v0.6.21 R5: md_hl p-vs-CI disagreement is a caveat, not an inconsistency", {
  # Under-application. The invariant assumes p and CI share a reference
  # distribution; a resampling p with a percentile interval need not agree.
  txt <- paste0("The bootstrap Hodges-Lehmann median difference was 1.2; ",
                "95% CI 0.1 to 2.3; p = .062.")
  res <- check_text(txt)
  row <- res[!is.na(res$test_type) & res$test_type == "md_hl", ]
  expect_gte(nrow(row), 1L)
  expect_true(isTRUE(row$resampling_inference[1]))
  expect_false(grepl("p-CI inconsistency", row$uncertainty_reasons[1], fixed = TRUE))
  expect_match(row$uncertainty_reasons[1], "need not agree exactly")
})

test_that("v0.6.21 R6: 'randomization inference' is recognised", {
  # Regex gap: only "randomization test" matched, so the standard econometrics
  # phrasing fell through and raised a false decision error.
  for (phrase in c("Using randomization inference",
                   "Using a randomization-based procedure",
                   "Using a permutation-based procedure",
                   "Using a nonparametric bootstrap")) {
    res <- check_text(paste0(phrase, ", t(58) = 2.31, p = .062, d = 0.61."))
    row <- res[!is.na(res$test_type) & res$test_type == "t", ]
    expect_true(isTRUE(row$resampling_inference[1]), info = phrase)
    expect_false(isTRUE(row$decision_error[1]), info = phrase)
  }
})

test_that("v0.6.21 R7: a genuine method artifact keeps its method-context cap", {
  # Over-suppression. A first draft released the ERROR cap whenever the row was
  # resampling-based, but "Monte Carlo simulation power analysis" IS a method
  # artifact and must keep it. The cap is released only when the clause carries
  # no method keyword other than the resampling vocabulary.
  mc <- parse_text("A Monte Carlo simulation power analysis showed t(58) = 2.31, p = .062, d = 0.10.")
  expect_true(isTRUE(mc$method_context_in_chunk[1]))
  expect_true(isTRUE(mc$resampling_inference[1]))

  # A pure permutation result carries no method-context keyword at all.
  pm <- parse_text("A permutation test showed t(58) = 2.31, p = .062, d = 0.61.")
  expect_false(isTRUE(pm$method_context_in_chunk[1]))
  expect_true(isTRUE(pm$resampling_inference[1]))
})

test_that("v0.6.21 R8: randomized-trial language is still not resampling", {
  # The 'randomization' alternative must stay qualified.
  for (phrase in c("In this randomized controlled trial",
                   "Participants were randomly assigned",
                   "Following randomization")) {
    res <- check_text(paste0(phrase, ", t(58) = 2.31, p = .062, d = 0.61."))
    row <- res[!is.na(res$test_type) & res$test_type == "t", ]
    expect_false(isTRUE(row$resampling_inference[1]), info = phrase)
  }
})

test_that("v0.7.3 R17: reporting a permutation p does not WORSEN the verdict", {
  # Found on a REAL paper, not a constructed case. PNAS cognitive-memory 2024
  # reports "t(2037) = -3.26, P = 0.001, P-permutation = 0.002" -- exemplary
  # practice, giving BOTH reference distributions. Five such rows went OK ->
  # WARN purely for containing the word "permutation".
  #
  # Cause: the p-consistency block is not only a check, it is also the
  # WARN -> OK rescue. Gating it on !resampling_inference left the row stuck at
  # the punitive default. Penalising a paper for reporting BETTER is exactly the
  # false-positive class this line of work exists to remove.
  perm <- check_text(
    "C-noncued versus C-cued: t(2037) = -3.26, P = 0.001, P-permutation = 0.002.")
  prow <- perm[!is.na(perm$test_type) & perm$test_type == "t", ]
  expect_gte(nrow(prow), 1L)
  expect_true(isTRUE(prow$resampling_inference[1]))
  expect_false(identical(prow$status[1], "WARN"))
  expect_false(identical(prow$status[1], "ERROR"))

  # The same statistic without permutation wording is OK; the permutation row
  # must be no worse than a NOTE -- "surfaced, and here is why it is not
  # checkable", not a penalty.
  ctl <- check_text("C-noncued versus C-cued: t(2037) = -3.26, P = 0.001.")
  crow <- ctl[!is.na(ctl$test_type) & ctl$test_type == "t", ]
  expect_equal(crow$status[1], "OK")
  expect_true(prow$status[1] %in% c("OK", "NOTE"))
})

# ---------------------------------------------------------------------------
# v0.7.3 DUAL-P: a clause can carry TWO p-values of different provenance.
# "resampling_inference" is a CLAUSE-level fact; whether the BOUND p came from
# a resampling distribution is a VALUE-level fact. Conflating them shipped a
# false claim on a real paper. This is the same category the codebase already
# solves for CIs via `ci_referent` -- classify which value a modifier attaches
# to, rather than flagging the whole row.
# ---------------------------------------------------------------------------

test_that("v0.7.3 R18: a parametric p beside a permutation p is verified, not disclaimed", {
  # PNAS cognitive-memory 2024, real sentence. The bound p is the PARAMETRIC
  # 0.001 -- verified independently: 2*pt(-3.26, 2037) = 0.001132. The row
  # nonetheless asserted "this p-value is not reproducible even with the raw
  # data" about it: a false statement about what is knowable, attached to a
  # number we had just checked. That is the v0.6.19 defect class.
  res <- check_text(
    "C-noncued versus C-cued: t(2037) = -3.26, P = 0.001, P-permutation = 0.002.")
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)

  expect_equal(as.numeric(row$p_reported[1]), 0.001)
  expect_false(isTRUE(row$p_reported_is_resampling[1]))
  # The parametric p is checkable, so the row must not carry resampling caveats.
  expect_false(grepl("not reproducible even with the raw data",
                     row$uncertainty_reasons[1], fixed = TRUE))
  expect_false(grepl("not recomputable from the test statistic",
                     row$uncertainty_reasons[1], fixed = TRUE))
  expect_false(row$status[1] %in% c("WARN", "ERROR"))
})

test_that("v0.7.3 R19: an UNPROVABLE binding stays conservative", {
  # "permutation p = .062" is SPACED, so pat_p can see it and may well have
  # bound it -- reproduced: it binds .062, not the parametric .025. Where the
  # binding cannot be proven, the row must stay conservative. Checking a value
  # you cannot prove you bound correctly is worse than not checking: it
  # publishes a verdict about the wrong number.
  res <- check_text(
    "The test gave t(58) = 2.31, permutation p = .062, parametric p = .025.")
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_true(isTRUE(row$p_reported_is_resampling[1]))
  expect_false(row$status[1] %in% c("WARN", "ERROR"))
  expect_false(isTRUE(row$decision_error[1]))
})

test_that("v0.7.3 R20: permutation-only reporting is unchanged", {
  # The commonest form. Only one p, so there is nothing to disambiguate and the
  # conservative default holds -- no decision error, no false verification.
  res <- check_text(paste0(
    "A permutation Welch t-test with 10,000 permutations, ",
    "t(58) = 2.31, p = .062, d = 0.61."))
  row <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gte(nrow(row), 1L)
  expect_true(isTRUE(row$p_reported_is_resampling[1]))
  expect_false(isTRUE(row$decision_error[1]))
})
