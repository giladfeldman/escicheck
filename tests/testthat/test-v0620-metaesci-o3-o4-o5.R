# v0.6.20 -- MetaESCI O-3, O-4, O-5, plus the honesty fixes the O-1 sweep found.
# Every test authored against the UNFIXED code and watched to fail first.

# ---------------------------------------------------------------------------
# O-3: which estimate is the reported CI centred on?
# ---------------------------------------------------------------------------

test_that("a CI centred on the unstandardized b is graded on the b scale", {
  # Before this, the reported interval was compared against the computed
  # STANDARDIZED-beta interval whatever it referred to, so a b-referenced row
  # failed for a reason with nothing to do with the paper. MetaESCI measured
  # 601/1,048 regression rows mis-dispatched (68.1% agreement when correctly
  # referenced vs a 5.9% pooled headline).
  #
  # Hand ground truth: b = 0.45, SE = 0.12, df = 200, t_crit = qt(.975, 200) =
  # 1.971896 -> Wald CI = 0.45 +/- 0.2366 = [0.2134, 0.6866], which the paper's
  # [0.21, 0.69] reproduces to the printed precision.
  res <- effectcheck::check_text(
    "b = 0.45, SE = 0.12, t(200) = 3.75, p < .001, 95% CI [0.21, 0.69]")
  expect_equal(as.character(res$ci_referent[1]), "b_coeff")
  expect_equal(as.character(res$ci_check_status[1]), "MATCH")
  expect_true(grepl("^b_coeff", as.character(res$ci_method_match[1])))
})

test_that("a CI centred on the standardized beta is still graded on the beta scale", {
  res <- effectcheck::check_text(
    "b = 1.40, SE = 0.37, beta = 0.26, t(200) = 3.78, p < .001, 95% CI [0.12, 0.40]")
  expect_equal(as.character(res$ci_referent[1]), "standardized_beta")
  expect_equal(as.character(res$ci_check_status[1]), "MATCH")
})

test_that("a CI matching neither candidate abstains rather than guessing", {
  res <- effectcheck::check_text(
    "b = 0.45, SE = 0.12, beta = 0.26, t(200) = 3.75, p < .001, 95% CI [0.90, 1.40]")
  expect_equal(as.character(res$ci_referent[1]), "unknown")
})

test_that("a b-scale CI with no SE is declared unverifiable, not INCONSISTENT", {
  # The parser types this row as a plain t-test (no SE -> no regression row), and
  # it hit the identical cross-scale defect: the b-scale interval was graded
  # against a Cohen's d interval and returned a false INCONSISTENT. This is why
  # the classification is keyed on the presence of a b, not on test_type.
  res <- effectcheck::check_text("b = 0.45, t(200) = 3.75, p < .001, 95% CI [0.21, 0.69]")
  expect_equal(as.character(res$ci_referent[1]), "b_coeff")
  expect_equal(as.character(res$ci_check_status[1]), "UNVERIFIABLE")
})

test_that("ci_referent stays NA where the question does not arise", {
  res <- effectcheck::check_text("t(48) = 2.31, p = .025, d = 0.65, 95% CI [0.08, 1.21]")
  expect_true(is.na(res$ci_referent[1]))
  expect_equal(as.character(res$ci_check_status[1]), "MATCH")
})

# ---------------------------------------------------------------------------
# O-4: R2 reported on a correlation
# ---------------------------------------------------------------------------

test_that("an R2 correctly reported for a correlation matches r^2 and passes", {
  # r = .32 -> r^2 = .1024, and .10 is its correct APA-rounded report. At 0.6.19
  # this WARNed, because r_squared was only ever an `alternatives` entry: with no
  # same-type computed counterpart the matcher fell through to Cohen's
  # f2 = r^2/(1-r^2) = .1141, a DIFFERENT SCALE. Even a hand-perfect R2 = 0.1024
  # WARNed. MetaESCI attributes this to the `r = c("r", "R2")` validity list;
  # that list is right -- R2 is a legitimate thing to report for a correlation --
  # the defect was the missing variant.
  res <- effectcheck::check_text("r(1526) = .32, p < .001, R2 = 0.10")
  expect_equal(as.character(res$matched_variant[1]), "R2")
  expect_equal(res$matched_value[1], 0.32^2)
  expect_equal(as.character(res$status[1]), "PASS")

  res <- effectcheck::check_text("r(1526) = .32, p < .001, R2 = 0.1024")
  expect_equal(as.character(res$status[1]), "PASS")
  expect_equal(res$delta_effect[1], 0, tolerance = 1e-9)
})

test_that("a reported f2 on a correlation still routes to Cohen's f2", {
  res <- effectcheck::check_text("r(1526) = .32, p < .001, f2 = 0.1141")
  expect_equal(as.character(res$matched_variant[1]), "cohens_f2")
  expect_equal(as.character(res$status[1]), "PASS")
})

# ---------------------------------------------------------------------------
# O-5: z-test reporting an odds ratio
# ---------------------------------------------------------------------------

test_that("an odds ratio on a z-test is not called unusual", {
  # RULING: a Wald z of a logistic coefficient is BY CONSTRUCTION
  # z = ln(OR)/SE(ln OR), and a meta-analytic z tests a pooled log-OR. The OR is
  # the natural effect size in both. The old message asserted a methodological
  # problem that does not exist, and MetaESCI measured it driving 56.5% of z rows
  # into an anomaly category against 11.2% for t-tests.
  res <- effectcheck::check_text("z = 2.45, p = .014, OR = 1.83")
  reasons <- paste(res$uncertainty_reasons, collapse = " ")
  expect_false(grepl("unusual for z-test", reasons))
  expect_true(grepl("cannot be verified from the z alone", reasons))
})

test_that("an odds ratio WITH its CI is verified against the reported z", {
  # SE(lnOR) = (ln U - ln L) / (2 * z_level); z_implied = ln(OR)/SE.
  # OR = 1.83, 95% CI [1.13, 2.96] -> SE = 0.245663, z_implied = 2.4600.
  se <- (log(2.96) - log(1.13)) / (2 * stats::qnorm(0.975))
  expect_equal(log(1.83) / se, 2.4600, tolerance = 1e-3)
  res <- effectcheck::check_text("z = 2.45, p = .014, OR = 1.83, 95% CI [1.13, 2.96]")
  reasons <- paste(res$uncertainty_reasons, collapse = " ")
  expect_true(grepl("implies z = 2\\.460", reasons))
  expect_true(grepl("reported z = 2\\.450", reasons))
})

test_that("the implied z is a message, NEVER an effect-size match", {
  # Caught by /ship review. The first version added the implied z to
  # `computed_variants`, which is the effect-size matcher's candidate pool: with
  # no same-type variant available the matcher falls back to ANY computed
  # variant, so it matched the implied z against the reported ODDS RATIO and
  # published matched_value = 2.460 with delta_effect = 0.630 -- an odds ratio
  # minus a z-statistic. `delta_effect` is exactly the field MetaESCI's pipeline
  # reads. Worse, it moved with the CI level (0.234 at 90%), which no effect-size
  # delta can do.
  for (lvl in c("95", "90")) {
    txt <- sprintf("z = 2.45, p = .014, OR = 1.83, %s%% CI [1.13, 2.96]", lvl)
    res <- effectcheck::check_text(txt)
    expect_true(is.na(res$matched_variant[1]), info = txt)
    expect_true(is.na(res$matched_value[1]), info = txt)
    expect_true(is.na(res$delta_effect[1]), info = txt)
  }
  # ...but the diagnostic itself must survive.
  expect_true(grepl("implies z = 2\\.460", paste(
    effectcheck::check_text("z = 2.45, p = .014, OR = 1.83, 95% CI [1.13, 2.96]")$uncertainty_reasons,
    collapse = " ")))
})

test_that("the b-scale CI variant is a CI target only, never an effect-size match", {
  # Same hazard as the implied z, so it is pinned rather than argued: b_coeff
  # belongs to no effect-size family, so the family filter keeps it out of the
  # matcher while `collect_ci_candidates` still reaches it. Constructed so
  # b_coeff would win on numbers alone -- the reported effect equals b exactly
  # (delta 0) while the standardized beta is 0.19 away.
  res <- effectcheck::check_text(
    "b = 0.45, SE = 0.12, t(200) = 3.75, p < .001, OR = 0.45, 95% CI [0.21, 0.69]")
  expect_false(identical(as.character(res$matched_variant[1]), "b_coeff"))
  # And it is still doing its actual job on a row with no competing effect.
  plain <- effectcheck::check_text(
    "b = 0.45, SE = 0.12, t(200) = 3.75, p < .001, 95% CI [0.21, 0.69]")
  expect_equal(as.character(plain$ci_method_match[1]), "b_coeff:primary")
  expect_true(is.na(plain$matched_variant[1]))
})

test_that("the OR/z check refuses inputs it cannot legitimately use", {
  # A non-positive OR has no logarithm; an estimate outside its own interval is a
  # different reporting defect, and back-deriving an SE from it would manufacture
  # a number from known-broken inputs.
  for (txt in c("z = 2.45, p = .014, OR = -1.83, 95% CI [1.13, 2.96]",
                "z = 2.45, p = .014, OR = 5.00, 95% CI [1.13, 2.96]")) {
    reasons <- paste(effectcheck::check_text(txt)$uncertainty_reasons, collapse = " ")
    expect_false(grepl("implies z =", reasons), info = txt)
  }
})

# ---------------------------------------------------------------------------
# Class B: silent substitution / silent loss found by the O-1 sweep
# ---------------------------------------------------------------------------

test_that("an impossible p is never published as a truncated in-range value", {
  # pat_p's bare `[01]` alternative had no right-hand boundary, so "p = 10"
  # matched the leading "1" and shipped p_reported = 1 with p_valid = TRUE and
  # p_out_of_range = FALSE. The [0,1] validation never saw the offending value
  # because the regex had already truncated it into range.
  res <- effectcheck::check_text("t(48) = 2.31, p = 10")
  expect_true(is.na(res$p_reported[1]))
  expect_false(res$p_valid[1])
  expect_true(res$p_out_of_range[1])

  # A p the pattern cannot match at all is flagged too, rather than reading
  # downstream as "this result reported no p-value".
  expect_true(effectcheck::check_text("t(48) = 2.31, p = 3.3")$p_out_of_range[1])
})

test_that("legitimate p-values are untouched, including a sentence-final 'p = 1.'", {
  # The first draft of the boundary used (?![0-9.]) and rejected "p = 1." at the
  # end of a sentence, turning a valid in-range p into a false out-of-range
  # claim (cross-model review, reproduced before fixing).
  expect_equal(effectcheck::check_text("F(1, 40) = 0.02, p = 1.")$p_reported[1], 1)
  expect_false(effectcheck::check_text("F(1, 40) = 0.02, p = 1.")$p_out_of_range[1])
  for (pr in list(list("p = .025", 0.025), list("p = 0.05", 0.05), list("p = 1", 1),
                  list("p = 0", 0), list("p = 1.0", 1), list("p < .001", 0.001))) {
    expect_equal(effectcheck::check_text(sprintf("t(48) = 2.31, %s", pr[[1]]))$p_reported[1],
                 pr[[2]], info = pr[[1]])
  }
})

test_that("an unrelated impossible p elsewhere in the chunk does not flag an ns row", {
  # The malformed detector is necessarily chunk-scoped, so it must not fire on a
  # row whose p is legitimately non-numeric (cross-model review, reproduced).
  res <- effectcheck::check_text(
    "The reaction time on trial p = 10 was recorded, and separately t(48) = 2.31, ns, d = 0.74.")
  expect_false(res$p_out_of_range[1])
})

test_that("the guard flags REACH check_text() output, not just the internal message", {
  # Caught by /ship QA after the rest of this release was already green: the
  # parser produced `effect_guard_rejected` / `effect_guard_reason` and check.R
  # consumed them internally (uncertainty message + extraction_suspect), but they
  # never reached the output tibble -- write-only columns, documented in API.md
  # and absent from the data. MetaESCI O-1 request 2 was explicitly so a consumer
  # could DISTINGUISH a suppressed effect size from an absent one, which needs a
  # column to filter on; the internal message alone does not satisfy it.
  res <- effectcheck::check_text("F(1, 30) = 4.42, p = .04, R2 = 52.2")
  expect_true("effect_guard_rejected" %in% names(res))
  expect_true("effect_guard_reason" %in% names(res))
  expect_true(res$effect_guard_rejected[1])
  expect_true(grepl("52.2", res$effect_guard_reason[1], fixed = TRUE))

  # And FALSE / NA on an ordinary row, so the flag means something.
  clean <- effectcheck::check_text("t(48) = 2.31, p = .025, d = 0.65")
  expect_false(clean$effect_guard_rejected[1])
  expect_true(is.na(clean$effect_guard_reason[1]))
})

test_that("a value the plausibility guard suppresses is reported as suppressed", {
  # The guard used to null the value and say nothing, leaving the row
  # indistinguishable from "this statistic reported no effect size" -- a false
  # all-clear, and the majority (27 of 42) of MetaESCI's O-1 corruptions took
  # exactly this silent-loss path.
  res <- effectcheck::check_text("F(1, 30) = 4.42, p = .04, R2 = 52.2")
  expect_true(is.na(res$effect_reported[1]))
  expect_true(res$extraction_suspect[1])
  expect_true(grepl("suppressed at parse time",
                    paste(res$uncertainty_reasons, collapse = " ")))
})

test_that("an impossible confidence level is rejected at BOTH ends", {
  # The guard tested only `ci_level < 0.50`, so "263.95% CI" yielded
  # ci_level = 2.6395 with ci_level_mismatch = NA and status PASS. A coverage
  # probability of 1 or more is not implausible but impossible.
  for (txt in c("r(100) = .30, 263.95% CI [0.11, 0.47]",
                "r(100) = .30, 100% CI [0.11, 0.47]")) {
    res <- effectcheck::check_text(txt)
    expect_equal(res$ci_level[1], 0.95, info = txt)
    expect_equal(as.character(res$ci_level_source[1]), "implausible_level", info = txt)
  }
  # A real non-95% level is still honoured.
  expect_equal(effectcheck::check_text("r(100) = .30, 90% CI [0.11, 0.47]")$ci_level[1], 0.90)
})
