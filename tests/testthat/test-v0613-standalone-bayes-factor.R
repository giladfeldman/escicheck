# v0.6.13 (F1): a STANDALONE Bayes factor (BF01 / BF10) reported as a PRIMARY
# finding of a RoBMA / Bayesian meta-analysis is extracted as an extraction-only
# NOTE (`test_type = "bayes_factor"`). The extraction is deliberately conservative:
# a bare `BF01 = <v>` matcher would flood every Bayesian paper. A qualifying
# standalone BF must satisfy ALL THREE, per-occurrence:
#   (1) an "evidence (for|against)" anchor within 60 chars before it,
#   (2) NO co-located frequentist statistic within +/-60 chars (F/t/r/d/OR/chi/
#       eta or a stripped-eta "= .0NN"), and
#   (3) it is NOT about "(the|an|average|main) effect" (an effect estimate).
#
# Surfaced + user-approved (option b) by the 2026-07-02 escicheck-iterate cycle-1
# canary re-audit of collabra.90203 (Identifiable Victim replication+extension,
# RoBMA reanalysis of Lee & Feeley 2016). Of the 13 `BF01 =` values the paper
# prints, the gold wants exactly 2 as standalone results: BF01 = 0.11 (publication
# bias) and BF01 = 1.24 (heterogeneity). The other 11 are companions of an F / t
# test (their frequentist row is already extracted), the model-averaged-r companion
# (BF01 = 14.93, already the v0.6.6 `r_model_averaged` row), or a DV-specific
# complementary Bayesian check (BF01 = 2.05 "an effect on moral responsibility").
#
# The exact 90203 clauses used below are verbatim from the docpluck-extracted text.

# Count bayes_factor rows safely even when check_text returns an empty tibble
# (no results parsed at all -> the tibble has no `test_type` column).
n_bf <- function(res) {
  if (!("test_type" %in% names(res)) || nrow(res) == 0L) return(0L)
  sum(res$test_type == "bayes_factor", na.rm = TRUE)
}

test_that("the two 90203 standalone RoBMA Bayes factors are extracted as NOTEs", {
  # The real 90203 RoBMA-reanalysis passage (docpluck text, loc ~10500-10730).
  txt <- paste0(
    "When applying RoBMA to the data by Lee and Freely (2016), we found moderate ",
    "evidence for publication bias (BF01 = 0.11) and strong evidence for the ",
    "absence of the average effect (BF01 = 14.93), with a model-averaged mean ",
    "effect size estimate of r = 0.002 (95% CI [0, 0.004]). In addition, we find ",
    "weak evidence against heterogeneity (BF01 = 1.24). We plotted the pattern."
  )
  res <- effectcheck::check_text(txt)
  bf <- res[!is.na(res$test_type) & res$test_type == "bayes_factor", ]

  # Exactly the two standalone Bayes factors, no more.
  expect_equal(nrow(bf), 2L)
  expect_setequal(round(bf$effect_reported, 2), c(0.11, 1.24))
  # NEVER the model-averaged-r companion (14.93) nor any F/t companion.
  expect_false(any(res$effect_reported == 14.93 &
                   res$test_type == "bayes_factor", na.rm = TRUE))

  # Both are extraction-only NOTEs (not SKIP -- the reported BF is surfaced).
  expect_true(all(bf$status == "NOTE"))
  expect_true(all(bf$check_scope == "extraction_only"))
  expect_true(all(bf$effect_reported_name == "BF01"))

  # The v0.6.6 model-averaged-r row is unaffected (still present, r = 0.002).
  ername <- ifelse(is.na(res$effect_reported_name), "", res$effect_reported_name)
  expect_true(any(ername == "r_model_averaged"))
})

test_that("a Bayes factor co-located with an F / t statistic is NOT extracted", {
  # 90203 loc ~48292: the BF is a companion of a reported t-test (already a row).
  txt <- paste0(
    "We found no support for differences between statistical and identifiable ",
    "victim conditions, t(998) = 0.097, p = 1.00, BF01 = 11.57, d = 0.01, ",
    "95% CI [-0.16, 0.14]."
  )
  res <- effectcheck::check_text(txt)
  expect_equal(n_bf(res), 0L)
  # The t-test itself is still extracted.
  expect_true(any(res$test_type == "t", na.rm = TRUE))
})

test_that("a Bayes factor without an evidence-for/against anchor is NOT extracted", {
  # 90203 loc ~51533: "complementary Bayesian analysis (BF01 = 25.03)" -- the
  # anchor is "analysis", not "evidence for/against", so it stays out.
  txt <- paste0(
    "Therefore, we conclude the effect did not replicate, and with strong evidence ",
    "against the effect in a complementary Bayesian analysis (BF01 = 25.03)."
  )
  res <- effectcheck::check_text(txt)
  # "evidence against the effect" would anchor, BUT guard (3) excludes "the effect".
  expect_equal(n_bf(res), 0L)
})

test_that("a Bayes factor about 'an effect on <DV>' is NOT extracted", {
  # 90203 loc ~56399: "weak evidence against an effect on moral responsibility
  # (BF01 = 2.05)" -- a DV-specific complementary check, excluded by guard (3).
  txt <- paste0(
    "However, a Bayesian analysis suggests weak evidence against an effect on ",
    "moral responsibility (BF01 = 2.05) and appropriateness of donation ",
    "(BF01 = 2.24)."
  )
  res <- effectcheck::check_text(txt)
  expect_equal(n_bf(res), 0L)
})

test_that("a bare BF01 with no '=' value (table header / definition) is NOT extracted", {
  txt <- paste0(
    "In our paper we report BF01. Table 8. Hypothetical Donations: Statistical ",
    "Tests for Identifiability and Explicit Learning F p BF01 95% CI. BF01 denotes ",
    "the Bayes factor in favor of the null."
  )
  res <- effectcheck::check_text(txt)
  expect_equal(n_bf(res), 0L)
})

test_that("a standalone BF10 is extracted and flagged as evidence for H1", {
  txt <- paste0(
    "We found strong evidence for the presence of publication bias (BF10 = 8.30)."
  )
  res <- effectcheck::check_text(txt)
  bf <- res[!is.na(res$test_type) & res$test_type == "bayes_factor", ]
  expect_equal(nrow(bf), 1L)
  expect_equal(bf$effect_reported_name[1], "BF10")
  expect_equal(bf$effect_reported[1], 8.30)
  expect_equal(bf$status[1], "NOTE")
  # The direction note names the alternative (H1) for a BF10.
  expect_true(grepl("alternative", bf$uncertainty_reasons[1], fixed = TRUE))
})

test_that("two distinct standalone Bayes factors do not collapse in dedup", {
  # The dedup key includes effect_reported, so 0.11 and 1.24 stay as 2 rows.
  txt <- paste0(
    "We found moderate evidence for publication bias (BF01 = 0.11). ",
    "We also find weak evidence against heterogeneity (BF01 = 1.24)."
  )
  res <- effectcheck::check_text(txt)
  expect_equal(n_bf(res), 2L)
})

test_that("bayes_factor is off unless requested in stats", {
  txt <- "We found moderate evidence for publication bias (BF01 = 0.11)."
  res <- effectcheck::check_text(txt, stats = c("t", "F", "r"))
  expect_equal(n_bf(res), 0L)
})

# --- v0.6.13 broadened anchor (from the collabra.32572 corpus-expansion audit) ---
# collabra.32572 (candy / expected-joy) reports its Bayes factors in the bare
# JASP/BayesFactor "B10" notation (no "F") with two primary-finding phrasings the
# narrow "evidence for/against" + "BF" anchor missed:
#   "The Bayes factor indicated that the data was in favor of the alternative
#    hypothesis, B10 = 20841.04"    and    "The Bayes factor was B10 = 1.25."
# The broadened anchor accepts "B01"/"B10" AND the "in favo(u)r of the
# (alternative|null)" / "Bayes factor (was|is|...)" primary-finding forms.

test_that("a bare 'B10' Bayes factor with an 'in favor of the alternative' verdict fires", {
  txt <- paste0(
    "The hypothesis was that self-reported willingness to help in the two candy ",
    "conditions was different. The Bayes factor indicated that the data was in ",
    "favor of the alternative hypothesis, B10 = 20841.04, yielding strong support."
  )
  res <- effectcheck::check_text(txt)
  bf <- res[!is.na(res$test_type) & res$test_type == "bayes_factor", ]
  expect_equal(nrow(bf), 1L)
  expect_equal(bf$effect_reported_name[1], "BF10")  # canonicalized from "B10"
  expect_equal(bf$effect_reported[1], 20841.04)
  expect_equal(bf$status[1], "NOTE")
})

test_that("a bare 'B10' Bayes factor introduced by 'The Bayes factor was' fires", {
  txt <- paste0(
    "The hypothesis was that expected joy in the two conditions was different. ",
    "The Bayes factor was B10 = 1.25. Thus, the analysis was inconclusive."
  )
  res <- effectcheck::check_text(txt)
  bf <- res[!is.na(res$test_type) & res$test_type == "bayes_factor", ]
  expect_equal(nrow(bf), 1L)
  expect_equal(bf$effect_reported_name[1], "BF10")  # canonicalized from "B10"
  expect_equal(bf$effect_reported[1], 1.25)
  expect_equal(bf$status[1], "NOTE")
})

test_that("a bare regression 'B = 0.45' is NOT mistaken for a Bayes factor", {
  # A bare "B" coefficient without the 01/10 subscript must never match.
  txt <- "The regression showed B = 0.45, SE = 0.12, p = .001 for the predictor."
  res <- effectcheck::check_text(txt)
  expect_equal(n_bf(res), 0L)
})

test_that("a line-wrapped 'BF01\\n= 1.24' yields a clean name (no stray newline)", {
  # docpluck line-wraps the token as "BF01\n= 1.24"; the extracted
  # effect_reported_name must be the clean "BF01", not "BF01\n= 1.24".
  txt <- "In addition, we find weak evidence against heterogeneity (BF01\n= 1.24)."
  res <- effectcheck::check_text(txt)
  bf <- res[!is.na(res$test_type) & res$test_type == "bayes_factor", ]
  expect_equal(nrow(bf), 1L)
  expect_equal(bf$effect_reported_name[1], "BF01")
  expect_equal(bf$effect_reported[1], 1.24)
  expect_false(grepl("\n", bf$effect_reported_name[1], fixed = TRUE))
})
