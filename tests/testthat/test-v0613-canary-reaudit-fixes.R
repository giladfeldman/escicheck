# v0.6.13: two effectcheck defects surfaced by the 2026-07-02 cycle-2 canary
# re-audit (independent Sonnet-watches-Opus over the fixed-3 + rotating set).
#
# (1) E-mcnemar-chisq-OR (collabra.37122 loc 305): a Table-6 restatement of a
#     McNemar finding written as a bare chi-square cell with an odds ratio +
#     CI -- "chi2 (1, N = 265) = 0.00, p = .951, OR = 0.99, 95% CI [0.77, 1.27]"
#     -- was classified chisq/contingency (with the OR bound as effect_reported)
#     and SKIPped as "OR unusual for chi-square, likely extraction artifact",
#     instead of routing to mcnemar_or like the paper's 3 prose McNemar rows. A
#     1-df chi-square whose only effect size is an OR with a CI is a McNemar test.
#
# (2) E-ownclause-2arm (collabra.57785 loc 167): an independent Welch t-test whose
#     own clause states two per-arm N's -- "(N = 393) ... (N = 350), t(741) = 5.36"
#     (393 + 350 = 743 = df + 2) -- bound N = 393 as a bogus TOTAL, flagged it
#     "implausibly small for df=741 (likely parsing error)", and left n1/n2 empty.
#     The two own-clause N's summing to df+2 ARE the per-arm sizes.

# small local %||% for the uncertainty guards below
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || is.na(x)) y else x

# --- (1) McNemar-OR reroute ------------------------------------------------

test_that("a 1-df chi-square reporting an OR + CI routes to mcnemar_or", {
  txt <- "chi2 (1, N = 265) = 0.00, p = .951, OR = 0.99, 95% CI [0.77, 1.27]."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "mcnemar_or", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$effect_reported_name[1], "OR")
  expect_equal(rr$effect_reported[1], 0.99)
  expect_equal(rr$ciL_reported[1], 0.77)
  expect_equal(rr$ciU_reported[1], 1.27)
  expect_equal(rr$status[1], "NOTE")
  # It must NOT remain a chisq row.
  expect_false(any(res$test_type == "chisq", na.rm = TRUE))
})

test_that("a 1-df chi-square reporting Cramer's V stays chisq (contingency)", {
  txt <- "chi2 (1, N = 200) = 5.4, p = .02, Cramer's V = 0.16."
  res <- effectcheck::check_text(txt)
  expect_true(any(res$test_type == "chisq", na.rm = TRUE))
  expect_false(any(res$test_type == "mcnemar_or", na.rm = TRUE))
})

test_that("a chi-square reporting an OR but NO CI stays chisq (conservative gate)", {
  txt <- "chi2 (1, N = 200) = 5.4, p = .02, OR = 1.8."
  res <- effectcheck::check_text(txt)
  expect_true(any(res$test_type == "chisq", na.rm = TRUE))
  expect_false(any(res$test_type == "mcnemar_or", na.rm = TRUE))
})

test_that("a >1-df chi-square with an OR + CI stays chisq (not a 2x2)", {
  txt <- "chi2 (2, N = 200) = 5.4, p = .02, OR = 1.8, 95% CI [1.1, 2.9]."
  res <- effectcheck::check_text(txt)
  expect_true(any(res$test_type == "chisq", na.rm = TRUE))
  expect_false(any(res$test_type == "mcnemar_or", na.rm = TRUE))
})

# --- (2) own-clause two-arm N binding --------------------------------------

test_that("an independent t-test binds two own-clause per-arm N's summing to df+2", {
  txt <- paste0(
    "We ran an independent Welch's t-test (two-tailed). Participants in the ",
    "material condition (M = 4.75, SD = 1.36, N = 393) were more willing to ",
    "exchange than participants in the experiential condition (M = 4.22, ",
    "SD = 1.33, N = 350), t(741) = 5.36, p < .001, d = 0.39, 95% CI [0.25, 0.54]."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(rr), 1L)
  expect_equal(rr$N[1], 743)
  expect_equal(rr$n1[1], 393)
  expect_equal(rr$n2[1], 350)
  expect_equal(rr$N_source[1], "own_clause_arms")
  # No false "implausibly small" / "likely parsing error" WARN.
  expect_false(grepl("implausibly small", rr$uncertainty_reasons[1] %||% "", fixed = TRUE))
  expect_false(grepl("Likely parsing error", rr$uncertainty_reasons[1] %||% "", fixed = TRUE))
})

test_that("a paired t-test with a single N is not mis-bound as two-arm", {
  txt <- "Participants (N = 50) rated both, paired t(49) = 3.2, p = .002, dz = 0.45."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(rr$N[1], 50)
  expect_true(is.na(rr$n1[1]))
  expect_true(is.na(rr$n2[1]))
  expect_false(identical(rr$N_source[1], "own_clause_arms"))
})

test_that("two own-clause N's that do NOT sum to df+2 are not bound as arms", {
  txt <- "Group A (N = 100) vs Group B (N = 90), t(150) = 2.1, p = .04, d = 0.3."
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  # 100 + 90 = 190 != 150 + 2 = 152, so the two-arm rule must not fire.
  expect_false(identical(rr$N_source[1], "own_clause_arms"))
  expect_true(is.na(rr$n1[1]))
})
