## Schema stability test (MetaESCI request E3)
## -----------------------------------------------
## check_text() must return a tibble with the columns MetaESCI and other
## downstream pipelines depend on. (In v0.3.x this test also covered
## checkPDF()/checkPDFdir() schema equivalence; v0.4.0 removed those entry
## points — extraction now happens via docpluck before check_text() is called.)
##
## The set of "critical columns" below is the MetaESCI contract (aggregate.R,
## validate_esci.py, analysis.Rmd) -- removing or renaming any of them is a
## breaking change and must bump the effectcheck major version.

critical_columns <- c(
  "source", "check_scope", "check_type", "status",
  "uncertainty_level", "uncertainty_reasons",
  "unknown_groups_downgraded", "r2_cross_pairing_detected",
  "decision_error_downgraded", "design_ambiguous",
  "ci_match", "ci_check_status", "ci_method_match",
  "ci_width_ratio", "ci_symmetry",
  "decision_error", "decision_error_reason",
  ## v0.3.5 additions (MetaESCI CI-audit pack)
  "effect_reported_decimals", "ciL_reported_decimals",
  "ciU_reported_decimals", "stat_value_decimals",
  "ci_expected", "ci_reported",
  "ci_level_mismatch", "ci_clipped_to_bound",
  "ci_symmetry_class",
  ## v0.3.6 addition (ScienceArena tier-5 deception detection)
  "df_arity_mismatch"
)

sample_text <- paste(
  "The difference was significant, t(48) = 2.34, p = .023, d = 0.67, 95% CI [0.09, 1.25].",
  "A one-way ANOVA revealed a main effect of condition, F(2, 87) = 5.12, p = .008, eta2 = 0.11.",
  "The correlation was moderate, r(98) = .34, p < .001.",
  sep = "\n"
)

test_that("check_text() exposes all MetaESCI-critical columns", {
  result <- check_text(sample_text)
  expect_true(is.data.frame(result))
  expect_gt(nrow(result), 0)

  missing_cols <- setdiff(critical_columns, names(result))
  expect_equal(
    missing_cols, character(0),
    info = paste(
      "check_text() is missing MetaESCI-critical columns:",
      paste(missing_cols, collapse = ", ")
    )
  )
})

test_that("check_text() column set is deterministic across inputs", {
  r1 <- check_text(sample_text)
  r2 <- check_text("z = 2.31, p = .021")
  # Stable column union for rows that actually compute checks.
  expect_setequal(names(r1), names(r2))
})

test_that("checkPDF() is defunct in v0.4.0", {
  # The v0.3.x test here verified schema equivalence between check_text()
  # and checkPDF() outputs against an EFFECTCHECK_TEST_PDF fixture. v0.4.0
  # removed checkPDF(); the docpluck-extracted-text -> check_text() path is
  # the only entry point now, so schema equivalence is true by construction.
  expect_error(
    checkPDF("dummy.pdf", messages = FALSE),
    regexp = "(Defunct|docpluck|effectcheck v0\\.4)"
  )
})
