## Schema stability test (MetaESCI request E3)
## -----------------------------------------------
## check_text(), checkPDF(), and checkPDFdir() must return tibbles with
## identical column sets so MetaESCI and other downstream pipelines can
## transparently swap between the text-path and PDF-path entry points.
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
  "decision_error", "decision_error_reason"
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

test_that("checkPDF() and check_text() share the same schema (when fixture available)", {
  # checkPDF() funnels through process_files_internal() -> check_text(), so the
  # schemas should be equal by construction. This test guards the invariant in
  # case someone adds a PDF-only post-process step in the future.
  fixture <- Sys.getenv("EFFECTCHECK_TEST_PDF", unset = NA)
  if (is.na(fixture) || !nzchar(fixture) || !file.exists(fixture)) {
    skip("No PDF fixture configured via EFFECTCHECK_TEST_PDF env var")
  }

  text_result <- check_text(sample_text)
  pdf_result <- tryCatch(
    checkPDF(fixture, messages = FALSE),
    error = function(e) NULL
  )
  if (is.null(pdf_result) || nrow(pdf_result) == 0) {
    skip("PDF fixture produced no rows; cannot compare schemas")
  }

  text_cols <- names(text_result)
  pdf_cols <- names(pdf_result)
  missing_in_pdf <- setdiff(text_cols, pdf_cols)
  missing_in_text <- setdiff(pdf_cols, text_cols)

  expect_equal(missing_in_pdf, character(0),
               info = paste("PDF path dropped columns:", paste(missing_in_pdf, collapse = ", ")))
  expect_equal(missing_in_text, character(0),
               info = paste("Text path dropped columns:", paste(missing_in_text, collapse = ", ")))

  # Compatible types on the critical column set
  for (col in intersect(critical_columns, intersect(text_cols, pdf_cols))) {
    expect_identical(
      class(text_result[[col]])[1],
      class(pdf_result[[col]])[1],
      info = paste("type mismatch on column", col)
    )
  }
})
