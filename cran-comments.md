## R CMD check results

0 errors | 0 warnings | 1 note

* The only NOTE is "unable to verify current time" which is a transient
  network/clock check issue, not a package problem.

* This is a resubmission (v0.2.3). Changes since previous CRAN submission:
  - Added nonparametric test support (Mann-Whitney U, Wilcoxon W, Kruskal-Wallis H)
  - Added regression coefficient verification (b/SE parsing)
  - Added HTML report generation and CSV/JSON export
  - Added statcheck comparison functions
  - Cross-type effect size matching reduces false ERROR rate
  - SKIP status for extraction-only results
  - Universal tail fallback for one-tailed/two-tailed detection
  - Plausibility bounds for effect sizes
  - User-configurable parameters (cross_type_action, ci_affects_status, plausibility_filter)
  - Removed pdftools and officer dependencies
  - Fixed thousands-separator N parsing, Welch t-test estimation, and many parser bugs

## Notes

* **SystemRequirements**: The package uses `pdftotext` from poppler-utils for
  PDF text extraction. This is an optional dependency -- all core functionality
  works on plain text, HTML, and DOCX files without it. PDF processing
  gracefully reports when pdftotext is not available.

* **Suggested packages**: `MBESS`, `effectsize`, `statcheck`, `tesseract`,
  `magick`, and `qpdf` are optional and used for enhanced functionality
  (confidence intervals, OCR, and comparison with statcheck).

## Downstream dependencies

* No downstream dependencies (new package).

## Test environments

* Windows 11 (local), R 4.4.0
* Ubuntu (GitHub Actions), R release
