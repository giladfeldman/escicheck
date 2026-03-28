## R CMD check results

0 errors | 0 warnings | 1 note

* The only NOTE is "unable to verify current time" which is a transient
  network/clock check issue, not a package problem.

* This is a resubmission (v0.2.7). Changes since v0.2.3:
  - Design-ambiguous t-test/F(1,df) downgrade to WARN (reduces false ERROR rate)
  - Unknown group sizes action (unknown_groups_action param)
  - Minimum confidence filtering (min_confidence param)
  - Confidence scoring (0-10) for each result
  - Check scope classification (effect_size_checked, p_value_only, ci_checked, extraction_only)
  - Dropped decimal p-value recovery (p=484 -> p=.484)
  - Header/footer stripping for PDF extraction
  - Decision error now requires reported p-value (prevents false alarms on bare stats)
  - Cross-type effect size matching reduces false ERROR rate
  - Plausibility bounds for computed effect sizes
  - 986 tests passing, 0 failures

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
