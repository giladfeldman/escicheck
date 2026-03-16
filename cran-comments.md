## R CMD check results

0 errors | 0 warnings | 1 note

* This is a resubmission. Changes from previous submission:
  - Fixed invalid file URI `../CLAUDE.md` in README.md
  - Spelled out "APA" (American Psychological Association) in DESCRIPTION to
    avoid CRAN spelling note

## Notes

* **SystemRequirements**: The package uses `pdftotext` from poppler-utils for
  PDF text extraction. This is an optional dependency -- all core functionality
  works on plain text, HTML, and DOCX files without it. PDF processing
  gracefully reports when pdftotext is not available.

* **Suggested packages**: `MBESS`, `effectsize`, `statcheck`, `tesseract`,
  `magick`, and `qpdf` are optional and used for enhanced functionality
  (confidence intervals, OCR, and comparison with statcheck).

## Test environments

* Windows 11 (local), R 4.4.0
* Ubuntu (GitHub Actions), R release
