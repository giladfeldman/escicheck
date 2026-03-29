## R CMD check results

0 errors | 0 warnings | 1 note

* The only NOTE is "unable to verify current time" which is a transient
  network/clock check issue, not a package problem.

* This is a resubmission (v0.2.8). Changes since v0.2.7:
  - z-test paired variants: dz = z/sqrt(N) plus dav, drm, gz, gav, grm
  - Structural design ambiguity detection (Phase 8A-bis) for t/F(1,df)/z
  - Relaxed extraction_suspect guard for design-ambiguous cases (range-guarded)
  - R-squared cross-pairing Signal 9 (large delta >0.5)
  - Cramer's V multi-m candidate selection
  - Extended unknown group sizes downgrade to z-tests
  - 1013 tests passing, 0 failures

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
