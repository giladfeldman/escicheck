# effectcheck 0.2.0

## New features

* **Nonparametric tests**: Mann-Whitney U, Wilcoxon W, and Kruskal-Wallis H
  test parsing and effect size computation (rank-biserial r, Cliff's delta,
  epsilon-squared, Kendall's W).
* **Regression support**: b/SE coefficient parsing, t = b/SE verification,
  regression type inference, standardized beta computation.
* **HTML report generation**: `generate_report()` produces self-contained HTML
  reports with executive summary, color-coded rows, and interactive tables.
  `render_report()` provides a convenience wrapper. PDF fallback available.
* **CSV and JSON export**: `export_csv()` and `export_json()` for machine-readable
  output.
* **statcheck comparison**: `compare_with_statcheck()` and
  `compare_file_with_statcheck()` for side-by-side comparison with statcheck
  results.
* **Variant helper functions**: `get_variants()`, `get_same_type_variants()`,
  `get_alternatives()`, `format_variants()`, `compare_to_variants()`,
  `get_variant_metadata()`, `get_effect_family()`.

## Bug fixes

* Fixed missing r p-value computation.
* Fixed missing CI for paired designs (ci_dz).
* Corrected eta-squared CI formula (partial eta-squared).
* Fixed MBESS `ci.sm` -> `ci.smd` call.
* Fixed `drm_from_dz` formula.
* Fixed Cohen's f regex false positives.
* z-test pattern now checked last to avoid capturing auxiliary z-values from
 U/W tests.

## Parser improvements

* Ultra-robust Unicode normalization (186+ character mappings).
* PDF column interleaving handled via pdftotext.
* Sub-chunking for multi-stat sentences.
* Greenhouse-Geisser corrected df support.
* Comprehensive handling of PDF extraction artifacts.

# effectcheck 0.1.0

* Initial release.
* Support for t-tests, F-tests/ANOVA, correlations (r), chi-square, and z-tests.
* APA-style parsing from text, PDF, HTML, and DOCX documents.
* Conservative variant-matching approach computing all plausible effect sizes.
* statcheck-compatible API (`checkPDF`, `checkHTML`, `checkPDFdir`, etc.).
* Decision error detection (significance reversals).
* S3 class with print, summary, and plot methods.
