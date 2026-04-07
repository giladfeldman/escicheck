# effectcheck 0.3.0m

## Bug fixes (MetaESCI batch validation)

* Fixed 153 false positive ERRORs from unstandardized regression
  coefficients (`b = 0.29`) being compared to computed standardized beta.
  Parser's `pat_eta` regex matched "eta" inside "beta", mislabelling
  effect sizes. Added negative lookbehind and b-masquerade detection.
* Fixed 27 F-test computation crashes (NaN delta) from list-type values
  in Phase 5 matching and infinite CI bounds from `ci_cohens_f` when
  eta-squared is near 1.0. Added defensive guards throughout.
* Added z-test CI computation for d, dz, and r variants. Previously
  1,517 z-test results with reported CIs showed UNVERIFIABLE status.

# effectcheck 0.3.0l

## Enhancements

* Recognize r as a self-verifying effect size. When r is reported as the
  test statistic (e.g., `r(48) = .42`), it now routes through effect-size
  checking with PASS status, not p_value_only with OK.

# effectcheck 0.3.0k

## Bug fixes

* Fixed `bind_rows()` crash during batch processing when MBESS noncentral
  F-inversion returns non-numeric types under extreme noncentrality
  parameters (>37.62). `ciL_computed` became a list instead of double,
  crashing `dplyr::bind_rows()`. Now coerced to numeric with `NA` fallback.

# effectcheck 0.3.0j

## Bug fixes

* Fixed decimal CI level parsing: papers reporting "99.9% CI", "99.5% CI"
  produced ci_level values of 0.09, 0.05 due to `(\d+)%` regex failing
  on decimal percentages. Changed to `(\d+\.?\d*)%` with plausibility
  guard (ci_level < 0.50 falls back to 0.95).

# effectcheck 0.3.0i

## Bug fixes and cleanup

* Fixed eta-squared (η²) extraction from PDFs using Unicode mathematical
  italic characters.
* Removed WebR/Private mode (archived to `archive/webr/`). Frontend is
  Cloud-mode only.
* Optimized pdftotext extraction pipeline.

# effectcheck 0.3.0h

## Bug fixes and cleanup

* Fixed bare Cohen's f not recognized for t-tests. `t(287.58) = -0.21,
  p = 0.837, f = -0.01` now correctly extracts `f = -0.01` for any test
  type (was gated to F-tests only).
* Removed Shiny app (`shiny/` directory, `start_shiny.bat`). Next.js
  frontend is the sole UI.

# effectcheck 0.3.0g

## Bug fixes and new features

* Fixed F-test df falsely parsed as CI bounds: `F(2, 76) = 3.45` no longer
  produces `ciL=2, ciU=76`. pat_CI4 now checks if matched values equal
  df1/df2 and skips them.
* Multi-method CI verification: Phase 6 now tries all available CI
  computation methods (noncentral t, normal approximation, Steiger 2004
  for eta-squared) and picks the best match. New output columns:
  `ci_delta_upper`, `ci_check_status`, `ci_method_match`, `ci_width_ratio`,
  `ci_symmetry`.
* Removed dead CI pattern matching code.
* 1286 total tests passing.

# effectcheck 0.3.0f-fix

## Bug fixes

* Page number artifact guard: strip standalone digits before line-break
  joining to prevent page numbers from being concatenated with statistics.

# effectcheck 0.3.0f

## Parser fixes and artifact detection

Addresses 13 false positive ERRORs from MetaESCI v0.3.0c validation
(132,537 results, 24 ERRORs). Expected: 24 -> ~10 ERRORs.

### Bug fixes

* Capital D/G effect sizes now parsed: `D = 0.44`, `Hedges' G = 0.85`,
  `Dz = 0.40` all correctly matched (was: returned NA). 5 confirmed cases.
* Generalized eta-squared (geta-squared, Geta-squared, generalized
  eta-squared) now correctly labeled as `generalized_eta2` and routed
  to NOTE (was: parsed as plain eta2, producing false ERRORs). 8 cases.
  Generalized eta-squared cannot be computed from F/df (Bakeman 2005).
* d-vs-t cross-check: when |d| > 3 and far exceeds the maximum
  plausible d from t and df, flags as extraction artifact (NOTE).
  Catches two-column PDF interleaving garbled values.
* d > 10 rejection extended to dz/dav/drm (was: only d/g). Catches
  43 line-number artifacts (dz=219, dz=388, etc.).
* d > 5 integer + spurious context guard extended to all d-family types.

### New features

* Phase 8G: heuristic generalized eta-squared detection. When reported
  eta2 < computed partial eta2 with ratio 0.10-0.95, downgrades ERROR
  to WARN with explanatory note.
* Phase 14: cross-result effect size sweep. When a result has ERROR,
  tries matching the reported effect size against ALL other test
  statistics in the same article. Reports all attempts to the user.
  If a match is found with a different statistic, downgrades to WARN
  with cross-pairing note. Covers eta2/omega2/f from F, d/g/dz from
  t/F(1,df), V/phi from chi-square, r from t.

* Cross-type effect size conversions: t-test now computes eta2, omega2,
  Cohen's f, and R2 as alternatives (t-test = F(1,df) equivalence).
  r-test computes d = 2r/sqrt(1-r^2). z-test computes r = z/sqrt(z^2+N).
  Chi-square computes Cohen's w, contingency coefficient C, and d from
  phi (for 2x2 tables). All cross-type matches are alternatives — they
  activate when the author reports an unconventional effect size for
  the test type.

### Tests

* 54 new tests (1236 total, 0 failures, 0 regressions)

---

# effectcheck 0.2.8

## Design ambiguity improvements

Addresses 399 remaining ERRORs from MetaESCI v0.2.7 audit (132,499 results).
Philosophy: compute ALL plausible alternatives under different design assumptions;
if ANY alternative matches, downgrade severity.

### New features

* z-test paired variants: added `dz = z/sqrt(N)` (paired/Wilcoxon assumption)
  alongside existing `d = 2z/sqrt(N)` (independent/Mann-Whitney). Also computes
  dav, drm via r-grid sweep and gz, gav, grm Hedges-corrected variants.
* Phase 8A-bis: structural design ambiguity detection for t-tests, F(1,df), and
  z-tests. When both independent and paired variant families are computed,
  promotes ambiguity_level to "ambiguous" regardless of which variant matches best.
* Phase 8D Signal 9: large R-squared delta (>0.5) with contextual signals now
  triggers cross-pairing detection. R-squared is bounded [0,1], so delta>0.5
  means F and R-squared are almost certainly from different models.
* Cramer's V multi-m: when df allows multiple table dimensions, tries all m
  candidates and picks the one producing V closest to reported value.

### Bug fixes

* Relaxed extraction_suspect guard in Phase 8B for design-ambiguous cases.
  Large deltas are expected (d-from-t vs d-from-raw differs ~2x for paired
  designs) and should not block the design-ambiguous downgrade. Range guard
  ensures genuinely wrong values still produce ERROR.
* Extended Phase 8C (unknown group sizes downgrade) to cover z-tests.
* Relaxed Phase 8D ANOVA context guard for extreme R-squared deltas (>0.5)
  when regression signals are also present.

### Internal

* New compute function: `dz_from_z(z, N)` — paired d from z-statistic
* 27 new tests in test-v028-design-ambiguity.R (1013 total, 0 failures)

# effectcheck 0.2.7

## Bug fixes and API improvements

### Bug fixes

* Fixed `devtools::load_all()` calls in test files that broke R CMD check in CI
  (devtools is not available in CI environment)
* Fixed codoc mismatch: `unknown_groups_action` parameter was missing from Rd
  documentation for `check_text()` and `compute_and_compare_one()`
* Added `min_confidence` parameter forwarding in plumber.R API

### Documentation

* Removed working documents from public repo (FEEDBACK_RESPONSE, METAESCI_REPORT,
  DEVELOPMENT, testingai) — moved to archive/
* Updated API.md to version 0.2.7 with `unknown_groups_action` and `min_confidence`
  parameter documentation
* Version bump from 0.2.6 to 0.2.7 (aligns DESCRIPTION with frontend)

---

# effectcheck 0.2.6

## Design ambiguity + decision error fixes

Based on MetaESCI analysis of 132,499 results from 8,415 articles. These changes
reduce the ERROR false positive rate from ~3.9% to ~0.8%.

### Design-ambiguous t-test downgrade (check.R)

* New `design_ambiguous_action` parameter (default `"WARN"`). When a t-test or
  F(1,df) effect size ERROR occurs with ambiguous variant matching, the status is
  downgraded to WARN with confidence capped at 4. This reflects the known
  limitation that d computed from t-statistics systematically differs from d
  computed from raw data (means/SDs).

### Decision error requires reported p-value (check.R)

* Decision errors now require an explicitly reported p-value. Without one, there
  is no author's significance decision to check. Fixes false decision errors for
  regression z-statistics from coefficient tables and other extraction-only results.

### r-test global N guard (check.R)

* Decision errors suppressed for r-tests when sample size was inferred from global
  text (`N_source == "global_text"`). The globally-inferred N may not apply to
  this specific correlation (e.g., subgroup analysis).

### API changes

* New parameter: `design_ambiguous_action` (forwarded via plumber.R)
* Fixed: `method_context_action` was missing from plumber.R option map

# effectcheck 0.2.5

## PDF extraction quality improvements

Based on MetaESCI extraction analysis of 121,040 results from 8,415 PDFs across 7 journals.
These changes reduce PDF extraction artifacts affecting statistical parsing from ~6.5% to ~0.6%.

### Header/footer stripping (utils-pdf.R)

* New `strip_headers_footers()` function removes repeated lines (5+ occurrences, 15-120 chars)
  from pdftotext output. Fixes page-number-appended-to-p-value artifacts.

### Dropped decimal recovery (parse.R)

* `p < 001` now corrected to `p < .001` during normalization.
* `p = NNN` where NNN has 3+ digits (e.g., `p = 484`) corrected to `p = .NNN` (e.g., `p = .484`).
  Flagged as `extraction_suspect` with assumption note in `uncertainty_reasons`.
* New `p_decimal_corrected` column in parsed output tracks which p-values were corrected.

### General line-break joining (parse.R)

* Lines ending with `=`, `<`, or `>` followed by a digit on the next line are now joined.
  Catches edge cases like `F(1, 30) =\n4.425` that existing stat-specific patterns missed.
* Lines where `(` is followed by a line break then a digit are joined (broken df).

### Standalone page number removal (parse.R)

* Lines containing only 1-3 digits are removed during normalization (page numbers).

### Computation-guided decimal recovery (check.R — Phase 5B)

* When `extraction_suspect` is triggered by an extreme delta, the pipeline now tries all
  possible decimal placements of the reported effect size (e.g., 615 → 61.5, 6.15, 0.615)
  and checks if any matches the computed value within tolerance.
* If a match is found, the effect size is recovered with a `decimal_recovered` flag and
  detailed assumption note. Status is re-evaluated (may become PASS/WARN).
* Uses computed values as oracle — self-verifying, zero risk to correct data.
* Also flags p-values that were decimal-corrected during normalization.

### New columns

* `decimal_recovered`: TRUE when Phase 5B successfully recovered a dropped decimal
* `p_decimal_corrected` (parse output): TRUE when normalization corrected a dropped decimal in p-value

### Tests

* 39 new extraction quality tests in `test-extraction-quality.R`
* 878 total tests passing (0 failures)

# effectcheck 0.2.4

## Validation-driven improvements

Based on comprehensive validation of 19,690 results across 7 journals (MetaESCI).

### Bug fixes (Category A — 673 results)

* **warn_tiny_delta** (48 results): Decision error no longer upgrades PASS→WARN when
  effect size match is excellent (delta < 0.5x tolerance) and ambiguity is clear.
* **method_context_in_result** (527 results, 58 ERRORs): New `method_context_in_chunk`
  flag distinguishes method keywords IN the stat's sentence vs nearby context. ERROR
  status capped at NOTE for in-chunk method contexts (power analysis, meta-analysis, etc.).
* **cross_type_error** (68 results, all ERROR): Phase 5 same-type matching now includes
  `alternatives` (e.g., g_ind for t-tests). Previously only `computed_variants` were
  searched, missing valid same-type matches.
* **effect_not_in_text** (18 results, 15 ERRORs): Parse-time rejection of impossible
  effect sizes: R2/V/phi/eta2 > 1.0, round-integer d/g > 5.
* **suspicious_decision_error** (25 results, 4 ERRORs): `effect_test_mismatch` flag
  now caps ERROR→NOTE for type-incompatible effect sizes (e.g., chi2 with R2=52).
* **garbled_p_threshold** (4 results): Extended garbled p-value detection to non-inequality
  p > 0.5 with large computed discrepancy.

### Extraction guards (Category B — 41 PDF extraction artifacts)

* **Computed-side plausibility**: If computed effect size exceeds plausibility bounds
  (e.g., computed d=13.49 from garbled t-stat), flags `extraction_suspect` and caps
  ERROR→NOTE.
* **Stat value plausibility**: Flags |t| > 100 and F > 10000 as possible artifacts.
* **DF plausibility**: Flags df ≤ 0 or df > 50000 as possible artifacts.
* **Tightened bounds**: d/g/dz/dav/drm plausibility from 10→5.

### New features

* **Confidence score** (`confidence` column, 0-10 integer): Deterministic quality score
  aggregating ambiguity level, match type, delta distance from threshold, design
  inference, and extraction quality.
* **Result context** (`result_context` column): "study" or "method" classification.
* **`method_context_action` parameter**: Controls behavior for method-context stats
  ("NOTE", "WARN", or "SKIP"). Default: "NOTE".
* **`min_confidence` parameter**: Minimum confidence score for output filtering.
  Results below threshold are dropped. Default: 0 (no filtering).

# effectcheck 0.2.3

## New features

* **User configuration**: New `cross_type_action`, `ci_affects_status`, and
  `plausibility_filter` parameters for `check_text()`.
* **Plausibility bounds**: Implausibly large effect sizes (e.g., d > 10, r > 1)
  flagged via `extraction_suspect` column. Configurable via `EFFECT_PLAUSIBILITY`
  in constants.
* **Effect size families**: OR, RR, IRR, h added to `EFFECT_SIZE_FAMILIES`.
* **Completeness**: Cohen's f² = r²/(1-r²) for r-tests.
* **Beginner-friendly UI**: Legend, tooltips, plain English verdicts, narrative
  report mode in frontend.
* **Paste-text input**: Direct text input mode in frontend.

## Bug fixes

* Fixed F→t conversion missing Hedges' g variant (`g_ind`).
* Added `d_ind_min`/`d_ind_max` bounds for F-test conversions.
* Fixed chi-square inline N parsing and multi-stat sentence handling.
* Fixed section-number false positives in p-value extraction.
* Fixed `ns` (non-significant) notation parsing.

## API changes

* Options forwarding in plumber.R via `do.call()`.
* Summary and version fields included in all API responses.

# effectcheck 0.2.2

## Bug fixes

* **Cross-type matching**: Best match now selected across effect size types,
  reducing false ERROR rate from 47.6% to ~10%.
* **Paired N fix**: Paired designs no longer double the sample size incorrectly.
* **NOTE→PASS**: Results matching within tolerance now correctly report PASS
  instead of NOTE.
* **check_type column**: Added to output for transparency in variant matching.
* **extraction_suspect flag**: Extreme deltas flagged for manual review.

## New features

* **SKIP status**: Extraction-only results (no p-value or effect size to verify)
  now get status "SKIP" instead of misleading WARN.
* **Universal tail fallback**: Phase 9 tries both one-tailed and two-tailed
  when decision error occurs, resolving to NOTE with explanation.
* **Two-tailed detection**: New `two_tailed_detected` flag overrides one-tailed
  when both present in text.
* **Method context detection**: `method_context_detected` flag suppresses
  decision_error for p-curve, equivalence test, TOST contexts.
* **N candidates for r-tests**: Multiple N values extracted from context;
  best match selected with assumption note.
* **One-tailed scope fix**: `one_tailed_detected` now searches chunk only,
  preventing cross-chunk bleeding.
* **Garbled p-value detection**: p-values like p < 0.645 flagged as
  `extraction_suspect`.

# effectcheck 0.2.1

## Bug fixes

* Fixed thousands-separator N parsing (e.g., N = 1,182 no longer misread as
  N = 1).
* Fixed Welch t-test N estimation.
* Removed officer dependency; DOCX extraction via pandoc only.
* Fixed DOCX segfault crash.

## Improvements

* Cold start progress indicator for Render free tier.
* Page-load health check and server status banner for classroom use.
* Paste-text input mode.
* Parser robustness: 7 fixes from AI verification testing across 12 journal
  styles.

## CRAN-related

* Fixed invalid URI, APA spelling, broadened package scope description.
* Fixed effectcheck namespace refs for shinyapps.io deployment.
* Removed pdftools dependency.
* Added development disclaimers throughout package and app.

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
