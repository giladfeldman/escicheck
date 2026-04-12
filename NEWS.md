# effectcheck 0.3.4

Addresses MetaESCI v0.3.4 request: 42 Category A ERROR false positives where
reported eta2/etap2 was cross-matched to cohens_f/cohens_f2 without detection.

## Check: Phase 8D Signal 14 — eta/f cross-family detection (E11)

* Phase 8D entry condition now includes `eta2`, `etap2`, `generalized_eta2`
  alongside the existing `R2`, `adjusted_R2`, `f2`, `cohens_f`.
* New Signal 14: when reported eta2/etap2 is matched to cohens_f/cohens_f2/f2
  with delta > 0.05, downgrades ERROR -> WARN with `r2_cross_pairing_detected`.
  Standalone (no contextual signals needed) — same rationale as Signal 13:
  both eta2 and cohens_f are deterministic from F, so any mismatch means the
  reported value came from a different analysis.
* Symmetric: also handles cohens_f reported + eta variant matched.
* Expected impact: MetaESCI Category A ERRORs 42 -> ~6, raw ERROR rate
  0.128% -> ~0.06%.

# effectcheck 0.3.3

Follow-up to 0.3.2 addressing MetaESCI v0.3.3 request: the E8 pre-strip
was a no-op on real docpluck output.

## Parse: thousand-sep comma strip now handles spaces after comma (E8 follow-up)

* The v0.3.2 regexes in `parse.R` required `t(2,758)` with no space —
  but docpluck v1.4.4's A4 paren-spacing normalizer always emits
  `t(2, 758)` with a space. The fix matched the pre-A4 raw text we'd
  been shown in the MetaESCI report, not the actual post-normalizer
  input. Net effect in v0.3.2: zero rows recovered on the PSPB article
  `10.1177/0146167220905712`.
* Fix: add `\s*` after the comma in all three pre-strip regexes
  (t/H/r/z, F, chi-square-N). Single-character change per regex.
* New tests: parallel with-space cases for all three statistic types,
  plus an end-to-end 5-line fixture copied verbatim from docpluck's
  output on `0146167220905712` (`t(2, 758) = -2.96, ...`).

# effectcheck 0.3.2

Follow-up to 0.3.1 addressing MetaESCI requests E8 and E10.

## Parse: thousand-separator commas in test-statistic parens (E8, HIGH)

* `parse.R` / `normalize_text()` previously let the decimal-comma
  converter mis-normalize `t(2,758)` as `t(2.758)`, after which `parse.R`
  silently read it as Welch df=2.758 and back-computed N≈5. In the
  MetaESCI 339-PDF pre-test, PSPB article `10.1177/0146167220905712`
  dropped 47 rows due to this, since every subsequent check treated the
  garbage df as genuine and the results were rejected downstream.
* Fix: `normalize_text()` now strips thousand-separator commas from
  inside `t(...)`, `F(...)`, `F[...]`, `H(...)`, `r(...)`, `z(...)` and
  `chi-square(df, N = ...)` parens *before* the decimal-comma converter
  runs, the same way `N = 1,234` is already pre-stripped.
* Handles `t(2,758)`, `F(2, 1,234)`, `F[1, 2,500]`, and
  `chi-square(3, N = 1,542)` — with an iterative pass so
  `N = 12,345,678`-style multi-comma numbers survive.
* New tests: `test-parse.R` now covers the t/F/chi-square cases and an
  end-to-end `check_text()` assertion that df=2758 round-trips.

## Compute: Cohen's dz CI uses noncentral-t inversion (E10, MEDIUM)

* `ci_dz()` / `ci_dz_all()` previously claimed a "noncentral_t" method
  but actually computed `qt(alpha/2, df, ncp = dz*sqrt(n)) / sqrt(n)` —
  i.e., quantiles of a single noncentral-t distribution, *not* the
  Algina & Keselman (2003) inversion. For small n this returned bounds
  that could be dramatically wrong: e.g., `dz=0.55, n=9` returned a
  one-sided-looking `[-1.66, 0.05]`-style interval instead of the
  correct `[-0.17, 1.24]`.
* Fix: new internal `ci_dz_noncentral_t()` uses `MBESS::ci.sm()` (the
  reference implementation of standardized-mean CI inversion) when
  available, falling back to a `stats::uniroot()`-based inversion that
  solves for the noncentrality parameters whose α/2 and 1−α/2 quantiles
  equal the observed t = dz·√n. The normal-approximation fallback is
  unchanged and still available when inversion fails.
* MetaESCI reported 20 divergent rows between the legacy
  `run_escicheck.R` pipeline and 0.3.1. Under 0.3.2 the new
  implementation agrees with MBESS on the fixture
  `dz = 0.55, n = 9, 95% CI`, which is what legacy `ci.sm` returned —
  so the 20 CI-width-ratio discrepancies should resolve.
* **Downstream impact**: any downstream consumer tracking
  `ci_match_rate` for Cohen's dz (and `ci_dz_all`) will see bounds
  shift. This is a correctness fix, not a silent behavior change —
  flag it in your analysis plan.
* New tests: `test-golden-exact.R` pins the `dz = 0.55, n = 9` fixture
  and adds sanity checks for `dz = 0, n = 20` (symmetric) and
  `dz = 0.5, n = 100` (narrow).

## Parse: decimal-comma no longer corrupts author affiliation markers

* `normalize_text()` previously fired the decimal-comma → decimal-dot
  conversion on author affiliation footnotes like `Braunstein1,3`
  (multi-affiliation) and `Wagner1,3,4`, rewriting them to
  `Braunstein1.3` / `Wagner1,3.4`. The corruption shifted context
  windows enough to flip at least one eLife t-test result from WARN
  to OK on a real paper.
* Fix: add a negative lookbehind `(?<![a-zA-Z,])` on both
  decimal-comma gsubs so a letter (or a preceding comma, for the
  middle of a 3-affiliation run) blocks the match. The trailing
  lookahead was also tightened from `[^0-9]` to `[^0-9a-zA-Z]` to
  block the `1,3Boryana` converse case. The second rule's leading
  quantifier was changed from `\d*` to `\d+` so the match is always
  anchored at a real digit, letting the lookbehind check the
  character before that digit rather than the character before the
  comma.
* 5 new tests in `test-extraction-quality.R` cover Braunstein/Wagner
  affiliation blocks, the `1,3Boryana` converse, the stat-expression
  case that must still convert (`d = 0,45`), and the
  thousands-separator-in-N regression guard.

## E9 — Smaller parse.R gaps (deferred, needs repro bundle)

* The 13-row residual across 5 PSPB/JESP/RSOS/MP sources needs the
  staged `.txt` files from MetaESCI's
  `data/results/subset_metaesci_regression_textstaging/` directory,
  which was not available at the time of triage. Will investigate when
  repro bundle is attached.

# effectcheck 0.3.1

This is a housekeeping release packaging the v0.3.0f → v0.3.0n bug-fix
wave with a stable CRAN-style version number, batch-stdout hygiene, a
schema stability test, and a new `decision_error_reason` diagnostic
column. Addresses MetaESCI requests E1–E4 and E7.

## DESCRIPTION version sync (E2)

* `DESCRIPTION Version:` bumped from `0.3.0` (which covered every build
  v0.3.0 → v0.3.0n) to `0.3.1`. Downstream pipelines can now
  discriminate the v0.3.0n bug-fix wave from earlier v0.3.0 builds via
  `packageVersion("effectcheck")` alone instead of requiring a git SHA.

## Batch stdout: noncentral-t overflow spam silenced (E1)

* `MBESS::ci.smd` (via `ci_d_ind_noncentral_t`) printed a multi-line
  warning to stdout every time the noncentrality parameter exceeded
  R's ~37.62 accuracy limit. At corpus scale this could print hundreds
  of lines per batch and drown out per-PDF progress output.
* Fix: preempt the overflow by routing `|ncp| > 37.62` directly to the
  large-sample normal approximation (which is no less accurate than
  MBESS's iterative fallback at that regime). Remaining MBESS calls
  are additionally wrapped in `utils::capture.output()` as a
  belt-and-suspenders silencer.
* Behaviour change: a small number of d-CI computations will now be
  tagged `method = "normal_approx"` instead of `"noncentral_t"`. The
  numerical difference is below the effect-size tolerance and does not
  affect PASS/WARN/ERROR status assignment.

## Schema stability test (E3)

* Added `tests/testthat/test-schema-stability.R`. The test asserts
  that `check_text()` returns a tibble containing every MetaESCI-
  critical column (`source`, `check_scope`, `check_type`, `status`,
  `uncertainty_level`, `uncertainty_reasons`,
  `unknown_groups_downgraded`, `r2_cross_pairing_detected`,
  `decision_error_downgraded`, `design_ambiguous`, `ci_match`,
  `ci_check_status`, `ci_method_match`, `ci_width_ratio`,
  `ci_symmetry`, `decision_error`, plus new
  `decision_error_reason`). An optional second check runs against a
  fixture PDF via the `EFFECTCHECK_TEST_PDF` env var and asserts the
  column set and element types are identical between `check_text()`
  and `checkPDF()`. By construction both paths funnel through
  `process_files_internal()` → `check_text()`, so this is an invariant
  guard against future regressions.

## New column: `decision_error_reason` (E7)

* Every row now carries a `decision_error_reason` character column.
  For rows where `decision_error == FALSE` the value is `NA`. For
  rows where `decision_error == TRUE` the value is one of:
    * `reported_sig_computed_ns` — reported p < alpha but recomputed
      p >= alpha (claimed significance does not reproduce).
    * `reported_ns_computed_sig` — reported p >= alpha but recomputed
      p < alpha (claimed non-significance does not reproduce).
    * `ns_label_vs_computed_sig` — paper reports "ns"/"not
      significant" but recomputed p < alpha.
    * `other` — catch-all for future decision-error variants.
* Downstream analysis (e.g. MetaESCI `analysis.Rmd`) can now break
  decision errors down by mechanism without reparsing `raw_text`.

## Expected row-count delta vs v0.3.0f (E4 — MetaESCI batch guidance)

On the MetaESCI `metaesci_regression` 200-PDF frozen benchmark (seed 42),
comparing v0.3.0f (last full batch) to v0.3.0n / 0.3.1:

| subset               | v0.3.0f rows | v0.3.0n rows | delta          | v0.3.0f ERRORs | v0.3.0n ERRORs |
|----------------------|-------------:|-------------:|---------------:|---------------:|---------------:|
| meta_psychology (139)|          464 |          464 |             0  |              0 |              0 |
| metaesci_regression  |        2,209 |        3,385 |  +1,176 (+53%) |             13 |              0 |

The +53% row-count delta on `metaesci_regression` is driven by
**parser gains**, not a config-default change (`plausibility_filter`
and `try_tables` defaults are unchanged). The new rows come from:

* Phase 7 multi-predictor regression rows that v0.3.0f short-circuited
  before v0.3.0m's multi-predictor-beta fix landed.
* F-test rows previously lost to the F ≈ 0 crash (fixed in v0.3.0n).
* z-test CI checks (1,517 previously UNVERIFIABLE rows now produce
  ci-check output, fixed in v0.3.0m).

Downstream consumers **must** re-derive all aggregate numbers from a
fresh v0.3.1 batch — old v0.3.0f aggregates are not directly
comparable. The 13 → 0 ERROR reduction on `metaesci_regression` is
real (v0.3.0n's F ≈ 0 crash fix + multi-predictor-beta fix), not
artefactual.

No columns were added or removed vs v0.3.0n other than the new
`decision_error_reason` column described above.

# effectcheck 0.3.0n

## Bug fixes (MetaESCI v0.3.0m batch deep-dive)

* Fixed ~100 F-test crashes (`'list' object cannot be coerced to type
  'double'`) for `F` near zero. The v0.3.0m defensive guard covered Phase 5
  matching but missed the Phase 6 CI-fallback path at `check.R:2809`, which
  extracted `computed_variants[[eff]]$ci` without a `tryCatch`. Now mirrors
  the guarded pattern already in use above.
* Fixed ~61 false positive ERRORs from multi-predictor regression rows
  where both unstandardized `b` and standardized `beta` are reported with
  different values (e.g., `b = 4.12, beta = 0.29`). v0.3.0m only detected
  the `b == beta` masquerade. effectcheck computes single-predictor
  `standardized_beta_from_t` which cannot match a multi-predictor reported
  beta — the comparison is now skipped with a "multi-predictor regression"
  uncertainty note instead of flagging ERROR.

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
