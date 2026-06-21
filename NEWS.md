# effectcheck 0.6.4

**Mode B docpluck table-row consumer (REQUEST_11 / docpluck v2.4.95).** `check_text()`
gains an optional `table_rows` argument that ingests docpluck's structured
`flattened_rows[]` (from `POST /api/extract?structured=true`, docpluck v2.4.95+) —
the typed table-cell statistics that have no inline APA form in the prose. This
captures the table-only results the 2026-06-16 canary audit flagged as PARSE-MISS
(deferred in v0.6.3 because the hosted API previously column-shredded tables).

- **Typed-key mapping, no sentence re-parsing.** A new internal
  `flattened_rows_to_parsed()` maps each row's `fields` to a parsed row by typed
  key only — `t` → t-test (with `df`, and Cohen's `d` when present), `F` →
  F-test (`df1`/`df2`), `r` → correlation (`N` from `n`). An effect family is
  never inferred from an untyped `est`. Rows are fed through the existing
  `compute_and_compare_one()` pipeline, so a verifiable row (e.g. an `r` with
  `n` + CI, or a `t` with `df` + `d`) is checked normally; a non-verifiable row
  is routed conservatively to NOTE.
- **`test_type = "table_estimate"`** — a row carrying only a point estimate + CI
  (and maybe p) with no test statistic cannot be recomputed, so it is surfaced as
  an honest extraction-only NOTE that reports the estimate / CI / p as extracted.
- **Provenance + dedup.** Table-derived rows are tagged `result_context = "table"`
  and deduplicated against any prose row that restates the same result (matched on
  the row's reported numeric signature), so a table cell echoing a body-text
  finding does not double-count.
- **Worker.** `worker/docpluck_client.R` now requests
  `?structured=true&sections=true` (default on; surfaces `flattened_rows` +
  `sections`), and `worker/plumber.R` passes `flattened_rows` to `check_text()`
  on `/process` and `/report`. The default no-flag call remains byte-identical.
- **Source-data caveats (docpluck v2.4.95 reply, not effectcheck defects):**
  `collabra.90203` Table 10 "Joint/No explicit" `r = .59` is a PDF text-layer
  mismatch (gold `.63`); the Table 3-vs-2 number attribution differs but values
  are correct. docpluck deferred the optional `fields.effect_type` to keep
  PROSECCO byte-identical, so partial-eta^2 estimates arrive untyped and are not
  bound as an effect (p is still verified from F + df).

Regression tests in `tests/testthat/test-v064-docpluck-table-rows.R`.

# effectcheck 0.6.3

Three fixes from the 2026-06-16 escicheck-iterate canary audit
(`docs/TRIAGE_iterate_2026-06-16.md`):

- **Clinical-trial N now sums the per-arm totals (E1).** For
  `test_type %in% {RR, rdpct}`, when both per-arm totals are parsed, `N` is
  their sum (`N_source = "arm_totals_sum"`) instead of a single arm picked up
  from `global_text`/`extended_context` — e.g. an RR with arms 106 + 101 now
  reports `N = 207`, not 106. `parse.R`'s `pat_two_props_slash` was also
  relaxed to allow a short alphabetic descriptor between the slash-count and
  the percent (`86/98 women (87.8%)`), so the PROSECCO primary-outcome
  risk-difference row binds its per-arm cells. Verified against the real
  PROSECCO PDF (`10.1371/journal.pmed.1004323`): all RR/rdpct N match the AI
  stats gold (205/207/207/145; rdpct 187). Tests in
  `tests/testthat/test-v063-e1-clinical-trial-N.R`.

- **Welch (non-integer df) no longer mis-tagged paired (E2).** A paired
  t-test has integer df (= n - 1), so a non-integer `df1` is a definitive
  Welch / independent-samples signal. `design_inferred` is reclassified
  `paired -> independent` whenever `df1` is fractional, with an explanatory
  `uncertainty_reasons` note. Tests in
  `tests/testthat/test-v063-e2-welch-design.R`.

- **Cochran Q accepts the flattened `QT [df]` form (E5).** PDF text
  extraction flattens the `Q_T` subscript to a glued `QT`, so `pat_cochran_q`
  now treats the subscript underscore as optional (`Q`, `QT`, `Q_T` all
  match). Tests in `tests/testthat/test-v063-e5-cochran-q-qt.R`.

- **CI binding is now position-aware; no more neighbour/table CI bleed (E3 +
  E4).** `parse.R` previously bound the *first* bracketed CI in a sub-chunk by
  pattern priority. When a docpluck-flattened table is interleaved between body
  sentences (E3) or an adjacent effect clause precedes the statistic (E4), the
  first bracket is a *foreign* CI and the row silently adopted it. The CI is
  now chosen by proximity to the row's effect-size value (`es_anchor`; for a
  correlation, the r-statistic position), preferring the bracket at/after the
  anchor — a single-CI sub-chunk is unchanged. The labeled patterns
  (`pat_CI1`/`pat_CI2`) also accept a `:`/`=` separator (`95% CI: [..]`,
  `95% CI = [..]`), which the colon form previously lost to the bare-bracket
  fallback.
  - **E3** (`10.1525/collabra.77859`): `t(133) = 4.44, dz = 0.38, 95%CI: [.21,
    .56]` no longer binds the interleaved Table-4 cell `[.50, 1.02]` (which had
    produced a spurious `INCONSISTENT`); it binds `[.21, .56]` (MATCH).
  - **E4** (`10.1525/collabra.57785`): the subsample correlation `r = -0.34`
    now binds its own CI `[-0.43, -0.24]` (not the adjacent `d = 0.39` clause's
    `[0.25, 0.54]`), and a new r-row dedup pass collapses correlation rows that
    report the same `r` with the same reported CI but a different `df1`
    (identical r + identical CI imply identical n, so a differing df is a
    global-N mis-bind), keeping the inline-df (`r(348)`) row. Result: exactly
    one `r = -0.34` row, `df = 348`, MATCH.
  - Both verified against the AI stats gold on the real PDFs (independent
    Sonnet audit, no NEW defects). Tests in
    `tests/testthat/test-v063-e3-ci-neighbour-bleed.R` and
    `tests/testthat/test-v063-e4-subsample-r-dedup.R`.

- **New `sign_ci_violation` column — dropped-minus sign-error detector, flag
  only (R-0007).** A reported point estimate must lie within its reported CI.
  When PDF extraction drops a leading minus glyph (e.g. `r = .74` reported with
  `95% CI [-0.92, -0.30]`, true value -.74), the estimate parses positive and
  lands outside its own CI while the sign-flip lands inside — a dropped-minus
  signature, and a sign error inverts the statistical conclusion. `check_text()`
  now flags this with a logical `sign_ci_violation` column and an
  `uncertainty_reasons` note. **Flag only:** the parsed value is never mutated
  (matching proceeds on the value as reported) — a deliberate conservative
  choice. Fires only for sign-bearing families (`d, g, dz, dav, drm, r, beta,
  partial_r, semi_partial_r`), only when exactly `-x` is inside the CI and `x`
  is outside (both-in / both-out is a different defect, left alone), with a
  rounding-aware epsilon. Lesson-transfer from docpluck's `W0g` recovery; logic
  independently Sonnet-audited. Tests in
  `tests/testthat/test-v063-ci-token-recovery.R`.

# effectcheck 0.6.2

Exact binomial test reported with Cohen's h. New `test_type = "binomial"`
matched via `pat_binom_h`, anchored on a "binomial p [op] <pval>" clause
followed (within ~80 non-period chars) by "Cohen('s)? h = <h>". When a
"<n> out of <N>" clause is present in the same verbatim, N is recovered
(`N_source = "binom_n_out_of_N"`) and check.R re-computes the two-sided
binomial p via `stats::binom.test()` assuming p_null = 0.5 (the most
common null in binomial-vs-chance reporting); the recomputed vs reported
delta appears in `uncertainty_reasons`. When N isn't recoverable, status
routes to NOTE -- the Cohen's h is accepted as reported.

Surfaced by the 2026-05-25 escicheck-iterate corpus expansion against the
CRSP decoy-effect papers (Xiao/Zeng/Feldman 2021 et al), where 2-5
binomial-with-h rows previously fell through to WEAK_GOLD or
OUT_OF_SCOPE. The NOTE-only template (LESSONS.md "NOTE-only test_type
template") was extended cleanly: parse layer adds the pattern + dispatch
branch, check.R adds a `tt == "binomial"` branch with conditional
recompute. A v0.6.3 follow-up could detect a stated null proportion ("vs
1/3 chance" etc.) to replace the p_null = 0.5 default.

Regression tests in `tests/testthat/test-v062-binomial-h.R` (7 cases:
full CRSP verbatim with N recovery, bare binomial+h with N=NA NOTE,
80-char-lookahead far-apart rejection, "h" without "binomial p" anchor
guard, chisq+h still routes to chisq, lowercase "cohen h" form, and
uncertainty-message contents when N is recovered).

# effectcheck 0.6.1

Bare `t = X, p [op] Y` (no df) extraction. Surfaced by the Lee-Feldman 2025
RSOS Newman-2014 RR replication during the 2026-05-25 escicheck-iterate
corpus expansion (24 occurrences in one paper's Tables 10-15: compact
`<label> M = m (sd), t = X, p < .001` form where df lives only in the table
header, not the immediate sentence). Before v0.6.1 such reports returned 0
rows from `check_text()`.

A new `pat_t_p_nodf` pattern matches `t = X` followed within ~80 chars by a
`p [<=>]` clause; `(?<![a-zA-Z])` keeps `dt =`, `pt =`, etc. from
false-positive matching, and the 80-char lookahead bound prevents a stray
`t = X` from being yoked to an unrelated downstream `p =` in long prose.
`df1` stays NA — check.R routes to status NOTE because the exact p-check
needs df. Dispatch position: AFTER `pat_t_nodf` (`t = X, df = Y` form keeps
priority and yields status=OK with full verification when df is present).

Regression tests in `tests/testthat/test-v061-bare-t-p-nodf.R`.

# effectcheck 0.6.0

Clinical-trial RR / rdpct / md_hl independent verification, completing the
v0.5.16/17/18 PROSECCO-trial test-type set. Closes the deferred v0.6.x
follow-through promised in the v0.5.16-18 NEWS entries.

## Verification (the v0.5.x NOTE rows now compute a comparison)

* **`RR`** -- when the per-arm slash-count clause
  (`<events1>/<total1> ... versus <events2>/<total2>`) is in the same
  sentence as the RR clause, `check_text()` computes
  `RR = (events1/total1) / (events2/total2)` independently and reports the
  reported-vs-computed delta + a Wald-on-log 95% CI in the row's
  uncertainty message. Fisher-exact / chi-square p-value verification
  remains future work (v0.6.x+).
* **`rdpct`** -- same per-arm cells produce
  `RD = 100 * (events1/total1 - events2/total2)` and a Wald 95% CI.
  Farrington-Manning iterative-MLE noninferiority p is honestly
  not-yet-wired and the message says so; the Wald approximation is
  suitable for sanity-checking the point estimate, not for
  noninferiority decisions.
* **`md_hl`** -- the Hodges-Lehmann point estimate cannot be recomputed
  from sentence-level text (needs per-arm rank data), so the row carries
  two sanity checks instead: (a) CI symmetry around the point estimate
  (asymmetric CIs are flagged: `|below - above| / width > 0.15`); (b)
  p-CI consistency (`p < .05 iff 0 outside the 95% CI`).
* **New per-row columns**: `arm1_events`, `arm1_total`, `arm2_events`,
  `arm2_total` -- the captured per-arm cells (NA for any row not parsed
  as RR or rdpct, or where the slash-count clause was absent). Additive
  schema change; does not break MetaESCI-critical columns.

Regression tests in `tests/testthat/test-v060-rr-rdpct-mdhl-verification.R`.
Closes the `2026-05-25-v06x-clinical-trial-compute-branches` handoff.

# effectcheck 0.5.18

Median-difference (Hodges-Lehmann) with IQR + CI (escicheck-iterate
cycle 8). Completes the PLOS Med PROSECCO-trial PARSE-MISS punch-list
opened in cycle 1.

## New test type

* **Median-difference (`test_type = "md_hl"`).** Parses Hodges-Lehmann
  median-difference reports of the form `median difference <val>; 95%
  CI <lo> to <hi>; p[-value]? = <pval>`. The HL estimate cannot be
  independently recomputed from a sentence-level extraction (needs
  per-arm rank data), so the row is captured as a NOTE for surface
  transparency. Regression tests in
  `tests/testthat/test-v0518-median-diff.R`. Caught by the 2026-05-23
  escicheck-iterate validation against the PROSECCO trial AI stats
  gold (10.1371/journal.pmed.1004323).

# effectcheck 0.5.17

Risk-difference percent with CI (escicheck-iterate cycle 7).

## New test type

* **Risk-difference percent (`test_type = "rdpct"`).** Parses
  clinical-trial noninferiority risk-difference reports of the form
  `risk difference <val>%; 95% [confidence interval (CI)|CI] <lo> to
  <hi>; ... P = <pval>`. Full Farrington-Manning noninferiority
  verification is deferred to v0.6.x; this cycle resolves the
  PARSE-MISS aspect so rows appear with status NOTE. Regression tests
  in `tests/testthat/test-v0517-risk-diff-pct.R`. Caught by the
  2026-05-23 escicheck-iterate validation against the PROSECCO trial
  AI stats gold (10.1371/journal.pmed.1004323).

# effectcheck 0.5.16

Clinical-trial risk ratio with two-proportion slash counts
(escicheck-iterate cycle 7).

## New test type

* **Risk ratio (`test_type = "RR"`).** Parses clinical-trial RR reports
  of the form `<n1>/<N1> (<pct>%) versus <n2>/<N2> (<pct>%) ... RR <val>;
  95% CI <lo> to <hi>; p[-value]? = <pval>`. The p-clause supports both
  `p = 0.15` and the operator-less `p-value 0.44` form common in PLOS
  Medicine / NEJM tables. Full verification of RR against per-arm cell
  counts is deferred to v0.6.x; this cycle resolves the PARSE-MISS
  aspect so the row appears with status NOTE (extracted but
  not-yet-fully-verified). Regression tests in
  `tests/testthat/test-v0516-rr-slash-counts.R`. Caught by the
  2026-05-23 escicheck-iterate validation against the PROSECCO trial
  AI stats gold (10.1371/journal.pmed.1004323).

# effectcheck 0.5.15

Cochran Q meta-analytic heterogeneity test (escicheck-iterate cycle-5, after
user scope decision 2026-05-24 to bring Q in-scope).

## New test type

* **Cochran Q (`test_type = "cochran_q"`).** Parses meta-analytic
  heterogeneity tests of the form `Q_T [40] = 104.65, p < .001` (optional
  subscript, brackets or parens for df). The Q statistic is chi-square
  distributed under the homogeneity null with the reported df, so the
  reported p-value is verified against `pchisq(Q, df, lower.tail = FALSE)`
  in the same dispatch path as Kruskal-Wallis H. No standard effect size
  is recoverable from Q alone; an uncertainty note records that I-squared
  (when reported) is not independently verified. Regression tests in
  `tests/testthat/test-v0515-cochran-q.R`. Caught by the 2026-05-23
  escicheck-iterate validation against the Identifiable-Victim AI stats
  gold (10.1525/collabra.90203, R03).

# effectcheck 0.5.14

Two narrow parse fixes from the 2026-05-24 escicheck-iterate cycle-4
validation against the Collabra canary.

## Parse fixes

* **Bayesian model-averaged estimates no longer inherit a global-text N.**
  A `r = 0.002 (95% CI [0; 0.004])` reported as the output of a RoBMA /
  Bayesian model-averaging / posterior-model-average analysis previously
  fell through the local -> extended -> global N cascade and picked up an
  unrelated paper's N from somewhere later in the text (producing a
  misleading `df1 = N-2, N = 1004` attribution on a model-averaged estimate
  with no recoverable per-study sample size). The cascade now recognizes
  "RoBMA", "Bayesian model-averaging", "model-averaged", "posterior model
  average", and "PMA" markers in the local + extended context and stops
  before the global fallback, leaving `N_source = "bayesian_model_no_n"`.
  Regression tests in `tests/testthat/test-v0513-bayesian-no-n.R`.

* **Table-fragment duplicates of body-text statistics now collapse.**
  Replication / extension papers commonly print a summary table that lists
  the same correlations / effect sizes already reported in the Results body.
  Each numeric appeared twice in the extracted output: once with the full
  parenthesized form (`r(741) = -.43, 95% CI [-.49, -.37]`) and once as a
  table cell (`r = -.43 [-.49, -.37]`). They are now collapsed to a single
  row by `(test_type, stat_value, df1, df2, N)` exact match, keeping the
  parenthesized body-text version. For r-rows, the missing df1 in the
  table-fragment row is normalized to N-2 before matching. Regression
  tests in `tests/testthat/test-v0514-dedup-table-fragments.R`.

# effectcheck 0.5.12

Recall fix for the Collabra / APA partial-eta-squared convention.

## Parse fixes

* **`pat_etap2` now recognizes the `eta^2p` / `eta^2_p` form** (subscript-p
  AFTER the squared symbol) in addition to the previously-supported `etap^2`
  form (subscript-p BEFORE). Caught by the 2026-05-23 escicheck-iterate
  validation against the AI stats gold: 13+ F-rows across two Collabra
  replications (Identifiable Victim, Experiential-vs-Material) dropped their
  reported partial-eta-squared point estimate (CI was captured, name + value
  null) because every Collabra paper writes `η^2p = .008` with the p
  trailing the caret-2. The point estimate now binds correctly; status
  upgrades OK → PASS once the reported effect matches the computed.
  Regression tests in `tests/testthat/test-v0512-etap2-caret-p-form.R`.

# effectcheck 0.5.11

Documentation-only release. The `design_ambiguous` output flag has always
combined two semantically distinct cases under one name; this release makes
the distinction explicit and parseable without changing behaviour.

## Documentation / output-string clarifications

* **`ambiguity_reason` now carries a stable bracket-tagged category suffix**
  when applicable: `"[category: structural-design]"` for the Phase 8A-bis
  paired-vs-independent case (a t / F(1,df) / z test reports d or g and BOTH
  the independent variant family and the paired variant family were
  computed), or `"[category: cross-family]"` when the reported ES type has
  no same-type variants in the computed-variants set (e.g. a Cohen's d
  reported on an F(2,df) omnibus, or ES type not specified at all). Existing
  reason substrings are preserved untouched (so downstream substring matches
  like the internal `"No same-type"` check continue to work); the tag is
  appended idempotently just before the output tibble is built. Consumers
  that want to programmatically distinguish the two semantics should grep
  the reason for the bracketed `category:` tag.
* **`design_ambiguous` flag semantics** are now documented end-to-end. The
  flag is intentionally broad (`ambiguity_level != "clear"`) and covers
  BOTH categories above; downstream consumers that only want the narrow
  paired-vs-independent meaning can filter on the new category tag.
  Documented in the `check_text()` `@return` block, in `API.md`, in the
  frontend `/api-docs` page, and in `LESSONS.md`. No code behaviour changed.
* **`@return` for `check_text()`** now enumerates the notable output columns
  inline (previously a single sentence "tibble with comparison results"),
  starting with `design_ambiguous`, `ambiguity_level`, `ambiguity_reason`,
  and `matched_variant`.

# effectcheck 0.5.10

Bare `r =` with a confidence interval — a parse fix found by escicheck-iterate.

## Bug fixes

* **A bare correlation `r = value` reported with a confidence interval but no
  p-value is now extracted.** The `r =` (no-df) pattern previously required a
  nearby p-value before it would emit a result — a guard against casual
  `r = .3` mentions. A correlation reported with a CI (e.g. `r = -.74
  [-0.92, -0.30]`) is a genuine result even without a p, so the guard now
  accepts *a p-value OR a confidence interval*, mirroring the chi-square
  (`p` or `df`) and Mann-Whitney (`p` or `z`) no-df guards. An explicitly
  labelled CI (`95% CI [...]`) always counts; a bare bracketed pair counts
  only when its bounds bracket the r value, so an unrelated bracketed pair
  (a page range, a citation index) is not mistaken for a CI.

# effectcheck 0.5.9

Chi-square `chi^2` caret token — a parse fix found by escicheck-iterate.

## Bug fixes

* **A chi-square written as `chi^2(df)` (the word "chi" with a caret
  superscript) is now parsed.** The chi-square token alternation was
  duplicated across four call sites — the sub-chunk splitter and `pat_chi` /
  `pat_chi_nodf` / `pat_chi_two_dfs` — and the copies had drifted: the symbol
  forms allowed an optional caret (`X^2`, and the Greek-letter form) but the
  word form only matched `chi2` with no caret, and the splitter copy lacked
  the precomposed superscript forms entirely. So `chi^2(1) = 3.74` returned
  zero statistics. The alternation is now hoisted to one shared `chi_tok`
  definition used by every chi path, so the accepted notations can no longer
  drift apart. No behaviour change for the previously-recognised forms
  (`chi2`, `chi-square`, `X2`, the Greek-letter and precomposed-superscript
  forms).

# effectcheck 0.5.8

Chi-square bare-`n` sample size — a parse fix found by escicheck-iterate.

## Bug fixes

* **A bare lowercase `n =` is now read as the total N for a chi-square** when
  no other sample-size token is present. `pat_N` deliberately matches only
  `N` / `nobs` because a bare `n =` is commonly a per-group size — but a
  chi-square reporting `chi2gof(1) = 31.01, p = ..., n = 329` (the JASP
  goodness-of-fit form) then had N come back NA and could not compute its
  effect size. A chi-square-scoped fallback now accepts a single bare `n =`
  as the total N, but only when the chunk carries no `n1` / `n2` per-group
  token and exactly one `n =` appears (two or more are per-group counts, not
  a total).

# effectcheck 0.5.7

DSCF (Dwass-Steel-Critchlow-Fligner) post-hoc W — a parse + categorisation fix
found by escicheck-iterate.

## Bug fixes

* **DSCF (Dwass-Steel-Critchlow-Fligner) post-hoc pairwise comparisons** —
  reported as `W = ...`, the post-hoc test following a significant
  Kruskal-Wallis — are now recognised. A negative DSCF W (`W = -3.84,
  p = .018`) returned 0 stats because `pat_W` and the sub-chunk splitter both
  rejected the leading minus; a positive DSCF W (`W = 5.99`) parsed but was
  mislabelled Wilcoxon's W. `pat_W` and the splitter now accept a leading
  sign, and a new `dscf` test type is assigned to a negative W, or to a W in
  an explicit DSCF / Dwass / Kruskal-pairwise context. No standard effect size
  is recoverable from the W statistic alone, so a DSCF result is an honest
  "cannot verify" NOTE — the same conservative route as Kendall's W, not the
  Wilcoxon-W mis-route it used to fall into.

# effectcheck 0.5.6

Bare regression-coefficient lines — a parse fix found by escicheck-iterate.

## Bug fixes

* **A bare regression-coefficient line** — `b = 0.45, SE = 0.12, p = .001`, the
  standard APA form for a coefficient with its standard error and p but no
  t-statistic written out — is now detected. effectcheck's regression-type
  promotion fired only when a t-test had already been parsed, so a bare `b` +
  `SE` had no test type to promote and the line returned 0 stats. When `b`,
  `SE` and a reported p all co-occur and no test statistic was parsed,
  effectcheck now creates a regression result and synthesises the coefficient
  t = b / SE; all three are required so an incidental `b`/`SE` co-occurrence
  cannot spuriously create a result. df is unknown (no test statistic was
  reported), so the row is reported as an honest NOTE.

# effectcheck 0.5.5

JASP "nobs" sample-size token — a parse fix found by escicheck-iterate running
effectcheck against the real-article AI gold corpus.

## Bug fixes

* **The JASP "nobs" sample-size token** (number of observations) is now
  recognised as the total N. A chi-square reporting `nobs = 659` had N come
  back NA, so the reported Cohen's w / Cramér's V could not be verified
  (status NOTE). `pat_N` now accepts `nobs` alongside capital `N`. A bare
  lowercase `n =` is deliberately still not matched — it is commonly a
  per-group size and would be mis-read as the total N.

# effectcheck 0.5.4

Regression-coefficient handling — a categorisation fix found by escicheck-iterate
running effectcheck against the real-article AI gold corpus.

## Bug fixes

* **A standardized regression coefficient (beta) reported on a plain t-test**
  is no longer cross-matched to a Cohen's d. `(β = 0.83, t(261) = 5.82,
  p < .001)` — a mediation / regression path coefficient — had its reported
  beta matched against the t-test's computed Cohen's d variants
  (`matched_variant = d_ind_equalN` / `gav` / `drm`), a meaningless
  cross-family comparison whose PASS/NOTE verdict depended on whether the beta
  value coincidentally resembled the computed d. A beta from a multi-predictor
  / mediation model is not recoverable from the t-statistic alone, so it is now
  left unmatched and reported as an honest NOTE — mirroring the Stage 1 Gap 3
  treatment of Cohen's h on a chi-square.

# effectcheck 0.5.3

Scientific-notation p-values — a parse fix found by escicheck-iterate running
effectcheck against the real-article AI gold corpus.

## Bug fixes

* **Scientific (E-notation) reported p-values** are now captured. A reported p
  written in E-notation — `p = 2.572e-08`, `p = 1.2e-3`, the form R / JASP /
  Python emit — was not parsed: `pat_p` requires a `[01].x` mantissa (so it
  rejects `2.572`) and `pat_p_sci` only handles the `p < 10^-N` form. 5 of the
  12 chi-square results in the gold for 10.1098/rsos.250367 carry an E-notation
  p, so effectcheck silently skipped a checkable p-value (status SKIP). A new
  `pat_p_enote` pattern captures the mantissa+exponent and converts it to a
  plain decimal; the reported p is now checked against the computed p.

# effectcheck 0.5.2

Subscripted chi-square notation — a parse fix found by escicheck-iterate
running effectcheck against the real-article AI gold corpus.

## Bug fixes

* **JASP-style subscripted chi-square** is now parsed. A chi-square written
  with a subscript label glued to the symbol — `chi2gof(2)`, `chi2Pearson(1)`,
  the form JASP emits — returned 0 stats: `parse.R`'s chi-square patterns
  required the open parenthesis to follow the chi token immediately, so a
  `gof` / `Pearson` word between them blocked the match. 7 of the 12
  chi-square results in the gold for 10.1098/rsos.250367 were invisible. An
  optional subscript group (an allowlist of gof / Pearson / Yates / LR / MH /
  Wald) is now accepted in `pat_chi`, `pat_chi_nodf`, `pat_chi_two_dfs` and in
  the sub-chunk splitter — the last so a paragraph of subscripted chi-squares
  splits into one result per statistic rather than collapsing into one row.

# effectcheck 0.5.1

Stage 1 validation fixes — four gaps found by validating the v0.5.0 Stage 1
coverage against six real articles (AI gold generated via the article-finder
skill).

## Bug fixes

* **One-sample t-test detection** (Gap 1) now covers the "mean vs a constant"
  family phrased with *than / from / compared to* — e.g. "higher than chance",
  "differed from the scale midpoint" — not only the "against chance / against
  the midpoint" forms. A one-sample t-test phrased "...were higher than chance"
  was still mislabelled `design_inferred = "independent"`, `matched_variant =
  "dz"`.
* **Kendall's W** (Gap 2) is no longer misparsed as Wilcoxon's W. The bare
  `W =` token is shared by Wilcoxon's W (a large rank-sum) and Kendall's W (the
  coefficient of concordance, bounded 0-1); a `W` in [0, 1] reported in a
  "Kendall" / "concordance" context is now classified as the new `kendall_w`
  test type and recognised as a `kendalls_W` effect size.
* **Cohen's h on a chi-square** (Gap 3) is no longer mis-matched to the
  contingency phi/V. A one-proportion / goodness-of-fit chi-square that reports
  Cohen's h as its effect size now yields an honest "cannot verify" NOTE — h is
  a function of two specific proportions and is not recoverable from the
  chi-square statistic alone.
* **Spearman confidence intervals** (Gap 4): a bare `r(df)` correlation now
  carries the Spearman (Bonett & Wright 2000) interval as an alternative method
  in the CI candidate pool alongside the Pearson Fisher-z interval. A Spearman
  correlation whose method was declared only in a distant Methods section no
  longer draws a spurious CI mismatch. No reclassification occurs, so papers
  mixing Pearson and Spearman are unaffected; the row stays labelled Pearson r
  and `ci_method_match` records which method matched.

# effectcheck 0.5.0

Coverage Stage 1 — closes effect-size / test-type gaps from the 2026-05-16
coverage roadmap (P1, P2, P3, P6, P7).

## New features

* **One-sample t-tests** are now labelled `design_inferred = "one-sample"` with
  a `d_onesample` matched variant. Previously a one-sample t-test was
  mislabelled `independent`/`dz` (the recomputed value was correct, since the
  one-sample d formula `t/sqrt(N)` coincides with `dz`, so only the labels were
  wrong).
* **Spearman's rho** and **Kendall's tau** are first-class test types. They are
  parsed from their symbol forms (`rho(df)=`, `tau(df)=`, Greek symbols) and an
  `r(df)=` reported in a Spearman/Kendall context is reclassified. Each gets a
  rank-appropriate p-value (Spearman: t-approximation; Kendall: normal
  approximation) and confidence interval (Spearman: Bonett & Wright 2000;
  Kendall: Fisher-z, Fieller et al. 1957) — never the Pearson path.
* **Chi-square sub-types** are detected (`chisq_subtype` column) and routed
  correctly: Friedman to Kendall's W, goodness-of-fit to Cohen's w, McNemar to
  an honest "cannot verify". None are silently given a contingency-table phi/V.
* **Cohen's h** is computed and verifiable from a two-proportion z-test.
* **Confidence intervals** now populate for omega-squared, partial
  omega-squared, epsilon-squared, Cohen's f-squared, adjusted R-squared, and
  Cohen's w.

## Internal

* New `chisq_subtype` output column.
* Strict `design_inferred` test assertions: a categorization regression to
  `"unclear"` now fails the test suite.

# effectcheck 0.4.2

## Bug fixes

* Correlation (`r`) parsing: a Cohen's-d-family token (`d`/`g`/`dz`/`dav`/`drm`)
  is now adopted as an `r`-test's reported effect size only when it appears
  *after* the `r` statistic (APA order: statistic, then effect size). A
  d-family token positioned *before* the `r` belongs to a preceding clause and
  is no longer conflated into the `r` result. Previously a two-analysis
  sentence such as an abstract's "...(d=0.39[0.25, 0.54]) ... (r=-.34[-.43,
  -.24])" produced a single row pairing the second clause's `r` with the first
  clause's `d`. A `d` co-reported with the `r` (`r(50)=.40, p=.003, d=0.87`) is
  still matched. Found by the escicheck-iterate corpus loop on Chen et al.
  (2023, Collabra).

# effectcheck 0.4.1

## Bug fixes

* t-test sample-size inference: a document-wide ("global text") N that is
  structurally incompatible with a t-test's degrees of freedom -- e.g. N = 608
  applied to a t-test with df = 364, where the design requires N = df + 2 -- is
  now overridden with the df-based N (df + 1 paired / df + 2 otherwise).
  Previously such a t-test kept the wrong N, producing a wrong recomputed d and
  a spurious WARN even when the reported effect size was consistent. The Welch
  branch already did this; the non-Welch branch only flagged it. Found by the
  escicheck-iterate corpus loop on Chen et al. (2021, JESP) Study 3 t-tests.

# effectcheck 0.4.0

## Breaking changes — extraction layer removed

All file-input functions are now `.Defunct()` and emit an error directing
callers to extract via [docpluck](https://docpluck.app) and pass the
resulting text to `check_text()`:

* `read_any_text()`
* `check_file()`, `check_dir()`, `check_files()`
* `checkPDF()`, `checkPDFdir()`
* `checkHTML()`, `checkHTMLdir()`, `checkDOCXdir()`
* `compare_file_with_statcheck()` — replaced by `compare_with_statcheck()`
  (text input)

The pure-text-analysis API (`check_text()`, `compute_and_compare_one()`,
the parsing layer, all effect-size and CI computations, and every output
column) is unchanged.

The package no longer requires `poppler-utils`, `tesseract`, `magick`, or
`qpdf` system dependencies. `SystemRequirements` field removed from
DESCRIPTION; corresponding entries removed from `Suggests`.

Migration: see <https://docpluck.app/api-docs> for the API contract.
Working R reference implementation in the ESCImate web-app repo at
`tests/scripts/docpluck_shootout.R`.

## New features (carried over from 0.3.6 deception-detection work)

* New per-row column `df_arity_mismatch` (logical, default FALSE) flags structurally
  malformed test statistics where the declared test label disagrees with the
  number of df arguments supplied — `F(48)` (F always takes two df), `t(36, 10)`
  (t always takes one df), `chi2(48, 14)` (chi-square takes one df), `r(50, 30)`
  (r takes one df). Such rows previously were silently dropped because the strict
  regex patterns rejected them; v0.3.6 emits the row with `df_arity_mismatch = TRUE`,
  `status = "NOTE"`, and an explanatory uncertainty message, while skipping all
  recomputation paths (`p_computed`, effect sizes, `decision_error` are all NA).

* New tier-5 verification fixture (`tests/testthat/test-deception-arena.R`)
  documents the ScienceArena `stats-extraction-v1` adapter contract: every row
  corresponding to a deceptive stat is flagged by at least one of
  `decision_error`, `extraction_suspect`, `insufficient_data`, `df_arity_mismatch`,
  `ambiguity_level == "highly_ambiguous"`, or `status %in% c("WARN", "ERROR")`.

## API documentation

* `API.md` documents `df_arity_mismatch` and adds a "Suspicion signals for
  downstream consumers" section listing the six row-level fields a benchmark
  adapter should OR together to derive `flagged_suspicious`.

# effectcheck 0.3.5

Addresses MetaESCI v0.3.5 request: CI-audit feature pack. Adds CI computation
coverage for previously-uncomputable effect-size families (OR, R², standardized
β, partial r, semi-partial r) and new per-row metadata for characterizing CI
reporting quality at scale (precision tracking, completeness flags, level
mismatch, bounded-parameter clipping, symmetry classification).

Purely additive — no v0.3.4 behavior changes.

## Compute: CI computation coverage gaps closed

* `ci_OR_all()` — odds-ratio CI via Wald-on-log(OR). Three sources for SE:
  (1) supplied `SE_logOR`, (2) Fisher exact CI from a 2×2 cell vector,
  (3) Wald inversion back-derived from a reported p-value when neither is
  available. Resolves MetaESCI 1A.
* `ci_R2_all()` — R² CI routed through `ci_etap2_all()` (R² ≡ partial η² in
  one-predictor / single-omnibus regression). Methods retagged with
  `_via_etap2` suffix so the matcher distinguishes R²-routed from native
  η²-routed CIs. Resolves MetaESCI 1B.
* `ci_standardized_beta_all()` — normal-approximation CI on standardized β.
  Uses supplied `SE_beta` when available, else back-derives from t-stat.
* `ci_partial_r_all()` and `ci_semi_partial_r_all()` — Fisher-z transform
  CIs for partial and semi-partial correlations. Resolves MetaESCI 1C.

## Parse: decimal-place precision tracking

* New helper `count_decimal_places()` extracts trailing-digit count from
  raw regex match strings *before* `numify()` (which loses trailing zeros).
* Four new output columns capture APA-7 precision: `effect_reported_decimals`,
  `ciL_reported_decimals`, `ciU_reported_decimals`, `stat_value_decimals`.
  Resolves MetaESCI 2A.

## Check: CI audit metadata (Phase 6)

* `ci_expected` (logical) — TRUE when row carries an effect size from a
  family for which CIs are normative reporting (d/g/r/η²/η_p²/R²/OR/V/φ).
* `ci_reported` (logical) — TRUE when both bounds parsed (F-test df
  artifact already excluded at parse time). Resolves MetaESCI 2B.
* `ci_level_mismatch` (character) — categorical `{match, 90_vs_95_anova,
  implausible, unstated_assumed_95, NA}`. Compares parsed level against
  the APA-95% canonical default. Resolves MetaESCI 2C.
* `ci_clipped_to_bound` (character) — `{none, lower_0, upper_1, both, NA}`
  for bounded ES families (η², η_p², R², ω², ε², generalized η², V, φ).
  Resolves MetaESCI 2D.
* `ci_symmetry_class` (character) — categorical refinement of the existing
  `ci_symmetry` ratio: `{symmetric_expected, asymmetric_expected,
  symmetric_unexpected, asymmetric_unexpected, NA}`. Resolves MetaESCI 2E.

## Frontend (escimate.app)

* CI block now renders `ci_width_ratio`, `ci_level_source`, the new
  `ci_level_mismatch` / `ci_clipped_to_bound` / `ci_symmetry_class` chips,
  a "CI expected, missing" badge, and an APA-7 precision row with
  precision-mismatch warning. Decision-error reason now appears as a
  tooltip on the badge. Downgrade-reason chips (`decision_error_downgraded`,
  `unknown_groups_downgraded`, `r2_cross_pairing_detected`) surfaced as
  inline status indicators instead of being hidden in raw metadata.
* Notes block surfaces `software_notes`, `best_practice_notes`, and
  `alternative_formulas` (previously visible only in the metadata panel).

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
