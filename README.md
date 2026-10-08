# effectcheck

> **Development version (Version 0.7.17).** The package is under active development and has not
> been fully validated. Verify results independently before using them in anything consequential
> (peer review, editorial or retraction decisions). Use is at the user's sole responsibility.
> Verification reports and bug reports are welcome at
> <https://github.com/giladfeldman/escicheck/issues>.

`effectcheck` is a conservative, assumption-aware **statistical consistency checker** for
already-extracted research-results text. It finds reported test statistics, effect sizes,
p-values and confidence intervals in text, recomputes what can be recomputed, and reports
whether the reported numbers are internally consistent — with every assumption it had to make
written into the output.

It is the checking engine behind the ESCImate web app (<https://escimate.app>).

## Contents

- [What it checks, and on what basis](#what-it-checks-and-on-what-basis)
- [Installation](#installation)
- [Quickstart](#quickstart)
- [Input: text, not files](#input-text-not-files)
- [check_text() arguments](#check_text-arguments)
- [Understanding a result](#understanding-a-result)
- [Working with results](#working-with-results)
- [Reports and export](#reports-and-export)
- [Comparing with statcheck](#comparing-with-statcheck)
- [Function reference](#function-reference)
- [Options and environment variables](#options-and-environment-variables)
- [Limitations and failure modes](#limitations-and-failure-modes)
- [How to cite](#how-to-cite) · [License](#license) · [Contributing](#contributing)

The full per-column reference for the result table is in
[`docs/output-columns.md`](https://github.com/giladfeldman/escicheck/blob/main/docs/output-columns.md).

## What it checks, and on what basis

For every statistic it recognises, `effectcheck`:

1. **Parses** the test statistic, degrees of freedom, sample sizes, p-value, reported effect
   size and confidence interval — across APA, Harvard, Frontiers, PLOS ONE, Scientific Reports,
   Nature Human Behaviour, PeerJ, eLife, PNAS and similar styles, with Unicode normalisation,
   locale-aware decimals and PDF-artifact handling.
2. **Recomputes** the p-value from the statistic and df, and **every plausible variant** of the
   effect size when the design is ambiguous (for a t-test: independent d, equal-n d, d bounds,
   Hedges' g, dz, dav, drm), rather than guessing one design.
3. **Compares like with like (type-matched comparison).** A reported d is graded only against
   d-family variants, never against g; other types are offered as *alternatives*, not verdicts.
4. **Checks the interval** against computed intervals (several methods) and checks structural
   invariants: the estimate inside its own CI, CI symmetry, CI level, bounds within the
   effect's mathematical range.
5. **Detects decision errors** — a reported p and a recomputed p on opposite sides of `alpha`
   (significance reversals, as in statcheck).
6. **Records uncertainty**: design inferred, assumptions used, and why a row could not be fully
   verified.

**Statistical coverage.** t-tests (independent, paired, one-sample); F / ANOVA (eta², partial
eta², generalised eta², omega², Cohen's f); correlations (Pearson r, Spearman ρ, Kendall τ);
chi-square (contingency, McNemar, Friedman, goodness-of-fit; phi, Cramér's V, Cohen's w, h);
z-tests; regression (standardized β, partial and semi-partial r, f², R², adjusted R²); ratio
measures (OR, RR, IRR, hazard ratios); nonparametric tests (Mann-Whitney U, Wilcoxon W,
Kruskal-Wallis H, Kendall's W, DSCF post-hoc; rank-biserial r, Cliff's delta, ε²); robust and
modern nonparametric tests (Wald-type and ANOVA-type statistics, Brunner-Munzel, Yuen); plus
extraction-only reporting of Bayes factors, mediation indirect effects and unstandardized mean
differences with a CI.

**Methodological basis.** Effect-size formulas and interval methods follow the *Guide to
Effect Sizes and Confidence Intervals* (Jané et al., 2024; doi:10.17605/OSF.IO/D8C4G).
Noncentral-t intervals for d and dz use `MBESS` when it is installed (otherwise analytic
approximations, and the method used is named in `ci_method_match`). Decision-error detection
follows the logic of statcheck (<https://CRAN.R-project.org/package=statcheck>).

## Installation

The development version (this repository) is the one documented here:

```r
install.packages("remotes")
remotes::install_github("giladfeldman/escicheck")
```

CRAN currently carries an older release (0.2.3, which still read files itself — see
[Input](#input-text-not-files)); `install.packages("effectcheck")` installs that.

Hard dependencies are common CRAN packages (stringr, stringi, dplyr, purrr, tibble, glue,
logger). Optional packages, used only when installed:

| Package | Used for |
|---|---|
| `MBESS` | noncentral-t confidence intervals for d and dz |
| `jsonlite` | `export_json()`, `get_variants()` and friends (parsing `all_variants`) |
| `statcheck` | `compare_with_statcheck()` |
| `rmarkdown` | `generate_report(format = "pdf")` |

## Quickstart

```r
library(effectcheck)

text <- "The groups differed, t(28) = 2.21, p = .035, d = 0.80, 95% CI [0.05, 1.54].
A one-way ANOVA showed an effect, F(2, 87) = 5.12, p = .008, eta2 = 0.11.
The correlation was moderate, r(98) = .34, p < .001, 95% CI [.15, .50]."

result <- check_text(text)
print(result)
summary(result)
table(result$status)

# the rows that need a human look
ec_identify(result, "all_problems")

# what was compared with what, for the first statistic
result[1, c("test_type", "reported_type", "effect_reported", "matched_variant",
            "matched_value", "delta_effect", "ci_check_status", "status")]

# write a self-contained HTML report and machine-readable exports
generate_report(result, out = file.path(tempdir(), "report.html"))
export_csv(result, file.path(tempdir(), "results.csv"))
```

## Input: text, not files

`check_text()` takes a character vector of **already-extracted text**. Since 0.4.0 the package
does not read PDF, DOCX or HTML files. Extract the text with any extractor (for example
docpluck, <https://docpluck.app>) and pass it in:

```r
paper_text <- paste(readLines("paper.txt", warn = FALSE), collapse = "\n")
result <- check_text(paper_text)
```

For end-to-end PDF/DOCX checking without writing your own extraction step, use the ESCImate web
app, which pairs `effectcheck` with a document extractor.

The former file-input functions are **defunct** and stop with a pointer to `check_text()`:
`check_file()`, `check_files()`, `check_dir()`, `checkPDF()`, `checkHTML()`, `checkPDFdir()`,
`checkHTMLdir()`, `checkDOCXdir()`, `read_any_text()` and `compare_file_with_statcheck()`.
Their old arguments (`path`, `paths`, `dir`, `subdir`, `pattern`, `try_tables`, `try_ocr`,
`messages`, `allowed_base_dirs`, `file`, `files`) are accepted only so that old calls reach the
defunct message.

Two optional inputs come from a structured extractor (docpluck's output shape):
`table_rows` (statistics read from table cells, checked through the same pipeline and tagged
`result_context = "table"`; rows from a grid no caption claims are dropped) and
`extraction_provenance` (the extractor's normalisation report, surfaced as
`upstream_sign_rewrites` and `upstream_normalization_version`).

## check_text() arguments

`check_text(text, stats, ci_level, alpha, one_tailed, paired_r_grid, assume_equal_ns_when_missing, ci_method_phi, ci_method_V, tol_effect, tol_ci, tol_p, messages, max_text_length, max_stats_per_text, cross_type_action, ci_affects_status, plausibility_filter, sign_sensitive, method_context_action, design_ambiguous_action, unknown_groups_action, min_confidence, table_rows, extraction_provenance)`

| Argument | Default | Meaning |
|---|---|---|
| `text` | — | character vector of extracted text |
| `stats` | all 29 types | test types to detect; a type left out is dropped from the result. Values: `t`, `F`, `r`, `chisq`, `z`, `U`, `W`, `H`, `regression`, `spearman`, `kendall`, `kendall_w`, `dscf`, `cochran_q`, `RR`, `rdpct`, `md_hl`, `binomial`, `interaction_p`, `mediation_indirect`, `mcnemar_or`, `bayes_factor`, `hazard_ratio`, `d_reported_only`, `wts`, `ats`, `brunner_munzel`, `yuen`, `mean_diff_ci` |
| `ci_level` | `0.95` | CI level assumed when the text does not state one (recorded as an assumption) |
| `alpha` | `0.05` | significance threshold for decision errors |
| `one_tailed` | `FALSE` | recompute p one-tailed |
| `paired_r_grid` | `c(seq(0.1, 0.9, by = 0.1), 0.95)` | assumed pre-post correlations for the paired dav/drm range |
| `assume_equal_ns_when_missing` | `TRUE` | split N equally when group sizes are not reported |
| `tol_effect` | `list(d = 0.02, r = 0.005, phi = 0.02, V = 0.02)` | per-type effect-size tolerance; a type not named falls back to a built-in per-type default (0.01–0.02 for most types), then 0.02 |
| `tol_ci` | `0.02` | tolerance on CI bounds |
| `messages` | `FALSE` | print progress messages |
| `max_text_length` | `10^7` | refuse text longer than this many characters |
| `max_stats_per_text` | `10000` | stop after this many statistics |
| `cross_type_action` | `"NOTE"` | status when only a cross-type comparison is possible |
| `ci_affects_status` | `TRUE` | a CI mismatch may downgrade PASS to NOTE (never raises a WARN) |
| `plausibility_filter` | `TRUE` | treat implausible effect sizes as extraction suspects, not errors |
| `method_context_action` | `"NOTE"` | status when the sentence reads as a method description, not a result (`"NOTE"`, `"WARN"` or `"SKIP"`) |
| `design_ambiguous_action` | `"WARN"` | status for an effect-size ERROR on a design-ambiguous t / F(1, df) |
| `unknown_groups_action` | `"WARN"` | status for a d/g ERROR when n1/n2 are unknown |
| `min_confidence` | `0` | drop rows whose `confidence` score (0–10) is below this |
| `table_rows` | `NULL` | optional structured table rows (see above) |
| `extraction_provenance` | `NULL` | optional extractor normalisation report (see above) |

**Accepted but currently without effect.** `tol_p`, `sign_sensitive`, `ci_method_phi` and
`ci_method_V` are accepted and recorded in the result's settings, but no computation reads
them: changing them does not change any value or status (verified by running `check_text()`
with each changed and comparing the full result). The p-value comparison uses fixed rounding
rules; phi and Cramér's V intervals always use the Bonett-Price method.

## Understanding a result

`check_text()` returns an `effectcheck` object: a tibble with **one row per detected statistic**
and a fixed set of columns (140 at this version; every one is described in
[`docs/output-columns.md`](https://github.com/giladfeldman/escicheck/blob/main/docs/output-columns.md)). The most used:

| Column | Meaning |
|---|---|
| `status` | final verdict (below) |
| `check_type`, `check_scope` | what drove the verdict: effect size, p-value, CI, or nothing (`extraction_only`) |
| `test_type`, `stat_value`, `df1`, `df2`, `N` | what was parsed |
| `reported_type`, `effect_reported` | the reported effect size |
| `matched_variant`, `matched_value`, `delta_effect` | closest same-type computed variant and its distance |
| `p_reported`, `p_computed`, `decision_error` | p-value check |
| `ci_check_status`, `ci_method_match`, `ci_unverifiable_reason` | interval check, the method it was compared against, and why an interval could not be compared (UNVERIFIABLE) |
| `uncertainty_level`, `uncertainty_reasons`, `assumptions_used` | what had to be assumed |
| `extraction_suspect` | the numbers look like an extraction artifact rather than an author error |

### Status

| `status` | Meaning |
|---|---|
| `PASS` | the reported effect size matches a same-type computed variant within tolerance |
| `OK` | the **p-value** is consistent; no effect size was checked (OK never vouches for an effect size) |
| `NOTE` | could not be fully verified: missing information, a CI mismatch downgrading a PASS, an extraction suspect, a cross-type-only comparison, or an extraction-only row that still carries a value worth showing |
| `WARN` | a moderate discrepancy (effect-size distance above 1× and up to 5× the tolerance), or a downgraded ERROR on an ambiguous design or unknown group sizes |
| `ERROR` | a large discrepancy (more than 5× the tolerance) |
| `SKIP` | extracted, nothing checkable and nothing worth surfacing |

All six are emitted; code consuming `status` must handle every one. `ci_check_status` grades the
interval separately: `MATCH`, `PLAUSIBLE`, `INCONSISTENT`, `UNVERIFIABLE` or `MISSING`.
`uncertainty_level` is `low`, `medium` or `high`. `ambiguity_level` is `clear`, `ambiguous` or
`highly_ambiguous`. `design_inferred` is one of `independent`, `paired`, `one-sample`,
`between`, `within`, `mixed`, `ambiguous` or `unclear`.

### Attributes

The object also carries attributes: `effectcheck_version`, `generated` (timestamp), `call` and
`settings` — every argument used, plus `n_table_rows` and `n_table_rows_uncaptioned_dropped`
(table rows received, and rows set aside because no table caption claimed them; both 0 without
`table_rows`). `summary()` reports them.

## Working with results

```r
summary(result)                          # counts by status, test type, uncertainty, design
plot(result, type = "status")            # also "uncertainty", "test_type", "delta", "all"
print(result, short = FALSE, n = 20)

get_errors(result); get_warnings(result); get_decision_errors(result)
ec_identify(result, what = "high_uncertainty")   # "errors", "warnings", "decision_errors",
                                                 # "high_uncertainty", "insufficient", "all_problems"
filter_by_test_type(result, types = c("t", "F"))
filter_by_uncertainty(result, levels = "high")
filter_by_delta(result, min_delta = 0.05, max_delta = Inf)
filter_by_source(result, files = "text", pattern = FALSE)
count_by(result, by = "status")          # "status", "test_type", "uncertainty", "design", "source"
is.effectcheck(result)
both <- rbind(result, check_text("z = 2.31, p = .021"))   # keeps the effectcheck class
result[result$status == "ERROR", ]                        # subsetting keeps the class too
```

Every computed effect-size variant for a row is stored as JSON in `all_variants`; these helpers
unpack it (they need `jsonlite`):

```r
get_variants(result, row_index = 1)              # list(same_type = ..., alternatives = ...)
get_same_type_variants(result, row_index = 1)
get_alternatives(result, row_index = 1)
cat(format_variants(result, row_index = 1, include_alternatives = TRUE))
compare_to_variants(result, row_index = 1)       # data frame: variant, value, delta, is_same_type, assumptions
get_variant_metadata(variant_name = "dz")        # name, assumptions, when_to_use, formula
get_effect_family(effect_type = "eta2")          # the family a reported type is graded within
```

## Reports and export

```r
generate_report(result, out = "report.html", format = "html", title = "EffectCheck Report",
                author = NULL, source_name = NULL, include_repro_code = TRUE,
                style = "beginner")        # or style = "expert"; format = "pdf" needs rmarkdown
render_report(result, out = "report.html")  # the older, simpler HTML table report
export_csv(result, out = "results.csv", na = "", row.names = FALSE)
export_json(result, out = "results.json", pretty = TRUE)
```

Each row's `repro_code` column holds R code that reproduces its computation; the HTML report
includes it when `include_repro_code = TRUE`.

## Comparing with statcheck

```r
comparison <- compare_with_statcheck(text)   # extra arguments go to check_text()
print(comparison)
```

Runs both tools on the same text and returns one table whose `source` column says
`both`, `effectcheck_only` or `statcheck_only`, with statcheck's own verdict in
`statcheck_error`. Without statcheck installed it warns and returns the effectcheck result
alone. The result has class `effectcheck_comparison`.

## Function reference

| Function | Purpose |
|---|---|
| `check_text()` | check text; returns an `effectcheck` object |
| `parse_text(text, context_window_size = 2)` | parse statistics without checking them (one row per candidate) |
| `compare_with_statcheck()` | run effectcheck and statcheck side by side |
| `ec_identify()`, `get_errors()`, `get_warnings()`, `get_decision_errors()` | select problem rows |
| `filter_by_test_type()`, `filter_by_uncertainty()`, `filter_by_source()`, `filter_by_delta()`, `count_by()` | filter and count |
| `get_variants()`, `get_same_type_variants()`, `get_alternatives()`, `format_variants()`, `compare_to_variants()` | per-row effect-size variants |
| `get_variant_metadata()`, `get_effect_family()` | what a variant or effect type means |
| `generate_report()`, `render_report()`, `export_csv()`, `export_json()` | reports and export |
| `is.effectcheck()` | class test |

S3 methods for the `effectcheck` class: `print.effectcheck`, `summary.effectcheck`,
`print.summary.effectcheck`, `plot.effectcheck`, `rbind.effectcheck`, `[.effectcheck`, and
`print.effectcheck_comparison` for the statcheck comparison.

Signatures (defaults as in the source):

```r
check_text(text, stats = <all 29 types>, ci_level = 0.95, alpha = 0.05, one_tailed = FALSE,
           paired_r_grid = c(seq(0.1, 0.9, by = 0.1), 0.95), assume_equal_ns_when_missing = TRUE,
           ci_method_phi = "bonett_price", ci_method_V = "bonett_price",
           tol_effect = list(d = 0.02, r = 0.005, phi = 0.02, V = 0.02), tol_ci = 0.02,
           tol_p = 0.001, messages = FALSE, max_text_length = 10^7, max_stats_per_text = 10000,
           cross_type_action = "NOTE", ci_affects_status = TRUE, plausibility_filter = TRUE,
           sign_sensitive = FALSE, method_context_action = "NOTE",
           design_ambiguous_action = "WARN", unknown_groups_action = "WARN",
           min_confidence = 0L, table_rows = NULL, extraction_provenance = NULL)
parse_text(text, context_window_size = 2)
compare_with_statcheck(text, ...)
ec_identify(x, what = c("errors", "warnings", "decision_errors", "high_uncertainty",
                        "insufficient", "all_problems"), ...)
get_errors(x); get_warnings(x); get_decision_errors(x)
filter_by_test_type(x, types)
filter_by_uncertainty(x, levels)
filter_by_source(x, files, pattern = FALSE)
filter_by_delta(x, min_delta = 0, max_delta = Inf)
count_by(x, by = c("status", "test_type", "uncertainty", "design", "source"))
get_variants(x, row_index = 1); get_same_type_variants(x, row_index = 1)
get_alternatives(x, row_index = 1)
format_variants(x, row_index = 1, include_alternatives = TRUE)
compare_to_variants(x, row_index = 1)
get_variant_metadata(variant_name)
get_effect_family(effect_type)
generate_report(res, out, format = "html", title = "EffectCheck Report", author = NULL,
                source_name = NULL, include_repro_code = TRUE, style = "beginner")
render_report(res, out)
export_csv(res, out, na = "", row.names = FALSE)
export_json(res, out, pretty = TRUE)
is.effectcheck(x)
print(x, short = TRUE, n = 10, ...)          # print.effectcheck
summary(object, ...)                          # summary.effectcheck
plot(x, type = c("status", "uncertainty", "test_type", "delta", "all"), ...)
rbind(...)                                    # rbind.effectcheck
```

## Options and environment variables

**No option and no environment variable changes what `effectcheck` computes.** Pass every
setting to `check_text()` as an argument.

For completeness, the source contains a dormant configuration reader that would look up
`effectcheck.tol_effect`, `effectcheck.tol_ci` and `effectcheck.tol_p` (or `EFFECTCHECK_TOL_EFFECT`,
`EFFECTCHECK_TOL_CI`, `EFFECTCHECK_TOL_P`), and logging helpers that read
`effectcheck.log_file` and `effectcheck.production_mode`. No exported function calls them in this
version, so setting any of these has no effect.

## Limitations and failure modes

- **Text only.** Results are only as good as the extracted text; a dropped minus sign, a merged
  table cell or a lost decimal becomes a wrong number. Such rows are flagged
  `extraction_suspect` where the numbers make it detectable, not always.
- **Design inference is heuristic.** When paired vs independent cannot be told from the text,
  all variants are computed and the row is marked `design_ambiguous`; a closest match is not
  proof of the design.
- **Missing sample sizes** are filled from df or from the surrounding text (`N_source` says
  which). Equal-n splits are an assumption and are listed in `assumptions_used`.
- **CI methods differ between software**; a CI mismatch is therefore only ever a NOTE, never a
  WARN. Read `ci_check_status` and `ci_method_match` for interval severity.
- **Some statistics cannot be recomputed** from a sentence (Bayes factors, hazard ratios,
  mediation indirect effects, resampling p-values): they are reported as extraction-only rows.
- **p-values from permutation or bootstrap procedures** are not recomputable from the statistic;
  `resampling_inference` suppresses the p-check for them.
- **Status thresholds are conventions**, not proofs of error: a WARN or ERROR is a reason to
  look, not a finding of misconduct.
- Four `check_text()` arguments currently have no effect (see above).

## How to cite

See [`CITATION.cff`](https://github.com/giladfeldman/escicheck/blob/main/CITATION.cff). In text:

> Feldman, G. (2026). *effectcheck: Statistical Consistency Checker for Published Research
> Results* (Version 0.7.17) [R package]. https://github.com/giladfeldman/escicheck

Please also cite the effect-size guide the computations follow (Jané et al., 2024;
doi:10.17605/OSF.IO/D8C4G).

## License

MIT — see [`LICENSE`](LICENSE).

## Contributing

Issues, verification reports and pull requests are welcome — see
[`CONTRIBUTING.md`](https://github.com/giladfeldman/escicheck/blob/main/CONTRIBUTING.md). This repository is published from the maintainer's
development tree; pull requests are applied there and then mirrored here.

## Related tools

- statcheck — recomputes p-values in APA-style text (<https://CRAN.R-project.org/package=statcheck>)
- effectsize — effect-size computation in R (Ben-Shachar et al., 2020; doi:10.21105/joss.02815)
- docpluck — PDF/DOCX text extraction for academic papers (<https://docpluck.app>)
