# effectcheck result columns

`check_text()` returns one row per detected statistic with a fixed column set. This page
describes every column. `NA` means "does not apply to this row" or "not found in the text";
where the difference matters, a companion column says which.

Columns are grouped by purpose; within a group they appear in table order.

## Identification

| Column | Type | Meaning |
|---|---|---|
| `location` | integer | 1-based index of the sentence-level chunk the statistic came from (renumbers if chunking changes) |
| `raw_text` | character | the verbatim fragment that was parsed |
| `context_window` | character | surrounding text used for design and sample-size inference |
| `test_type` | character | detected test; one of the `stats` values of `check_text()`, or `table_estimate` for a table cell with an effect/CI but no test statistic |
| `chisq_subtype` | character | for chi-square rows: `contingency`, `mcnemar`, `friedman` or `gof` |
| `source` | character | source label: `text` for `check_text()` input; `both` / `effectcheck_only` / `statcheck_only` after `compare_with_statcheck()` |
| `result_context` | character | `study` (a result), `method` (a sentence describing a planned analysis) or `table` (from `table_rows`) |

## Parsed values

| Column | Type | Meaning |
|---|---|---|
| `df1`, `df2` | numeric | degrees of freedom (df2 = denominator df of F) |
| `stat_value` | numeric | the test statistic |
| `N`, `n1`, `n2` | numeric | total and per-group sample sizes |
| `N_source` | character | where `N` came from, e.g. `own_clause`, `own_clause_denominator`, `local_context`, `extended_context`, `global_text`, `subgroup_sum`, `arm_totals_sum`, `chi_inline`, `chi_bare_n`, `corr_df_plus_2`, `df_inferred`, `omnibus_df_contrast`, `not_found` |
| `table_r`, `table_c` | numeric | rows and columns of a contingency table |
| `z_auxiliary` | numeric | a z value reported alongside a nonparametric statistic, used for r when group sizes are missing |
| `arm1_events`, `arm1_total`, `arm2_events`, `arm2_total` | numeric | per-arm counts for risk-ratio / risk-difference rows (`RR`, `rdpct`) |
| `b_coeff`, `SE_coeff`, `adj_R2` | numeric | unstandardized coefficient, its standard error, adjusted R² (regression) |

## Reported values

| Column | Type | Meaning |
|---|---|---|
| `reported_type` | character | canonical name of the reported effect size (`d`, `g`, `eta2`, `r`, …) |
| `effect_reported_name` | character | the label as written (e.g. `partial eta2`) |
| `effect_reported` | numeric | the reported effect size |
| `effect_fallback` | logical | the effect-size type was inferred rather than stated |
| `ci_level` | numeric | CI level used |
| `ci_level_source` | character | `explicit_with_bounds`, `inferred_from_context`, `assumed_95` or `implausible_level` |
| `ciL_reported`, `ciU_reported` | numeric | reported CI bounds |
| `ci_reported` | logical | both bounds were parsed |
| `ci_expected` | logical | the effect-size family is one for which a CI is normative reporting |
| `effect_reported_decimals`, `ciL_reported_decimals`, `ciU_reported_decimals`, `stat_value_decimals` | integer | decimal places as printed (trailing zeros kept) |
| `p_reported` | numeric | reported p-value |
| `p_symbol` | character | comparator as written (`=`, `<`, `>`, `ns`) |
| `p_valid` | logical | the reported p is a usable probability |
| `p_ns` | logical | p was reported as "ns" |
| `p_out_of_range` | logical | a p-clause in the row is not a usable probability (e.g. `p = 10`) |

## Type-matched comparison

| Column | Type | Meaning |
|---|---|---|
| `matched_variant` | character | the same-type computed variant closest to the reported value |
| `matched_value` | numeric | its value |
| `delta_effect` | numeric | absolute difference, reported vs matched |
| `closest_method`, `delta_effect_abs` | — | legacy aliases of `matched_variant` and `delta_effect` |
| `ambiguity_level` | character | how uniquely the value could be matched: `clear`, `ambiguous`, `highly_ambiguous` |
| `ambiguity_reason` | character | why; carries a stable tag `[category: structural-design]`, `[category: cross-family]` or `[category: not-computed]` when applicable |
| `design_ambiguous` | logical | `ambiguity_level != "clear"` |
| `all_variants` | character (JSON) | every computed variant with value, CI and metadata, split into `same_type` and `alternatives`; unpack with `get_variants()` |
| `variants_tested` | character | comma-separated list of computed variants |
| `design_inferred` | character | `independent`, `paired`, `one-sample`, `between`, `within`, `mixed`, `ambiguous` or `unclear` |
| `sign_mismatch` | logical | reported and matched values differ in sign (informational; magnitude is what is graded) |

## Computed effect sizes

Convenience columns alongside `all_variants`; `NA` when not computable for the row.

| Column | Meaning |
|---|---|
| `d_ind`, `d_ind_equalN`, `d_ind_min`, `d_ind_max` | Cohen's d for independent groups: with known n, assuming equal n, and the range over unknown allocation |
| `g_ind` | Hedges' g |
| `dz`, `drm` | paired d (difference-score SD) and repeated-measures d |
| `d_av_median`, `d_av_min`, `d_av_max` | d_av across the `paired_r_grid` correlations |
| `r_from_t_or_reported`, `r_ciL`, `r_ciU` | correlation (converted or reported) and its CI |
| `phi`, `phi_ciL`, `phi_ciU`, `V` | phi with CI; Cramér's V |
| `eta`, `eta2`, `partial_eta2`, `generalized_eta2`, `omega2`, `cohens_f` | ANOVA effect sizes |
| `standardized_beta`, `partial_r`, `semi_partial_r`, `cohens_f2`, `R2` | regression effect sizes |
| `rank_biserial_r`, `cliffs_delta`, `epsilon_squared`, `kendalls_W` | nonparametric effect sizes |

## p-value and decision error

| Column | Type | Meaning |
|---|---|---|
| `p_computed` | numeric | p recomputed from statistic and df |
| `decision_error` | logical | reported and recomputed p fall on opposite sides of `alpha` |
| `decision_error_reason` | character | why it was (or was not) called |
| `decision_error_downgraded` | logical | a decision error whose status was downgraded (ambiguous design, unknown groups, or R² cross-pairing) |
| `resampling_inference` | logical | the clause names a permutation / bootstrap / Monte Carlo procedure, so p is not recomputable and the p-check is suppressed |
| `resampling_method` | character | which keyword matched |
| `p_reported_is_resampling` | logical | the bound `p_reported` itself is the resampling p (not merely in a resampling clause) |
| `resampling_B`, `resampling_B_source` | numeric, character | stated resample count and where it came from (`own_clause`, `methods_prescan`) |
| `p_reported_secondary`, `p_secondary_symbol` | numeric, character | a second p of different provenance in the same clause (e.g. `P-permutation = 0.002`) |
| `resampling_p_below_floor` | logical | the reported p is below the smallest value the stated procedure can produce by counting (flag only, never ERROR) |

## Confidence-interval check

| Column | Type | Meaning |
|---|---|---|
| `ci_match` | logical | reported bounds match a computed interval within `tol_ci` |
| `ciL_computed`, `ciU_computed` | numeric | the closest computed interval |
| `ci_delta_lower`, `ci_delta_upper` | numeric | absolute bound differences |
| `ci_check_status` | character | `MATCH`, `PLAUSIBLE`, `INCONSISTENT`, `UNVERIFIABLE` or `MISSING` |
| `ci_method_match` | character | variant and method of the closest interval (e.g. `d_ind:noncentral_t`); a `:sign-aligned` suffix means it was direction-flipped to match the paper's convention |
| `ci_referent` | character | which estimate a regression row's reported CI is centred on: `b_coeff`, `standardized_beta`, `effect_reported`, `ambiguous_b_equals_effect`, `unknown` |
| `ci_width_ratio` | numeric | computed width / reported width |
| `ci_symmetry` | character | `symmetric` or `asymmetric` around the estimate |
| `ci_symmetry_class` | character | `symmetric_expected`, `asymmetric_expected`, `symmetric_unexpected`, `asymmetric_unexpected` |
| `ci_level_mismatch` | character | `match`, `90_vs_95_anova`, `implausible`, `unstated_assumed_95` |
| `ci_clipped_to_bound` | character | for bounded families: `none`, `lower_0`, `upper_1`, `both` |
| `sign_ci_violation` | logical | the estimate lies outside its own CI but its sign-flip lies inside — the signature of a dropped minus sign (flag only) |
| `estimate_outside_ci` | logical | the estimate lies outside its own CI without that signature (flag only) |

## Guards against impossible or implausible input

| Column | Type | Meaning |
|---|---|---|
| `extraction_suspect` | logical | the row looks like an extraction artifact (implausible magnitude, impossible value, rejected field) |
| `decimal_recovered` | logical | a dropped decimal point was recovered by testing decimal placements against the computed value |
| `effect_guard_rejected`, `effect_guard_reason` | logical, character | a reported effect size was present but withheld as impossible or implausible — different from "none reported" |
| `SE_guard_rejected`, `SE_guard_reason` | logical, character | a reported standard error was refused as impossible (negative) |
| `df_guard_rejected`, `df_guard_reason` | logical, character | a table row typed as a test that cannot exist (e.g. F with numerator df < 1); every number on it is withheld and the row is a NOTE |
| `df_arity_mismatch` | logical | the test label does not fit the df count (e.g. `F(48)`); computation is skipped |
| `upstream_sign_rewrites` | integer | document-level: sign rewrites the extractor reported making (`NA` = no report supplied; `0` = report supplied, none made). Read as a lower bound |
| `upstream_normalization_version` | character | document-level: the extractor's normalisation version |

## Verdict and uncertainty

| Column | Type | Meaning |
|---|---|---|
| `status` | character | `PASS`, `OK`, `NOTE`, `WARN`, `ERROR`, `SKIP` (see the README) |
| `check_type` | character | what drove the status: `effect_size`, `p_value`, `ci`, `none` |
| `check_scope` | character | `effect_size_checked`, `p_value_only`, `ci_checked`, `extraction_only` (or `error` for a row that failed before scoring) |
| `confidence` | integer | 0–10 heuristic confidence in the verdict (`min_confidence` filters on it) |
| `uncertainty_level` | character | `low`, `medium`, `high` |
| `uncertainty_reasons` | character | semicolon-separated reasons |
| `assumptions_used` | character | semicolon-separated assumptions |
| `insufficient_data` | logical | no variant could be computed |
| `unknown_groups_downgraded` | logical | a d/g ERROR downgraded because n1/n2 were unknown |
| `r2_cross_pairing_detected` | logical | an F-test ERROR attributed to an R² paired with the wrong F in a regression table |
| `repro_code`, `repro_output` | character | R code reproducing the computation, and its key output |
| `software_notes` | character | why values may differ across statistics software |
| `alternative_formulas` | character | other formulas a paper might have used |
| `best_practice_notes` | character | reporting suggestions |

## Deriving a "needs review" flag

A row is worth a human look when any of these hold: `decision_error`, `extraction_suspect`,
`insufficient_data`, `df_arity_mismatch`, `status %in% c("WARN", "ERROR")`, or
`ambiguity_level == "highly_ambiguous"`. `ec_identify(result, "all_problems")` covers most of
them.
