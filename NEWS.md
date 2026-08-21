# effectcheck 0.7.7

**A retry that skipped its own second chance, a test that guarded a branch
production never runs, and a comparison endpoint that quietly published NA.**
No parser, effect-size or verdict logic changed in this release: every fix is in
the worker's transport and provenance plumbing, plus the release gates that were
supposed to catch these and did not.

The occasion was the v0.7.6 consumer-readiness handoff, which listed three
changes that had shipped **asserted only by source grep** -- a `429` +
`Retry-After` retry whose retry never executed in any test, the removal of
`sections=true` proved by grepping our own source rather than the query string,
and `extraction_provenance` with zero worker tests on the one expression that
carries it to production. Closing those gaps found real defects behind two of
the three.

## Fixed

* **`429 -> 502 -> 200` returned the 502.** The 502 and 429 retries were two
  sequential `if`s with 502 first, so each condition was evaluated against a
  status the other retry had not produced yet. A 502 arriving as the answer to
  the 429-retry was returned unretried -- surfaced to the browser as HTTP 503
  after only two requests. It is not a corner case: docpluck emits 502 and 429
  for the same underlying reason (it is loaded), so the two arriving in one call
  is the likely shape. Now a `repeat` loop re-evaluates both conditions after
  every attempt, with a per-condition budget of one retry each. The bound is
  unchanged: at most two extra requests, at most `2 + retry_after_max_sec`
  seconds of sleep. Reproduced before fixing; the reverse order (`502 -> 429 ->
  200`) was already tested and passed against the bug, which is why both orders
  are now asserted.

* **`/api/v1/compare` published `NA` for both provenance columns** while
  `/process` and `/report` populated them from the same document.
  `compare_with_statcheck(text, ...)` splats `...` straight into `check_text()`,
  and that call site passed nothing. Found by a test that enumerates the router
  rather than a hand-written list of endpoints, so a fourth extraction endpoint
  is covered the day it is written.

* **A retry cap of `NA` crashed the extraction** instead of returning the honest
  429: `wait > retry_after_max_sec` is `NA`, and `if (NA)` is an error in R, not
  `FALSE`.

* **Stale documentation corrected.** The `structured` roxygen still claimed
  `?structured=true&sections=true`; v0.7.6 stopped sending `sections`. The
  `docpluck_extract()` return-shape contract omitted `retry_after`, added in
  v0.7.6 -- and because that contract test is network-gated and the v0.7.6 gate
  ran without a key, it skipped, and a skipped test reports exactly like a
  passing one.

* **`upstream_sign_rewrites` is now documented as a LOWER BOUND**, not a count.
  docpluck's `NormalizationReport._track` *assigns* rather than accumulates
  (`normalize.py:2686`), and three of its rules write that same key over one
  document, so when two fire only the last survives; the value is also a
  character-length delta rather than a count. Verified by reading the upstream
  source and filed to docpluck. Non-zero still reliably means "the extractor
  rewrote values here", which is what the column is for.

## Tests

Worker suite **46 passed / 2 skipped -> 145 passed / 0 skipped**. Every new
assertion was watched failing against a real mutation of the code it guards,
including reverting the retry to its previous sequential-`if` form (three tests
red) and removing the `NA` cap guard (R's `missing value where TRUE/FALSE
needed`).

The end-to-end provenance test now drives a **real multipart request through
plumber's own router** (`pr$call()`), so the CORS filter, body parser, route
match, handler and serializer all run. The previous version built `req$FILES` by
hand and passed -- and scanning plumber 1.3.3, webutils 1.2.2 and httpuv 1.6.17
symbol by symbol finds `"FILES"` **zero times**: nothing in the serving stack
sets it. That test was green on a branch production never takes.

Package suite unchanged at 1231 `test_that` blocks / 3719 assertions / 0
failures; no package logic changed in this release.

## Release gates

New `scripts/verify-upstream-and-consumers.mjs`, wired into all four project
skills, with checks for: an unread upstream docpluck outbox (a FAIL, not a
note -- three went unread for eight days and one broke production for seven);
corpus provenance and completeness; the symbol-contract canary; the consumer pin
table in the new `docs/CONSUMERS.md`; and a release notification for the version
being shipped.

Three of its own checks were false greens on first review and are fixed: `SKIP`
exited 0, the canary hash check matched the word "SHAPE" in a comment, and the
pin table was validated against itself rather than against each consumer's
source.

**All testing now runs against a LOCAL docpluck service.** The harnesses refuse
a non-loopback URL unless `--allow-remote` is passed, because `.Renviron` points
`DOCPLUCK_URL` at the metered hosted endpoint and any script that merely read
the environment billed production silently.

# effectcheck 0.7.6

**Two live defects that turned a real verdict green, and the six our extractor
filed against us that nobody had read.** docpluck had sent THREE outboxes since
v0.7.5 and only the newest reached this repository; the two older ones carried
the larger changes.

**A partial eta-squared stopped parsing on ~2026-08-14 and the row went GREEN.**
docpluck's symbol contract v2.0 (shipped v2.4.130) `_`-joins every subscript
run, so `eta2p` became **`eta2_p`** and `omega2p` became `omega2_p`. Neither was
in effectcheck's alternation. Measured on the released 0.7.5:
`eta2_p = .11` published `effect_reported = NA` at status **OK**, where the
identical `eta2p = .11` scored **ERROR** — the effect size was dropped AND the
verdict flipped from ERROR to a clean pass. `omega2_p` did the same from WARN.
Scope was measured rather than assumed: those two tokens are the ONLY ones that
broke; `eta2`, `eta2G`, `omega2`, `epsilon2`, `R2`, `f2`, `chi2` and both
`p < 10^-8` and `p < 10-8` were unaffected.

**Restored page boundaries reverted the v0.7.4 chunk fix.** docpluck v2.4.136
stopped destroying form feeds (its page-number strip used `\s`, which matches
U+000C, so it ate the page break beside the number it deleted). effectcheck had
no form-feed handling at all. Measured through `check_text()`: two results
separated by `\n\f\n` collapsed into ONE row carrying the first result's
`d = 0.33` against the second's `t = 7.47` — a pairing that appears in no
paper, and precisely the defect v0.7.4 shipped to remove. `normalize_text()`
also fabricated `dz = 3` from a page number, the v0.6.20 bridging class.

The fix DELETES the form feed early, before the whitespace collapse and both
number strips. Deletion rather than translation was measured, not reasoned:
`\f -> \n` turns the corpus-majority `\n\f` into `\n\n` and manufactures a
chunk split that never existed, and one cross-model reviewer recommended
exactly that. A form feed was never invisible here — PCRE `\s` matches it, so
the sentence splitter already broke on one and the `=`-joiner already bridged
across one. What it was not was a PARAGRAPH.

**Impossible values are now refused rather than computed with.** `pat_SE` has
always accepted a leading sign and nothing checked it, so a negative standard
error flowed into the `t = b/SE` synthesis and inverted the statistic — and
`verify_t_from_b_SE`, the one check that exists to validate that synthesis,
absolutises BOTH sides and is structurally incapable of noticing. A negative SE
is now rejected before the division, with `extraction_suspect` and a named
reason; the row is still emitted, because suppressing it would restore the
silent-loss class v0.6.20 removed. A **reversed reported interval**
(`ciL > ciU`) joins the impossible-value family, and the b-scale CI message no
longer claims a field is "absent" when it was present and refused. The docpluck
flattened-rows path now computes `p_valid` / `p_out_of_range` from the value
instead of hardcoding them.

The occasion is docpluck's rule W0g, which infers a missing minus from
arithmetic. Reproduced on THIS repository's own corpus by diffing docpluck
2.4.136 with the rule disabled: `frontiers_music_mood_2024` had `SE = 0.199`
rewritten to `-0.199` and `p = 0.069` to `-0.069`; `efendic_2022_affect` had
`[0.22, 0.75]` rewritten to `[-0.22, 0.75]` and `[0.04, 0.25]` to
`[0.04, -0.25]`. These guards are NOT about W0g and do not expire with it — a
standard error is non-negative and an interval is ordered, whatever produced
them.

**Upstream provenance is now visible.** `check_text()` gains
`extraction_provenance`; the worker forwards docpluck's `normalization` block,
which it had received all along and never passed on (`steps_changed` had zero
occurrences in this package before this release). Two new columns,
`upstream_sign_rewrites` and `upstream_normalization_version`, report whether
the extractor REWROTE VALUES in this document. Keyed on docpluck's metric key,
never on a rule name, so it degrades to a no-op when they delete the rule.
DOCUMENT-level and deliberately NOT wired to `extraction_suspect`: that flag
gates effect-size decimal REWRITING and two ERROR-path downgrades, so raising it
on every row because one span was rewritten would demote unrelated genuine
inconsistencies.

**The six defects docpluck filed against 0.7.5 on 2026-08-13**, every one
reproduced at HEAD before being touched and verified red on the released 0.7.5:
`AF [6, 7]` was rewritten into F-test notation because the bracket rule had no
letter lookbehind (an F-test fabricated out of prose); the outline stripper
**deleted a published value** (`90.6 Third-plus generation ...`) it could not
tell from a heading; `v2.1.451,52.` fused into `v2.1451.52.`; `9999999,1`
converted as a decimal; a space-separated affiliation run became `Frank 1.2`;
and `~25%6,28` became `%6.28`.

The seventh is the one worth repeating. docpluck read `scored 0,87` failing to
convert as a **false negative in our `coded|dummy|scored` vocabulary**, and was
about to adopt that vocabulary because of it. The vocabulary was never the
cause: each element of the protected run is a single `\d`, so the pattern
matched the PREFIX `0,8`. Measurement stopped a sibling project adopting a
mechanism for a reason that was not true.

Two of these fixes needed a second pass, both caught by testing rather than by
reading the pattern: the first outline-stripper guard still deleted values, and
the first affiliation guard used `\s*,` and therefore also protected
`Median 0,45, SD 0,12`, suppressing a real European decimal.

**Normalization spec 1.4.0 -> 1.5.0, 72 -> 80 conformance cases**, and its
status changes. docpluck DELETED its entire EU->US separator machinery (`A3`,
`A3a`, `A3c`, `A3d`, `A2`, `W0n`, and the whole document-level locale feature)
in v2.4.129-v2.4.130 — verified against the live library, which now delivers
`d = 0,80`, `U = 12,345` and `N = 185,178` verbatim. effectcheck is therefore
the ONLY implementation, the spec is no longer a two-implementation contract,
and `REQUEST_TO_DOCPLUCK_normalization_spec.md` is moot rather than pending. One
consequence is the opposite of what was feared: v0.7.5's locale inference now
works BETTER on docpluck output, because docpluck no longer touches the tokens
it votes on.

**Cost and resilience.** `sections=true` is no longer requested — it has been
sent on every extraction since v0.6.4 and consumed by nothing. A `429` is now
retried once, bounded, honouring `Retry-After` (which had zero occurrences
anywhere in this repository). `/health` reports the docpluck release behind the most recent result, and it reports the one that can be trusted. `metadata.docpluck_version` resolves from an environment variable inside docpluck's own frontend and fails two ways: UNSET gives the literal string `"unknown"` (measured against a local instance that was running 2.4.136), and STALE gives a real-looking wrong number — production reported `"2.4.101"` while returning `normalization.version = "1.9.57"`, which is 2.4.136's normalization version. The first draft of this guard excluded only `"unknown"` and therefore published the stale 2.4.101 as fact. **It was caught by the post-deploy probe, not by any local gate** — no fixture can contain a value only production knows. `normalization.version` is computed from the library and cannot drift from it, so it is authoritative; the self-reported string survives as a labelled fallback (`self-reported-<x>`) for the case where no normalization report exists at all.

**Corpus evidence.** Whole-corpus diff over 37 real-article texts, all v0.7.6
parser changes against unchanged input: **0 rows gained, 0 lost, 2 verdict
changes** — both the new reversed-CI guard firing on genuinely reversed
published intervals, and the 2026-08-13 fixes adding no further change at all.
One was confirmed against the rasterized source page: Frontiers in Psychology
10.3389/fpsyg.2024.1303262 p6 prints
`(r = 0.195, p = 0.240, CI 95%[0.485, 0.132])`. That row previously passed.

Known limitation, disclosed rather than discovered later: the regression corpus
was found to be **raw pdftotext, not docpluck output** — 38 of its 48 files came
from the pre-v0.4.0 `read_any_text()` path, so every whole-corpus diff quoted
for v0.7.4 and v0.7.5 was measured on text production does not consume.
Re-extraction through the production HTTP path is underway and tracked
separately; this release does not claim it is finished.

# effectcheck 0.7.5

**A backlog item that would have shipped a feature doing nothing, a p-value
scraped off a figure legend, and a locale signal three rules computed and none
read.** v0.7.5 closes the 2026-08-09 handoff. Two of its defects were found by
*checking the premise before writing the code*, and one by verifying a fix
against the article text rather than against the test that had just gone green.

## Issue C -- the resample count B, and the feature that would have been inert

The Monte Carlo floor check shipped in v0.6.22 (`p >= 1/(B+1)`, Phipson & Smyth
2010) had **never fired on any paper**. Measured across the 48-file validation
corpus before any change: ten rows carried a `resampling_method` and
`resampling_B` was non-NA on **zero** of them. `resampling_p_below_floor` was
FALSE everywhere -- not because every paper passed, but because the check had no
B to test against. *A check that cannot fire is indistinguishable from one that
passes.*

The handoff attributed this to B being declared once in Methods, and prescribed a
Methods prescan. That is half of it. The other half only appears by reading the
paper: PNAS 10.1073/pnas.2404157121 does not write `B = 10,000` anywhere. It
writes "For permutation tests, **ten thousand** random shuffles of labels ... were
sampled" -- the count is SPELLED OUT, and a qualifier sits between it and its
noun. A prescan for the digit form would have added a helper, a provenance value
and a test suite, and still bound nothing on the one paper it was written for. Of
the four resample-count declarations in the corpus, the previous clause-level
scan could read exactly one.

So the fix is a document-level Methods prescan (`.doc_resampling_b()`) **plus** a
bounded number-word reader, `bootstrap\w*` on the qualifier, and a generic noun
admitted when a resampling word is elsewhere in the same sentence. New column
`resampling_B_source` (`own_clause` / `methods_prescan`), and every message that
quotes B now says which. The PNAS paper's six resampling rows carry B = 10000 and
the floor check is live.

**The first draft of this feature shipped the exact defect the feature exists to
prevent**, and the corpus scan caught it. `brjpsych_1.txt` contains
`(b=0.81, z=2.80, p=0.005, OR=2.25, ...)`; the case-insensitive `B = <num>` form
matched `b=0.81` and then stripped the `.` -- correct for a thousands separator,
catastrophic for a decimal -- giving **B = 81**. A floor of 1/82 = 0.0122 would
have declared every p below .0122 in that paper unattainable. Three guards now:
case-sensitive uppercase `B`, an integral-shape requirement so `0.81` and `2.25`
cannot pass, and a resampling word required in the same sentence.

A whole-document scan was measured against the Methods-scoped one and **refused
on the evidence**: it gains one correct bind (collabra.126266, whose declaration
genuinely sits in Results) and two wrong ones, including `B = 60` scraped off a
**grading scale** (`Grade A+=80% or above, A=70-79%, B=60-69%`). The positional
scope pays for itself. collabra.126266 stays uncovered, deliberately, with the
reason recorded.

## Issue D -- the permutation p was being discarded

A clause can report two p-values of different provenance:
`t(2037) = -3.26, P = 0.001, P-permutation = 0.002`. `pat_p` binds the parametric
0.001 -- it cannot see the hyphenated form at all -- and 0.002 was thrown away, so
a reader of the output could not tell that a permutation p had been reported.

New sibling **column** `p_reported_secondary` (plus `p_secondary_symbol`), never a
second row: a new row would change `nrow()` for every consumer and silently shift
every downstream index, and MetaESCI's field registry is frozen at v0.4.0, so it
already tolerates unknown columns and cannot tolerate unknown rows. Scoped to the
GLUED qualifier only, which is the whole safety argument -- a glued qualifier is
*provably* invisible to `pat_p`, so what this captures is provably not what
`pat_p` bound. A spaced qualifier may already BE the primary, and capturing it
could publish the same number twice under two provenances.

The floor / missing-B / Monte-Carlo-SE caveats now retarget to whichever p came
out of the resampling distribution, and name it ("Reported permutation p (0.002)
is below the minimum attainable ..."). Previously the entire block was skipped on
exactly these rows.

## A p-value scraped off a figure legend -- found by checking the source

Verifying Issue D against the article text turned up a defect in the same paper.
The PNAS figure caption is merged into the body by the extractor, ends
`...***P < 0.001.`, and continues in LOWERCASE, so the chunk splitter -- which
needs a capital or a digit -- cannot separate them. `pat_p` took the FIRST p in
the merged chunk and the row published `t(2037) = -2.19, p_reported = 0.1`, status
WARN, where the paper prints **P = 0.029** (and `2*pt(-2.19, 2037) = 0.02864`, so
the correct value is consistent and the row is OK). A threshold from an asterisk
key, attached to a real published statistic, with no flag.

`pat_p` now refuses a p preceded by a legend marker (`*`, `~`, `+`, `#`, either
spacing). Fixed at the p-binding rather than at the chunk boundary deliberately:
a boundary rule would have to guess where the caption ends, while this states
something simply true, and it cannot separate a statistic from its own values
(invariant 6). 80 occurrences across 12 of the 48 corpus papers.

**Whole-corpus diff: 0 rows gained, 0 lost, THREE changed -- all three this same
defect in three different papers**, each new value checked against the article:

| paper | was | is | legend |
|---|---|---|---|
| pnas_cognitive_memory_2024 | `p = 0.1` (WARN) | `p = 0.029` (OK) | `~P < 0.1` |
| frontiers_retrocue_2024 | `p = 0.05` | `p = 0.271` | `*p<0.050` |
| scireports_exercise_2025 | `p = 0.01` | `p = 0.021` | `# P<0.01` |

## Locale `conflict` was computed by one function and read by none

`infer_numeric_locale()` returns `decisive` / `none` / `conflict` and sets
`decimal_mark = NA` for **two** of them. Three separate rules gated on
`identical(decimal_mark, ",")`, which collapses `conflict` into `none`, so a
document that actively contradicts itself was normalized as if it were decisively
US. Reproduced: a document mixing `p = .035, d = 0.80` with
`Welch's correction gave t(2,758) = 3,21, d = 0,45` stripped the comma and
published `df = 2758`, `N = 2760` and a **computed** `d = 0.122` against a
reported `0.45` -- a false WARN carrying a fabricated effect size on a correctly
reported result. Under the decisive-European branch the identical string yields
"cannot verify" with no computed value; `conflict` now reaches that same outcome.

The third of those rules is one the v0.7.3 cross-model audit had **already fixed
for the European case** -- by writing a fourth hand-maintained copy of the same
test, which is precisely why the third state got past all of them. They now share
`.locale_comma_unresolved()`. Corpus diff for this change alone: **0 of 764 rows
change**, because 0 of 48 papers infer `conflict`. That emptiness is the evidence
the change is safe, not a reason it was skippable -- a computed signal nothing
reads is a trap, because it reads as handled at every site that mentions it.

Shared normalization spec **1.3.0 -> 1.4.0**, 70 -> 72 conformance cases (new rule
`L3`). SPEC.md's own version stamp had been stale at `1.0.0` for three minor
versions; corrected.

## New test type `mean_diff_ci` -- estimate + CI + p, no test statistic

`ieee_access_alt` reports three results as `Mean difference of 457.66 articles,
p-value = 7.171e-11, confidence interval (320.98, 594.35)` and produced **zero
rows**: every pattern anchors on a test statistic or a standardized effect size
and this clause has neither. The triple is nonetheless mutually checkable, and
`md_hl` already establishes that a row may carry CI-symmetry and
p-CI-consistency checks while claiming no effect size. Scope confirmed with the
user before implementing.

Three checks, and **no effect size is ever claimed** (a mean difference "in
articles" has no standardizer in the clause; inventing one would be the
cross-scale defect `ci_referent` exists to prevent):

1. the estimate must be the **midpoint** of its own interval -- a
   normal-approximation CI is symmetric by construction, unlike a
   Hodges-Lehmann one;
2. the interval implies `SE = (U - L)/(2z)`, and SE plus the estimate imply a
   p-value, compared on a **ratio** -- these p-values are of order 1e-11, where
   any absolute tolerance waves everything through (the v0.6.18 lesson);
3. p and the interval must agree on the significance decision.

The correctly reported paper is NOT flagged: implied 5.291e-11 against a reported
7.171e-11, a factor of 1.36.

Two supporting fixes, both general rather than scoped to the new type:

- **`p-value = <e-notation>` parsed nothing.** `pat_p_enote` required the operator
  directly after `p`, so `p-value = 7.171e-11` published `p_reported = NA` with
  "not a valid probability (outside [0,1] or unparseable)" -- about an ordinary
  probability written the way IEEE and the clinical journals write it. Fixed in
  the shared pattern, so every test type gets it.
- **Consecutive bulleted results were one chunk.** The splitter needed a capital
  immediately after the whitespace and a list marker sits in between, so only the
  first statistic in a bulleted list was ever extracted. The marker set includes
  `U+FFFD`, because that is what the extractor actually delivers here -- the
  bullet glyph is already lost upstream, and a rule that only knew `U+2022` would
  not fire on the real text. The `(?<=[.!?])` anchor is kept, so invariant 6
  holds.

Corpus diff: **0 rows lost, 0 changed, 3 gained**, all in this previously
zero-row paper.

## Build determinism (P0 from the 2026-08-07 portfolio survey)

`Dockerfile:1` was the only literal `:latest` base image in the portfolio, and
`effectsize` and `MBESS` -- which compute the effect sizes and confidence
intervals -- installed from a moving repository. The same commit could publish a
different CI on two different days with nothing to attribute the change to.

- Base image **pinned by digest**, verified two ways: it is what Docker Hub's
  `:latest` currently resolves to AND what the live Render build resolved for the
  image serving production. The pin is a no-op for behaviour.
- CRAN **pinned to a dated snapshot**. Not via `ENV CRAN=` -- the base image bakes
  its repository into `Rprofile.site` as a literal at image-build time
  (rocker-versioned2 `setup_R.sh:34`, read at the pinned revision) and never
  re-reads the variable, so that would have looked like a pin and changed
  nothing. The date is deliberately not "today": p3m.dev publishes a day in
  arrears and `.../2026-08-09` returned 404 while `2026-08-08` returned 200.
- Every `repos=` override removed, so there is one source of truth. The previous
  explicit `cloud.r-project.org` also silently bypassed the image's own binary
  repository, so those packages compiled from source while
  `remotes::install_local` used a different, binary one. Two repositories, one
  image.
- `/health` now reports `engine_versions` (R, platform, effectsize, MBESS,
  stringi, stringr, ICU). Pinning makes the number stable; recording makes a
  change attributable, and neither substitutes for the other.
- `worker/requirements.txt` deleted -- referenced by no Dockerfile or workflow,
  and it still listed `tesseract`, removed in v0.4.0.

**The Docker build is not verified locally** (no Docker daemon available in this
session). Every input was verified against its primary source -- the registry
digest, the p3m snapshot URL, and the base image's own `setup_R.sh` -- but the
first real proof is the deploy, and the version gate below is what will surface a
failure instead of hiding it.

## Deploy and CI

- **A push is not a deploy.** The 2026-08-08 v0.7.3 deploy is recorded on Render
  as `update_failed`: the Docker build SUCCEEDED in 78 seconds, then the container
  never bound its port, Render timed the deploy out after 15 minutes and rolled
  back to the v0.6.20 image -- which then answered `{"status":"healthy"}` for 15
  hours. **The `:latest` base image was NOT the cause** (the build was cache-warm
  and the digest resolved fine); the leading hypothesis in the handoff is refuted
  by the build log. Also note the five "lost releases" were ONE push: 0.6.21
  through 0.7.3 are NEWS entries inside commit `07c4822`.
- `scripts/verify-deployed-version.mjs` polls `/health` until the committed
  version is served (exit 0 landed / 1 drift / 2 unverifiable -- the same contract
  as the one-shot `deploy-drift-check.sh`, which answers a different question).
  Both read `.claude/deploy-targets.json`, and a release contract now fails if
  either drifts from it.
- `npm audit` moved AFTER the browser E2E gate. It ran before `playwright
  install`, so two HIGH transitive advisories turned the job red and GitHub
  SKIPPED the E2E suite -- it had not run in CI for at least two releases, behind
  a single red X that read as one dependency problem. Pinned by a release
  contract.
- The 4 remaining MODERATE advisories are **cleared**, and not by the upgrade the
  previous note assumed: they traced to our own `overrides` block pinning
  `postcss` to `8.5.21`, which is *below* the advisory's fix line (`<=8.5.22`). A
  July 2026 security pin had outlived its reason and become the cause. Bumped to
  `8.5.26`; `npm audit` reports 0 vulnerabilities, `next` untouched.

## Filed to docpluck, not fixed here

**DP-14** (`docs/DOCPLUCK_HANDOFF_2026-08-09.md`): `bmj_1`'s supplementary table
arrives **column-major** -- one cell per line, no row grouping, and a header split
mid-word (`Odds Ratio or Coefficien` / `t for Treatmen t Group`). Reassembling it
means guessing which estimate pairs with which interval, and a wrong guess there
is not a missing row but a **fabricated** one -- the v0.7.4 two-column defect and
DP-11 both. ESCImate correctly renders nothing rather than something plausible
and wrong.

## Cross-model review — 12 findings, 8 confirmed and fixed, 4 refuted

Codex (gpt-5.5) and Claude Sonnet reviewed the diff independently. Every finding
was REPRODUCED locally before being acted on, and every refutation was
reproduced before being dismissed. Two of the confirmed defects were in code
this release added to PREVENT that exact defect class, which is the argument for
the review in one line.

**Confirmed and fixed (8).** Seven concern `.resample_count_in()` / the Methods
prescan, where a wrong B produces a Monte-Carlo floor of `1/(B+1)` and therefore
a FALSE ACCUSATION against a correctly reported p-value:

| # | input that bound a wrong B | was |
|---|---|---|
| 1 | `"Bootstrap analyses were not used; vitamin B = 60 mg was administered."` | `B = 60` — the sentence-level resampling gate is satisfied by a NEGATED mention |
| 2 | `"Grade A+=80%, A=70-79%, B=60-69%, C=50-59%"` | `B = 60` — the UNSPACED grading scale; the first fix only refused the spaced `B = 60 mg` form |
| 3 | `"Each condition was tested with 60 replicates."` | `B = 60` — `replicates` is ordinary wet-lab vocabulary and its branch was ungated |
| 4 | `"We enrolled 240 iterations of the survey; a permutation test followed later."` | `B = 240` — the loose noun accepted a resampling word anywhere in the sentence, in any order |
| 5 | a count in an `Appendix` after a `Methods` heading | adopted — `Appendix` was not a closing heading |
| 6 | a count after `3.1 Sample Characteristics` | adopted — and no numbering rule can fix it, because `normalize_text()` strips section numbers before the prescan runs (verified, after a first fix that assumed otherwise and did nothing) |

Every count form now requires a resampling word in its own sentence — the
unambiguous nouns satisfy that themselves, so the gate costs nothing — the loose
noun additionally requires the qualifier to PRECEDE the number and sit near it,
`B = <n>` must have nothing else claiming the number, and the Methods region
closes on any short standalone heading-like line rather than on a vocabulary
that can always be incomplete.

7. **`mean_diff_ci` false-fired on ordinary rounding.** The CI-symmetry check
   used a fraction of the interval width, and a correctly reported narrow
   interval at 2 dp exceeds it for no reason but rounding: `0.06, 95% CI [0.02,
   0.09]` gives a relative asymmetry of 0.143 against a 0.02 threshold, on
   numbers that are exactly consistent before rounding. The tolerance is now
   ROUNDING, not a fraction: each value is rounded to its own last place, so the
   midpoint can legitimately differ from the estimate by about one unit there.
   A genuinely mis-stated estimate is still caught.

8. **The bullet chunk rule could sever a chi-square from its own odds ratio.**
   `"chi2(1) = 12.74, p = .013. - N = 211, OR = 0.99, 95% CI [0.77, 1.27]."`
   degraded from `mcnemar_or` carrying OR = 0.99 [0.77, 1.27] to a bare `chisq`
   with both NA — invariant 6, and exactly how collabra.37122 lost an odds ratio
   in v0.7.4. A hyphen is also a dash, a minus and a range, so the marker alone
   is not evidence; the boundary now additionally requires that what FOLLOWS the
   marker is not a statistic assignment (`N =`, `OR =`).

   Removing `-` from the marker set was tried first and **silently reverted
   ieee_access_alt from 3 rows to 1**: `normalize_text()` maps the U+FFFD the
   extractor delivers to a HYPHEN before chunking, so `-` was the load-bearing
   member all along and this file's own comment crediting U+FFFD was wrong. Now
   asserted per-marker so it cannot recur unnoticed.

**Refuted, and recorded rather than "fixed" (4).**

- *"a 99% CI is graded against z(.975)"* — `ci_level` is read from the text
  (`ci_level_source = "inferred_from_context"`) and the implied p uses z(.995).
- *"the midpoint check does not fire on 1.04 vs a midpoint of 1.00"* — it does;
  the reviewer's asymmetry arithmetic was wrong by a factor of two.
- *"a blank line before a digit splits a mean difference from its CI"* — the
  `(?<!\d\.)` guard blocks it, as v0.7.4 designed.
- *"the dual-p retargeting names the wrong value"* — independently re-verified:
  the floor message binds to the permutation p and says so, while `p_computed`
  is still compared against the parametric `p_reported`.

One finding is **accepted as a deliberate trade-off, not fixed**: the legend
guard also refuses a per-result marginal-significance marker (`+p = .08`). The
corpus has 80 legend occurrences and ZERO alnum-preceded ones, and the two
failure directions are not symmetric — refusing loses a p (the row reports NA
and checks nothing), while admitting publishes a legend threshold AS a reported
p-value. Losing a value is recoverable; fabricating one is not.

**Blast radius of all eight fixes: 0 rows gained, 0 lost, 0 changed** across the
48-paper corpus. They are pure tightening on shapes that do not occur in it —
which is the point: they were found by construction, not by the corpus, and the
corpus is what proves they cost nothing.

## Verification

- Suite **1187 `test_that` blocks / 0 failures** across 141 files; every new test
  watched RED against the unfixed code first.
- Conformance corpus 72 cases.
- Whole-corpus diff over 48 real-article texts: 767 rows (from 764) -- 3 gained,
  0 lost, 3 changed, every change verified against the article text.
- One pre-existing test was **sharpened, not deleted**. v0.7.3 R18 asserted the
  absence of a substring, which was right while the permutation p was discarded
  and is not now that the note is true of a value the row actually holds. It now
  asserts the property that always mattered -- the row must not claim the
  PARAMETRIC p is unknowable -- and additionally that any such note names the
  permutation p as its subject.

# effectcheck 0.7.4

**A column boundary was being read as part of a sentence, so an effect size from
one column was graded against a test statistic from another.** The chunk splitter
broke text on `[.!?]` followed by whitespace and an uppercase letter. A two-column
PDF whose columns the extractor merges resumes with whatever the next column
happens to start with — often a bare number — and the lookahead required a
capital, so the two columns stayed one chunk.

`spps.txt` location 216 shipped

```
The overall effect size was d = 0.33, 95% CI [0.09, 0.57].

0.75, 95% CI = [0.54, 0.95], t = 7.47, p < .001).
```

as ONE row: `test_type = "t"`, `stat_value = 7.47` from column B, `effect_reported
= 0.33` from column A, graded `g_ind = 0.379` against N = 1555, status **WARN**.
Both numbers appear in the article. The pairing does not. The `0.75` that belongs
with `t = 7.47` was dropped, and a reported effect was charged against a statistic
it was never reported with. This was disclosed as a known limitation in the 0.6.20
entry; it is now fixed. The two values are separate rows — `d = 0.33` with its
interval as a `d_reported_only` row, `t = 7.47` on its own as NOTE — and the
orphaned effect is verified to survive, since losing it would be worse than the
mis-pairing.

**The fix is a lookahead, not a paragraph rule, and that distinction was decided
by measurement rather than taste.** The first version dropped the sentence anchor
and split on any blank line followed by a capital or a digit — the shape a merge
takes when the extractor truncates a column mid-sentence. Two independent
cross-model reviews converged on the same class of counterexample, and the
whole-corpus diff found the class in the wild:

| input | what the general rule did |
|---|---|
| `..., p = .026, d = 0.75`⏎⏎`95% CI [0.09, 1.41].` | t row loses its CI |
| `..., p = .026`⏎⏎`Cohen's d = 0.75, ...` | t row loses its effect size — the consistency check silently stops running while the row still reports OK |
| `The effect was medium, d = 0.`⏎⏎`65, 95% CI ...` | boundary inside a decimal |
| `t(58) = 3.45, p = .03`⏎⏎`1, ...` | p published as `.03` instead of `.031` |
| `..., d = 0.65, 95%`⏎⏎`CI [0.40, 0.90], ...` | CI severed from its own effect |
| collabra.37122's flattened appendix table | `OR = 0.99, 95% CI [0.77, 1.27]` dropped from the output entirely |

Requiring the boundary to sit after sentence-ending punctuation refuses every one
of them: in each case the character before the blank line is a digit, a `%`, or a
comma — text that is mid-statement by construction. Five reproduced as row-level
defects and are pinned as regression tests watched RED against that first version;
two did not reproduce (both sample-size cases, where the truncated N is rejected
and the document-level scan supplies the value regardless) and are recorded as
such rather than promoted.

`(?<!\d\.)` is the one guard the anchor does not supply: a period preceded by a
digit is a decimal point, not a sentence end. Rejoining `<digits>.`⏎`<digits>` was
considered and **refused on corpus evidence** — across the 48 real-article texts
that shape occurs 221 times and is dominated by reference numbering and section
headings (`...962-967.`⏎⏎`30.`⏎⏎`Singh H, ...`, which a joiner would fuse into
`967.30`), while all 22 occurrences of `<label> = <digits>.` at end of line are
sentence-final periods. A bridge with no observed benefit and a demonstrated
corruption path is the v0.6.20 defect class exactly.

**Whole-corpus diff** (48 real-article texts, 765 rows, per-row field comparison —
not row counts, per the v0.6.20 lesson that `nrow()` cannot see value corruption):
the general rule touched 11 files with 6 rows lost, 13 gained and 16 changed. The
shipped rule produces **one change in the entire corpus** — the target defect, and
nothing else. 0 rows gained, 0 lost.

**Still not fixed, deliberately:** a column truncated *without* sentence-ending
punctuation still merges. Covering it costs a reported value on real text, which
is worse on a science tool than one unchecked row.

## Three separator defects, from the review v0.7.3 never ran

v0.7.3 shipped without `/escicheck-qa`, `/escicheck-review` or a cross-model
review. Running them found the following. Twelve reviewer findings were produced
across two models; **every one was run against the working tree before anything
was changed**, three were refuted outright, and the three below reached a
published number.

**A confidence interval was being destroyed on real published text.** An interval
written with no space after the comma — `95%CI=[7.944,11.984]`, which is how
`nathumbeh_replication_2025` writes all 11 of its intervals — matched the
European full-notation rule as `7.944,11` and was rebuilt into `7944.11`, leaving
the text as `95%CI=[7944.11.984]`. Both bounds came back `NA` and the row still
reported status **OK**. The same clause written with a space parses correctly,
which is exactly what made it invisible: the paper's other rows look fine. The
guard is structural and needs no locale inference — in genuine full notation the
part after the comma is a *terminal fraction*, so it cannot be followed by another
decimal point and more digits. Both lookaheads in the fix are load-bearing:
`(?!\.\d)` alone is defeated by backtracking, which gives up a digit and matches
`7.944,1` instead.

**A European p-value was silently dropped.** Rule D1 required at least one digit
before the comma, but a continental paper omits the leading zero exactly as APA
does: `p = ,025` is `p = .025`, and `p < ,001` is `p < .001`. Neither matched, so
`t(48) = 2,31, p = ,025, d = 0,74` published the row with `p_reported = NA` while
the same clause's `t` and `d` converted normally — and the fuller form carrying
`p < ,001` returned **zero rows**. New rule D1b keys on the *value position* (a
comma directly after `=`, `<` or `>` with no space), not on the locale, so an
English document carrying a stray continental value is repaired rather than
dropped. Requiring no space is what keeps `CI [0.45, 0.89]` and `F(1, 30)` out.

**An OCR-spaced sample size lost everything after its first group.** The repair
loop's anchor was `\d{1,3}`, so it could only ever run once: after the first join
the prefix is four digits and the pattern stopped matching itself.
`nobs = 1, 234, 567` became `nobs = 1234, 567` and the row published
**N = 1 234** — a wrong sample size, not a missing one.

**Refuted, and recorded rather than "fixed":** that `F (1,234)` with an extracted
space fuses its df pair (it normalizes to `F (1, 234)` and parses as df1 = 1,
df2 = 234); that Indian grouping `12,34,567` is partially stripped (left
verbatim); and that a 1:5 lopsided locale contradiction leaves `d = 0,80` reading
as 0 (it converts to 0.80).

**Reproduced but deliberately left:** `F = 1,234` with no locale evidence reads as
1234 rather than 1.234 — genuinely undecidable from shape, and it publishes no
effect size. `RGB = 120,120,120` fuses to `120120120` — reproduced, and worse than
the reviewer claimed, but the fused token is read as no statistic and the
neighbouring row is byte-identical with and without it. A scan of the 48
real-article texts found 71 comma chains of three or more groups and **every one
is a citation superscript** already protected upstream, so the residue is the
known undecidable case rather than an observed defect.

The four normalization rules are part of the cross-language contract with
docpluck, so all three fixes are added to `inst/normalization-spec/` as
conformance cases (spec 1.2.0 → **1.3.0**, 61 → 69 cases) rather than only to the
R implementation.

## Measured, not implemented

Two items carried in the v0.7.3 handoff as open work were resolved by measuring
the premise instead of writing code, and are recorded here so the measurement is
not re-derived:

- **A magnitude prior for fused comma chains.** Proposed to stop
  `IDs 101,102,103` becoming `101102103`. Across the 48 texts, all 71 chains of
  three or more groups are citation superscripts and **every one is already left
  untouched**; the only string that fuses is a chain opening a document with no
  preceding word at all. Meanwhile the real large counts in the corpus
  (`N = 854,720`, `N = 201,939`) convert correctly, and the false-fusion
  candidates produce 3–6 digit values that a 1e6 prior would not catch. The prior
  would add risk without removing an observed defect.
- **Widening the resampling keyword vocabulary** to genomics/network forms.
  `empirical p` occurs **0** times in the corpus; `rewiring`, `configuration
  model`, `degree-preserving` and `surrogate data` occur 0 times; `null model`
  occurs 5 times and **none** is a resampling context (four are an analytic null
  of random connectivity, one is an SEM baseline), so adding it would suppress a
  legitimate p-check — the failure the existing "exact test" exclusion exists to
  prevent. The corpus contains no paper from those literatures, so the gap is
  *untested* rather than absent: the prerequisite is a corpus addition, not a
  vocabulary edit.

## Frontend

Four test types added in v0.7.0 — `wts`, `ats`, `brunner_munzel`, `yuen` — shipped
with **no user-facing explanation**, so the audit table named them and said
nothing about what they are. The result-contract test had been failing since
v0.7.0; `npm run lint` was never run for that release.

# effectcheck 0.7.3

**A comma between digits means at least four different things, and the code only
knew two.** v0.7.2 replaced the thousands/decimal whitelist with a shared spec. Testing
that against the real 38-paper validation corpus — **2,984 comma occurrences**, which the
`tmp/` corpus used until then contained *none* of — showed the remaining classes were
larger than the ones already fixed.

**Contextual signals, measured rather than assumed.** A space after the comma is strong
evidence against a thousands separator: 68% of no-space occurrences are the thousands
shape versus 22% of spaced ones. The spaced+3-digit class is overwhelmingly reference-list
furniture (`Psychol. Methods 3, 424-453`, `Nature genetics 54, 437-449`) and RGB triples.
An earlier draft admitted a space to repair `N = 1, 234`, and on real strings that fused
volume into page (`3424-453`), collapsed `RGB = 120, 120, 120` into `120120120`, and
turned the table row `All places 403,669 107,081` into `403.669107.081`. The general rule
now refuses a space; a narrow `N =`/`nobs =` rule repairs the count case, where the
spacing genuinely is extraction damage.

**Chains resolved by group width.** Of 35 bare comma chains in the corpus, "every group
after the first is exactly three digits" classifies **34 correctly**: `1,000,000` and
`1,054,908` are numbers; `Figures 6,7,8`, `10,14,19`, `Ye1,2,3,4` and `2017,10,39` (a year
followed by citation superscripts) are lists. Previously the decimal rule converted only
the first pair, producing `1.2,3,4,5` — worse than either answer.

**Structural spans are lifted out before any numeric rule runs**: head-noun lists, tuples
of any length, bracketed indices, coded variables, DOIs. Self-identifying full notation
(`1.234,56`, `1,234.56`, apostrophe and NBSP variants) is resolved without locale
knowledge. Terminator whitelists are replaced by a negative `(?!\d)` boundary — that
whitelist had been patched for `/`, then `%`, then `-`, each after a silent failure.

**Document-level locale inference** (`infer_numeric_locale`) resolves the one shape
structure cannot: `1,234`. The two conventions are mutually exclusive, so a single
unambiguous token settles a document. Markers are operator-guarded — a bare `0,1`/`0.1`
misfires on coded variables, version numbers, ratios and lists (5 of 10 realistic
strings); requiring a comparison operator fixes all five. Conflicting evidence is reported
as `conflict`, never majority-voted.

**Impossible values can no longer exit as OK.** Cramér's V, |r|, |rank-biserial|,
|Cliff's δ|, η², ω², R² are bounded by construction, and U/W are integers by construction.
A violation is now WARN + `extraction_suspect`, with the value kept visible so a reader
can recognise the failure. This catches the whole class independently of cause: it would
have caught all three separator defects on its own.

**Dual-p reporting.** `resampling_inference` is a CLAUSE-level fact; whether the BOUND p
is resampling-derived is a VALUE-level fact. Conflating them shipped a false claim — for
the real sentence `t(2037) = -3.26, P = 0.001, P-permutation = 0.002` the bound p is the
parametric 0.001 (verified: `2*pt(-3.26, 2037)` = 0.001132), yet the row asserted "this
p-value is not reproducible even with the raw data" about it. New
`p_reported_is_resampling` keys every claim about the bound value. The safety condition is
**provability**: only a GLUED qualifier (`P-permutation`) is invisible to `pat_p`, so the
bound value is provably parametric and is verified normally; a SPACED qualifier
(`permutation p = .062`) may itself have been bound, so the row stays conservative.
Corpus evidence: 13 real dual-p instances, all verified against `pt()`, of which
**2 (15%) disagree about significance at α = .05** — routine, not a corner case. Magnitude
thresholds remain refused, now empirically: agreeing pairs range from nearly identical to
**29× apart**, and discordant pairs are not magnitude outliers.

Also: a permutation result no longer *worsens* a verdict. Five rows in a PNAS paper
reporting both p-values went OK → WARN purely for containing the word "permutation",
because the p-consistency block is also the WARN → OK rescue.

Suite **3319 passing / 0 failures** across 135 files; conformance corpus 61/61. Validation
corpus (25 papers, 550 rows): **0 rows lost, 0 value changes**, 10 new extractions,
2 verdict changes. Two corrected values are the defect caught in published work —
`U = 55,890` and `U = 84,716` in an RSOS paper were being read as 55.89 and 84.716.

# effectcheck 0.7.2

**A normalization rule was turning thousands separators into decimal points.**
`normalize_text()` resolved the decimal-comma / thousands-separator ambiguity with a
whitelist of four syntactic contexts. Everything outside that list was corrupted:

| input | produced | consequence |
|---|---|---|
| `U = 12,345` | `12.345` | rank-biserial **0.99938** published where the truth is 0.38275 — status `OK` |
| `nobs = 1,182` | `1.182` → N = 1 | Cramér's V = **3.5355** published — V is bounded in [0,1], so that value is impossible — status `OK` |
| `BF10 = 1,234,567.89` | `1.234,567.89` | unparseable; a degraded BF flips "decisive" to "negligible" |
| `1,234/5,678` | arm counts NA | risk-ratio verification silently skipped |
| `M = 1,234.56`, `SE = 1,234.5`, `AIC = 12,345.6`, `H(2) = 1,234.56` | two decimal points | unparseable |

The whitelist had already failed three times — MetaESCI E8 (47 rows lost in one
article), the `N =` case, and a resample-count case in v0.6.22 — each fix adding one
more entry. It was an enumeration, not a rule, and `nobs` (a *documented* sample-size
token since v0.5.5) was never in it.

**Root cause.** Two decimal rules allowed unlimited digits before the comma
(`(\d{1,3}),` and `([-+]?\d+),`). A European decimal has exactly **one** integer digit
(`0,05`, `1,5`, `9,81`); two or more digits before a comma is a thousands group. That
single constraint is the whole fix.

**The rules now live in one place.** `inst/normalization-spec/` holds a versioned
`SPEC.md` and a language-neutral `conformance.json` that **both** effectcheck and
docpluck must satisfy. docpluck had already solved this correctly — its A3a rule
handles every case above, and its source still carries the comment "ESCImate Request
1.1" recording that *we asked for the fix and then never retired our own broken copy*.
Two implementations with no shared test cannot stay in sync; the corpus is that test.
`normalization_spec_version()` is recorded so a published number carries the provenance
of the rules that produced it.

**We also found a case where docpluck is wrong**, and filed it back
(`REQUEST_TO_DOCPLUCK_normalization_spec.md`): its lookahead omits `,`, so
`t(28) = 2,21, d = 0,45` leaves `2,21` unconverted and a parser reads **2** — the same
class of silent error, in the other direction. A European decimal is routinely followed
by the list comma. Both divergence cases are tagged in the corpus.

**Deliberately kept as a separate layer:** `t(1,197)` and `F(7,140)` are the same shape
with opposite answers — a *t*-test takes one df (so 1197 is a thousands separator), an
F-test takes two (so 7,140 is a genuine pair). That depends on **test arity**, which is
statistics-aware, so the shared spec protects all `X(…)` brackets and effectcheck strips
the single-df ones afterwards. docpluck cannot make that call and should not try.

Three defects surfaced during verification that the unit tests alone would have missed:
the corpus caught **my own** wrong expectation about `t(1,197)`; the end-to-end check
caught `/` missing from the boundary set, which silently truncated a clinical arm count
from 1234 to 234; and the corpus-diff harness was picking up this session's scratch
prompt files and reporting them as regressions.

Suite **3181 passing / 0 failures** across 135 files. Whole-corpus render diff over 8
real articles / 169 rows: **byte-identical**. Note the corpus contains **zero**
thousands-separated numbers, so it is structurally blind to this defect class — the
conformance corpus exists precisely because the render diff cannot see it.

# effectcheck 0.7.1

**A rank test's estimand, stated.** The methodological point underlying the whole
0.6.21–0.7.1 line of work: switching from a t-test to Mann-Whitney when normality looks
doubtful is not a like-for-like substitution. MWU targets **stochastic superiority**,
`P(X>Y) + 0.5·P(X=Y)` — not a difference in means, and not generally a difference in
*medians* either, since that reading requires a location-shift / equal-shape assumption
papers rarely state.

The claim is exact, not rhetorical, and the test pins the arithmetic: X uniform on
{1, 5, 6} and Y uniform on {4, 5, 9} have **identical medians** (5), yet
`P(X<Y) + 0.5·P(X=Y) = 11/18 = .611` — far from the .5 null, so the procedure has power
against distributions whose medians coincide. (Fay & Proschan 2010, *Statistics Surveys*
4:1–39; Divine, Norton, Baron & Juarez-Colunga 2018, *The American Statistician*
72:278–286, "The Wilcoxon-Mann-Whitney Procedure Fails as a Test of Medians".)

A NOTE, never an error, on `U` and `W` rows only. We deliberately do **not** scan
surrounding prose for mean/median language: an interpretation sentence cannot be reliably
linked to a specific test, and a false accusation there would be worse than silence.

Implementation note: written as a standalone block rather than a branch of the
computation chain. Folding it in as `} else if (tt %in% c("U","W"))` terminated that chain
early, which would have let its trailing clauses fire for unrelated test types.

## Second cross-model review round (v0.6.22 / v0.7.0 / v0.7.1)

Eight further defects raised against the new material, **all eight reproduced locally
before being acted on**. Four wrote a *wrong number* rather than merely a wrong flag:

- **B scraped from the wrong clause.** *"Across 500 samples, a permutation test with
  10,000 permutations …"* bound `B = 500` and then **false-flagged** the p as below
  "1/(B+1) = 0.002" — a wrong accusation built on a neighbouring count. A bare
  `<n> samples/draws/iterations` no longer counts without a resampling qualifier.
- **The exact floor was applied to a bootstrap.** A bootstrap resamples *with
  replacement* and has no `choose(n1+n2, n1)` reference set, so a legitimate bootstrap p
  was flagged against a bound that never constrained it. The exact floor is now gated on
  a permutation-type method (`resampling_is_permutation`).
- **Two robust statistics in one sentence.** `"WTS(2) = 12.34, p = .002; ATS(1.87, Inf) =
  3.45, p = .061"` stayed a single chunk: the ATS row published **the WTS's p**, and the
  WTS row was dropped. WTS/ATS starts added to the sub-chunk splitter.
- **Brunner-Munzel claimed another test's statistic.** *"The Brunner-Munzel alternative
  was considered, but the reported Wilcoxon result was W = 123"* typed the row
  `brunner_munzel` with the Wilcoxon's W as its statistic. A competing test name inside
  the gap now refuses the match instead of guessing.

Plus: `B = 10,000` with no unit noun was missed; a strict inequality *at* the floor
(`p < .001` with 999 permutations) was not flagged because `.001 < .001` is false, now
compared with `<=`; a Monte Carlo SE and 95% interval were printed around an
**inequality** p (`p < .05` → "SE = 0.00218, interval 0.0457 to 0.0543"), attaching
concrete numbers to a value the paper never reported; and — the commonest reporting form
of all — **Yuen and Brunner-Munzel written with a plain `t(df)`** were claimed by the
generic t branch and had ordinary Cohen's d variants computed for them.

Suite **3024 passing / 0 failures** across 134 files. Whole-corpus render diff:
**byte-identical** at every stage of this work.

# effectcheck 0.7.0

**The modern nonparametric / robust family: Brunner-Munzel, ATS, WTS, Yuen.** The
reviewer asked whether the permutation work was "extendable to other tests (e.g.
Brunner-Munzel, ATS, WTS etc)", assuming that without raw data none could be reproduced.
It is extendable, and further than the question supposed: **all four report a statistic
against a known reference distribution, so the reported p IS independently verifiable**
from the statistic and df alone. It is the effect size that is not recoverable — the same
shape as the existing `cochran_q` branch (v0.5.15).

| new `test_type` | reference distribution | p verification |
|---|---|---|
| `wts` | chi-square(df), df = rank of contrast matrix | `pchisq(WTS, df, lower=FALSE)` |
| `ats` | F(df1, df2), df1 typically **non-integer**, df2 may be `Inf` | `pf(ATS, df1, df2, lower=FALSE)` |
| `brunner_munzel` | t(Satterthwaite df) | `2*pt(-abs(W), df)` |
| `yuen` | t(trimmed df) | `2*pt(-abs(t), df)` |

`pf(F, df1, Inf)` reduces exactly to `pchisq(df1*F, df1)`; the identity is asserted in the
test rather than assumed. Brunner-Munzel and Yuen require their **name** in the clause —
their statistics are written `W` / `W_BF` / `t`, which collide with Wilcoxon and with an
ordinary t-test, so the dispatch uses the same discipline `chisq_subtype` applies to
McNemar/Friedman. All four branches sit before the generic ones, and a regression test
pins that ordinary t / F / W rows are unaffected.

Each is exempted by the v0.6.21 resampling machinery when its own clause says
permuted/bootstrapped — GFD reports both an asymptotic and a permuted WTS precisely
because the asymptotic one is liberal in small samples, and bootstrap Yuen is standard in
WRS2. Grading those against the parametric reference would be the v0.6.21 defect
reintroduced through a new door.

**Orientation warning, recorded because we got it wrong first.** Brunner-Munzel's estimand
`p̂ = P(X<Y) + 0.5·P(X=Y)` is the **complement** of Vargha-Delaney's `A_XY` (they sum to
exactly 1), so **`δ = 1 − 2p̂`**, not `2p̂ − 1`. Our design document had it backwards; a
third verification pass caught it, and direct computation over three cases (including one
with a tie) confirmed the sign flip. Shipping the original would have published every
Brunner-Munzel effect with the wrong sign. No cross-check against a reported Cliff's δ or
Vargha-Delaney A is performed until the reporting orientation can be pinned.

**Pre-existing defect fixed in the same run**: `.friendly_test_name()` covered only 12 of
the 25 shipped test types, so `spearman`, `kendall`, `cochran_q`, `RR`, `rdpct`, `md_hl`,
`binomial`, `interaction_p`, `mediation_indirect`, `mcnemar_or`, `bayes_factor`,
`hazard_ratio` and `d_reported_only` fell through to their raw slug in every report. All
filled in, and a test now asserts every type in the default `stats` vector has a display
name — so the next new type cannot silently regress it.

# effectcheck 0.6.22

**What CAN be checked on a resampling p-value without the raw data.** 0.6.21 stopped
grading a permutation p against the parametric reference. That left the reviewer's actual
question open: if it cannot be recomputed, can anything be verified? Three things can,
none of which need the data.

**1. The minimum attainable p.** A Monte Carlo permutation p sits on the lattice
`(r+1)/(B+1)` (Phipson & Smyth 2010), so with B stated the smallest value the procedure
can produce by counting is `1/(B+1)`. `"1,000 permutations, p < .0001"` is below that
floor. New `resampling_B` column parses the resample count.

**2. The exact-permutation floor.** With n1 and n2 known the reference set has
`choose(n1+n2, n1)` members, so no exact p below `1/M` is reachable — at n1 = n2 = 5,
M = 252 and nothing under `1/252` exists.

**3. Monte Carlo fragility.** `SE(p̂) = sqrt(p(1−p)/B)`, so `p = .048` with B = 1,000 has
SE ≈ .0068 and an approximate 95% interval straddling .05 — the significance decision is
not stable at that resample count. Reported only when the interval actually crosses alpha.

Plus a reporting-completeness note: a resampling result that never states B is not
reproducible **even with the raw data**.

**None of these is a hard ERROR, deliberately.** Legitimate practice reaches below the
counting floor — GPD tail approximation, sequential Monte Carlo, combining per-stratum
p-values, mid-p (`0.5/M`), randomized p (no positive floor at all). They state an
arithmetic fact and let the reader judge.

Two calibration findings, both from enumeration rather than assertion:

- A cross-model reviewer argued the two-sided floor is provably `2/M` when n1 = n2 (the
  complement of an n1-subset being another n1-subset) and that `1/M` becomes reachable
  when n1 ≠ n2. **Enumeration refutes it**: the floor is `2/M` for every configuration
  tried — 5,5 → 2/252; 4,6 → 2/210; 3,7 → 2/120; 2,8 → 2/45; 5,7 → 2/792 — because the
  mirror comes from the smallest-values-vs-largest-values split, which exists at any
  n1, n2. So no special case is needed, and the shipped bound is the conservative `1/M`,
  which cannot false-flag whatever the statistic, tie structure, or two-sided convention.
  (Ties only raise the true floor: a tied x gave .0317 against .0079 untied.)
- **`normalize_text()` turns "10,000 permutations" into "10.000", and
  `numify_int("10.000")` is 10.** Taking that at face value would have set B = 10, a floor
  of `1/11 ≈ .09`, and false-flagged essentially every permutation p in the corpus. A
  resample count is always an integer, so every `.` and `,` in it is a thousands
  separator and is stripped rather than parsed.

New columns: `resampling_B`, `resampling_p_below_floor`. 8 `test_that` blocks, watched to
FAIL first (11 failures + 2 errors before implementation). Suite **2947 passing /
0 failures** across 132 files. Whole-corpus render diff: **byte-identical**, 0 rows
gained, 0 lost, 0 verdict changes.

# effectcheck 0.6.21

**A resampling-derived p-value was being graded against a parametric reference.**
Raised by a methodologist reviewing ESCImate, who asked whether a "perm Welch t" could
be approximately checked. Investigating it surfaced two defects, both reproduced at
0.6.20 before any fix was written.

**The false flag.** `"A permutation Welch t-test with 10,000 permutations showed no
significant difference, t(58) = 2.31, p = .062"` returned `decision_error = TRUE`,
`reason = reported_ns_computed_sig`. The paper is correct: the parametric p for
t(58) = 2.31 is .0245, but the reported .062 came from the permutation distribution.
The pre-existing `method_context_in_chunk` cap did **not** rescue it — "permutation"
was absent from `method_kw`, and that cap only fires on `status == "ERROR"`, not WARN.

**The wrong published interval.** `ci_OR_all()` back-derives a CI from the reported p
via `SE = |log(OR)| / qnorm(1 - p/2)`, an inversion that assumes a **normal** reference
distribution. Fed a permutation p, a McNemar row reporting `OR = 2.50, 95% CI
[1.05, 5.95], p = .062` produced a computed interval of **[0.9551, 6.5441]** — crossing
1 where the paper's does not — and then declared the paper's own correct CI
`INCONSISTENT`. Reachable from four test types (`mcnemar_or`, `chisq`, `regression`,
`z`); it requires a *reported* CI for the comparison to fire, which is why a first
probe without one returned NA and looked clean.

**The fix is scoped to the p-value, not the row.** A permutation changes only the
reference distribution — the statistic itself is computed identically, so
`d = 2t/sqrt(df)` remains exactly as valid and is still checked. Blanket-capping the
row at NOTE would have discarded a real, correct check. New parse columns
`resampling_inference` / `resampling_method` read the row's **own clause** (the v0.6.18
Welch precedent — reading `context_window` leaked a modifier onto a neighbouring row,
N 132 → 403). When set: the p-consistency comparison and `decision_error` are
suppressed, the p-back-derivation is gated off, and an honest note names the parametric
comparator without asserting an error.

Deliberately **not** a numeric `|p_perm − p_param|` threshold. Under the very conditions
that motivate permuting, the two legitimately differ, so any magnitude threshold would
manufacture false flags; only decision discordance is reported, and only as a note.
Verified by simulation (1,000 sims, equal means, nominal α = .05): permuting the raw
mean difference gives **26.6%** type-I error at n₁=10 (sd 3) vs n₂=40 (sd 1), while
permuting the Welch t itself gives 7.2% against the parametric 6.4% — so a reported
"permutation Welch t" must mean the *studentized* form (Janssen 1997), and its p sits
close to, but not on, the parametric p.

Two deliberate exclusions in the keyword set: `"randomization"` must be qualified by
`"test"` (a bare `randomi[sz]ed` would match every randomized controlled trial), and
`"exact test"` is absent entirely — Fisher's exact is a closed-form conditional test
whose p **is** computable, so matching it would suppress a legitimate check. Both are
pinned by tests, as are `"randomly assigned"` and `"randomized controlled trial"`.

Also: `"bootstrap"` belongs to both `method_kw` and the resampling set, so a
bootstrapped **result** used to be explained as a methods-section artifact ("power
analysis, meta-analysis, etc.") — a false statement about the row. The method-context
message is now suppressed when the row is recognised as resampling-based.

**Cross-model review raised eight further paths against the first draft, and all eight
reproduced locally before being acted on** — three over-suppression, four
under-application, one regex gap:

- A **bootstrapped CI** ("`p = .50, d = 0.61, bootstrapped 95% CI [0.10, 1.10]`") marked
  the whole row resampling-based and hid a genuine decision error. A resampling word
  now counts for the p-value only when at least one occurrence is *not* immediately
  followed by interval language.
- An **`n.s.` label** bypassed the guard entirely — that branch keys on `p_ns`, not
  `p_reported` — so `"permutation Welch t-test, t(58) = 2.31, n.s."` still returned
  `ns_label_vs_computed_sig`.
- The **correlation N-candidate selector** picked N by minimising
  `|p_try − p_reported|` with `p_try` computed *parametrically*; with candidates 42 and
  380 a permutation `r = .30, p = .001` moved N to 380 (df 378), and N drives the
  effect size and its CI. The most consequential of the eight.
- The **p > .5 extraction-artifact rule** is a magnitude comparison, so a correct
  `p = .62` permutation row was marked `extraction_suspect`.
- The **`md_hl` p-vs-CI invariant** assumes p and interval share a reference
  distribution; a bootstrap p with a percentile interval need not agree, so the
  disagreement is now a caveat rather than an "inconsistency".
- **`randomization inference`** (standard econometrics phrasing) was not matched, only
  `randomization test`. `permutation-based` and `nonparametric bootstrap` did match.
- Releasing the method-context ERROR cap whenever a row was resampling-based was **too
  broad**: `"A Monte Carlo simulation power analysis showed ..."` is a genuine method
  artifact and must keep its cap. The cap is now released only when the clause carries
  no method keyword beyond the resampling vocabulary.

14 `test_that` blocks, every defect-targeting one watched to FAIL against the unfixed
code first (23 failures + 1 error at clean HEAD). Two blocks are green at HEAD by
design — they guard against this fix over-reaching. Suite **2911 passing / 0 failures**
across 131 files (baseline at HEAD 2849). Whole-corpus render diff over 9 real-article
texts, 192 rows: **byte-identical**, 0 rows gained, 0 lost, 0 verdict changes.

# effectcheck 0.6.20

**A normalization rule was deleting reported statistics.** MetaESCI filed two
apparently unrelated defects (O-1, O-2) with two different diagnoses. Both traced to
one line in `normalize_text()`, and neither diagnosis was right. The sweep that
followed found four more defects of the same class — one of them destroying a real
effect size in a paper already in the regression corpus.

## The defect class: normalization that deletes source text

`normalize_text()` repairs PDF text-layer artifacts before anything is parsed. Several
of its rules "bridge" a line wrap by skipping a span and adopting a number from the
next line — the repair for `d =\n0.80`. None of the three that skipped a *bounded*
span checked whether that span contained a value, so a **reported statistic was
destroyed and replaced by whatever number happened to open the next line**:

```
"etap2 = .86, and Experiment\n1b"                 -> "partial eta-squared = 1"
"d = 0.74 (see Table\n2)"                         -> "p = 2)"
"chi2 (4, n = 211) = 12.74, p = .013\n\n10 items" -> "chi2 (4, n = 10 items"
"t(48) = 2.31, p = .025, d = 0.65\n\n10 items"    -> "t(48) = 2.31, p = 10"
"r(351) = .164, p = .050\n\n10 items"             -> "r(351) = .164, p = 10"
```

Two invariants now govern every bridging rule:

1. **The skipped span must be digit-free.** A bridge may never discard a number that
   is already present. (One sibling rule already did this and was correct; the
   difference had gone unnoticed for eleven releases.)
2. **When prose is skipped, the adopted number must carry a decimal point.** A bare
   integer opening a line is a page or section number, not a statistic. The genuinely
   bare case — an assignment whose line simply ends at the `=`, where a wrapped
   integer `n =\n120` is legitimate — is handled by the whitespace-only joiner, which
   deletes nothing.

Corrections to the filed diagnoses, since MetaESCI is writing this up:

- **O-1 is not a capture bleed.** No effect-size capture pattern is involved; every
  one already terminates at its own value. `normalize_text` deletes the value before
  the parser ever sees it.
- **The label vocabulary is incidental.** The rule keys on `[a-z]+ =` plus a wrapped
  digit. `Table`/`Figure`/`Study`/`Experiment` are not special, and any intervening
  prose triggers it — so 42 rows is a floor set by the search vocabulary, not a census.
- **O-2 is the same defect, not a chi-square continuation guard.** The rule matched
  `n = ` inside the chi-square's own parentheses and deleted `211) = 12.74, p = .013`,
  destroying the chi-square token itself — hence zero rows.
- **The t and r rows filed as O-2 controls were not controls.** They return one row,
  which is why they looked clean, but that row carries a **fabricated p-value of 10**.
  Row count is not a sufficient probe for this class.

## Four more defects of the same class, found by the sweep

- **The section-number stripper deleted line-wrapped values.** `\d+(\.\d+)+\.?[ \t]+`
  at line start matches `0.86 ` exactly as it matches `3.3.1 `, so
  `"d =\n0.86 in the treatment group"` shipped `effect_reported = NA` with status
  **OK** — a false all-clear from a rule nobody had looked at. A section number is now
  distinguished by shape (two or more decimal groups, or an explicit trailing dot) or,
  for the single-group form, by being followed by a **capitalised word** — a heading
  introduces one, a wrapped value continues its sentence in lower case or terminates
  it with a period first.
- **A wrapped value that ends a sentence was stripped too**, because the sentence
  period supplies the trailing dot: `"d =\n0.86. In Study 2"` lost the 0.86. A
  dangling assignment operator on the previous line now exempts it.
- **`p = 10` was published as `p_reported = 1`**, with `p_valid = TRUE` and
  `p_out_of_range = FALSE`. `pat_p`'s bare `[01]` alternative had no right-hand
  boundary, so it matched the leading digit of a longer number; the `[0, 1]`
  validation never saw the offending value because the regex had already truncated it
  into range. A malformed p is now detected and reported honestly.
- **The parse-time plausibility guard nulled values silently.** A suppressed effect
  size was indistinguishable downstream from a statistic that reported none — the
  more dangerous of the two O-1 failure modes, and by MetaESCI's count the majority
  (27 of 42) of the corrupted rows. The guard now records
  `effect_guard_rejected` / `effect_guard_reason`, surfaced as an uncertainty message
  with `extraction_suspect = TRUE` (MetaESCI O-1 request 2).

## MetaESCI O-3, O-4, O-5

- **O-3 — `ci_referent`** (new column). A row carrying a regression coefficient can
  print its interval on the unstandardized `b` scale or the standardized `beta` scale,
  and nothing in the APA string says which; the interval was graded against the
  standardized computation either way. A Wald interval is symmetric about its own
  estimate, so its **midpoint** identifies the referent (tolerance 5e-3). When the
  referent is `b`, the comparison target is now the Wald-t interval on `b` itself,
  `b +/- t(df) * SE`, added as a computed variant. Not scoped to
  `test_type == "regression"`: a `b = ..., t(df) = ..., CI` row with no SE is typed as
  a plain t-test and hit the identical cross-scale defect, returning a false
  INCONSISTENT; it is now `UNVERIFIABLE`. The column stays `"unknown"` when the
  midpoint matches neither candidate — an honest abstention beats a coin flip.
- **O-4 — R-squared on a correlation.** `r_squared` was only ever an `alternatives`
  entry, so a reported `R2` had no same-type computed counterpart and the matcher fell
  through to Cohen's `f2 = r^2/(1-r^2)`, a different scale. `r(1526) = .32, R2 = 0.10`
  (r^2 = .1024, a correct APA-rounded report) shipped WARN, and a hand-perfect
  `R2 = 0.1024` WARNed too. Now promoted to a computed variant when an `R2` is
  reported, and both PASS. MetaESCI attributes this to the `r = c("r", "R2")` validity
  list; that list is right — R-squared is a legitimate thing to report for a
  correlation — the defect was the missing variant.
- **O-5 — RULING: a z-test reporting an odds ratio is not anomalous**, and the WARN
  was a false positive. The Wald z of a logistic coefficient is by construction
  `z = ln(OR)/SE(ln OR)`, and a meta-analytic z tests a pooled log-OR; the OR is the
  natural effect size in both. `OR` joins the valid z-test effects. Better than
  silencing it: when the OR carries its own interval the z **is** recoverable, since
  `SE(ln OR) = (ln U - ln L)/(2*z_level)`, so the reported z is now verified against
  `ln(OR)/SE` and the difference reported. Without an interval the row says plainly
  that the OR is not recoverable from the z alone, rather than calling it unusual.
  The implied z is reported **in the uncertainty message only** — deliberately not as
  a computed variant. See the release-review finding below.

## Also

- **`ci_level` is now bounded at both ends.** The guard tested only `< 0.50`, so
  `263.95% CI` yielded `ci_level = 2.6395`, `ci_level_mismatch = NA` and status PASS.
  A coverage probability of 1 or more is not implausible but impossible. A plausibility
  guard on a two-sided quantity has to be two-sided.
- **Two deferred MetaESCI E9 items re-verified at 0.6.20.** Pipe-delimited table rows
  parse to one clean PASS row (confirms MetaESCI's finding that this closed silently).
  The trailing `BF10 > 100)` dedup produces no duplicate across four constructed
  shapes; the original input was never filed, so this is as far as it can be taken
  from this side.

## Two defects in this release's own diff, caught by the release pipeline

Recorded because both would have shipped an inaccuracy, and both were found *after*
the change was otherwise green (suite passing, CRAN check clean, corpus diff clean,
two cross-model reviews complete).

- **QA — `effect_guard_rejected` / `effect_guard_reason` were write-only columns.**
  The parser produced them and `check.R` consumed them internally (uncertainty message
  and `extraction_suspect`), but they never reached `check_text()`'s output — while
  `API.md` documented them as columns. That is MetaESCI's O-1 request 2 only
  half-delivered: the point was to let a consumer *filter* on suppression, which needs
  a column to filter on. Now emitted at every output constructor, carrying the real
  values wherever they are in scope, and added to the E3 schema contract so a future
  removal fails loudly.
- **Review — the implied z polluted the effect-size matcher.** The OR/CI check
  initially registered `z_from_OR_ci` as a computed variant. That list is the matcher's
  candidate pool, and with no same-type variant available the matcher falls back to any
  computed variant — so it matched the implied z against the reported **odds ratio**
  and published `matched_value = 2.460` with `delta_effect = 0.630`: an odds ratio
  minus a z-statistic. It also moved with the confidence level (0.234 at a 90% CI),
  which no effect-size delta can do, and `delta_effect` is exactly the field MetaESCI's
  pipeline reads. The variant carried no CI of its own, so it contributed nothing to
  the CI-candidate collector either — pure liability. The diagnostic now lives only in
  the uncertainty message. The sibling `b_coeff` variant was tested for the same
  hazard and is safe: it belongs to no effect-size family, so the family filter keeps
  it out of the matcher while the CI collector still reaches it — pinned by a test
  constructed so it would win on numbers alone (reported effect equal to `b` exactly).

## Verification

- **Test suite: 2849 passing, 0 failures** (was 2751 / 1054 blocks; now 1084 blocks).
  30 new `test_that` blocks in `test-v0620-normalizer-value-deletion.R` and
  `test-v0620-metaesci-o3-o4-o5.R`, **every one authored against the unfixed code and
  watched to fail first** (26, 27 and 6 failures across the three rounds).
- **Cross-model review of the diff**, both reviewers instructed to find wrong numbers
  rather than to approve. Codex raised three surviving paths and Sonnet three more;
  **all six were reproduced locally before any was acted on**, and two of them were
  regressions introduced by the first version of this fix (a decimal-shaped section
  number surviving into a fabricated `d = 3.3`, which the decimal-recovery step then
  silently rewrote to `0.33`; and sentence-final `p = 1.` being rejected as
  out-of-range). Both are fixed and pinned.
- **Whole-corpus row-count delta** (11 cached real-article texts, 215 rows): **0 rows
  gained, 0 lost, 1 verdict change** — reinstating the per-release practice MetaESCI
  notes lapsed after 0.3.1. The single change is the defect caught in the wild:
  `spps.txt` loc 216 is a two-column merge where 0.6.19's bridging rule deleted the
  article's stated `d = 0.33, 95% CI [0.09, 0.57]` and manufactured
  `d = 0.75, 95% CI = [0.54, 0.95], t = 7.47` — **a sentence that does not appear in
  the paper**, while the article's actual overall meta-analytic effect vanished from
  the output entirely. The row now reports the stated 0.33 and the garbled merge is
  visible in `raw_text`. Not found by the label-vocabulary search that produced the
  42-row estimate.

## Known limitation (new, deliberate)

The sentence splitter does not break on a paragraph boundary, so a two-column merge
like the `spps.txt` case above still yields one row pairing an effect from one column
with a test statistic from another. Splitting on a blank line would separate them (and
the bare-d-with-CI pattern would pick up the orphaned effect), but it changes chunking
for every document and belongs in its own release with its own corpus validation.

# effectcheck 0.6.19

**Two shipped "cannot verify" messages claimed mathematical impossibility that does
not hold.** Found by the 2026-08-05 re-audit of every won't-fix / not-recoverable
ruling in the repo, run under the portfolio-wide triple-verification rule (each claim
checked against the primary source, then codex, then a Sonnet pass instructed to
refute). **No verdict, status, or computed value changes** — the conservative
behaviour was and remains correct. Only the stated *reason* was wrong, and on a
science platform a false claim about what is mathematically knowable is itself a
defect.

- **Cohen's h on a goodness-of-fit chi-square** said h *"is not recoverable from the
  chi-square statistic alone"*. That is overbroad: for **df = 1 against an explicit
  50-50 null with N known**, `chi2 = N(2p̂ − 1)²`, so `|p̂ − 0.5| = sqrt(chi2/N)/2` and
  h is determined **up to sign** (chi2 = 6.4, N = 100 → p̂ = .6265, h = .2558). The
  claim does hold for k > 2 categories, a non-.5 null, or unknown N. The message now
  says the chi-square does not pin the proportions down *here* (it aggregates all
  categories and the null proportion is not established from the text), rather than
  asserting impossibility in principle.

- **A standardized beta reported on a t-test** said a beta *"is not recoverable from
  the t-statistic alone"*, unconditionally. For **simple (single-predictor)
  regression** the standardized beta equals `r = t/sqrt(t² + df)` exactly — a formula
  this package already implements (`standardized_beta_from_t()`) and applies to rows
  typed `regression`. The claim is true only for multi-predictor / mediation models.
  The guard still fires unconditionally **by design**: a bare `t` clause does not
  establish the predictor count, and assuming k = 1 would silently publish a wrong
  beta for every mediation path — the exact failure this guard exists to prevent. The
  message now says the model is not identifiable from the clause.

- **The DP-3 partial-η² mechanism** is corrected at its two remaining sites
  (`NEWS.md` 0.6.7 entry, `R/parse.R`). The won't-fix verdict for body prose stands
  (OCR / shape-recognition tier); the *reason* — "the glyph lives in a font with no
  ToUnicode CMap" — is refuted: the symbol is **4 filled vector curves with no char
  object**, i.e. drawn ink that was never text. The 0.6.7 entry also recorded the
  ruling as "confirmed" on the strength of a circular check (counting `η` in the
  extracted text, i.e. asking the extractor under suspicion whether its own output was
  complete). Practical consequence: font/CMap/ToUnicode recovery can never fix this
  class; a vector-path recogniser can.

Regression tests: the two message assertions previously grepped the retracted
phrases, so correcting them would have looked like a regression. Both now pin
**behaviour plus semantic content** and add an explicit `expect_false` on the
retracted phrase — verified RED against the old wording before being restored.

Full report: `docs/FINDINGS_2026-08-05_rejection_reaudit.md`.

# effectcheck 0.6.18

**Four sample-size defects that published wrong or unlabeled Ns**, found by the
2026-08-04 escicheck-iterate canary audits of `collabra.57785`, `collabra.90203`
and `pci.rr.100726`. Every one was reproduced at HEAD before any code changed,
and each carries a regression test that was watched RED against the unfixed code.

- **A docpluck table row never received the document-level N.**
  `flattened_rows_to_parsed()` emits no `N_source`, so a flattened table t-row
  with no printed `n` reached the checker with `N = NA` and fell to the internal
  `df + 2` default — even when the paper states its N plainly in prose. On
  `collabra.57785` the Table-8 `t(742)` rows published **N = 744** where the
  paper states 743 (`= df + 1`; gold `n_total = 743`). A table t-row now binds
  the document N when that N is **df-compatible** — exactly `df + 1` or
  `df + 2`. The exact-match window is the evidence gate: two independently
  sourced numbers agreeing to the unit is real information, while a scraped
  document N matching neither candidate cannot slip in. Table rows deliberately
  do NOT get the unconditional global-N fallback prose rows have.

- **An ambiguous-design row published its internal assumption as fact.** The
  ambiguous branch stores the independent `df + 2` in `N` "for independent
  calculations", and that internal value leaked to the published top-level `N`
  with no `N_source` and no message naming the paired alternative;
  `repro_code` then asserted "Assuming equal n". Now the row states both
  candidates (`df + 1` paired / `df + 2` independent) in `uncertainty_reasons`
  and `repro_code` emits the paired formula alongside the independent one.

- **`N_source` said `"not_found"` next to a populated `N`.** A self-contradiction:
  the N *was* found — by inference from df. `pci.rr.100726` published
  `N = 870` with `N_source = "not_found"`. Any N derived from df is now labeled
  **`"df_inferred"`**.

- **A scraped N outranked the row's own df for sources other than `global_text`.**
  v0.6.17 fixed this for the one `N_source` its reproduction happened to use;
  `local_context` / `extended_context` are the same kind of evidence — a number
  the statistic's own clause never claimed — and carried the identical defect.
  `"Participants (N = 1001) … t(667) = 3.67, d = 0.28 [0.13, 0.44]"` bound
  N = 1001 where df fixes N at 669, computing `d = 0.2320` against a reported
  0.28 and firing a **false WARN plus a false CI mismatch** — while the row's own
  uncertainty text already read "Reported N (1001) is larger than expected
  (668-669) for df=667" and used 1001 anyway. The override is now keyed on a
  named set, `.SCRAPED_N_SOURCES`. Sources stated *by* the statistic's own
  clause (`own_clause`, `subgroup_sum`, `arm_totals_sum`, `chi_inline`, …) are
  deliberately excluded — an explicitly reported N that disagrees with df is a
  finding to surface, not a value to silently overwrite.

**A post-hoc contrast that reprints the omnibus ANOVA error df is no longer
scored against `df + 2`.** After a k-level ANOVA, stats packages routinely
reprint the omnibus error df on each pairwise contrast, but a two-level contrast
uses only ~2/k of the sample. On `collabra.90203` an `F(2, 998)` omnibus
(N = 1001) is followed by `t(998) = 2.46, p = .041, d = 0.19 [0.04, 0.35]`;
binding N = 1000 computed `d = 0.1556` (delta 0.0344) and produced a **WARN plus
an INCONSISTENT CI flag**, both false — at the true contrast N = 669 the
computed `d = 0.1902` (delta 0.0002) and the CI reproduces the reported one.

The rule ships in a deliberately conservative form. A cross-model review
(Codex) **refuted the first draft**, which fired on same-document df equality
alone: a paper can legitimately contain a 3-arm `F(2, 998)` *and* a real
two-group comparison of the full sample with a genuine `t(998)`. That
counterexample was reproduced locally before the design changed. So omnibus-df
matching now only **proposes** a candidate N, and adoption requires BOTH that
the surrounding text describes a post-hoc / pairwise / Tukey / Bonferroni
comparison AND that the row's own reported effect size is explained materially
better by the candidate than by `df + 2`.

A second review round (also Codex) showed why the text requirement is
load-bearing: effect-size fit alone is **circular**, because an unequal-groups
full-sample t reports a d larger than the equal-n `df + 2` estimate and can fit
the candidate coincidentally — a document with `F(2, 98)` and a genuine
`t(98) = 2.00` whose unprinted cells are `n1 = 20, n2 = 80` would have been
rewritten to N = 67 against a true 100. CI-width corroboration does not rescue
it (unequal groups widen the interval in the same direction a smaller N does),
and `contrast_N / (df + 2)` is `2/k` in both cases, so the two are
mathematically indistinguishable from the numbers alone. Both counterexamples
are pinned as tests.

A row whose reported effect matches *neither* N keeps its inconsistency flag;
suppressing it unconditionally would mask a genuine reporting error. Explicit
per-group sizes always outrank the hypothesis, and a 2-group `F(1, df)` never
triggers it.
Adopted Ns are labeled `N_source = "omnibus_df_contrast"` and disclose the
balanced-cell assumption in `uncertainty_reasons`.

Verified on the real corpus render: the two false positives clear
(WARN → PASS, CI INCONSISTENT → MATCH) while the same paper's honestly-reported
`t(667)` / `t(668)` rows stay byte-identical.

A cross-model review of this release's own diff surfaced six further defects,
each reproduced locally before being fixed: `subgroup_sum` was exempted from the
df-authority override although it is matched over the wider context (so a
subgroup pair in `[df+3, df+12]` bypassed it); the Welch branch's
implausible-N cross-check was still keyed on `global_text` alone, which became
the only remaining guard once stated-Welch rows began skipping the non-Welch
path; the Welch back-computation `N = 4t²/d²` treated a reported Hedges' *g* as
a Cohen's *d* (now converted via `d = g / J`, after a first attempt that simply
excluded *g* proved worse — it left the implausible scraped N in place); the new
relative p-value gate falsely flagged legitimate rounding (`p = .01` printed at
two decimals honestly represents a computed `.0051`), so the ratio now applies
only outside the rounding band the printed precision implies; and
`N_source = "df_inferred"` was not emitted when a *scraped* source had been
discarded and replaced, leaving the row advertising a provenance its published
number no longer had.

**The z-branch scraped-N disclosure now covers every scraped source.** v0.6.17
added the warning because a z-test has no df, so none of the df-keyed
N-plausibility guards that protect t-rows can fire — whatever N is bound is used
for `d = 2z/sqrt(N)`, `dz = z/sqrt(N)` and `r = z/sqrt(z² + N)` with nothing to
contradict it. But it was keyed on `global_text` alone, leaving `local_context`
/ `extended_context` / `subgroup_sum` silent. `"The calibration sample
(N = 100) was used first. In the target subsample (n = 25), z = 2.00, p = .046,
r = .20."` bound N = 100, published `status = "OK"` with an entirely **empty**
`uncertainty_reasons`, and computed `r = 0.196` against the reported `.20` — an
apparent match, where the clause's own `n = 25` gives `0.371`. Re-keyed on
`.SCRAPED_N_SOURCES`. Found by the final pre-push cross-model review.

**Known, unfixed:** a statistic quoted twice in running text is still scored
twice. `pci.rr.100726` is a peer-review letter whose comment prints one
`t(868) = -3.01, p = .006` twice to illustrate APA comma placement, and the
render emits two rows. Three dedup rules were built and each was disproved —
by the v0.6.14 invariant that two genuinely distinct correlations can share
every reported number (`r(797) = .16` twice in one sentence, different
variables), and finally by the real paper itself, whose echo carries its own
`t(df)` anchor and so is indistinguishable from a second result. Separating the
two needs a signal this layer does not have. **The duplicate is left in place
deliberately**: a duplicate row is a counting error the reader can see, a
dropped row is a lost result they cannot. The invariants any future fix must
respect are pinned in `test-v0618-prose-restatement-dedup.R`.

**A reported CI with no parseable effect size is no longer silent.** Such a row
narrowed to a p-value-only check and could still publish `status = "OK"` with
`ci_check_status = "MATCH"` and nothing in `uncertainty_reasons` — a reader saw
a green row and could not tell the paper had reported an effect size the tool
never verified. (On `collabra.90203` the partial-eta-squared symbol is drawn in
the source PDF as filled vector curves with no character object at all, so the
body text arrives as a nameless `= .008`; the value is recovered from the table
view, but silence on the body-text row was still dishonest. *Mechanism corrected
2026-08-05: this entry originally said the glyph "has no ToUnicode mapping" —
that cause is refuted; the symbol is ink, not badly-encoded text. The behaviour
described here is unchanged and correct.*) A confidence interval cannot exist without
an estimate, so its presence is proof an effect size was reported — the row now
says the effect size was not verified. Found by the cycle-2 canary audit.

Also fixed in the same render: the ANOVA-design uncertainty message was authored
with an escaped em-dash, which passed the source-file ASCII check but reached
the **user** as a corrupted byte. A new test asserts emitted messages contain no
non-ASCII bytes — checking the source alone was blind to this.

Internal: `pat_N` is hoisted to a package-level `.pat_doc_N` with a shared
`.doc_global_n()` helper, so `check_text()` and `parse_text()` cannot drift
apart (an attribute on `parse_text()`'s return value was tried first and
silently vanished on the zero-statistics early-return paths).

# effectcheck 0.6.17

**Three sample-size defects that published wrong effect sizes**, found by
expanding the escicheck-iterate comparison harness onto papers that had an AI
`stats` gold but had never been compared against the library (~548 gold results
the library had never been audited against).

- **`global_N` resolved a frequency TIE to the SMALLEST candidate.** The
  document-level fallback took the mode of every `N = <int>` in the text, but
  `table()` orders counts by ascending numeric name and `which.max()` returns
  the FIRST maximum — so whenever the top frequency was shared, the "mode"
  silently became the smallest number in the paper. On
  10.1016/j.jesp.2009.12.010 every candidate tied at frequency 2 (7, 13, 25, 31,
  38 — each twice, all cells of one accepters/rejecters subgroup table), so the
  paper's global N became **7**, its smallest subgroup cell. The rule is now a
  documented, tested helper (`global_n_from_candidates()`): a tie resolves to
  the largest **tied** value, never by escaping to the global maximum. That last
  distinction matters — an intermediate version that used `max(ns)` on a tie was
  caught by the corpus diff handing 10.1525/collabra.32572's F rows N = 3302 (a
  lone outlier among a 273–279 cluster) against a true 999.

- **A z-test published effect sizes from a scraped N in complete silence.** The
  `z` branch computes `d = 2z/sqrt(N)`, `dz = z/sqrt(N)` and `r = z/sqrt(z²+N)`,
  but every N-plausibility guard in the package is keyed on df — and a z-test
  has no df — so none of them could fire. The two Study-2 Sobel mediation rows
  on jesp.2009.12.010 published `r_from_z = 0.7341` and `d = 2.162` from N = 7
  with an entirely **empty** `uncertainty_reasons`; the study's real N is 76,
  giving 0.312 and 0.328 — more than **double** the truth. The branch now
  announces a document-level N and states that every effect size below scales
  with it. (As in v0.6.16's E7, a `SKIP`/`NOTE` status does not contain this:
  `all_variants` values reach the reader regardless of status.)

- **A global-text N could outrank a stated df.** The existing
  "global-text N incompatible with df" override only fired at `N > df + 12`, so
  any scraped N in `[df+1, df+12]` cleared both it and the minimum-N guard and
  was kept — even though df fixes N to within one unit (df+1 paired, df+2
  independent). This was latent until the tie fix above raised jesp.2009's
  global N from an obviously-broken 7 (rejected by the minimum-N guard, then
  correctly re-derived as 34 from df) to a plausible-looking 38 that sailed
  through both guards, degrading `g_ind` from 1.1052 to 1.0482 on the Study-1
  t(32) rows. For a `global_text` N, df now wins for any value above df+2.
  Welch rows are untouched (they take a separate branch where N legitimately
  exceeds df+2) — a cross-model reviewer flagged that risk and local
  reproduction refuted it; the routing is now pinned by a regression test.

- **A df-replaced N could contradict the row's own design label.** The override
  above picks its replacement with `if (canonical_type %in% c("dz","dav","drm"))
  df1 + 1 else df1 + 2` — but `canonical_type` is the reported *effect-size
  family*, not the design. A t-test reporting no effect size at all has
  `canonical_type = NA`, so it fell to the `else` and took the
  independent-samples N even on a row the checker itself labelled
  `design_inferred = "paired"`: a paired `t(49)` published N = 51 where the true
  paired N is 50. Two corrections: when the effect-size family does not settle
  the design, the incompatible N is discarded so the existing ambiguous-design
  path re-infers it (computing *both* variants); and the published `N` is
  reconciled to df+1 when the final design label is paired/one-sample and `N` is
  still the df+2 default. Scoped narrowly — a t-test with a known df, no
  explicit group sizes, and `N` exactly equal to df+2 — so an explicitly
  reported N is never touched. Verified against ground truth on
  10.1525/collabra.23443, whose three one-sample `t(798)` rows move from
  N = 800 to N = 799, matching the gold's `n_total = 799` exactly.

Also verified on the fixed-3 canary: 10.1525/collabra.90203's `t(998)` pairwise
contrasts now bind N = 1000 (df+2, the two conditions actually compared) instead
of the paper-level 1004, and their reported-vs-computed CI mismatch shrinks
accordingly.

# effectcheck 0.6.16

**Nine audit findings from the 2026-08-04 canary sweep** (CI sign-alignment E3,
honest design labels E2, self-consistent deltas E4, Cochran-Q sample-size guard
E5, multiplicity-adjusted-p guard E6, own-clause N binding E7, reproducible
repro-code E8, and two recovered PARSE-MISS classes E10/E11). Two further audit
findings were REFUTED by local reproduction and documented rather than "fixed"
(see `docs/TRIAGE_iterate_2026-08-03.md`).

- **E7 / E-zrow-subsample-n** — a clause stating its own denominator
  (`"113/133 ... versus 20/133 ..., z = 7.98"`) now binds that N instead of a
  parent total scraped from the surrounding window. collabra.37122's
  reversal-subsample rows carried `N = 493` (the whole study) and published
  `r_from_z = 0.3382` where the correct N = 133 gives ~0.57, and `d_ind` 0.7188
  against a true 1.3839 — nearly double. A `SKIP` status did NOT contain this:
  `all_variants` values are surfaced to the reader regardless of status. The
  v0.6.8 own-clause preference is also generalized beyond t-tests (r-tests stay
  excluded so their best-N-by-p-value selection and "Multiple sample sizes" note
  keep working — caught by two existing tests when the exclusion was missing).

- **E8 / E-repro-output-vs-graded-value** — `repro_code` / `repro_output` now
  emit the variant and value the row's verdict was actually graded against.
  Independent-samples rows printed a crude `2 * stat / sqrt(df1)` approximation
  under a flat `d_ind` label while the row had been graded against
  `d_ind_equalN` / `g_ind`, diverging by up to 0.0256 on 7 of 15 t-test rows in
  collabra.77859. A user following the tool's own instruction to run the code
  got a different number than the verdict used — a direct violation of Design
  Principle 3. The approximation is retained but renamed `d_ind_approx` and
  marked "NOT the graded value".

- **E10 / E-bare-mediation-ci** — a bootstrapped mediation effect (ACME / ADE)
  reports a CI, never a Sobel Z, so the v0.6.10 Sobel-anchored pattern never
  fired and the result was dropped entirely. Now extracted with its CI (the
  `to` / `-` bracket separators are not in the generic CI pattern set) and
  routed to NOTE — a bootstrapped ACME is not recomputable from the reported
  numbers, so surfacing it honestly is the correct outcome.

- **E11 / E-bare-d-ci** — a post-hoc contrast reported as a bare
  `d = X, 95% CI [L, U]` with no test statistic (Scheffé / Games-Howell style)
  is extracted as a new `d_reported_only` test type. Six such contrasts in
  cog_emo produced zero rows. Last-resort only: a normally-reported t-test whose
  d carries a CI is still a fully-checked `t` row (pinned by test). The effect
  is bound from the CI-adjacent match rather than the first `d =` in the chunk —
  the whole-corpus render diff caught an intermediate version pairing one
  finding's `d = 0.39` with another's interval `[0.47, 0.62]`, a fabricated
  result worse than dropping the row.

**Earlier findings from the same sweep: CI sign-alignment (E3),
an honest design label for bare table rows (E2), a self-consistent
reported-vs-computed delta (E4), a Cochran-Q sample-size guard (E5), and a
multiplicity-adjusted-p decision-error guard (E6).**

- **E4 / E-delta-vs-matched-value** — `delta_effect` and `matched_value` must
  describe the same number. For a correlation-dependent variant (`drm` / `dav`:
  the value depends on the unknown within-pair correlation, so it is computed as
  a grid), the delta was measured against the nearest GRID POINT — the
  statistically honest question ("is the reported value achievable under some
  plausible r?") — while `matched_value` published the r-midpoint. The row
  therefore emitted three mutually contradictory numbers. Worst observed case:
  reported `0.64` vs published matched `0.888` with a stated gap of `0.015`,
  where the true gap to the midpoint is `0.248` (a 16x understatement in the
  field that drives the PASS/WARN/ERROR threshold and that users read as the
  reported-vs-computed gap). The grid point actually measured against is now
  published as `matched_value`, with its provenance in `assumptions_used`;
  verdict thresholds are unchanged (they always used the grid distance).
  `delta_effect` is also `unname()`d at all three assignment sites — it was
  carrying a stray variant-name attribute that serialized as an object rather
  than a bare number through the JSON API. Found by the Sonnet canary audit of
  collabra.57785 on 2 rows; a corpus-wide invariant check then found **11
  violations across 3 papers** (`drm`, `dav`, and `d_onesample`). Regression
  tests in `tests/testthat/test-v0616-delta-matches-matched-value.R`.

- **E5 / E-cochranq-global-n** — a Cochran Q heterogeneity row no longer adopts
  a sample size scraped from surrounding document context. Q is computed over a
  meta-analysis's effects (its "N" is k, the number of effects), so a host
  paper's participant count is a different quantity entirely: collabra.90203's
  `Q_T [40] = 104.65` carried `N = 1004` — the replication's own participant
  count — with fabricated `global_text` provenance. The guard is concept-based
  rather than source-list-based (an earlier draft listed only
  `global_text`/`extended_context` and still adopted a study-level `N` a few
  sentences away, which binds as `local_context`): there is no provenance under
  which a participant count legitimately becomes a Q row's N. Same class as the
  v0.5.14 Bayesian-model-averaged guard and the v0.5.18 `md_hl` guard, never
  extended to `cochran_q`. Found by the Sonnet canary audit of collabra.90203.
  Regression tests in `tests/testthat/test-v0616-cochranq-no-global-n.R`.

- **E6 / E-multiplicity-adjusted-p** — when the surrounding text states a
  multiple-comparison correction (Bonferroni / Holm / Šidák / Tukey / Scheffé /
  Games-Howell / Dunnett / Benjamini-Hochberg / FDR), a
  `reported_ns_computed_sig` decision error is a false positive: the reported p
  is ADJUSTED while the computed p is the raw per-test p, so the two are
  different quantities. collabra.90203's Bonferroni-corrected post-hoc
  `t(998) = 2.37, p = .053` was flagged WARN against a raw p of `.0180` — and
  `.0180 x 3 comparisons = .0539`, the reported value; the sibling rows
  corroborate (`t = 2.46` -> `.0422` vs reported `.041`; `t = 0.097` -> `2.77`
  clamped to the reported `p = 1.00`, a value unreachable without an
  adjustment). **Only that direction is suppressed**: a correction only ever
  makes p larger, so a `reported_sig_computed_ns` row stays flagged even under a
  stated correction, and the flag still fires when no correction is mentioned —
  both pinned by tests, because a broader guard would launder a real error
  class. The adjusted p is deliberately NOT re-derived (the comparison count is
  not reliably recoverable from prose); the row keeps its computed p, drops to
  NOTE, and states the reason. Found by the Sonnet canary audit of
  collabra.90203. Regression tests in
  `tests/testthat/test-v0616-multiplicity-adjusted-p.R`.

- **E-design-label-vs-dz (E2)** — a bare table row (Mode B: a typed table row
  handed over by the extractor) carries no design signal of its own: the table
  CAPTION that names the design is not attached to the row (DP-3 / DP-9 class).
  Such a row fell through every design-inference branch to the
  independent-samples default, which then CONTRADICTED the variant the check
  itself matched — collabra.23443 Tables 5/7 are one-sample t-tests whose only
  matching variant is `dz`, yet the row shipped
  `design_inferred = "independent"`. The verdicts were already correct (PASS via
  dz); only the metadata label lied. Such a row now reports
  `design_inferred = "ambiguous"` with an `uncertainty_reasons` entry naming the
  dz-only match. Deliberately NOT flipped to `"paired"` / `"one-sample"`: a dz
  match is consistent with both, and a genuine independent-samples row can match
  dz coincidentally, so asserting a within-design would overclaim a signal the
  text never delivered (conservative-when-ambiguous). The "does this row state
  its own design?" guard reuses the branch's existing `one_sample_patterns` /
  `paired_patterns` / `independent_patterns` vocabularies rather than a
  hand-copied subset — a Sonnet cross-model review (2026-08-04) caught an earlier
  hardcoded regex that omitted `"against chance"` / `"against the midpoint"` and
  would have ambiguated rows that DID state their design.
  Follow-through (Codex CLI review 2026-08-04, reproduced before fixing): the
  `repro_code` / `repro_output` emission now keys off the matched dz-family
  variant as well as the design label. An `"ambiguous"` row was still emitting
  the independent `d_ind <- 2 * stat / sqrt(df1)` formula, which on a df-less
  table row evaluates to `NA` — a user checking our work would have run a
  formula that does not reproduce the number the PASS is based on (same defect
  class as the v0.6.8 one-sample fix, newly reachable through the new label).
  Regression tests in `tests/testthat/test-v0616-e2-bare-table-design-ambiguous.R`.

**CI comparison follows the magnitude convention of the value match
(sign-alignment).**

- **E-ci-sign-align** — the effect-size value match has always been
  magnitude-based (`abs(computed)` vs `abs(reported)`), because the sign of a
  computed t-derived effect follows the arbitrary group-coding direction of
  the t statistic, not the paper's reporting convention. The CI comparison,
  however, compared SIGNED bounds — so a reported positive CI on a negative-t
  row was checked against a computed CI on the opposite side of zero, flagging
  a spurious `ci_check_status = "INCONSISTENT"` with fabricated deltas
  (collabra.23443 S1-R13: `t(1596) = -7.67, d = 0.19 [0.14, 0.24]` vs the
  computed r-scale CI `[-0.235, -0.141]`, deltas ~0.38). The best-candidate
  loop now also tries the sign-flipped candidate `[-cU, -cL]` when — and only
  when — the computed CI lies entirely on the opposite side of zero from the
  reported CI, keeping whichever aligns better. The alignment is never
  silent: `ci_method_match` carries a `:sign-aligned` suffix (surfaced in the
  app UI as "(sign-aligned)"). A zero-straddling CI is never flipped, and a
  genuine magnitude mismatch stays INCONSISTENT — flipping aligns direction
  only, it cannot shrink a magnitude discrepancy. The flip is additionally
  gated on the paper's own reporting being internally coherent — the reported
  point estimate must lie within its own reported CI. Without that guard a
  dropped-minus row (`r = -0.50, 95% CI [0.34, 0.63]`, the v0.6.3
  `sign_ci_violation` signature) was laundered into `MATCH` + PASS — found by
  a Codex CLI cross-model review (2026-08-04), reproduced locally, and pinned
  red-then-green. When no point estimate exists, the flip never fires
  (conservative: a false INCONSISTENT is recoverable; a false MATCH is not).
  Surfaced by the 2026-08-03 Sonnet canary audit of collabra.23443
  (escicheck-iterate cycle 5, finding E3); regression tests in
  `tests/testthat/test-v0616-ci-sign-align.R`.

# effectcheck 0.6.15

**Restatement guard for the v0.6.14 prose-dedup un-collapse + Mode B typed-n binding.**

- **E-modeb-t-n** — Mode B (`check_text(table_rows=)`) now binds a typed `n`
  field on a t-test table row. docpluck types a per-sample `n` column on rows
  that print n but NOT df (collabra.23443 Table 5:
  `{t: 16.6, d: 0.59, n: 799, CI [0.51, 0.66]}`); the t branch previously
  discarded it (only the r branch consumed `fields.n`), so such rows carried no
  N and fell to SKIP/insufficient_data even though the sample size was
  delivered. With N bound, the reported d verifies against the dz / d_ind
  variant family (Table 5's d = 0.59 matches dz = 16.6/sqrt(799) = 0.587 —
  SKIP -> PASS). Surfaced by the 2026-08-03 Sonnet canary audit; regression
  tests in `tests/testthat/test-v0615-modeb-t-n-binding.R`.

- **E-corr-two-prose-ci-gate** — v0.6.14's un-collapse of same-key parenthesized
  prose rows was CI-blind, so a RESTATED finding that repeats its own CI
  verbatim ("we ran a two-tailed paired t-test ... t(742) = 3.15, d = 0.15,
  95% CI [0.07, 0.22]" later restated as "Additionally, as reported in Study
  3A, ... t(742) = 3.15, d = 0.15, 95% CI [0.07, 0.22]" -- collabra.57785)
  double-counted as two results. The un-collapse now fires ONLY when the key
  group reports NO CI: an identical non-NA reported CI marks a restatement of
  one finding (a repeated report quotes its own CI; two genuinely-distinct
  results sharing stat, df, N, effect AND exact CI bounds is not a real case),
  which still collapses to the first parenthesized row. The collabra.23443
  H2A/H2C same-r-no-CI case (the v0.6.14 motivation) is unaffected -- both
  distinct correlations are still kept. Caught by the 2026-07-04
  whole-corpus baseline-vs-fixed render diff (57785: 23 -> 24 rows);
  regression test added to
  `tests/testthat/test-v0614-corr-two-prose-not-collapsed.R`.

# effectcheck 0.6.14

**Correlation deduplication correctness fix.**

- **E-corr-two-prose** — correlation deduplication now preserves two distinct
  prose findings that happen to have the same reported `r`, df, and inferred
  N. The earlier body-versus-table-fragment safeguard treated every matching
  parenthesized `r(df)` row as one finding, silently dropping a real second
  correlation when a paper reported the same numeric value for different
  variables. The deduplication now keeps all parenthesized prose rows and
  removes only their non-parenthesized table-fragment counterparts. Regression
  coverage is in `tests/testthat/test-v0614-corr-two-prose-not-collapsed.R`.

# effectcheck 0.6.13

**Two new test types + two canary-re-audit fixes from the 2026-07-02 escicheck-iterate cycles 2-3
(F1 bare Bayes factor + HR hazard ratio, both user-approved; independent Sonnet-watches-Opus canary
re-audit over the fixed-3 + rotating set).**

- **`bayes_factor`** (new `test_type`; cycle-2 F1) — a STANDALONE evidential Bayes factor reported as
  a PRIMARY finding of a RoBMA / Bayesian meta-analysis is extracted as an extraction-only NOTE
  surfacing the reported `BF01`/`BF10` (also the bare JASP/BayesFactor `B01`/`B10` form). The
  extraction is deliberately conservative: a bare `BF01 = <v>` matcher would flood every Bayesian
  paper (collabra.90203 alone prints 13+ `BF01 =` values, of which the gold wants only 2 as standalone
  results). A qualifying standalone BF must satisfy ALL THREE, evaluated per-occurrence over a bounded
  window around the BF's own position: (1) a primary-finding ANCHOR within 70 chars before it — one of
  "evidence (for|against) <finding>", "in favo(u)r of the (alternative|null)", or "Bayes factor
  (was|is|of|indicated|…)"; (2) NO co-located frequentist statistic within ±60 chars (excludes the F/t
  companions AND the model-averaged-r companion whose clause carries `r = 0.002`); (3) NOT about "(the|
  an|average|main) effect" (excludes the model-averaged-r companion and DV-specific complementary
  checks). Validated by a whole-corpus guard-live-vs-bypassed false-positive sweep: fires on exactly
  `BF01 = 0.11` (publication bias) + `BF01 = 1.24` (heterogeneity) on collabra.90203 and
  `B10 = 20841.04` + `B10 = 1.25` on collabra.32572, and ZERO spurious rows on the other corpus papers
  (including both SPPS Bayesian papers). Added to the `check_text()` `stats` allowlist + the Phase-9
  extraction-only-SKIP exclusion + API.md. Regression tests in
  `tests/testthat/test-v0613-standalone-bayes-factor.R`.

- **`hazard_ratio`** (new `test_type`; cycle-3 HR) — a Cox proportional-hazards / survival-analysis
  hazard ratio reported in a clean prose sentence — "HR = 1.87, 95% CI [1.54, 2.28], p < .01" (also
  `aHR` / `adjusted HR` / `hazard ratio`) — is extracted as an extraction-only NOTE surfacing the HR +
  its CI + p (a Cox HR is not independently recomputable from the reported numbers; it needs the full
  time-to-event data). The value must be TIGHTLY bound to the HR token by an explicit `=`/`:`/`of` and
  is forbidden from being a percentage (negative lookahead on `%`), and the standalone dispatch
  additionally requires a CO-LOCATED CI — so a bare "HR" mention, the "HR" heart-rate abbreviation, or
  the "95" of a nearby "95% CI" never fires. New bracketless medical/epi CI patterns
  (`95% CI 1.54-2.28` dash/en-dash/"to" range and `95% CI: 0.45, 0.85` colon-comma) bind the CI for a
  ratio effect (HR/OR/RR/IRR) when no bracketed CI is present. The p-back-derived CI recompute is
  suppressed for an HR so its extraction-only CI verdict is never a false INCONSISTENT (a Cox HR p is
  routinely an inequality; the reported CI is authoritative). Whole-corpus zero-FP sweep: 0 spurious
  `hazard_ratio` rows on all 12 papers. **Scope note:** s41598-023-50401-z's 58 hazard ratios are ALL
  in a docpluck-column-shredded survival table with no clean prose form, filed as docpluck DP-5
  (`docs/DOCPLUCK_HANDOFF_2026-07-02.md`) — a docpluck extraction defect, not an effectcheck parse gap.
  Regression tests in `tests/testthat/test-v0613-hazard-ratio.R`.

- **E-mcnemar-chisq-OR** (cycle-2 canary re-audit, collabra.37122 loc 305) — a 1-df chi-square whose
  ONLY reported effect size is an ODDS RATIO with a CI is a McNemar test, not a contingency /
  goodness-of-fit chi-square (whose canonical effect is phi / Cramér's V; an OR comes from the 2×2
  discordant-pair structure). Such a row now reroutes to `test_type = "mcnemar_or"` (an honest
  extraction-only NOTE surfacing the OR + CI), instead of staying a chi-square whose OR is "unusual for
  chi-square" and gets SKIPped as a likely extraction artifact. The mirror of the v0.6.5 rule ("a
  V-bearing chi-square is contingency/gof, never McNemar"). Gated to `df1 == 1` + an OR effect + a
  bound CI. Surfaced by an independent Sonnet re-audit (a Table-6 restatement of a McNemar finding the
  paper's 3 other McNemar rows report in prose); re-audit confirmed the fix (4 McNemar rows now match
  gold).

- **E-ownclause-2arm** (cycle-2 canary re-audit, collabra.57785 loc 167) — an independent (Welch)
  t-test whose OWN clause states two per-arm N's summing to the independent-samples total
  (`n1 + n2 - 2 = df1`) — "(M = 4.75, SD = 1.36, N = 393) … (M = 4.22, SD = 1.33, N = 350),
  t(741) = 5.36" (393 + 350 = 743 = 741 + 2) — now binds those two N's as `n1`/`n2`
  (`N_source = "own_clause_arms"`) and sets the total N to their sum, eliminating a false "N = 393
  implausibly small for df=741 (likely parsing error)" WARN and the empty `n1`/`n2`. The v0.6.11
  E-subgroupN context scan required EXACTLY two N's across ±2 sentences, but a stats-dense results
  section repeats the two arm N's in a neighbouring restatement (4+ copies), so that gate silently
  failed. Placed AFTER the df1 dispatch (df1 is not assigned until later in the parse loop). Surfaced
  + confirmed by an independent Sonnet re-audit.

- **E-welch-n-clamp** (cycle-3 canary re-audit, cog_emo loc 284) — the Welch global-N override
  back-computes N from the reported d (equal-groups `N = 4t²/d²`) when the bound N is a `global_text`
  value implausibly larger than the Welch floor `df + 2`. For a SMALL effect the equal-groups
  back-computation UNDERestimates N and can dip a few units below `df + 2`, which previously made the
  guard reject the override and keep the implausible global N — corrupting the recomputed d to the
  wrong sign/magnitude and firing a spurious WARN. The override now accepts a back-computed N within a
  plausible band of the Welch minimum (`≥ 0.85·min_N_welch`) and CLAMPS it up to `df + 2`. Three
  sibling Welch clauses in one sentence recovered N~530 but the 3rd (t=-1.93, d=0.17) kept the global
  N=794; now N=523, WARN→NOTE.

- **E-pairedci-indep-substring** (cycle-3 canary re-audit, cog_emo loc 284) — the v0.6.12 paired-CI-
  unverifiable guard's within-subjects keyword regex used an UNANCHORED `dependent samples`
  alternative, which matches as a substring of `INdependent samples`. So a genuinely independent-
  samples Welch clause ("We conducted independent samples Welch's t-tests") was falsely treated as
  within-subjects and its CI verdict capped at UNVERIFIABLE — masking a real reported-vs-computed CI
  discrepancy. Anchored with a negative lookbehind `(?<!in)`. HIGH severity: affected any
  independent-samples row with a computed CI. loc 284 now INCONSISTENT (correct); genuine
  within-subjects paired rows still UNVERIFIABLE.

- **E-corr-target-article-N** (cycle-3 canary re-audit, cog_emo loc 124) — a correlation explicitly
  attributed to the TARGET / ORIGINAL article — a value a replication reproduces from the paper it
  replicates (a "Table 2. Target article" intercorrelation block, or prose "the weakest effect in the
  target article … r = 0.36") — now carries the TARGET article's OWN sample size, not the current
  study's (global) N. For a bare `r` with no co-located N, the current study's global N is bound by
  default, which is wrong for a target-article statistic. When BOTH the context names the
  target/original article AND states that article's sample size ("target article's sample size of
  239"), N is rebound to that value (loc 124: N=794→239, df=237, CI MATCH). A current-study r keeps
  its own N. User-approved (bind the co-located target-article N).

Two new `N_source` values (`own_clause_arms`) and two new `test_type`s (`bayes_factor`, `hazard_ratio`)
are documented in API.md. Cycle-3's HR feature is orthogonal to the canary papers: 4 of the 5 canary
renders were byte-IDENTICAL to their cycle-2 PASS renders (a deterministic diff carries the prior
verdict), and cog_emo was re-audited PASS. Full suite 964 test_that blocks / 0 fail, `R CMD check
--as-cran` 0E/0W. Residual canary findings are all docpluck-boundary and filed to
`docs/DOCPLUCK_HANDOFF_2026-07-02.md` (DP-4 collabra.37122 loc-202 figure-caption CI truncation; DP-5
s41598 shredded survival table; DP-6 cog_emo garbled Table-7 duplicate; DP-3
collabra.57785 Table-8 Importance d/CI+design re-confirmed).

# effectcheck 0.6.12

**Three fixes from the 2026-07-02 escicheck-iterate cycle-1 canary re-audit (independent
Sonnet-watches-Opus over the v0.6.11 canary set), all on collabra.57785 (Experiential-vs-Material
Purchases replication+extension of Carter & Gilovich 2012).**

- **E-ownclause-N** (collabra.57785 loc 170) — a t-test's N is now taken from the row's OWN
  sub-chunk when its clause states one, instead of the first `N =` in the wider ±2-sentence
  context window. The clause "(M = 4.90, SD = 1.42, N = 743) ... (M = 4.11, SD = 1.44, N = 743;
  t(742) = 12.24, ...)" states `N = 743` twice, yet the parser bound `N = 350` from the PRECEDING
  sentence (loc 167 "N = 350 ... N = 393"), `check.R` rejected 350 as implausibly small for
  df=742, and fell back to the independent-samples default `N = df + 2 = 744` — a fabricated N.
  `parse.R` now scans the row's own sub-chunk `s` for `N =` first and binds it
  (`N_source = "own_clause"`) — but ONLY when that sub-chunk is a t-test (`t(` present) AND carries
  exactly one distinct N value. The narrow gate keeps the fix off the r-test's multi-N-candidate
  p-value-fit selection (its "Multiple sample sizes" path, where the sub-chunk splitter glues a
  preceding "N = ..." sentence to the r's chunk) and off between-groups clauses that legitimately
  carry two different N's. Mirrors the v0.6.8 "prefer the signal closest to / inside the row's own
  clause" discriminator.
- **E-repcol-dedup** (collabra.57785 Table 8 "Replication" column) — a test-statistic-bearing table
  row that carries NO CI of its own (docpluck delivered only the statistic + df) is now
  deduplicated against an identical prose finding. The v0.6.11 E-origcol fix dropped the
  "(Original)" comparison column but left the kept "(Replication)" rows undeduplicated against
  their body-prose twins, so 8 findings (t(742) = 17.61 / 24.00 / 30.74 / 31.08 / 53.32 / 12.24 /
  3.15 + Welch 5.36 / 2.51) were emitted TWICE — once as the richer prose row (with d + CI, PASS)
  and once as a bare table row (no ES, no CI). None of the three existing CI-based dedup passes
  could match a CI-less table row. `.dedup_table_vs_prose()` now collapses a CI-less table F/t/r
  row when its (test_type + statistic value + df1) matches a prose row's — for a CI-less row, df1
  is the discriminator the CI would otherwise provide. The 2 genuinely table-only rows (t = 3.93
  "Importance/Welch", t = 6.79 "Importance/Paired", no prose twin) are correctly kept
  (collabra.57785 32 → 23 rows).
- **E-pairedci-unverifiable** (collabra.57785 loc 170) — a within-subjects (paired) t reported with
  a CI the authors computed from the raw paired data (e.g. `d = 0.55, 95% CI [0.47, 0.62]` on a
  paired `t(742) = 12.24`) is no longer flagged `ci_check_status = "INCONSISTENT"`. effectcheck
  cannot reproduce a paired / d_av CI from t + df alone (it lacks the per-arm SDs and the within-pair
  correlation), so its computed CI is an independent-samples over-approximation; comparing the
  reported paired CI to that approximation and declaring INCONSISTENT falsely implies the reported
  values are wrong. `check.R` now records when a row's CI could only be computed as an independent
  approximation, and — for a within-subjects row (paired/within design keyword in the row's own
  clause/context, or a dz/dav/drm effect) — caps the CI verdict at `UNVERIFIABLE` instead of
  escalating to INCONSISTENT. A MATCH / PLAUSIBLE still stands when the approximation lands close
  (loc 151, delta ~0.03, stays PLAUSIBLE), and genuine independent-samples rows can still surface
  INCONSISTENT (the within-design guard scopes the cap).

Full suite **924 test_that blocks / 0 fail**; `R CMD check --as-cran` 0E/0W. Regression tests in
`tests/testthat/test-v0612-ownclause-n-and-repcol-dedup.R`. Two docpluck text-extraction defects
were filed (NOT effectcheck defects) to `docs/DOCPLUCK_HANDOFF_2026-07-02.md`: DP-1 (collabra.77859
`camelot_t10` Study-1 Table-1 binds the wrong column as t/d/df/CI — delivered `t = 0.6` where the
gold reads `t = 5.65`, so effectcheck faithfully rendered docpluck's wrong values), DP-2
(collabra.77859 "Expensive" manipulation-check row `t = 15.57` not delivered as a flattened_row). A
standalone-Bayes-factor gap on collabra.90203 (bare `BF01 = 0.11` / `1.24` not extracted) was
surfaced for a product decision rather than fixed — the paper reports 13 `BF01 =` values of which
the gold wants only 2 as standalone results, and no parse-pattern rule reliably separates the 2
primary-analysis Bayes factors from the 11 supporting/companion ones (see
`docs/TRIAGE_iterate_2026-07-02.md` F1).

# effectcheck 0.6.11

**Two fixes from the 2026-07-01 escicheck-iterate cycle-2 canary audit (independent
Sonnet-watches-Opus over the v0.6.10 canary set).**

- **E-origcol** (collabra.57785 Table 8) — the original/comparison column of a
  replication-vs-original summary table is not always labeled "Original study/article".
  collabra.57785 Table 8 tags its two columns "Replication Effect and CI" / "Original
  Effect and CI" and its rows "… (Replication)" / "… (Original)", so the v0.6.6
  comparison-column filter (which required `original` + `article|study|paper`) missed them
  and every one of the 11 Table-8 findings was emitted TWICE — the Original-column copy
  leaked as a spurious own-result (43 → would-be-32 rows). The `comparison_col_re` in
  `flattened_rows_to_parsed()` now also matches the column-header forms ("Original Effect /
  Result / Finding / Value / Cohen's d / r / F", "Original … CI/[stat]") AND a standalone
  parenthetical row-tag "(Original)" / "(Target article)", while still keeping the paper's
  own "(Replication)" column and any substantive condition label that merely contains the
  word "original" in prose. collabra.57785 drops from 43 to 32 rows (11 duplicate Original
  findings removed); independent Sonnet re-audit no longer reports the duplication.
- **E-mdhl-N** (PROSECCO md_hl) — a Hodges-Lehmann median difference has no recoverable N
  from a sentence (it needs per-arm rank data), and the per-arm n's are typically not in
  the result's own clause. The v0.6.3 fix that stopped RR/rdpct rows inheriting an
  unrelated global N (via `arm_totals_sum`) was never extended to `md_hl`, so a md_hl row
  still attached a bled `global_text`/`extended_context` N — PROSECCO showed an unrelated
  N=106 on two distinct median-difference outcomes the source never quantifies. `check.R`
  now clears a non-co-located N (and its `N_source`) for a md_hl row, mirroring the
  interaction_p / mediation_indirect handlers.
- **E-subgroupN** (collabra.74820) — an independent t-test with two unsubscripted per-group
  sizes ("high CA (N = 223) and low CA (N = 19)") bound only the first (223) as the TOTAL N,
  firing a bogus "N=223 implausibly small for df=240" WARN and forcing an equal-split Cohen's d
  (-0.30 vs the true -0.55). `parse.R` now binds two split-context N values as n1/n2 (N = their
  sum, `N_source = "subgroup_sum"`), guarded by a group-split keyword + an exactly-two-N
  requirement (a lone total N is untouched).
- **E-mcnemar-OR** (collabra.37122) — a McNemar test reported only as a discordant-pairs ODDS
  RATIO with no chi-square value ("We also conducted a McNemar test … OR = 0.18, 95% CI
  [0.10, 0.29], p < .001") produced NO result row — all four of the paper's McNemar tests were
  silently dropped. New `test_type = "mcnemar_or"` + `pat_mcnemar_or` (case-insensitive, "McNemar"
  + an "OR = …" anchor within one sentence) routes the row to an extraction-only NOTE surfacing
  the OR + CI + p. Recovers 3 of the 4; the 4th has its "McNemar" anchor severed from its OR
  clause by a docpluck paragraph-break (filed to docs/DOCPLUCK_HANDOFF_2026-07-01.md DP-1).

Full suite 917 test_that blocks / 0 fail, `R CMD check --as-cran` 0E/0W. Regression tests
in `tests/testthat/test-v0611-origcol-and-mdhl-n.R`. Surfaced alongside three new corpus
golds generated via article-finder for the deeper audit (collabra.74820 neuroticism×EC
moderation, collabra.122515 creativity-depression mediation, collabra.88158 daily-diary
multilevel — the last dropped from the audit set due to a source-PDF binding defect:
pages 7-12 are a mis-bound different article).

# effectcheck 0.6.10

**A bootstrapped mediation indirect effect reported with a Sobel Z is now a first-class
`test_type = "mediation_indirect"`, from the 2026-06-29 escicheck-iterate new-corpus pass
against the Outcome Bias replication+extension (collabra.126266, Aiyer/Chan/Feldman 2024).**

A clause like *"the bootstrapped indirect effect of X on Y was .05, 95% CI [-.04, .12],
Sobel Z = 0.84, p = .40, ACME found to be robust until ρ = 0.7"* previously routed the
`Sobel Z = 0.84` to a PLAIN z-test, and then the fallback effect-size pattern grabbed the
sensitivity-analysis `ρ = 0.7` — the value of the error-term correlation at which the ACME
mediation stops being robust (an Imai/Keele/Tingley sensitivity bound) — as the EFFECT
SIZE, discarding the actual indirect effect (.05) and emitting a spurious WARN. All four
mediation rows (H2 + H5) were mis-typed `z` with `effect_reported_name = "rho"` and
`effect_reported` = the sensitivity bound.

`parse.R` adds `pat_mediation_indirect` (anchored on *"indirect effect … was <value>"* AND
*"Sobel Z = <z>"*, both required) and a dispatch branch that classifies the row
`mediation_indirect`, binds the indirect-effect coefficient as the reported effect
(`effect_reported_name = "indirect_effect"`), the Sobel Z as the test statistic, and the
bootstrapped CI as the indirect-effect CI (anchored at the indirect-effect value, before
the trailing ρ). An `is_mediation_indirect` flag suppresses the fallback-ES ρ grab.
`check.R` routes `mediation_indirect` to an honest extraction-only NOTE (the indirect
effect is not recomputable from the reported numbers without the a/b path coefficients) and
excludes it from the Phase-9 SKIP downgrade so the indirect effect + CI are surfaced.
`mediation_indirect` is added to the `check_text()` `stats` allowlist and documented in
API.md. The same new-corpus audit confirmed all 28 other reported statistics
(replication/extension ANOVAs + Welch t post-hocs) are extracted correctly and the gold's
Table-5 "Original" (Gino 2009 comparison) rows and abstract-only effect-size restatements are
correctly NOT extracted.

It also fixes the malformed **`p = <.001`** form (a spurious `=` immediately before the real
`<`/`>` operator — a common PDF text-layer artifact) in `pat_p` / `pat_p_sci` / `pat_p_enote`:
the operator group now accepts an optional leading `=` ONLY when a real `<`/`>` follows (a
lookahead), so `p = <.001` parses to `p < .001` while a normal `p = .40` still captures `=`
and `p <= .05` still captures `<=`. Surfaced by the same collabra.126266 H5 punishment
mediation row, where docpluck delivers "Sobel Z = 4.87, p = <.001" (the PDF prints "p < .001")
and the p had been dropped (`p_valid = FALSE`). Full suite 909 test_that blocks / 0 fail,
`R CMD check --as-cran` 0E/0W. Regression tests in
`tests/testthat/test-v0610-mediation-indirect-sobel-z.R`.

# effectcheck 0.6.9

**A `=`-as-U+00BC glyph-corruption normalization, from the 2026-06-29 escicheck-iterate
new-corpus pass (SPPS "Inaction Inertia" replications, 10.1177/1948550619900570).**

Some PDFs encode the `=` glyph such that the text layer emits **U+00BC ("¼", the fraction
one-quarter)**. A whole paper can come through with EVERY equals sign as U+00BC and no real
`=` at all (this SPPS paper: 120 U+00BC, zero `=`), so `t ¼ -7.81`, `F (3, 1791) ¼ 200.12`,
`d ¼ 0.57`, `M ¼ 20.20` all parsed to nothing — the entire body-prose statistics surface was
invisible. `normalize_text()` now folds U+00BC → `=` ONLY in a statistical-operator position
(flanked by whitespace and adjacent to a value / sign / bracket / a stat-word like
"confidence"), so a genuine one-quarter fraction in prose ("¼ cup of sugar", "¼ of
participants") is NOT rewritten. This is the same class of character-level normalization as
the existing U+2212-minus and U+FFFD-eta-squared recovery. +11 results recovered on the SPPS
paper; ZERO change on the canary + sweep corpus (they contain no U+00BC). Regression tests in
`tests/testthat/test-v068-equals-glyph-u00bc.R`. The corruption is also filed to docpluck
(`docs/DOCPLUCK_HANDOFF_2026-06-29.md` §5) as the preferred upstream fix so all consumers
benefit. Full suite 904 test_that blocks / 0 fail; `R CMD check --as-cran` 0E/0W.

The same SPPS new-corpus audit filed four docpluck table-extraction defects (sign-stripped
negative table-cell t-values, a `camelot_t11` t→F + d→p mis-typing of pairwise tests, an
undelivered df column on Table-4 ANOVAs, and figure-embedded forest-plot estimates) — none of
which are effectcheck defects; see the handoff §5.

# effectcheck 0.6.8

**Six parser/classification fixes from the 2026-06-29 escicheck-iterate canary audit
(independent Sonnet-watches-Opus over the Collabra / PCI-RR / PLOS-Med canary set).**

- **RoBMA model-averaged `r` now routes to NOTE, not SKIP** (E-C1-regress). The v0.6.6
  block set `effect_reported_name = "r_model_averaged"` and intended an honest NOTE,
  but its status guard omitted `"SKIP"` AND the Phase-9 extraction-only SKIP downgrade
  re-overrode the NOTE (a model-averaged `r` carries no p and no adopted effect, so it
  reached both rules at status `SKIP`). Added `"SKIP"` to the guard and a
  `bayes_model_avg_surfaced` exclusion to the Phase-9 downgrade (mirroring
  `r_ci_surfaced`). collabra.90203 `r = 0.002, BF01 = 14.93` now NOTE.

- **A docpluck Mode B joint-evaluation table row is classified `paired`/`within`, not
  `independent`** (E-A3). The design lives in the table NOTE ("Paired-samples t for
  joint"), which docpluck does not carry onto the flattened row — the row carries only
  its `group`/`row_label` column label. `flattened_rows_to_parsed()` now injects a
  within/paired (joint) or between/independent (separate) design phrase derived from
  the column label into the row's `context_window`, and renames a within-row's
  docpluck-generic `d` effect to `dz` (table note: "d_z for paired"). collabra.77859 /
  collabra.57785 Table-3 joint `t(131)` rows independent→paired.

- **A prose t-test reporting a paired effect family (`dz`/`dav`/`drm`) in its own clause
  is no longer forced `independent` by a Welch / independent signal that BLED from a
  neighboring sentence's test** (E-A3 prose). collabra.77859 Study 2 joint
  `t(131) = 6.92 (dz = 0.60)` independent→paired; collabra.57785 `t(741) = 5.36` (a plain
  `d` with a same-clause Welch) correctly stays `independent`.

- **The table-vs-prose dedup no longer collapses two distinct findings that share an `F`
  and a rounded CI but differ in their reported p** (E-D-dedup). The
  `.dedup_table_vs_prose()` test-statistic key now includes the reported p. collabra.90203
  H2b (donations interaction `F(2,998) = 1.48, p = .228, η²p = .003`) is recovered instead
  of being merged into H6 (`F(2,998) = 1.48, p = .229`); the intended glyph-stripped
  H5b/H5c collapse (whose p's agree) is preserved.

- **A bare "p-value for interaction <op>? <pval>" report is extracted as an
  extraction-only NOTE** (E-interaction-p) under the new `test_type = "interaction_p"`
  (`pat_interaction_p`). A subgroup / moderation interaction carrying only a p, with no F
  / df / effect size (the F lives in a supplement), surfaces the p rather than being
  dropped. PLOS Medicine PROSECCO trial `p-value for interaction 0.029`.

- **A one-sample t-test mislabeled `independent` because its "one-sample t-test against
  the {scale midpoint|chance|N}" declaration sits outside the per-row ±2-sentence context
  window is now classified `one-sample`** (E-A1). `parse.R` builds a section-scoped
  one-sample carry-forward map (two-tier: a plain declaration reaches the next ≤4 chunks;
  a multi-scope "for each of the sub-questions/items/…" declaration reaches ≤18 chunks,
  past an interleaved table the PDF flattened between body paragraphs), cancelled by an
  intervening prose "we ran a paired / independent / Welch t-test" declaration.
  collabra.57785 Study 3B/3C 4 one-sample `t(742)` rows independent→one-sample. Two
  long-standing one-sample false positives were fixed in the same change: a one-sample
  signal living ONLY in a trailing "Table N." caption, or ONLY in a FOLLOWING sentence
  describing other tests, no longer relabels a preceding paired test (rsos.250908
  `t(801) = 8.73`, collabra.23443 `t(798) = 23.7` → paired, gold-correct), while a
  one-sample signal in the row's OWN clause (collabra.23443 `t(604) = 19.9` against
  `mu = 0`) is always honored.

- **A t-test reported as a bare CONTINUATION of the previous test's sentence now inherits the
  preceding sibling's design** (E-A1 continuation). "…t(798) = 23.7 … for the Prolific sample
  and t(798) = 24.3 … for the MTurk sample" splits the second test into its own sub-chunk with
  no design signal, so it defaulted to `independent`; `check_text()` now propagates a determined
  paired / within / one-sample design from the immediately-preceding prose t-row when they share
  `df1` and the reported effect-size name and the continuation row carries no design keyword of
  its own. collabra.23443 `t(798) = 24.3` independent→paired (gold: within-subjects), and the
  `t(1599) = 12.49 / 33.89` continuations of one-sample tests independent→one-sample — all
  gold-correct, zero canary changes.

Full suite 901 test_that blocks / 0 fail; `R CMD check --as-cran` 0E/0W. Regression tests
in `tests/testthat/test-v068-*.R` (6 files). One residual item filed (non-canary): collabra.23443
Table-5's 4 one-sample-vs-mu=0 rows arrive as docpluck flattened rows whose one-sample design
lives only in surrounding body prose (not on the row) — routed to `docs/DOCPLUCK_HANDOFF_2026-06-29.md`
(docpluck enhancement: carry the table's introducing design onto the flattened row), alongside the
docpluck table-shred / untyped-est handoffs.

# effectcheck 0.6.7

**Consumes the two newly-typed docpluck v2.4.98 table fields (`fields.eta2` /
`fields.r`), the reply to ESCImate's 2026-06-25 docpluck handoff (DP-3 / DP-5).**
docpluck v2.4.98 now types the partial-η² column on a structurally-identified
ANOVA table (`fields.eta2`) and types correlation-matrix cells (`fields.r` with
rejoined CIs). Verified by re-extracting collabra.90203 + cog_emo from **live
docpluck v2.4.98** (`/api/version` → `library.version 2.4.98`) and checking against
the AI stats gold. Full suite 877 test_that blocks / 0 fail, `R CMD check --as-cran`
0E/0W. Regression tests in `tests/testthat/test-v067-docpluck-v2498-eta2-r.R`.

- **A typed `fields.eta2` on a docpluck table F-row is now bound as the reported
  partial-η² (`etap2`) instead of being discarded.** Previously the consumer left the
  partial-η² unbound because docpluck emitted it untyped; v2.4.98 types it (DP-3), so
  `flattened_rows_to_parsed()` binds it as `etap2` — the same canonical reported name the
  prose parser produces — and the row flows through the existing partial-η² verification
  path. When the table row carries `df1`+`df2` the value is **recomputed from F and
  verified** (collabra.90203 Table 9 H5d `F(2,998)=0.792, η²p=.002 [.000,.009]` → PASS);
  when df is absent it routes to an honest NOTE that surfaces the η²p + CI. This is the
  **only** recoverable source of η²p for this paper — the body-text symbol is absent from
  the delivered text, leaving a bare `( = .000, …)` (OCR / shape-recognition tier).
  *(Mechanism corrected 2026-08-05: this entry originally read "the body-text glyph has no
  ToUnicode CMap … docpluck OCR-tier won't-fix, confirmed". The verdict stands but that
  reason is **refuted** — the symbol is 4 filled vector curves with no char object, not a
  mis-encoded font glyph — and "confirmed" rested on a circular check (counting `η` in the
  extracted text, i.e. asking the extractor under suspicion whether its own output was
  complete). See `docs/REPLY_FROM_DOCPLUCK_2026-06-25.md`.)*
  An effect-only ANOVA cell (typed `eta2` + CI, blank F) becomes a `table_estimate` row
  named `etap2`. An UNtyped `est` is still left unbound (no regression).

- **A typed `fields.r` correlation cell reported with a CI but no df/N now surfaces as a
  NOTE (r + CI shown, estimate-in-CI invariant checked) instead of collapsing to a bare
  SKIP.** docpluck's typed Table-10-style r-cells (DP-5) arrive with their CI but no usable
  N — docpluck mis-binds the per-row `n` to the comparison column (filed back to docpluck in
  `docs/REPLY_TO_DOCPLUCK_2026-06-26.md`). Such a row adopts the r as its own effect and its
  reported CI is consistency-checked (a dropped-minus / r-outside-CI is flagged via
  `sign_ci_violation`), so SKIP ("nothing was checked") understated it; it now stays NOTE.
  An r-cell with neither a CI nor df/N is unchanged (conservative no-CI route).

- **The prose↔table dedup now also collapses a test-statistic-bearing table row (F/t/r)
  that restates a glyph-stripped prose row, matched on the (statistic + CI) signature.**
  Because docpluck strips the prose effect-size glyph, a body F-row ends up with only
  `{F, CI}` while the now-typed table row carries `{F, η²p, CI}`; the signatures differ by
  the η²p term alone, so neither the full-signature nor the `table_estimate` CI-only dedup
  collapsed them and the same H-test surfaced twice (collabra.90203 H5b/H5c). A table F/t/r
  row is now dropped when BOTH its statistic value AND its reported CI pair match a prose
  row's — a stronger same-test signature than CI alone (df is intentionally excluded from
  the key, since docpluck's table df can disagree with the body's while the F + CI still
  pin the identical finding).

# effectcheck 0.6.6

**Six parser/classification fixes from the 2026-06-25 escicheck-iterate canary audit
(double independent Sonnet audit over the 5-paper canary set, after reconciling the
2026-06-23 Dropbox ai_gold conflict and regenerating the collabra.90203 + cog_emo
stats golds).** Verified against the AI stats gold; full suite 868 test_that blocks /
0 fail, `R CMD check --as-cran` 0E/0W.

- **A `robust_bayesian_meta_analysis` / Bayesian model-averaged effect reported as
  `r = value` is no longer flattened to a plain Pearson correlation marked PASS.** A RoBMA
  model-averaged estimate (e.g. collabra.90203 "model-averaged mean effect size estimate of
  r = 0.002, 95% CI [0, 0.004]" with `BF01 = 14.93`) is a posterior estimate accompanied by
  a Bayes factor — not a frequentist r recomputable from df. `check.R` now detects the
  model-averaging phrase in the r's OWN clause (not the wider context — same near-cue
  discipline as the Pearson/Spearman fix) and routes the row to NOTE with
  `effect_reported_name = "r_model_averaged"` and an explicit Bayesian-nature note, instead
  of letting the r-adopts-itself block mark it a verified PASS. Regression tests in
  `tests/testthat/test-v066-robma-model-averaged-r.R`.

- **The Mode B docpluck table-row consumer no longer extracts a "Target article" /
  "Original study" comparison-column row as one of the audited paper's own results, and
  deduplicates a `table_estimate` row that restates a body result by its CI.** A
  replication/extension paper often prints the original study's statistics in a comparison
  column beside its own "Replication" column; docpluck flattens both, and the consumer
  ingested the original values as the paper's findings (collabra.90203 surfaced Small et al.
  2007's Table-8 `F = 6.75 / 5.32` and the Table-10 Target-article correlations as spurious
  own-result rows). `flattened_rows_to_parsed()` now drops a row whose `row_label`/`group`
  marks it as the comparison/original column (10 spurious rows removed; all 12 Replication
  rows kept). Separately, `.dedup_table_vs_prose()` now also collapses a `table_estimate`
  row whose exact reported CI pair matches a prose row's — catching a restatement the full
  numeric-signature dedup missed because the body row's effect size was stripped by docpluck
  (collabra.90203 η²p = .01, CI [0, .021] restating body `F(2,998) = 3.91`). Regression tests
  in `tests/testthat/test-v066-table-comparison-column-and-dedup.R`.

- **A bare correlation `r(df) = value, 95% CI [..]` with NO co-located p-value now adopts
  the r as its own reported effect and verifies via the CI, instead of dropping to SKIP.**
  The r-adopts-itself-as-effect block in `check.R` (a correlation's r IS its effect size)
  only fired when `check_type == "p_value"` (a p was present). A CI-only row had
  `check_type == "extraction_only"`, so the adoption was skipped, `effect_reported` stayed
  NA, and the row went to SKIP — even though the r is the effect and the reported CI gives
  a verification path (the v0.5.10 bare-r-with-CI form). collabra.57785's Discussion summary
  `r(741) = -0.43, 95% CI [-0.49, -0.37]` / `r(741) = -0.44, 95% CI [-0.50, -0.38]` (no
  co-located p) are now verified. The adoption requires a CI when no p is present, so a truly
  unverifiable bare `r(df)` (no p, no CI) still routes to extraction-only. Regression tests
  in `tests/testthat/test-v066-bare-r-ci-no-p-effect-adoption.R`.

- **A body-text Pearson `r(df)` is no longer mislabeled `spearman` because a DISTANT table
  note mentions "Spearman's rho".** The Stage-1/P2 reclassification cue (a plain `r(df)` in a
  Spearman/Kendall context routes to the rank path) was computed over `paste(s, context)` —
  the wide context window — so cog_emo's (Chan & Feldman 2024) body Pearson
  `r(261) = -0.43, 95% CI [-0.52, -0.33]` was tagged `spearman` purely because a far table
  note read "Format: Pearson's correlations [CI] (Spearman's rho)". The reclassification cue
  now reads only `s` (the immediate sub-chunk containing the r), matching the documented
  "cue near the statistic" intent — an explicit near-statistic cue ("A Spearman correlation
  was computed, r(20) = 0.50") still reclassifies; the Gap-4 Spearman-CI offer still consults
  the wider context separately. Regression tests in
  `tests/testthat/test-v066-pearson-not-spearman-context-bleed.R`.

- **ANOVA design classification no longer mislabels a within-subjects / repeated-measures
  ANOVA as `between`.** (Details below — this was the cycle-1 fix.)

- **ANOVA design classification no longer mislabels a within-subjects / repeated-measures
  ANOVA as `between`.** The `tt == "F"` block in `check.R` keyed `design_inferred` on the
  bare word "between" *first* (first-keyword-wins), so a within-subjects ANOVA discussed in
  a multi-sentence context window that *also* mentioned a between-subjects comparison was
  mislabeled `between` — collabra.57785's "2 (purchase type) x 2 (feeling time)
  within-subjects two-way ANOVA" rows `F(1,742) = 101.10 / 54.70 / 5.54` (gold: repeated-
  measures 2x2) were all tagged `between`. The classifier now (1) recognizes a **definitive**
  signal — the design keyword directly modifying "ANOVA" ("within-subjects two-way ANOVA",
  "repeated-measures ANOVA", "between-subjects ANOVA"), which wins over a stray opposite
  keyword elsewhere in the window (mirrors the v0.6.5 t-test `definitive_independent_t`
  rule); and (2) in the fallback, gives a `within-subjects`/`repeated measures` **design
  phrase** precedence over the bare preposition "between" (which appears in non-design
  English like "interaction between purchase type and feeling type"), while still labeling
  `between` from a bare "between" when no within design phrase is present — preserving
  collabra.90203's genuine 2x3 between-subjects ANOVAs whose delivered text dropped the
  contiguous "between-subjects" token. (NB: the runner's default TRE regex engine does not
  honour `\b` word boundaries, so "ANOVA" is bounded with an explicit non-letter group.)
  Whole-corpus re-verification confirmed the three collabra.57785 rows flip `between`->`within`
  with zero design-label regressions on collabra.90203 / collabra.77859. Regression tests in
  `tests/testthat/test-v066-within-anova-design.R`.

# effectcheck 0.6.5

**Five parser/classification fixes from the 2026-06-21 escicheck-iterate canary audit
(7-paper Collabra/RR/PLOS-Med set, independent Sonnet verification of the docpluck
v2.4.95 production path).** All verified against the AI stats gold; full suite 2117
pass / 0 fail.

- **Chi-square sub-type: a goodness-of-fit / contingency chi-square that reports a
  Cramér's V is no longer mislabeled `chisq_subtype = "mcnemar"`** when a *separate*
  "We also conducted a McNemar test, ... OR = ..." clause shares its sentence. A McNemar
  test yields an odds ratio from discordant pairs, never a Cramér's V, so a chi-square
  carrying a reported V is contingency/gof regardless of co-occurring "mcnemar" text.
  The reported V is now computed and verified instead of routed to a "not recoverable"
  NOTE (collabra.37122: 4 reversal tests). The sub-type classifier was further
  hardened to distinguish goodness-of-fit (one variable vs a 50-50 / chance
  baseline) from a test of independence (two categorical variables): it reads the
  chi-square's OWN sentence (`raw_text`) first — where "test of independence" /
  "goodness of fit" sit — and falls back to the wider context only for the
  unambiguous gof signal, never a bled independence keyword. This resolves the
  context-window keyword bleed that had reversal tests tagged contingency and
  "test of independence" rows tagged gof (collabra.37122: all 20 body chi-square
  rows now match gold). gof and contingency compute an identical Cramér's V for a
  1-df table, so the reported-V verification is unaffected — only the label.
  (`check.R`)
- **Design inference: an explicitly independent Welch / independent-samples / two-sample
  t-test is no longer mislabeled `design_inferred = "paired"`** when the multi-sentence
  context window also mentions within-subjects analyses. "Welch's t-test" is by
  definition unequal-variance independent-samples; that signal now wins over a stray
  "paired"/"within" keyword. The v0.6.3 E2 fix only caught fractional Welch df; this
  catches the integer-df case (collabra.57785: t(741) rows). (`check.R`)
- **Standardized-coefficient binding: a `beta = X` that PRECEDES its t in a
  "(beta = X, t(df) = Y, p)" clause now binds to THAT t**, not the next clause's beta.
  The sub-chunk splitter previously stranded the preceding beta in the prior sub-chunk
  (cog_emo: t(260) = 11.32 took beta = 0.91 instead of 0.74). (`parse.R`)
- **Bare binomial: a "binomial[ test]: p [op] X" with no Cohen's h** is now extracted as
  an extraction-only NOTE rather than dropped (guarded to fire only when no Cohen's h
  co-occurs). The prose dedup key also now includes the reported p for "thin" rows with
  no test statistic, so two distinct bare binomials no longer collapse to one
  (collabra.77859: Study 1 p = .002, Study 4 p = .047). (`parse.R`)
- **`render_report()` no longer warns "Unknown or uninitialised column"** on a minimal
  result tibble — four column accesses (`insufficient_data`, `variants_tested`,
  `uncertainty_reasons`, `assumptions_used`) are now membership-guarded. (`report.R`)

Regression tests: `test-v065-mcnemar-subtype-guard.R`, `test-v065-welch-not-paired.R`,
`test-v065-beta-precedes-t-binding.R`, `test-v065-bare-binomial.R`,
`test-v065-chi-subtype-gof-vs-independence.R`. The canary-audit
harness `scripts/render_for_audit.R` was also fixed to pass `table_rows` (mirroring the
production `/process` path) so the audit exercises the v0.6.4 Mode B consumer.

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
