# Numeric separator normalization spec

**Spec version:** `1.5.0`
**Derived from:** originally docpluck `normalize.py` steps A3a + A3 (as of docpluck
v2.4.126). **Those steps no longer exist.**
**Status:** **effectcheck is now the ONLY implementation.** This document records the
semantics effectcheck implements and tests against; it is no longer a two-implementation
contract awaiting docpluck's adoption.

## Status change, 2026-08-21 (effectcheck v0.7.6)

docpluck **deleted its entire EU→US separator machinery**: `A3` (decimal comma) and
`A3a` (thousands strip) in v2.4.129–v2.4.130, along with `A2`, `A3c`, `A3d`, `W0n`, and
the whole document-level locale feature (`infer_numeric_locale`, `NumericLocale`,
`NormalizationReport.numeric_locale`). Verified against the live library on 2026-08-21:
`d = 0,80`, `U = 12,345`, `N = 185,178` and `M = 1,234.56` are all delivered **verbatim**.

Three consequences, all of which change how this document should be read:

1. **The provenance line above is history, not a citation.** The steps this spec was
   derived from are gone. Nothing here can be checked against docpluck's implementation
   any more, because docpluck has no implementation.
2. **effectcheck's locale inference now works BETTER, not worse.** docpluck's 2026-08-13
   outbox argued that rules L1/L2/L3 "cannot fire in the real pipeline", because
   docpluck's own conversion inverted the evidence the inference votes on. That was true
   when written and is now false: docpluck no longer touches the tokens the detector
   reads, so the detector sees the source convention. v0.7.5's `conflict`-state work is
   validated by this change, not obsoleted by it.
3. **`REQUEST_TO_DOCPLUCK_normalization_spec.md` is moot** and should be closed rather
   than sent. Its central ask — that docpluck distinguish `conflict` from `none` when
   gating on an inferred locale — asks docpluck to fix a rule it has deleted. The two
   cases it raised where we believed docpluck's lookahead was wrong are likewise moot.

**Rule D1's integer-part bound changed in this release, and the SPEC yielded to the
code rather than the other way round.** docpluck correctly reported that this document
called "exactly one digit before the comma" the single most important constraint while
the shipped regex allowed `(\d+)`. Both were wrong. One digit is too strict to ship — a
continental paper really does write `M = 12,34` and `t = 1234,56`, and an earlier
one-digit port silently stopped converting them — while unbounded produced
`9999999,1 → 9999999.1`. The integer part is now capped at **four digits**, which covers
the real continental shapes and refuses the rest; rule T1 has already removed every
unambiguous thousands group by the time D1 runs, so nothing longer can be a decimal.

## Why this exists

Academic text uses the comma for two incompatible purposes:

- **European decimal separator** — `d = 0,80` means zero point eight.
- **English thousands separator** — `U = 12,345` means twelve thousand.

Resolving one breaks the other. Two implementations of that resolution existed
independently — docpluck's (Python) and effectcheck's (R) — and they diverged.
effectcheck's was provably wrong: `U = 12,345` became `12.345`, which published a
rank-biserial correlation of `0.99938` where the truth was `0.38275`, with status
`OK`. docpluck's handled the same input correctly.

The divergence was not an accident of carelessness. effectcheck *asked* docpluck
to fix this (the request is still cited in docpluck's source as "ESCImate
Request 1.1"), docpluck implemented it more generally, and effectcheck never
retired its own narrower copy. **Two implementations with no shared test cannot
stay in sync.** This spec plus `conformance.json` is that shared test.

## Design note: why the rules are documented, not executed, as data

The conformance **corpus** is language-neutral data. The **rules** are specified
here in prose plus reference regexes, and implemented natively in each language.

Executing shared regex strings across PCRE (R, `perl = TRUE`) and Python `re`
would be a false economy: the dialects differ in lookbehind support, character
class semantics, and Unicode handling, so an identical pattern string is not a
guarantee of identical behaviour — it merely *looks* like one. The corpus is what
actually proves the two agree. This is the CLDR model: shared data and a shared
conformance suite, native implementations.

## Rule T1 — protect thousands separators (runs FIRST)

Strip the comma from any integer that is unambiguously thousands-grouped, so the
decimal rule below never sees it.

```
(?<![A-Z][(\[])\b([1-9]\d{0,2}(?:,\d{3})+)(?=[\s,;.)\]:]|$)
```
→ the captured integer with all `,` removed.

Four independent guards, each load-bearing:

1. **Must start `[1-9]`** — rejects `0,001`, which is a European decimal.
2. **Structure `\d{1,3}(?:,\d{3})+`** — each comma must be followed by *exactly*
   three digits. `0,05` (two) and `1,5` (one) cannot match. This is what carries
   most of the discrimination.
3. **Trailing boundary `[\s,;.)\]:]|$`** — avoids mid-token matches.
4. **Negative lookbehind `(?<![A-Z][(\[])`** — leaves statistical brackets alone.
   `F(7,140)` and `F[2,42]` are df *pairs*; stripping the comma destroys them.

   T1 deliberately protects **all** `X(…)` brackets, because deciding which are
   pairs requires knowing the test. See rule T2.

## Rule T2 — single-df brackets (effectcheck layer, NOT part of the shared spec)

`t(1,197)` and `F(7,140)` are the same shape and get **opposite** answers: a
t-test takes exactly one df, so `1,197` is a thousands separator meaning 1197; an
F-test takes two, so `7,140` is a genuine pair. The difference is **test arity**,
which is statistics-aware and cannot be decided from the token's shape.

So T1 (context-free, shared) protects every bracket, and effectcheck then strips
the separator inside brackets belonging to **single-df tests only** — `t`, `H`,
`r`, `Z` — leaving `F` and chi-square df pairs intact. This is the layer split in
action: docpluck cannot make this call, and should not try.

MetaESCI E8 (2026-04-11) is the incident: `t(2,758)` became `t(2.758)`, silently
reinterpreted as a Welch df of 2.758, and one article dropped 47 rows.

## Rule D1 — decimal comma (runs SECOND, on T1's output)

```
(?<![a-zA-Z,0-9.%])(?<![A-Z][\[(])(\d{1,4}),(\d+)(?!\d)
```
→ `\1.\2`

**At most FOUR digits before the comma** (`\d{1,4}`), revised in spec 1.5.0.

The original wording — "exactly one digit before the comma" — was called the single
most important constraint here, and it was half right. It correctly diagnosed the
1000× defect it was written for: effectcheck's rule allowed `[-+]?\d+`, so `12,345`
matched and was destroyed, publishing a rank-biserial of `0.99938` against a truth of
`0.38275` at status `OK`.

But one digit is **too strict to ship**. Continental papers write `M = 12,34` and
`t = 1234,56`, and a port that required a single digit silently stopped converting
them — trading a loud error for a quiet one. Unbounded is also wrong: docpluck
reported, and effectcheck reproduced, `9999999,1 → 9999999.1`.

Four digits is the bound that admits every real continental shape and refuses the
rest. It is safe because **rule T1 runs first** and has already consumed every
unambiguous thousands group, so a comma still standing between digits at this point
is a decimal — unless the integer run is so long that no decimal notation would
produce it.

Lookbehind exclusions, each with a known failure it prevents:

- `a-zA-Z` — author affiliation superscripts (`Braunstein1,3`).
- `,` — the middle of a multi-affiliation run (`Wagner1,3,4`).
- `0-9` — CI pairs (`[0.45,0.89]`) and already-formed decimal lists.
- `.` — an already-formed decimal (`[0.45,0.89]` matched `45,0` without it).
- `%` — **added in spec 1.5.0.** A percentage followed by citation superscripts:
  docpluck reported, and effectcheck reproduced, `~25%6,28 → %6.28`. A European
  decimal is never written immediately after a percent sign, so the exclusion
  costs nothing.
- `[A-Z]` immediately followed by `[` or `(` — tight df brackets (`F[2,42]`),
  which MetaESCI D2 (2026-04-11) showed being corrupted into `F[2.42]`. Keyed on
  a preceding CAPITAL rather than a bare bracket, so a CI written `CI [0,12, 0,78]`
  still converts while a stat bracket written `F[` does not.

The lookahead is deliberately restrictive. Broadening it to `[^0-9a-zA-Z]`
(as effectcheck previously did) caused ordering regressions with CI parsing.
The `\.(?!\d)` alternative admits a sentence-final decimal (`d = 0,87.`) while
still refusing `1,234.567`.

## Rule D1b — decimal comma with no integer part (runs after D1)

```
([=<>]\s*),(\d+)(?!\d)
```
→ `\1.\2`

A continental paper omits the leading zero exactly as APA does: `p = ,025` is
how `p = .025` is written, and `p < ,001` is how `p < .001` is written. D1
requires at least one digit before the comma, so neither matched anything — and
the p-value was **silently dropped** while the same clause's `t` and `d`
converted normally. `t(48) = 2,31, p = ,025, d = 0,74` yielded `p_reported = NA`
with the row still reported; the fuller form carrying `p < ,001` yielded **zero
rows**. Found by cross-model review of v0.7.3 (2026-08-09) and reproduced.

The guard is the **value position**, not the locale. A comma sitting directly
after `=`, `<` or `>` with nothing on its left cannot be a list separator or a
thousands group — there is nothing there to group — so no locale inference is
needed and none is used. Requiring **no space** after the comma is what keeps
list and CI shapes out: `CI [0.45, 0.89]` and `F(1, 30)` both have one.

## Rule L3 — a CONFLICTED document resolves nothing (added 1.4.0)

Locale inference has **three** outcomes, not two, and an implementation that
tests only "is the decimal mark a comma?" collapses two of them:

| outcome | meaning | `decimal_mark` | T1 / D1 behaviour on `d,ddd` |
|---|---|---|---|
| `decisive` | one convention attested | `","` or `"."` | resolve per the convention |
| `none` | no evidence either way | `NA` — *absent* | apply the documented thousands default (L2) |
| `conflict` | BOTH attested, neither dominant | `NA` — *unknown* | **resolve nothing; preserve verbatim** |

`conflict` and `none` both carry `decimal_mark = NA` and they mean opposite
things. `none` says "no reason to depart from the default"; `conflict` says "the
document actively contradicts itself." Treating the second as the first
normalizes a document containing European decimals as if it were decisively US.

**Both halves are required.** If T1 steps aside for a conflicted document but D1
still converts, "leave it alone" silently becomes "call it a European decimal" —
merely the other unfounded guess. The token is preserved only when every rule
that could touch it steps aside.

Worked example (effectcheck v0.7.5, reproduced against the unfixed code). A
document mixing `p = .035, d = 0.80` with
`Welch's correction gave t(2,758) = 3,21, d = 0,45` infers `conflict`. Reading it
as US stripped the comma and published `df = 2758`, `N = 2760` and a **computed**
`d = 0.122` against a reported `0.45` — a false WARN carrying a fabricated effect
size, on a correctly reported result. Under `decisive` European the identical
string yields "cannot verify" with no computed value; `conflict` must reach that
same outcome.

This rule adds no new *transformation*. It only states which of the three
outcomes licenses one, and it exists because the third state had been computed
and never read — a signal nothing consults reads as handled at every call site
that mentions it.

## Residual ambiguity — what neither rule can decide

`M = 1,234` is genuinely undecidable from the token alone: a mean of 1234, or a
European 1.234. T1 resolves it toward **thousands**, which is the right default
for English-language APA output, but it *is* a guess.

Ambiguity is a statistics-aware question — whether the token is a sample size, a
Bayes factor, or a Cohen's d changes the answer — so it is **not** resolved in
this spec. It belongs to the consumer. effectcheck's obligation is to record when
a value was resolved under ambiguity rather than known, and never to present such
a value as verified. A platform that reports a number as checked when it was
guessed has manufactured exactly the false confidence these tools exist to remove.

## Layer ownership

| Concern | Owner | Why |
|---|---|---|
| Unicode, ligatures, glyph repair, spacing | docpluck | Context-free: a minus sign is a minus sign regardless of the surrounding statistics. |
| Thousands vs decimal separator (T1, D1) | this spec, both implementations | Structural, decidable from the token's shape alone. |
| Residual ambiguity (`M = 1,234`) | effectcheck | Requires knowing the statistical role of the token. |
| Which statistic a number *is* | effectcheck | Not a text concern at all. |

## Conformance

`conformance.json` is the executable contract. Every case carries an `input`, an
`expected` output, the governing `rule`, and a `note` explaining what breaks if
the case regresses. Both implementations must pass every case.

Cases marked `"ambiguous": true` document a deliberate default rather than a
provable answer; changing one is a semantic decision requiring a spec version
bump, not a bug fix.
