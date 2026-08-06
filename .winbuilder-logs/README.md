# Pre-submission check evidence — effectcheck 0.6.19

CRAN submissions are **closed 2026-08-05 → 2026-08-19** (team vacation +
maintenance), so 0.6.19 was fully validated on 2026-08-06 and parked until the
window reopens. These logs are archived here because win-builder deletes its
result directories after ~72 hours.

## Verdicts

| Log | Environment | Status |
|---|---|---|
| `R-devel_2026-08-06_00check.log` | win-builder R-devel (2026-08-05 r90355) | **OK** — 0 errors, 0 warnings, 0 notes |
| `R-release_2026-08-06_00check.log` | win-builder R release (R 4.6.1) | 1 NOTE — the `docpluck` misspelling, fixed after this run |

The R-devel log is the authoritative one: it was run on the final tarball and is
completely clean, including `checking CRAN incoming feasibility ... OK`,
`checking tests ... OK`, and `checking PDF version of manual ... OK`.

## When submissions reopen (on/after 2026-08-19)

1. Rebuild — do **not** reuse a stale tarball; `Packaged:` should be current:
   ```
   R CMD build effectcheck
   ```
2. Re-run the local gates (all were green on 2026-08-06):
   ```
   Rscript scripts/cran-spellcheck.R effectcheck      # must exit 0
   node scripts/verify-release-contracts.mjs          # must print OK
   R CMD check --as-cran --no-manual <tarball>        # expect 0E 0W, 1 timestamp NOTE
   ```
3. Re-upload to win-builder R-devel to confirm nothing drifted, since R-devel
   itself moves:
   ```
   curl -T <tarball> ftp://win-builder.r-project.org/R-devel/
   ```
4. Submit at <https://cran.r-project.org/submit.html> with `cran-comments.md`,
   then click the confirmation email link.

If `cran-comments.md`'s "Test environments" section still cites the 2026-08-06
R-devel run, update it to the fresh one rather than shipping a stale claim.

## Local gotchas worth remembering

- The **local Windows** check reports 1 ERROR + 1 WARNING from
  "checking PDF version of manual" — this machine has no `pdflatex`. Both
  disappear under `--no-manual` and neither reproduces on win-builder. Not a
  package defect.
- `inst/WORDLIST` does **not** affect CRAN's DESCRIPTION spellcheck (that file
  belongs to the `spelling` package) — adding `docpluck` to it left win-builder
  still reporting the word. The mechanism that works is CRAN's `ignore` rule
  blanking **single-quoted spans**, which is why `'docpluck'` and `'statcheck'`
  pass. `scripts/cran-spellcheck.R` reproduces that rule locally; its header has
  the full call path and the RED/GREEN proof.
