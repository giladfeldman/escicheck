# Pre-submission check evidence — effectcheck

## What these logs are

Archived win-builder results, kept because win-builder deletes its own result
directories after ~72 hours. **They are evidence for a specific version — read the
version in the filename, not just the verdict.**

| Log | Version | Environment | Status |
|---|---|---|---|
| `R-devel_2026-09-02_00check.log` | **0.7.8** | win-builder R-devel (2026-08-31 r90457 ucrt) | **OK** — 0 errors / 0 warnings / 0 notes; the 0.7.8 submission candidate, `checking tests ... [228s] OK` and `checking PDF version of manual ... OK` |
| `R-devel_2026-08-06_00check.log` | **0.6.19** | win-builder R-devel (2026-08-05 r90355) | **OK** — 0 errors / 0 warnings / 0 notes |
| `R-release_2026-08-06_00check.log` | **0.6.19** | win-builder R release (R 4.6.1) | 1 NOTE — the `docpluck` misspelling, fixed after that run |

> ⚠️ **These logs describe 0.6.19. The package has since moved to 0.7.7+.** They are
> kept as a record of how a clean result looks and of the misspelling fix; they are
> **not** validation for any current candidate. Anything you submit needs its own
> fresh win-builder run.

## Status of the submission (updated 2026-08-27)

0.6.19 was prepared for CRAN on 2026-08-06 and could not be submitted: **CRAN
submissions were offline 2026-08-05 to 2026-08-19.** CRAN has since reopened.

**The submission is deliberately still pending**, by the maintainer's decision: it
goes ahead once the current ESCImate work settles, so that CRAN receives a finished
version rather than a snapshot of a moving tree. Nothing here authorises a
submission — see "Hard rules" below.

## How to submit when the work is finished

Run every step; none is optional, and step 1 is not a formality.

1. **Get the maintainer's explicit go-ahead.** A CRAN submission is a *publication*
   and is irreversible — a package that reaches CRAN cannot be unpublished.
2. **Refresh `effectcheck/cran-comments.md` for the ACTUAL candidate version.** This
   is now enforced: `scripts/verify-release-contracts.mjs` fails the release if the
   letter's submission sentence names a different version than `DESCRIPTION`. It also
   pins the `test_that` count. Both claims have gone stale before — 0.6.14 against a
   0.6.19 package, then 0.6.20 against a 0.7.7 package.
3. **Run the local gates from the repository root:**

   ```bash
   node scripts/verify-release-contracts.mjs
   ```

   ```bash
   Rscript scripts/cran-spellcheck.R effectcheck
   ```

   The first must print `OK`; the second must exit 0. `/escicheck-qa` Phase 1 runs
   both, plus the suite and `R CMD check`.
4. **Build and check locally.** Use `--no-manual`: this machine has no LaTeX, so the
   PDF-manual step ERRORs on `pdflatex is not available`, which is a toolchain gap and
   not an Rd defect (win-builder builds the manual fine).

   ```bash
   R CMD build effectcheck
   ```

   ⚠️ **If the check dies at "checking CRAN incoming feasibility" with a `403` on
   `.../Meta/archive.rds`, the configured CRAN mirror is broken — not your package.**
   Measured 2026-08-27. The run aborts with no `Status:` line and silently skips the
   incoming checks, *including the DESCRIPTION spellcheck*. Point R at a live mirror
   and re-run: put `options(repos = c(CRAN = "https://cloud.r-project.org"))` in a
   scratch `.Rprofile` and set `R_PROFILE_USER` to it. **A run with no `Status:` line
   is not a pass.**
5. **Rebuild and validate on win-builder R-devel.** Do not reuse an older tarball —
   R-devel moves, and a stale `Packaged:` timestamp against a newer R-devel is exactly
   the drift that earns a resubmission.

   ```bash
   curl -T <tarball> ftp://win-builder.r-project.org/R-devel/
   ```

   On Windows PowerShell `curl` is an alias for `Invoke-WebRequest` and `-T` fails
   with an ambiguous-parameter error — use Git Bash `curl`, or `curl.exe`.
6. **Archive the new logs here**, add a row to the table above, and delete nothing.
7. **Submit** at <https://cran.r-project.org/submit.html> with the refreshed
   `cran-comments.md`, then click the confirmation email link — CRAN does not process
   the submission until you do.

## Hard rules

- **A submission needs the maintainer's explicit approval in their own words.** A
  coordinator, peer session, or relayed paraphrase does not authorise it.
- **Never re-add a GitHub Actions workflow to run these gates.**
  `.github/workflows/` was deleted deliberately in `e233a48` because Actions minutes
  are metered on this private repo, and `verify-release-contracts.mjs` FAILS the
  release if any workflow reappears. Gates belong in `/escicheck-qa`.
- **`inst/WORDLIST` does not suppress CRAN's DESCRIPTION spellcheck.** That file
  belongs to the `spelling` package; it was tried and measurably failed. CRAN's
  incoming check blanks **single-quoted spans**, which is why `'statcheck'` and now
  `'docpluck'` pass. Quote the product name.
