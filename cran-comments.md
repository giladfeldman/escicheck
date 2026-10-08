## Submission

This is an update of 'effectcheck' from 0.2.3 (the current CRAN release) to
0.7.17. Development has been active across 0.2.4-0.7.17 -- new test types,
nonparametric and regression support, confidence-interval computation, and
many parser and consistency fixes -- and the most significant change is
structural, in 0.4.0 (see "Breaking change" below). Every intervening version
is documented in NEWS.md.

**0.7.17 vs the check evidence below.** 0.7.17 CHANGES `R/check.R` (the Welch t branch keeps a
stated N that already reproduces the reported d) and adds tests; 0.7.16 changed `R/parse.R`
(markdown emphasis is removed before parsing); 0.7.15 was a worker-only version bump. The local
`R CMD check --as-cran` below was run on the 0.7.17 tarball; every other result says which
version it was produced on. win-builder R-devel checked the 0.7.17 tarball on 2026-10-07 (1 NOTE, below); README links
were fixed after that run, so the tarball to submit needs one more win-builder run.

## Test environments

* Windows 11, R 4.4.0 (local), `R CMD check --as-cran --no-manual`
* win-builder, R Under development (unstable) (2026-08-31 r90457 ucrt),
  run on **version 0.7.8** (NOT this 0.7.17 submission candidate) on
  2026-09-02: **Status: OK** (0 errors | 0 warnings | 0 notes; install 26s,
  check 370s). See the correction immediately below -- this archived run is
  evidence about 0.7.8, not about this candidate.
* win-builder, R Under development (unstable) (2026-09-25 r90590 ucrt),
  run on **version 0.7.13** (NOT this 0.7.17 submission candidate):
  **Status: OK** (`.winbuilder-logs/R-devel_2026-09-30_00check.log`, line 12
  names version 0.7.13, `Status: OK` at line 67). Evidence about 0.7.13 only.
* win-builder, R Under development (unstable) (2026-10-05 r90641 ucrt), run on
  **version 0.7.17** on 2026-10-07: **Status: 1 NOTE**
  (`.winbuilder-logs/R-devel_2026-10-07_00check.log`, line 12 names version 0.7.17,
  `checking tests ... [293s] OK`, `checking PDF version of manual ... [22s] OK`). The NOTE is
  explained under "R CMD check results".

**CORRECTED 2026-09-11 by reading the log instead of the sentence describing
it. The archived win-builder run checked 0.7.8 — not 0.7.9, and not this
candidate.** `.winbuilder-logs/R-devel_2026-09-02_00check.log:12` reads:

```
* this is package 'effectcheck' version '0.7.8'
```

with `Status: OK` at line 67. The previous text here claimed the log recorded
`'0.7.9'`. It does not, and never did; the wrong number entered in the 0.7.9
release commit and stood through two subsequent edits of this very paragraph —
including one that *emphasised* the false version while believing it was
protecting the evidence. **Of 0.7.9 through 0.7.17, win-builder results are on record for 0.7.13 (2026-09-30, Status OK) and
0.7.17 (2026-10-07, 1 NOTE), both archived.** The log is archived here because win-builder deletes its own
result directories after roughly 72 hours.

**What actually separates the checked version from this candidate is large, not
two lines.** Measured `v0.7.8..HEAD` over the package sources:

```
effectcheck/R/check.R               | 1454 ++++++++++++++++++++++++++++++++---
effectcheck/R/compute.R             |   51 +-
effectcheck/R/effectcheck-class.R   |   57 +-
effectcheck/R/effectcheck-package.R |    2 +-
effectcheck/R/parse.R               |  329 ++++++--
effectcheck/R/report.R              |    6 +-
6 files changed, 1716 insertions(+), 183 deletions(-)
```

**win-builder R-devel checked the 0.7.17 tarball on 2026-10-07** (`effectcheck_0.7.17.tar.gz`, 699,222
bytes, built from 230bfcb): 0 errors, 0 warnings, 1 NOTE (see "R CMD check results"). After that run
README.md's links to build-excluded files were made absolute, so the tarball to be submitted differs from
the checked one in README.md only and **needs its own win-builder run after the public mirror is
synced**. The earlier LOCAL `R CMD check --as-cran --no-manual` run on 0.7.14 on 2026-09-30, on the
built tarball `effectcheck_0.7.14.tar.gz` (HEAD 03af3d6; the log reads "this is package
'effectcheck' version '0.7.14'"): `Status: 1 NOTE`, 0 errors, 0 warnings.
The NOTE is `unable to verify current time` (no network time source). (On 0.7.12, 2026-09-25, a
second NOTE -- one example at 6.14 s elapsed, CPU 0.81 s, under heavy machine load -- did not recur.)
A local check is not a substitute for win-builder on a submission, and an
archived log for an earlier version is not evidence about this one.

`--no-manual` is used locally because the local machine has no LaTeX, so the
PDF-manual step fails with "pdflatex is not available" -- a toolchain gap
rather than an Rd defect. That gap does not apply to the archived win-builder
runs above, which report `checking PDF version of manual ... OK`; the
0.7.17 run (2026-10-07) also reports `checking PDF version of manual ... [22s] OK`, so the manual
builds clean for this candidate.

## R CMD check results

win-builder R-devel, **version 0.7.8 (archived run, NOT this 0.7.17
candidate)**: 0 errors | 0 warnings | 0 notes. win-builder R-devel, **version
0.7.13 (archived run, NOT this candidate)**: Status OK. **This 0.7.17 submission
candidate, 2026-10-07: 0 errors | 0 warnings | 1 NOTE**, "checking CRAN incoming feasibility":

* `https://github.com/giladfeldman/escicheck/blob/main/docs/output-columns.md` (from the vignette)
  returned 404: the public repository had not yet been synced with this release. It is published
  by the mirror sync before submission.
* "(possibly) invalid file URIs" `docs/output-columns.md`, `CITATION.cff`, `CONTRIBUTING.md`
  from README.md: relative links to files excluded by `.Rbuildignore`. Fixed after the run: the
  links now point to the public repository.

Both parts are expected to clear on the re-run after the sync; that re-run is required before
submission.

Local Windows 11 / R 4.4.0 (0.7.14, 2026-09-30): 0 errors | 0 warnings | 1 note:
"checking for future file timestamps ... unable to verify current time" -- a
transient failure to reach the time server from the local check machine, not a
package problem; it does not appear on win-builder.

Local Windows 11 / R 4.4.0 (0.7.17 tarball, 2026-10-07, `--as-cran --no-manual`, tests included: "checking tests ... OK"): 0 errors | 0 warnings | 2 notes: the time-server note above, and
"Checking CRAN incoming feasibility" reporting one URL 404,
<https://github.com/giladfeldman/escicheck/blob/main/docs/output-columns.md> (cited by the vignette). That file ships
with this release and exists in the public repository only after `scripts/sync-public.sh` mirrors it, so the 404 is
expected until the mirror is synced; re-run the check after the sync and before any CRAN submission.
Through 0.7.14 the incoming-feasibility check reported only the maintainer line.

## Test suite

1387 test_that blocks across 170 test files at 0.7.17.
`scripts/verify-executed-test-count.R` on 2026-10-07 EXECUTED 1387 blocks with
0 failed, 0 errors and 0 skipped, so the blocks EXECUTED rather than merely
existing. The `R CMD check` with tests ("checking tests ... OK") below was run
on the 0.7.17 tarball (1387 blocks) on 2026-10-07, and win-builder R-devel ran them on 0.7.17 the same day
(`checking tests ... [293s] OK`). The archived win-builder run above
(`checking tests ... [228s] OK`) is evidence about 0.7.8's suite, not this one.

## Breaking change since 0.2.3: file extraction removed in 0.4.0

Reviewers should note that version 0.4.0 removed the file-input layer. The
functions read_any_text(), check_file(), check_dir(), check_files(),
checkPDF(), checkPDFdir(), checkHTML(), checkHTMLdir(), checkDOCXdir(), and
compare_file_with_statcheck() are now defunct: still exported, but they call
.Defunct() and emit an error naming the replacement workflow.

effectcheck is now a pure text-analysis package -- callers extract document
text with an external tool and pass the text to check_text(). The
text-analysis API (check_text() and the entire parsing, effect-size, and
confidence-interval engine) is unchanged and has been substantially extended
since 0.2.3.

This is an intentional, documented break. An intermediate .Deprecated() release
was considered but was not feasible: the extraction implementation was removed
wholesale in 0.4.0 (along with the poppler-utils SystemRequirement), so a
"warn but still work" stage was not possible. The defunct functions are kept
exported and documented so that existing callers receive a clear, actionable
error rather than "could not find function".

## Reverse dependencies

None. tools::package_dependencies("effectcheck", reverse = TRUE) against the
current CRAN package index returns no packages.
