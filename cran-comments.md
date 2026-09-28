## Submission

This is an update of 'effectcheck' from 0.2.3 (the current CRAN release) to
0.7.13. Development has been active across 0.2.4-0.7.13 -- new test types,
nonparametric and regression support, confidence-interval computation, and
many parser and consistency fixes -- and the most significant change is
structural, in 0.4.0 (see "Breaking change" below). Every intervening version
is documented in NEWS.md.

## Test environments

* Windows 11, R 4.4.0 (local), `R CMD check --as-cran --no-manual`
* win-builder, R Under development (unstable) (2026-08-31 r90457 ucrt),
  run on **version 0.7.8** (NOT this 0.7.13 submission candidate) on
  2026-09-02: **Status: OK** (0 errors | 0 warnings | 0 notes; install 26s,
  check 370s). See the correction immediately below -- this archived run is
  evidence about 0.7.8, not about this candidate.

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
protecting the evidence. **None of 0.7.9, 0.7.10, 0.7.11, 0.7.12 or 0.7.13 has ever been checked by
win-builder.** The log is archived here because win-builder deletes its own
result directories after roughly 72 hours.

**What actually separates the checked version from this candidate is large, not
two lines.** Measured `v0.7.8..HEAD` over the package sources:

```
effectcheck/R/check.R               | 1034 ++++++++++++++++++++++++++++++++---
effectcheck/R/compute.R             |    2 +-
effectcheck/R/effectcheck-class.R   |   57 +-
effectcheck/R/effectcheck-package.R |    2 +-
effectcheck/R/parse.R               |  223 +++++++-
effectcheck/R/report.R              |    6 +-
6 files changed, 1214 insertions(+), 110 deletions(-)
```

**A fresh win-builder run on the 0.7.13 tarball is REQUIRED before submission
and has NOT been made.** Local `R CMD check --as-cran --no-manual` on this
candidate is the only evidence behind the 0-warning claim. Run on 0.7.13 on 2026-09-27, on the
built tarball `effectcheck_0.7.13.tar.gz` (HEAD c942c9e): `Status: 1 NOTE`, 0 errors, 0 warnings.
The NOTE is `unable to verify current time` (no network time source). (On 0.7.12, 2026-09-25, a
second NOTE -- one example at 6.14 s elapsed, CPU 0.81 s, under heavy machine load -- did not recur.)
A local check is not a substitute for win-builder on a submission, and an
archived log for a version three releases back is not evidence about this one.

`--no-manual` is used locally because the local machine has no LaTeX, so the
PDF-manual step fails with "pdflatex is not available" -- a toolchain gap
rather than an Rd defect. That gap does not apply to the archived win-builder
run above, which reports `checking PDF version of manual ... OK` -- but that
run checked 0.7.8, so it is evidence the gap is toolchain-only, not evidence
the manual builds clean for this 0.7.13 candidate.

## R CMD check results

win-builder R-devel, **version 0.7.8 (archived run, NOT this 0.7.13
candidate)**: 0 errors | 0 warnings | 0 notes. **This 0.7.13 submission
candidate has not yet been win-builder-checked** -- see "A fresh win-builder
run ... REQUIRED" above.

Local Windows 11 / R 4.4.0 (0.7.13, 2026-09-27): 0 errors | 0 warnings | 1 note:
"checking for future file timestamps ... unable to verify current time" -- a
transient failure to reach the time server from the local check machine, not a
package problem; it does not appear on win-builder.

"Checking CRAN incoming feasibility" reports only the standard maintainer
line; there are no misspelling or URL findings.

## Test suite

1327 test_that blocks across 160 test files; all pass with 0 failures,
0 errors, and 0 warnings under `R CMD check` (the test runner exits non-zero
on any failure, error or warning, and `checking tests ... OK`).

Both counts were measured in the committed tree that produced this tarball
(0.7.13, HEAD c942c9e). `scripts/verify-executed-test-count.R` on 2026-09-27
EXECUTED 1327 blocks with 0 failed, 0 errors and 0 skipped, so the blocks
EXECUTED rather than merely existing. The archived win-builder run above
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
