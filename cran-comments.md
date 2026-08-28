## Submission

This is an update of 'effectcheck' from 0.2.3 (the current CRAN release) to
0.7.7. Development has been active across 0.2.4-0.7.7 -- new test types,
nonparametric and regression support, confidence-interval computation, and
many parser and consistency fixes -- and the most significant change is
structural, in 0.4.0 (see "Breaking change" below). Every intervening version
is documented in NEWS.md.

## Test environments

* Windows 11, R 4.4.0 (local), `R CMD check --as-cran --no-manual`

win-builder R-devel is run on the submission candidate immediately before
upload; that result is recorded here in place of this note when it is in hand.
(The archived win-builder logs in this package's development repository are for
0.6.19 and are kept as history, not as validation for this version.)

`--no-manual` is used locally because the local machine has no LaTeX, so the
PDF-manual step fails with "pdflatex is not available" -- a toolchain gap
rather than an Rd defect. Every Rd check passes, and win-builder built the
manual without error when this package was last checked there.

## R CMD check results

0 errors | 0 warnings | 1 note.

The NOTE is "checking for future file timestamps ... unable to verify current
time" -- a transient failure to reach the time server from the check machine,
not a package problem.

"Checking CRAN incoming feasibility" reports only the standard maintainer
line; there are no misspelling or URL findings.

## Test suite

1232 test_that blocks across 144 test files; all pass with 0 failures,
0 errors, and 0 warnings (13 minutes under `R CMD check`).

This is the count in the committed tree. The submission candidate is
rebuilt and re-checked from a clean committed tree before upload, and
these figures are refreshed with it.

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
