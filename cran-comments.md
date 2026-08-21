## Submission

This is an update of 'effectcheck' from 0.2.3 (the current CRAN release) to
0.6.20. Development has been active across 0.2.4-0.6.20 -- new test types,
nonparametric and regression support, confidence-interval computation, and
many parser and consistency fixes -- and the most significant change is
structural, in 0.4.0 (see "Breaking change" below). Every intervening version
is documented in NEWS.md.

## Test environments

* win-builder, R-devel (2026-08-05 r90355) -- Status: OK
* Ubuntu 24.04 (GitHub Actions), R release, `R CMD check --as-cran
  --no-manual` with `error_on = "warning"`
* Windows 11, R 4.4.0 (local)

## R CMD check results

0 errors | 0 warnings | 0 notes on win-builder R-devel.

The only local result not reproduced there is an ERROR and a WARNING from
"checking PDF version of manual", caused by this machine having no LaTeX
installation ("pdflatex is not available"). win-builder reports "checking PDF
version of manual ... OK", confirming a local toolchain gap rather than an Rd
defect.

## Test suite

1231 test_that blocks across 144 test files; all pass with 0 failures,
0 errors, and 0 warnings (approx. 15 minutes under `R CMD check`).

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

None. tools::package_dependencies("effectcheck", reverse = TRUE) returns no
packages.
