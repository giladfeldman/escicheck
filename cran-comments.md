## Submission

This is an update of 'effectcheck' from 0.2.3 (the current CRAN release) to
0.5.9. Development has been active across 0.2.4-0.5.9 -- new test types,
nonparametric and regression support, confidence-interval computation, and
many parser and consistency fixes -- and the most significant change is
structural, in 0.4.0 (see "Breaking change" below). Every intervening version
is documented in NEWS.md.

## Test environments

* Windows 11, R 4.4.0 (local)
* win-builder, R-devel (R Under development (unstable), 2026-05-15 r90061)

## R CMD check results

Local `R CMD check --as-cran` (Windows, R 4.4.0): 0 errors | 0 warnings | 1 note.
win-builder R-devel: 0 errors | 0 warnings | 1 note.

The local NOTE is "checking for future file timestamps ... unable to verify
current time" -- a transient failure to reach a time server from the local
check machine, not a package problem.

The win-builder NOTE has two parts, both benign:

* "Possibly misspelled words in DESCRIPTION: docpluck (18:12)". 'docpluck' is
  the name of the companion text-extraction service the package directs users
  to; its URL is given in the Description field. It is spelled correctly.

* "Found the following (possibly) invalid URLs: https://escimate.app (From:
  README.md), SSL connect error, Recv failure: Connection was reset".
  https://escimate.app is the package's live companion web application; it
  currently responds with HTTP 200 and a valid TLS certificate. The flagged
  error is a transient network failure on the check machine, not a broken link.

## Test suite

1769 tests pass, 0 failures (12 skipped placeholders).

## Breaking change since 0.2.3: file extraction removed in 0.4.0

Reviewers should note that version 0.4.0 removed the file-input layer. The
functions read_any_text(), check_file(), check_dir(), check_files(),
checkPDF(), checkPDFdir(), checkHTML(), checkHTMLdir(), checkDOCXdir(), the
extract_text_*() helpers, and compare_file_with_statcheck() are now defunct:
still exported, but they call .Defunct() and emit an error naming the
replacement workflow.

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
