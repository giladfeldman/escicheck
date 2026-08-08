# Conformance of effectcheck's numeric-separator normalization against the
# shared spec at inst/normalization-spec/.
#
# This file deliberately contains almost no assertions of its own: the contract
# lives in conformance.json, which is language-neutral and shared with docpluck.
# Adding a case there is how a new requirement enters BOTH implementations.
#
# Background. The comma is a decimal separator in European papers ("d = 0,80")
# and a thousands separator in English ones ("U = 12,345"). Two independent
# implementations of that resolution existed -- docpluck's and effectcheck's --
# and they diverged, because nothing tested them against each other. The
# divergence was not silent-but-harmless: effectcheck turned "U = 12,345" into
# 12.345 and published a rank-biserial correlation of 0.99938 where the truth
# was 0.38275, with status OK.
#
# Two implementations with no shared test cannot stay in sync. This is that test.

spec_dir <- system.file("normalization-spec", package = "effectcheck")
if (!nzchar(spec_dir)) spec_dir <- file.path("..", "..", "inst", "normalization-spec")

test_that("the conformance corpus is present and well-formed", {
  path <- file.path(spec_dir, "conformance.json")
  expect_true(file.exists(path))
  spec <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_true(nzchar(spec$spec_version))
  expect_gt(length(spec$cases), 20L)
  for (cs in spec$cases) {
    expect_true(nzchar(cs$id))
    expect_true(!is.null(cs$input))
    expect_true(!is.null(cs$expected))
    expect_true(nzchar(cs$note), info = cs$id)   # every case explains itself
  }
  ids <- vapply(spec$cases, function(cs) cs$id, character(1))
  expect_equal(length(unique(ids)), length(ids))  # no duplicate ids
})

# The corpus governs NUMERIC SEPARATOR SEMANTICS only. Each implementation is
# free to differ cosmetically -- effectcheck also harmonizes bracket style and
# spacing ("F[2,42]" -> "F(2, 42)"), which docpluck does not and should not be
# held to. Canonicalising both sides isolates the question the spec actually
# answers: did the digits and their separators survive correctly?
#
# This deliberately blinds the corpus to bracket/whitespace bugs. Those are real
# but they belong to other tests; conflating them here would make a shared,
# cross-language contract depend on one implementation's cosmetics.
.canon_sep <- function(s) {
  s <- gsub("[[:space:]]+", "", s)
  s <- chartr("[]", "()", s)
  s
}

test_that("normalize_text() satisfies every conformance case", {
  spec <- jsonlite::fromJSON(file.path(spec_dir, "conformance.json"),
                             simplifyVector = FALSE)
  failures <- character(0)
  for (cs in spec$cases) {
    got <- normalize_text(cs$input)
    if (!identical(.canon_sep(got), .canon_sep(cs$expected))) {
      failures <- c(failures, sprintf(
        "[%s] rule=%s\n     input:    %s\n     expected: %s\n     got:      %s\n     why it matters: %s",
        cs$id, cs$rule, cs$input, cs$expected, got, cs$note))
    }
  }
  if (length(failures) > 0) {
    fail(paste0("\n", length(failures), " conformance case(s) failed:\n\n",
                paste(failures, collapse = "\n\n")))
  }
  expect_true(TRUE)
})

test_that("the spec version is recorded so a published number is traceable", {
  # A number is only as trustworthy as the rules that produced it. The version
  # must be readable from the package, not just from the file.
  expect_true(nzchar(normalization_spec_version()))
  spec <- jsonlite::fromJSON(file.path(spec_dir, "conformance.json"),
                             simplifyVector = FALSE)
  expect_equal(normalization_spec_version(), spec$spec_version)
})

test_that("ambiguous cases are labelled as defaults, not proofs", {
  # A case marked ambiguous encodes a judgement call, not a derivable answer.
  # Flipping one is a semantic change requiring a spec version bump, so the
  # label has to survive in the corpus.
  spec <- jsonlite::fromJSON(file.path(spec_dir, "conformance.json"),
                             simplifyVector = FALSE)
  amb <- Filter(function(cs) isTRUE(cs$ambiguous), spec$cases)
  expect_gte(length(amb), 1L)
  for (cs in amb) {
    expect_match(cs$note, "AMBIGUOUS|ambiguous|ambiguity", info = cs$id)
  }
})
