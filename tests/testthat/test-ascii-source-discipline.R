# Regression test added 2026-09-09 (escicheck-qa run), Sonnet 5.
#
# Root cause: R/check.R:6152 (added 2026-09-06, the CI-escalation reword)
# introduced a literal em-dash (U+2014) inside a user-facing message string.
# `R CMD check --as-cran` flags this as a WARNING ("Found the following file
# with non-ASCII characters: R/check.R"), which is a release blocker on a
# non-ASCII-safe platform even though the string renders fine on this
# machine. Watched RED against the unfixed line (showNonASCIIfile reported
# line 6152) before the em-dash was replaced with the ASCII "--" already used
# two lines above in the same message.
#
# Scope deliberately narrow: only effectcheck/R/*.R, per the ASCII-sweep trap
# in project memory -- sweeping tests/fixtures would delete the very glyphs
# the parser exists to handle.

test_that("every effectcheck/R/*.R source file is ASCII-only (CRAN as-cran check)", {
  pkg_root <- if (dir.exists("../../R")) "../../R" else "R"
  skip_if_not(dir.exists(pkg_root), "package R/ directory not found from this working directory")

  r_files <- list.files(pkg_root, pattern = "\\.R$", full.names = TRUE)
  expect_true(length(r_files) > 0)

  offenders <- character(0)
  for (f in r_files) {
    hits <- tools::showNonASCIIfile(f)
    if (length(hits) > 0) {
      offenders <- c(offenders, f)
    }
  }

  expect_equal(
    offenders, character(0),
    info = paste0(
      "non-ASCII character(s) found in: ", paste(offenders, collapse = ", "),
      " -- R CMD check --as-cran WARNS on this; use \\uXXXX escapes or plain ASCII"
    )
  )
})
