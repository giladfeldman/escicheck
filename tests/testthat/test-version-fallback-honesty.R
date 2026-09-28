# A failed version lookup must never look like a version.
#
# WHY THIS EXISTS (2026-09-04).
#
# `.effectcheck_version()` used to fall back to the literal "0.2.0" when
# `utils::packageVersion("effectcheck")` errored -- a source()'d checkout, a
# broken install, stale dist metadata. DESCRIPTION has said 0.7.x for five
# minor versions, so every result object produced under a failed lookup was
# stamped `effectcheck_version = "0.2.0"` and a consumer could not tell that
# apart from a genuine 0.2.0 run. That is the portfolio "no pretending" defect
# class: a wrong provenance value is worse than an absent one, because it looks
# like provenance. Same class as a sibling R service before T-0003, whose
# engine_versions.R answers it with the explicit markers mirrored here.
#
# Bumping the literal to the current version would re-arm the identical trap on
# the next release, so the fallback is a marker, not a number.
#
# These tests were watched RED against the unfixed code first: the two failure
# paths returned "0.7.8" and "0.2.0" respectively where a marker is required.

test_that("an unreadable version reports the marker, never a version-shaped string", {
  observed <- testthat::with_mocked_bindings(
    .effectcheck_version(),
    packageVersion = function(...) stop("simulated: DESCRIPTION unreadable"),
    .package = "utils"
  )

  expect_identical(observed, "unreadable")
  # The load-bearing assertion: whatever the marker is, it must not be
  # mistakable for a release. "0.2.0" is the specific historical lie.
  expect_false(grepl("^[0-9]+\\.[0-9]+", observed))
  expect_false(identical(observed, "0.2.0"))
})

test_that("an uninstalled package reports the marker, never a version-shaped string", {
  observed <- testthat::with_mocked_bindings(
    .effectcheck_version(),
    # NOT system.file: pkgload shims that into the package's imports env during
    # load_all(), so a base-level mock of it never reaches the call site.
    find.package = function(package, ...) {
      if (identical(package, "effectcheck")) character(0) else "/nonempty"
    },
    .package = "base"
  )

  expect_identical(observed, "not-installed")
  expect_false(grepl("^[0-9]+\\.[0-9]+", observed))
  expect_false(identical(observed, "0.2.0"))
})

test_that("the two failure modes are distinguishable from each other", {
  unreadable <- testthat::with_mocked_bindings(
    .effectcheck_version(),
    packageVersion = function(...) stop("boom"),
    .package = "utils"
  )
  absent <- testthat::with_mocked_bindings(
    .effectcheck_version(),
    # NOT system.file: pkgload shims that into the package's imports env during
    # load_all(), so a base-level mock of it never reaches the call site.
    find.package = function(package, ...) {
      if (identical(package, "effectcheck")) character(0) else "/nonempty"
    },
    .package = "base"
  )

  expect_false(identical(unreadable, absent))
})

test_that("CONTROL: the healthy path still reports the real DESCRIPTION version", {
  # Two-sided control. Without this, a fix that returned a marker
  # unconditionally would satisfy every assertion above.
  observed <- .effectcheck_version()
  expect_identical(observed, as.character(utils::packageVersion("effectcheck")))
  expect_true(grepl("^[0-9]+\\.[0-9]+", observed))
})

test_that("the result object is stamped with the marker, not a stale version", {
  stamped <- testthat::with_mocked_bindings(
    attr(new_effectcheck(data.frame(a = 1)), "effectcheck_version"),
    packageVersion = function(...) stop("boom"),
    .package = "utils"
  )

  expect_identical(stamped, "unreadable")
  expect_false(identical(stamped, "0.2.0"))
})

test_that("CONTROL: a healthy result object still carries the real version", {
  stamped <- attr(new_effectcheck(data.frame(a = 1)), "effectcheck_version")
  expect_identical(stamped, as.character(utils::packageVersion("effectcheck")))
})

test_that("the human-readable label never renders a marker as a version number", {
  healthy <- .effectcheck_version_display()
  expect_identical(healthy, paste0("v", as.character(utils::packageVersion("effectcheck"))))

  degraded <- testthat::with_mocked_bindings(
    .effectcheck_version_display(),
    packageVersion = function(...) stop("boom"),
    .package = "utils"
  )
  # "vunreadable" would read as a corrupted version string; the label must say
  # plainly that the version could not be determined.
  expect_false(grepl("^v[^ ]", degraded))
  expect_true(grepl("unreadable", degraded, fixed = TRUE))
})

test_that("no package source file returns a version literal from an error handler", {
  # A grep-level backstop: the "0.2.0" literal could be reintroduced anywhere,
  # and the behavioural tests above only cover effectcheck-class.R.
  src <- testthat::test_path("..", "..", "R")
  skip_if(!dir.exists(src), "package R/ source directory not reachable from tests")

  pattern <- paste0(
    "error\\s*=\\s*function\\s*\\([^)]*\\)\\s*",
    "[\"'][0-9]+\\.[0-9]+"
  )

  offenders <- character(0)
  for (f in list.files(src, pattern = "[.][Rr]$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE)
    hits <- grep(pattern, lines)
    if (length(hits)) offenders <- c(offenders, paste0(basename(f), ":", hits))
  }
  expect_identical(offenders, character(0))
})
