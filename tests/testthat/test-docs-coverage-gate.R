## Two-sided pin for tools/check_docs_coverage.R (the documentation-drift gate).
##
## A gate that passes on the real package proves nothing unless it is also shown to FAIL when
## the thing it guards is broken -- otherwise an empty derived surface or a pattern that
## matches everything would read as "fully documented". Each planted case below must be caught.
##
## The gate and the docs live in the SOURCE tree (tools/ and docs/ are .Rbuildignore'd), so this
## file skips under R CMD check on a built tarball and runs under testthat::test_local() /
## devtools::test() from the package root, which is where the cleanup gate runs it.

find_pkg_root <- function() {
  for (cand in c("../..", ".", "..")) {
    if (file.exists(file.path(cand, "tools", "check_docs_coverage.R")) &&
        file.exists(file.path(cand, "DESCRIPTION"))) {
      return(normalizePath(cand, winslash = "/"))
    }
  }
  NULL
}

pkg_root <- find_pkg_root()
skip_if(is.null(pkg_root), "source tree not available (built package): docs gate runs from the source root")

gate <- new.env()
sys.source(file.path(pkg_root, "tools", "check_docs_coverage.R"), envir = gate)
surface <- gate$ec_docs_surface(pkg_root, ns = asNamespace("effectcheck"))
code <- gate$ec_documented_code_text(gate$ec_doc_files(pkg_root))

test_that("the surface is derived from the code, not empty", {
  # Known-positive controls: if derivation broke, every later "all documented" is vacuous.
  expect_true("check_text" %in% surface$export)
  expect_true("print.effectcheck" %in% surface$`S3 method`)
  expect_true("design_ambiguous_action" %in% surface$parameter)
  expect_true(all(c("PASS", "OK", "NOTE", "WARN", "ERROR", "SKIP") %in% surface$value))
  expect_true(all(c("MATCH", "INCONSISTENT", "all_problems", "mean_diff_ci") %in% surface$value))
  expect_true(all(c("df_guard_rejected", "ci_check_status", "upstream_sign_rewrites") %in% surface$column))
  expect_gt(length(surface$column), 100)
  expect_true("settings" %in% surface$attribute)
  expect_true("n_table_rows_uncaptioned_dropped" %in% surface$setting)
  expect_true("effectcheck.production_mode" %in% surface$option)
})

test_that("the real package is fully documented and its versions agree", {
  expect_equal(gate$ec_find_undocumented(surface, code), character(0))
  expect_equal(gate$ec_check_versions(pkg_root), character(0))
})

planted <- list(
  export = "planted_undocumented_export",
  `S3 method` = "print.planted_class",
  parameter = "planted_undocumented_param",
  value = "PLANTED_STATUS",
  column = "planted_undocumented_column",
  attribute = "planted_attribute",
  setting = "planted_setting",
  option = "effectcheck.planted_option",
  `env var` = "EFFECTCHECK_PLANTED"
)
# ONE test_that around the loop, not one per kind: a test_that created inside a
# loop executes 9 blocks from 1 written call, so the static and executed test
# counts (verify-release-contracts.mjs vs verify-executed-test-count.R) could
# never agree. Every kind is still asserted, labelled by `info`.
test_that("a planted undocumented token of every kind fails the gate", {
  for (k in names(planted)) {
    tok <- planted[[k]]
    s <- surface
    s[[k]] <- c(s[[k]], tok)
    expect_equal(gate$ec_find_undocumented(s, code), paste0(k, ": ", tok), info = k)
  }
})

test_that("prose mentions do not count, and prefixes do not match longer names", {
  doc <- tempfile(fileext = ".md")
  writeLines(c("A Widget in prose.", "", "`Gadget` inline.", "", "```r", "Gizmo(1)", "```"), doc)
  txt <- gate$ec_documented_code_text(doc)
  expect_false(gate$ec_is_documented("Widget", txt))
  expect_true(gate$ec_is_documented("Gadget", txt))
  expect_true(gate$ec_is_documented("Gizmo", txt))
  expect_false(gate$ec_is_documented("check_text", "check_text_all"))
  expect_false(gate$ec_is_documented("OK", "`LOOK`"))
})

test_that("a version mismatch is reported", {
  tmp <- tempfile("ecroot_"); dir.create(tmp)
  file.copy(file.path(pkg_root, c("DESCRIPTION", "NEWS.md", "README.md", "CITATION.cff")), tmp)
  cff <- readLines(file.path(tmp, "CITATION.cff"))
  writeLines(sub("^version:.*$", "version: \"0.0.1\"", cff), file.path(tmp, "CITATION.cff"))
  expect_match(gate$ec_check_versions(tmp), "CITATION.cff cites 0.0.1", all = FALSE)
})

test_that("a quickstart without an R block is refused", {
  expect_error(gate$ec_quickstart_blocks("# x\n\n## Quickstart\n\nno code\n\n## Next\n"),
               "nothing would be executed")
})
