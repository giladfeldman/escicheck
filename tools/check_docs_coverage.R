# Documentation-drift gate: the public surface in the CODE must appear in the DOCS.
#
#   Rscript tools/check_docs_coverage.R            # surface + versions + quickstart
#   Rscript tools/check_docs_coverage.R --no-run   # skip executing the quickstart
#
# Run from the package root. Exit 0 = every public name is documented and the README
# quickstart runs against a fresh install of this tree; exit 1 = drift, with every missing
# item listed. Pinned two-sided by tests/testthat/test-docs-coverage-gate.R.
#
# WHY (2026-09-27). The README still cited version 0.5.7 two minor versions and ~40 releases
# later, documented `filter_by_source(x, sources)` and `filter_by_delta(x, min, max)` with
# argument names the functions do not have, described `effectsize` as the computation engine
# although its code path is empty, listed 18 of 140 result columns, and said nothing about
# four `check_text()` arguments that no computation reads. Nothing failed, because nothing
# compared the docs with the code. This does, and the surface is DERIVED -- NAMESPACE
# exports and S3 methods, formals() of every exported function, the columns and settings a
# real check_text() call returns, the status / match.arg / `stats` values in the source, and
# every getOption()/Sys.getenv()/get_config() key -- so there is no hand-kept list to forget.
#
# "Documented" means: appears inside an inline code span or a fenced code block of README.md
# or any docs/*.md. Prose mentions do not count.

ec_docs_root <- function() {
  for (cand in c(".", "..", "../..")) {
    if (file.exists(file.path(cand, "DESCRIPTION")) &&
        file.exists(file.path(cand, "tools", "check_docs_coverage.R"))) {
      return(normalizePath(cand, winslash = "/"))
    }
  }
  stop("run from the effectcheck package root (DESCRIPTION + tools/ not found)")
}

ec_doc_files <- function(root) {
  c(file.path(root, "README.md"),
    list.files(file.path(root, "docs"), pattern = "\\.md$", full.names = TRUE))
}

.ec_source_code_lines <- function(root) {
  lines <- unlist(lapply(list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE),
                         readLines, warn = FALSE, encoding = "UTF-8"))
  lines[!grepl("^\\s*#", lines)]   # roxygen and comments are not behaviour
}

.ec_matches <- function(pattern, x) {
  m <- regmatches(x, gregexpr(pattern, x, perl = TRUE))
  unique(unlist(lapply(m, function(v) sub(pattern, "\\1", v, perl = TRUE))))
}

# ------------------------------------------------------------------------------ surface
#' Every public token, grouped by kind. `ns` is the loaded effectcheck namespace.
ec_docs_surface <- function(root, ns = asNamespace("effectcheck")) {
  nsfile <- readLines(file.path(root, "NAMESPACE"), warn = FALSE)
  exports <- .ec_matches("^export\\(([^)]+)\\)$", nsfile)
  s3 <- regmatches(nsfile, regexec("^S3method\\(\"?([^\",]+)\"?,([^)]+)\\)$", nsfile))
  s3 <- Filter(length, s3)
  s3_methods <- vapply(s3, function(m) paste0(m[2], ".", m[3]), "")

  fun_names <- c(exports, s3_methods)
  params <- character(0)
  values <- character(0)
  for (fn in fun_names) {
    f <- get(fn, envir = ns)
    fm <- formals(f)
    params <- c(params, setdiff(names(fm), "..."))
    for (nm in names(fm)) {   # match.arg-style choice vectors and the `stats` default
      if (identical(fm[[nm]], quote(expr = ))) next   # no default
      a <- fm[[nm]]
      if (is.call(a) && identical(a[[1]], as.name("c"))) {
        v <- tryCatch(eval(a), error = function(e) NULL)
        if (is.character(v) && length(v) > 1) values <- c(values, v)
      }
    }
  }

  src <- .ec_source_code_lines(root)
  status <- .ec_matches("(?<![A-Za-z_.])status\\s*(?:<-|=)\\s*\"([A-Z]+)\"", src)
  ci_status <- .ec_matches("ci_check_status\\s*(?:<-|=)\\s*\"([A-Z]+)\"", src)

  sample <- paste(
    "The difference was significant, t(48) = 2.34, p = .023, d = 0.67, 95% CI [0.09, 1.25].",
    "A one-way ANOVA revealed a main effect, F(2, 87) = 5.12, p = .008, eta2 = 0.11.",
    "The correlation was moderate, r(98) = .34, p < .001.", sep = "\n")
  res <- get("check_text", envir = ns)(sample)
  columns <- names(res)
  attrs <- setdiff(names(attributes(res)), c("names", "row.names", "class"))
  settings <- names(attr(res, "settings"))

  options <- c(.ec_matches("getOption\\(\"(effectcheck\\.[A-Za-z_.]+)\"", src),
               paste0("effectcheck.", .ec_matches("get_config\\(\"([a-z_]+)\"", src)))
  env <- c(.ec_matches("Sys\\.getenv\\(\"([A-Z][A-Z0-9_]+)\"", src),
           paste0("EFFECTCHECK_", toupper(.ec_matches("get_config\\(\"([a-z_]+)\"", src))))

  list(
    export = unique(exports),
    `S3 method` = unique(s3_methods),
    parameter = unique(params),
    value = unique(c(values, status, ci_status)),
    column = unique(columns),
    attribute = unique(attrs),
    setting = unique(settings),
    option = unique(options),
    `env var` = unique(env)
  )
}

# ------------------------------------------------------------------------------ docs
ec_documented_code_text <- function(doc_files) {
  chunks <- character(0)
  for (f in doc_files) {
    text <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    fence_re <- "(?s)```[^\n]*\n(.*?)```"
    fences <- regmatches(text, gregexpr(fence_re, text, perl = TRUE))[[1]]
    chunks <- c(chunks, fences)
    rest <- gsub(fence_re, "", text, perl = TRUE)
    inline <- regmatches(rest, gregexpr("`[^`\n]+`", rest, perl = TRUE))[[1]]
    chunks <- c(chunks, inline)
  }
  paste(chunks, collapse = "\n")
}

ec_is_documented <- function(token, code) {
  esc <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", token)
  grepl(paste0("(?<![A-Za-z0-9_.])", esc, "(?![A-Za-z0-9_])"), code, perl = TRUE)
}

ec_find_undocumented <- function(surface, code) {
  out <- character(0)
  for (kind in names(surface)) {
    for (tok in surface[[kind]]) {
      if (!ec_is_documented(tok, code)) out <- c(out, paste0(kind, ": ", tok))
    }
  }
  sort(out)
}

# ------------------------------------------------------------------------------ versions
ec_check_versions <- function(root) {
  problems <- character(0)
  desc_v <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])
  news <- readLines(file.path(root, "NEWS.md"), warn = FALSE, encoding = "UTF-8")
  head <- grep("^# effectcheck ", news, value = TRUE)[1]
  news_v <- sub("^# effectcheck ([0-9.]+).*$", "\\1", head)
  if (!identical(news_v, desc_v)) {
    problems <- c(problems, sprintf("NEWS.md: newest entry is %s, DESCRIPTION says %s", news_v, desc_v))
  }
  checks <- list(c("README.md", "\\(Version ([0-9][0-9.]*)\\)"),
                 c("CITATION.cff", "^version:\\s*\"?([0-9][0-9.]*)"))
  for (ck in checks) {
    path <- file.path(root, ck[1])
    txt <- if (file.exists(path)) readLines(path, warn = FALSE, encoding = "UTF-8") else character(0)
    hit <- regmatches(txt, regexec(ck[2], txt, perl = TRUE))
    hit <- Filter(length, hit)
    if (!length(hit)) {
      problems <- c(problems, sprintf("citation: no version found in %s", ck[1]))
    } else if (!identical(hit[[1]][2], desc_v)) {
      problems <- c(problems, sprintf("citation: %s cites %s, DESCRIPTION says %s", ck[1], hit[[1]][2], desc_v))
    }
  }
  problems
}

# ------------------------------------------------------------------------------ quickstart
ec_quickstart_blocks <- function(readme_text) {
  start <- regexpr("\n## Quickstart\n", readme_text, fixed = TRUE)
  if (start < 0) stop("README.md has no '## Quickstart' section")
  body <- substring(readme_text, start + 1)
  nxt <- regexpr("\n## ", substring(body, 5), fixed = TRUE)
  if (nxt > 0) body <- substring(body, 1, nxt + 4)
  blocks <- regmatches(body, gregexpr("(?s)```r\n(.*?)```", body, perl = TRUE))[[1]]
  blocks <- sub("^```r\n", "", sub("```$", "", blocks))
  if (!length(blocks)) stop("README quickstart has no ```r block -- nothing would be executed")
  blocks
}

#' Install THIS tree into an empty temporary library and run the quickstart against it
#' in a fresh R process, so a stale installed copy can never make the quickstart pass.
ec_run_quickstart <- function(root) {
  blocks <- ec_quickstart_blocks(paste(readLines(file.path(root, "README.md"), warn = FALSE,
                                                 encoding = "UTF-8"), collapse = "\n"))
  lib <- tempfile("ec_lib_"); dir.create(lib)
  work <- tempfile("ec_qs_"); dir.create(work)
  on.exit(unlink(c(lib, work), recursive = TRUE), add = TRUE)
  rbin <- file.path(R.home("bin"), "R")
  log <- system2(rbin, c("CMD", "INSTALL", "--no-multiarch", "--no-test-load",
                         paste0("--library=", shQuote(lib)), shQuote(root)),
                 stdout = TRUE, stderr = TRUE)
  if (!dir.exists(file.path(lib, "effectcheck"))) {
    return(c("quickstart: R CMD INSTALL into a temp library failed:", tail(log, 15)))
  }
  desc_v <- unname(read.dcf(file.path(root, "DESCRIPTION"), fields = "Version")[1, 1])
  problems <- character(0)
  for (i in seq_along(blocks)) {
    script <- file.path(work, sprintf("block%d.R", i))
    writeLines(c(sprintf(".libPaths(c(%s, .libPaths()))", deparse(lib)),
                 sprintf("setwd(%s)", deparse(work)),
                 blocks[i],
                 sprintf("stopifnot(identical(as.character(packageVersion('effectcheck')), %s))",
                         deparse(desc_v)),
                 sprintf("stopifnot(identical(normalizePath(dirname(find.package('effectcheck')), '/'), normalizePath(%s, '/')))",
                         deparse(lib))),
               script)
    out <- system2(file.path(R.home("bin"), "Rscript"), shQuote(script), stdout = TRUE, stderr = TRUE)
    st <- attr(out, "status")
    if (!is.null(st) && st != 0) {
      problems <- c(problems, sprintf("quickstart R block %d exited %s:", i, st), tail(out, 15))
    }
  }
  if (!length(problems)) {
    message(sprintf("quickstart: %d R block(s) ran OK against a fresh install of %s", length(blocks), desc_v))
  }
  problems
}

# ------------------------------------------------------------------------------ main
ec_docs_gate_main <- function(args = commandArgs(trailingOnly = TRUE)) {
  root <- ec_docs_root()
  suppressMessages(pkgload::load_all(root, quiet = TRUE, export_all = FALSE))
  surface <- ec_docs_surface(root)
  if (!length(surface$export) || !length(surface$column)) {
    cat("FAIL: derived an EMPTY public surface -- the instrument is broken, not the docs clean\n")
    return(1L)
  }
  code <- ec_documented_code_text(ec_doc_files(root))
  problems <- c(ec_find_undocumented(surface, code), ec_check_versions(root))
  if (!("--no-run" %in% args)) problems <- c(problems, ec_run_quickstart(root))
  n <- sum(lengths(surface))
  if (length(problems)) {
    cat(sprintf("FAIL: %d documentation problem(s) against %d public tokens:\n", length(problems), n))
    cat(paste0("  - ", problems), sep = "\n")
    return(1L)
  }
  cat(sprintf("OK: all %d public tokens documented (%s)\n", n,
              paste(sprintf("%d %s", lengths(surface), names(surface)), collapse = ", ")))
  0L
}

if (sys.nframe() == 0L) quit(save = "no", status = ec_docs_gate_main())
