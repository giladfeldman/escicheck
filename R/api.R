#' EffectCheck API Functions
#'
#' This file provides statcheck-compatible API functions for checking
#' statistical consistency in APA-style results.

#' Check a single file for statistical consistency
#'
#' Reads a file and checks all detected statistics for consistency.
#'
#' @param path Path to the file (.pdf, .html, .docx, or .txt)
#' @param try_tables Logical, attempt table extraction from PDFs (default TRUE)
#' @param try_ocr Logical, attempt OCR for scanned PDFs (default FALSE)
#' @param ... Additional arguments passed to check_text()
#' @return An effectcheck object with results
#' @export
#' @examples
#' \donttest{
#' tmp <- tempfile(fileext = ".txt")
#' writeLines("t(28) = 2.21, p = .035, d = 0.80", tmp)
#' result <- check_file(tmp)
#' print(result)
#' unlink(tmp)
#' }
check_file <- function(path, try_tables = TRUE, try_ocr = FALSE, ...) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  # Read text from file
  txt <- read_any_text(path, try_tables = try_tables, try_ocr = try_ocr)

  # Check text
  result <- check_text(txt, ...)

  # Add source column
  if (nrow(result) > 0) {
    result$source <- basename(path)
  }

  # Ensure effectcheck class
  if (!inherits(result, "effectcheck")) {
    result <- new_effectcheck(result, call = match.call(), settings = list(...))
  }

  result
}

#' Internal helper to process a list of files
#'
#' @param files Character vector of file paths to process
#' @param read_args List of arguments for read_any_text()
#' @param check_args List of arguments for check_text()
#' @param messages Logical, show progress messages (default TRUE)
#' @return An effectcheck object with results from all files
#' @keywords internal
process_files_internal <- function(files,
                                   read_args = list(),
                                   check_args = list(),
                                   messages = TRUE) {
  all_results <- list()

  for (i in seq_along(files)) {
    f <- files[i]
    if (messages) {
      message(sprintf("[%d/%d] Processing: %s", i, length(files), basename(f)))
    }

    result <- tryCatch(
      {
        # Merge file path with read arguments
        do.call(read_any_text, c(list(path = f), read_args)) |>
          # Pass text to check_text with check arguments
          (function(txt) do.call(check_text, c(list(text = txt), check_args)))() -> res

        if (nrow(res) > 0) {
          res$source <- basename(f)
          res$source_path <- f
        }
        res
      },
      error = function(e) {
        if (messages) {
          message(sprintf("  Warning: Failed to process %s: %s", basename(f), e$message))
        }
        tibble::tibble()
      }
    )

    all_results[[i]] <- result
  }

  dplyr::bind_rows(all_results)
}

#' Check a directory for statistical consistency
#'
#' Recursively scans a directory for supported files and checks all
#' detected statistics for consistency.
#'
#' @param dir Directory path to scan
#' @param subdir Logical, recurse into subdirectories (default TRUE)
#' @param pattern File pattern regex (default matches .pdf, .html, .htm, .docx, .txt)
#' @param try_tables Logical, attempt table extraction from PDFs (default TRUE)
#' @param try_ocr Logical, attempt OCR for scanned PDFs (default FALSE)
#' @param messages Logical, show progress messages (default TRUE)
#' @param allowed_base_dirs Optional character vector of allowed base directories for security
#' @param ... Additional arguments passed to check_text()
#' @return An effectcheck object with results from all files
#' @export
#' @examples
#' \donttest{
#' d <- tempdir()
#' writeLines("t(28) = 2.21, p = .035, d = 0.80", file.path(d, "test.txt"))
#' results <- check_dir(d, pattern = "\\.txt$")
#' summary(results)
#' unlink(file.path(d, "test.txt"))
#' }
check_dir <- function(dir,
                      subdir = TRUE,
                      pattern = "\\.(pdf|html?|docx|txt)$",
                      try_tables = TRUE,
                      try_ocr = FALSE,
                      messages = TRUE,
                      allowed_base_dirs = NULL,
                      ...) {
  # Security Fix: Path validation
  if (!dir.exists(dir)) {
    stop("Directory not found: ", dir)
  }

  # Normalize path
  dir <- normalizePath(dir, mustWork = TRUE)

  # Check against allowed base directories if provided
  if (!is.null(allowed_base_dirs)) {
    allowed_base_dirs <- vapply(allowed_base_dirs, normalizePath, character(1), mustWork = TRUE)
    if (!any(startsWith(dir, allowed_base_dirs))) {
      stop("Global protection: Directory not in allowed list: ", dir)
    }
  }

  # Find all matching files
  files <- list.files(
    path = dir,
    pattern = pattern,
    recursive = subdir,
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(files) == 0) {
    if (messages) message("No matching files found in: ", dir)
    return(new_effectcheck(tibble::tibble(), call = match.call(), settings = list(...)))
  }

  if (messages) {
    message(sprintf("Found %d files to process in: %s", length(files), dir))
  }

  # Process using helper
  combined <- process_files_internal(
    files,
    read_args = list(try_tables = try_tables, try_ocr = try_ocr),
    check_args = list(...),
    messages = messages
  )

  if (messages) {
    message(sprintf(
      "\nProcessed %d files, found %d statistics",
      length(files), nrow(combined)
    ))
  }

  new_effectcheck(combined, call = match.call(), settings = list(...))
}

#' Check PDF files for statistical consistency
#'
#' Checks one or more PDF files for statistical consistency.
#' This function is provided for compatibility with statcheck's checkPDF().
#'
#' @param files Character vector of PDF file paths
#' @param try_tables Logical, attempt table extraction (default TRUE)
#' @param try_ocr Logical, attempt OCR for scanned PDFs (default FALSE)
#' @param messages Logical, show progress messages (default TRUE)
#' @param ... Additional arguments passed to check_text()
#' @return An effectcheck object with results
#' @export
#' @examples
#' \dontrun{
#' # Requires PDF files
#' results <- checkPDF(c("paper1.pdf", "paper2.pdf"))
#' summary(results)
#' }
checkPDF <- function(files,
                     try_tables = TRUE,
                     try_ocr = FALSE,
                     messages = TRUE,
                     ...) {
  # Validate files exist and are PDFs
  valid_files <- character(0)

  for (f in files) {
    if (!file.exists(f)) {
      if (messages) message("File not found: ", f)
      next
    }
    if (!grepl("\\.pdf$", f, ignore.case = TRUE)) {
      if (messages) message("Not a PDF file: ", f)
      next
    }
    valid_files <- c(valid_files, f)
  }

  if (length(valid_files) == 0) {
    if (messages) message("No valid PDF files to process.")
    return(new_effectcheck(tibble::tibble(), call = match.call(), settings = list(...)))
  }

  if (messages) {
    message(sprintf("Processing %d PDF file(s)...", length(valid_files)))
  }

  # Process using helper
  combined <- process_files_internal(
    valid_files,
    read_args = list(try_tables = try_tables, try_ocr = try_ocr),
    check_args = list(...),
    messages = messages
  )

  if (messages) {
    message(sprintf(
      "\nFound %d statistics in %d PDF(s)",
      nrow(combined), length(valid_files)
    ))
  }

  new_effectcheck(combined, call = match.call(), settings = list(...))
}

#' Check HTML files for statistical consistency
#'
#' Checks one or more HTML files for statistical consistency.
#' This function is provided for compatibility with statcheck's checkHTML().
#'
#' @param files Character vector of HTML file paths
#' @param messages Logical, show progress messages (default TRUE)
#' @param ... Additional arguments passed to check_text()
#' @return An effectcheck object with results
#' @export
#' @examples
#' \donttest{
#' tmp <- tempfile(fileext = ".html")
#' writeLines("<p>t(28) = 2.21, p = .035, d = 0.80</p>", tmp)
#' results <- checkHTML(tmp)
#' summary(results)
#' unlink(tmp)
#' }
checkHTML <- function(files, messages = TRUE, ...) {
  # Validate files exist and are HTML
  valid_files <- character(0)

  for (f in files) {
    if (!file.exists(f)) {
      if (messages) message("File not found: ", f)
      next
    }
    if (!grepl("\\.html?$", f, ignore.case = TRUE)) {
      if (messages) message("Not an HTML file: ", f)
      next
    }
    valid_files <- c(valid_files, f)
  }

  if (length(valid_files) == 0) {
    if (messages) message("No valid HTML files to process.")
    return(new_effectcheck(tibble::tibble(), call = match.call(), settings = list(...)))
  }

  if (messages) {
    message(sprintf("Processing %d HTML file(s)...", length(valid_files)))
  }

  # Process using helper
  combined <- process_files_internal(
    valid_files,
    read_args = list(),
    check_args = list(...),
    messages = messages
  )

  if (messages) {
    message(sprintf(
      "\nFound %d statistics in %d HTML file(s)",
      nrow(combined), length(valid_files)
    ))
  }

  new_effectcheck(combined, call = match.call(), settings = list(...))
}

#' Check a directory of PDF files
#'
#' Scans a directory for PDF files and checks all detected statistics.
#' This function is provided for compatibility with statcheck's checkPDFdir().
#'
#' @param dir Directory path to scan
#' @param subdir Logical, recurse into subdirectories (default TRUE)
#' @param try_tables Logical, attempt table extraction (default TRUE)
#' @param try_ocr Logical, attempt OCR for scanned PDFs (default FALSE)
#' @param messages Logical, show progress messages (default TRUE)
#' @param ... Additional arguments passed to check_text()
#' @return An effectcheck object with results
#' @export
#' @examples
#' \dontrun{
#' # Requires a directory with PDF files
#' results <- checkPDFdir("manuscripts/")
#' summary(results)
#' }
checkPDFdir <- function(dir,
                        subdir = TRUE,
                        try_tables = TRUE,
                        try_ocr = FALSE,
                        messages = TRUE,
                        ...) {
  check_dir(
    dir = dir,
    subdir = subdir,
    pattern = "\\.pdf$",
    try_tables = try_tables,
    try_ocr = try_ocr,
    messages = messages,
    ...
  )
}

#' Check a directory of HTML files
#'
#' Scans a directory for HTML files and checks all detected statistics.
#' This function is provided for compatibility with statcheck's checkHTMLdir().
#'
#' @param dir Directory path to scan
#' @param subdir Logical, recurse into subdirectories (default TRUE)
#' @param messages Logical, show progress messages (default TRUE)
#' @param ... Additional arguments passed to check_text()
#' @return An effectcheck object with results
#' @export
#' @examples
#' \dontrun{
#' # Requires a directory with HTML files
#' results <- checkHTMLdir("manuscripts/")
#' summary(results)
#' }
checkHTMLdir <- function(dir,
                         subdir = TRUE,
                         messages = TRUE,
                         ...) {
  check_dir(
    dir = dir,
    subdir = subdir,
    pattern = "\\.html?$",
    try_tables = FALSE,
    try_ocr = FALSE,
    messages = messages,
    ...
  )
}

#' Check Word documents in a directory
#'
#' Scans a directory for .docx files and checks all detected statistics.
#'
#' @param dir Directory path to scan
#' @param subdir Logical, recurse into subdirectories (default TRUE)
#' @param messages Logical, show progress messages (default TRUE)
#' @param ... Additional arguments passed to check_text()
#' @return An effectcheck object with results
#' @export
#' @examples
#' \dontrun{
#' # Requires a directory with DOCX files
#' results <- checkDOCXdir("manuscripts/")
#' summary(results)
#' }
checkDOCXdir <- function(dir,
                         subdir = TRUE,
                         messages = TRUE,
                         ...) {
  check_dir(
    dir = dir,
    subdir = subdir,
    pattern = "\\.docx$",
    try_tables = FALSE,
    try_ocr = FALSE,
    messages = messages,
    ...
  )
}
