#' EffectCheck API Functions (DEFUNCT in v0.4.0)
#'
#' All file-input functions in this file became `.Defunct()` in
#' effectcheck 0.4.0. ESCImate delegates document extraction to
#' [docpluck](https://docpluck.vercel.app/api-docs); pass the resulting
#' text to [check_text()] for analysis.
#'
#' Migration:
#' \preformatted{
#'   ## Before (errors in 0.4.0):
#'   results <- effectcheck::checkPDFdir("path/to/pdfs/")
#'
#'   ## After:
#'   library(httr2)
#'   pdfs <- list.files("path/to/pdfs/", pattern = "\\\\.pdf$", full.names = TRUE)
#'   results <- purrr::map_dfr(pdfs, function(p) \{
#'     resp <- request("https://docpluck.vercel.app/api/extract") |>
#'       req_headers(Authorization = paste("Bearer", Sys.getenv("DOCPLUCK_API_KEY"))) |>
#'       req_url_query(normalize = "academic", quality = "true") |>
#'       req_body_multipart(file = curl::form_file(p)) |>
#'       req_perform()
#'     dplyr::mutate(check_text(resp_body_json(resp)$text), source = basename(p))
#'   \})
#' }
#'
#' @name effectcheck-defunct-api
NULL

#' Check a single file for statistical consistency (DEFUNCT)
#'
#' Removed in effectcheck 0.4.0. Use [check_text()] on docpluck-extracted text.
#' @param path Defunct argument.
#' @param try_tables Defunct argument.
#' @param try_ocr Defunct argument.
#' @param ... Defunct argument.
#' @return Errors with a migration message.
#' @export
#' @keywords internal
check_file <- function(path, try_tables = TRUE, try_ocr = FALSE, ...) {
  .Defunct(new = "check_text", package = "effectcheck",
           msg = .extraction_defunct_msg())
}

#' Check a directory for statistical consistency (DEFUNCT)
#'
#' Removed in effectcheck 0.4.0. Extract via docpluck and call [check_text()]
#' per file.
#' @param dir Defunct argument.
#' @param subdir Defunct argument.
#' @param pattern Defunct argument.
#' @param try_tables Defunct argument.
#' @param try_ocr Defunct argument.
#' @param messages Defunct argument.
#' @param allowed_base_dirs Defunct argument.
#' @param ... Defunct argument.
#' @return Errors with a migration message.
#' @export
#' @keywords internal
check_dir <- function(dir,
                      subdir = TRUE,
                      pattern = "\\.(pdf|html?|docx|txt)$",
                      try_tables = TRUE,
                      try_ocr = FALSE,
                      messages = TRUE,
                      allowed_base_dirs = NULL,
                      ...) {
  .Defunct(new = "check_text", package = "effectcheck",
           msg = .extraction_defunct_msg())
}

#' Check PDF files for statistical consistency (DEFUNCT)
#'
#' Removed in effectcheck 0.4.0. Extract PDFs via docpluck.
#' @param files Defunct argument.
#' @param try_tables Defunct argument.
#' @param try_ocr Defunct argument.
#' @param messages Defunct argument.
#' @param ... Defunct argument.
#' @return Errors with a migration message.
#' @export
#' @keywords internal
checkPDF <- function(files, try_tables = TRUE, try_ocr = FALSE,
                     messages = TRUE, ...) {
  .Defunct(new = "check_text", package = "effectcheck",
           msg = .extraction_defunct_msg())
}

#' Check HTML files for statistical consistency (DEFUNCT)
#'
#' Removed in effectcheck 0.4.0. HTML can be passed directly to [check_text()]
#' via `check_text(rvest::html_text2(xml2::read_html(path)))`.
#' @param files Defunct argument.
#' @param messages Defunct argument.
#' @param ... Defunct argument.
#' @return Errors with a migration message.
#' @export
#' @keywords internal
checkHTML <- function(files, messages = TRUE, ...) {
  .Defunct(new = "check_text", package = "effectcheck",
           msg = .extraction_defunct_msg())
}

#' Check a directory of PDF files (DEFUNCT)
#'
#' Removed in effectcheck 0.4.0.
#' @param dir Defunct argument.
#' @param subdir Defunct argument.
#' @param try_tables Defunct argument.
#' @param try_ocr Defunct argument.
#' @param messages Defunct argument.
#' @param ... Defunct argument.
#' @return Errors with a migration message.
#' @export
#' @keywords internal
checkPDFdir <- function(dir, subdir = TRUE, try_tables = TRUE,
                        try_ocr = FALSE, messages = TRUE, ...) {
  .Defunct(new = "check_text", package = "effectcheck",
           msg = .extraction_defunct_msg())
}

#' Check a directory of HTML files (DEFUNCT)
#'
#' Removed in effectcheck 0.4.0.
#' @param dir Defunct argument.
#' @param subdir Defunct argument.
#' @param messages Defunct argument.
#' @param ... Defunct argument.
#' @return Errors with a migration message.
#' @export
#' @keywords internal
checkHTMLdir <- function(dir, subdir = TRUE, messages = TRUE, ...) {
  .Defunct(new = "check_text", package = "effectcheck",
           msg = .extraction_defunct_msg())
}

#' Check Word documents in a directory (DEFUNCT)
#'
#' Removed in effectcheck 0.4.0.
#' @param dir Defunct argument.
#' @param subdir Defunct argument.
#' @param messages Defunct argument.
#' @param ... Defunct argument.
#' @return Errors with a migration message.
#' @export
#' @keywords internal
checkDOCXdir <- function(dir, subdir = TRUE, messages = TRUE, ...) {
  .Defunct(new = "check_text", package = "effectcheck",
           msg = .extraction_defunct_msg())
}
