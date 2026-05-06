
# Standard message used by every v0.4.0 .Defunct() shell. Kept here so all
# extraction-side functions (utils-io, api, check.R::check_files, compare.R)
# point users to the same migration guide.
.extraction_defunct_msg <- function() {
  paste0(
    "effectcheck v0.4.0+ no longer ships file extraction. ",
    "Extract text via docpluck (https://docpluck.vercel.app/api-docs) and ",
    "pass the result to check_text(). See effectcheck NEWS.md for the ",
    "migration guide."
  )
}

#' Read text from .docx, .html, .txt, or .pdf (DEFUNCT in v0.4.0)
#'
#' This function was removed in effectcheck 0.4.0. ESCImate now delegates
#' file extraction to [docpluck](https://docpluck.vercel.app). Extract text
#' externally and pass the result to [check_text()].
#'
#' @param path File path
#' @param try_tables (defunct argument)
#' @param try_ocr (defunct argument)
#' @return Errors with a migration message.
#' @export
#' @keywords internal
read_any_text <- function(path, try_tables = TRUE, try_ocr = FALSE) {
  .Defunct(new = "check_text", package = "effectcheck",
           msg = .extraction_defunct_msg())
}
