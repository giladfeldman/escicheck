
#' Read text from .docx, .html, .txt, or .pdf
#'
#' @param path File path
#' @param try_tables Logical, attempt table extraction from PDFs (default TRUE)
#' @param try_ocr Logical, attempt OCR for scanned PDFs (default FALSE)
#' @return character vector of full text
#' @export
#' @examples
#' \dontrun{
#' text <- read_any_text("paper.pdf")
#' }
read_any_text <- function(path, try_tables = TRUE, try_ocr = FALSE) {
  stopifnot(file.exists(path))
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("txt", "")) {
    return(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))

  } else if (ext %in% c("html", "htm")) {
    # HTML: strip tags and get text
    h <- xml2::read_html(path)
    # collapse visible text nodes
    txt <- rvest::html_text2(h)
    return(txt)

  } else if (ext %in% c("docx")) {
    # .docx using officer
    doc <- officer::read_docx(path)
    s <- officer::docx_summary(doc)
    if (!"text" %in% names(s)) return("")
    return(paste(s$text, collapse = "\n"))

  } else if (ext == "pdf") {
    # PDF: use comprehensive extraction
    result <- extract_pdf_comprehensive(path, try_tables = try_tables,
                                       try_ocr = try_ocr, min_text_length = 100)
    if (nchar(result$text) == 0) {
      stop("PDF text extraction failed or yielded no text. The PDF may be empty, scanned (try enabling OCR), or corrupted.")
    }
    return(result$text)

  } else {
    stop("Unsupported file type: ", ext, ". Supported formats: .txt, .html, .htm, .docx, .pdf")
  }
}
