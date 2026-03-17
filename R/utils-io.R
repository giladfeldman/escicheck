
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
    h <- tryCatch(
      xml2::read_html(path),
      error = function(e) stop("Failed to parse HTML file: ", e$message)
    )
    txt <- rvest::html_text2(h)
    return(txt)

  } else if (ext %in% c("docx")) {
    # .docx using officer — validate before parsing to prevent C-level segfaults.
    # officer::read_docx() calls C libraries (libzip/libxml2) that segfault on
    # corrupted files, bypassing R's tryCatch entirely and killing the process.

    # Step 1: Verify ZIP magic bytes (PK\x03\x04) — pure R, no C calls
    con <- file(path, "rb")
    magic <- readBin(con, "raw", n = 4)
    close(con)
    if (length(magic) < 4 || !identical(magic, as.raw(c(0x50, 0x4b, 0x03, 0x04)))) {
      stop("DOCX file is not a valid ZIP archive (bad magic bytes). The file may be corrupted or not a real .docx file.")
    }

    # Step 2: Verify it contains word/document.xml (required for DOCX) using
    # system unzip which won't crash our R process if the ZIP is malformed
    zip_check <- suppressWarnings(
      system2("unzip", args = c("-l", shQuote(path)), stdout = TRUE, stderr = TRUE)
    )
    zip_status <- attr(zip_check, "status")
    if (!is.null(zip_status) && zip_status != 0) {
      stop("DOCX file has a corrupted ZIP structure and cannot be read.")
    }
    if (!any(grepl("word/document\\.xml", zip_check))) {
      stop("File has .docx extension but is not a valid Word document (missing word/document.xml).")
    }

    # Step 3: Parse with officer (safe now that ZIP structure is validated)
    doc <- tryCatch(
      officer::read_docx(path),
      error = function(e) stop("Failed to parse DOCX: ", e$message)
    )
    s <- tryCatch(
      officer::docx_summary(doc),
      error = function(e) stop("Failed to extract text from DOCX: ", e$message)
    )
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
