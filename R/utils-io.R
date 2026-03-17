
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
    # .docx extraction — prefer pandoc (cleaner output), fall back to officer.
    #
    # Why pandoc over officer:
    # - officer dumps raw Zotero/Mendeley citation JSON as text (ADDIN CSL_CITATION)
    # - officer fragments table cells (loses row/column context for stats)
    # - officer can inflate table-heavy files to millions of chars (memory crash)
    # - officer's C libraries segfault on corrupted files
    # - pandoc produces clean plain text, handles footnotes, no field code junk

    pandoc <- Sys.which("pandoc")
    if (nchar(pandoc) > 0) {
      txt <- tryCatch({
        lines <- system2("pandoc", args = c("-t", "plain", "--wrap=none", shQuote(path)),
                         stdout = TRUE, stderr = TRUE)
        status <- attr(lines, "status")
        if (!is.null(status) && status != 0) {
          stop("pandoc exited with status ", status, ": ", paste(lines, collapse = "\n"))
        }
        paste(lines, collapse = "\n")
      }, error = function(e) NULL)

      if (!is.null(txt) && nchar(txt) > 0) return(txt)
      # pandoc failed or returned empty — fall through to officer
    }

    # Fallback: officer with safety validation
    # Step 1: Verify ZIP magic bytes (PK\x03\x04) — pure R, no C calls
    con <- file(path, "rb")
    magic <- readBin(con, "raw", n = 4)
    close(con)
    if (length(magic) < 4 || !identical(magic, as.raw(c(0x50, 0x4b, 0x03, 0x04)))) {
      stop("DOCX file is not a valid ZIP archive (bad magic bytes). The file may be corrupted or not a real .docx file.")
    }

    # Step 2: Validate ZIP structure via system unzip (won't crash R on bad files)
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
    # Only use paragraph text — table cells fragment stats and inflate output
    para_text <- s$text[s$content_type == "paragraph"]
    return(paste(para_text, collapse = "\n"))

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
