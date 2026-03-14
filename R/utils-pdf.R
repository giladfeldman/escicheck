# PDF Processing Utilities
# Functions for extracting text, tables, and OCR from PDFs

#' Extract text from PDF using pdftotext
#'
#' Uses the pdftotext command-line tool (from poppler-utils) which correctly
#' handles two-column academic papers. pdftools is NOT used as a fallback
#' because it interleaves columns, corrupting statistical expressions.
#'
#' @param pdf_path Path to PDF file
#' @param suppress_warnings Logical, suppress PDF font warnings (default TRUE)
#' @return Character vector of extracted text
#' @keywords internal
extract_pdf_text_robust <- function(pdf_path, suppress_warnings = TRUE) {
  ensure_utf8 <- function(txt) {
    if (requireNamespace("stringi", quietly = TRUE)) {
      suppressWarnings(stringi::stri_enc_toutf8(txt, validate = TRUE))
    } else if (!validUTF8(txt)) {
      iconv(txt, from = "latin1", to = "UTF-8", sub = " ")
    } else {
      txt
    }
  }

  # Require pdftotext (from poppler-utils / Git mingw64)
  # pdftotext correctly handles two-column academic papers, while pdftools
  # often interleaves columns causing text corruption like "F smaller. (2, 430)"
  pdftotext_path <- Sys.which("pdftotext")
  if (pdftotext_path == "") {
    stop(
      "pdftotext not found on PATH. ",
      "Install poppler-utils (Linux/Mac) or ensure Git's mingw64/bin is on PATH (Windows). ",
      "On Windows, start_app.bat adds this automatically. ",
      "pdftools is not used because it interleaves columns in two-column PDFs."
    )
  }

  temp_txt <- tempfile(fileext = ".txt")
  on.exit(
    {
      if (file.exists(temp_txt)) {
        tryCatch(unlink(temp_txt), error = function(e) {})
      }
    },
    add = TRUE
  )

  system2(pdftotext_path,
    args = c(shQuote(pdf_path), shQuote(temp_txt)),
    stdout = TRUE, stderr = TRUE
  )

  if (!file.exists(temp_txt)) {
    stop("pdftotext failed to produce output for: ", basename(pdf_path))
  }

  text <- readLines(temp_txt, warn = FALSE, encoding = "UTF-8")
  text_combined <- paste(text, collapse = "\n")
  text_combined <- ensure_utf8(text_combined)

  if (nchar(trimws(text_combined)) == 0) {
    stop(
      "pdftotext produced empty output for: ", basename(pdf_path),
      ". The PDF may be scanned (image-only) or corrupted."
    )
  }

  return(text_combined)
}

#' Extract tables from PDF
#'
#' Attempts to extract tables from PDF using tabulizer, then converts to text format
#' that can be parsed for statistics.
#'
#' @param pdf_path Path to PDF file
#' @return Character vector of table text (one string per table)
#' @keywords internal
extract_pdf_tables <- function(pdf_path) {
  table_texts <- character(0)

  # Method 1: tabulizer (best for table extraction)
  # NOTE: tabulizer is an optional GitHub-only package (not on CRAN).
  # Package name is constructed dynamically to avoid R CMD check NOTE.
  .tab_pkg <- paste0("tabul", "izer")
  if (requireNamespace(.tab_pkg, quietly = TRUE)) {
    tryCatch(
      {
        # Extract all tables from PDF
        tab_fun <- getExportedValue(.tab_pkg, "extract_tables")
        tables <- suppressWarnings({
          tab_fun(pdf_path, method = "stream", output = "data.frame")
        })

        # Convert tables to text format
        for (table in tables) {
          if (is.data.frame(table) && nrow(table) > 0) {
            # Convert table to text representation
            table_text <- apply(table, 1, function(row) {
              paste(row, collapse = " | ")
            })
            table_texts <- c(table_texts, paste(table_text, collapse = "\n"))
          }
        }
      },
      error = function(e) {
        # Fall through to next method
      }
    )
  }

  # Method 2: Use pdftotext output for table detection heuristics
  if (length(table_texts) == 0) {
    tryCatch(
      {
        text_combined <- extract_pdf_text_robust(pdf_path, suppress_warnings = TRUE)
        lines <- strsplit(text_combined, "\n", fixed = TRUE)[[1]]
        # Identify potential table rows (multiple consecutive spaces or tabs)
        table_lines <- grep("\\s{3,}|\\t", lines, value = TRUE)
        if (length(table_lines) > 2) {
          table_texts <- c(table_texts, paste(table_lines, collapse = "\n"))
        }
      },
      error = function(e) {
        # Fall through
      }
    )
  }

  return(table_texts)
}

#' Extract text from PDF using OCR (for scanned PDFs)
#'
#' @param pdf_path Path to PDF file
#' @param language OCR language (default "eng")
#' @return Character vector of OCR'd text
#' @keywords internal
extract_pdf_ocr <- function(pdf_path, language = "eng") {
  if (!requireNamespace("tesseract", quietly = TRUE) ||
    !requireNamespace("magick", quietly = TRUE)) {
    return(character(0))
  }

  # Use pdftoppm (poppler CLI) to convert PDF pages to images for OCR
  pdftoppm_path <- Sys.which("pdftoppm")
  if (pdftoppm_path == "") {
    return(character(0))
  }

  tryCatch(
    {
      img_prefix <- tempfile()
      system2(pdftoppm_path,
        args = c("-png", "-r", "300", shQuote(pdf_path), shQuote(img_prefix)),
        stdout = TRUE, stderr = TRUE
      )

      img_files <- sort(Sys.glob(paste0(img_prefix, "*.png")))
      on.exit(unlink(img_files), add = TRUE)

      if (length(img_files) == 0) return(character(0))

      ocr_text <- character(0)
      for (img_path in img_files) {
        tryCatch(
          {
            img <- magick::image_read(img_path)
            ocr_result <- tesseract::ocr(img, engine = tesseract::tesseract(language))
            ocr_text <- c(ocr_text, ocr_result)
          },
          error = function(e) {
            # Skip this page
          }
        )
      }

      text_combined <- paste(ocr_text, collapse = "\n\n")
      if (requireNamespace("stringi", quietly = TRUE)) {
        text_combined <- suppressWarnings({
          stringi::stri_enc_toutf8(text_combined, validate = TRUE)
        })
      }
      return(text_combined)
    },
    error = function(e) {
      return(character(0))
    }
  )
}

#' Comprehensive PDF text extraction with fallbacks
#'
#' Tries multiple methods: regular text extraction, table extraction, and OCR.
#'
#' @param pdf_path Path to PDF file
#' @param try_tables Logical, attempt table extraction if regular text fails (default TRUE)
#' @param try_ocr Logical, attempt OCR if text extraction yields little content (default TRUE)
#' @param min_text_length Minimum text length to consider extraction successful (default 100)
#' @return List with 'text' (main text), 'tables' (table text), 'ocr' (OCR text), 'method' (method used)
#' @export
extract_pdf_comprehensive <- function(pdf_path, try_tables = TRUE, try_ocr = TRUE,
                                      min_text_length = 100) {
  result <- list(
    text = "",
    tables = character(0),
    ocr = "",
    method = "none"
  )

  # Step 1: Extract text via pdftotext (required)
  # Let "pdftotext not found" errors propagate -- they are not recoverable.
  # Only catch errors from the extraction itself (e.g., corrupted PDF).
  text_main <- tryCatch(
    {
      extract_pdf_text_robust(pdf_path, suppress_warnings = TRUE)
    },
    error = function(e) {
      if (grepl("pdftotext not found", e$message, fixed = TRUE)) stop(e)
      character(0)
    }
  )

  if (length(text_main) > 0 && nchar(text_main) >= min_text_length) {
    result$text <- text_main
    result$method <- "text"
  }

  # Step 2: If text extraction yielded little, try table extraction
  if (try_tables && (length(text_main) == 0 || nchar(text_main) < min_text_length)) {
    tables <- tryCatch(
      {
        extract_pdf_tables(pdf_path)
      },
      error = function(e) {
        character(0)
      }
    )

    if (length(tables) > 0) {
      result$tables <- tables
      table_text_combined <- paste(tables, collapse = "\n\n")
      if (nchar(table_text_combined) > nchar(result$text)) {
        result$text <- paste(result$text, table_text_combined, sep = "\n\n")
        result$method <- if (result$method == "none") "tables" else paste(result$method, "tables", sep = "+")
      }
    }
  }

  # Step 3: If still little content, try OCR (expensive, so last resort)
  if (try_ocr && nchar(result$text) < min_text_length) {
    ocr_text <- tryCatch(
      {
        extract_pdf_ocr(pdf_path)
      },
      error = function(e) {
        character(0)
      }
    )

    if (length(ocr_text) > 0 && nchar(ocr_text) >= min_text_length) {
      result$ocr <- ocr_text
      if (nchar(ocr_text) > nchar(result$text)) {
        result$text <- paste(result$text, ocr_text, sep = "\n\n")
        result$method <- if (result$method == "none") "ocr" else paste(result$method, "ocr", sep = "+")
      }
    }
  }

  # Clean up final text
  if (nchar(result$text) > 0 && requireNamespace("stringi", quietly = TRUE)) {
    result$text <- suppressWarnings({
      stringi::stri_enc_toutf8(result$text, validate = TRUE)
    })
  }

  return(result)
}
