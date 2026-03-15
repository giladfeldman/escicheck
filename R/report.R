
#' Generate a submission-ready EffectCheck report
#'
#' Creates a self-contained HTML report with executive summary, color-coded
#' results table, expandable details, reproducible R code, and footer stamp.
#'
#' @param res tibble returned by check_text() / check_files()
#' @param out output file path (html)
#' @param format Output format: "html" (default) or "pdf" (requires rmarkdown)
#' @param title Report title (default: "EffectCheck Report")
#' @param author Author name (optional)
#' @param source_name Source file name (optional)
#' @param include_repro_code Logical, include reproducible R code section (default TRUE)
#' @return Invisible path to the generated report file
#' @export
#' @examples
#' \donttest{
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' generate_report(res, out = tempfile(fileext = ".html"))
#' }
generate_report <- function(res, out = "effectcheck_report.html",
                            format = "html", title = "EffectCheck Report",
                            author = NULL, source_name = NULL,
                            include_repro_code = TRUE) {
  stopifnot("data.frame" %in% class(res))

  if (format == "pdf") {
    return(render_report_pdf(res, out, title, author, source_name, include_repro_code))
  }

  if (nrow(res) == 0) {
    html <- paste0(
      "<!doctype html><html><head><meta charset='utf-8'><title>", htmlEscape(title),
      "</title></head><body><h1>", htmlEscape(title),
      "</h1><p>No statistics detected.</p>",
      "<footer style='margin-top:40px;padding-top:10px;border-top:1px solid #ccc;color:#999;font-size:12px;'>",
      "Checked with EffectCheck v", .effectcheck_version(), " on ", Sys.Date(),
      "</footer></body></html>"
    )
    writeLines(html, out)
    return(invisible(out))
  }

  # Summary statistics
  total_rows <- nrow(res)
  status_counts <- table(res$status, useNA = "ifany")
  test_type_counts <- table(res$test_type, useNA = "ifany")

  pass_count <- if ("PASS" %in% names(status_counts)) as.integer(status_counts["PASS"]) else 0L
  ok_count <- if ("OK" %in% names(status_counts)) as.integer(status_counts["OK"]) else 0L
  note_count <- if ("NOTE" %in% names(status_counts)) as.integer(status_counts["NOTE"]) else 0L
  warn_count <- if ("WARN" %in% names(status_counts)) as.integer(status_counts["WARN"]) else 0L
  error_count <- if ("ERROR" %in% names(status_counts)) as.integer(status_counts["ERROR"]) else 0L
  insufficient_count <- if (any(res$insufficient_data, na.rm = TRUE)) sum(res$insufficient_data, na.rm = TRUE) else 0L

  # Key columns for the table
  all_cols <- names(res)
  key_cols <- c("location", "test_type", "stat_value", "df1", "df2", "effect_reported_name",
                "effect_reported", "status", "closest_method", "delta_effect_abs",
                "p_reported", "p_computed")
  key_cols <- key_cols[key_cols %in% all_cols]

  # Metadata section
  meta_parts <- c(
    if (!is.null(source_name)) paste0("Source: ", htmlEscape(source_name)) else NULL,
    paste0("Date: ", Sys.Date()),
    paste0("Version: ", .effectcheck_version()),
    paste0("Statistics found: ", total_rows)
  )
  if (!is.null(author)) meta_parts <- c(paste0("Author: ", htmlEscape(author)), meta_parts)

  def <- function(x) {
    if (is.null(x) || length(x) == 0 || (is.atomic(x) && is.na(x))) return("")
    if (is.numeric(x)) return(as.character(signif(x, 4)))
    as.character(x)
  }

  # Build HTML
  html <- paste0(
'<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>', htmlEscape(title), '</title>
<style>
*{box-sizing:border-box}
body{font-family:system-ui,-apple-system,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;margin:0;padding:24px 32px;line-height:1.6;color:#333;max-width:1200px;margin:0 auto}
h1{color:#2c3e50;border-bottom:3px solid #3498db;padding-bottom:12px;margin-bottom:4px}
h2{color:#34495e;margin-top:32px;border-bottom:1px solid #eee;padding-bottom:6px}
.meta{color:#6c757d;font-size:13px;margin-bottom:24px}
.meta span{margin-right:16px}
.exec-summary{display:flex;gap:12px;margin:20px 0;flex-wrap:wrap}
.exec-bar{flex:1;min-width:120px;padding:16px;border-radius:8px;text-align:center;font-weight:bold}
.exec-bar .count{font-size:36px;display:block}
.exec-bar .label{font-size:12px;text-transform:uppercase;letter-spacing:0.5px;opacity:0.8}
.bar-pass{background:#d4edda;color:#155724}
.bar-ok{background:#d1ecf1;color:#0c5460}
.bar-note{background:#e2e3e5;color:#383d41}
.bar-warn{background:#fff3cd;color:#856404}
.bar-error{background:#f8d7da;color:#721c24}
.bar-insuf{background:#e2e3e5;color:#383d41}
table{border-collapse:collapse;width:100%;margin:16px 0;font-size:13px}
th,td{border:1px solid #dee2e6;padding:6px 10px;text-align:left}
th{background:#f1f3f5;font-weight:600;color:#495057;position:sticky;top:0}
tr.row-PASS{background:#d4edda}
tr.row-OK{background:#d1ecf1}
tr.row-NOTE{background:#e2e3e5}
tr.row-WARN{background:#fff3cd}
tr.row-ERROR{background:#f8d7da}
tr.expandable{cursor:pointer}
tr.expandable:hover{filter:brightness(0.95)}
.detail-row{display:none;background:#f8f9fa}
.detail-row.show{display:table-row}
.detail-cell{padding:12px 16px;font-size:12px;line-height:1.7}
.detail-cell strong{color:#495057}
.repro-section{background:#f4f4f4;border:1px solid #ddd;border-radius:6px;padding:16px;margin:8px 0;overflow-x:auto}
.repro-section pre{margin:0;font-size:12px;white-space:pre-wrap;word-wrap:break-word}
.test-dist{display:flex;gap:8px;flex-wrap:wrap;margin:8px 0}
.test-chip{background:#e9ecef;border-radius:12px;padding:4px 12px;font-size:12px;color:#495057}
footer{margin-top:40px;padding-top:12px;border-top:1px solid #dee2e6;color:#999;font-size:12px;text-align:center}
</style>
<script>
function toggleDetail(id){var r=document.getElementById("detail-"+id);r.classList.toggle("show")}
</script>
</head>
<body>
<h1>', htmlEscape(title), '</h1>
<div class="meta">', paste(sprintf("<span>%s</span>", meta_parts), collapse = ""), '</div>

<div class="exec-summary">
  <div class="exec-bar bar-pass"><span class="count">', pass_count, '</span><span class="label">Pass</span></div>
  <div class="exec-bar bar-ok"><span class="count">', ok_count, '</span><span class="label">OK</span></div>
  <div class="exec-bar bar-note"><span class="count">', note_count, '</span><span class="label">Note</span></div>
  <div class="exec-bar bar-warn"><span class="count">', warn_count, '</span><span class="label">Warning</span></div>
  <div class="exec-bar bar-error"><span class="count">', error_count, '</span><span class="label">Error</span></div>
</div>

<h2>Test Type Distribution</h2>
<div class="test-dist">',
paste(sprintf('<span class="test-chip">%s: %d</span>', names(test_type_counts), test_type_counts), collapse = ""),
'</div>

<h2>Results</h2>
<table>
<thead><tr>',
paste(sprintf("<th>%s</th>", key_cols), collapse = ""),
'</tr></thead>
<tbody>')

  for (i in seq_len(nrow(res))) {
    row <- res[i, ]
    status_val <- def(row$status)

    html <- paste0(html,
      "<tr class='row-", status_val, " expandable' onclick='toggleDetail(", i, ")'>",
      paste(vapply(key_cols, function(col) paste0("<td>", def(row[[col]]), "</td>"), character(1)), collapse = ""),
      "</tr>",
      "<tr id='detail-", i, "' class='detail-row'><td colspan='", length(key_cols), "' class='detail-cell'>",
      "<strong>Raw Text:</strong> ", htmlEscape(def(row$raw_text)), "<br>"
    )

    if ("context_window" %in% all_cols && !is.na(row$context_window)) {
      html <- paste0(html, "<strong>Context:</strong> ", htmlEscape(def(row$context_window)), "<br>")
    }
    if (!is.null(row$variants_tested) && !is.na(row$variants_tested) && nchar(row$variants_tested) > 0) {
      html <- paste0(html, "<strong>Variants Tested:</strong> ", htmlEscape(def(row$variants_tested)), "<br>")
    }
    if (!is.null(row$uncertainty_reasons) && !is.na(row$uncertainty_reasons) && nchar(row$uncertainty_reasons) > 0) {
      html <- paste0(html, "<strong>Uncertainty:</strong> ", htmlEscape(def(row$uncertainty_reasons)), "<br>")
    }
    if (!is.null(row$assumptions_used) && !is.na(row$assumptions_used) && nchar(row$assumptions_used) > 0) {
      html <- paste0(html, "<strong>Assumptions:</strong> ", htmlEscape(def(row$assumptions_used)), "<br>")
    }
    if (include_repro_code && "repro_code" %in% all_cols && !is.na(row$repro_code) && nchar(row$repro_code) > 0) {
      html <- paste0(html,
        "<div class='repro-section'><strong>Reproducible R Code:</strong><pre>",
        htmlEscape(row$repro_code), "</pre></div>"
      )
    }
    html <- paste0(html, "</td></tr>")
  }

  # Collated repro code section
  if (include_repro_code && "repro_code" %in% all_cols) {
    all_code <- res$repro_code[!is.na(res$repro_code) & nchar(res$repro_code) > 0]
    if (length(all_code) > 0) {
      html <- paste0(html,
        '</tbody></table>
        <h2>Collated Reproducible R Code</h2>
        <div class="repro-section"><pre>',
        htmlEscape(paste(all_code, collapse = "\n\n# ----\n\n")),
        '</pre></div>'
      )
    } else {
      html <- paste0(html, '</tbody></table>')
    }
  } else {
    html <- paste0(html, '</tbody></table>')
  }

  html <- paste0(html,
    '<footer>Checked with EffectCheck v', .effectcheck_version(),
    ' on ', Sys.Date(), '</footer>
</body>
</html>')

  writeLines(html, out)
  invisible(out)
}

#' Render report as PDF via rmarkdown
#'
#' Falls back to HTML if rmarkdown is not available.
#'
#' @param res Results tibble
#' @param out Output file path
#' @param title Report title
#' @param author Author name
#' @param source_name Source file name
#' @param include_repro_code Include reproducible code
#' @return Invisible path to generated file
#' @keywords internal
render_report_pdf <- function(res, out = "effectcheck_report.pdf",
                              title = "EffectCheck Report", author = NULL,
                              source_name = NULL, include_repro_code = TRUE) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    warning("rmarkdown not available, falling back to HTML format")
    html_out <- sub("\\.pdf$", ".html", out)
    return(generate_report(res, out = html_out, format = "html",
                           title = title, author = author,
                           source_name = source_name,
                           include_repro_code = include_repro_code))
  }

  # Create temporary Rmd
  tmpdir <- tempdir()
  rmd_path <- file.path(tmpdir, "effectcheck_report.Rmd")

  rmd_content <- paste0(
    "---\ntitle: '", title, "'\n",
    if (!is.null(author)) paste0("author: '", author, "'\n") else "",
    "date: '", Sys.Date(), "'\noutput: pdf_document\n---\n\n",
    "```{r setup, include=FALSE}\nknitr::opts_chunk$set(echo=FALSE)\n```\n\n",
    "## Summary\n\n",
    "- **Total statistics:** ", nrow(res), "\n",
    "- **PASS:** ", sum(res$status == "PASS", na.rm = TRUE), "\n",
    "- **OK:** ", sum(res$status == "OK", na.rm = TRUE), "\n",
    "- **NOTE:** ", sum(res$status == "NOTE", na.rm = TRUE), "\n",
    "- **WARN:** ", sum(res$status == "WARN", na.rm = TRUE), "\n",
    "- **ERROR:** ", sum(res$status == "ERROR", na.rm = TRUE), "\n\n",
    "```{r}\nknitr::kable(res[, c('location','test_type','stat_value','status','delta_effect_abs')])\n```\n"
  )

  writeLines(rmd_content, rmd_path)

  tryCatch({
    rmarkdown::render(rmd_path, output_file = out, quiet = TRUE)
    invisible(out)
  }, error = function(e) {
    warning("PDF rendering failed: ", conditionMessage(e), ". Falling back to HTML.")
    html_out <- sub("\\.pdf$", ".html", out)
    generate_report(res, out = html_out, format = "html",
                    title = title, author = author,
                    source_name = source_name,
                    include_repro_code = include_repro_code)
  })
}

#' Render an enhanced HTML report
#'
#' Creates an HTML report with summary statistics, expandable sections,
#' and uncertainty visualization.
#'
#' @param res tibble returned by check_text() / check_files()
#' @param out output file path (html)
#' @return Invisible path to the generated HTML report file.
#' @export
#' @examples
#' \donttest{
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' render_report(res, out = tempfile(fileext = ".html"))
#' }
render_report <- function(res, out = "effectcheck_report.html") {
  stopifnot("data.frame" %in% class(res))
  
  if (nrow(res) == 0) {
    html <- "<!doctype html><html><head><meta charset='utf-8'><title>effectcheck report</title></head><body><h2>Effect Size Checker Report</h2><p>No statistics detected.</p></body></html>"
    writeLines(html, out)
    return(invisible(out))
  }
  
  # Summary statistics
  total_rows <- nrow(res)
  status_counts <- table(res$status, useNA = "ifany")
  uncertainty_counts <- table(res$uncertainty_level, useNA = "ifany")
  test_type_counts <- table(res$test_type, useNA = "ifany")
  
  pass_count <- if ("PASS" %in% names(status_counts)) status_counts["PASS"] else 0
  ok_count <- if ("OK" %in% names(status_counts)) status_counts["OK"] else 0
  note_count <- if ("NOTE" %in% names(status_counts)) status_counts["NOTE"] else 0
  warn_count <- if ("WARN" %in% names(status_counts)) status_counts["WARN"] else 0
  error_count <- if ("ERROR" %in% names(status_counts)) status_counts["ERROR"] else 0
  insufficient_count <- if (any(res$insufficient_data, na.rm = TRUE)) sum(res$insufficient_data, na.rm = TRUE) else 0
  
  # Get all column names dynamically
  all_cols <- names(res)
  key_cols <- c("location", "test_type", "stat_value", "df1", "df2", "effect_reported_name", 
                "effect_reported", "status", "uncertainty_level", "closest_method", "delta_effect_abs")
  key_cols <- key_cols[key_cols %in% all_cols]
  
  html <- paste0(
"<!doctype html>
<html>
<head>
<meta charset='utf-8'>
<title>EffectCheck Report</title>
<style>
body {
  font-family: system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif;
  margin: 24px;
  line-height: 1.6;
  color: #333;
}
h1 { color: #2c3e50; border-bottom: 2px solid #3498db; padding-bottom: 10px; }
h2 { color: #34495e; margin-top: 30px; }
.summary-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
  gap: 15px;
  margin: 20px 0;
}
.summary-card {
  background: #f8f9fa;
  border: 1px solid #dee2e6;
  border-radius: 8px;
  padding: 15px;
  text-align: center;
}
.summary-card h3 {
  margin: 0 0 10px 0;
  font-size: 14px;
  color: #6c757d;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}
.summary-card .value {
  font-size: 32px;
  font-weight: bold;
  color: #2c3e50;
}
.summary-card.pass .value { color: #28a745; }
.summary-card.ok .value { color: #17a2b8; }
.summary-card.note .value { color: #6c757d; }
.summary-card.warn .value { color: #ffc107; }
.summary-card.error .value { color: #dc3545; }
.summary-card.uncertainty .value { color: #17a2b8; }
table {
  border-collapse: collapse;
  width: 100%;
  margin: 20px 0;
  font-size: 13px;
  box-shadow: 0 2px 4px rgba(0,0,0,0.1);
}
th, td {
  border: 1px solid #dee2e6;
  padding: 8px 12px;
  text-align: left;
}
th {
  background: #f8f9fa;
  position: sticky;
  top: 0;
  font-weight: 600;
  color: #495057;
}
.status-PASS { background: #d4edda; }
.status-OK { background: #d1ecf1; }
.status-NOTE { background: #e2e3e5; }
.status-WARN { background: #fff3cd; }
.status-ERROR { background: #f8d7da; }
.uncertainty-low { border-left: 4px solid #28a745; }
.uncertainty-medium { border-left: 4px solid #ffc107; }
.uncertainty-high { border-left: 4px solid #dc3545; }
.expandable {
  cursor: pointer;
  user-select: none;
}
.expandable:hover {
  background: #f0f0f0;
}
.details {
  display: none;
  padding: 10px;
  background: #f8f9fa;
  border-top: 1px solid #dee2e6;
}
.details.show {
  display: block;
}
.metadata {
  color: #6c757d;
  font-size: 12px;
  margin: 10px 0;
}
</style>
<script>
function toggleDetails(rowId) {
  var details = document.getElementById('details-' + rowId);
  details.classList.toggle('show');
}
</script>
</head>
<body>
<h1>Effect Size Checker Report</h1>
<div style='background:#fef3c7;border:1px solid #f59e0b;border-radius:6px;padding:10px 14px;margin:10px 0;font-size:13px;color:#92400e;'>
<strong>Development version.</strong> This tool is under active development and has not been fully validated. Results should be independently verified before use in any consequential context. Use is at your sole responsibility. Report issues at <a href='https://github.com/giladfeldman/escicheck/issues'>github.com/giladfeldman/escicheck</a> or contact <a href='mailto:giladfel@gmail.com'>Gilad Feldman</a>.
</div>
<div class='metadata'>Generated: ", as.character(Sys.time()), " | Total Statistics: ", total_rows, "</div>

<div class='summary-grid'>
  <div class='summary-card pass'>
    <h3>Pass</h3>
    <div class='value'>", pass_count, "</div>
  </div>
  <div class='summary-card ok'>
    <h3>OK</h3>
    <div class='value'>", ok_count, "</div>
  </div>
  <div class='summary-card note'>
    <h3>Note</h3>
    <div class='value'>", note_count, "</div>
  </div>
  <div class='summary-card warn'>
    <h3>Warning</h3>
    <div class='value'>", warn_count, "</div>
  </div>
  <div class='summary-card error'>
    <h3>Error</h3>
    <div class='value'>", error_count, "</div>
  </div>
  <div class='summary-card uncertainty'>
    <h3>Insufficient Data</h3>
    <div class='value'>", insufficient_count, "</div>
  </div>
</div>

<h2>Results Table</h2>
<table>
<thead>
<tr>",
paste(sprintf("<th>%s</th>", key_cols), collapse = ""),
"</tr>
</thead>
<tbody>"
  )
  
  def <- function(x) {
    if (is.null(x) || length(x) == 0 || (is.atomic(x) && is.na(x))) return("")
    if (is.numeric(x)) return(as.character(signif(x, 4)))
    as.character(x)
  }
  
  for (i in seq_len(nrow(res))) {
    row <- res[i,]
    status_cls <- paste0("status-", def(row$status))
    uncertainty_cls <- paste0("uncertainty-", def(row$uncertainty_level))
    
    html <- paste0(html, 
      "<tr class='", status_cls, " ", uncertainty_cls, " expandable' onclick='toggleDetails(", i, ")'>",
      paste(sprintf("<td>%s</td>", 
                   vapply(key_cols, function(col) def(row[[col]]), character(1))), 
            collapse = ""),
      "</tr>",
      "<tr id='details-", i, "' class='details'><td colspan='", length(key_cols), "'>",
      "<strong>Raw Text:</strong> ", htmlEscape(def(row$raw_text)), "<br>",
      if ("context_window" %in% names(res) && !is.na(row$context_window)) {
        paste0("<strong>Context:</strong> ", htmlEscape(def(row$context_window)), "<br>")
      } else "",
      if (!is.null(row$variants_tested) && !is.na(row$variants_tested) && nchar(row$variants_tested) > 0) {
        paste0("<strong>Variants Tested:</strong> ", htmlEscape(def(row$variants_tested)), "<br>")
      } else "",
      if (!is.null(row$uncertainty_reasons) && !is.na(row$uncertainty_reasons) && nchar(row$uncertainty_reasons) > 0) {
        paste0("<strong>Uncertainty Reasons:</strong> ", htmlEscape(def(row$uncertainty_reasons)), "<br>")
      } else "",
      if (!is.null(row$assumptions_used) && !is.na(row$assumptions_used) && nchar(row$assumptions_used) > 0) {
        paste0("<strong>Assumptions:</strong> ", htmlEscape(def(row$assumptions_used)), "<br>")
      } else "",
      "</td></tr>"
    )
  }
  
  html <- paste0(html, 
"</tbody>
</table>

<h2>Test Type Distribution</h2>
<ul>",
paste(sprintf("<li>%s: %d</li>", names(test_type_counts), test_type_counts), collapse = ""),
"</ul>

<h2>Uncertainty Level Distribution</h2>
<ul>",
paste(sprintf("<li>%s: %d</li>", names(uncertainty_counts), uncertainty_counts), collapse = ""),
"</ul>

</body>
</html>"
  )
  
  writeLines(html, out)
  invisible(out)
}

# Helper function to escape HTML
htmlEscape <- function(text) {
  if (is.na(text) || length(text) == 0) return("")
  text <- gsub("&", "&amp;", text)
  text <- gsub("<", "&lt;", text)
  text <- gsub(">", "&gt;", text)
  text <- gsub('"', "&quot;", text)
  text <- gsub("'", "&#39;", text)
  text
}

#' Export results to CSV
#'
#' Exports check results to CSV format with proper handling of special characters
#' and NA values.
#'
#' @param res tibble returned by check_text() / check_files()
#' @param out output file path (csv)
#' @param na string to use for NA values (default: "")
#' @param row.names logical, include row names (default: FALSE)
#' @return Invisible path to the generated CSV file.
#' @export
#' @examples
#' \donttest{
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' export_csv(res, out = tempfile(fileext = ".csv"))
#' }
export_csv <- function(res, out = "effectcheck_results.csv", na = "", row.names = FALSE) {
  stopifnot("data.frame" %in% class(res))
  
  # Convert all columns to character for consistent handling
  res_export <- res
  
  # Handle special characters and formatting
  # Convert NA to specified string
  for (col in names(res_export)) {
    if (is.numeric(res_export[[col]])) {
      # Format numeric columns, preserving NA
      res_export[[col]] <- ifelse(is.na(res_export[[col]]), na, 
                                  as.character(signif(res_export[[col]], 6)))
    } else if (is.logical(res_export[[col]])) {
      # Convert logical to character
      res_export[[col]] <- ifelse(is.na(res_export[[col]]), na, 
                                  as.character(res_export[[col]]))
    } else {
      # Character columns - replace NA
      res_export[[col]] <- ifelse(is.na(res_export[[col]]), na, 
                                  as.character(res_export[[col]]))
    }
  }
  
  # Write CSV with UTF-8 encoding
  utils::write.csv(res_export, file = out, row.names = row.names, 
                   fileEncoding = "UTF-8", na = na)
  invisible(out)
}

#' Export results to JSON
#'
#' Exports check results to JSON format with structured metadata.
#'
#' @param res tibble returned by check_text() / check_files()
#' @param out output file path (json)
#' @param pretty logical, pretty-print JSON (default: TRUE)
#' @return Invisible path to the generated JSON file.
#' @export
#' @examples
#' \donttest{
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' export_json(res, out = tempfile(fileext = ".json"))
#' }
export_json <- function(res, out = "effectcheck_results.json", pretty = TRUE) {
  stopifnot("data.frame" %in% class(res))
  
  # Check if jsonlite is available
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("jsonlite package required for JSON export. Install with: install.packages('jsonlite')")
  }
  
  # Convert to list structure with metadata
  json_data <- list(
    metadata = list(
      version = "1.0",
      generated = as.character(Sys.time()),
      total_rows = nrow(res),
      columns = names(res)
    ),
    results = jsonlite::toJSON(res, dataframe = "rows", na = "null", 
                               auto_unbox = TRUE, pretty = pretty)
  )
  
  # Write JSON
  jsonlite::write_json(json_data, path = out, pretty = pretty, auto_unbox = TRUE)
  invisible(out)
}
