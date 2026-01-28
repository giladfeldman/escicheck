
#' Render an enhanced HTML report
#'
#' Creates an HTML report with summary statistics, expandable sections,
#' and uncertainty visualization.
#'
#' @param res tibble returned by check_text() / check_files()
#' @param out output file path (html)
#' @export
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
<div class='metadata'>Generated: ", as.character(Sys.time()), " | Total Statistics: ", total_rows, "</div>

<div class='summary-grid'>
  <div class='summary-card pass'>
    <h3>Pass</h3>
    <div class='value'>", pass_count, "</div>
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
      if (!is.na(row$variants_tested) && nchar(row$variants_tested) > 0) {
        paste0("<strong>Variants Tested:</strong> ", htmlEscape(def(row$variants_tested)), "<br>")
      } else "",
      if (!is.na(row$uncertainty_reasons) && nchar(row$uncertainty_reasons) > 0) {
        paste0("<strong>Uncertainty Reasons:</strong> ", htmlEscape(def(row$uncertainty_reasons)), "<br>")
      } else "",
      if (!is.na(row$assumptions_used) && nchar(row$assumptions_used) > 0) {
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
#' @export
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
#' @export
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
