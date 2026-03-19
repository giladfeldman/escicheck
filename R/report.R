
# ---- Plain English helper functions ----

# Friendly display name for effect size types
.friendly_effect_name <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) return("")
  lookup <- c(
    d = "Cohen's d", g = "Hedges' g", dz = "Cohen's dz",
    dav = "Cohen's dav", drm = "Cohen's drm",
    eta2 = "eta-squared", etap2 = "partial eta-squared",
    partial_eta2 = "partial eta-squared", eta = "eta",
    omega2 = "omega-squared", cohens_f = "Cohen's f",
    r = "r", phi = "phi", V = "Cramer's V",
    rank_biserial_r = "rank-biserial r", cliffs_delta = "Cliff's delta",
    epsilon_squared = "epsilon-squared", kendalls_W = "Kendall's W",
    beta = "beta", standardized_beta = "std. beta", b = "b"
  )
  if (name %in% names(lookup)) return(lookup[name])
  name
}

# Friendly display name for test types
.friendly_test_name <- function(type) {
  if (is.null(type) || is.na(type) || !nzchar(type)) return("")
  lookup <- c(
    t = "t-test", F = "F-test (ANOVA)", r = "Correlation",
    chisq = "Chi-square", chi2 = "Chi-square", z = "z-test",
    U = "Mann-Whitney U", W = "Wilcoxon W", H = "Kruskal-Wallis H",
    regression = "Regression"
  )
  if (type %in% names(lookup)) return(lookup[type])
  type
}

# Friendly status labels
.friendly_status <- function(status) {
  lookup <- c(
    PASS = "Consistent", OK = "Verified", NOTE = "Minor caveat",
    WARN = "Needs review", ERROR = "Inconsistency"
  )
  if (!is.null(status) && !is.na(status) && status %in% names(lookup)) return(lookup[status])
  if (is.null(status) || is.na(status)) return("")
  status
}

# Generate a plain English verdict for one row
.verdict_text <- function(row) {
  status <- if ("status" %in% names(row)) as.character(row$status) else ""
  has_effect <- !is.null(row$effect_reported) && !is.na(row$effect_reported) && nzchar(as.character(row$effect_reported))
  has_delta <- !is.null(row$delta_effect_abs) && !is.na(row$delta_effect_abs)
  effect_name <- if (!is.null(row$effect_reported_name) && !is.na(row$effect_reported_name)) as.character(row$effect_reported_name) else "effect size"
  delta <- if (has_delta) sprintf("%.3f", as.numeric(row$delta_effect_abs)) else ""
  decision_error <- if (!is.null(row$decision_error)) isTRUE(row$decision_error) else FALSE

  if (status == "PASS") {
    if (has_effect && nzchar(delta)) {
      return(sprintf("Consistent: the reported %s of %s matches our calculation (difference: %s).", effect_name, row$effect_reported, delta))
    }
    return("The reported values are consistent with our recomputation.")
  }
  if (status == "OK") return("The p-value checks out. No effect size was reported to compare.")
  if (status == "NOTE") {
    insufficient <- if (!is.null(row$insufficient_data)) isTRUE(row$insufficient_data) else FALSE
    if (insufficient) return("Cannot fully verify -- key information (like sample size) could not be extracted.")
    reasons <- if (!is.null(row$uncertainty_reasons) && !is.na(row$uncertainty_reasons) && nzchar(row$uncertainty_reasons)) as.character(row$uncertainty_reasons) else NULL
    if (!is.null(reasons)) return(paste0("Likely correct, but note: ", reasons))
    return("Likely correct, but we couldn't fully verify due to ambiguous study design details.")
  }
  if (status == "WARN") {
    if (decision_error) return("Warning: our recomputed p-value changes the significance conclusion.")
    if (has_effect && nzchar(delta)) return(sprintf("The reported %s of %s differs from our estimate by %s. Worth reviewing.", effect_name, row$effect_reported, delta))
    return("Moderate discrepancy detected. Worth a closer look.")
  }
  if (status == "ERROR") {
    if (decision_error) return("Inconsistency: the significance conclusion changes when we recompute. This is a decision error.")
    if (has_effect && nzchar(delta)) return(sprintf("Inconsistency: the reported %s of %s differs from our calculation by %s. This is a large discrepancy.", effect_name, row$effect_reported, delta))
    return("Large inconsistency detected between the reported and computed values.")
  }
  ""
}

# Generate overall assessment paragraph
.overall_verdict <- function(res) {
  total <- nrow(res)
  if (total == 0) return("No statistical results were found to check.")
  consistent <- sum(res$status %in% c("PASS", "OK"), na.rm = TRUE)
  errors <- sum(res$status == "ERROR", na.rm = TRUE)
  warns <- sum(res$status == "WARN", na.rm = TRUE)
  decision_errors <- sum(res$decision_error, na.rm = TRUE)
  pct <- round(consistent / total * 100)

  if (errors == 0 && warns == 0) {
    return(sprintf("Overall: The statistical reporting in this paper appears consistent. All %d results checked match our independent calculations.", total))
  }
  if (pct >= 80 && decision_errors == 0) {
    return(sprintf("Overall: The statistical reporting appears largely consistent (%d%% match). The flagged items may reflect rounding differences but are worth double-checking.", pct))
  }
  if (decision_errors > 0) {
    return(sprintf("Overall: %d of %d results are consistent, but %d decision error(s) were found where the significance conclusion changes. These should be carefully reviewed.", consistent, total, decision_errors))
  }
  sprintf("Overall: %d of %d results (%d%%) are consistent. %d result(s) show discrepancies that warrant review.", consistent, total, pct, errors + warns)
}

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
#' @param style Report style: "beginner" for plain English narrative (default),
#'   "expert" for the traditional technical table format
#' @return Invisible path to the generated report file
#' @export
#' @examples
#' \donttest{
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' generate_report(res, out = tempfile(fileext = ".html"))
#' }
generate_report <- function(res, out,
                            format = "html", title = "EffectCheck Report",
                            author = NULL, source_name = NULL,
                            include_repro_code = TRUE,
                            style = "beginner") {
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

  # Dispatch to style-specific generator

  if (identical(style, "beginner")) {
    return(.generate_report_beginner(res, out, title, author, source_name, include_repro_code))
  }

  # ===== EXPERT STYLE (original format) =====
  .generate_report_expert(res, out, title, author, source_name, include_repro_code)
}

# Expert report: original technical table format
.generate_report_expert <- function(res, out, title, author, source_name, include_repro_code) {
  total_rows <- nrow(res)
  status_counts <- table(res$status, useNA = "ifany")
  test_type_counts <- table(res$test_type, useNA = "ifany")

  pass_count <- if ("PASS" %in% names(status_counts)) as.integer(status_counts["PASS"]) else 0L
  ok_count <- if ("OK" %in% names(status_counts)) as.integer(status_counts["OK"]) else 0L
  note_count <- if ("NOTE" %in% names(status_counts)) as.integer(status_counts["NOTE"]) else 0L
  warn_count <- if ("WARN" %in% names(status_counts)) as.integer(status_counts["WARN"]) else 0L
  error_count <- if ("ERROR" %in% names(status_counts)) as.integer(status_counts["ERROR"]) else 0L

  all_cols <- names(res)
  key_cols <- c("location", "test_type", "stat_value", "df1", "df2", "effect_reported_name",
                "effect_reported", "status", "check_type", "closest_method", "delta_effect_abs",
                "p_reported", "p_computed", "extraction_suspect")
  key_cols <- key_cols[key_cols %in% all_cols]

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

# Beginner report: narrative format with plain English
.generate_report_beginner <- function(res, out, title, author, source_name, include_repro_code) {
  total_rows <- nrow(res)
  status_counts <- table(res$status, useNA = "ifany")
  test_type_counts <- table(res$test_type, useNA = "ifany")

  consistent_count <- sum(res$status %in% c("PASS", "OK"), na.rm = TRUE)
  caveat_count <- sum(res$status == "NOTE", na.rm = TRUE)
  review_count <- sum(res$status %in% c("WARN", "ERROR"), na.rm = TRUE)
  consistent_pct <- round(consistent_count / total_rows * 100)
  caveat_pct <- round(caveat_count / total_rows * 100)
  review_pct <- round(review_count / total_rows * 100)

  def <- function(x) {
    if (is.null(x) || length(x) == 0 || (is.atomic(x) && is.na(x))) return("")
    if (is.numeric(x)) return(as.character(signif(x, 4)))
    as.character(x)
  }

  meta_parts <- c(
    if (!is.null(source_name)) paste0("Source: ", htmlEscape(source_name)) else NULL,
    paste0("Date: ", Sys.Date()),
    paste0("Version: ", .effectcheck_version()),
    paste0("Statistics found: ", total_rows)
  )
  if (!is.null(author)) meta_parts <- c(paste0("Author: ", htmlEscape(author)), meta_parts)

  html <- paste0(
'<!doctype html>
<html>
<head>
<meta charset="utf-8">
<title>', htmlEscape(title), '</title>
<style>
*{box-sizing:border-box}
body{font-family:system-ui,-apple-system,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;margin:0;padding:24px 32px;line-height:1.7;color:#333;max-width:900px;margin:0 auto}
h1{color:#2c3e50;border-bottom:3px solid #3498db;padding-bottom:12px;margin-bottom:4px}
h2{color:#34495e;margin-top:32px;border-bottom:1px solid #eee;padding-bottom:6px}
.meta{color:#6c757d;font-size:13px;margin-bottom:24px}
.meta span{margin-right:16px}
.summary-box{background:#f0f7ff;border:1px solid #c8ddf0;border-radius:10px;padding:20px 24px;margin:20px 0}
.summary-box p{margin:6px 0}
.summary-box ul{margin:10px 0;padding-left:20px}
.summary-box li{margin:6px 0}
.summary-box .good{color:#155724}
.summary-box .caveat{color:#383d41}
.summary-box .problem{color:#721c24}
.overall{font-style:italic;color:#495057;margin-top:12px;padding-top:10px;border-top:1px solid #dde6f0}
.stacked-bar{display:flex;height:20px;border-radius:10px;overflow:hidden;margin:16px 0;background:#e9ecef}
.stacked-bar div{height:100%}
.bar-g{background:#28a745}.bar-c{background:#17a2b8}.bar-n{background:#adb5bd}.bar-w{background:#ffc107}.bar-r{background:#dc3545}
.result-card{border:1px solid #dee2e6;border-radius:10px;margin:16px 0;overflow:hidden}
.result-card .card-header{padding:14px 18px;display:flex;align-items:center;gap:12px;cursor:pointer}
.result-card .card-header:hover{filter:brightness(0.97)}
.card-PASS .card-header{background:#d4edda;border-left:5px solid #28a745}
.card-OK .card-header{background:#d1ecf1;border-left:5px solid #17a2b8}
.card-NOTE .card-header{background:#e2e3e5;border-left:5px solid #adb5bd}
.card-WARN .card-header{background:#fff3cd;border-left:5px solid #ffc107}
.card-ERROR .card-header{background:#f8d7da;border-left:5px solid #dc3545}
.badge{display:inline-block;padding:2px 10px;border-radius:12px;font-size:11px;font-weight:bold;color:white}
.badge-PASS{background:#28a745}.badge-OK{background:#17a2b8}.badge-NOTE{background:#6c757d}
.badge-WARN{background:#e6a800}.badge-ERROR{background:#dc3545}
.stat-ref{font-family:monospace;font-size:13px;color:#495057}
.card-body{padding:14px 18px;font-size:14px;line-height:1.7}
.card-body .verdict{font-weight:500;margin-bottom:8px}
.card-details{padding:10px 18px;background:#f8f9fa;border-top:1px solid #dee2e6;font-size:12px;display:none}
.card-details.show{display:block}
.card-details strong{color:#495057}
.repro-section{background:#f4f4f4;border:1px solid #ddd;border-radius:6px;padding:12px;margin:8px 0;overflow-x:auto}
.repro-section pre{margin:0;font-size:11px;white-space:pre-wrap;word-wrap:break-word}
.legend{display:grid;grid-template-columns:repeat(auto-fit,minmax(140px,1fr));gap:10px;margin:16px 0}
.legend-item{padding:10px;border-radius:8px;font-size:12px}
.legend-item strong{display:block;margin-bottom:2px}
.legend-PASS{background:#d4edda;color:#155724}.legend-OK{background:#d1ecf1;color:#0c5460}
.legend-NOTE{background:#e2e3e5;color:#383d41}.legend-WARN{background:#fff3cd;color:#856404}
.legend-ERROR{background:#f8d7da;color:#721c24}
footer{margin-top:40px;padding-top:12px;border-top:1px solid #dee2e6;color:#999;font-size:12px;text-align:center}
</style>
<script>
function toggleCard(id){var d=document.getElementById("card-details-"+id);d.classList.toggle("show")}
</script>
</head>
<body>
<h1>', htmlEscape(title), '</h1>
<div class="meta">', paste(sprintf("<span>%s</span>", meta_parts), collapse = ""), '</div>

<p style="color:#6c757d;font-size:14px;">EffectCheck reads statistical results from papers, recomputes them independently, and tells you whether the numbers are consistent.</p>

<div class="summary-box">
<h2 style="margin-top:0;border:none;padding:0">Summary</h2>
<p>We checked <strong>', total_rows, ' statistical result', if (total_rows != 1) "s" else "", '</strong> from this paper and recomputed each one independently.</p>
<ul>',
  if (consistent_count > 0) paste0('<li class="good"><strong>', consistent_count, ' result', if (consistent_count != 1) "s" else "", ' (', consistent_pct, '%)</strong> ', if (consistent_count != 1) "are" else "is", ' consistent &mdash; the reported numbers match our calculations.</li>') else "",
  if (caveat_count > 0) paste0('<li class="caveat"><strong>', caveat_count, ' result', if (caveat_count != 1) "s" else "", ' (', caveat_pct, '%)</strong> ', if (caveat_count != 1) "have" else "has", ' minor caveats or could not be fully verified.</li>') else "",
  if (review_count > 0) paste0('<li class="problem"><strong>', review_count, ' result', if (review_count != 1) "s" else "", ' (', review_pct, '%)</strong> ', if (review_count != 1) "show" else "shows", ' discrepancies that should be reviewed.</li>') else "",
'</ul>
<p class="overall">', htmlEscape(.overall_verdict(res)), '</p>
</div>

<div class="stacked-bar">',
  if (consistent_count > 0) paste0('<div class="bar-g" style="width:', round(sum(res$status=="PASS",na.rm=TRUE)/total_rows*100), '%" title="Consistent"></div>') else "",
  if (sum(res$status=="OK",na.rm=TRUE) > 0) paste0('<div class="bar-c" style="width:', round(sum(res$status=="OK",na.rm=TRUE)/total_rows*100), '%" title="Verified"></div>') else "",
  if (caveat_count > 0) paste0('<div class="bar-n" style="width:', round(caveat_count/total_rows*100), '%" title="Minor caveat"></div>') else "",
  if (sum(res$status=="WARN",na.rm=TRUE) > 0) paste0('<div class="bar-w" style="width:', round(sum(res$status=="WARN",na.rm=TRUE)/total_rows*100), '%" title="Needs review"></div>') else "",
  if (sum(res$status=="ERROR",na.rm=TRUE) > 0) paste0('<div class="bar-r" style="width:', round(sum(res$status=="ERROR",na.rm=TRUE)/total_rows*100), '%" title="Inconsistency"></div>') else "",
'</div>

<h2>What the Categories Mean</h2>
<div class="legend">
<div class="legend-item legend-PASS"><strong>Consistent</strong>The numbers check out.</div>
<div class="legend-item legend-OK"><strong>Verified</strong>P-value checks out (no effect size to compare).</div>
<div class="legend-item legend-NOTE"><strong>Minor Caveat</strong>Likely correct, but with a small caveat.</div>
<div class="legend-item legend-WARN"><strong>Needs Review</strong>Something looks off &mdash; worth double-checking.</div>
<div class="legend-item legend-ERROR"><strong>Inconsistency</strong>The numbers don\'t add up.</div>
</div>

<h2>Detailed Results</h2>')

  for (i in seq_len(nrow(res))) {
    row <- res[i, ]
    status_val <- def(row$status)
    verdict <- .verdict_text(row)
    stat_str <- paste0(def(row$test_type), "(", def(row$df1), if (!is.na(row$df2) && nzchar(def(row$df2))) paste0(", ", def(row$df2)) else "", ") = ", def(row$stat_value))
    has_effect <- !is.null(row$effect_reported) && !is.na(row$effect_reported) && nzchar(def(row$effect_reported))
    has_matched <- !is.null(row$matched_value) && !is.na(row$matched_value) && nzchar(def(row$matched_value))

    html <- paste0(html,
      '<div class="result-card card-', status_val, '">',
      '<div class="card-header" onclick="toggleCard(', i, ')">',
      '<span class="badge badge-', status_val, '">', .friendly_status(status_val), '</span>',
      '<span class="stat-ref">', htmlEscape(stat_str), '</span>',
      if (!is.null(row$p_reported) && !is.na(row$p_reported)) paste0('<span class="stat-ref">, p = ', def(row$p_reported), '</span>') else "",
      '</div>',
      '<div class="card-body">',
      '<div class="verdict">', htmlEscape(verdict), '</div>'
    )

    # Reported vs computed comparison
    if (has_effect) {
      html <- paste0(html, '<p>The paper reported: <strong>', def(row$effect_reported_name), ' = ', def(row$effect_reported), '</strong>')
      if (has_matched) {
        html <- paste0(html, '<br>We computed: <strong>', .friendly_effect_name(def(row$matched_variant)), ' = ', def(row$matched_value), '</strong>')
        if (!is.null(row$delta_effect_abs) && !is.na(row$delta_effect_abs)) {
          html <- paste0(html, ' (difference: ', sprintf("%.3f", as.numeric(row$delta_effect_abs)), ')')
        }
      }
      html <- paste0(html, '</p>')
    }

    # P-value comparison
    if (!is.null(row$p_computed) && !is.na(row$p_computed)) {
      html <- paste0(html, '<p>P-value: reported = ', def(row$p_reported), ', computed = ', sprintf("%.4f", as.numeric(row$p_computed)))
      if (isTRUE(row$decision_error)) {
        html <- paste0(html, ' <strong style="color:#dc3545">(significance conclusion differs!)</strong>')
      }
      html <- paste0(html, '</p>')
    }

    html <- paste0(html, '</div>')

    # Expandable technical details
    html <- paste0(html,
      '<div id="card-details-', i, '" class="card-details">',
      '<strong>Original text:</strong> ', htmlEscape(def(row$raw_text)), '<br>'
    )
    if (!is.null(row$N) && !is.na(row$N)) {
      html <- paste0(html, '<strong>Sample size:</strong> N = ', def(row$N))
      if (!is.null(row$n1) && !is.na(row$n1) && !is.null(row$n2) && !is.na(row$n2)) {
        html <- paste0(html, ' (', def(row$n1), ' + ', def(row$n2), ')')
      }
      html <- paste0(html, '<br>')
    }
    if (!is.null(row$design_inferred) && !is.na(row$design_inferred) && nzchar(def(row$design_inferred))) {
      html <- paste0(html, '<strong>Design:</strong> ', def(row$design_inferred), '<br>')
    }
    if (!is.null(row$assumptions_used) && !is.na(row$assumptions_used) && nzchar(row$assumptions_used)) {
      html <- paste0(html, '<strong>Assumptions:</strong> ', htmlEscape(def(row$assumptions_used)), '<br>')
    }
    if (!is.null(row$uncertainty_reasons) && !is.na(row$uncertainty_reasons) && nzchar(row$uncertainty_reasons)) {
      html <- paste0(html, '<strong>Notes:</strong> ', htmlEscape(def(row$uncertainty_reasons)), '<br>')
    }
    if (include_repro_code && "repro_code" %in% names(res) && !is.na(row$repro_code) && nzchar(def(row$repro_code))) {
      html <- paste0(html,
        '<div class="repro-section"><strong>Verify in R:</strong><pre>',
        htmlEscape(row$repro_code), '</pre></div>'
      )
    }
    html <- paste0(html, '</div></div>')
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
render_report_pdf <- function(res, out,
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
  on.exit(unlink(rmd_path), add = TRUE)

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
render_report <- function(res, out) {
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
                "effect_reported", "status", "check_type", "uncertainty_level", "closest_method",
                "delta_effect_abs", "extraction_suspect")
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
export_csv <- function(res, out, na = "", row.names = FALSE) {
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
export_json <- function(res, out, pretty = TRUE) {
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
