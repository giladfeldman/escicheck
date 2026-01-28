\name{effectcheck-package}
\alias{effectcheck-package}
\alias{effectcheck}
\docType{package}
\title{
  Effect Size Checker for APA-Style Results
}
\description{
  A conservative, assumption-aware statistical consistency checker for APA-style 
  research results. Parses test statistics, effect sizes, and confidence intervals 
  from text, PDF, HTML, and Word documents. Recomputes effect sizes using all 
  plausible variants when design is ambiguous, and validates internal consistency.
}
\details{
  \strong{Key Features:}
  \itemize{
    \item Ultra-robust parsing with Unicode normalization and PDF artifact handling
    \item Comprehensive CI computation with method tracking
    \item Uncertainty framework that tracks all assumptions
    \item Decision error detection (significance reversals)
    \item statcheck-compatible API for batch processing
    \item Multiple export formats: HTML, CSV, JSON
  }
  
  \strong{Supported Test Types:}
  \itemize{
    \item t-tests (independent and paired)
    \item F-tests / ANOVA
    \item Correlations (r)
    \item Chi-square tests
    \item z-tests
  }
  
  \strong{Supported Effect Sizes:}
  \itemize{
    \item Mean differences: Cohen's d, Hedges' g, dz, dav, drm
    \item ANOVA: eta-squared, partial eta-squared, omega-squared, Cohen's f
    \item Correlations: Pearson r
    \item Contingency: phi, Cramer's V
    \item Regression: standardized beta, partial r, f-squared, R-squared
  }
}
\section{Core Functions}{
  \describe{
    \item{\code{\link{check_text}}}{Check plain text for statistical consistency}
    \item{\code{\link{check_file}}}{Check a single file}
    \item{\code{\link{check_files}}}{Check multiple files}
    \item{\code{\link{check_dir}}}{Check all files in a directory}
  }
}
\section{statcheck-Compatible Functions}{
  \describe{
    \item{\code{\link{checkPDF}}}{Check PDF files}
    \item{\code{\link{checkHTML}}}{Check HTML files}
    \item{\code{\link{checkPDFdir}}}{Check directory of PDFs}
    \item{\code{\link{checkHTMLdir}}}{Check directory of HTML files}
    \item{\code{\link{checkDOCXdir}}}{Check directory of Word documents}
  }
}
\section{S3 Methods}{
  \describe{
    \item{\code{print}}{Print formatted summary}
    \item{\code{summary}}{Comprehensive summary statistics}
    \item{\code{plot}}{Visualize results}
  }
}
\section{Filter Functions}{
  \describe{
    \item{\code{\link{identify}}}{Filter problematic results}
    \item{\code{\link{get_errors}}}{Get ERROR status results}
    \item{\code{\link{get_warnings}}}{Get WARN status results}
    \item{\code{\link{get_decision_errors}}}{Get significance reversals}
    \item{\code{\link{filter_by_test_type}}}{Filter by test type}
    \item{\code{\link{filter_by_uncertainty}}}{Filter by uncertainty level}
    \item{\code{\link{count_by}}}{Count results by category}
  }
}
\section{Export Functions}{
  \describe{
    \item{\code{\link{render_report}}}{Generate HTML report}
    \item{\code{\link{export_csv}}}{Export to CSV}
    \item{\code{\link{export_json}}}{Export to JSON}
  }
}
\author{
  Your Name
}
\seealso{
  \code{\link{check_text}}, \code{\link{checkPDF}}, \code{\link{summary.effectcheck}}
}
\examples{
\dontrun{
# Check text directly
text <- "t(28) = 2.21, p = .035, d = 0.80"
result <- check_text(text)
print(result)
summary(result)

# Check PDF files
results <- checkPDF(c("paper1.pdf", "paper2.pdf"))

# Check directory
results <- check_dir("manuscripts/")

# Filter and export
errors <- get_errors(results)
export_csv(results, "results.csv")
render_report(results, "report.html")
}
}
\keyword{package}
