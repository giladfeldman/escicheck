
#' Identify and Filter EffectCheck Results
#'
#' Functions for filtering and identifying problematic results
#' in effectcheck output.

#' Identify problematic results
#'
#' Filters effectcheck results to show only problematic cases based on
#' specified criteria.
#'
#' @param x An effectcheck object
#' @param what Character vector specifying what to identify:
#'   - "errors": Results with ERROR status
#'   - "warnings": Results with WARN status
#'   - "decision_errors": Results with significance reversal
#'   - "high_uncertainty": Results with high uncertainty level
#'   - "insufficient": Results with insufficient data
#'   - "all_problems": All of the above
#' @param ... Additional arguments (ignored)
#' @return An effectcheck object containing only the identified results
#' @export
#' @examples
#' \dontrun{
#' results <- check_dir("manuscripts/")
#' 
#' # Get only errors
#' errors <- ec_identify(results, "errors")
#' 
#' # Get errors and warnings
#' problems <- ec_identify(results, c("errors", "warnings"))
#' 
#' # Get all problematic results
#' all_problems <- ec_identify(results, "all_problems")
#' }
ec_identify <- function(x, what = c("errors", "warnings", "decision_errors", 
                                  "high_uncertainty", "insufficient", "all_problems"),
                     ...) {
  if (!inherits(x, "effectcheck")) {
    stop("x must be an effectcheck object")
  }
  
  if (nrow(x) == 0) {
    return(x)
  }
  
  what <- match.arg(what, several.ok = TRUE)
  
  # Handle "all_problems" shortcut
  if ("all_problems" %in% what) {
    what <- c("errors", "warnings", "decision_errors", "high_uncertainty", "insufficient")
  }
  
  # Build filter mask
  mask <- rep(FALSE, nrow(x))
  
  if ("errors" %in% what && "status" %in% names(x)) {
    mask <- mask | (x$status == "ERROR" & !is.na(x$status))
  }
  
  if ("warnings" %in% what && "status" %in% names(x)) {
    mask <- mask | (x$status == "WARN" & !is.na(x$status))
  }
  
  if ("decision_errors" %in% what && "decision_error" %in% names(x)) {
    mask <- mask | (x$decision_error == TRUE & !is.na(x$decision_error))
  }
  
  if ("high_uncertainty" %in% what && "uncertainty_level" %in% names(x)) {
    mask <- mask | (x$uncertainty_level == "high" & !is.na(x$uncertainty_level))
  }
  
  if ("insufficient" %in% what) {
    if ("status" %in% names(x)) {
      mask <- mask | (x$status == "INSUFFICIENT_DATA" & !is.na(x$status))
    }
    if ("insufficient_data" %in% names(x)) {
      mask <- mask | (x$insufficient_data == TRUE & !is.na(x$insufficient_data))
    }
  }
  
  # Return filtered results
  x[mask, ]
}

#' Get errors from effectcheck results
#'
#' Convenience function to extract only ERROR status results.
#'
#' @param x An effectcheck object
#' @return An effectcheck object containing only errors
#' @export
#' @examples
#' \dontrun{
#' results <- check_dir("manuscripts/")
#' errors <- get_errors(results)
#' }
get_errors <- function(x) {
  ec_identify(x, "errors")
}

#' Get warnings from effectcheck results
#'
#' Convenience function to extract only WARN status results.
#'
#' @param x An effectcheck object
#' @return An effectcheck object containing only warnings
#' @export
#' @examples
#' \dontrun{
#' results <- check_dir("manuscripts/")
#' warnings <- get_warnings(results)
#' }
get_warnings <- function(x) {
  ec_identify(x, "warnings")
}

#' Get decision errors from effectcheck results
#'
#' Extracts results where the significance decision would be reversed
#' (i.e., reported as significant when computed is not, or vice versa).
#'
#' @param x An effectcheck object
#' @return An effectcheck object containing only decision errors
#' @export
#' @examples
#' \dontrun{
#' results <- check_dir("manuscripts/")
#' decision_errors <- get_decision_errors(results)
#' }
get_decision_errors <- function(x) {
  ec_identify(x, "decision_errors")
}

#' Filter results by test type
#'
#' Filters effectcheck results to show only specific test types.
#'
#' @param x An effectcheck object
#' @param types Character vector of test types to include (e.g., "t", "F", "r", "chisq", "z")
#' @return An effectcheck object containing only the specified test types
#' @export
#' @examples
#' \dontrun{
#' results <- check_dir("manuscripts/")
#' 
#' # Get only t-tests
#' t_tests <- filter_by_test_type(results, "t")
#' 
#' # Get t-tests and F-tests
#' t_and_F <- filter_by_test_type(results, c("t", "F"))
#' }
filter_by_test_type <- function(x, types) {
  if (!inherits(x, "effectcheck")) {
    stop("x must be an effectcheck object")
  }
  
  if (!"test_type" %in% names(x)) {
    warning("No test_type column found in results")
    return(x)
  }
  
  mask <- x$test_type %in% types
  x[mask, ]
}

#' Filter results by uncertainty level
#'
#' Filters effectcheck results by uncertainty level.
#'
#' @param x An effectcheck object
#' @param levels Character vector of uncertainty levels to include ("low", "medium", "high")
#' @return An effectcheck object containing only the specified uncertainty levels
#' @export
#' @examples
#' \dontrun{
#' results <- check_dir("manuscripts/")
#' 
#' # Get only high uncertainty results
#' high_unc <- filter_by_uncertainty(results, "high")
#' 
#' # Get medium and high uncertainty
#' med_high <- filter_by_uncertainty(results, c("medium", "high"))
#' }
filter_by_uncertainty <- function(x, levels) {
  if (!inherits(x, "effectcheck")) {
    stop("x must be an effectcheck object")
  }
  
  if (!"uncertainty_level" %in% names(x)) {
    warning("No uncertainty_level column found in results")
    return(x)
  }
  
  mask <- x$uncertainty_level %in% levels
  x[mask, ]
}

#' Filter results by source file
#'
#' Filters effectcheck results to show only results from specific files.
#'
#' @param x An effectcheck object
#' @param files Character vector of file names or patterns to include
#' @param pattern Logical, if TRUE treat files as regex patterns (default FALSE)
#' @return An effectcheck object containing only results from specified files
#' @export
#' @examples
#' \dontrun{
#' results <- check_dir("manuscripts/")
#' 
#' # Get results from specific file
#' paper1 <- filter_by_source(results, "paper1.pdf")
#' 
#' # Get results matching pattern
#' recent <- filter_by_source(results, "2024.*\\.pdf", pattern = TRUE)
#' }
filter_by_source <- function(x, files, pattern = FALSE) {
  if (!inherits(x, "effectcheck")) {
    stop("x must be an effectcheck object")
  }
  
  # Find source column
  source_col <- if ("source" %in% names(x)) "source" else if ("file" %in% names(x)) "file" else NULL
  
  if (is.null(source_col)) {
    warning("No source or file column found in results")
    return(x)
  }
  
  if (pattern) {
    # Use regex matching
    mask <- grepl(paste(files, collapse = "|"), x[[source_col]], ignore.case = TRUE)
  } else {
    # Exact matching
    mask <- x[[source_col]] %in% files
  }
  
  x[mask, ]
}

#' Filter results by effect size delta
#'
#' Filters effectcheck results by the magnitude of effect size discrepancy.
#'
#' @param x An effectcheck object
#' @param min_delta Minimum absolute delta to include (default 0)
#' @param max_delta Maximum absolute delta to include (default Inf)
#' @return An effectcheck object containing only results within the delta range
#' @export
#' @examples
#' \dontrun{
#' results <- check_dir("manuscripts/")
#' 
#' # Get results with large discrepancies (delta > 0.1)
#' large_delta <- filter_by_delta(results, min_delta = 0.1)
#' 
#' # Get results with small discrepancies (delta < 0.02)
#' small_delta <- filter_by_delta(results, max_delta = 0.02)
#' }
filter_by_delta <- function(x, min_delta = 0, max_delta = Inf) {
  if (!inherits(x, "effectcheck")) {
    stop("x must be an effectcheck object")
  }
  
  if (!"delta_effect_abs" %in% names(x)) {
    warning("No delta_effect_abs column found in results")
    return(x)
  }
  
  delta <- x$delta_effect_abs
  mask <- !is.na(delta) & delta >= min_delta & delta <= max_delta
  x[mask, ]
}

#' Count statistics by category
#'
#' Provides counts of statistics grouped by various categories.
#'
#' @param x An effectcheck object
#' @param by Character, grouping variable: "status", "test_type", "uncertainty", 
#'   "design", or "source"
#' @return A data frame with counts
#' @export
#' @examples
#' \dontrun{
#' results <- check_dir("manuscripts/")
#' 
#' # Count by status
#' count_by(results, "status")
#' 
#' # Count by test type
#' count_by(results, "test_type")
#' }
count_by <- function(x, by = c("status", "test_type", "uncertainty", "design", "source")) {
  if (!inherits(x, "effectcheck")) {
    stop("x must be an effectcheck object")
  }
  
  by <- match.arg(by)
  
  # Map to column names
  col_map <- c(
    status = "status",
    test_type = "test_type",
    uncertainty = "uncertainty_level",
    design = "design_inferred",
    source = "source"
  )
  
  col <- col_map[by]
  
  # Try alternative column names
  if (!col %in% names(x)) {
    if (by == "source" && "file" %in% names(x)) {
      col <- "file"
    } else {
      stop(sprintf("Column '%s' not found in results", col))
    }
  }
  
  # Create counts
  counts <- as.data.frame(table(x[[col]], useNA = "ifany"))
  names(counts) <- c(by, "count")
  
  # Add percentage
  counts$percent <- round(100 * counts$count / sum(counts$count), 1)
  
  # Sort by count descending

  counts <- counts[order(-counts$count), ]
  rownames(counts) <- NULL
  
  counts
}
