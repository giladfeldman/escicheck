#' EffectCheck S3 Class Definition and Methods
#'
#' This file defines the effectcheck S3 class and its associated methods
#' for printing, summarizing, and plotting results.

# Safe version getter - works both as installed package and when source()'d
.effectcheck_version <- function() {
  tryCatch(
    as.character(utils::packageVersion("effectcheck")),
    error = function(e) "0.2.0"
  )
}

#' Create an effectcheck object
#'
#' Wraps a tibble of results with the effectcheck S3 class and metadata.
#'
#' @param x A tibble of check results
#' @param call The original function call (for reproducibility)
#' @param settings List of settings used for the check
#' @return An effectcheck S3 object
#' @keywords internal
new_effectcheck <- function(x, call = NULL, settings = list()) {
  stopifnot(inherits(x, "data.frame"))

  structure(
    x,
    class = c("effectcheck", class(x)),
    effectcheck_version = .effectcheck_version(),
    call = call,
    settings = settings,
    generated = Sys.time()
  )
}

#' Test if object is an effectcheck object
#'
#' @param x Object to test
#' @return Logical
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' is.effectcheck(res)
is.effectcheck <- function(x) {
  inherits(x, "effectcheck")
}

#' Print method for effectcheck objects
#'
#' Displays a formatted summary of effectcheck results.
#'
#' @param x An effectcheck object
#' @param short Logical, if TRUE show abbreviated output (default TRUE)
#' @param n Maximum number of rows to display (default 10)
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns `x`.
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' print(res)
print.effectcheck <- function(x, short = TRUE, n = 10, ...) {
  cat("\n")
  cat("=== EffectCheck Results ===\n")
  cat("\n")

  # Basic counts
  total <- nrow(x)
  if (total == 0) {
    cat("No statistics detected.\n")
    return(invisible(x))
  }

  # Status counts
  status_counts <- if ("status" %in% names(x)) {
    table(x$status, useNA = "ifany")
  } else {
    table(factor(character(0), levels = c("PASS", "WARN", "ERROR", "INSUFFICIENT_DATA")))
  }
  pass_n <- if ("PASS" %in% names(status_counts)) status_counts["PASS"] else 0
  warn_n <- if ("WARN" %in% names(status_counts)) status_counts["WARN"] else 0
  error_n <- if ("ERROR" %in% names(status_counts)) status_counts["ERROR"] else 0
  insuff_n <- if ("INSUFFICIENT_DATA" %in% names(status_counts)) status_counts["INSUFFICIENT_DATA"] else 0

  cat(sprintf("Total statistics found: %d\n", total))
  cat(sprintf(
    "  PASS: %d | WARN: %d | ERROR: %d | INSUFFICIENT: %d\n",
    pass_n, warn_n, error_n, insuff_n
  ))
  cat("\n")

  # Test type distribution
  if ("test_type" %in% names(x)) {
    test_counts <- table(x$test_type, useNA = "ifany")
    cat("Test types: ")
    cat(paste(sprintf("%s(%d)", names(test_counts), test_counts), collapse = ", "))
    cat("\n")
  }

  # Uncertainty distribution
  if ("uncertainty_level" %in% names(x)) {
    unc_counts <- table(x$uncertainty_level, useNA = "ifany")
    cat("Uncertainty: ")
    cat(paste(sprintf("%s(%d)", names(unc_counts), unc_counts), collapse = ", "))
    cat("\n")
  }

  cat("\n")

  if (!short) {
    # Show detailed results
    cat("--- Detailed Results ---\n\n")

    # Select key columns for display
    display_cols <- c(
      "location", "test_type", "stat_value", "effect_reported",
      "status", "delta_effect_abs", "uncertainty_level"
    )
    display_cols <- display_cols[display_cols %in% names(x)]

    if (length(display_cols) > 0) {
      display_data <- x[seq_len(min(n, nrow(x))), display_cols, drop = FALSE]
      print(display_data, n = n)

      if (nrow(x) > n) {
        cat(sprintf("\n... and %d more rows\n", nrow(x) - n))
      }
    }
  } else {
    cat("Use print(x, short = FALSE) for detailed results.\n")
    cat("Use summary(x) for comprehensive statistics.\n")
  }

  # Show source files if present
  if ("source" %in% names(x) || "file" %in% names(x)) {
    file_col <- if ("source" %in% names(x)) "source" else "file"
    files <- unique(x[[file_col]])
    files <- files[!is.na(files) & files != "" & files != "text"]
    if (length(files) > 0) {
      cat(sprintf("\nSource files: %d\n", length(files)))
      if (length(files) <= 5) {
        cat(paste("  -", files, collapse = "\n"), "\n")
      } else {
        cat(paste("  -", files[1:3], collapse = "\n"), "\n")
        cat(sprintf("  ... and %d more\n", length(files) - 3))
      }
    }
  }

  cat("\n")
  invisible(x)
}

#' Summary method for effectcheck objects
#'
#' Provides comprehensive summary statistics for effectcheck results.
#'
#' @param object An effectcheck object
#' @param ... Additional arguments (ignored)
#' @return A list of class "summary.effectcheck" containing summary statistics
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' summary(res)
summary.effectcheck <- function(object, ...) {
  x <- object

  result <- list(
    total = nrow(x),
    version = attr(x, "effectcheck_version"),
    generated = attr(x, "generated"),
    call = attr(x, "call"),
    settings = attr(x, "settings")
  )

  if (nrow(x) == 0) {
    result$status_counts <- integer(0)
    result$test_type_counts <- integer(0)
    result$uncertainty_counts <- integer(0)
    result$design_counts <- integer(0)
    result$error_rate <- 0
    result$decision_error_rate <- 0
    class(result) <- "summary.effectcheck"
    return(result)
  }

  # Status distribution
  result$status_counts <- table(x$status, useNA = "ifany")

  # Test type distribution
  if ("test_type" %in% names(x)) {
    result$test_type_counts <- table(x$test_type, useNA = "ifany")
  }

  # Uncertainty distribution
  if ("uncertainty_level" %in% names(x)) {
    result$uncertainty_counts <- table(x$uncertainty_level, useNA = "ifany")
  }

  # Design inference distribution
  if ("design_inferred" %in% names(x)) {
    result$design_counts <- table(x$design_inferred, useNA = "ifany")
  }

  # Error rate
  error_n <- if ("ERROR" %in% names(result$status_counts)) result$status_counts["ERROR"] else 0
  result$error_rate <- error_n / nrow(x)

  # Decision error rate (significance reversal)
  if ("decision_error" %in% names(x)) {
    result$decision_error_count <- sum(x$decision_error, na.rm = TRUE)
    result$decision_error_rate <- result$decision_error_count / nrow(x)
  } else {
    result$decision_error_count <- NA
    result$decision_error_rate <- NA
  }

  # Effect size discrepancy statistics
  if ("delta_effect_abs" %in% names(x)) {
    deltas <- x$delta_effect_abs[!is.na(x$delta_effect_abs)]
    if (length(deltas) > 0) {
      result$delta_effect_mean <- mean(deltas)
      result$delta_effect_median <- median(deltas)
      result$delta_effect_max <- max(deltas)
      result$delta_effect_sd <- sd(deltas)
    }
  }

  # CI match rate
  if ("ci_match" %in% names(x)) {
    ci_vals <- x$ci_match[!is.na(x$ci_match)]
    if (length(ci_vals) > 0) {
      result$ci_match_rate <- mean(ci_vals)
    }
  }

  # File statistics
  if ("source" %in% names(x) || "file" %in% names(x)) {
    file_col <- if ("source" %in% names(x)) "source" else "file"
    files <- unique(x[[file_col]])
    files <- files[!is.na(files) & files != "" & files != "text"]
    result$n_files <- length(files)
    result$files <- files
  }

  class(result) <- "summary.effectcheck"
  result
}

#' Print method for summary.effectcheck objects
#'
#' @param x A summary.effectcheck object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns `x`.
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' s <- summary(res)
#' print(s)
print.summary.effectcheck <- function(x, ...) {
  cat("\n")
  cat("========================================\n")
  cat("     EffectCheck Summary Report\n")
  cat("========================================\n")
  cat("\n")

  if (!is.null(x$version)) {
    cat(sprintf("Version: %s\n", x$version))
  }
  if (!is.null(x$generated)) {
    cat(sprintf("Generated: %s\n", as.character(x$generated)))
  }
  cat("\n")

  cat(sprintf("Total statistics analyzed: %d\n", x$total))
  cat("\n")

  if (x$total == 0) {
    cat("No statistics detected.\n")
    return(invisible(x))
  }

  # Status breakdown
  cat("--- Status Distribution ---\n")
  if (length(x$status_counts) > 0) {
    for (status in names(x$status_counts)) {
      pct <- round(100 * x$status_counts[status] / x$total, 1)
      bar_len <- round(30 * x$status_counts[status] / x$total)
      bar <- paste(rep("#", bar_len), collapse = "")
      cat(sprintf(
        "  %-15s %4d (%5.1f%%) %s\n",
        status, x$status_counts[status], pct, bar
      ))
    }
  }
  cat("\n")

  # Test type breakdown
  if (!is.null(x$test_type_counts) && length(x$test_type_counts) > 0) {
    cat("--- Test Types ---\n")
    for (tt in names(x$test_type_counts)) {
      cat(sprintf("  %-10s %4d\n", tt, x$test_type_counts[tt]))
    }
    cat("\n")
  }

  # Uncertainty breakdown
  if (!is.null(x$uncertainty_counts) && length(x$uncertainty_counts) > 0) {
    cat("--- Uncertainty Levels ---\n")
    for (unc in names(x$uncertainty_counts)) {
      cat(sprintf("  %-10s %4d\n", unc, x$uncertainty_counts[unc]))
    }
    cat("\n")
  }

  # Design inference breakdown
  if (!is.null(x$design_counts) && length(x$design_counts) > 0) {
    cat("--- Design Inferred ---\n")
    for (des in names(x$design_counts)) {
      cat(sprintf("  %-15s %4d\n", des, x$design_counts[des]))
    }
    cat("\n")
  }

  # Key metrics
  cat("--- Key Metrics ---\n")
  cat(sprintf("  Error rate:           %5.1f%%\n", 100 * x$error_rate))

  if (!is.na(x$decision_error_rate)) {
    cat(sprintf(
      "  Decision error rate:  %5.1f%% (%d cases)\n",
      100 * x$decision_error_rate, x$decision_error_count
    ))
  }

  if (!is.null(x$ci_match_rate)) {
    cat(sprintf("  CI match rate:        %5.1f%%\n", 100 * x$ci_match_rate))
  }

  if (!is.null(x$delta_effect_mean)) {
    cat(sprintf("  Mean effect delta:    %.4f\n", x$delta_effect_mean))
    cat(sprintf("  Median effect delta:  %.4f\n", x$delta_effect_median))
    cat(sprintf("  Max effect delta:     %.4f\n", x$delta_effect_max))
  }
  cat("\n")

  # File info
  if (!is.null(x$n_files) && x$n_files > 0) {
    cat(sprintf("Files processed: %d\n", x$n_files))
  }

  cat("========================================\n")
  cat("\n")

  invisible(x)
}

#' Plot method for effectcheck objects
#'
#' Creates visualizations of effectcheck results.
#'
#' @param x An effectcheck object
#' @param type Type of plot: "status", "uncertainty", "test_type", "delta", or "all"
#' @param ... Additional arguments passed to plotting functions
#' @return Invisibly returns `x`.
#' @export
#' @examples
#' \donttest{
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' plot(res, type = "status")
#' }
plot.effectcheck <- function(x, type = c("status", "uncertainty", "test_type", "delta", "all"), ...) {
  type <- match.arg(type)

  if (nrow(x) == 0) {
    message("No data to plot.")
    return(invisible(x))
  }

  # Store original par settings

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  if (type == "all") {
    # Create multi-panel plot
    graphics::par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    plot_status(x)
    plot_uncertainty(x)
    plot_test_type(x)
    plot_delta(x)
  } else if (type == "status") {
    plot_status(x)
  } else if (type == "uncertainty") {
    plot_uncertainty(x)
  } else if (type == "test_type") {
    plot_test_type(x)
  } else if (type == "delta") {
    plot_delta(x)
  }

  invisible(x)
}

#' Plot status distribution
#' @keywords internal
plot_status <- function(x) {
  if (!"status" %in% names(x)) {
    message("No status column found.")
    return(invisible(NULL))
  }

  counts <- table(x$status, useNA = "ifany")

  # Define colors for each status
  status_colors <- c(
    "PASS" = "#28a745",
    "WARN" = "#ffc107",
    "ERROR" = "#dc3545",
    "INSUFFICIENT_DATA" = "#6c757d"
  )

  colors <- sapply(names(counts), function(s) {
    if (s %in% names(status_colors)) status_colors[s] else "#999999"
  })

  graphics::barplot(counts,
    main = "Status Distribution",
    col = colors,
    ylab = "Count",
    las = 2,
    cex.names = 0.8
  )
}

#' Plot uncertainty distribution
#' @keywords internal
plot_uncertainty <- function(x) {
  if (!"uncertainty_level" %in% names(x)) {
    message("No uncertainty_level column found.")
    return(invisible(NULL))
  }

  counts <- table(x$uncertainty_level, useNA = "ifany")

  # Define colors for each level
  unc_colors <- c(
    "low" = "#28a745",
    "medium" = "#ffc107",
    "high" = "#dc3545"
  )

  colors <- sapply(names(counts), function(u) {
    if (u %in% names(unc_colors)) unc_colors[u] else "#999999"
  })

  graphics::barplot(counts,
    main = "Uncertainty Levels",
    col = colors,
    ylab = "Count",
    las = 2,
    cex.names = 0.8
  )
}

#' Plot test type distribution
#' @keywords internal
plot_test_type <- function(x) {
  if (!"test_type" %in% names(x)) {
    message("No test_type column found.")
    return(invisible(NULL))
  }

  counts <- table(x$test_type, useNA = "ifany")

  graphics::barplot(counts,
    main = "Test Type Distribution",
    col = "#3498db",
    ylab = "Count",
    las = 2,
    cex.names = 0.8
  )
}

#' Plot effect size delta distribution
#' @keywords internal
plot_delta <- function(x) {
  if (!"delta_effect_abs" %in% names(x)) {
    message("No delta_effect_abs column found.")
    return(invisible(NULL))
  }

  deltas <- x$delta_effect_abs[!is.na(x$delta_effect_abs)]

  if (length(deltas) == 0) {
    message("No delta values to plot.")
    return(invisible(NULL))
  }

  graphics::hist(deltas,
    main = "Effect Size Discrepancy Distribution",
    xlab = "Absolute Delta",
    col = "#3498db",
    border = "white",
    breaks = 20
  )

  # Add vertical line at common tolerance
  graphics::abline(v = 0.02, col = "#dc3545", lty = 2, lwd = 2)
  graphics::legend("topright",
    legend = "Tolerance (0.02)",
    col = "#dc3545",
    lty = 2,
    lwd = 2,
    bty = "n"
  )
}

#' Subset method for effectcheck objects
#'
#' Preserves effectcheck class when subsetting.
#'
#' @param x An effectcheck object
#' @param ... Subsetting arguments
#' @return An effectcheck object
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035. F(1, 50) = 4.03, p = .049")
#' res[1, ]
`[.effectcheck` <- function(x, ...) {
  result <- NextMethod("[")

  # Preserve effectcheck class and attributes
  if (inherits(result, "data.frame")) {
    class(result) <- c("effectcheck", class(result))
    attr(result, "effectcheck_version") <- attr(x, "effectcheck_version")
    attr(result, "call") <- attr(x, "call")
    attr(result, "settings") <- attr(x, "settings")
    attr(result, "generated") <- attr(x, "generated")
  }

  result
}

#' Combine effectcheck objects
#'
#' @param ... effectcheck objects to combine
#' @return Combined effectcheck object
#' @export
#' @examples
#' res1 <- check_text("t(28) = 2.21, p = .035")
#' res2 <- check_text("F(1, 50) = 4.03, p = .049")
#' combined <- rbind(res1, res2)
rbind.effectcheck <- function(...) {
  objects <- list(...)

  # Get first object's attributes
  first <- objects[[1]]

  # Combine using dplyr::bind_rows
  combined <- dplyr::bind_rows(objects)

  # Restore effectcheck class
  new_effectcheck(
    combined,
    call = attr(first, "call"),
    settings = attr(first, "settings")
  )
}

# ============================================================================
# Helper functions for working with variant structure
# ============================================================================

#' Get all variants for a specific row
#'
#' Extracts and parses the all_variants JSON structure for a given row.
#'
#' @param x An effectcheck object
#' @param row_index The row index to extract variants from
#' @return A list with same_type and alternatives sublists
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' get_variants(res, 1)
get_variants <- function(x, row_index = 1) {
  if (!is.effectcheck(x)) {
    stop("Input must be an effectcheck object")
  }

  if (row_index > nrow(x) || row_index < 1) {
    stop("Row index out of bounds")
  }

  if (!"all_variants" %in% names(x)) {
    return(list(same_type = list(), alternatives = list()))
  }

  json_str <- x$all_variants[row_index]
  if (is.na(json_str) || json_str == "" || json_str == "{}") {
    return(list(same_type = list(), alternatives = list()))
  }

  tryCatch(
    {
      jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    },
    error = function(e) {
      list(same_type = list(), alternatives = list())
    }
  )
}

#' Get same-type variants for a row
#'
#' @param x An effectcheck object
#' @param row_index The row index
#' @return A list of same-type variants with their values and metadata
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' get_same_type_variants(res, 1)
get_same_type_variants <- function(x, row_index = 1) {
  variants <- get_variants(x, row_index)
  variants$same_type
}

#' Get alternative suggestions for a row
#'
#' @param x An effectcheck object
#' @param row_index The row index
#' @return A list of alternative effect size suggestions
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' get_alternatives(res, 1)
get_alternatives <- function(x, row_index = 1) {
  variants <- get_variants(x, row_index)
  variants$alternatives
}

#' Format variants for display
#'
#' Creates a formatted string representation of variants for a row.
#'
#' @param x An effectcheck object
#' @param row_index The row index
#' @param include_alternatives Whether to include alternative suggestions
#' @return A character string with formatted variant information
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' cat(format_variants(res, 1))
format_variants <- function(x, row_index = 1, include_alternatives = TRUE) {
  variants <- get_variants(x, row_index)

  lines <- c()

  # Same-type variants
  if (length(variants$same_type) > 0) {
    lines <- c(lines, "Same-Type Variants:")
    for (vname in names(variants$same_type)) {
      v <- variants$same_type[[vname]]
      value <- if (!is.null(v$value)) signif(v$value, 4) else "NA"
      assumptions <- if (!is.null(v$assumptions) && v$assumptions != "") {
        paste0(" (", v$assumptions, ")")
      } else {
        ""
      }
      lines <- c(lines, sprintf("  %s = %s%s", vname, value, assumptions))
    }
  }

  # Alternatives
  if (include_alternatives && length(variants$alternatives) > 0) {
    lines <- c(lines, "", "Alternative Suggestions:")
    for (vname in names(variants$alternatives)) {
      v <- variants$alternatives[[vname]]
      value <- if (!is.null(v$value)) signif(v$value, 4) else "NA"
      type_info <- if (!is.null(v$type) && v$type != "") {
        paste0(" [", v$type, "]")
      } else {
        ""
      }
      why <- if (!is.null(v$why_consider) && v$why_consider != "") {
        paste0("\n      ", v$why_consider)
      } else {
        ""
      }
      lines <- c(lines, sprintf("  %s = %s%s%s", vname, value, type_info, why))
    }
  }

  paste(lines, collapse = "\n")
}

#' Get variant metadata
#'
#' Returns metadata for a specific effect size variant type.
#'
#' @param variant_name The name of the variant (e.g., "d_ind", "dz", "eta2")
#' @return A list with name, assumptions, when_to_use, and formula
#' @export
#' @examples
#' get_variant_metadata("d_ind")
get_variant_metadata <- function(variant_name) {
  # Access VARIANT_METADATA from check.R (works as package or source()'d)
  ns <- tryCatch(asNamespace("effectcheck"), error = function(e) globalenv())
  if (exists("VARIANT_METADATA", envir = ns)) {
    metadata <- get("VARIANT_METADATA", envir = ns)
    if (variant_name %in% names(metadata)) {
      return(metadata[[variant_name]])
    }
  }

  # Fallback - return basic info
  list(
    name = variant_name,
    assumptions = "Unknown",
    when_to_use = "Unknown",
    formula = "Unknown"
  )
}

#' Get effect size family information
#'
#' Returns information about an effect size family and its variants.
#'
#' @param effect_type The effect size type (e.g., "d", "eta2", "r")
#' @return A list with family, variants, alternatives, and description
#' @export
#' @examples
#' get_effect_family("d")
get_effect_family <- function(effect_type) {
  # Access EFFECT_SIZE_FAMILIES from check.R (works as package or source()'d)
  ns <- tryCatch(asNamespace("effectcheck"), error = function(e) globalenv())
  if (exists("EFFECT_SIZE_FAMILIES", envir = ns)) {
    families <- get("EFFECT_SIZE_FAMILIES", envir = ns)
    if (effect_type %in% names(families)) {
      return(families[[effect_type]])
    }
  }

  # Fallback
  list(
    family = effect_type,
    variants = c(effect_type),
    alternatives = c(),
    description = paste("Effect size:", effect_type)
  )
}

#' Compare reported value to all variants
#'
#' Creates a comparison table showing the reported value against all computed variants.
#'
#' @param x An effectcheck object
#' @param row_index The row index
#' @return A data frame with variant comparisons
#' @export
#' @examples
#' res <- check_text("t(28) = 2.21, p = .035, d = 0.80")
#' compare_to_variants(res, 1)
compare_to_variants <- function(x, row_index = 1) {
  if (!is.effectcheck(x)) {
    stop("Input must be an effectcheck object")
  }

  if (row_index > nrow(x) || row_index < 1) {
    stop("Row index out of bounds")
  }

  row <- x[row_index, ]
  reported <- row$effect_reported
  reported_type <- row$reported_type

  variants <- get_variants(x, row_index)

  # Build comparison data frame
  comparisons <- data.frame(
    variant = character(),
    value = numeric(),
    delta = numeric(),
    is_same_type = logical(),
    assumptions = character(),
    stringsAsFactors = FALSE
  )

  # Add same-type variants
  for (vname in names(variants$same_type)) {
    v <- variants$same_type[[vname]]
    value <- if (!is.null(v$value)) v$value else NA_real_
    delta <- if (!is.na(value) && !is.na(reported)) abs(value - reported) else NA_real_
    assumptions <- if (!is.null(v$assumptions)) v$assumptions else ""

    comparisons <- rbind(comparisons, data.frame(
      variant = vname,
      value = value,
      delta = delta,
      is_same_type = TRUE,
      assumptions = assumptions,
      stringsAsFactors = FALSE
    ))
  }

  # Add alternatives
  for (vname in names(variants$alternatives)) {
    v <- variants$alternatives[[vname]]
    value <- if (!is.null(v$value)) v$value else NA_real_
    delta <- if (!is.na(value) && !is.na(reported)) abs(value - reported) else NA_real_
    why <- if (!is.null(v$why_consider)) v$why_consider else ""

    comparisons <- rbind(comparisons, data.frame(
      variant = vname,
      value = value,
      delta = delta,
      is_same_type = FALSE,
      assumptions = why,
      stringsAsFactors = FALSE
    ))
  }

  # Sort by delta
  if (nrow(comparisons) > 0) {
    comparisons <- comparisons[order(comparisons$delta, na.last = TRUE), ]
  }

  # Add reported value info as attribute
  attr(comparisons, "reported_value") <- reported

  attr(comparisons, "reported_type") <- reported_type
  attr(comparisons, "matched_variant") <- row$matched_variant

  comparisons
}
