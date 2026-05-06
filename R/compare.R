
#' Compare effectcheck results with statcheck
#'
#' Runs both effectcheck and statcheck on the same text and returns a
#' merged comparison tibble.
#'
#' @param text Character string containing APA-formatted statistics
#' @param ... Additional arguments passed to check_text()
#' @return A tibble with source column ("both", "effectcheck_only", "statcheck_only")
#' @export
#' @examples
#' \donttest{
#' comp <- compare_with_statcheck("t(28) = 2.21, p = .035, d = 0.80")
#' print(comp)
#' }
compare_with_statcheck <- function(text, ...) {
  if (!requireNamespace("statcheck", quietly = TRUE)) {
    warning("statcheck package not installed. Install with: install.packages('statcheck')")
    ec <- check_text(text, ...)
    ec$source <- "effectcheck_only"
    ec$statcheck_error <- NA_character_
    result <- ec
    class(result) <- c("effectcheck_comparison", class(result))
    return(result)
  }

  ec <- check_text(text, ...)
  sc <- tryCatch(
    statcheck::statcheck(text),
    error = function(e) {
      warning("statcheck failed: ", conditionMessage(e))
      NULL
    }
  )

  result <- match_results(ec, sc)
  class(result) <- c("effectcheck_comparison", class(result))
  result
}

#' Compare effectcheck and statcheck on a file (DEFUNCT in v0.4.0)
#'
#' Removed in effectcheck 0.4.0. The text-input variant
#' [compare_with_statcheck()] is the supported entry point — extract via
#' docpluck and pass the result.
#'
#' @param path Defunct argument.
#' @param ... Defunct argument.
#' @return Errors with a migration message.
#' @export
#' @keywords internal
compare_file_with_statcheck <- function(path, ...) {
  .Defunct(new = "compare_with_statcheck", package = "effectcheck",
           msg = .extraction_defunct_msg())
}

#' Match effectcheck and statcheck results
#'
#' Uses fuzzy matching on test_type and stat_value to pair results
#' from both tools.
#'
#' @param ec effectcheck results tibble
#' @param sc statcheck results data.frame (or NULL)
#' @return Merged tibble with source column
#' @keywords internal
match_results <- function(ec, sc) {
  if (is.null(sc) || nrow(sc) == 0) {
    ec$source <- "effectcheck_only"
    ec$statcheck_error <- NA_character_
    return(ec)
  }

  # Normalize statcheck test types to effectcheck conventions
  sc_types <- tolower(as.character(sc$statistic))
  sc_types[sc_types == "chi2"] <- "chisq"

  sc_stats <- as.numeric(sc$value)
  sc_matched <- rep(FALSE, nrow(sc))

  ec$source <- NA_character_
  ec$statcheck_error <- NA_character_

  for (i in seq_len(nrow(ec))) {
    ec_type <- tolower(as.character(ec$test_type[i]))
    ec_stat <- as.numeric(ec$stat_value[i])

    if (is.na(ec_stat)) {
      ec$source[i] <- "effectcheck_only"
      next
    }

    # Find closest unmatched statcheck result with same type
    candidates <- which(sc_types == ec_type & !sc_matched)
    if (length(candidates) == 0) {
      ec$source[i] <- "effectcheck_only"
      next
    }

    diffs <- abs(sc_stats[candidates] - ec_stat)
    best <- candidates[which.min(diffs)]

    if (min(diffs) <= 0.1) {
      ec$source[i] <- "both"
      ec$statcheck_error[i] <- if (!is.null(sc$error) && !is.na(sc$error[best])) {
        as.character(sc$error[best])
      } else {
        NA_character_
      }
      sc_matched[best] <- TRUE
    } else {
      ec$source[i] <- "effectcheck_only"
    }
  }

  # Add unmatched statcheck-only results
  sc_only_idx <- which(!sc_matched)
  if (length(sc_only_idx) > 0) {
    for (j in sc_only_idx) {
      sc_row <- tibble::tibble(
        location = NA_integer_,
        raw_text = if (!is.null(sc$raw[j])) as.character(sc$raw[j]) else NA_character_,
        test_type = sc_types[j],
        stat_value = sc_stats[j],
        df1 = if (!is.null(sc$df1[j])) as.numeric(sc$df1[j]) else NA_real_,
        df2 = if (!is.null(sc$df2[j])) as.numeric(sc$df2[j]) else NA_real_,
        p_reported = if (!is.null(sc$reported.p[j])) as.numeric(sc$reported.p[j]) else NA_real_,
        p_computed = if (!is.null(sc$computed[j])) as.numeric(sc$computed[j]) else NA_real_,
        status = NA_character_,
        source = "statcheck_only",
        statcheck_error = if (!is.null(sc$error[j])) as.character(sc$error[j]) else NA_character_
      )
      ec <- dplyr::bind_rows(ec, sc_row)
    }
  }

  ec
}

#' Print method for effectcheck comparison
#'
#' @param x An effectcheck_comparison object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns `x`.
#' @export
#' @examples
#' \donttest{
#' comp <- compare_with_statcheck("t(28) = 2.21, p = .035, d = 0.80")
#' print(comp)
#' }
print.effectcheck_comparison <- function(x, ...) {
  cat("EffectCheck vs statcheck comparison\n")
  cat("-----------------------------------\n")

  src <- x$source
  both_n <- sum(src == "both", na.rm = TRUE)
  ec_only <- sum(src == "effectcheck_only", na.rm = TRUE)
  sc_only <- sum(src == "statcheck_only", na.rm = TRUE)

  cat(sprintf("  Both tools:      %d\n", both_n))
  cat(sprintf("  effectcheck only: %d\n", ec_only))
  cat(sprintf("  statcheck only:   %d\n", sc_only))
  cat(sprintf("  Total rows:       %d\n", nrow(x)))

  if (both_n > 0) {
    matched <- x[src == "both", ]
    agree <- sum(matched$statcheck_error == "FALSE" & matched$status == "PASS", na.rm = TRUE)
    cat(sprintf("  Agreement (both pass): %d / %d\n", agree, both_n))
  }

  invisible(x)
}
