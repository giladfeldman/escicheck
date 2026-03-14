#' Logging Infrastructure for EffectCheck
#'
#' Provides structured logging capabilities with fallback to standard R messaging.
#'
#' @name effectcheck-logging
#' @keywords internal
NULL

#' Initialize Logger
#'
#' Sets up the logging configuration.
#'
#' @param level Logging level (default "INFO")
#' @param file Optional file path to log to
#' @param console Logical, whether to log to console (default TRUE)
#' @return Invisible NULL. Called for its side effect of configuring the logger.
#' @keywords internal
init_logger <- function(level = c("DEBUG", "INFO", "WARN", "ERROR"),
                        file = NULL,
                        console = TRUE) {
    level <- match.arg(level)

    if (requireNamespace("logger", quietly = TRUE)) {
        logger::log_threshold(level)

        # Reset appenders
        appenders <- list()

        if (console) {
            appenders <- c(appenders, logger::appender_console)
        }

        if (!is.null(file)) {
            appenders <- c(appenders, logger::appender_file(file))
        }

        if (length(appenders) > 0) {
            if (length(appenders) == 1) {
                logger::log_appender(appenders[[1]])
            } else {
                # logger doesn't natively support multiple appenders easily without tee-ing
                # simpler to just use console if both selected or fallback
                logger::log_appender(logger::appender_tee(file))
            }
        }
    } else {
        # Fallback storage for options
        options(effectcheck.logging = TRUE)
        options(effectcheck.log_file = file)
        options(effectcheck.log_level = level)
    }
}

#' Log Information Message
#'
#' @param msg Message string (supports glue-style interpolation)
#' @param ... variables for interpolation
#' @return Invisible NULL. Called for its side effect of logging.
#' @keywords internal
log_info <- function(msg, ...) {
    if (requireNamespace("logger", quietly = TRUE)) {
        logger::log_info(msg, ...)
    } else {
        # Simple fallback
        formatted_msg <- tryCatch(glue::glue(msg, ...), error = function(e) paste(msg, ..., collapse = " "))
        message(sprintf("[INFO] %s", formatted_msg))

        log_file <- getOption("effectcheck.log_file")
        if (!is.null(log_file)) {
            cat(sprintf("[%s] [INFO] %s\n", Sys.time(), formatted_msg), file = log_file, append = TRUE)
        }
    }
}

#' Log Error Message
#'
#' @param msg Message string
#' @param ... variables for interpolation
#' @return Invisible NULL. Called for its side effect of logging.
#' @keywords internal
log_error <- function(msg, ...) {
    if (requireNamespace("logger", quietly = TRUE)) {
        # logger::log_error handles captured locals, but we pass ... for glue
        logger::log_error(msg, ...)
    } else {
        formatted_msg <- tryCatch(glue::glue(msg, ...), error = function(e) paste(msg, ..., collapse = " "))
        warning(sprintf("[ERROR] %s", formatted_msg), call. = FALSE)

        log_file <- getOption("effectcheck.log_file")
        if (!is.null(log_file)) {
            cat(sprintf("[%s] [ERROR] %s\n", Sys.time(), formatted_msg), file = log_file, append = TRUE)
        }
    }
}

#' Safe Stop
#'
#' Stops execution with a sanitized error message in production,
#' or full details in development.
#'
#' @param msg Internal detailed error message
#' @param public_msg Optional public-facing message (default: generic error)
#' @return Does not return; always calls [stop()].
#' @keywords internal
safe_stop <- function(msg, public_msg = "An error occurred during processing.") {
    is_production <- getOption("effectcheck.production_mode", FALSE)

    # Log the detail regardless
    log_error(msg)

    if (is_production) {
        stop(public_msg, call. = FALSE)
    } else {
        stop(msg, call. = FALSE)
    }
}
