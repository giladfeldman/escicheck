#' Configuration Management for EffectCheck
#'
#' Retrieves configuration values from environment variables, options, or defaults.
#'
#' @keywords internal
NULL

#' Get Configuration Value
#'
#' Prioritizes:
#' 1. R Options (effectcheck.key)
#' 2. Environment Variables (EFFECTCHECK_KEY)
#' 3. Default value
#'
#' @param key Configuration key (lowercase)
#' @param default Default value if not found
#' @return Configuration value
#' @export
get_config <- function(key, default = NULL) {
    # 1. Check Options
    opt_key <- paste0("effectcheck.", key)
    opt_val <- getOption(opt_key)
    if (!is.null(opt_val)) {
        return(opt_val)
    }

    # 2. Check Environment Variables
    env_key <- paste0("EFFECTCHECK_", toupper(key))
    env_val <- Sys.getenv(env_key)
    if (env_val != "") {
        # Attempt to coerce to appropriate type if default is provided
        if (!is.null(default)) {
            if (is.numeric(default)) {
                num_val <- suppressWarnings(as.numeric(env_val))
                if (!is.na(num_val)) {
                    return(num_val)
                }
            } else if (is.logical(default)) {
                if (tolower(env_val) %in% c("true", "1", "yes")) {
                    return(TRUE)
                }
                if (tolower(env_val) %in% c("false", "0", "no")) {
                    return(FALSE)
                }
            }
        }
        return(env_val)
    }

    # 3. Return Default
    return(default)
}

#' Get Tolerance Config
#'
#' Helper to get tolerances, falling back to constants.
#'
#' @param type Type of tolerance ("effect", "ci", "p")
#' @export
get_tolerance <- function(type = c("effect", "ci", "p")) {
    type <- match.arg(type)

    if (type == "effect") {
        # Check if there's an option for the whole list
        tol <- get_config("tol_effect", DEFAULT_TOL_EFFECT)
        # If it's a JSON string from env var, parse it
        if (is.character(tol) && grepl("^\\{", tol)) {
            tryCatch(
                {
                    json_tol <- jsonlite::fromJSON(tol)
                    # Merge with defaults
                    parsed_tol <- utils::modifyList(DEFAULT_TOL_EFFECT, as.list(json_tol))
                    return(parsed_tol)
                },
                error = function(e) DEFAULT_TOL_EFFECT
            )
        }
        return(tol)
    } else if (type == "ci") {
        return(get_config("tol_ci", DEFAULT_TOL_CI))
    } else if (type == "p") {
        return(get_config("tol_p", DEFAULT_TOL_P))
    }
}
