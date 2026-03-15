#' @importFrom dplyr %>%
#' @importFrom stats median qnorm sd
#' @keywords internal
"_PACKAGE"

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "===========================================================================\n",
    "  effectcheck v", .effectcheck_version(), " - DEVELOPMENT VERSION\n",
    "===========================================================================\n",
    "  WARNING: This package is under heavy development and has NOT yet been\n",
    "  fully validated. Results should be independently verified before use in\n",
    "  any consequential context. Use is at your sole responsibility.\n",
    "\n",
    "  Report issues & help verify: https://github.com/giladfeldman/escicheck\n",
    "  Contact: Gilad Feldman <giladfel@gmail.com>\n",
    "==========================================================================="
  )
}
