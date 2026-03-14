#' Constants for EffectCheck
#'
#' Default values and tolerances used across the package.
#'
#' @keywords internal

# Effect Size Tolerances (Defaults)
DEFAULT_TOL_EFFECT <- list(
    d = 0.02,
    g = 0.02,
    dz = 0.02,
    dav = 0.02,
    drm = 0.02,
    r = 0.005,
    phi = 0.02,
    V = 0.02,
    eta2 = 0.01,
    etap2 = 0.01,
    omega2 = 0.01,
    cohens_f = 0.02,
    beta = 0.01,
    partial_r = 0.005,
    f2 = 0.02,
    R2 = 0.01,
    rank_biserial_r = 0.02,
    cliffs_delta = 0.02,
    epsilon_squared = 0.01,
    kendalls_W = 0.01
)

# Other Tolerances
DEFAULT_TOL_CI <- 0.02
DEFAULT_TOL_P <- 0.001

# Defaults for checking
DEFAULT_ALPHA <- 0.05
DEFAULT_CI_LEVEL <- 0.95

# Resource Limits
DEFAULT_MAX_TEXT_LENGTH <- 10^7 # 10 MB
DEFAULT_MAX_STATS_PER_TEXT <- 10000

# CI Methods
DEFAULT_CI_METHOD_PHI <- "bonett_price"
DEFAULT_CI_METHOD_V <- "bonett_price"

# File Extensions (Regex)
REGEX_FILE_EXTENSIONS <- "\\.(pdf|html?|docx|txt)$"
