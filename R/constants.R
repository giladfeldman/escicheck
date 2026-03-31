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
    epsilon2_anova = 0.01,
    partial_omega2 = 0.01,
    bias_corrected_eta2 = 0.01,
    kendalls_W = 0.01,
    h = 0.02,
    OR = 0.5,
    RR = 0.5,
    IRR = 0.5
)

# Other Tolerances
DEFAULT_TOL_CI <- 0.02
DEFAULT_TOL_P <- 0.001

# Defaults for checking
DEFAULT_ALPHA <- 0.05
DEFAULT_CI_LEVEL <- 0.95

# Extreme Delta Threshold — deltas above this are flagged as likely extraction errors
EXTREME_DELTA_THRESHOLD <- 1.0

# Plausibility Bounds — effect sizes above these are flagged as likely extraction errors
EFFECT_PLAUSIBILITY <- list(
    d = 5, g = 5, dz = 5, dav = 5, drm = 5,
    r = 1, phi = 1, V = 1,
    eta2 = 1, etap2 = 1, omega2 = 1, partial_omega2 = 1, epsilon2_anova = 1,
    cohens_f = 10, f2 = 100,
    beta = 10, R2 = 1,
    OR = 100, RR = 100, IRR = 100, h = 3.15,
    rank_biserial_r = 1, cliffs_delta = 1,
    epsilon_squared = 1, kendalls_W = 1
)

# Default cross-type action — status when no same-type variant can be computed
DEFAULT_CROSS_TYPE_ACTION <- "NOTE"

# Default design-ambiguous action — status when t-test (or F(1,df)) effect size
# ERROR occurs with ambiguous design. d-from-t systematically differs from
# d-from-raw-data when groups are unequal or different SD pooling is used.
DEFAULT_DESIGN_AMBIGUOUS_ACTION <- "WARN"

# Resource Limits
DEFAULT_MAX_TEXT_LENGTH <- 10^7 # 10 MB
DEFAULT_MAX_STATS_PER_TEXT <- 10000

# CI Methods
DEFAULT_CI_METHOD_PHI <- "bonett_price"
DEFAULT_CI_METHOD_V <- "bonett_price"

# File Extensions (Regex)
REGEX_FILE_EXTENSIONS <- "\\.(pdf|html?|docx|txt)$"
