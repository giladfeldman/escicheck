# Suppress R CMD check NOTEs for NSE column references
utils::globalVariables(c("p_reported", "test_type"))

#' Normalize text for parsing
#'
#' Comprehensive normalization pipeline handling Unicode, decimals, whitespace,
#' and CI delimiters. Designed to handle PDF extraction artifacts and locale variations.
#'
#' @param x Character vector to normalize
#' @return Normalized character vector
#' @keywords internal
normalize_text <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(x)
  }

  # Ensure valid UTF-8 encoding first (critical for Perl regex)
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_enc_toutf8(x, validate = TRUE)
  } else {
    # Fallback: try to fix encoding
    if (!all(validUTF8(x))) {
      x <- iconv(x, from = "UTF-8", to = "UTF-8", sub = "?")
      # If that fails, try latin1
      if (any(is.na(x))) {
        x <- iconv(x, from = "latin1", to = "UTF-8", sub = "?")
      }
    }
  }

  # Unicode normalization (simple replacements)
  # --- Minus/dash variants (all to ASCII hyphen-minus) ---
  x <- gsub("\u2212", "-", x)  # Unicode minus sign (U+2212)
  # U+FFFD context-aware recovery: in effect-size context, likely corrupted eta-squared
  # Pattern: ", FFFD = 0.04" or ", FFFD = 0.04, 90% CI" (pdftotext corrupts η² to U+FFFD)
  x <- gsub(",\\s*\uFFFD\\s*=\\s*([-+]?\\d)", ", eta-squared = \\1", x, perl = TRUE)
  x <- gsub("\\b\uFFFD\\s*=\\s*([-+]?\\d)", "eta-squared = \\1", x, perl = TRUE)
  x <- gsub("\uFFFD", "-", x)  # Remaining U+FFFD to dash (genuine minus signs)
  x <- gsub("[\u2013\u2014]", "-", x)  # En dash (U+2013) and em dash (U+2014)
  x <- gsub("[\u2010\u2011\u2012]", "-", x)  # Hyphen (U+2010), non-breaking hyphen (U+2011), figure dash (U+2012)
  x <- gsub("\uFE63", "-", x)  # Small hyphen-minus (U+FE63)
  x <- gsub("\uFF0D", "-", x)  # Fullwidth hyphen-minus (U+FF0D)
  x <- gsub("\u207B", "-", x)  # Superscript minus (U+207B)

  # --- Whitespace variants (all to regular space or removed) ---
  x <- gsub("\u00A0", " ", x)  # Non-breaking space (U+00A0)
  x <- gsub("[\u2000-\u200A\u202F\u205F\u3000]", " ", x)  # Various typographic spaces
  x <- gsub("[\u200B-\u200D\uFEFF]", "", x)  # Zero-width spaces and BOM (remove entirely)

  # --- Quotation marks (all to straight ASCII) ---
  x <- gsub("[\u201C\u201D\u201E\u201F]", "\"", x)  # Curly/low/reversed double quotes
  x <- gsub("[\u00AB\u00BB]", "\"", x)  # Guillemets (angle double quotes)
  x <- gsub("[\u2018\u2019\u201A\u201B]", "'", x)  # Curly/low/reversed single quotes
  x <- gsub("[\u2039\u203A]", "'", x)  # Single angle quotes
  x <- gsub("[\u2032\u00B4]", "'", x)  # Prime (U+2032) and acute accent (U+00B4)
  x <- gsub("\u2033", "\"", x)  # Double prime (U+2033)

  # --- Mathematical comparison operators ---
  x <- gsub("\u2264", "<=", x)  # Less-than-or-equal (U+2264)
  x <- gsub("\u2265", ">=", x)  # Greater-than-or-equal (U+2265)
  x <- gsub("\u2260", "!=", x)  # Not-equal (U+2260)
  x <- gsub("\u2248", "~", x)   # Almost-equal / approximately (U+2248)

  # --- Mathematical operators ---
  x <- gsub("\u00D7", "x", x)   # Multiplication sign (U+00D7) -- for "2x2 ANOVA"
  x <- gsub("\u00B1", "+/-", x)  # Plus-minus sign (U+00B1)
  x <- gsub("\u00B7", ".", x)    # Middle dot (U+00B7) -- decimal separator in some locales

  # --- Superscript digits to caret notation ---
  x <- gsub("\u00B9", "^1", x)  # Superscript 1
  x <- gsub("\u00B3", "^3", x)  # Superscript 3
  x <- gsub("[\u2074]", "^4", x)  # Superscript 4
  x <- gsub("[\u2075]", "^5", x)  # Superscript 5
  x <- gsub("[\u2076]", "^6", x)  # Superscript 6
  x <- gsub("[\u2077]", "^7", x)  # Superscript 7
  x <- gsub("[\u2078]", "^8", x)  # Superscript 8
  x <- gsub("[\u2079]", "^9", x)  # Superscript 9
  x <- gsub("[\u2070]", "^0", x)  # Superscript 0

  # --- Subscript digits (strip -- used in notation like eta2 which we handle separately) ---
  x <- gsub("\u2080", "0", x)  # Subscript 0
  x <- gsub("\u2081", "1", x)  # Subscript 1
  x <- gsub("\u2082", "2", x)  # Subscript 2
  x <- gsub("\u2083", "3", x)  # Subscript 3
  x <- gsub("\u2084", "4", x)  # Subscript 4
  x <- gsub("\u2085", "5", x)  # Subscript 5
  x <- gsub("\u2086", "6", x)  # Subscript 6
  x <- gsub("\u2087", "7", x)  # Subscript 7
  x <- gsub("\u2088", "8", x)  # Subscript 8
  x <- gsub("\u2089", "9", x)  # Subscript 9

  # Line break normalization (CRLF -> LF, then normalize)
  x <- gsub("\r\n", "\n", x)
  x <- gsub("\r", "\n", x)

  # Re-validate UTF-8 after byte operations before Perl regex
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_enc_toutf8(x, validate = TRUE)
  }

  # ============================================================================
  # Fix PDF two-column interleaving artifacts near statistical expressions
  # pdftools::pdf_text() sometimes inserts text from adjacent columns into stat
  # expressions, e.g. "F smaller. (2, 430)" instead of "F(2, 430)"
  # ============================================================================

  # Remove spurious words between F/t and their parenthesized df arguments
  # Pattern: standalone F or t, then alphabetic junk (1-60 chars), then (digit
  # Safe because APA never has "F word. (df1, df2)" -- F is always directly
  # followed by parentheses
  x <- gsub("\\bF\\s+[a-zA-Z][a-zA-Z .',;:-]{0,60}\\(\\s*(\\d)", "F(\\1", x, perl = TRUE)
  x <- gsub("\\bt\\s+[a-zA-Z][a-zA-Z .',;:-]{0,60}\\(\\s*(\\d)", "t(\\1", x, perl = TRUE)

  # ============================================================================
  # PDF-specific stat notation normalization (BEFORE decimal comma conversion)
  # Must run before decimal comma conversion because "F1,200" would be corrupted
  # ============================================================================

  # Fix F-test with square brackets: F[1,30] = 8.33 -> F(1, 30) = 8.33
  # Common in Scientific Reports and some Nature portfolio journals
  x <- gsub("F\\s*\\[\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*\\]",
             "F(\\1, \\2)", x, perl = TRUE)

  # Fix subscript notation: t754 = -33 -> t(754) = -33
  # Common in Royal Society Open Science (RSOS) and other journals
  x <- gsub("(?<![a-zA-Z])t(\\d{2,})\\s*=\\s*([-+]?\\d)", "t(\\1) = \\2", x, perl = TRUE)

  # Fix subscript r: r757 = 0.34 -> r(757) = 0.34
  x <- gsub("(?<![a-zA-Z])r(\\d{2,})\\s*=\\s*([-+]?\\d)", "r(\\1) = \\2", x, perl = TRUE)

  # Fix subscript F: F1,200 = 5.32 -> F(1, 200) = 5.32
  x <- gsub("(?<![a-zA-Z])F(\\d+)\\s*,\\s*(\\d+)\\s*=\\s*(\\d)", "F(\\1, \\2) = \\3", x, perl = TRUE)

  # Fix subscript F with decimal df (GG-corrected): F1.87, 654.3 = 37.32 -> F(1.87, 654.3) = 37.32
  # PDF extraction drops parentheses from repeated-measures ANOVA results
  x <- gsub("(?<![a-zA-Z])F(\\d+\\.\\d+)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*=\\s*([-+]?\\d)",
             "F(\\1, \\2) = \\3", x, perl = TRUE)

  # Fix spaced-df from PDF extraction: t(4 2 1) -> t(421)
  # Iteratively collapse spaces between digits inside t(...) and F(...)
  # Use a loop because "4 2 1" needs two passes: "42 1" -> "421"
  for (i in 1:3) {
    x <- gsub("(t\\s*\\(\\d*)(\\d)\\s+(\\d)", "\\1\\2\\3", x, perl = TRUE)
  }
  # Same for F-test df2: F(1, 2 0 0) -> F(1, 200)
  for (i in 1:3) {
    x <- gsub("(F\\s*\\([0-9]+\\s*,\\s*\\d*)(\\d)\\s+(\\d)", "\\1\\2\\3", x, perl = TRUE)
  }

  # Fix eta2p / etap2 / eta_p^2 / eta_p2 notation -> partial eta-squared = value
  x <- gsub("(?:eta2p|\u03b72p|etap2|\u03b7p2|eta_p2|eta_p\\^2|\u03b7_p2|\u03b7_p\\^2)\\s*=", "partial eta-squared =", x, perl = TRUE)
  # Also handle n2p (PDF corruption of eta2p) but only if followed by = and a number
  x <- gsub("(?<![a-zA-Z])n2p\\s*=\\s*(\\d)", "partial eta-squared = \\1", x, perl = TRUE)
  # v0.3.0a: omega2p / omegap2 notation -> partial omega-squared = value
  x <- gsub("(?:omega2p|\u03c92p|omegap2|\u03c9p2)\\s*=", "partial omega-squared =", x, perl = TRUE)
  # Superscript 2 (U+00B2) to caret notation (e.g., chi squared, eta squared)
  x <- gsub("\u00B2", "^2", x)

  # Greek letter + regular digit 2 (pdftotext -enc UTF-8 output):
  # η2 → eta-squared, ω2 → omega-squared, ε2 → epsilon-squared
  x <- gsub("\u03b7\\s*2\\s*=", "eta-squared =", x, perl = TRUE)
  x <- gsub("\u03c9\\s*2\\s*=", "omega-squared =", x, perl = TRUE)
  x <- gsub("\u03b5\\s*2\\s*=", "epsilon-squared =", x, perl = TRUE)

  # Fix stripped chi-square symbol: PDF extraction sometimes strips chi/X leaving
  # bare " 2 (df) = value" or " 2(df) = value" for chi-squared tests.
  # Only match when preceded by space/paren/start and followed by (digit
  x <- gsub("(^|[\\s(;,])2\\s*\\(\\s*(\\d+)\\s*(?:,\\s*N\\s*=\\s*\\d+)?\\s*\\)\\s*=",
             "\\1chi-square(\\2) =", x, perl = TRUE)

  # ============================================================================
  # Pre-strip thousands-separator commas in sample size contexts
  # Must run BEFORE decimal comma conversion to prevent N = 1,182 -> N = 1.182
  # Safe: sample sizes are always integers, so digit,3-digits in N/n context
  # is unambiguously a thousands separator (not a European decimal comma)
  # Handles: N = 1,182 | n = 1,341 | n1 = 2,500 | N = 12,345,678
  # ============================================================================
  for (.i in 1:3) { # Iterative: handles multi-comma numbers (e.g., 12,345,678)
    x <- gsub("(\\b[Nn]\\d?\\s*=\\s*\\d+),(\\d{3}\\b)", "\\1\\2", x, perl = TRUE)
  }

  # ============================================================================
  # Pre-strip thousands-separator commas inside test-statistic parentheses
  # Must run BEFORE decimal comma conversion to prevent t(2,758) -> t(2.758)
  # which would silently be parsed as Welch df=2.758 with nonsense N estimate.
  # (MetaESCI E8, 2026-04-11: one article dropped 47 rows to this bug.)
  # ============================================================================
  for (.i in 1:3) {
    # t(d,ddd), H(d,ddd), r(d,ddd), Z(d,ddd): single df with thousand separator
    # \s* after comma handles docpluck A4 spacing: "t(2, 758)" as well as "t(2,758)"
    x <- gsub(
      "(\\b[tHrZz]\\s*\\(\\s*\\d{1,3}),\\s*(\\d{3})(?=\\s*\\))",
      "\\1\\2", x, perl = TRUE
    )
    # F(df1, d,ddd) / F[df1, d,ddd]: denominator df with thousand separator
    x <- gsub(
      "(\\bF\\s*[\\(\\[]\\s*\\d+(?:\\.\\d+)?\\s*,\\s*\\d{1,3}),\\s*(\\d{3})(?=\\s*[\\)\\]])",
      "\\1\\2", x, perl = TRUE
    )
    # chi-square(df, N = d,ddd) and variants: N inside chi-square parens
    x <- gsub(
      "((?:chi-?square|\u03c7\\s*\\^?2|\u03c7\u00b2|Chi-?square|chi2|X\\s*\\^?2|X\u00b2)\\s*\\(\\s*\\d{1,3}\\s*,\\s*[Nn]\\s*=\\s*\\d{1,3}),\\s*(\\d{3})(?=\\s*\\))",
      "\\1\\2", x, perl = TRUE
    )
  }

  # ============================================================================
  # Decimal separator normalization (locale-aware) - uses Perl regex
  # ============================================================================
  # Strategy: Convert comma to dot only when it appears between digits
  # Pattern: digit, comma, digit (with optional spaces) -> digit, dot, digit
  # This preserves thousands separators in large numbers

  # Pattern for decimal comma: digit(s), comma, 1-3 digits, then space/punctuation/end
  # But exclude if it looks like thousands (4+ digits before comma).
  #
  # Leading lookbehind guard (?<![a-zA-Z,]) prevents false positives on author
  # affiliations like:
  #   "Braunstein1,3"   -> 'n' before '1' is a letter -> blocked
  #   "Wagner1,3,4"     -> the ',3,4' middle match has ',' before '3' -> blocked
  # Without the letter exclusion, "Braunstein1,3" became "Braunstein1.3".
  # Without the comma exclusion, "Wagner1,3,4" became "Wagner1,3.4" (the middle
  # digit pair was still converted because the preceding char was a comma).
  #
  # Trailing lookahead adds a-zA-Z to the exclusion so "1,3Boryana" doesn't fire.
  x <- gsub("(?<![a-zA-Z,])(\\d{1,3}),([0-9]{1,3})(?=\\s|[^0-9a-zA-Z]|$)",
            "\\1.\\2", x, perl = TRUE)

  # Handle cases where decimal comma might be in effect sizes or CIs
  # Pattern: [-+]?digit+,digit+ (with optional leading sign)
  # This catches "d = 0,45" or "CI [0,12, 0,45]"
  #
  # IMPORTANT: \\d+ (one or more) NOT \\d* here. With \\d* the engine could
  # anchor the match at the comma itself (zero leading digits), which meant the
  # (?<![a-zA-Z]) lookbehind would check the digit BEFORE the comma (not the
  # letter before the digit) and therefore fire on patterns like
  # "Braunstein1,3" where 1 is not a letter.
  #
  # With \\d+ the match is always anchored at a real digit, so the lookbehind
  # applies to the character before that digit, and affiliation patterns are
  # correctly blocked. The comma exclusion also handles "Wagner1,3,4" style
  # 3-affiliation sequences.
  x <- gsub("(?<![a-zA-Z,])([-+]?\\d+),([0-9]+)(?=\\s|,|\\]|\\)|;|$)",
            "\\1.\\2", x, perl = TRUE)

  # CI delimiter harmonization
  # Convert semicolons to commas in CI bounds: (0.12; 0.45) -> (0.12, 0.45)
  x <- gsub("([\\[\\(]\\s*[-+]?\\d*\\.?\\d+)\\s*;\\s*([-+]?\\d*\\.?\\d+\\s*[\\]\\)])", "\\1, \\2", x, perl = TRUE)

  # Normalize bracket styles for CIs (standardize to square brackets with spaces)
  x <- gsub("\\{\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\}", "[\\1, \\2]", x, perl = TRUE)

  # Ensure consistent spacing in CI brackets
  x <- gsub("\\[\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\]", "[\\1, \\2]", x, perl = TRUE)
  x <- gsub("\\(\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\)", "(\\1, \\2)", x, perl = TRUE)

  # Fix period separator in F-test df: F(1.45) -> F(1, 45)
  # Decimal comma conversion above may turn F(1,45) into F(1.45)
  # Must run AFTER decimal comma conversion
  # Safe: only fires when df2 has 2+ digits (distinguishes from fractional GG-corrected df)
  x <- gsub("F\\(\\s*(\\d{1,3})\\.(\\d{2,})\\s*\\)", "F(\\1, \\2)", x, perl = TRUE)

  # Whitespace normalization
  # Collapse multiple spaces to single space (but preserve intentional line breaks)
  # First, normalize tabs to spaces (simple replacement)
  x <- gsub("\t", " ", x)

  # Re-validate UTF-8 before more Perl regex
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_enc_toutf8(x, validate = TRUE)
  }

  # Collapse multiple spaces, but preserve newlines (Perl regex)
  x <- gsub("[ \t]+", " ", x, perl = TRUE)
  # Normalize multiple newlines to single newline (preserve paragraph breaks as single newline)
  x <- gsub("\n{3,}", "\n\n", x, perl = TRUE)
  # Trim leading/trailing whitespace from each line (but keep line structure)
  lines <- strsplit(x, "\n", fixed = TRUE)
  lines <- lapply(lines, function(l) trimws(l, which = "both"))
  x <- vapply(lines, function(l) paste(l, collapse = "\n"), character(1))

  # Fix space between sign and number in stat values: "= - 3.79" -> "= -3.79"
  # Common in PDF extraction where minus sign gets separated from the number
  x <- gsub("=\\s+([-+])\\s+(\\d)", "= \\1\\2", x, perl = TRUE)
  # Also fix in CI bounds: "[- 0.58, - 0.18]" -> "[-0.58, -0.18]"
  x <- gsub("\\[\\s*-\\s+(\\d)", "[-\\1", x, perl = TRUE)
  x <- gsub(",\\s*-\\s+(\\d)", ", -\\1", x, perl = TRUE)

  # Fix missing separator before p-values: "= 2.21p = .035" -> "= 2.21, p = .035"
  x <- gsub("(=\\s*[-+]?\\d+\\.?\\d*)\\s*(p\\s*[<=>])", "\\1, \\2", x, perl = TRUE)

  # Handle common PDF extraction artifacts
  # Fix broken hyphenation (word-\nword -> wordword, but be careful)
  # Fix obvious cases: hyphen at end of line followed by word on next line
  x <- gsub("-\\s*\\n\\s*([a-z])", "\\1", x, perl = TRUE)

  # Strip section numbers at start of lines (e.g., "3.3." or "3.3.1.") to prevent
  # them from being captured as p-values when joined across line breaks
  x <- gsub("(^|\\n)([ \\t]*)\\d+(\\.\\d+)+\\.?[ \\t]+", "\\1\\2", x, perl = TRUE)

  # v0.3.0f: Remove standalone page/section numbers BEFORE line-break joining.
  # Must run here (not later at line ~317) because the joiner below would
  # concatenate "dz =\n3\n" into "dz = 3" before the later stripper runs.
  # Pattern: a line containing only 1-3 digits (optionally with period)
  # preceded and followed by blank lines or text lines.
  x <- gsub("\\n[ \\t]*\\d{1,3}[ \\t]*\\n", "\n", x, perl = TRUE)

  # General line-break joining for statistical expressions (v0.2.5)
  # When a line ends with = < > and the next line starts with a digit or minus,
  # join them. This catches edge cases that the stat-specific patterns below miss.
  # E.g., "F(1, 30) =\n4.425" -> "F(1, 30) = 4.425"
  x <- gsub("([=<>])\\s*\\n\\s*([-+]?[.\\d])", "\\1 \\2", x, perl = TRUE)
  # Join lines where ( is followed by a line break then a digit (df broken at line break)
  # E.g., "F(\n1, 30)" -> "F(1, 30)"
  x <- gsub("\\(\\s*\\n\\s*(\\d)", "(\\1", x, perl = TRUE)

  # Fix line breaks in the middle of statistics
  # Pattern: "p = " or "p<" or "p>" followed by newline and optional text, then a number
  # This fixes cases like "p = \n0.837" or "p = on social distance\n0.837" -> "p = 0.837"
  # Allow up to 50 chars of text between p= and the number (to handle OCR errors)
  # Guard: if there's already a valid p-value right after p=, don't replace
  # v0.3.0d fix: old (?![.0]?\d) failed on "0.001" — [.0]? ate '0', then \d couldn't match '.'
  # Fix uses [ \t]*+ (possessive horizontal whitespace) after [<=>] to prevent two bugs:
  # 1. Backtracking: \s* would backtrack past space, lookahead sees space not digit, fires
  # 2. Newline eating: \s*+ would consume \n, leaving nothing for the \n literal in pattern
  x <- gsub("(p\\s*[<=>][ \\t]*+)(?!\\d|[.]\\d)([^\\n]{0,50})\\n\\s*([.\\d]+)", "\\1\\3", x, perl = TRUE)

  # Fix line breaks between test statistic and p-value
  # Pattern: "t(df) = value,\n p = value" -> "t(df) = value, p = value"
  x <- gsub("([,;])\\s*\\n\\s*(p\\s*[<=>])", "\\1 \\2", x, perl = TRUE)

  # Fix line breaks between effect size and CI
  # Pattern: "d = value,\n 95% CI" -> "d = value, 95% CI"
  x <- gsub("([,;])\\s*\\n\\s*(\\d+%\\s*CI)", "\\1 \\2", x, perl = TRUE)

  # Fix line breaks in effect size assignments
  # Pattern: "f = \n0.01" or "d = \n0.80" -> "f = 0.01" or "d = 0.80"
  # Allow optional text between = and number (up to 30 chars)
  x <- gsub("([a-z]+\\s*=\\s*)[^\\n]{0,30}\\n\\s*([-+]?[.\\d]+)", "\\1\\2", x, perl = TRUE)

  # Fix cases where p-value pattern got broken: "p = text" followed by number on next line
  # More aggressive: look for "p = " followed by non-numeric text, then newline, then number
  x <- gsub("(p\\s*=\\s*)[a-zA-Z][^\\n]*\\n\\s*([.\\d]+)", "\\1\\2", x, perl = TRUE)

  # Fix orphaned p-values: Look for "p = [text]" followed by newline and a number
  # Replace the text with the number: "p = on social distance\n0.837" -> "p = 0.837"
  # This is more aggressive and handles OCR errors where p-value got separated
  x <- gsub("(p\\s*=\\s*)[^\\d\\n]{1,100}\\n\\s*([.\\d]+)(?=\\s*[,;]|\\s*$)", "\\1\\2", x, perl = TRUE)

  # General mid-sentence line-break joining (lowercase to lowercase across newlines)
  # Runs after stat-specific joins so those get priority
  x <- gsub("([a-z,;])\\s*\\n\\s*([a-z])", "\\1 \\2", x, perl = TRUE)

  # ============================================================================
  # Dropped decimal fixes (v0.2.5)
  # PDF extraction sometimes drops the leading "0." from decimal values at page
  # boundaries, producing e.g. "p = 484" instead of "p = .484"
  # ============================================================================

  # Fix "p < 001" -> "p < .001" (missing dot before 001)
  # This is always an artifact — "001" is never a valid p-value representation
  x <- gsub("(p\\s*<\\s*)001\\b", "\\1.001", x, perl = TRUE)

  # Fix "p = NNN" where NNN has 3+ digits -> "p = .NNN"
  # Valid p-values are in [0,1], so any integer >= 100 is always a dropped decimal.
  # Requires trailing whitespace or punctuation to avoid matching mid-number.
  # Flag: adds [decimal_corrected] marker for downstream tracking
  x <- gsub("(p\\s*=\\s*)(\\d{3,})(\\s|,|;|$)", "\\1.\\2\\3 [decimal_corrected]", x, perl = TRUE)

  # Remove standalone page numbers (lines containing only 1-3 digits)
  # These are page numbers from PDF extraction, never meaningful statistical content
  lines_split <- strsplit(x, "\n", fixed = TRUE)
  lines_split <- lapply(lines_split, function(ll) {
    ll[!grepl("^\\s*\\d{1,3}\\s*$", ll)]
  })
  x <- vapply(lines_split, function(ll) paste(ll, collapse = "\n"), character(1))

  # Fix ligature issues (common in PDFs) - simple replacements
  # ff, fi, fl, ffi, ffl ligatures
  x <- gsub("\uFB00", "ff", x, useBytes = TRUE)
  x <- gsub("\uFB01", "fi", x, useBytes = TRUE)
  x <- gsub("\uFB02", "fl", x, useBytes = TRUE)
  x <- gsub("\uFB03", "ffi", x, useBytes = TRUE)
  x <- gsub("\uFB04", "ffl", x, useBytes = TRUE)

  # DOCX table pipe normalization: pandoc outputs tables with | delimiters
  # which can split stats across cells. | has no meaning in APA notation.
  x <- gsub("\\|", " ", x)

  # Final UTF-8 validation before returning (suppress warnings)
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- suppressWarnings({
      stringi::stri_enc_toutf8(x, validate = TRUE)
    })
  }

  x
}

#' Convert string to numeric with warning suppression
#'
#' @param x String or vector
#' @return Numeric value(s)
#' @keywords internal
numify <- function(x) {
  suppressWarnings(as.numeric(x))
}

#' Convert string to integer, stripping thousands-separator commas
#'
#' Used ONLY for sample size values (N, n1, n2) where commas are always
#' thousands separators, never decimal commas.
#'
#' @param x String or vector
#' @return Integer value(s)
#' @keywords internal
numify_int <- function(x) {
  x <- gsub(",", "", x)
  suppressWarnings(as.integer(x))
}

#' Count decimal places in the raw matched string
#'
#' Counts trailing digits after the decimal point in a numeric string,
#' preserving trailing zeros (which numify() loses). Used for APA-precision
#' tracking — "0.0400" returns 4, "0.04" returns 2, "2" returns 0.
#'
#' Must be called on the raw regex match group, before numify().
#'
#' @param x Character (single value) — the raw matched string
#' @return Integer count of decimal places, or NA_integer_ if input is NA/empty
#' @keywords internal
count_decimal_places <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_integer_)
  x <- x[1]
  if (is.na(x) || !nzchar(x)) return(NA_integer_)
  s <- as.character(x)
  s <- sub("^\\s*[+-]?", "", s)
  m <- regmatches(s, regexpr("\\.([0-9]+)", s))
  if (!length(m)) return(0L)
  nchar(m) - 1L
}

#' Extract context window around a sentence
#'
#' Gets n sentences around a given sentence index for design inference.
#'
#' @param chunks Character vector of sentence chunks
#' @param idx Index of current sentence
#' @param window_size Number of sentences before/after to include (default 2)
#' @param extended Logical, return extended context (default FALSE)
#' @return Character vector of context sentences
#' @keywords internal
extract_context <- function(chunks, idx, window_size = 2, extended = FALSE) {
  if (extended) {
    window_size <- 5 # Larger window for N search to catch distant mentions
  }
  start <- max(1, idx - window_size)
  end <- min(length(chunks), idx + window_size)
  context <- chunks[start:end]
  paste(context, collapse = " ")
}

#' Parse APA-style stats and effects from text
#'
#' Extracts test statistics, effect sizes, confidence intervals, and sample sizes
#' from APA-style text. Includes context window extraction for design inference.
#'
#' @param text Character vector of text to parse
#' @param context_window_size Number of sentences before/after to capture (default 2)
#' @return Tibble with parsed elements including context windows
#' @export
#' @examples
#' parsed <- parse_text("t(28) = 2.21, p = .035, d = 0.80")
#' parsed$test_type
#' parsed$stat_value
parse_text <- function(text, context_window_size = 2) {
  if (length(text) == 0 || all(is.na(text))) {
    return(tibble::tibble(
      location = integer(0),
      raw_text = character(0),
      context_window = character(0),
      test_type = character(0),
      df1 = numeric(0),
      df2 = numeric(0),
      stat_value = numeric(0),
      p_reported = numeric(0),
      p_symbol = character(0),
      p_valid = logical(0),
      p_out_of_range = logical(0),
      p_decimal_corrected = logical(0),
      one_tailed_detected = logical(0),
      two_tailed_detected = logical(0),
      method_context_detected = logical(0),
      method_context_in_chunk = logical(0),
      N = numeric(0),
      N_source = character(0),
      N_candidates_str = character(0),
      n1 = numeric(0),
      n2 = numeric(0),
      table_r = numeric(0),
      table_c = numeric(0),
      effect_reported_name = character(0),
      effect_reported = numeric(0),
      effect_reported_decimals = integer(0),
      stat_value_decimals = integer(0),
      effect_fallback = logical(0),
      eta = numeric(0),
      ci_level = numeric(0),
      ci_level_source = character(0),
      ciL_reported = numeric(0),
      ciU_reported = numeric(0),
      ciL_reported_decimals = integer(0),
      ciU_reported_decimals = integer(0),
      z_auxiliary = numeric(0),
      b_coeff = numeric(0),
      SE_coeff = numeric(0),
      adj_R2 = numeric(0),
      df_arity_mismatch = logical(0)
    ))
  }

  # Normalize and split into sentences
  text_normalized <- normalize_text(paste(text, collapse = "\n"))

  # Improved sentence splitting: handle abbreviations, decimals, etc.
  # Split on period/exclamation/question mark followed by space and capital letter or end
  # But not if period is part of number or abbreviation
  chunks <- unlist(strsplit(text_normalized, "(?<=[\\.!?])\\s+(?=[A-Z]|$)", perl = TRUE))
  chunks <- chunks[nchar(trimws(chunks)) > 0]

  # Sub-chunk splitting: when a sentence contains multiple test statistics,
  # split it so each sub-chunk has exactly one test statistic.
  # This prevents str_match() from silently dropping 2nd/3rd/4th matches.
  stat_start_pattern <- paste0(
    "(?:",
    "(?:^|(?<=\\s|,|;|\\(|\\[|\\{))F\\s*[\\(\\[]\\s*\\d",  # F(df1, df2) or F[df1, df2]
    "|(?:^|(?<=\\s|,|;|\\(|\\{))t\\s*\\(\\s*\\d",  # t(df)
    "|(?:^|(?<=\\s|,|;|\\(|\\{))t\\s*=\\s*[-+]?\\.?\\d",  # t = value (bare, for t_nodf; .5 ok)
    "|(?:^|(?<=\\s|,|;|\\(|\\{))r\\s*\\(\\s*\\d",  # r(df)
    "|(?:^|(?<=\\s|,|;|\\(|\\{))(?<![a-zA-Z])r\\s*=\\s*[-+]?\\.?\\d",  # r = value (bare, for r_nodf; .45 ok)
    "|(?:chi-?square|\u03c7\\s*\\^?2|Chi-?square|chi2|X\\s*\\^?2)\\s*[\\(\\[]\\s*\\d",
    "|(?:^|(?<=\\s|,|;|\\(|\\{))H\\s*\\(\\s*\\d",  # H(df)
    "|(?:^|(?<=\\s|,|;|\\(|\\{))(?:Sobel\\s+)?[Zz]\\s*=\\s*[-+]?\\.?\\d",  # z = value, Sobel Z = value; .5 ok
    "|(?:^|(?<=\\s|,|;|\\(|\\{))U\\s*=\\s*\\d",    # U = value
    "|(?:^|(?<=\\s|,|;|\\(|\\{))W\\s*=\\s*\\d",    # W = value
    ")"
  )
  chunks <- unlist(lapply(chunks, function(chunk) {
    # Find positions of all test stat starts
    positions <- gregexpr(stat_start_pattern, chunk, perl = TRUE)[[1]]
    if (length(positions) <= 1 || positions[1] == -1) {
      return(chunk)  # 0 or 1 stat -- keep as-is
    }
    # Filter out z positions that are auxiliary to a U/W test
    # (z co-reported after "U = digits," or "W = digits," within 30 chars)
    keep <- rep(TRUE, length(positions))
    for (j in seq_along(positions)) {
      pos <- positions[j]
      match_char <- substr(chunk, pos, pos)
      if (tolower(match_char) == "z") {
        # Check if U or W appears within 30 chars before this z
        lookback_start <- max(1, pos - 30)
        lookback <- substr(chunk, lookback_start, pos - 1)
        if (grepl("[UW]\\s*=\\s*\\d", lookback, perl = TRUE)) {
          keep[j] <- FALSE  # This z is auxiliary to U/W, don't split here
        }
      }
    }
    positions <- positions[keep]
    if (length(positions) <= 1) return(chunk)

    # Split at positions of 2nd, 3rd, ... stats
    sub_chunks <- character(length(positions))
    for (j in seq_along(positions)) {
      start <- positions[j]
      end <- if (j < length(positions)) positions[j + 1] - 1L else nchar(chunk)
      sub_chunks[j] <- substr(chunk, start, end)
    }
    # Prepend any text before the first stat to the first sub-chunk
    if (positions[1] > 1) {
      sub_chunks[1] <- paste0(substr(chunk, 1, positions[1] - 1L), sub_chunks[1])
    }
    sub_chunks[nchar(trimws(sub_chunks)) > 0]
  }))
  chunks <- chunks[nchar(trimws(chunks)) > 0]

  if (length(chunks) == 0) {
    return(tibble::tibble(
      location = integer(0),
      raw_text = character(0),
      context_window = character(0),
      test_type = character(0),
      df1 = numeric(0),
      df2 = numeric(0),
      stat_value = numeric(0),
      p_reported = numeric(0),
      p_symbol = character(0),
      p_valid = logical(0),
      p_out_of_range = logical(0),
      p_decimal_corrected = logical(0),
      one_tailed_detected = logical(0),
      two_tailed_detected = logical(0),
      method_context_detected = logical(0),
      method_context_in_chunk = logical(0),
      N = numeric(0),
      N_source = character(0),
      N_candidates_str = character(0),
      n1 = numeric(0),
      n2 = numeric(0),
      table_r = numeric(0),
      table_c = numeric(0),
      effect_reported_name = character(0),
      effect_reported = numeric(0),
      effect_reported_decimals = integer(0),
      stat_value_decimals = integer(0),
      effect_fallback = logical(0),
      eta = numeric(0),
      ci_level = numeric(0),
      ci_level_source = character(0),
      ciL_reported = numeric(0),
      ciU_reported = numeric(0),
      ciL_reported_decimals = integer(0),
      ciU_reported_decimals = integer(0),
      z_auxiliary = numeric(0),
      b_coeff = numeric(0),
      SE_coeff = numeric(0),
      adj_R2 = numeric(0),
      df_arity_mismatch = logical(0)
    ))
  }

  # Regex patterns for test statistics (improved to catch more variants)
  # t-test: t(df) = value, t(df)=value (with/without spaces)
  pat_t <- "t\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # t-test without parenthetical df: "t = value, df = value"
  pat_t_nodf <- "\\bt\\s*=\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*df\\s*=\\s*(\\d+(?:\\.\\d+)?)"
  # F-test: F(df1, df2) = value OR F[df1, df2] = value (square brackets for Scientific Reports)
  pat_F <- "F\\s*[\\(\\[]\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*[\\)\\]]\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # z-test: z = value, z=value (with/without spaces)
  # Negative lookbehind excludes dz (Cohen's d paired) from matching
  # Also exclude fMRI coordinates: "x = NN, y = NN, z = NN" pattern
  # Also match "Sobel Z = value" as a named z-test variant
  pat_z <- "(?:(?<![a-zA-Z])z|Sobel\\s+[Zz])\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # Pattern to detect fMRI/MNI coordinate context (used to filter z false positives)
  pat_fmri_coords <- "[xyz]\\s*=\\s*[-+]?\\d+\\s*,\\s*[xyz]\\s*=\\s*[-+]?\\d+\\s*,\\s*[xyz]\\s*=\\s*[-+]?\\d+"
  # Correlation: r(df) = value, r(df)=value
  pat_r <- "r\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # Correlation without df: r = value (requires p-value nearby for validation)
  pat_r_nodf <- "(?<![a-zA-Z])r\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # Chi-square: chi-square(df) = value, \u03c7\u00b2(df) = value, Chi-square(df)=value
  # Also match: \u03c72, chi2, X2, X\u00b2
  # APA format includes optional N inside parens: \u03c7\u00b2(2, N = 150) = 8.73
  pat_chi <- "(?:chi-?square|\u03c7\\s*\\^?2|\u03c7\u00b2|Chi-?square|chi2|X\\s*\\^?2|X\u00b2)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*(?:,\\s*[Nn]\\s*=\\s*([\\d,]+))?\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # Chi-square without parenthesized df: chi2 = 27.04, df = 1 (or chi2(N = 100) = 5.03)
  pat_chi_nodf <- "(?:chi-?square|\u03c7\\s*\\^?2|\u03c7\u00b2|Chi-?square|chi2|X\\s*\\^?2|X\u00b2)\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # v0.3.6: Shadow patterns for df_arity_mismatch detection.
  # These run only when the strict patterns above fail (see dispatch chain
  # below). They capture malformed-arity stats so we can emit a row with
  # df_arity_mismatch = TRUE rather than silently dropping the extraction.
  # See docs/superpowers/specs/2026-05-03-deception-detection-design.md sec 5.
  pat_F_one_df <- "F\\s*[\\(\\[]\\s*(\\d+(?:\\.\\d+)?)\\s*[\\)\\]]\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_t_two_dfs <- "\\bt\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_chi_two_dfs <- "(?:chi-?square|\u03c7\\s*\\^?2|\u03c7\u00b2|Chi-?square|chi2|X\\s*\\^?2|X\u00b2)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(?![Nn]\\s*=)(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_r_two_dfs <- "(?<![a-zA-Z])r\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Nonparametric test patterns
  # Mann-Whitney U: require co-occurrence with p or z to avoid bare "U" false positives
  pat_U <- "U\\s*=\\s*(\\d+(?:\\.\\d+)?)"
  # Wilcoxon W
  pat_W <- "W\\s*=\\s*(\\d+(?:\\.\\d+)?)"
  # Kruskal-Wallis H: H(df) = value

  pat_H <- "H\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # Auxiliary z for nonparametric tests (z co-reported with U/W)
  pat_z_aux <- "(?<![a-zA-Z])z\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Patterns for sample sizes and design info
  # Improved p-value regex: handle optional leading '0', various separators, and spaces
  # Match both lowercase p and uppercase P (Nature, Scientific Reports, medical journals)
  # Also match "p < 0.001" (with leading zero) and "p = .05" (without)
  pat_p <- "\\b[pP]\\s*([<=>]{1,2})\\s*(0?\\.[0-9]+|[01]\\.[0-9]+|[01])"
  # Scientific notation p-values: p < 10^-15, p < 10-12 (PDF strips ^ in exponent)
  pat_p_sci <- "\\b[pP]\\s*([<=>]{1,2})\\s*10\\s*\\^?\\s*[-\u2212](\\d+)"
  # "Not significant" notation: "ns", "n.s.", "NS" (only after comma/semicolon)
  pat_p_ns <- "[,;]\\s*(?:ns\\.?|n\\.s\\.?|NS|N\\.S\\.?)(?=[\\s.,;)]|$)"
  # N regex: restrict to word boundary and look for nearby equals
  # Belt-and-suspenders: also capture comma-thousands in case any slip through normalization
  pat_N  <- "\\bN\\s*=\\s*(\\d[\\d,]*\\d|\\d+)"
  pat_n1 <- "\\bn1\\s*=\\s*(\\d[\\d,]*\\d|\\d+)"
  pat_n2 <- "\\bn2\\s*=\\s*(\\d[\\d,]*\\d|\\d+)"
  pat_dims <- "(\\d+)\\s*[x\u00d7]\\s*(\\d+)"

  # Patterns for effect sizes
  # v0.3.0f: Match both lowercase d and uppercase D (Cohen's D = 0.44)
  pat_d <- "\\b[Dd]\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_g <- "(?:Hedges'?\\s*[Gg]|\\b[Gg]\\b)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_dz <- "\\b[Dd]z\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_dav <- "\\b[Dd]av\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_drm <- "\\b[Dd]rm\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_phi <- "(?:phi|\u03c6)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_V <- "(?:Cramer'?s?\\s*V|\\bV\\b)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # v0.3.0m: Added negative lookbehind to prevent matching "eta" inside "beta"
  pat_eta <- "(?<![a-zA-Z])(?:eta|\u03b7)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # v0.3.0a: Added eta[-]?2, omega[-]?2, partial eta[-]?2, eta p^2 forms
  # Handles plain text (eta2=), caret (eta^2=), Unicode (eta-squared=), superscript (after normalize_text)
  # v0.3.0m: Added negative lookbehind to prevent matching within "beta2", "beta-squared"
  pat_eta2 <- "(?<![a-zA-Z])(?:eta\\s*[-]?squared|eta[-]?2|\u03b7\u00b2|eta\\^2|\u03b7\\^2)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_etap2 <- "(?:partial\\s*eta\\s*[-]?squared|partial\\s*eta[-]?2|partial\\s*\u03b7\u00b2|\u03b7p\u00b2|partial\\s*\u03b7\\^2|\u03b7p\\^2)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # v0.3.0f: Generalized eta-squared — explicit labels + PDF corruption forms
  # Must be checked BEFORE pat_eta2 since "geta-squared" contains "eta-squared"
  pat_gen_eta2 <- paste0(
    "(?:[Gg]eta\\s*[-]?squared|[Gg]eta[-]?2",
    "|generalized\\s*eta\\s*[-]?squared|generalized\\s*eta[-]?2",
    "|generalized\\s*\u03b7\u00b2|\u03b7[Gg]\u00b2|\u03b7[Gg]\\^2",
    "|2G|n2G|\u03b72G|etaG2",
    ")\\s*=\\s*([-+]?\\d*\\.?\\d+)")
  pat_eta2_corrupted <- pat_gen_eta2 # backward compat alias
  pat_omega2 <- "(?:omega\\s*[-]?squared|omega[-]?2|\u03c9\u00b2|omega\\^2|\u03c9\\^2)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_partial_omega2 <- "(?:partial\\s*omega\\s*[-]?squared|partial\\s*omega[-]?2|partial\\s*\u03c9\u00b2|\u03c9p\u00b2|partial\\s*\u03c9\\^2|\u03c9p\\^2)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_epsilon2 <- "(?:epsilon\\s*[-]?squared|epsilon[-]?2|\u03b5\u00b2|epsilon\\^2|\u03b5\\^2|\u03b5[-]?2)\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Cohen's f - explicit labels (always safe)
  pat_cohens_f <- "(?:Cohen'?s?\\s*f|effect\\s*size\\s*f)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # Bare "f = value" in statistical context: preceded by comma/semicolon + optional space
  # Safe because in "p < .001, f = 0.16" the bare f is unambiguously Cohen's f
  pat_bare_f <- "[,;]\\s*f\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Generic/Fallback effect size pattern (Phase 2F - RESTRICTED)
  # Only matches explicit Greek symbols to avoid false positives and PDF corruption char
  # Previous permissive pattern matched any letter, causing false matches with variables
  pat_fallback_es <- "\\b([\u03b5\u03b4\u03c1\u03c4]|[a-z]\uFFFD)\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  pat_beta <- "(?:beta|\u03b2|standardized\\s*beta)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_f2 <- "f\\^?2\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_R2 <- "R\\^?2\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_OR <- "(?:OR|odds\\s*ratio)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_RR <- "(?:RR|risk\\s*ratio)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_IRR <- "(?:IRR|incidence\\s*rate\\s*ratio)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # Cohen's h (effect size for proportion comparisons)
  pat_h <- "(?:Cohen'?s?\\s*h|\\bh\\b)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # v0.3.0f: Cohen's w (chi-square effect size)
  pat_cohens_w <- "(?:Cohen'?s?\\s*w|\\bw\\b)\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Regression coefficient patterns
  pat_b_coeff <- "\\bb\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_SE <- "(?:SE|Std\\.?\\s*Error|standard\\s*error)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_adj_R2 <- "(?:adjusted\\s*R\\^?2|adj\\.?\\s*R\\^?2|R\\^?2\\s*adj)\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Comprehensive CI patterns (Phase 2H - Enhanced with level detection)
  pat_CI1 <- "(\\d+\\.?\\d*)%\\s*(?:CI|C\\.I\\.|confidence\\s*interval)\\s*\\[\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\]"
  pat_CI2 <- "(?:CI|C\\.I\\.|confidence\\s*interval)\\s*(\\d+\\.?\\d*)%\\s*\\[\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\]"
  pat_CI3 <- "\\[\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\]"
  pat_CI4 <- "\\(\\s*([-+]?\\d*\\.?\\d+)\\s*[;,]\\s*([-+]?\\d*\\.?\\d+)\\s*\\)"
  # Pattern for standalone CI level (when stated separately from bounds)
  pat_CI_level <- "(\\d+\\.?\\d*)%\\s*(?:CI|C\\.I\\.|confidence\\s*interval)"
  # Pattern 5: "90% CI [-0.3, 1.2]" (with negative values)
  # (covered by pat_CI1, but ensure it handles negatives)

  # ============================================================================
  # GLOBAL SAMPLE SIZE EXTRACTION (Phase 2C Enhancement)
  # Extract N from entire text as fallback when not found locally
  # ============================================================================
  global_N_matches <- stringr::str_match_all(text_normalized, pat_N)
  global_N <- if (length(global_N_matches[[1]]) > 0) {
    # Extract all N values found in text (strip commas from thousands separators)
    ns <- as.numeric(gsub(",", "", global_N_matches[[1]][, 2]))
    ns <- ns[!is.na(ns) & ns > 0]

    if (length(ns) > 0) {
      # If multiple N values, take the most common (mode)
      n_counts <- table(ns)
      if (max(n_counts) > 1) {
        # Mode exists - most frequently mentioned N
        as.numeric(names(n_counts)[which.max(n_counts)])
      } else {
        # All unique - take the largest (likely total sample)
        max(ns)
      }
    } else {
      NA_real_
    }
  } else {
    NA_real_
  }

  out <- lapply(seq_along(chunks), function(i) {
    s <- chunks[[i]]
    context <- extract_context(chunks, i, context_window_size)

    # Detect and strip [decimal_corrected] marker (v0.2.5)
    # This marker was inserted by normalize_text() when a dropped decimal in
    # a p-value was corrected (e.g., "p = 484" -> "p = .484 [decimal_corrected]")
    p_decimal_corrected <- grepl("\\[decimal_corrected\\]", s, perl = TRUE)
    s <- gsub("\\s*\\[decimal_corrected\\]", "", s, perl = TRUE)

    # Match test statistics
    m_t <- stringr::str_match(s, pat_t)
    m_t_nodf <- stringr::str_match(s, pat_t_nodf)
    m_F <- stringr::str_match(s, pat_F)
    m_z <- stringr::str_match(s, pat_z)
    m_r <- stringr::str_match(s, pat_r)
    m_r_nodf <- stringr::str_match(s, pat_r_nodf)
    m_chi <- stringr::str_match(s, pat_chi)
    m_chi_nodf <- stringr::str_match(s, pat_chi_nodf)
    m_U <- stringr::str_match(s, pat_U)
    m_W_stat <- stringr::str_match(s, pat_W)
    m_H <- stringr::str_match(s, pat_H)

    # Match p-values (try scientific notation first, then standard)
    m_p_sci <- stringr::str_match(s, pat_p_sci)
    m_p <- stringr::str_match(s, pat_p)
    # If scientific notation p matched, convert to standard format for downstream use
    if (!all(is.na(m_p_sci))) {
      sci_exp <- as.integer(m_p_sci[3])
      # Format as plain decimal string (not scientific notation) to survive downstream gsub
      sci_val <- format(10^(-sci_exp), scientific = FALSE)
      # Override m_p with synthesized match: symbol from original, value as decimal
      m_p <- matrix(c(m_p_sci[1], m_p_sci[2], sci_val), nrow = 1)
    }

    # Check for "ns" (not significant) notation when no numeric p-value found
    p_ns_flag <- FALSE
    if (all(is.na(m_p))) {
      p_ns_flag <- grepl(pat_p_ns, s, perl = TRUE)
    }

    # Detect one-tailed test from local chunk only (not context, to prevent bleeding)
    one_tailed_detected <- grepl(
      "\\b(?:one[- ]?tailed|one[- ]?tail|1[- ]?tailed)\\b",
      s, ignore.case = TRUE, perl = TRUE
    )
    # Detect two-tailed test from local chunk
    two_tailed_detected <- grepl(
      "\\b(?:two[- ]?tailed|two[- ]?tail|2[- ]?tailed)\\b",
      s, ignore.case = TRUE, perl = TRUE
    )
    if (two_tailed_detected) one_tailed_detected <- FALSE

    # Detect methodological context (p-curve, equivalence test, etc.)
    # v0.2.4: Separate in-chunk (high confidence) vs nearby (lower confidence) detection
    method_kw <- "\\b(?:p[- ]?curve|equivalence test|TOST|power analysis|simulation|meta-analy|sensitivity analy|bootstrap|applet|sample size calculation|a priori power|post[- ]?hoc power)\\b"
    method_context_in_chunk <- grepl(method_kw, s, ignore.case = TRUE, perl = TRUE)
    method_context_detected <- grepl(method_kw, paste(s, context), ignore.case = TRUE, perl = TRUE)

    # Enhanced N extraction with extended context and global fallback (Phase 2C)
    # Priority: local context > extended context > global
    # Extract ALL N values from local context (not just first) for candidate selection
    m_N_all_local <- stringr::str_match_all(context, pat_N)[[1]]
    N_candidates <- if (nrow(m_N_all_local) > 0) {
      unique(na.omit(sapply(m_N_all_local[, 2], numify_int)))
    } else {
      numeric(0)
    }
    N_value <- if (length(N_candidates) > 0) N_candidates[1] else NA_real_
    N_source <- if (!is.na(N_value)) "local_context" else NA_character_

    # Try extended context if local failed
    if (is.na(N_value)) {
      context_extended <- extract_context(chunks, i, context_window_size, extended = TRUE)
      m_N_all_ext <- stringr::str_match_all(context_extended, pat_N)[[1]]
      ext_candidates <- if (nrow(m_N_all_ext) > 0) {
        unique(na.omit(sapply(m_N_all_ext[, 2], numify_int)))
      } else {
        numeric(0)
      }
      if (length(ext_candidates) > 0) {
        N_value <- ext_candidates[1]
        N_source <- "extended_context"
        # Merge extended candidates with local (for N_candidates_str)
        N_candidates <- unique(c(N_candidates, ext_candidates))
      }
    }

    # Fall back to global N if both failed
    if (is.na(N_value) && !is.na(global_N)) {
      N_value <- global_N
      N_source <- "global_text"
    }

    # If still no N, mark as not found
    if (is.na(N_value)) {
      N_source <- "not_found"
    }

    m_n1 <- stringr::str_match(context, pat_n1)
    m_n2 <- stringr::str_match(context, pat_n2)
    m_dim <- stringr::str_match(s, pat_dims)

    # Match effect sizes
    m_d <- stringr::str_match(s, pat_d)
    m_g <- stringr::str_match(s, pat_g)
    m_dz <- stringr::str_match(s, pat_dz)
    m_dav <- stringr::str_match(s, pat_dav)
    m_drm <- stringr::str_match(s, pat_drm)
    m_phi <- stringr::str_match(s, pat_phi)
    m_V <- stringr::str_match(s, pat_V)
    m_eta <- stringr::str_match(s, pat_eta)
    m_eta2 <- stringr::str_match(s, pat_eta2)
    m_etap2 <- stringr::str_match(s, pat_etap2)
    m_eta2_corrupted <- stringr::str_match(s, pat_eta2_corrupted)
    m_omega2 <- stringr::str_match(s, pat_omega2)
    m_partial_omega2 <- stringr::str_match(s, pat_partial_omega2)
    m_epsilon2 <- stringr::str_match(s, pat_epsilon2)
    m_cohens_f <- stringr::str_match(s, pat_cohens_f)
    m_bare_f <- stringr::str_match(s, pat_bare_f)
    m_beta <- stringr::str_match(s, pat_beta)
    m_f2 <- stringr::str_match(s, pat_f2)
    m_R2 <- stringr::str_match(s, pat_R2)
    m_OR <- stringr::str_match(s, pat_OR)
    m_RR <- stringr::str_match(s, pat_RR)
    m_IRR <- stringr::str_match(s, pat_IRR)
    m_h <- stringr::str_match(s, pat_h)
    m_cohens_w <- stringr::str_match(s, pat_cohens_w)
    m_fallback_es <- stringr::str_match(s, pat_fallback_es)

    # v0.3.6: shadow-pattern matches (only consulted if strict patterns fail)
    m_F_one_df    <- stringr::str_match(s, pat_F_one_df)
    m_t_two_dfs   <- stringr::str_match(s, pat_t_two_dfs)
    m_chi_two_dfs <- stringr::str_match(s, pat_chi_two_dfs)
    m_r_two_dfs   <- stringr::str_match(s, pat_r_two_dfs)

    # Match regression coefficients
    m_b_coeff <- stringr::str_match(s, pat_b_coeff)
    m_SE <- stringr::str_match(s, pat_SE)
    m_adj_R2 <- stringr::str_match(s, pat_adj_R2)

    # Determine test type and extract values
    test_type <- NA_character_
    df1 <- NA_real_
    df2 <- NA_real_
    stat_value <- NA_real_
    stat_value_decimals <- NA_integer_
    chi_inline_N <- NA_real_
    df_arity_mismatch <- FALSE

    if (!all(is.na(m_t))) {
      test_type <- "t"
      df1 <- numify(m_t[2])
      stat_value <- numify(m_t[3])
      stat_value_decimals <- count_decimal_places(m_t[3])
    } else if (!all(is.na(m_t_nodf))) {
      # t = value, df = value (non-standard format, e.g., "one-sample t-test: t = -1.30, df = 42")
      test_type <- "t"
      stat_value <- numify(m_t_nodf[2])
      stat_value_decimals <- count_decimal_places(m_t_nodf[2])
      df1 <- numify(m_t_nodf[3])
    } else if (!all(is.na(m_F))) {
      test_type <- "F"
      df1 <- numify(m_F[2])
      df2 <- numify(m_F[3])
      stat_value <- numify(m_F[4])
      stat_value_decimals <- count_decimal_places(m_F[4])
    } else if (!all(is.na(m_r))) {
      test_type <- "r"
      df1 <- numify(m_r[2])
      stat_value <- numify(m_r[3])
      stat_value_decimals <- count_decimal_places(m_r[3])
    } else if (!all(is.na(m_r_nodf))) {
      # r = value without df (requires p-value nearby to avoid false positives)
      has_p <- !all(is.na(stringr::str_match(s, pat_p)))
      r_val <- numify(m_r_nodf[2])
      # Only accept if: (a) p-value nearby AND (b) |r| <= 1 to avoid matching unrelated "r = ..."
      if (has_p && !is.na(r_val) && abs(r_val) <= 1) {
        test_type <- "r"
        stat_value <- r_val
        stat_value_decimals <- count_decimal_places(m_r_nodf[2])
        # df is NA -- will be flagged as "requires verification" in check.R
      }
    } else if (!all(is.na(m_chi))) {
      test_type <- "chisq"
      df1 <- numify(m_chi[2])
      chi_inline_N <- if (!is.na(m_chi[3])) numify_int(m_chi[3]) else NA_real_
      stat_value <- numify(m_chi[4])
      stat_value_decimals <- count_decimal_places(m_chi[4])
    } else if (!all(is.na(m_chi_nodf))) {
      # Chi-square without parenthesized df (e.g., "chi2 = 27.04, df = 1")
      # Only match if there's also a p-value or df stated nearby to avoid false positives
      has_p <- !all(is.na(stringr::str_match(s, pat_p)))
      has_df_nearby <- grepl("\\bdf\\s*=\\s*\\d+", s) || grepl("\\bdf\\s*=\\s*\\d+", context)
      if (has_p || has_df_nearby) {
        test_type <- "chisq"
        stat_value <- numify(m_chi_nodf[2])
        stat_value_decimals <- count_decimal_places(m_chi_nodf[2])
        # Try to extract df from nearby "df = N" pattern
        m_df_sep <- stringr::str_match(s, "\\bdf\\s*=\\s*(\\d+)")
        if (all(is.na(m_df_sep))) {
          m_df_sep <- stringr::str_match(context, "\\bdf\\s*=\\s*(\\d+)")
        }
        if (!all(is.na(m_df_sep))) {
          df1 <- numify(m_df_sep[2])
        }
      }
    } else if (!all(is.na(m_H))) {
      # Kruskal-Wallis H(df) = value
      test_type <- "H"
      df1 <- numify(m_H[2])
      stat_value <- numify(m_H[3])
      stat_value_decimals <- count_decimal_places(m_H[3])
    } else if (!all(is.na(m_U))) {
      # Mann-Whitney U - require p or z co-occurrence to avoid false positives
      has_p <- !all(is.na(stringr::str_match(s, pat_p)))
      has_z <- !all(is.na(stringr::str_match(s, pat_z_aux)))
      if (has_p || has_z) {
        test_type <- "U"
        stat_value <- numify(m_U[2])
        stat_value_decimals <- count_decimal_places(m_U[2])
      }
    } else if (!all(is.na(m_W_stat))) {
      # Wilcoxon W - require p or z co-occurrence
      has_p <- !all(is.na(stringr::str_match(s, pat_p)))
      has_z <- !all(is.na(stringr::str_match(s, pat_z_aux)))
      if (has_p || has_z) {
        test_type <- "W"
        stat_value <- numify(m_W_stat[2])
        stat_value_decimals <- count_decimal_places(m_W_stat[2])
      }
    }
    # z-test is checked last - if U or W consumed the sentence, z is auxiliary
    if (is.na(test_type) && !all(is.na(m_z))) {
      # Filter out fMRI/MNI coordinate false positives (x = NN, y = NN, z = NN)
      is_fmri <- grepl(pat_fmri_coords, s, perl = TRUE)
      if (!is_fmri) {
        test_type <- "z"
        stat_value <- numify(m_z[2])
        stat_value_decimals <- count_decimal_places(m_z[2])
      }
    }

    # v0.3.6: shadow patterns - fire only when no strict pattern matched.
    # Emit a row with df_arity_mismatch = TRUE so downstream check.R can
    # short-circuit to status=NOTE and the arena adapter can flag suspicion.
    if (is.na(test_type) && !all(is.na(m_F_one_df))) {
      test_type <- "F"
      df1 <- numify(m_F_one_df[2])
      df2 <- NA_real_
      stat_value <- numify(m_F_one_df[3])
      stat_value_decimals <- count_decimal_places(m_F_one_df[3])
      df_arity_mismatch <- TRUE
    } else if (is.na(test_type) && !all(is.na(m_t_two_dfs))) {
      test_type <- "t"
      df1 <- numify(m_t_two_dfs[2])
      df2 <- numify(m_t_two_dfs[3])
      stat_value <- numify(m_t_two_dfs[4])
      stat_value_decimals <- count_decimal_places(m_t_two_dfs[4])
      df_arity_mismatch <- TRUE
    } else if (is.na(test_type) && !all(is.na(m_chi_two_dfs))) {
      test_type <- "chisq"
      df1 <- numify(m_chi_two_dfs[2])
      df2 <- numify(m_chi_two_dfs[3])
      stat_value <- numify(m_chi_two_dfs[4])
      stat_value_decimals <- count_decimal_places(m_chi_two_dfs[4])
      df_arity_mismatch <- TRUE
    } else if (is.na(test_type) && !all(is.na(m_r_two_dfs))) {
      test_type <- "r"
      df1 <- numify(m_r_two_dfs[2])
      df2 <- numify(m_r_two_dfs[3])
      stat_value <- numify(m_r_two_dfs[4])
      stat_value_decimals <- count_decimal_places(m_r_two_dfs[4])
      df_arity_mismatch <- TRUE
    }

    # Extract z_auxiliary for nonparametric tests
    z_auxiliary <- NA_real_
    if (!is.na(test_type) && test_type %in% c("U", "W")) {
      m_z_aux <- stringr::str_match(s, pat_z_aux)
      if (!all(is.na(m_z_aux))) {
        z_auxiliary <- numify(m_z_aux[2])
      }
    }

    # For chi-square, prefer inline N from parentheses over context/global
    if (test_type == "chisq" && !is.na(chi_inline_N)) {
      N_value <- chi_inline_N
      N_source <- "chi_inline"
    }

    # Extract regression coefficients
    b_coeff <- if (!all(is.na(m_b_coeff))) numify(m_b_coeff[2]) else NA_real_
    SE_coeff <- if (!all(is.na(m_SE))) numify(m_SE[2]) else NA_real_
    adj_R2_val <- if (!all(is.na(m_adj_R2))) numify(m_adj_R2[2]) else NA_real_

    # Regression type promotion: if t-test AND b + SE co-occur, set type to "regression"
    if (!is.na(test_type) && test_type == "t" && !is.na(b_coeff) && !is.na(SE_coeff)) {
      test_type <- "regression"
    }

    # Extract effect size (prioritize by specificity)
    effect_name <- NA_character_
    effect_reported <- NA_real_
    effect_reported_decimals <- NA_integer_
    effect_fallback <- FALSE # NEW: Initialize fallback flag (Phase 2F)

    # Check more specific patterns first (prioritize more specific over more general)
    # f^2 must come BEFORE plain f
    if (!all(is.na(m_f2))) {
      effect_name <- "f2"
      effect_reported <- numify(m_f2[2])
      effect_reported_decimals <- count_decimal_places(m_f2[2])
    } else if (!all(is.na(m_etap2))) {
      effect_name <- "etap2"
      effect_reported <- numify(m_etap2[2])
      effect_reported_decimals <- count_decimal_places(m_etap2[2])
    } else if (!all(is.na(m_eta2_corrupted))) {
      # v0.3.0f: Generalized eta-squared (geta-squared, Geta-squared, 2G, etc.)
      # Must be checked BEFORE pat_eta2 since "geta-squared" contains "eta-squared"
      effect_name <- "generalized_eta2"
      effect_reported <- numify(m_eta2_corrupted[2])
      effect_reported_decimals <- count_decimal_places(m_eta2_corrupted[2])
      effect_fallback <- TRUE # Flag as uncertain extraction
    } else if (!all(is.na(m_eta2))) {
      effect_name <- "eta2"
      effect_reported <- numify(m_eta2[2])
      effect_reported_decimals <- count_decimal_places(m_eta2[2])
    } else if (!all(is.na(m_eta))) {
      effect_name <- "eta"
      effect_reported <- numify(m_eta[2])
      effect_reported_decimals <- count_decimal_places(m_eta[2])
    } else if (!all(is.na(m_partial_omega2))) {
      effect_name <- "partial_omega2"
      effect_reported <- numify(m_partial_omega2[2])
      effect_reported_decimals <- count_decimal_places(m_partial_omega2[2])
    } else if (!all(is.na(m_omega2))) {
      effect_name <- "omega2"
      effect_reported <- numify(m_omega2[2])
      effect_reported_decimals <- count_decimal_places(m_omega2[2])
    } else if (!all(is.na(m_epsilon2))) {
      effect_name <- "epsilon_squared"
      effect_reported <- numify(m_epsilon2[2])
      effect_reported_decimals <- count_decimal_places(m_epsilon2[2])
    } else if (!all(is.na(m_cohens_f))) {
      effect_name <- "f"
      effect_reported <- numify(m_cohens_f[2])
      effect_reported_decimals <- count_decimal_places(m_cohens_f[2])
    } else if (!all(is.na(m_bare_f))) {
      # Bare "f = value" after comma — Cohen's f (for F-tests or t-tests reporting f)
      effect_name <- "f"
      effect_reported <- numify(m_bare_f[2])
      effect_reported_decimals <- count_decimal_places(m_bare_f[2])
    } else if (!all(is.na(m_dz))) {
      effect_name <- "dz"
      effect_reported <- numify(m_dz[2])
      effect_reported_decimals <- count_decimal_places(m_dz[2])
    } else if (!all(is.na(m_dav))) {
      effect_name <- "dav"
      effect_reported <- numify(m_dav[2])
      effect_reported_decimals <- count_decimal_places(m_dav[2])
    } else if (!all(is.na(m_drm))) {
      effect_name <- "drm"
      effect_reported <- numify(m_drm[2])
      effect_reported_decimals <- count_decimal_places(m_drm[2])
    } else if (!all(is.na(m_g))) {
      effect_name <- "g"
      effect_reported <- numify(m_g[2])
      effect_reported_decimals <- count_decimal_places(m_g[2])
    } else if (!all(is.na(m_d))) {
      effect_name <- "d"
      effect_reported <- numify(m_d[2])
      effect_reported_decimals <- count_decimal_places(m_d[2])
    } else if (!all(is.na(m_phi))) {
      effect_name <- "phi"
      effect_reported <- numify(m_phi[2])
      effect_reported_decimals <- count_decimal_places(m_phi[2])
    } else if (!all(is.na(m_V))) {
      effect_name <- "V"
      effect_reported <- numify(m_V[2])
      effect_reported_decimals <- count_decimal_places(m_V[2])
    } else if (!all(is.na(m_beta))) {
      effect_name <- "beta"
      effect_reported <- numify(m_beta[2])
      effect_reported_decimals <- count_decimal_places(m_beta[2])
    } else if (!all(is.na(m_R2))) {
      effect_name <- "R2"
      effect_reported <- numify(m_R2[2])
      effect_reported_decimals <- count_decimal_places(m_R2[2])
    } else if (!all(is.na(m_OR))) {
      effect_name <- "OR"
      effect_reported <- numify(m_OR[2])
      effect_reported_decimals <- count_decimal_places(m_OR[2])
    } else if (!all(is.na(m_RR))) {
      effect_name <- "RR"
      effect_reported <- numify(m_RR[2])
      effect_reported_decimals <- count_decimal_places(m_RR[2])
    } else if (!all(is.na(m_IRR))) {
      effect_name <- "IRR"
      effect_reported <- numify(m_IRR[2])
      effect_reported_decimals <- count_decimal_places(m_IRR[2])
    } else if (!all(is.na(m_h))) {
      # Cohen's h - only accept when co-occurring with a chi-square or z-test
      # to avoid false positives from bare "h = X" in other contexts
      if (!is.na(test_type) && test_type %in% c("chisq", "z")) {
        effect_name <- "h"
        effect_reported <- numify(m_h[2])
        effect_reported_decimals <- count_decimal_places(m_h[2])
      }
    } else if (!all(is.na(m_cohens_w))) {
      # v0.3.0f: Cohen's w - only with chi-square or z context
      if (!is.na(test_type) && test_type %in% c("chisq", "z")) {
        effect_name <- "cohens_w"
        effect_reported <- numify(m_cohens_w[2])
        effect_reported_decimals <- count_decimal_places(m_cohens_w[2])
      }
    } else if (!all(is.na(m_fallback_es))) {
      # Fallback match - likely PDF corruption or non-standard notation (Phase 2F)
      sym <- if (length(m_fallback_es) >= 2) m_fallback_es[2] else "ES"

      # Try to identify the symbol
      effect_name <- if (sym == "\u03b5") {
        "epsilon"
      } else if (sym == "\u03b4") {
        "delta"
      } else if (sym == "\u03c1") {
        "rho"
      } else if (sym == "\u03c4") {
        "tau"
      } else if (grepl("\uFFFD", sym)) {
        "unknown_symbol" # PDF corruption replacement character
      } else if (sym %in% c("\u03b7", "\u03B7")) {
        "eta" # Already handled above, but keep for safety
      } else {
        sym # Unknown - use as-is
      }

      effect_reported <- if (length(m_fallback_es) >= 3) numify(m_fallback_es[3]) else NA_real_
      effect_reported_decimals <- if (length(m_fallback_es) >= 3) count_decimal_places(m_fallback_es[3]) else NA_integer_
      effect_fallback <- TRUE # NEW: Flag this as fallback match for uncertainty tracking
    }

    # ========================================================================
    # PARSE-TIME PLAUSIBILITY GUARD (v0.2.4)
    # Reject mathematically impossible or highly implausible effect sizes
    # before they enter the pipeline. This prevents false ERRORs from
    # garbled PDF extractions like R2=52.2, V=173.5, d=8.
    # ========================================================================
    if (!is.na(effect_reported) && !is.na(effect_name)) {
      # Hard-bounded effect sizes: reject values outside mathematical bounds [0, 1]
      bounded_at_one <- c("R2", "r", "phi", "V", "eta2", "etap2", "omega2",
                          "rank_biserial_r", "cliffs_delta", "epsilon_squared", "kendalls_W")
      if (effect_name %in% bounded_at_one && abs(effect_reported) > 1.0) {
        effect_name <- NA_character_
        effect_reported <- NA_real_
        effect_reported_decimals <- NA_integer_
      }

      # v0.3.0f: Extended d-family guard to include dz, dav, drm
      # d > 10: virtually always a line number or page artifact (43 cases
      # in MetaESCI corpus: dz=219, dz=388, etc.)
      d_family <- c("d", "g", "dz", "dav", "drm")
      if (!is.na(effect_reported) && !is.na(effect_name) &&
          effect_name %in% d_family && abs(effect_reported) > 10) {
        effect_name <- NA_character_
        effect_reported <- NA_real_
        effect_reported_decimals <- NA_integer_
      }

      # Round-integer d/g/dz > 2 without decimal point:
      # likely "Study 1", "d = 8 ms", line number, etc.
      # Reject if abs > 5 regardless; if 2 < abs <= 5, reject with context
      if (!is.na(effect_reported) && !is.na(effect_name) &&
          effect_name %in% d_family &&
          effect_reported == floor(effect_reported) &&
          abs(effect_reported) > 2) {
        if (abs(effect_reported) > 5) {
          # d=6, d=8 etc. — virtually never a real effect size
          effect_name <- NA_character_
          effect_reported <- NA_real_
          effect_reported_decimals <- NA_integer_
        } else {
          # d=3, d=4, d=5 — check context for spurious patterns
          reject <- FALSE
          context_lower <- tolower(s)
          spurious <- c("study\\s+\\d", "experiment\\s+\\d",
                        "table\\s+\\d", "figure\\s+\\d",
                        "\\bms\\b", "\\bsec\\b", "\\bmin\\b",
                        "\\bday", "\\bhour", "\\byear",
                        "condition\\s+\\d")
          if (any(sapply(spurious, function(p) {
            grepl(p, context_lower, perl = TRUE)
          }))) {
            reject <- TRUE
          }
          # v0.3.0f: Parse-time t-stat plausibility for round integers.
          # Round-integer d/dz with a t-stat: check if d is plausible.
          # Real d values reported as exact integers are extremely rare
          # (authors write "d = 3.00" not "d = 3"). When d is integer
          # AND > 2x the max plausible d from the t-stat, reject it
          # as a likely page number or extraction artifact.
          if (!reject && !is.na(stat_value) && !is.na(df1) &&
              test_type == "t" && df1 > 0) {
            max_d <- abs(stat_value) * 2 / sqrt(max(df1, 1))
            if (abs(effect_reported) > max(2 * max_d, 2)) {
              reject <- TRUE
            }
          }
          if (reject) {
            effect_name <- NA_character_
            effect_reported <- NA_real_
            effect_reported_decimals <- NA_integer_
          }
        }
      }
    }

    # Validate effect size is appropriate for test type (DEPRECATED: let check.R handle it)
    # Cohen's f, eta2, etap2, omega2 are for F-tests/ANOVA only, not t-tests

    # ========================================================================
    # CI EXTRACTION WITH SOURCE TRACKING (Phase 2H Enhancement)
    # ========================================================================
    ci_level <- NA_real_
    ciL <- NA_real_
    ciU <- NA_real_
    ciL_reported_decimals <- NA_integer_
    ciU_reported_decimals <- NA_integer_
    ci_level_source <- NA_character_

    # Match all patterns
    m_CI1 <- stringr::str_match(s, pat_CI1)
    m_CI2 <- stringr::str_match(s, pat_CI2)
    m_CI3 <- stringr::str_match(s, pat_CI3)
    m_CI4_all <- stringr::str_match_all(s, pat_CI4)[[1]]
    m_CI_level <- stringr::str_match(s, pat_CI_level)

    if (!all(is.na(m_CI1))) {
      # Pattern 1: Level explicitly with bounds
      ci_level <- numify(m_CI1[2]) / 100
      ciL <- numify(m_CI1[3])
      ciU <- numify(m_CI1[4])
      ciL_reported_decimals <- count_decimal_places(m_CI1[3])
      ciU_reported_decimals <- count_decimal_places(m_CI1[4])
      ci_level_source <- "explicit_with_bounds"
    } else if (!all(is.na(m_CI2))) {
      # Pattern 2: Level explicitly with bounds (alternate format)
      ci_level <- numify(m_CI2[2]) / 100
      ciL <- numify(m_CI2[3])
      ciU <- numify(m_CI2[4])
      ciL_reported_decimals <- count_decimal_places(m_CI2[3])
      ciU_reported_decimals <- count_decimal_places(m_CI2[4])
      ci_level_source <- "explicit_with_bounds"
    } else if (!all(is.na(m_CI3))) {
      # Pattern 3: Bounds without level in brackets
      ciL <- numify(m_CI3[2])
      ciU <- numify(m_CI3[3])
      ciL_reported_decimals <- count_decimal_places(m_CI3[2])
      ciU_reported_decimals <- count_decimal_places(m_CI3[3])

      # Look for level stated separately in same sentence
      if (!all(is.na(m_CI_level))) {
        ci_level <- numify(m_CI_level[2]) / 100
        ci_level_source <- "inferred_from_context"
      } else {
        # Default to 95%
        ci_level <- 0.95
        ci_level_source <- "assumed_95"
      }
    } else if (nrow(m_CI4_all) > 0) {
      # Pattern 4: Bounds without level (parentheses)
      # Use str_match_all to find ALL matches, then skip F-test df notation.
      # F(df1, df2) matches CI4 pattern but is NOT a CI — skip that match
      # and use the next one if present (e.g., the actual CI after effect size).
      ci4_found <- FALSE
      for (ci4_row_idx in seq_len(nrow(m_CI4_all))) {
        ci4_val1 <- numify(m_CI4_all[ci4_row_idx, 2])
        ci4_val2 <- numify(m_CI4_all[ci4_row_idx, 3])

        is_f_test_df <- (!is.na(test_type) && test_type == "F" &&
                         !is.na(df1) && !is.na(df2) &&
                         isTRUE(ci4_val1 == df1) && isTRUE(ci4_val2 == df2))

        if (!is_f_test_df) {
          ciL <- ci4_val1
          ciU <- ci4_val2
          ciL_reported_decimals <- count_decimal_places(m_CI4_all[ci4_row_idx, 2])
          ciU_reported_decimals <- count_decimal_places(m_CI4_all[ci4_row_idx, 3])
          ci4_found <- TRUE
          break
        }
      }

      if (ci4_found) {
        # Look for level stated separately
        if (!all(is.na(m_CI_level))) {
          ci_level <- numify(m_CI_level[2]) / 100
          ci_level_source <- "inferred_from_context"
        } else {
          ci_level <- 0.95
          ci_level_source <- "assumed_95"
        }
      }
    }

    # Guard: CI level < 0.50 is implausible (likely parsing artifact)
    if (!is.na(ci_level) && ci_level < 0.50) {
      ci_level_source <- "implausible_level"
      ci_level <- 0.95
    }

    # Only return row if we found a test statistic
    if (is.na(test_type)) {
      return(NULL)
    }

    tibble::tibble(
      location = i,
      raw_text = s,
      context_window = context,
      test_type = test_type,
      df1 = df1,
      df2 = df2,
      stat_value = stat_value,
      # P-VALUE EXTRACTION AND VALIDATION
      # Extract p-value and convert to numeric immediately with validation
      p_reported = {
        p_char <- if (!all(is.na(m_p))) m_p[3] else NA_character_
        if (!is.na(p_char)) {
          # Clean: "0.05" or ".05" both -> 0.05
          p_clean <- gsub("^0?\\.", "0.", p_char)
          p_clean <- gsub("[^0-9.]", "", p_clean)
          val <- suppressWarnings(as.numeric(p_clean))
          # Validate range [0, 1]
          if (!is.na(val) && val >= 0 && val <= 1) val else NA_real_
        } else {
          NA_real_
        }
      },
      p_symbol = if (!all(is.na(m_p))) m_p[2] else NA_character_,
      p_valid = {
        p_char <- if (!all(is.na(m_p))) m_p[3] else NA_character_
        !is.na(p_char) && !is.na(p_reported)
      },
      p_out_of_range = {
        p_char <- if (!all(is.na(m_p))) m_p[3] else NA_character_
        !is.na(p_char) && is.na(p_reported)
      },
      p_decimal_corrected = p_decimal_corrected,
      p_ns = p_ns_flag,
      one_tailed_detected = one_tailed_detected,
      two_tailed_detected = two_tailed_detected,
      method_context_detected = method_context_detected,
      method_context_in_chunk = method_context_in_chunk,
      N = N_value, # From enhanced extraction above
      N_source = N_source, # NEW: Track where N came from
      N_candidates_str = if (length(N_candidates) > 1) paste(N_candidates, collapse = ";") else NA_character_,
      n1 = if (!all(is.na(m_n1))) numify_int(m_n1[2]) else NA_real_,
      n2 = if (!all(is.na(m_n2))) numify_int(m_n2[2]) else NA_real_,
      table_r = if (!all(is.na(m_dim))) numify(m_dim[2]) else NA_real_,
      table_c = if (!all(is.na(m_dim))) numify(m_dim[3]) else NA_real_,
      effect_reported_name = effect_name,
      effect_reported = effect_reported,
      effect_reported_decimals = effect_reported_decimals, # v0.3.5 (MetaESCI 2A)
      stat_value_decimals = stat_value_decimals,           # v0.3.5 (MetaESCI 2A)
      effect_fallback = effect_fallback, # NEW: Phase 2F - flag fallback pattern use
      eta = if (length(effect_name) > 0 && !is.na(effect_name) && effect_name == "eta") effect_reported else NA_real_,
      ci_level = ci_level,
      ci_level_source = ci_level_source, # NEW: Phase 2H - Track where CI level came from
      ciL_reported = ciL,
      ciU_reported = ciU,
      ciL_reported_decimals = ciL_reported_decimals,       # v0.3.5 (MetaESCI 2A)
      ciU_reported_decimals = ciU_reported_decimals,       # v0.3.5 (MetaESCI 2A)
      z_auxiliary = z_auxiliary,
      b_coeff = b_coeff,
      SE_coeff = SE_coeff,
      adj_R2 = adj_R2_val,
      df_arity_mismatch = df_arity_mismatch
    )
  })

  # Filter out NULLs and rows without test statistics
  out <- out[!vapply(out, is.null, logical(1))]
  if (length(out) == 0) {
    return(tibble::tibble(
      location = integer(0),
      raw_text = character(0),
      context_window = character(0),
      test_type = character(0),
      df1 = numeric(0),
      df2 = numeric(0),
      stat_value = numeric(0),
      p_reported = numeric(0),
      p_symbol = character(0),
      p_valid = logical(0),
      p_out_of_range = logical(0),
      p_decimal_corrected = logical(0),
      one_tailed_detected = logical(0),
      two_tailed_detected = logical(0),
      method_context_detected = logical(0),
      method_context_in_chunk = logical(0),
      N = numeric(0),
      N_source = character(0),
      N_candidates_str = character(0),
      n1 = numeric(0),
      n2 = numeric(0),
      table_r = numeric(0),
      table_c = numeric(0),
      effect_reported_name = character(0),
      effect_reported = numeric(0),
      effect_reported_decimals = integer(0),
      stat_value_decimals = integer(0),
      effect_fallback = logical(0),
      eta = numeric(0),
      ci_level = numeric(0),
      ci_level_source = character(0),
      ciL_reported = numeric(0),
      ciU_reported = numeric(0),
      ciL_reported_decimals = integer(0),
      ciU_reported_decimals = integer(0),
      z_auxiliary = numeric(0),
      b_coeff = numeric(0),
      SE_coeff = numeric(0),
      adj_R2 = numeric(0),
      df_arity_mismatch = logical(0)
    ))
  }

  dplyr::bind_rows(out) %>%
    dplyr::filter(!is.na(test_type))
}
