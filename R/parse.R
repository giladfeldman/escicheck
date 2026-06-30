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

  # --- Equals-sign corruption (U+00BC fraction-one-quarter standing in for "=") ---
  # v0.6.8: some PDFs encode the "=" glyph such that the text layer emits U+00BC
  # ("\u00BC", the fraction one-quarter). Whole papers come through with EVERY
  # equals sign as U+00BC and no real "=" at all (10.1177/1948550619900570: 120
  # U+00BC, zero "="), so `t \u00BC -7.81`, `F (3, 1791) \u00BC 200.12`, `d \u00BC
  # 0.57`, `M \u00BC 20.20` parse to nothing. Fold U+00BC to "=" ONLY in a
  # statistical-operator position -- flanked by whitespace and adjacent to a value
  # (a number / sign / bracket) or a stat-token-like word -- so a genuine
  # one-quarter fraction in prose ("\u00BC cup of sugar") is NOT rewritten. This is
  # the same class of character-level normalisation as the U+2212 minus and the
  # U+FFFD eta-squared recovery above. (Also filed to docpluck to fold upstream.)
  # U+00BC written as \u00BC to keep R *code* ASCII-only (R CMD check requirement).
  x <- gsub("([A-Za-z0-9)%\\]])\\s*\u00BC\\s*([-+]?[.\\d\\[]|conf|not|extrem)",
            "\\1 = \\2", x, perl = TRUE)

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
      df_arity_mismatch = logical(0),
      arm1_events = numeric(0),
      arm1_total  = numeric(0),
      arm2_events = numeric(0),
      arm2_total  = numeric(0)
    ))
  }

  # Normalize and split into sentences
  text_normalized <- normalize_text(paste(text, collapse = "\n"))

  # Improved sentence splitting: handle abbreviations, decimals, etc.
  # Split on period/exclamation/question mark followed by space and capital letter or end
  # But not if period is part of number or abbreviation
  chunks <- unlist(strsplit(text_normalized, "(?<=[\\.!?])\\s+(?=[A-Z]|$)", perl = TRUE))
  chunks <- chunks[nchar(trimws(chunks)) > 0]

  # v0.5.2: an optional subscript label (gof / Pearson / Yates / LR / MH / Wald)
  # glued or underscore-joined to the chi token -- JASP-style "chi2gof(2)" /
  # "chi2_Pearson(1)". Shared by the sub-chunk splitter below and by pat_chi /
  # pat_chi_nodf / pat_chi_two_dfs so every chi path recognises the subscript.
  chi_sub <- "(?:[\\s_]*(?:[Gg][Oo][Ff]|[Pp]earson|[Yy]ates|[Ll][Rr]|[Mm][Hh]|[Ww]ald))?"

  # v0.5.9: the chi-square token alternation, hoisted to one shared definition.
  # It was duplicated across the sub-chunk splitter and pat_chi / pat_chi_nodf /
  # pat_chi_two_dfs, and the copies had drifted: the splitter copy lacked the
  # precomposed superscript forms, and the word form "chi" lacked the optional
  # caret that the symbol forms (chi^2, X^2) already allowed -- so "chi^2(1)"
  # parsed as a chi-square only via the symbol forms, never the word form.
  # Every chi path now uses chi_tok so the accepted forms can never drift again.
  # The Greek chi and the superscript-two are written as \u escapes so the R
  # source stays pure ASCII (R CMD check warns on non-ASCII in code).
  chi_tok <- "(?:chi-?square|Chi-?square|chi\\s*\\^?2|\u03c7\\s*\\^?2|\u03c7\u00b2|X\\s*\\^?2|X\u00b2)"

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
    "|", chi_tok, chi_sub, "\\s*[\\(\\[]\\s*\\d",  # chi-square(df), shared chi_tok
    "|(?:^|(?<=\\s|,|;|\\(|\\{))H\\s*\\(\\s*\\d",  # H(df)
    "|(?:^|(?<=\\s|,|;|\\(|\\{))(?:Sobel\\s+)?[Zz]\\s*=\\s*[-+]?\\.?\\d",  # z = value, Sobel Z = value; .5 ok
    "|(?:^|(?<=\\s|,|;|\\(|\\{))U\\s*=\\s*\\d",    # U = value
    "|(?:^|(?<=\\s|,|;|\\(|\\{))W\\s*=\\s*[-+]?\\.?\\d",    # W = value (DSCF W may be negative)
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

    # v0.6.5: keep a standardized-coefficient clause that IMMEDIATELY PRECEDES a
    # test statistic together with that statistic. In regression reporting
    # "(beta = 0.74, t(260) = 11.32, p < .001)" the beta belongs to the t that
    # FOLLOWS it; splitting at the t strands the beta at the end of the previous
    # sub-chunk, so the t adopts the NEXT clause's beta (cog_emo: t(260) = 11.32
    # wrongly took beta = 0.91 instead of 0.74). For each split start (after the
    # first) whose preceding text ends in a "(beta|standardized beta = <num>, "
    # clause, back the split up to the start of that clause so the beta stays with
    # its t. Scoped to beta / standardized-beta (the pat_beta vocabulary): a beta
    # PRECEDES its t, whereas Cohen's d FOLLOWS its r, so the r-d adoption path
    # (test-v030f-parser-fixes) is unaffected.
    # Match the WORD "beta" (the pat_beta vocabulary's common form); the rare
    # Greek-symbol "B = X, t(...)" keep-together edge is left to existing
    # behaviour to keep this source pure ASCII (R CMD check non-ASCII rule).
    precede_beta_pat <- "\\(?\\s*(?:standardized\\s*)?beta\\s*=\\s*[-+]?\\d*\\.?\\d+\\s*,\\s*$"
    for (j in seq_along(positions)) {
      if (j == 1L) next
      prev_span <- substr(chunk, positions[j - 1L], positions[j] - 1L)
      mbeta <- regexpr(precede_beta_pat, prev_span, perl = TRUE)
      if (mbeta[1] > 0) {
        new_start <- positions[j - 1L] + mbeta[1] - 1L
        if (new_start > positions[j - 1L]) positions[j] <- new_start
      }
    }

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
      df_arity_mismatch = logical(0),
      arm1_events = numeric(0),
      arm1_total  = numeric(0),
      arm2_events = numeric(0),
      arm2_total  = numeric(0)
    ))
  }

  # Regex patterns for test statistics (improved to catch more variants)
  # t-test: t(df) = value, t(df)=value (with/without spaces)
  pat_t <- "t\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # t-test without parenthetical df: "t = value, df = value"
  pat_t_nodf <- "\\bt\\s*=\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*df\\s*=\\s*(\\d+(?:\\.\\d+)?)"
  # v0.6.1: bare "t = value, ..., p [op] value" with NO df anywhere.
  # Common in compact tables / inline reports where df lives in a header / sample
  # description but not in the immediate sentence. Accepts t followed (within
  # ~80 chars) by a p-clause. Word-boundary lookbehind avoids matching `dt =`,
  # `pt = `, etc. The trailing p-anchor distinguishes a genuine t-test report
  # from any unrelated "t = value" (e.g. a time variable). df1 stays NA and the
  # downstream NA-N guard at check.R:1390 yields status=NOTE (extracted but not
  # exactly verifiable without df).
  pat_t_p_nodf <- "(?<![a-zA-Z])t\\s*=\\s*([-+]?\\d*\\.?\\d+)(?=[^a-zA-Z]{1,80}?[pP]\\s*[<=>])"
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
  # chi_sub (defined above, shared with the sub-chunk splitter) allows a
  # JASP-style subscript label glued to the chi token.
  pat_chi <- paste0(chi_tok, chi_sub, "\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*(?:,\\s*[Nn]\\s*=\\s*([\\d,]+))?\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)")
  # Chi-square without parenthesized df: chi2 = 27.04, df = 1 (or chi2(N = 100) = 5.03)
  pat_chi_nodf <- paste0(chi_tok, chi_sub, "\\s*=\\s*([-+]?\\d*\\.?\\d+)")

  # v0.3.6: Shadow patterns for df_arity_mismatch detection.
  # These run only when the strict patterns above fail (see dispatch chain
  # below). They capture malformed-arity stats so we can emit a row with
  # df_arity_mismatch = TRUE rather than silently dropping the extraction.
  # See docs/superpowers/specs/2026-05-03-deception-detection-design.md sec 5.
  pat_F_one_df <- "F\\s*[\\(\\[]\\s*(\\d+(?:\\.\\d+)?)\\s*[\\)\\]]\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_t_two_dfs <- "\\bt\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_chi_two_dfs <- paste0(chi_tok, chi_sub, "\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(?![Nn]\\s*=)(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)")
  pat_r_two_dfs <- "(?<![a-zA-Z])r\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Rank-correlation patterns (Stage 1 / P2): Spearman's rho and Kendall's tau
  # in the symbol-with-df form. A plain r(df) in a Spearman/Kendall context is
  # reclassified separately in the test-type block below.
  pat_rho <- "(?:\\brho|\u03c1|\\br_?s\\b)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  pat_tau <- "(?:\\btau|\u03c4)\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Nonparametric test patterns
  # Mann-Whitney U: require co-occurrence with p or z to avoid bare "U" false positives
  pat_U <- "U\\s*=\\s*(\\d+(?:\\.\\d+)?)"
  # W: shared by Wilcoxon's W, Kendall's W, and the DSCF post-hoc W. The DSCF
  # statistic can be negative, so an optional leading sign is allowed; the W
  # block below disambiguates the three.
  pat_W <- "W\\s*=\\s*([-+]?\\d+(?:\\.\\d+)?)"
  # Kruskal-Wallis H: H(df) = value

  pat_H <- "H\\s*\\(\\s*(\\d+(?:\\.\\d+)?)\\s*\\)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # v0.5.15: Cochran Q heterogeneity test for meta-analysis. Forms in the wild:
  #   "Q_T [40] = 104.65"  (Bartos / RoBMA convention; T = total, df in brackets)
  #   "Q_T(40) = 104.65"   (parenthesized df variant)
  #   "Q[40] = 104.65" / "Q(40) = 104.65"  (bare Q)
  #   "Q_M[k] = ..." / "Q_B[k] = ..." / "Q_W[k] = ..."  (model / between / within
  #   subscripts -- treated identically here, all chi-square distributed under
  #   homogeneity null with the reported df). The "_<letter>" subscript is
  #   optional; the df may be in [brackets] or (parens) with optional spaces.
  # v0.6.3 (E5): the subscript underscore is ALSO optional -- PDF text extraction
  #   (docpluck) flattens the "Q_T" subscript to a glued "QT" (e.g.
  #   "QT [40] = 104.65" in collabra.90203), so accept "Q", "QT", and "Q_T".
  pat_cochran_q <- "\\bQ(?:_?[A-Za-z])?\\s*[\\[(]\\s*(\\d+(?:\\.\\d+)?)\\s*[\\])]\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  # v0.6.2: exact binomial test with Cohen's h effect size.
  # Form: "(exact )?binomial p [op] <pval>[, ]Cohen('s)? h = <h>[, 95% CI [<lo>, <hi>]]"
  # The two anchors -- "binomial p" and "Cohen('s)? h" -- are matched together
  # in one regex (within ~80 non-period chars) so an unrelated bare "h = X"
  # elsewhere in the verbatim cannot mismatch. Cohen's h is the verifiable
  # quantity (the binomial p-value can be recomputed only when n/N are
  # recoverable from the same verbatim -- handled via pat_n_out_of_N below;
  # otherwise status routes to NOTE per the established NOTE-only template).
  # Found in CRSP decoy-effect papers (Xiao/Zeng/Feldman 2021 et al), 2-5
  # rows in the current harness; the analogous template will scale to any
  # paper using exact-binomial-test reporting with Cohen's h.
  pat_binom_h <- paste0(
    "(?:exact\\s+)?binomial\\s+p\\s*([<=>]{1,2})\\s*",
    "([01]?\\.\\d+|[01])",
    "[^.]{0,80}?",
    "Cohen'?s?\\s*h\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  )
  # v0.6.5: bare binomial test reported WITHOUT Cohen's h, e.g.
  #   "...preferred the scarf (63%) to the coat (37%; binomial: p = .002)"
  #   "...most participants (59%) ... binomial test: p = .047"
  # Anchor: "binomial[ test][:] p [op] X". Used ONLY when pat_binom_h did not
  # match (no Cohen's h). Neither an effect size nor in-sentence counts are
  # available, so the row routes to an extraction-only NOTE -- closes a
  # PARSE-MISS without fabricating a verification (collabra.77859 Study 1 & 4).
  pat_binom_bare <- paste0(
    "\\bbinomial(?:\\s+test)?\\s*:?\\s*p\\s*([<=>]{1,2})\\s*",
    "([01]?\\.\\d+|[01])"
  )
  # Helper for the binomial branch: "<n> out of <N> (participants|cases|...)"
  # form, used to recover n_total when present in the same sentence.
  pat_n_out_of_N <- "\\b(\\d+)\\s+out\\s+of\\s+(\\d+)\\b"
  # v0.6.8 (E-interaction-p): a bare "p-value for interaction <op>? <pval>" report
  # -- a subgroup / moderation interaction test reported with ONLY a p and no F /
  # test statistic and no effect size. The interaction F lives in a supplementary
  # table not in the main PDF, so nothing is independently recomputable; the p is
  # surfaced as an extraction-only NOTE. Accepts the operator-less form common in
  # medical reporting ("p-value for interaction 0.029") and the "p_interaction" /
  # "interaction p[-value]" variants. PLOS Medicine 10.1371/journal.pmed.1004323
  # PROSECCO trial: "significant subgroup effects ... for parity (p-value for
  # interaction 0.029; Table B in S1 Text)".
  pat_interaction_p <- paste0(
    "(?:p[- ]?value\\s+for\\s+interaction|",
    "p[- ]?interaction|",
    "interaction\\s+p(?:[- ]?value)?)\\s*",
    "([<=>]{0,2})\\s*([01]?\\.\\d+|[01])"
  )
  # v0.5.16: clinical-trial risk ratio with two-proportion slash counts.
  # Form: "<n1>/<N1> (<pct1>%) versus|and|vs <n2>/<N2> (<pct2>%) ... RR <val>;
  # 95% CI <lo> to <hi>; p[-]?(value)? = <pval>". The two-proportion clause
  # and the RR clause are matched separately and conjoined in the dispatch.
  # Found in PLOS Medicine 10.1371/journal.pmed.1004323 PROSECCO trial
  # (4-5 results in body text); previously returned 0 stats because RR was
  # not a recognised test_type and the slash-count form is non-APA.
  # v0.6.0: relaxed to allow up to ~30 characters of non-comma/non-semicolon
  # text between the closing %) and the separator word, so clinical-trial
  # sentences with intervening descriptors -- "8/106 (7.5%) women under PSA
  # and 5/101 (5.0%) women under GA" -- still capture the per-arm cells.
  # Limited to [^,;] to prevent the match from crossing into a different
  # clause; clinical-trial sentences universally keep arm descriptors
  # comma/semicolon-free.
  # v0.6.3 (E1): a per-arm cell may carry a short alphabetic descriptor
  # BETWEEN the slash-count and the percent -- "86/98 women (87.8%)" in the
  # PROSECCO primary-outcome sentence previously bound no cells because the
  # word "women" sat between "86/98" and "(87.8%)". The descriptor is
  # letters/spaces only (no digits) and bounded, so it cannot swallow an
  # adjacent fraction. The single-source `prop_cell` token is shared by both
  # arm groups so the two halves never drift apart (a duplicated-alternation
  # bug class documented in LESSONS.md). Capture groups stay (e1, N1) then
  # (e2, N2): m_two_props[2..5] downstream is unchanged.
  prop_cell <- "(\\d+)\\s*/\\s*(\\d+)\\s*(?:[A-Za-z][A-Za-z ]{0,24}\\s*)?\\(\\s*\\d+(?:\\.\\d+)?\\s*%\\s*\\)"
  pat_two_props_slash <- paste0(
    prop_cell,
    "\\s*[^,;]{0,40}?\\s*(?:versus|vs\\.?|and|compared\\s+to)\\s*[^,;]{0,40}?\\s*",
    prop_cell)
  pat_RR_ci_p <- "\\bRR\\s*=?\\s*([-+]?\\d*\\.?\\d+)\\s*[;,]?\\s*95\\s*%\\s*CI\\s*([-+]?\\d*\\.?\\d+)\\s*(?:to|-)\\s*([-+]?\\d*\\.?\\d+)\\s*[;,]?\\s*[pP][- ]?(?:value)?\\s*([<=>]{0,2})\\s*([01]?\\.\\d+|[01])"
  # v0.5.17: risk-difference percent with CI (clinical trial, Farrington-
  # Manning noninferiority). Form: "risk difference <val>%; 95% [confidence
  # interval (CI)|CI] <lo> to <hi>; ... p[-value]? = <pval>". The p-clause
  # may be in the same clause (";", ",") or further along ("; noninferiority,
  # P = 0.09"). Found in PLOS Medicine 10.1371/journal.pmed.1004323
  # PROSECCO trial; previously returned 0 stats because risk-difference
  # percent was not a recognised test_type.
  pat_risk_diff <- "risk[- ]?difference\\s+(?:was\\s+|of\\s+)?([-+]?\\d+(?:\\.\\d+)?)\\s*%?\\s*[;,]?\\s*95\\s*%\\s*(?:confidence\\s*interval\\s*\\(CI\\)|CI)\\s*([-+]?\\d+(?:\\.\\d+)?)\\s*(?:to|-)\\s*([-+]?\\d+(?:\\.\\d+)?)"
  # v0.5.18: median-difference (Hodges-Lehmann) with IQR. Form: "median
  # difference <val>; 95% CI <lo> to <hi>; p[-value]? = <pval>". Often
  # preceded by per-arm "<med> (<iqr_lo> to <iqr_hi>) versus <med> (...)".
  # Found in PLOS Medicine 10.1371/journal.pmed.1004323; previously
  # returned 0 stats because median-difference was not a recognised
  # test_type.
  pat_median_diff <- "median\\s+difference\\s+(?:was\\s+|of\\s+)?([-+]?\\d+(?:\\.\\d+)?)\\s*[;,]?\\s*95\\s*%\\s*(?:confidence\\s*interval\\s*\\(CI\\)|CI)\\s*([-+]?\\d+(?:\\.\\d+)?)\\s*(?:to|-)\\s*([-+]?\\d+(?:\\.\\d+)?)(?:[^a-zA-Z]*?[pP][- ]?(?:value)?\\s*([<=>]{0,2})\\s*([01]?\\.\\d+|[01]))?"
  # Auxiliary z for nonparametric tests (z co-reported with U/W)
  pat_z_aux <- "(?<![a-zA-Z])z\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Patterns for sample sizes and design info
  # Improved p-value regex: handle optional leading '0', various separators, and spaces
  # Match both lowercase p and uppercase P (Nature, Scientific Reports, medical journals)
  # Also match "p < 0.001" (with leading zero) and "p = .05" (without)
  pat_p <- "\\b[pP]\\s*([<=>]{1,2})\\s*(0?\\.[0-9]+|[01]\\.[0-9]+|[01])"
  # Scientific notation p-values: p < 10^-15, p < 10-12 (PDF strips ^ in exponent)
  pat_p_sci <- "\\b[pP]\\s*([<=>]{1,2})\\s*10\\s*\\^?\\s*[-\u2212](\\d+)"
  # v0.5.3: scientific E-notation p-values -- p = 2.572e-08, p = 1.2e-3 (the
  # form R / JASP / Python emit). pat_p rejects these (the mantissa is not a
  # [01].x number) and pat_p_sci only handles the "10^-N" form. The whole
  # mantissa+exponent number is captured, then converted to a decimal string.
  # (normalize_text has already folded Unicode minus U+2212 to ASCII '-', so an
  # ASCII hyphen in the exponent suffices -- same assumption pat_p relies on.)
  pat_p_enote <- "\\b[pP]\\s*([<=>]{1,2})\\s*(\\d+(?:\\.\\d+)?[eE]\\s*-\\s*\\d+)"
  # "Not significant" notation: "ns", "n.s.", "NS" (only after comma/semicolon)
  pat_p_ns <- "[,;]\\s*(?:ns\\.?|n\\.s\\.?|NS|N\\.S\\.?)(?=[\\s.,;)]|$)"
  # N regex: restrict to word boundary and look for nearby equals
  # Belt-and-suspenders: also capture comma-thousands in case any slip through normalization
  # v0.5.5: also accept "nobs" (the JASP "number of observations" token = total
  # N). Bare lowercase "n =" is intentionally NOT matched -- it is commonly a
  # per-group size, and matching it would mis-read a group n as the total N.
  pat_N  <- "\\b(?:N|nobs)\\s*=\\s*(\\d[\\d,]*\\d|\\d+)"
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
  # v0.5.12: added eta^2p / eta^2_p forms (subscript-p AFTER the squared) \u2014 the
  # Collabra / APA convention in the Identifiable-Victim, Experiential-vs-Material,
  # and Less-Is-Better replications all write `\u03b7^2p = .008` with the `p`
  # trailing the caret-2 (eta-squared-partial). Previously only `\u03b7p^2` was
  # recognized; 13+ rows across two papers dropped their reported point estimate
  # despite the CI being captured. Caught by the 2026-05-23 escicheck-iterate
  # validation against the AI stats gold.
  pat_etap2 <- "(?:partial\\s*eta\\s*[-]?squared|partial\\s*eta[-]?2|partial\\s*\u03b7\u00b2|\u03b7p\u00b2|partial\\s*\u03b7\\^2|\u03b7p\\^2|\u03b7\\^2p|\u03b7\\^2_p|eta\\^2p|eta\\^2_p)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
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
  # The labeled forms (CI1/CI2) accept an optional ":" or "=" between the CI
  # label and the bracket -- "95% CI: [..]" / "95% CI = [..]" are common APA
  # variants, and the colon form previously fell through to the bare-bracket
  # pat_CI3 (v0.6.3 E3: a "95%CI: [.21, .56]" body CI lost its labeled status
  # and the first bare bracket in the sub-chunk -- a flattened table cell --
  # won instead).
  pat_CI1 <- "(\\d+\\.?\\d*)%\\s*(?:CI|C\\.I\\.|confidence\\s*interval)\\s*[:=]?\\s*\\[\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\]"
  pat_CI2 <- "(?:CI|C\\.I\\.|confidence\\s*interval)\\s*(\\d+\\.?\\d*)%\\s*[:=]?\\s*\\[\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\]"
  pat_CI3 <- "\\[\\s*([-+]?\\d*\\.?\\d+)\\s*,\\s*([-+]?\\d*\\.?\\d+)\\s*\\]"
  pat_CI4 <- "\\(\\s*([-+]?\\d*\\.?\\d+)\\s*[;,]\\s*([-+]?\\d*\\.?\\d+)\\s*\\)"
  # Pattern for standalone CI level (when stated separately from bounds)
  pat_CI_level <- "(\\d+\\.?\\d*)%\\s*(?:CI|C\\.I\\.|confidence\\s*interval)"
  # Pattern 5: "90% CI [-0.3, 1.2]" (with negative values)
  # (covered by pat_CI1, but ensure it handles negatives)

  # v0.6.3 (E3/E4): choose, among several CI matches in one sub-chunk, the one
  # bound to THIS row's effect size rather than the first bracket in the chunk.
  # A docpluck-flattened table interleaved between body sentences, or an
  # adjacent effect clause, can place a foreign bracketed CI earlier in the
  # sub-chunk than the row's own; binding the first match silently adopts the
  # neighbour's CI (E3: a Table-4 cell [.50, 1.02]; E4: an abstract d-clause
  # [0.25, 0.54] preceding the r). Given the character positions of every CI
  # match and an anchor (the effect-size value position), prefer the match
  # at/after the anchor nearest to it; fall back to the nearest one before it,
  # and to the first match when no anchor is known -- identical to the
  # pre-0.6.3 first-match behaviour for the common single-CI sub-chunk.
  pick_ci_idx <- function(positions, anchor) {
    n <- length(positions)
    if (n <= 1L) return(1L)
    if (is.na(anchor) || anchor < 1L) return(1L)
    after <- which(positions >= anchor)
    if (length(after) > 0L) {
      return(after[which.min(positions[after] - anchor)])
    }
    which.min(anchor - positions)
  }

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

  # v0.6.8 (E-A1): section-scoped one-sample carry-forward map.
  # A "one-sample t-test against {the midpoint|scale midpoint|chance|N}"
  # declaration scopes ALL of the t-tests that follow it within its study/section
  # -- but those tests can sit many sentences later, past an interleaved table the
  # PDF flattened between body paragraphs, so the per-row +-2-sentence context
  # window does NOT reach the declaration (collabra.57785 Study 3C: the
  # declaration is ~12 chunks before its t(742) stats, separated by a foreign
  # Study-3A-1/5 design table). The v0.6.5 detector only caught Study 3A-2 because
  # ITS declaration was in its own window.
  #
  # We precompute, per chunk index, whether a one-sample declaration is "in scope"
  # there: scan BACKWARD up to a bounded number of chunks for the nearest one-
  # sample declaration, and STOP (do not carry) if a PROSE contradicting design
  # declaration ("we ran/conducted/performed a[n] paired / independent / Welch /
  # two-sample / between- / within-subjects t-test") intervenes first. A
  # bracketed "[Analysis: ...]" table annotation or a table-structure line is NOT
  # a prose declaration and does NOT block the carry -- those are table metadata
  # for OTHER conditions, interleaved by docpluck, not the analysis statement for
  # the tests in flow. The carry only ADDS a one-sample hint; check.R still
  # requires the row to be a t-test, and an explicit Welch/independent signal in
  # the row's OWN clause or a paired effect family still wins downstream.
  pat_one_sample_decl <- paste0(
    "one[- ]?sample\\s+t[- ]?test.{0,60}?",
    "(?:against|than|from|compared\\s+to|relative\\s+to|vs\\.?|versus)\\s+",
    "(?:the\\s+)?(?:scale\\s+)?(?:mid[- ]?point|chance|midpoint)"
  )
  # A declaration that EXPLICITLY scopes MULTIPLE following tests ("... for each
  # of the sub-questions / items / conditions / measures ..."). Only such a
  # declaration may carry FAR (past an interleaved table); a plain one-sample
  # declaration carries only to the few tests immediately after it. This is the
  # discriminator that keeps the carry from bleeding onto a DIFFERENT analysis
  # reported many sentences later (collabra.57785 Study 3C "for each of the
  # sub-questions in Study 3C" carries far and is correct; rsos.250908's plain
  # "manipulation check (one-sample t-test against midpoint 0)" must NOT reach the
  # paired condition-comparison t-tests 15 chunks away that the gold marks
  # dependent/paired).
  pat_one_sample_multiscope <- paste0(
    "for\\s+each\\s+(?:of\\s+the\\s+)?",
    "(?:sub.?question|item|condition|measure|vignette|scenario|",
    "dependent\\s+variable|dv|outcome|sub.?scale|domain)"
  )
  # A PROSE analysis declaration naming a NON-one-sample t-test design. Anchored
  # on "we (ran|conducted|...) a[n] <design> t-test" so a bare keyword inside a
  # table annotation or an unrelated sentence does not block the carry.
  pat_prose_design_block <- paste0(
    "\\bwe\\s+(?:(?:also|then|next|further|subsequently|additionally|first|",
    "therefore|thus|again)\\s+){0,2}",
    "(?:ran|conducted|performed|used|computed|carried\\s+out)\\b",
    "[^.]{0,80}?\\b(?:paired|independent|two[- ]sample|between[- ]subjects?|",
    "within[- ]subjects?|welch)\\b[^.]{0,30}?t[- ]?test"
  )
  # Two-tier carry distance: a plain one-sample declaration reaches only the next
  # few chunks (handles a declaration immediately followed by its test, e.g.
  # collabra.57785 Study 3B, decl->stat 4 chunks); a multi-scope ("for each ...")
  # declaration reaches far (handles Study 3C across the interleaved Study-3A
  # design table, decl->stat 12 chunks).
  one_sample_short_window <- 4L
  one_sample_long_window <- 18L
  onesample_in_scope <- logical(length(chunks))
  if (length(chunks) > 0) {
    decl_chunk <- grepl(pat_one_sample_decl, chunks, ignore.case = TRUE, perl = TRUE)
    multiscope_chunk <- decl_chunk &
      grepl(pat_one_sample_multiscope, chunks, ignore.case = TRUE, perl = TRUE)
    block_chunk <- grepl(pat_prose_design_block, chunks, ignore.case = TRUE, perl = TRUE)
    for (ci in seq_along(chunks)) {
      lo <- max(1L, ci - one_sample_long_window)
      # Walk backward from ci-1 to lo; the first declaration found wins, a prose
      # block encountered first cancels the carry. A plain declaration counts only
      # within the SHORT window; a multi-scope declaration counts out to the LONG
      # window.
      j <- ci
      hit <- FALSE
      while (j > lo) {
        j <- j - 1L
        if (isTRUE(block_chunk[j])) break
        if (isTRUE(decl_chunk[j])) {
          dist <- ci - j
          if (isTRUE(multiscope_chunk[j]) || dist <= one_sample_short_window) {
            hit <- TRUE
          }
          # Either way the nearest declaration is the deciding one -- stop here so
          # a far PLAIN declaration does not get overruled by an even-farther
          # multi-scope one (and vice versa); the nearest wins.
          break
        }
      }
      # A chunk that is itself a declaration is trivially in scope.
      onesample_in_scope[ci] <- hit || isTRUE(decl_chunk[ci])
    }
  }

  out <- lapply(seq_along(chunks), function(i) {
    s <- chunks[[i]]
    context <- extract_context(chunks, i, context_window_size)
    # v0.6.8 (E-A1): if a one-sample declaration is in scope for this chunk but the
    # +-2-sentence context window did not capture it, append an explicit
    # one-sample hint so the check.R t-test design detector classifies it
    # one-sample. (No-op for non-t rows; check.R reads it only for t-tests.)
    if (isTRUE(onesample_in_scope[i]) &&
        !grepl("one[- ]?sample\\s+t", context, ignore.case = TRUE, perl = TRUE)) {
      context <- paste0(context, " [Analysis: one-sample t-test against the scale midpoint.]")
    }

    # Detect and strip [decimal_corrected] marker (v0.2.5)
    # This marker was inserted by normalize_text() when a dropped decimal in
    # a p-value was corrected (e.g., "p = 484" -> "p = .484 [decimal_corrected]")
    p_decimal_corrected <- grepl("\\[decimal_corrected\\]", s, perl = TRUE)
    s <- gsub("\\s*\\[decimal_corrected\\]", "", s, perl = TRUE)

    # Match test statistics
    m_t <- stringr::str_match(s, pat_t)
    m_t_nodf <- stringr::str_match(s, pat_t_nodf)
    m_t_p_nodf <- stringr::str_match(s, pat_t_p_nodf)
    m_F <- stringr::str_match(s, pat_F)
    m_z <- stringr::str_match(s, pat_z)
    m_r <- stringr::str_match(s, pat_r)
    m_r_nodf <- stringr::str_match(s, pat_r_nodf)
    m_rho <- stringr::str_match(s, pat_rho)
    m_tau <- stringr::str_match(s, pat_tau)
    m_chi <- stringr::str_match(s, pat_chi)
    m_chi_nodf <- stringr::str_match(s, pat_chi_nodf)
    m_U <- stringr::str_match(s, pat_U)
    m_W_stat <- stringr::str_match(s, pat_W)
    m_H <- stringr::str_match(s, pat_H)
    m_cochran_q <- stringr::str_match(s, pat_cochran_q)
    m_binom_h   <- stringr::str_match(s, pat_binom_h)
    m_binom_bare <- stringr::str_match(s, pat_binom_bare)
    m_interaction_p <- stringr::str_match(s, pat_interaction_p)
    m_n_outN    <- stringr::str_match(s, pat_n_out_of_N)
    m_RR_ci_p <- stringr::str_match(s, pat_RR_ci_p)
    m_two_props <- stringr::str_match(s, pat_two_props_slash)
    m_risk_diff <- stringr::str_match(s, pat_risk_diff)
    m_median_diff <- stringr::str_match(s, pat_median_diff)

    # Match p-values (try scientific notation first, then standard)
    m_p_sci <- stringr::str_match(s, pat_p_sci)
    m_p_enote <- stringr::str_match(s, pat_p_enote)
    m_p <- stringr::str_match(s, pat_p)
    # If scientific notation p matched, convert to standard format for downstream use
    if (!all(is.na(m_p_sci))) {
      sci_exp <- as.integer(m_p_sci[3])
      # Format as plain decimal string (not scientific notation) to survive downstream gsub
      sci_val <- format(10^(-sci_exp), scientific = FALSE)
      # Override m_p with synthesized match: symbol from original, value as decimal
      m_p <- matrix(c(m_p_sci[1], m_p_sci[2], sci_val), nrow = 1)
    }
    # v0.5.3: E-notation p (p = 2.572e-08) -- pat_p cannot match it. Synthesize
    # m_p with the value as a plain-decimal string; this takes precedence over
    # any partial pat_p match of the bare mantissa.
    if (!all(is.na(m_p_enote))) {
      enote_num <- suppressWarnings(as.numeric(gsub("\\s", "", m_p_enote[3])))
      if (!is.na(enote_num) && enote_num >= 0 && enote_num <= 1) {
        m_p <- matrix(c(m_p_enote[1], m_p_enote[2],
                        format(enote_num, scientific = FALSE)), nrow = 1)
      }
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

    # v0.5.13: suppress global-N attribution when the chunk is a Bayesian
    # model-averaged effect (RoBMA, Bayesian model-averaging, posterior model
    # average). Such estimates have no recoverable per-study sample size; the
    # earlier code fell through to global_N and pulled an unrelated paper's N
    # onto the model-averaged r, producing rows like `r = 0.002, df1 = 1002,
    # N = 1004` (the 1004 came from a much later sentence about a frequentist
    # r(1002) regression). Caught by the 2026-05-24 escicheck-iterate cycle 4
    # validation against the Collabra Identifiable-Victim stats gold (finding
    # F-002, R04).
    bayesian_model_ctx <- grepl(
      "\\b(?:RoBMA|Bayesian\\s+model[- ]?averag|model[- ]?averaged|posterior\\s+model\\s+average|PMA)\\b",
      paste(s, context), ignore.case = TRUE, perl = TRUE
    )

    # Fall back to global N if both failed
    if (is.na(N_value) && !is.na(global_N) && !bayesian_model_ctx) {
      N_value <- global_N
      N_source <- "global_text"
    } else if (is.na(N_value) && bayesian_model_ctx) {
      N_source <- "bayesian_model_no_n"
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

    # Rank-correlation context (Stage 1 / P2): an r(df) reported in a Spearman
    # or Kendall context must be routed to the rank path, not the Pearson path.
    # The reclassification cue must be NEAR THE STATISTIC -- the immediate
    # sub-chunk `s` -- NOT the wider context window. A bare r(df) defaults to
    # Pearson; only an "A Spearman correlation was computed, r(20) = 0.50"-style
    # cue in the same clause flips it. Using the wide context window here caused
    # a body-text Pearson r(261) in cog_emo (Chan & Feldman 2024) to be
    # mislabeled "spearman" purely because a DISTANT table note read "Format:
    # Pearson's correlations [CI] (Spearman's rho)" -- the note describes a
    # parenthetical alternative column, not this r. (The Gap-4 Spearman-CI
    # offer still consults the wider context separately; only the test_type
    # RELABEL is restricted to the near cue.)
    rank_ctx <- tolower(s)
    is_kendall_ctx <- isTRUE(grepl("kendall", rank_ctx, fixed = TRUE))
    is_spearman_ctx <- isTRUE(grepl("spearman", rank_ctx, fixed = TRUE)) ||
      isTRUE(grepl("rank[ -]order correlation|rank correlation", rank_ctx))

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
    } else if (!all(is.na(m_t_p_nodf))) {
      # v0.6.1: bare "t = value" near a p-clause, NO df. Compact table /
      # inline-report form. df1 stays NA; check.R routes to NOTE because the
      # exact p-check needs df. Less strict than pat_t_nodf — the p-clause
      # anchor in the regex is what disambiguates a real t-test from any
      # unrelated "t = value". See parse.R::pat_t_p_nodf for the rationale.
      test_type <- "t"
      stat_value <- numify(m_t_p_nodf[2])
      stat_value_decimals <- count_decimal_places(m_t_p_nodf[2])
      # df1 remains NA — downstream Phase 5/6/7 paths handle NA-df by either
      # using a back-computed bound from N (when N is recoverable) or returning
      # status=NOTE.
    } else if (!all(is.na(m_F))) {
      test_type <- "F"
      df1 <- numify(m_F[2])
      df2 <- numify(m_F[3])
      stat_value <- numify(m_F[4])
      stat_value_decimals <- count_decimal_places(m_F[4])
    } else if (!all(is.na(m_rho)) ||
               (!all(is.na(m_r)) && is_spearman_ctx && !is_kendall_ctx)) {
      # Spearman's rho (Stage 1 / P2): symbol form, or an r(df) in a Spearman
      # context. Group 2 = df, group 3 = coefficient (same arity as pat_r).
      test_type <- "spearman"
      m_rank <- if (!all(is.na(m_rho))) m_rho else m_r
      df1 <- numify(m_rank[2])
      stat_value <- numify(m_rank[3])
      stat_value_decimals <- count_decimal_places(m_rank[3])
    } else if (!all(is.na(m_tau)) ||
               (!all(is.na(m_r)) && is_kendall_ctx)) {
      # Kendall's tau (Stage 1 / P2): symbol form, or an r(df) in a Kendall context.
      test_type <- "kendall"
      m_rank <- if (!all(is.na(m_tau))) m_tau else m_r
      df1 <- numify(m_rank[2])
      stat_value <- numify(m_rank[3])
      stat_value_decimals <- count_decimal_places(m_rank[3])
    } else if (!all(is.na(m_r))) {
      test_type <- "r"
      df1 <- numify(m_r[2])
      stat_value <- numify(m_r[3])
      stat_value_decimals <- count_decimal_places(m_r[3])
    } else if (!all(is.na(m_r_nodf))) {
      # r = value without df. Extract when corroborated by a nearby p-value OR
      # a confidence interval -- a bare r with neither is too ambiguous (it
      # could be any ratio labelled r). A CI is as strong a signal as a p that
      # this is a genuine reported correlation, mirroring the chi_nodf
      # ("p OR df") and U ("p OR z") guards. v0.5.10.
      has_p <- !all(is.na(stringr::str_match(s, pat_p)))
      r_val <- numify(m_r_nodf[2])
      # has_ci: an explicitly-labelled CI (pat_CI1 / pat_CI2) always counts; a
      # bare bracketed pair (pat_CI3) counts only when its bounds bracket the r
      # value -- the straddle check disambiguates a real CI from an unrelated
      # bracketed pair (a page range, a citation index, etc.).
      has_ci <- !all(is.na(stringr::str_match(s, pat_CI1))) ||
                !all(is.na(stringr::str_match(s, pat_CI2)))
      if (!has_ci && !is.na(r_val)) {
        m_ci3 <- stringr::str_match(s, pat_CI3)
        if (!all(is.na(m_ci3))) {
          ci_lo <- numify(m_ci3[2])
          ci_hi <- numify(m_ci3[3])
          if (!is.na(ci_lo) && !is.na(ci_hi) &&
              r_val >= min(ci_lo, ci_hi) - 1e-6 &&
              r_val <= max(ci_lo, ci_hi) + 1e-6) {
            has_ci <- TRUE
          }
        }
      }
      # Only accept if: (a) a p-value OR a CI nearby AND (b) |r| <= 1, to avoid
      # matching an unrelated "r = ...".
      if ((has_p || has_ci) && !is.na(r_val) && abs(r_val) <= 1) {
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
    } else if (!all(is.na(m_RR_ci_p))) {
      # v0.5.16: clinical-trial risk ratio with two-proportion slash counts.
      # v0.6.0: when the two-proportion clause IS in the same chunk, capture
      # the per-arm event/total cells so check.R can compute RR independently.
      test_type <- "RR"
      stat_value <- numify(m_RR_ci_p[2])
      stat_value_decimals <- count_decimal_places(m_RR_ci_p[2])
      # df not meaningful for RR (2x2 table); leave NA.
      # Synthesize m_p from the RR clause when "p-value 0.44" form (no '='
      # operator) was missed by pat_p.
      if (all(is.na(m_p))) {
        p_op_rr <- if (!is.na(m_RR_ci_p[5]) && nchar(m_RR_ci_p[5]) > 0) m_RR_ci_p[5] else "="
        p_val_rr <- m_RR_ci_p[6]
        m_p <- matrix(c(paste0("p", p_op_rr, p_val_rr), p_op_rr, p_val_rr), nrow = 1)
      }
    } else if (!all(is.na(m_median_diff))) {
      # v0.5.18: median-difference (Hodges-Lehmann) with IQR + CI.
      test_type <- "md_hl"
      stat_value <- numify(m_median_diff[2])
      stat_value_decimals <- count_decimal_places(m_median_diff[2])
      # Same operator-optional p synthesis as RR -- captures "p-value 0.027".
      if (all(is.na(m_p)) && !is.na(m_median_diff[6])) {
        p_op_md <- if (!is.na(m_median_diff[5]) && nchar(m_median_diff[5]) > 0) m_median_diff[5] else "="
        p_val_md <- m_median_diff[6]
        m_p <- matrix(c(paste0("p", p_op_md, p_val_md), p_op_md, p_val_md), nrow = 1)
      }
    } else if (!all(is.na(m_risk_diff))) {
      # v0.5.17: risk-difference percent with CI (clinical-trial, Farrington-
      # Manning noninferiority). p-value may be in the same clause or later
      # in the sentence; if pat_p didn't match anywhere in the chunk, leave
      # p_reported NA -- check.R routes to a NOTE either way.
      test_type <- "rdpct"
      stat_value <- numify(m_risk_diff[2])
      stat_value_decimals <- count_decimal_places(m_risk_diff[2])
    } else if (!all(is.na(m_cochran_q))) {
      # v0.5.15: Cochran Q heterogeneity (meta-analysis). Q is chi-square
      # distributed under the homogeneity null with the bracketed/parenthesized
      # df. No standard effect size; I-squared is a heterogeneity proportion
      # reported separately (verified in check.R if extractable).
      test_type <- "cochran_q"
      df1 <- numify(m_cochran_q[2])
      stat_value <- numify(m_cochran_q[3])
      stat_value_decimals <- count_decimal_places(m_cochran_q[3])
    } else if (!all(is.na(m_binom_h))) {
      # v0.6.2: exact binomial test reported with Cohen's h. The h is the
      # verifiable effect size; the p-value may be re-checked against
      # binom.test() when "<n> out of <N>" is recoverable (handled in check.R).
      # NOTE-only template -- no compute branch in this parse layer.
      test_type <- "binomial"
      stat_value <- numify(m_binom_h[4])
      stat_value_decimals <- count_decimal_places(m_binom_h[4])
      # df not meaningful for an exact binomial test (no df concept); leave NA.
      # Synthesize m_p from the binomial clause when the in-clause "binomial p"
      # is the ONLY p-value in the verbatim (pat_p may also catch it; this is
      # belt-and-suspenders).
      if (all(is.na(m_p)) && !is.na(m_binom_h[3])) {
        p_op_b  <- if (!is.na(m_binom_h[2]) && nchar(m_binom_h[2]) > 0) m_binom_h[2] else "="
        p_val_b <- m_binom_h[3]
        m_p <- matrix(c(paste0("p", p_op_b, p_val_b), p_op_b, p_val_b), nrow = 1)
      }
    } else if (!all(is.na(m_binom_bare)) && all(is.na(m_h))) {
      # v0.6.5: bare binomial ("binomial[ test]: p [op] X") with no Cohen's h
      # and no in-sentence counts. Surfaces the p-value but nothing is
      # independently recomputable -> extraction-only NOTE (check.R's binomial
      # branch handles the no-h case). collabra.77859 Study 1 gift-preference
      # (p = .002) and Study 4 willingness-to-pay-more (p = .047). Reached only
      # when no t/F/r/chi/z/binomial-with-h matched (dispatched after pat_binom_h).
      # Guard `all(is.na(m_h))`: if a Cohen's h co-occurs in the chunk but
      # pat_binom_h did not bind it (wrong order / >80 chars apart), the case is
      # ambiguous -- do NOT extract a bare binomial that would let the generic
      # h-adoption bind an unrelated h (preserves the v0.6.2 80-char-lookahead).
      test_type <- "binomial"
      # No test statistic and no Cohen's h for a bare binomial; both stay NA.
      if (all(is.na(m_p)) && !is.na(m_binom_bare[3])) {
        p_op_b  <- if (!is.na(m_binom_bare[2]) && nchar(m_binom_bare[2]) > 0) m_binom_bare[2] else "="
        p_val_b <- m_binom_bare[3]
        m_p <- matrix(c(paste0("p", p_op_b, p_val_b), p_op_b, p_val_b), nrow = 1)
      }
    } else if (!all(is.na(m_interaction_p))) {
      # v0.6.8 (E-interaction-p): a bare "p-value for interaction <op>? <pval>"
      # subgroup / moderation interaction test reported with ONLY a p -- no F, no
      # effect size (the interaction F is in a supplement, not the main PDF).
      # Reached only when no t/F/r/chi/z/binomial matched, so an interaction whose
      # F IS reported ("F(2,998)=1.48 ... interaction") binds the F instead. The
      # p is surfaced; nothing is independently recomputable -> extraction-only
      # NOTE (check.R routes test_type "interaction_p" to NOTE). PLOS Medicine
      # PROSECCO trial: "(p-value for interaction 0.029; Table B in S1 Text)".
      test_type <- "interaction_p"
      # No test statistic, no df, no effect size for a bare interaction p.
      if (all(is.na(m_p)) && !is.na(m_interaction_p[3])) {
        p_op_i  <- if (!is.na(m_interaction_p[2]) && nchar(m_interaction_p[2]) > 0) m_interaction_p[2] else "="
        p_val_i <- m_interaction_p[3]
        m_p <- matrix(c(paste0("p", p_op_i, p_val_i), p_op_i, p_val_i), nrow = 1)
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
      # The bare "W =" token is shared by three different statistics:
      #   - Wilcoxon's W : a rank-sum, always >= 0
      #   - Kendall's W  : the coefficient of concordance, bounded 0-1
      #   - DSCF's W     : the Dwass-Steel-Critchlow-Fligner post-hoc statistic
      #                    for Kruskal-Wallis pairwise comparisons; can be < 0.
      # Disambiguate before classifying. A W in [0, 1] in a "Kendall" /
      # "concordance" context is Kendall's W. A negative W cannot be Wilcoxon's
      # (a rank-sum) nor Kendall's (bounded 0-1), so it is DSCF; an explicit
      # DSCF / Dwass / Kruskal-pairwise context confirms a positive DSCF W.
      # DSCF is routed (in check.R) to an honest "cannot verify" NOTE -- no
      # standard effect size is recoverable from the W alone.
      w_val <- numify(m_W_stat[2])
      w_ctx <- tolower(paste(s, context))
      is_kendall_W <- !is.na(w_val) && w_val >= 0 && w_val <= 1 &&
        grepl("kendall|concordance", w_ctx) && !grepl("wilcoxon", w_ctx)
      is_dscf <- !is_kendall_W && !is.na(w_val) && (
        w_val < 0 ||
          grepl("dscf|dwass", w_ctx) ||
          (grepl("kruskal", w_ctx) && grepl("pairwise|post[ -]?hoc", w_ctx)))
      if (is_kendall_W) {
        test_type <- "kendall_w"
        stat_value <- w_val
        stat_value_decimals <- count_decimal_places(m_W_stat[2])
      } else {
        # Wilcoxon's W or DSCF's W -- both require a p or z co-occurrence to
        # avoid a bare "W =" false positive.
        has_p <- !all(is.na(stringr::str_match(s, pat_p)))
        has_z <- !all(is.na(stringr::str_match(s, pat_z_aux)))
        if (has_p || has_z) {
          test_type <- if (is_dscf) "dscf" else "W"
          stat_value <- w_val
          stat_value_decimals <- count_decimal_places(m_W_stat[2])
        }
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

    # v0.6.2: For binomial tests with a "<n> out of <N>" clause in the same
    # sentence, prefer N from that clause (the binomial trial count) over
    # context/global. check.R can then verify the reported p against
    # binom.test(n, N, p_null) when n_total + effect_reported (Cohen's h)
    # are both present.
    if (!is.na(test_type) && test_type == "binomial" && !all(is.na(m_n_outN))) {
      N_value <- numify_int(m_n_outN[3])
      N_source <- "binom_n_out_of_N"
    }

    # v0.5.8 (T3 residual): chi-square-scoped bare-n fallback. A bare lowercase
    # "n = X" is deliberately NOT in pat_N -- it is commonly a per-group size.
    # But for a chi-square with no N from any other source, a single bare "n ="
    # reported alongside it is the total sample size (e.g. the JASP goodness-of-
    # fit line "chi2gof(1) = 31.01, p = ..., n = 329"). Fire only when:
    # test_type is chisq, N is still NA, the chunk carries no per-group token
    # (n1/n2), and EXACTLY ONE "n =" appears in the chunk -- two or more "n ="
    # are per-group counts, not a total, so the fallback must not fire.
    if (!is.na(test_type) && test_type == "chisq" && is.na(N_value)) {
      n_bare_all <- stringr::str_match_all(s, "\\bn\\s*=\\s*(\\d[\\d,]*\\d|\\d+)")[[1]]
      has_group_n <- !all(is.na(stringr::str_match(s, pat_n1))) ||
        !all(is.na(stringr::str_match(s, pat_n2)))
      if (nrow(n_bare_all) == 1 && !has_group_n) {
        N_value <- numify_int(n_bare_all[1, 2])
        N_source <- "chi_bare_n"
      }
    }

    # Extract regression coefficients
    b_coeff <- if (!all(is.na(m_b_coeff))) numify(m_b_coeff[2]) else NA_real_
    SE_coeff <- if (!all(is.na(m_SE))) numify(m_SE[2]) else NA_real_
    adj_R2_val <- if (!all(is.na(m_adj_R2))) numify(m_adj_R2[2]) else NA_real_

    # Regression type promotion: if t-test AND b + SE co-occur, set type to "regression"
    if (!is.na(test_type) && test_type == "t" && !is.na(b_coeff) && !is.na(SE_coeff)) {
      test_type <- "regression"
    }
    # v0.5.6 (T5): a bare regression line -- "b = .., SE = .., p = .." with NO
    # test statistic of its own. Create a regression result and synthesize the
    # coefficient t = b / SE. All three of b, SE and a reported p are required,
    # so an incidental b/SE co-occurrence cannot spuriously create a result; df
    # is unknown (no test statistic was reported), so the downstream check is a
    # NOTE rather than a full verification.
    if (is.na(test_type) && !is.na(b_coeff) && !is.na(SE_coeff) &&
        SE_coeff != 0 && !all(is.na(m_p))) {
      test_type <- "regression"
      stat_value <- b_coeff / SE_coeff
    }

    # Extract effect size (prioritize by specificity)
    effect_name <- NA_character_
    effect_reported <- NA_real_
    effect_reported_decimals <- NA_integer_
    effect_fallback <- FALSE # NEW: Initialize fallback flag (Phase 2F)

    # v0.4.2 (T3): a Cohen's-d-family token counts as an r-test's reported
    # effect only when it is reported AFTER the r statistic (APA order:
    # statistic, then effect size). A d-family token positioned BEFORE the r
    # belongs to a preceding clause -- e.g. a two-analysis abstract sentence
    # "...(d=0.39[...]) ... (r=-.34[...])" that the test-stat-only sub-chunk
    # splitter cannot separate. A d co-reported after the r (e.g.
    # "r(50)=.40, p=.003, d=0.87") is legitimate and still adopted.
    is_correlation_test <- !is.na(test_type) && test_type == "r"
    r_stat_pos <- if (is_correlation_test) {
      regexpr(if (!all(is.na(m_r))) pat_r else pat_r_nodf, s, perl = TRUE)[1]
    } else {
      0L
    }

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
    } else if (!all(is.na(m_dz)) &&
               (!is_correlation_test || regexpr(pat_dz, s, perl = TRUE)[1] > r_stat_pos)) {
      effect_name <- "dz"
      effect_reported <- numify(m_dz[2])
      effect_reported_decimals <- count_decimal_places(m_dz[2])
    } else if (!all(is.na(m_dav)) &&
               (!is_correlation_test || regexpr(pat_dav, s, perl = TRUE)[1] > r_stat_pos)) {
      effect_name <- "dav"
      effect_reported <- numify(m_dav[2])
      effect_reported_decimals <- count_decimal_places(m_dav[2])
    } else if (!all(is.na(m_drm)) &&
               (!is_correlation_test || regexpr(pat_drm, s, perl = TRUE)[1] > r_stat_pos)) {
      effect_name <- "drm"
      effect_reported <- numify(m_drm[2])
      effect_reported_decimals <- count_decimal_places(m_drm[2])
    } else if (!all(is.na(m_g)) &&
               (!is_correlation_test || regexpr(pat_g, s, perl = TRUE)[1] > r_stat_pos)) {
      effect_name <- "g"
      effect_reported <- numify(m_g[2])
      effect_reported_decimals <- count_decimal_places(m_g[2])
    } else if (!all(is.na(m_d)) &&
               (!is_correlation_test || regexpr(pat_d, s, perl = TRUE)[1] > r_stat_pos)) {
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
      # Cohen's h - accept when co-occurring with a chi-square, z, or binomial
      # test (v0.6.2 adds binomial: the binomial-with-h pattern is the v0.6.2
      # use case). Other contexts left out to avoid false positives from a
      # bare "h = X" elsewhere.
      if (!is.na(test_type) && test_type %in% c("chisq", "z", "binomial")) {
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

    # Stage 1 Gap 2: a bare Kendall's W IS its own reported effect size (the
    # coefficient of concordance), so when no other effect was extracted, the
    # W value carries through as a kendalls_W effect for check.R to recognise.
    if (!is.na(test_type) && test_type == "kendall_w" &&
        is.na(effect_name) && !is.na(stat_value)) {
      effect_name <- "kendalls_W"
      effect_reported <- stat_value
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

    # v0.6.3 (E3/E4): textual anchor for CI selection -- the position of the
    # reported effect-size token in s. The CI bound to this row sits adjacent
    # to (and in APA order, after) the effect-size value, so bind the CI
    # nearest this anchor rather than the first bracket in the chunk. For a
    # correlation test the effect IS the r statistic (adopted later in check.R,
    # so effect_name is still NA here); use the r statistic's position instead.
    effect_match_text <- NA_character_
    if (!is.na(effect_name)) {
      em <- switch(effect_name,
        "f2" = m_f2, "etap2" = m_etap2, "generalized_eta2" = m_eta2_corrupted,
        "eta2" = m_eta2, "eta" = m_eta, "partial_omega2" = m_partial_omega2,
        "omega2" = m_omega2, "epsilon_squared" = m_epsilon2,
        "f" = if (!all(is.na(m_cohens_f))) m_cohens_f else m_bare_f,
        "dz" = m_dz, "dav" = m_dav, "drm" = m_drm, "g" = m_g, "d" = m_d,
        "phi" = m_phi, "V" = m_V, "beta" = m_beta, "R2" = m_R2, "OR" = m_OR,
        "RR" = m_RR, "IRR" = m_IRR, "h" = m_h, "cohens_w" = m_cohens_w,
        NULL)
      if (is.null(em) && isTRUE(effect_fallback) && !all(is.na(m_fallback_es))) {
        em <- m_fallback_es
      }
      if (!is.null(em) && !all(is.na(em))) {
        effect_match_text <- em[1]
      }
    }
    es_anchor <- if (!is.na(effect_match_text)) {
      regexpr(effect_match_text, s, fixed = TRUE)[1]
    } else if (isTRUE(is_correlation_test) && r_stat_pos > 0) {
      r_stat_pos
    } else {
      NA_integer_
    }
    if (!is.na(es_anchor) && es_anchor < 1L) es_anchor <- NA_integer_

    # Match all CI patterns, collecting ALL occurrences with their character
    # positions so the CI bound to this row's effect size can be chosen by
    # proximity (pick_ci_idx + es_anchor) rather than by first-in-chunk. A
    # single-CI sub-chunk yields one candidate -> index 1 -> identical to the
    # pre-0.6.3 first-match behaviour.
    m_CI1_all <- stringr::str_match_all(s, pat_CI1)[[1]]
    m_CI2_all <- stringr::str_match_all(s, pat_CI2)[[1]]
    m_CI3_all <- stringr::str_match_all(s, pat_CI3)[[1]]
    m_CI4_all <- stringr::str_match_all(s, pat_CI4)[[1]]
    p_CI1_all <- gregexpr(pat_CI1, s, perl = TRUE)[[1]]
    p_CI2_all <- gregexpr(pat_CI2, s, perl = TRUE)[[1]]
    p_CI3_all <- gregexpr(pat_CI3, s, perl = TRUE)[[1]]
    p_CI4_all <- gregexpr(pat_CI4, s, perl = TRUE)[[1]]
    m_CI_level <- stringr::str_match(s, pat_CI_level)

    if (nrow(m_CI1_all) > 0L) {
      # Pattern 1: Level explicitly with bounds
      k <- if (length(p_CI1_all) == nrow(m_CI1_all)) pick_ci_idx(p_CI1_all, es_anchor) else 1L
      ci_level <- numify(m_CI1_all[k, 2]) / 100
      ciL <- numify(m_CI1_all[k, 3])
      ciU <- numify(m_CI1_all[k, 4])
      ciL_reported_decimals <- count_decimal_places(m_CI1_all[k, 3])
      ciU_reported_decimals <- count_decimal_places(m_CI1_all[k, 4])
      ci_level_source <- "explicit_with_bounds"
    } else if (nrow(m_CI2_all) > 0L) {
      # Pattern 2: Level explicitly with bounds (alternate format)
      k <- if (length(p_CI2_all) == nrow(m_CI2_all)) pick_ci_idx(p_CI2_all, es_anchor) else 1L
      ci_level <- numify(m_CI2_all[k, 2]) / 100
      ciL <- numify(m_CI2_all[k, 3])
      ciU <- numify(m_CI2_all[k, 4])
      ciL_reported_decimals <- count_decimal_places(m_CI2_all[k, 3])
      ciU_reported_decimals <- count_decimal_places(m_CI2_all[k, 4])
      ci_level_source <- "explicit_with_bounds"
    } else if (nrow(m_CI3_all) > 0L) {
      # Pattern 3: Bounds without level in brackets
      k <- if (length(p_CI3_all) == nrow(m_CI3_all)) pick_ci_idx(p_CI3_all, es_anchor) else 1L
      ciL <- numify(m_CI3_all[k, 2])
      ciU <- numify(m_CI3_all[k, 3])
      ciL_reported_decimals <- count_decimal_places(m_CI3_all[k, 2])
      ciU_reported_decimals <- count_decimal_places(m_CI3_all[k, 3])

      # Look for level stated separately in same sentence
      if (!all(is.na(m_CI_level))) {
        ci_level <- numify(m_CI_level[2]) / 100
        ci_level_source <- "inferred_from_context"
      } else {
        # Default to 95%
        ci_level <- 0.95
        ci_level_source <- "assumed_95"
      }
    } else if (nrow(m_CI4_all) > 0L) {
      # Pattern 4: Bounds without level (parentheses). F(df1, df2) df notation
      # also matches this pattern but is NOT a CI -- drop those candidates,
      # then pick the CI nearest the effect-size anchor among the remainder.
      keep_rows <- integer(0)
      for (ci4_row_idx in seq_len(nrow(m_CI4_all))) {
        ci4_val1 <- numify(m_CI4_all[ci4_row_idx, 2])
        ci4_val2 <- numify(m_CI4_all[ci4_row_idx, 3])
        is_f_test_df <- (!is.na(test_type) && test_type == "F" &&
                         !is.na(df1) && !is.na(df2) &&
                         isTRUE(ci4_val1 == df1) && isTRUE(ci4_val2 == df2))
        if (!is_f_test_df) keep_rows <- c(keep_rows, ci4_row_idx)
      }
      if (length(keep_rows) > 0L) {
        k <- if (length(p_CI4_all) == nrow(m_CI4_all)) {
          keep_rows[pick_ci_idx(p_CI4_all[keep_rows], es_anchor)]
        } else {
          keep_rows[1L]
        }
        ciL <- numify(m_CI4_all[k, 2])
        ciU <- numify(m_CI4_all[k, 3])
        ciL_reported_decimals <- count_decimal_places(m_CI4_all[k, 2])
        ciU_reported_decimals <- count_decimal_places(m_CI4_all[k, 3])

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

    # v0.6.0: clinical-trial CI fallback. The pat_CI1..4 patterns only match
    # bracket / parenthesis forms ([lo, hi], (lo; hi)); the "<lo> to <hi>"
    # form used by RR / rdpct / md_hl sentences is captured INSIDE
    # pat_RR_ci_p (groups 3, 4) / pat_risk_diff (3, 4) / pat_median_diff
    # (3, 4) and would otherwise be discarded. Pull the CI bounds from
    # whichever clinical-trial pattern is in scope when no other CI was
    # matched, so md_hl can sanity-check CI symmetry and downstream CI-audit
    # columns (ci_reported etc.) see the bounds.
    if (is.na(ciL) && is.na(ciU)) {
      if (!is.na(test_type) && test_type == "RR" && !all(is.na(m_RR_ci_p))) {
        ciL <- numify(m_RR_ci_p[3])
        ciU <- numify(m_RR_ci_p[4])
        ciL_reported_decimals <- count_decimal_places(m_RR_ci_p[3])
        ciU_reported_decimals <- count_decimal_places(m_RR_ci_p[4])
        if (is.na(ci_level)) { ci_level <- 0.95; ci_level_source <- "assumed_95" }
      } else if (!is.na(test_type) && test_type == "rdpct" && !all(is.na(m_risk_diff))) {
        ciL <- numify(m_risk_diff[3])
        ciU <- numify(m_risk_diff[4])
        ciL_reported_decimals <- count_decimal_places(m_risk_diff[3])
        ciU_reported_decimals <- count_decimal_places(m_risk_diff[4])
        if (is.na(ci_level)) { ci_level <- 0.95; ci_level_source <- "assumed_95" }
      } else if (!is.na(test_type) && test_type == "md_hl" && !all(is.na(m_median_diff))) {
        ciL <- numify(m_median_diff[3])
        ciU <- numify(m_median_diff[4])
        ciL_reported_decimals <- count_decimal_places(m_median_diff[3])
        ciU_reported_decimals <- count_decimal_places(m_median_diff[4])
        if (is.na(ci_level)) { ci_level <- 0.95; ci_level_source <- "assumed_95" }
      }
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
      df_arity_mismatch = df_arity_mismatch,
      # v0.6.0: clinical-trial per-arm cells (events / totals) extracted from
      # pat_two_props_slash when a "<n1>/<N1> ... versus <n2>/<N2>" clause is
      # in the same chunk as an RR or risk-difference report. Used by check.R
      # to compute RR / RD% independently and verify against the reported value.
      # NA for any row where the slash-count clause was absent or the test is
      # not RR / rdpct.
      arm1_events = if (!all(is.na(m_two_props)) && !is.na(test_type) &&
                        test_type %in% c("RR", "rdpct")) numify_int(m_two_props[2]) else NA_real_,
      arm1_total  = if (!all(is.na(m_two_props)) && !is.na(test_type) &&
                        test_type %in% c("RR", "rdpct")) numify_int(m_two_props[3]) else NA_real_,
      arm2_events = if (!all(is.na(m_two_props)) && !is.na(test_type) &&
                        test_type %in% c("RR", "rdpct")) numify_int(m_two_props[4]) else NA_real_,
      arm2_total  = if (!all(is.na(m_two_props)) && !is.na(test_type) &&
                        test_type %in% c("RR", "rdpct")) numify_int(m_two_props[5]) else NA_real_
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
      df_arity_mismatch = logical(0),
      arm1_events = numeric(0),
      arm1_total  = numeric(0),
      arm2_events = numeric(0),
      arm2_total  = numeric(0)
    ))
  }

  raw_out <- dplyr::bind_rows(out) %>%
    dplyr::filter(!is.na(test_type))

  # v0.5.14: dedup table-fragment duplicates of body-text statistics.
  # Replication/extension papers commonly print a summary table that lists the
  # same correlations / effect sizes already reported in the Results body
  # text. Each numeric appears twice in the extracted corpus: once with a full
  # parenthesized form (e.g. `r(741) = -.43, 95% CI [-.49, -.37]`) and once
  # as a table cell with the same value (e.g. `r = -.43 [-.49, -.37]`). The
  # parser legitimately picks up both — but they are the same statistical
  # result, so emitting two rows inflates the row count and lets the table
  # fragment's check_scope drag the user-facing summary toward extraction-only
  # status.
  #
  # Dedup conservatively: only collapse rows that match exactly on
  # (test_type, stat_value within 1e-3, df1, df2, N). When two rows match,
  # keep the one whose raw_text contains the parenthesized canonical form
  # (e.g. `r(741)`, `t(50)`, `F(2,40)`) — that is the body-text version. The
  # table fragment is dropped.
  #
  # Caught by the 2026-05-23 escicheck-iterate cycle-1 validation against
  # collabra_57785 (Experiential-vs-Material): 3 r-row duplicates between
  # body text and Table 8 summary.
  if (nrow(raw_out) > 1L) {
    has_paren <- grepl("\\b[a-zA-Z]+\\s*\\(\\d", raw_out$raw_text, perl = TRUE)
    # For r-rows, df1 is deterministic from N (df1 = N - 2). The body-text
    # form prints df1 explicitly (e.g. `r(741)`); the table-fragment form
    # omits it (e.g. `r = -.43 [-.49, -.37]`). To recognize them as
    # duplicates, normalize df1 for r-rows: when missing, fill from N-2.
    norm_df1 <- raw_out$df1
    is_r <- raw_out$test_type == "r"
    fillable <- is_r & is.na(norm_df1) & !is.na(raw_out$N)
    norm_df1[fillable] <- raw_out$N[fillable] - 2L

    # Build a dedup key per row. v0.5.14 also keys on the reported CI bounds
    # (round-tripped via the per-row ciL_reported / ciU_reported fields) and
    # the reported effect size. The 2026-05-24 cycle-4 verifiers caught a
    # too-coarse v0.5.14a key collapsing distinct hypothesis tests that
    # happened to share (test_type, stat_value, df, N) but differed in CI
    # bounds (H1a r(261)=0.45 with CI [.35,.55] vs H2a r(261)=0.45 with CI
    # [.35,.54]) or in effect-size binding (one row has d, another does not).
    # Two rows collapse only when ALL keyed fields match (or are both NA);
    # any discriminating signal (CI bound differing by even 0.01, distinct
    # effect_reported, distinct effect_reported_name) keeps them separate.
    ciL <- raw_out$ciL_reported
    ciU <- raw_out$ciU_reported
    er  <- raw_out$effect_reported
    ern <- raw_out$effect_reported_name
    # v0.6.5: a "thin" row with no test statistic (stat_value NA) carries no
    # stat/df/N/CI/effect to distinguish it from another thin row of the same
    # test_type -- two bare binomials ("binomial: p = .002" and "binomial test:
    # p = .047") both key as "binomial|NA|NA|..." and wrongly collapse to one.
    # Add the reported p to the key ONLY for such thin rows; rows WITH a
    # stat_value keep the original key, so the r-row / table-fragment dedup
    # (which intentionally ignores p) is unaffected. (collabra.77859: Study 1
    # gift-preference p=.002 and Study 4 willingness-to-pay-more p=.047.)
    thin_pkey <- ifelse(
      is.na(raw_out$stat_value) & !is.na(raw_out$p_reported),
      as.character(round(raw_out$p_reported, 6L)),
      ""
    )
    keys <- paste(
      raw_out$test_type,
      round(raw_out$stat_value, 3L),
      ifelse(is.na(norm_df1), "NA", as.character(norm_df1)),
      ifelse(is.na(raw_out$df2), "NA", as.character(raw_out$df2)),
      ifelse(is.na(raw_out$N),   "NA", as.character(raw_out$N)),
      ifelse(is.na(ciL), "NA", as.character(round(ciL, 3L))),
      ifelse(is.na(ciU), "NA", as.character(round(ciU, 3L))),
      ifelse(is.na(er),  "NA", as.character(round(er,  4L))),
      ifelse(is.na(ern), "NA", as.character(ern)),
      thin_pkey,
      sep = "|"
    )
    keep <- rep(TRUE, nrow(raw_out))
    for (k in unique(keys)) {
      idx <- which(keys == k)
      if (length(idx) > 1L) {
        # Prefer rows whose raw_text has a parenthesized form (body text).
        paren_idx <- idx[has_paren[idx]]
        if (length(paren_idx) > 0L) {
          # Keep the FIRST parenthesized row, drop all others in this key
          keep[idx] <- FALSE
          keep[paren_idx[1L]] <- TRUE
        } else {
          # No parenthesized form — keep the first, drop the rest
          keep[idx] <- FALSE
          keep[idx[1L]] <- TRUE
        }
      }
    }
    raw_out <- raw_out[keep, , drop = FALSE]
  }

  # v0.6.3 (E4): collapse correlation rows that report the same r with the
  # same reported CI but a DIFFERENT df1. Identical r AND identical CI bounds
  # imply identical n, so a differing df1 is a mis-bound (global-N) duplicate
  # of one true correlation -- e.g. an abstract "(r=-.34[-.43, -.24])" that
  # inherited a global df1=741 alongside the body "r(348)=-0.34, 95% CI
  # [-0.43, -0.24]". The v0.5.14 key above keeps them apart because it keys on
  # df1; this pass keys WITHOUT df1/N (the CI is the safe discriminator) and
  # keeps the parenthesized/inline-df row (its df came from "r(df)"). Guarded
  # on both CI bounds present so it never fires on a row lacking the
  # discriminating CI.
  if (nrow(raw_out) > 1L) {
    is_r_row <- raw_out$test_type == "r"
    rkey <- ifelse(
      is_r_row & !is.na(raw_out$stat_value) &
        !is.na(raw_out$ciL_reported) & !is.na(raw_out$ciU_reported),
      paste(
        round(raw_out$stat_value, 3L),
        ifelse(is.na(raw_out$effect_reported), "NA",
               as.character(round(raw_out$effect_reported, 4L))),
        round(raw_out$ciL_reported, 3L),
        round(raw_out$ciU_reported, 3L),
        sep = "|"
      ),
      NA_character_
    )
    has_paren_r <- grepl("\\b[a-zA-Z]+\\s*\\(\\d", raw_out$raw_text, perl = TRUE)
    keep_r <- rep(TRUE, nrow(raw_out))
    for (k in unique(rkey[!is.na(rkey)])) {
      idx <- which(rkey == k)
      if (length(idx) > 1L) {
        paren_idx <- idx[has_paren_r[idx]]
        keep_r[idx] <- FALSE
        if (length(paren_idx) > 0L) {
          keep_r[paren_idx[1L]] <- TRUE
        } else {
          keep_r[idx[1L]] <- TRUE
        }
      }
    }
    raw_out <- raw_out[keep_r, , drop = FALSE]
  }

  raw_out
}

#' Map docpluck structured table rows to parsed-statistic rows
#'
#' v0.6.4: consumes docpluck's `?structured=true` `flattened_rows[]` (typed
#' `fields`, REQUEST_11 / docpluck v2.4.95) and emits rows in the same shape
#' `parse_text()` returns, so the existing `compute_and_compare_one()` pipeline
#' verifies / routes them with no sentence re-parsing. Only rows whose `fields`
#' carry a recognised statistic are mapped; everything else is skipped (the same
#' safe no-op as an empty `fields`).
#'
#' Mapping (typed keys only -- an effect family is never inferred from an
#' untyped `est`):
#'   - `t`   -> test_type "t" (df from `df`; Cohen's `d` bound when present)
#'   - `F`   -> test_type "F" (df1/df2 when present; a typed partial-eta^2
#'              `eta2` is bound as `etap2` -- docpluck v2.4.98 types it on
#'              structurally-identified ANOVA tables, DP-3 -- so the effect is
#'              recomputed + verified when df1+df2 are present, else surfaced in
#'              an honest NOTE. An UNtyped `est` is still left unbound.)
#'   - `r`   -> test_type "r" (N from `n`; reported CI carried so a row with no
#'              df/N still adopts the r as its own effect and is checked against
#'              its CI rather than dropped)
#'   - `eta2` with no usable F/t/r -> test_type "table_estimate" naming the
#'              effect `etap2` (an effect-only ANOVA cell: partial-eta^2 + CI,
#'              surfaced as an extraction-only NOTE, DP-3)
#'   - `est` (no test statistic) -> test_type "table_estimate", an
#'              extraction-only NOTE that surfaces est + CI + p (cannot be
#'              independently recomputed from an estimate alone)
#'
#' Reported CI bounds map to `ciL_reported` / `ciU_reported`; `p_op` to
#' `p_symbol`. Each row is tagged `from_table = TRUE` (so check.R sets
#' `result_context = "table"`) and carries `source_table` / `table_group`
#' (the docpluck arm tag: ITT/PP, Separate/Joint, Target article/Replication).
#'
#' @param table_rows A list of docpluck flattened-row records, each a list with
#'   `label`, `row_label`, `row_idx`, and a `fields` list. NULL or empty returns
#'   NULL.
#' @return A tibble of parsed-statistic rows (or NULL), bindable to the
#'   `parse_text()` output via `dplyr::bind_rows()`.
#' @keywords internal
flattened_rows_to_parsed <- function(table_rows) {
  if (is.null(table_rows) || length(table_rows) == 0L) {
    return(NULL)
  }
  num1 <- function(v) {
    if (is.null(v) || length(v) == 0L) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(v[[1]]))
  }
  has <- function(f, k) {
    !is.null(f[[k]]) && length(f[[k]]) > 0L && !is.na(num1(f[[k]]))
  }
  rows <- lapply(seq_along(table_rows), function(i) {
    rec <- table_rows[[i]]
    f <- rec$fields
    if (is.null(f) || length(f) == 0L) {
      return(NULL)
    }
    grp <- if (!is.null(f$group) && length(f$group) > 0L) {
      as.character(f$group[[1]])
    } else {
      NA_character_
    }
    label <- if (!is.null(rec$label) && length(rec$label) > 0L) {
      as.character(rec$label[[1]])
    } else {
      ""
    }
    rlab <- if (!is.null(rec$row_label) && length(rec$row_label) > 0L) {
      as.character(rec$row_label[[1]])
    } else {
      ""
    }
    # v0.6.6 (E-D1): a replication/extension paper's summary table often prints
    # the ORIGINAL study's statistics in a "Target article" / "Original study"
    # column next to the paper's own "Replication" column (e.g. collabra.90203
    # Tables 8-10 reproduce Small et al. 2007's F / r values for comparison).
    # docpluck flattens both columns into rows; a row whose row_label OR group
    # marks it as the comparison/original column is NOT one of THIS paper's
    # results and must not be emitted (it would be checked + counted as the
    # audited paper's finding -- e.g. F = 6.75 / 5.32 from the Target-article
    # column surfaced as spurious own-result rows). The paper's own rows
    # (row_label/group = "Replication", or a substantive condition label) pass.
    comparison_col_re <- paste0(
      "(?i)\\b(target\\s+article|original\\s+(article|study|paper)|",
      "source\\s+article|prior\\s+(study|work))\\b"
    )
    is_comparison_row <-
      grepl(comparison_col_re, rlab, perl = TRUE) ||
      (!is.na(grp) && grepl(comparison_col_re, grp, perl = TRUE))
    if (is_comparison_row) {
      return(NULL)
    }
    # Human-readable provenance string used as raw_text so the row is
    # identifiable in output and audit (there is no source sentence).
    prov <- trimws(paste0(
      label,
      if (nzchar(label) && nzchar(rlab)) ": " else "",
      rlab,
      if (!is.na(grp)) paste0(" (", grp, ")") else ""
    ))

    # v0.6.8 (E-A3): a flattened table row's design lives in the table NOTE
    # ("Paired-samples t for joint"), which docpluck does NOT carry onto the row
    # -- the row only carries its column label (group / row_label). So a
    # joint-evaluation t-test row arrived with no design signal in its
    # context_window (which is just the bare table label) and check.R defaulted
    # it to "independent" (collabra.77859 / collabra.57785 Table-3 joint rows
    # t(131) tagged independent though the gold says within-subjects / paired).
    # Map a recognised within-design column label (joint evaluation / within /
    # paired / repeated) or a between-design label (separate evaluation /
    # between / independent) to an explicit design phrase and inject it into the
    # row's context_window, so check.R's EXISTING t-test design detector fires
    # uniformly. Scoped to the structured column label only (a bounded surface);
    # "joint" / "separate" are the within/between markers of the joint-vs-separate
    # evaluation paradigm (Hsee 1998), where joint = same participant rates both
    # (within) and separate = different participants (between). The label must
    # match as a whole word so "disjoint"/"separately-and-jointly" prose cannot
    # trip it.
    design_hint <- ""
    label_for_design <- tolower(paste(rlab, if (!is.na(grp)) grp else ""))
    if (grepl("\\b(joint|within|paired|repeated[- ]measures|within[- ]subjects?)\\b",
              label_for_design, perl = TRUE)) {
      design_hint <- " [Analysis: within-subjects paired-samples t-test (joint condition).]"
    } else if (grepl("\\b(separate|between[- ]subjects?|independent[- ]samples?)\\b",
                     label_for_design, perl = TRUE)) {
      design_hint <- " [Analysis: between-subjects independent-samples t-test (separate condition).]"
    }

    tt <- NA_character_
    stat <- NA_real_
    d1 <- NA_real_
    d2 <- NA_real_
    ern <- NA_character_
    er <- NA_real_
    nn <- NA_real_
    if (has(f, "t")) {
      tt <- "t"
      stat <- num1(f$t)
      if (has(f, "df")) d1 <- num1(f$df)
      if (has(f, "d")) {
        # v0.6.8 (E-A3 follow-on): docpluck types the joint/separate-evaluation
        # effect column generically as `d`, but a within-subjects (joint /
        # paired) row's standardized mean difference IS a dz (the table note
        # reads "d_z for paired"). When this row's column label marks a within
        # design (design_hint set to the paired phrase below), name the effect
        # `dz` so the reported-effect metadata matches the design; the value is
        # unchanged. A separate / between row keeps the plain `d`. (collabra.77859
        # Table-3 Attractive/Affect Joint rows were labeled `d` though the table
        # note + gold say d_z.)
        is_within_label <- grepl(
          "\\b(joint|within|paired|repeated[- ]measures|within[- ]subjects?)\\b",
          tolower(paste(rlab, if (!is.na(grp)) grp else "")), perl = TRUE)
        ern <- if (is_within_label) "dz" else "d"
        er <- num1(f$d)
      }
    } else if (has(f, "F")) {
      tt <- "F"
      stat <- num1(f[["F"]])
      if (has(f, "df1")) d1 <- num1(f$df1)
      if (has(f, "df2")) d2 <- num1(f$df2)
      # v0.6.7 (DP-3, docpluck v2.4.98): docpluck now TYPES the partial-eta^2
      # column as `fields.eta2` on a structurally-identified F-test/ANOVA table
      # (an F column + BF01/CI, no competing d/dz/r/OR), so the effect is no
      # longer nameless. Bind it as `etap2` -- the same canonical reported name
      # the prose parser emits for partial eta-squared (parse.R `pat_etap2` ->
      # `effect_name <- "etap2"`) -- so the row flows through the identical
      # partial_eta2 verification path: with df1+df2 it is recomputed from F and
      # compared; without df it routes to an honest NOTE that still surfaces the
      # reported eta2 + CI. (Previously left unbound: docpluck emitted it
      # untyped, so the value was discarded. The body-text glyph stays stripped
      # -- WON'T-FIX, no ToUnicode CMap -- so the table is the ONLY source.)
      if (has(f, "eta2")) {
        ern <- "etap2"
        er <- num1(f$eta2)
      }
    } else if (has(f, "r")) {
      tt <- "r"
      stat <- num1(f$r)
      if (has(f, "n")) nn <- num1(f$n)
    } else if (has(f, "eta2")) {
      # v0.6.7 (DP-3): an effect-only table cell -- a typed partial-eta^2 with
      # its CI but NO usable F/t/r in the row (e.g. collabra.90203 Table 8 rows
      # where docpluck delivers eta2 + CI but the F cell is blank). Surface it as
      # an extraction-only NOTE that names the effect (etap2) and carries its CI,
      # rather than dropping the row. Not independently recomputable without F+df,
      # but the named value + CI are now visible instead of lost.
      tt <- "table_estimate"
      ern <- "etap2"
      er <- num1(f$eta2)
    } else if (has(f, "est")) {
      tt <- "table_estimate"
      ern <- "estimate"
      er <- num1(f$est)
    } else {
      return(NULL)
    }

    p_val <- if (has(f, "p")) num1(f$p) else NA_real_
    p_sym <- if (!is.null(f$p_op) && length(f$p_op) > 0L) {
      as.character(f$p_op[[1]])
    } else if (!is.na(p_val)) {
      "="
    } else {
      NA_character_
    }
    ciL <- if (has(f, "CI_lower")) num1(f$CI_lower) else NA_real_
    ciU <- if (has(f, "CI_upper")) num1(f$CI_upper) else NA_real_

    # Inject the design hint into context_window for t-test rows only (the
    # t-test design detector in check.R reads context_window; F-tests have their
    # own within/between detector that should not be steered by a paired/joint
    # column label, and r/table_estimate rows have no design dimension).
    ctx_win <- if (identical(tt, "t") && nzchar(design_hint)) {
      paste0(label, design_hint)
    } else {
      label
    }

    tibble::tibble(
      location = i,
      raw_text = prov,
      context_window = ctx_win,
      test_type = tt,
      df1 = d1,
      df2 = d2,
      stat_value = stat,
      p_reported = p_val,
      p_symbol = p_sym,
      p_valid = !is.na(p_val),
      p_out_of_range = FALSE,
      N = nn,
      effect_reported_name = ern,
      effect_reported = er,
      ci_level = 0.95,
      ci_level_source = "assumed_95",
      ciL_reported = ciL,
      ciU_reported = ciU,
      from_table = TRUE,
      source_table = label,
      table_group = grp
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0L) {
    return(NULL)
  }
  dplyr::bind_rows(rows)
}
