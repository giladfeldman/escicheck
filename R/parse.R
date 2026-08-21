# Suppress R CMD check NOTEs for NSE column references
utils::globalVariables(c("p_reported", "test_type"))

# ============================================================================
# Numeric separator normalization -- shared spec (v0.7.2)
#
# The comma is a decimal separator in European papers ("d = 0,80") and a
# thousands separator in English ones ("U = 12,345"). Resolving one breaks the
# other. docpluck (Python) and effectcheck (R) each implemented that resolution
# independently, nothing tested them against each other, and they diverged --
# effectcheck's version turned "U = 12,345" into 12.345 and published a
# rank-biserial correlation of 0.99938 where the truth was 0.38275, status OK.
#
# The semantics now live in one place: inst/normalization-spec/SPEC.md, with an
# executable, language-neutral contract in conformance.json that BOTH
# implementations must satisfy. Adding a case there is how a new requirement
# enters both. Two implementations with no shared test cannot stay in sync.
# ============================================================================

#' Version of the numeric-separator normalization spec implemented here
#'
#' Recorded on every result row so a published number carries the provenance of
#' the rules that produced it. Must match `spec_version` in
#' `inst/normalization-spec/conformance.json` (pinned by a regression test).
#'
#' @return Character scalar, e.g. "1.0.0".
#' @keywords internal
normalization_spec_version <- function() "1.5.0"

#' Spec rules S1-S4 -- spans where a comma between digits is a REAL comma
#'
#' A comma between digits has at least four meanings, not two: thousands
#' separator, decimal separator, list/pair separator, and structural (an
#' identifier). The numeric rules below can only reason about the first two, so
#' the structural spans are lifted out of the text before they run and restored
#' afterwards. Protecting by substitution rather than by lookbehind keeps the
#' numeric patterns readable and avoids PCRE's fixed-width lookbehind limit.
#'
#' Every entry here corresponds to an observed corruption:
#'   "Experiments 1,2"  -> "Experiments 1.2"   (a list read as a decimal)
#'   "anchors 1,2,3,4,5" -> "anchors 1.2,3,4,5" (worse: partially converted)
#'   "gender coded 0,1" -> "coded 0.1"         (and it poisons locale inference,
#'                                              since "0,1" looks decisive)
#'   "matrix[1,2]"      -> "matrix[1.2]"
#'   "doi:...45,6"      -> "doi:...45.6"
#' @keywords internal
.NUM_STRUCTURAL_PATTERNS <- c(
  # Identifiers first -- they may contain any of the shapes below.
  "(?i)\\b(?:doi|https?|www)\\S*",
  # v0.7.6: SPACE-SEPARATED AUTHOR AFFILIATION RUNS. The glued form
  # ("Braunstein1,3") is blocked by D1's letter lookbehind and a 3+ element run
  # by the bare-chain width test, but a TWO-element run separated from the name
  # by a space falls through both -- docpluck reported, and this was reproduced,
  # that "Erik. T. Frank 1,2 , Lucie Kesner 3" became "Frank 1.2".
  #
  # Deliberately narrow. A surname followed by a small digit run is not by
  # itself distinguishable from a European decimal ("Median 0,45" has the same
  # shape), so the discriminator is the AUTHOR-LIST PUNCTUATION that follows:
  # a SPACE BEFORE the separating comma, which is how an author list is set
  # ("Frank 1,2 , Lucie") and is not how a statistic is ever written.
  #
  # The space is load-bearing and was found by testing rather than reasoning: a
  # first draft used `\\s*,` and therefore also matched "Median 0,45, SD 0,12",
  # protecting a real European decimal from conversion. `\\s*` admits zero
  # spaces, so "0,45," satisfied it. Requiring a literal space separates the
  # two shapes exactly.
  #
  # Deliberately narrow. A surname followed by a small digit run is not by
  # itself distinguishable from a European decimal ("Median 0,45" has the same
  # shape), so matching only the reported punctuation is the point -- a general
  # "name + digits" rule would start suppressing real continental decimals,
  # which is the more expensive error of the two.
  "\\b[A-Z][a-z]{2,}\\s\\d{1,2}(?:\\s*,\\s*\\d{1,2})+(?= ,)",
  # v0.7.6: SOFTWARE VERSION STRINGS. "v2.1.451" is dotted exactly like European
  # full notation, so a trailing citation run fused with it -- docpluck
  # reported, and this was reproduced, that "software v2.1.451,52." became
  # "v2.1451.52.": the full-notation rule read "1.451,52" as a grouped European
  # number and rewrote a version identifier into garbage. A version is an
  # identifier, not a quantity, so it belongs here with the DOIs and URLs.
  "(?i)\\bv\\d+(?:\\.\\d+)+",
  # Head noun + integer list: "Experiments 1,2", "items 1,5 and 7".
  # Head noun + integer list. The noun must PRECEDE the numbers, so a genuine
  # count written the other way round ("1,234 participants") is untouched.
  # This is a SECONDARY signal: the primary discriminator is group width (see
  # the bare-chain pattern below). An enumerated vocabulary cannot be complete
  # -- that limitation is real and is why it is not the main mechanism -- but
  # it resolves the one shape width cannot, where every group is 3 digits and
  # a list is therefore indistinguishable from a grouped number
  # ("excluded IDs 101,102,103" vs a genuine 101102103).
  paste0("(?i)\\b(?:experiments?|stud(?:y|ies)|tables?|figures?|items?|",
         "conditions?|sections?|appendi(?:x|ces)|models?|groups?|anchors?|",
         "levels?|phases?|blocks?|waves?|panels?|steps?|ids?|trials?|",
         "runs?|sites?|cohorts?|rounds?|sessions?|clusters?|chapters?|",
         "equations?|refs?|references?|questions?|scales?)\\s+",
         "\\d{1,3}(?:\\s*,\\s*\\d{1,3})+"),
  # Coded / dummy variables: "coded 0,1", "dummy coded 0,1,2".
  #
  # v0.7.6: the trailing `(?!\\d)` fixes a PREFIX MATCH, not a vocabulary
  # problem. Each element here is a single `\\d`, so on "Participants scored
  # 0,87" the pattern happily matched the leading "0,8" and protected it --
  # leaving a genuine continental score unconverted while "Participants had
  # 0,87" converted correctly. docpluck reported this as a false negative in
  # the word list and was about to adopt that word list because of it;
  # measuring showed the vocabulary was never the cause. "coded 0,1" is still
  # protected, because there the run really does end after one digit.
  "(?i)\\b(?:coded|dummy[- ]coded|scored)\\s+\\d(?:\\s*,\\s*\\d)+(?!\\d)",
  # Integer-only bracket pair = an index, not a CI. A CI carries decimal points,
  # so requiring NO period separates "[1,2]" from "[0.12, 0.45]". Two variants,
  # because the bracket must NOT be a statistical one: "t(1,197)" and "H(1,024)"
  # are single-df tests whose comma IS a thousands separator (rule T2), and a
  # first draft protected those by mistake. A stat bracket is a short token
  # glued to the bracket ("t(", "H(", "F["); an index is either preceded by a
  # non-letter ("cell (1,2)") or by a longer identifier ("matrix[1,2]").
  # `(?:,\s*\d{1,3})+` not a single pair: vectors and tuples of three or more
  # ("(0,0,0)", "the state vector was (0,1,0,1)") occur in real papers in this
  # repo's own validation corpus, and a pair-only pattern turned "(0,0,0)" into
  # "(0.0, 0)". The sibling noun-list pattern above already used `+`; this one
  # was simply written without it.
  # A PAIR may be loosely spaced: "cell (1, 2)".
  "(?<=[^A-Za-z])[\\[(]\\s*\\d{1,3}\\s*,\\s*\\d{1,3}\\s*[\\])]",
  "(?<=[A-Za-z]{4})[\\[(]\\s*\\d{1,3}\\s*,\\s*\\d{1,3}\\s*[\\])]",
  # A TUPLE of 3+ must be tight: "(0,0,0)", "(0,1,0,1)". The no-space
  # requirement is what separates it from a CI whose bounds are separated by
  # ", " -- generalising the pair pattern with `\s*` swallowed
  # "95% CI [1,234, 5,678]" and stopped it converting at all.
  "[\\[(]\\d{1,3}(?:,\\d{1,3}){2,}[\\])]",
  # BARE chains of three or more comma-separated integers where the groups are
  # NOT all 3 digits wide. A genuine grouped number has every group after the
  # first exactly three digits ("1,000,000", "1,054,908"); a list does not
  # ("Figures 6,7,8", "10,14,19", "Ye1,2,3,4", "2017,10,39" -- a year followed
  # by citation superscripts). Corpus evidence: of 35 bare chains in the
  # validation set, that single width test classifies 34 correctly.
  #
  # Without this the decimal rule converted only the FIRST pair of such a chain
  # -- "1,2,3,4,5" became "1.2,3,4,5" -- which is worse than either answer,
  # because it corrupts rather than merely mis-reads.
  "(?<![\\[(0-9.])\\d{1,4}(?:,\\d{1,2}(?![\\d]))(?:,\\d{1,4})+(?![\\d])"
)

#' Lift structural spans out of the text, returning placeholders and a store
#' @keywords internal
.protect_structural <- function(x) {
  store <- character(0)
  for (pat in .NUM_STRUCTURAL_PATTERNS) {
    repeat {
      m <- regexpr(pat, x, perl = TRUE)
      if (m[1] == -1) break
      hit <- regmatches(x, m)
      store <- c(store, hit)
      # \x01 is not present in extracted document text; the index makes each
      # placeholder unique so restoration is exact.
      regmatches(x, m) <- sprintf("\x01%d\x01", length(store))
    }
  }
  list(x = x, store = store)
}

#' @keywords internal
.restore_structural <- function(x, store) {
  if (length(store) == 0L) return(x)
  for (i in rev(seq_along(store))) {
    x <- gsub(sprintf("\x01%d\x01", i), store[i], x, fixed = TRUE)
  }
  x
}

#' Infer the document's numeric locale from decisive markers
#'
#' The two conventions are MUTUALLY EXCLUSIVE -- if the comma is the decimal
#' separator then thousands must be grouped with a period, space or apostrophe,
#' and vice versa. So a single unambiguous token anywhere in the text settles
#' how every ambiguous token in it should be read. That is why this returns an
#' article-level fact rather than a per-sentence guess.
#'
#' Markers are OPERATOR-GUARDED. A bare "0,1" or "0.1" is not decisive: it also
#' matches coded variables ("gender was coded 0,1"), version numbers ("version
#' 0.1"), ratios ("0.5:1") and lists ("levels 0,1 and 2"). Requiring a
#' comparison or assignment operator is what identifies a REPORTED STATISTIC.
#' Verified: the bare form misfires on 5 of 10 realistic strings, the guarded
#' form on none.
#'
#' `\d,\d{1,2}` is deliberately excluded -- it is contaminated both by lists and
#' by tight CI separators ("[0.57,0.73]"), which produced a false CONFLICT on a
#' real article whose six "European" signals were all CI commas.
#'
#' @param text Character scalar (any amount -- a clause, a section, a document).
#' @return List with `decimal_mark`, `grouping_marks`, `confidence`
#'   ("decisive", "conflict" or "none") and `evidence`.
#' @keywords internal
infer_numeric_locale <- function(text) {
  txt <- paste(text, collapse = "\n")
  op <- "[=<>\u2264\u2265]\\s*"
  eu <- list(
    # 1.234,56 / 1'234,56 / NBSP and narrow-NBSP variants. A PLAIN space is
    # excluded for the same reason as in .apply_full_notation: it matched
    # "403,669 107,081" (two English table counts) as one European number and
    # classified the whole document European, which then gated the thousands
    # rule off and left every count in that table unstripped.
    F1 = "\\d[.\u00A0\u202F'](\\d{3}),\\d",
    E2 = paste0(op, ",\\d{2,}"),                       # p < ,001
    E3 = paste0(op, "0,\\d"),                          # d = 0,80
    E4 = "\\d,\\d{4,}",                                # 0,12345
    E5 = "\\d,\\d+[eE][-+]?\\d"                        # 1,23e-4
  )
  us <- list(
    F2 = "\\d,\\d{3}\\.\\d",                           # 1,234.56
    U2 = paste0(op, "\\.\\d{2,}"),                     # p < .001
    U3 = paste0(op, "0\\.\\d"),                        # d = 0.80
    U4 = "\\d,\\d{3},\\d{3}"                           # 1,234,567
  )
  hits <- function(set) {
    out <- list()
    for (nm in names(set)) {
      if (grepl(set[[nm]], txt, perl = TRUE)) {
        m <- regexpr(set[[nm]], txt, perl = TRUE)
        out[[nm]] <- list(n = length(gregexpr(set[[nm]], txt, perl = TRUE)[[1]]),
                          text = regmatches(txt, m))
      }
    }
    out
  }
  e <- hits(eu); u <- hits(us)
  ne <- sum(vapply(e, function(h) h$n, numeric(1)))
  nu <- sum(vapply(u, function(h) h$n, numeric(1)))

  if (ne == 0 && nu == 0) {
    return(list(decimal_mark = NA_character_, grouping_marks = NA_character_,
                confidence = "none", evidence = list()))
  }
  # Do not majority-vote a genuine two-sided conflict; report it. A lopsided
  # ratio is treated as contamination in the minority channel.
  if (ne > 0 && nu > 0 && min(ne, nu) / max(ne, nu) > 0.2) {
    return(list(decimal_mark = NA_character_, grouping_marks = NA_character_,
                confidence = "conflict", evidence = list(european = e, us = u)))
  }
  if (ne > nu) {
    list(decimal_mark = ",", grouping_marks = c(".", " ", "'"),
         confidence = "decisive", evidence = e)
  } else {
    list(decimal_mark = ".", grouping_marks = ",",
         confidence = "decisive", evidence = u)
  }
}

#' Is the comma's role in this document still unestablished?
#'
#' v0.7.5. THE single predicate every locale-gated rule asks. Three rules asked
#' it independently -- `.apply_thousands_protect()`, the test-statistic
#' paren pre-strip, and D1's `.amb_shape` exclusion -- and all three wrote
#' `identical(decimal_mark, ",")`, which is only two of the three states.
#'
#' `infer_numeric_locale()` returns THREE outcomes, and it sets `decimal_mark`
#' to NA for two of them:
#'   "decisive"  -- one convention attested; decimal_mark is "," or "."
#'   "none"      -- no evidence either way; decimal_mark NA. The US-style
#'                  thousands default applies, which is the right prior.
#'   "conflict"  -- BOTH conventions attested and neither dominant; decimal_mark
#'                  NA to say *unknown*, not *absent*.
#' Testing the decimal mark alone collapses "conflict" into "none", so a document
#' with European decimals in it was normalized as if it were decisively US.
#'
#' REPRODUCED (2026-08-09): a document mixing "p = .035, d = 0.80" with "Welch's
#' correction gave t(2,758) = 3,21, d = 0,45" infers `conflict`, then stripped
#' the comma from `t(2,758)` and published df = 2758, N = 2760 and a COMPUTED
#' d = 0.122 against a reported 0.45 -- a false WARN, carrying a fabricated
#' effect size, against a correctly reported result. Under the decisive-European
#' branch the identical string yields status NOTE with no computed value. That
#' is the honest outcome and conflict now reaches it.
#'
#' Note this is the OPPOSITE of a guess: when the role is unresolved every rule
#' steps aside and the token survives verbatim, so the row carries the ambiguity
#' instead of a guess dressed as a value. Both halves are required -- if T1 steps
#' aside but D1 still converts, "leave it alone" silently becomes "call it a
#' European decimal", which is merely the other unfounded guess.
#'
#' 0 of the 48 validation-corpus papers infer `conflict`, so the whole-corpus
#' diff for this change is empty by construction. That is the evidence it is
#' safe, not a reason to have skipped it: a computed signal that nothing reads
#' is a trap, because it reads as handled at every site that mentions it.
#'
#' @param locale The list returned by `infer_numeric_locale()`, or NULL
#' @return TRUE when no rule may act on a bare digit-comma-digit token
#' @keywords internal
.locale_comma_unresolved <- function(locale) {
  !is.null(locale) &&
    (identical(locale$decimal_mark, ",") || identical(locale$confidence, "conflict"))
}

#' Spec rules F1/F2 -- full notation, where BOTH marks are present
#'
#' "1.234,56" and "1,234.56" are self-identifying: seeing both separators in one
#' token fixes their roles with no locale knowledge at all. Handled before T1/D1
#' because those rules see only one separator at a time and previously turned
#' "1.234,56" into the unparseable "1.234.56".
#' @keywords internal
.apply_full_notation <- function(x) {
  # Match the WHOLE number, then rebuild it. A first draft did this with
  # incremental gsubs and a marker token, and one of those steps matched
  # "0.1100" as a period-grouped thousands token -- deleting the decimal point
  # and turning eta2 = 0.1100 into 01100. Any 4-decimal value was exposed.
  # Rebuilding a fully-matched token cannot do that: the pattern REQUIRES both
  # separators, so an ordinary decimal never enters it.

  # European: 1.234,56 / 1'234,56 / 1.234.567,89, plus the TYPOGRAPHIC space
  # variants (NBSP, narrow NBSP). A PLAIN space is deliberately excluded: in
  # English-language journals a space between digit groups separates two
  # numbers far more often than it groups one. Including it destroyed a real
  # table row -- "All places 403,669 107,081" matched "669 107,081", read the
  # space as a thousands separator and the comma as a decimal, and produced
  # "403.669107.081", fusing two counts into one fictional number.
  #
  # v0.7.4: the trailing `(?!\.\d)` is what makes "self-identifying" actually
  # true. Without it the rule fired on a TIGHT CI PAIR -- an interval written
  # with no space after the comma, which is how nathumbeh_replication_2025
  # writes all 11 of its intervals:
  #   "95%CI=[7.944,11.984]"  ->  matched "7.944,11"  ->  "95%CI=[7944.11.984]"
  # and the interval was DESTROYED (ciL and ciU both NA, status still OK). The
  # same clause written with a space parses correctly, which is exactly what
  # made it invisible: the paper's other rows look fine.
  #
  # The guard is structural, not locale-based, so it needs no inference: in
  # genuine full notation the part after the comma is a TERMINAL fraction, and a
  # terminal fraction cannot be followed by another decimal point and more
  # digits. "7.944,11.984" therefore cannot be one number in any locale, while
  # "1.234,56" and a sentence-final "1.234,56." both still match.
  # (Cross-model review of v0.7.3, Claude Sonnet 5 2026-08-09; the constructed
  # case was "d = 0.123,456 participants", the corpus supplied the real one.)
  #
  # BOTH lookaheads are load-bearing. `(?!\.\d)` alone is defeated by
  # backtracking: on "7.944,11.984" the engine gives up the second `1` and
  # matches "7.944,1", whose next character is `1` rather than `.`, so the
  # rebuild still fires and still corrupts. `(?!\d)` forbids exactly those
  # shortened alternatives, leaving no match at all. A possessive `\d++` would
  # do the same in PCRE, but this rule is part of the cross-language spec and
  # Python's `re` has no possessive quantifier, so the two lookaheads are the
  # portable form.
  #
  # `(?!\s*%)` is the SECOND shape of the same defect, found in the same paper
  # by inspecting what the first guard left behind:
  #   "=-0.008,95%CI=[-0.023,0.007]"  ->  "=-0008.95%CI=[...]"
  # Here the comma introduces the CI's confidence LEVEL, so the reported
  # coefficient was fused with "95" and the effect size destroyed. `(?!\.\d)`
  # cannot see it because what follows the fraction is `%`. A European
  # percentage written in full notation ("1.234,56%") is refused as collateral;
  # that only leaves the token verbatim, which is the conservative direction,
  # and such a value is not a statistic this parser reads.
  m <- gregexpr("\\d{1,3}(?:[.\u00A0\u202F']\\d{3})+,\\d+(?!\\d)(?!\\.\\d)(?!\\s*%)",
                x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) {
    if (!length(v)) return(v)
    sub(",", ".", gsub("[.\u00A0\u202F ']", "", v), fixed = TRUE)
  })

  # US: 1,234.56 / 1,234,567.89
  m <- gregexpr("\\d{1,3}(?:,\\d{3})+\\.\\d+", x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) {
    if (!length(v)) return(v)
    gsub(",", "", v, fixed = TRUE)
  })
  x
}

#' Spec rule T1 -- strip commas from unambiguously thousands-grouped integers
#'
#' Runs BEFORE the decimal rule so that rule never sees a thousands group.
#' Four independent guards (see SPEC.md): the integer must start `[1-9]` (so
#' "0,001" is left for the decimal rule); every comma must be followed by
#' EXACTLY three digits (so "0,05" and "1,5" cannot match); a trailing boundary
#' is required; and a negative lookbehind leaves statistical brackets alone,
#' because deciding whether `F(7,140)` is a df pair needs to know the test --
#' which is effectcheck's layer (rule T2), not this context-free one.
#'
#' @param x Character vector.
#' @return `x` with thousands separators removed from qualifying integers.
#' @keywords internal
.apply_thousands_protect <- function(x, locale = NULL) {
  # Locale gate (spec rule L1). "N = 1,234" is the ONE shape structure cannot
  # decide: 1234 grouped, or a European 1.234? If the document elsewhere carries
  # a decisive marker that the comma is its DECIMAL separator, then stripping
  # here would be wrong -- so leave the token alone for the decimal rule and the
  # ambiguity flag to handle. The two conventions are mutually exclusive, which
  # is what makes one observation anywhere in the document sufficient.
  if (.locale_comma_unresolved(locale)) return(x)
  # The boundary set includes "/" for clinical-trial arm counts, which are
  # written "1,234/5,678 (21.7%) versus ...". Without it the first count kept its
  # comma, the arm regex's \d+ stopped at the comma, and arm1_events came back
  # 234 instead of 1234 -- a silently wrong event count feeding the risk-ratio
  # recomputation. Caught by the end-to-end check, not by the unit tests.
  # NEGATIVE boundary, not a whitelist of terminators. The whitelist form had to
  # be patched for "/", then "%", then "-", each time after a silent failure --
  # the same enumeration anti-pattern this whole rewrite exists to remove.
  # "not followed by another digit" admits every terminator at once.
  # `,\s?` admits the OCR/extraction spacing variant "N = 1, 234". Without it
  # that string reached .pat_doc_N, whose digit run stops at the comma, and the
  # document N became 1 -- with status OK, and with no df-based plausibility
  # guard to catch it on a z/U/W test. Found by cross-model audit; it is the
  # most severe defect in this area precisely because it lives OUTSIDE the
  # normalization rules everything else here was focused on.
  #
  # The leading anchor is (?<![\w]) rather than \b: digits and letters are both
  # word characters in PCRE, so \b never fires after a glued letter and
  # "1,920x1,080" stripped only its first number.
  # "." joins the leading exclusion. Admitting "\s?" after the comma (needed for
  # the OCR spacing variant "N = 1, 234") let T1 reach ACROSS an F df pair:
  # "F(1.87, 654.3)" matched "87, 654" and fused it into "F(1.87654.3)", losing
  # both degrees of freedom. A digit preceded by a decimal point is the
  # fractional part of another number, never the start of a thousands group.
  # NO space after the comma. An earlier draft admitted "\s?" to fix the OCR
  # variant "N = 1, 234", and corpus evidence showed that was badly wrong: among
  # 2,984 real digit-comma-digit occurrences in the validation corpus, the
  # space-then-3-digits shape is overwhelmingly a REFERENCE LIST ("Psychol.
  # Methods 3, 424-453", "Nature genetics 54, 437-449") or an RGB triple, not a
  # grouped number. The permissive form fused volume into page ("3424-453") and
  # "RGB = 120, 120, 120" into "120120120". A space after the comma is strong
  # evidence AGAINST a thousands separator: 68% of no-space occurrences are the
  # thousands shape, versus 22% of spaced ones. "N = 1, 234" is handled by the
  # narrow sample-size rule below, where the spacing IS an extraction artifact.
  pat <- "(?<![A-Z][\\(\\[])(?<![0-9.])[1-9]\\d{0,2}(?:,\\d{3})+(?!\\d)"
  m <- gregexpr(pat, x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) {
    if (length(v) == 0L) return(v)
    gsub(",", "", v, fixed = TRUE)
  })

  # Narrow spaced variant, SAMPLE-SIZE CONTEXT ONLY. "N = 1, 234" and
  # "nobs = 12, 345" are extraction artifacts: the token is explicitly labelled
  # a count, so a space inside it is damage rather than a separator. Scoping to
  # the N/n label is what makes this safe -- the same spacing in running prose
  # is a reference volume or a list, and the general rule above must not touch
  # it. Without this, .pat_doc_N's digit run stopped at the comma and the
  # document N became 1, with status OK and no df-based guard on a z/U/W test.
  #
  # v0.7.4: this was a `repeat` loop over a gsub anchored on `\d{1,3}`, and it
  # could only ever run ONCE -- after the first join the prefix is four digits
  # and the pattern no longer matches itself. "nobs = 1, 234, 567" stopped at
  # "nobs = 1234, 567" and the row published N = 1234: a wrong sample size, not
  # a missing one. (Cross-model review of v0.7.3, Codex 2026-08-09, reproduced.)
  #
  # It now matches the WHOLE labelled run in one go and strips the separators
  # inside it. Deliberately NOT done by widening the anchor to `\d{1,3}(?:\d{3})*`
  # -- the obvious repair, and wrong: that also admits a run whose FIRST group is
  # already four digits, so "N = 1234, 567 of whom were female" would fuse into
  # N = 1234567, a case v0.7.3 correctly refused. The start anchor stays exactly
  # as strict as it was; only the continuation is new.
  m <- gregexpr("\\b[Nn](?:obs|\\d)?\\s*=\\s*\\d{1,3}(?:,[ \\t]+\\d{3})+(?!\\d)",
                x, perl = TRUE)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v) {
    if (length(v) == 0L) return(v)
    gsub(",[ \\t]+(\\d{3})", "\\1", v, perl = TRUE)
  })
  x
}

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
  # Pattern: ", FFFD = 0.04" or ", FFFD = 0.04, 90% CI" (pdftotext corrupts eta-squared (Greek eta + 2) to U+FFFD)
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

  # v0.7.6: DELETE the form feed. docpluck v2.4.136 stopped destroying page
  # boundaries -- its standalone-page-number strip was `^\s*\d{1,3}\s*$` under
  # MULTILINE, and that whitespace class matches U+000C, so the rule consumed the
  # page break standing beside the number it deleted. Correct of them to fix; it
  # means form feeds now arrive in text where they never used to (+1..+17 per
  # paper, 46% of documents).
  #
  # A form feed was never invisible here -- PCRE `\s` matches it, so the sentence
  # chunk boundary already split on one and the `=`-joiner at ~line 900 already
  # bridged across one. What it was NOT is a PARAGRAPH: the rules written with
  # `[ \t]` or `(^|\n)` -- the standalone-page-number strip, the section-number
  # strips, and chunk-boundary alternative 2 -- cannot match it. So a page break
  # read as a line wrap to the joiner and as nothing at all to the splitter.
  #
  # MEASURED, not reasoned about (2026-08-21, through check_text):
  #   * two results separated by "\n\f\n" collapsed to ONE row carrying the first
  #     result's d against the second's t -- a pairing that appears in no paper,
  #     and the exact defect v0.7.4 shipped to remove;
  #   * normalize_text("... dz =\n\f3\n\n...") produced "dz = 3", adopting a PAGE
  #     NUMBER as an effect size -- the v0.6.20 bridging class that the strip
  #     below exists to prevent. After this deletion it does not.
  #
  # DELETION, not translation. `\f -> \n` was measured and rejected: it turns the
  # corpus-majority "\n\f" into "\n\n" and manufactures a chunk split that never
  # existed, and a cross-model reviewer independently warned it also opens new
  # cross-page joins. `\f -> \n\n` (a hard page boundary) is a real option but a
  # different feature -- it would sever a statistic from its own effect size
  # across a page break, which is the counterexample class that killed the v0.7.4
  # general blank-line rule. It needs its own corpus measurement, not a free ride
  # on this fix. Deletion restores exactly the text shape every rule below was
  # written and measured against.
  #
  # Placed HERE, before the whitespace collapse and both number strips, so every
  # rule downstream sees pre-v2.4.136 text. Note this also fixes the `trimws()`
  # gap: R's default whitespace class is "[ \t\r\n]" and does NOT strip a form
  # feed, so a form-feed-only chunk used to survive the empty-chunk filters and
  # shift every `location` ordinal -- which MetaESCI joins on.
  x <- gsub("\f", "", x, fixed = TRUE)

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
  #
  # v0.7.6: the leading `(?<![A-Za-z])` is the whole point of this edit. Without
  # it ANY word ending in "F" supplied the F -- docpluck reported, and this was
  # reproduced verbatim, that
  #     "Patients with AF [6, 7], were excluded"
  # became "Patients with AF(6, 7)," -- an abbreviation followed by a CITATION
  # BRACKET rewritten into F-test notation, from which a consumer can report an
  # F(6, 7) that appears nowhere in the paper. Fabricating a test out of prose
  # is worse than missing one.
  #
  # A real F statistic is never preceded by a letter (it opens a sentence, or
  # follows a space, comma or bracket), so the guard costs no coverage.
  x <- gsub("(?<![A-Za-z])F\\s*\\[\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*(\\d+(?:\\.\\d+)?)\\s*\\]",
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
  #
  # v0.7.6: the `_?` after the 2 admits `eta2_p`, docpluck's SYMBOL CONTRACT v2.0
  # spelling (shipped v2.4.130, live ~2026-08-14), which `_`-joins every subscript
  # run. This list already carried the underscore forms `eta_p2` / `eta_p^2`, so
  # the shape was expected -- the one spelling docpluck actually emits was the one
  # missing. Until this was added, `eta2_p = .11` bound NOTHING: the row published
  # `effect_reported = NA` with status OK, where the identical `eta2p = .11` scored
  # ERROR. A dropped effect size that also turns the verdict green is the worst
  # shape this package can ship -- a green result from an empty input is a false
  # green. Scoped to an optional underscore on THIS token rather than a general
  # `_`-eater, so no unrelated identifier changes meaning.
  x <- gsub("(?:eta2_?p|\u03b72_?p|etap2|\u03b7p2|eta_p2|eta_p\\^2|\u03b7_p2|\u03b7_p\\^2)\\s*=", "partial eta-squared =", x, perl = TRUE)
  # Also handle n2p (PDF corruption of eta2p) but only if followed by = and a number
  x <- gsub("(?<![a-zA-Z])n2p\\s*=\\s*(\\d)", "partial eta-squared = \\1", x, perl = TRUE)
  # v0.3.0a: omega2p / omegap2 notation -> partial omega-squared = value
  # v0.7.6: `_?` admits docpluck symbol contract v2.0's `omega2_p` -- same change
  # and same reason as the partial-eta line above. This alternation carried NO
  # underscore variant at all, so the omega case was strictly worse than the eta
  # case: `omega2_p = .09` published effect_reported = NA at status OK where
  # `omega2p = .09` scored WARN.
  x <- gsub("(?:omega2_?p|\u03c92_?p|omegap2|\u03c9p2)\\s*=", "partial omega-squared =", x, perl = TRUE)
  # Superscript 2 (U+00B2) to caret notation (e.g., chi squared, eta squared)
  x <- gsub("\u00B2", "^2", x)

  # Greek letter + regular digit 2 (pdftotext -enc UTF-8 output):
  # Greek eta2 -> eta-squared, omega2 -> omega-squared, epsilon2 -> epsilon-squared
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
  # v0.7.2: replaced by the generic rule T1 below. The four narrow context
  # whitelists that used to live here (N/n, t/H/r/Z df, F df, chi-square inline
  # N) were an enumeration, not a rule: every statistic outside the list had its
  # thousands separator turned into a decimal point. "U = 12,345" became 12.345
  # and published a rank-biserial correlation of 0.99938 where the truth was
  # 0.38275, with status OK. See inst/normalization-spec/SPEC.md.
  # Order matters, and each step earns its position:
  #   1. lift structural spans (lists, indices, DOIs) -- a comma there is a real
  #      comma and no numeric rule should see it;
  #   2. full notation (both marks present) -- self-identifying, needs no locale;
  #   3. thousands groups -- unambiguous by shape;
  #   4. decimal commas -- whatever is left.
  # Structural spans are restored after the decimal rule, further below.
  .prot <- .protect_structural(x)
  x <- .prot$x
  x <- .apply_full_notation(x)
  # Locale is inferred from the text this call receives -- a clause, a section
  # or a whole document. One decisive marker is enough; when there is none the
  # thousands default applies unchanged.
  .loc <- infer_numeric_locale(x)
  x <- .apply_thousands_protect(x, locale = .loc)

  # ============================================================================
  # Pre-strip thousands-separator commas inside test-statistic parentheses
  # Must run BEFORE decimal comma conversion to prevent t(2,758) -> t(2.758)
  # which would silently be parsed as Welch df=2.758 with nonsense N estimate.
  # (MetaESCI E8, 2026-04-11: one article dropped 47 rows to this bug.)
  #
  # v0.7.3: locale-gated, like T1 and D1. Cross-model audit caught that this
  # sibling rule was missed by the locale pass: inside a decisively European
  # document, "Welch's t(2,758) = 3.21" still had its comma stripped to an
  # integer df of 2758, discarding a legitimate Welch df of 2.758 -- which is
  # the E8 incident reopened through the one rule the pass did not reach.
  # ============================================================================
  for (.i in if (.locale_comma_unresolved(.loc)) integer(0) else 1:3) {
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
  # v0.7.2 (spec rule D1): EXACTLY ONE digit before the comma.
  #
  # This single constraint is what the old implementation got wrong. It allowed
  # `\d{1,3}` here and `[-+]?\d+` in the rule that followed, so "U = 12,345"
  # matched and became 12.345 -- a 1000x error that published a rank-biserial
  # correlation of 0.99938 where the truth was 0.38275, status OK. A European
  # decimal in real text has one integer digit ("0,05", "1,5", "9,81"); two or
  # more digits before a comma is a thousands group, which T1 has already
  # handled above. See inst/normalization-spec/SPEC.md rule D1.
  #
  # Lookbehind exclusions, each with a known failure it prevents:
  #   a-zA-Z  author affiliation superscripts  ("Braunstein1,3")
  #   ,       multi-affiliation runs           ("Wagner1,3,4")
  #   0-9     CI pairs and decimal lists       ("[0.45,0.89]")
  #   [ and ( tight df brackets                ("F[2,42]" -- MetaESCI D2)
  # The lookahead includes `,` -- a DIVERGENCE from docpluck A3, found by
  # effectcheck's own suite and filed back to docpluck (see SPEC.md rule D1).
  # A European paper writes "t(28) = 2,21, d = 0,45": the decimal is followed by
  # the list comma. docpluck's lookahead omits `,`, so it leaves "2,21" alone and
  # the parser reads 2. Admitting `,` is safe because the lookbehind already
  # excludes a preceding digit, which is what blocks CI pairs like "[0.45,0.89]",
  # and because T1 has already removed genuine thousands groups.
  # TWO lookbehinds, because a bare "[" exclusion is too blunt. "F[2,42]" is a df
  # pair that must survive; "CI [0,12, 0,78]" is two European decimals that must
  # convert -- and both are the shape `[d,dd`. What separates them is a CAPITAL
  # LETTER IMMEDIATELY before the bracket (docpluck's own T1 guard 4): a stat
  # bracket is written "F[", a CI bracket is written "CI [" with a space.
  #   (?<![a-zA-Z,0-9])   char before the digit: blocks affiliations, multi-
  #                       affiliation runs, and CI pairs like "[0.45,0.89]"
  #   (?<![A-Z][\[\(])    two chars back: blocks stat brackets "F[", "F(", "t("
  # The DISCRIMINATOR is the digit count AFTER the comma, not before it.
  # An earlier draft of this port required exactly one digit before ("(\d),"),
  # copying docpluck. That silently stopped converting European decimals with
  # two or more integer digits -- "M = 12,34", "t = 1234,56" -- which the code
  # this replaced handled correctly. A shape enumeration caught it.
  #
  # Because T1 above has already consumed every unambiguous thousands group,
  # anything still carrying a comma between digits is a decimal, so this rule can
  # be permissive on both sides. The lookbehinds remain the real guards.
  # "." joins the lookbehind exclusions. Without it, the negative boundary lets
  # the rule bite into an ALREADY-FORMED decimal: "[0.45,0.89]" matched "45,0"
  # (the following "." is not a digit, so `(?!\d)` passed) and produced
  # "0.45.0.89". A European decimal is never preceded by a period, so excluding
  # it costs nothing and closes the hole.
  #
  # Under a European locale the `d,ddd` shape is EXACTLY the one structure
  # cannot decide -- 1234 grouped, or 1.234 as a decimal. T1 already stepped
  # aside for it; D1 must too, or "leave it alone" silently becomes "call it a
  # decimal". Neither reading is knowable, so the token is preserved verbatim
  # and the row carries the ambiguity rather than a guess dressed as a value.
  #
  # v0.7.5: the same applies under `confidence == "conflict"`, and BOTH halves
  # are needed. T1 above now steps aside for a conflicted document, but if D1
  # still converted, "leave it alone" would become "call it a European decimal"
  # -- the opposite guess, equally unfounded. The token is preserved verbatim
  # only when neither rule touches it.
  .amb_shape <- "\\d{1,3},\\d{3}(?!\\d)"
  .eu_locale <- identical(.loc$decimal_mark, ",")
  .amb_locale <- .locale_comma_unresolved(.loc)
  # v0.7.6, two bounded corrections, both reported by docpluck and reproduced
  # here before being touched:
  #
  # (1) `%` JOINS THE LOOKBEHIND EXCLUSIONS. "The rate was ~25%6,28" became
  #     "%6.28" -- a percentage followed by CITATION SUPERSCRIPTS read as a
  #     decimal. A European decimal is never written immediately after a percent
  #     sign, so excluding it costs nothing and closes the hole. Same class as
  #     the letter and comma exclusions already here.
  #
  # (2) THE INTEGER PART IS CAPPED AT FOUR DIGITS. The SPEC says "exactly one
  #     digit before the comma" and the code said `(\d+)` -- unbounded -- so
  #     "The value was 9999999,1" became "9999999.1". The SPEC is too strict to
  #     ship (a European paper really does write "M = 12,34" and "t = 1234,56",
  #     and an earlier one-digit draft silently stopped converting those), but
  #     unbounded is plainly wrong: T1 above has already consumed every genuine
  #     thousands group, so a run of five or more digits before a comma is not a
  #     decimal under any reading. Four digits covers the real continental
  #     shapes and refuses the rest. The SPEC is updated to match this, rather
  #     than the code being crippled to match the SPEC -- see
  #     inst/normalization-spec/SPEC.md rule D1.
  .d1_pat <- if (.amb_locale) {
    paste0("(?<![a-zA-Z,0-9.%])(?<![A-Z][\\[\\(])",
           "(?!", .amb_shape, ")(\\d{1,4}),(\\d+)(?!\\d)")
  } else {
    paste0("(?<![a-zA-Z,0-9.%])(?<![A-Z][\\[\\(])(\\d{1,4}),(\\d+)(?!\\d)")
  }
  x <- gsub(.d1_pat, "\\1.\\2", x, perl = TRUE)

  # v0.7.4 (spec rule D1b): a European decimal with NO INTEGER PART.
  #
  # D1 above requires at least one digit before the comma -- `(\d+),(\d+)`. But
  # a continental paper omits the leading zero exactly as APA does: "p = ,025"
  # is how "p = .025" is written, and "p < ,001" is how "p < .001" is written.
  # Neither matched anything, so the p-value was SILENTLY DROPPED while the same
  # clause's t and d converted normally:
  #   "t(48) = 2,31, p = ,025, d = 0,74"  ->  p_reported = NA, row still WARN
  # and the fuller form with "p < ,001" returned ZERO rows. Found by the
  # cross-model review of v0.7.3 (Codex, 2026-08-09) and reproduced.
  #
  # The guard is the VALUE POSITION, not the locale: a comma sitting directly
  # after `=`, `<` or `>` with no space and no digits before it cannot be a list
  # separator or a thousands group -- there is nothing on its left to group. So
  # no locale inference is needed and none is used, which also means an English
  # document carrying a stray continental value is repaired rather than dropped.
  # Requiring NO space after the comma is what keeps list and CI shapes out:
  # "CI [0.45, 0.89]" and "F(1, 30)" both have one.
  x <- gsub("([=<>]\\s*),(\\d+)(?!\\d)", "\\1.\\2", x, perl = TRUE)

  # Structural spans go back untouched.
  x <- .restore_structural(x, .prot$store)

  # v0.7.2: the second decimal rule that used to sit here is DELETED, not
  # narrowed. It matched `([-+]?\d+),([0-9]+)` -- unlimited digits on both sides
  # -- which is the single widest hole in the old implementation: it turned
  # "BF10 = 1,234,567.89" into "1.234,567.89" (unparseable) and "SE = 1,234.5"
  # into "1.234.5" (unparseable). Rule D1 above, applied AFTER T1 has removed
  # genuine thousands groups, covers every European-decimal case the corpus
  # exercises. See inst/normalization-spec/conformance.json.

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
  # them from being captured as p-values when joined across line breaks.
  #
  # v0.6.20 (MetaESCI O-1, class A): the old pattern was `\d+(\.\d+)+\.?[ \t]+`,
  # which also matches a LINE-WRAPPED REPORTED VALUE -- "d =\n0.86 in the
  # treatment group" had "0.86 " stripped, so the row shipped
  # effect_reported = NA with status OK (a false all-clear), and
  # "p =\n0.037 for the interaction" lost its p-value. A section number is
  # distinguishable from a wrapped value by SHAPE: it carries either two or more
  # decimal groups ("3.3.1") or an explicit trailing period ("3.3."), whereas a
  # reported value is a single group with no trailing dot ("0.86", "0.037").
  # Requiring one of those two forms keeps every intended strip and stops the
  # rule from ever destroying a number. The genuinely ambiguous "3.3 Results"
  # form (single group, no trailing dot) is now KEPT: preferring a possible
  # section number over a possible reported value is the wrong trade on a
  # verification tool, and a p-value taking a section number is caught
  # downstream by the [0, 1] validation at the p_reported extraction.
  #
  # The leading `(?<![=<>])(?<![=<>][ \t])` is the second half of the fix, for a
  # case neither MetaESCI nor the cross-model review raised: a wrapped value that
  # ENDS A SENTENCE is shaped exactly like a section number, because the sentence
  # period supplies the trailing dot.
  #
  #   "..., d =\n0.86. In Study 2 we replicated it"
  #        -> "0.86. " matched `\d+(\.\d+)+\.[ \t]+` and was stripped, so the row
  #           shipped effect_reported = NA with status OK -- the silent-loss
  #           class again, from the opposite direction.
  #
  # A dangling assignment operator at the end of the previous line settles it:
  # nothing legitimately numbers a section immediately after "d =", and a value
  # is the only thing that can follow. Two fixed-width lookbehinds (PCRE allows
  # alternatives of differing fixed width) cover "d =" and "d = " alike; at
  # position 0 a lookbehind has nothing to match and correctly succeeds.
  x <- gsub("(?<![=<>])(?<![=<>][ \\t])(^|\\n)([ \\t]*)\\d+(?:\\.\\d+){2,}\\.?[ \\t]+",
            "\\1\\2", x, perl = TRUE)
  x <- gsub("(?<![=<>])(?<![=<>][ \\t])(^|\\n)([ \\t]*)\\d+(?:\\.\\d+)+\\.[ \\t]+",
            "\\1\\2", x, perl = TRUE)
  # Third form: a SINGLE-group number followed by a CAPITALISED word ("3.3
  # Discussion", "4.2 Results", "1.0 Method"). Shape alone cannot separate this
  # from a wrapped value -- "0.86" and "3.3" are the same shape -- so the
  # discriminator is what FOLLOWS. A section number introduces a heading, which
  # is capitalised; a wrapped value continues its sentence in lower case
  # ("0.86 in the treatment group") or terminates it with a period first
  # ("0.86. In Study 2"), and the required `[ \t]+` before the capital excludes
  # that second case because the period intervenes.
  #
  # Deliberately WITHOUT the dangling-operator lookbehind used above: this rule
  # exists precisely for the "d =\n3.3 Discussion" case, where the previous line
  # DOES end in an operator. Found by a cross-model adversarial review of the
  # first version of this fix, and reproduced at HEAD before being acted on --
  # the surviving path shipped d = 3.3, which check.R's decimal-recovery step
  # then silently rewrote to 0.33 and framed as a rounding discrepancy. Two
  # fabricated values in sequence, neither of them in the paper.
  #
  # It also settles the "p =\n1.0 Results" case raised independently by the
  # other reviewer, which the earlier draft had accepted as a residual.
  #
  # v0.7.6: the capital letter alone was NOT a sufficient discriminator, and
  # the counterexample is the worst class this package has. docpluck reported,
  # and this was reproduced verbatim, that a flattened table row
  #
  #     "90.6 Third-plus generation had lower rates of reported discrimination."
  #
  # lost its "90.6 " -- a PUBLISHED VALUE DELETED WITH NO RESIDUE, along with
  # "0.01", "0.00" and "0.03" in the same paper. The row shape is identical to
  # "3.3 Discussion": line-initial single-group number, space, capital.
  #
  # What separates them is not the number, it is WHAT FOLLOWS. A section
  # heading is a short noun phrase with no sentence punctuation; a table row
  # continues into a sentence and terminates with a period. So the rest of the
  # line must now contain NO period and be at most 60 characters.
  #
  # A leading zero is also refused outright: nothing numbers a section "0.01".
  #
  # The residual cost is a heading that contains a period ("Study 1. Results")
  # surviving as text -- cosmetic. The cost it replaces was deleting a number
  # the paper printed, which is unrecoverable and silent. On a verification
  # tool that trade is not close.
  x <- gsub("(^|\\n)([ \\t]*)[1-9]\\d*\\.\\d+[ \\t]+(?=[A-Z][^\\n.]{0,59}(?:\\n|$))",
            "\\1\\2", x, perl = TRUE)

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
  # v0.3.0d fix: old (?![.0]?\d) failed on "0.001" -- [.0]? ate '0', then \d couldn't match '.'
  # Fix uses [ \t]*+ (possessive horizontal whitespace) after [<=>] to prevent two bugs:
  # 1. Backtracking: \s* would backtrack past space, lookahead sees space not digit, fires
  # 2. Newline eating: \s*+ would consume \n, leaving nothing for the \n literal in pattern
  # v0.6.20 (MetaESCI O-1/O-2, class A): the skipped span is DIGIT-FREE
  # ([^\n\d]), so this rule can never discard a number that is already present.
  # It previously skipped `[^\n]{0,50}`, which happily swallowed real reported
  # statistics -- "p < ns although F(1,20) = 3.1\n2 participants" collapsed to
  # "p < 2", deleting the F-test. See the shared rationale block at the
  # `[a-z]+ =` bridge below.
  # The adopted number must carry a DECIMAL POINT -- see the second invariant in
  # the `[a-z]+ =` block below. A bare integer opening a line after a dangling
  # "p =" is a page or section number, never a p-value.
  x <- gsub("(p\\s*[<=>][ \\t]*+)(?!\\d|[.]\\d)([^\\n\\d]{0,50})\\n\\s*(\\d*\\.\\d+)", "\\1\\3", x, perl = TRUE)

  # Fix line breaks between test statistic and p-value
  # Pattern: "t(df) = value,\n p = value" -> "t(df) = value, p = value"
  x <- gsub("([,;])\\s*\\n\\s*(p\\s*[<=>])", "\\1 \\2", x, perl = TRUE)

  # Fix line breaks between effect size and CI
  # Pattern: "d = value,\n 95% CI" -> "d = value, 95% CI"
  x <- gsub("([,;])\\s*\\n\\s*(\\d+%\\s*CI)", "\\1 \\2", x, perl = TRUE)

  # Fix line breaks in effect size assignments
  # Pattern: "f = \n0.01" or "d = \n0.80" -> "f = 0.01" or "d = 0.80"
  # Allow optional non-numeric text between = and number (up to 30 chars)
  #
  # ==========================================================================
  # v0.6.20 (MetaESCI O-1/O-2) -- THE INVARIANT FOR EVERY LINE-WRAP BRIDGE:
  #
  #   A bridging rule may JOIN a wrapped number to its label. It must NEVER
  #   DELETE a number that is already present.
  #
  # The skipped span must therefore be digit-free. `[^\n\d]{0,30}` also encodes
  # the precondition directly: if every character between the `=` and the line
  # break is a non-digit, then this assignment genuinely has no value on its own
  # line, which is the only situation the rule exists to repair.
  #
  # The old class was `[^\n]{0,30}`, with no guard of any kind, and it was the
  # single root cause of BOTH MetaESCI O-1 and O-2 (they were filed as separate
  # defects with separate diagnoses; both traced here):
  #
  #   "etap2 = .86, and Experiment\n1b"      -> "partial eta-squared = 1"
  #        (the eta rewrite above turns this into "...squared = ", whereupon
  #         `[a-z]+` matches "squared" and ".86, and Experiment" is discarded)
  #   "d = 0.74 (see Table\n2)"              -> "p = 2)"
  #   "chi2 (4, n = 211) = 12.74, p = .013\n\n10 items" -> "chi2 (4, n = 10 items"
  #        (this one destroys the chi-square token itself, hence "0 rows")
  #   "t(48) = 2.31, p = .025, d = 0.65\n\n10 items"    -> "t(48) = 2.31, p = 10"
  #   "r(351) = .164, p = .050\n\n10 items"             -> "r(351) = .164, p = 10"
  #
  # Note what the last two show: the t and r cases were filed as *controls* that
  # "parse fine" because they still return one row. They do -- carrying a
  # fabricated p-value. Row count is not a sufficient probe for this class.
  #
  # Note also that the label vocabulary (Table / Figure / Study / Experiment)
  # reported alongside O-1 is incidental. The rule keys on `[a-z]+ =` and a
  # wrapped digit; ANY intervening prose triggers it.
  #
  # SECOND INVARIANT (added after a cross-model review of the first fix found
  # three surviving paths, all reproduced locally before being acted on):
  #
  #   When prose is skipped, the adopted number must carry a DECIMAL POINT.
  #
  # Digit-freeness alone still let the rule reach ACROSS prose and adopt a bare
  # integer that opens the next line -- which is a page number, a list marker or
  # a section number, essentially never a statistic:
  #
  #   "d = see Table\n1 for means"  -> "d = 1"   (shipped effect_reported = 1)
  #   "p = ns\n1 Results"           -> "p = 1"   (shipped p_reported = 1, p_valid TRUE)
  #
  # Values in APA prose are written with a decimal point (.80, 0.037, -0.34), so
  # requiring one costs nothing real. The genuinely BARE case -- "d =" at the very
  # end of a line, where an integer continuation such as "n =\n120" IS legitimate
  # -- is unaffected: it is already joined upstream by the `([=<>])\s*\n\s*` rule,
  # which only removes whitespace and can never discard text.
  # ==========================================================================
  x <- gsub("([a-z]+\\s*=\\s*)[^\\n\\d]{0,30}\\n\\s*([-+]?\\d*\\.\\d+)", "\\1\\2", x, perl = TRUE)

  # Fix cases where p-value pattern got broken: "p = text" followed by number on next line
  # More aggressive: look for "p = " followed by non-numeric text, then newline, then number
  # v0.6.20 (MetaESCI O-1/O-2, class A): digit-free skip span per the invariant
  # above, and BOUNDED. The old `[^\n]*` was unbounded, so a single match could
  # discard an entire line of statistics -- "p = ns, t(20) = 2.51, d = 0.55, 95%
  # CI [0.1, 1.0]\n10 items" collapsed to "p = 10", taking the t-test, the effect
  # size and the interval with it.
  x <- gsub("(p\\s*=\\s*)[a-zA-Z][^\\n\\d]{0,100}\\n\\s*(\\d*\\.\\d+)", "\\1\\2", x, perl = TRUE)

  # Fix orphaned p-values: Look for "p = [text]" followed by newline and a number
  # Replace the text with the number: "p = on social distance\n0.837" -> "p = 0.837"
  # This is more aggressive and handles OCR errors where p-value got separated
  # v0.6.20: decimal point required, per the second invariant above.
  x <- gsub("(p\\s*=\\s*)[^\\d\\n]{1,100}\\n\\s*(\\d*\\.\\d+)(?=\\s*[,;]|\\s*$)", "\\1\\2", x, perl = TRUE)

  # General mid-sentence line-break joining (lowercase to lowercase across newlines)
  # Runs after stat-specific joins so those get priority
  x <- gsub("([a-z,;])\\s*\\n\\s*([a-z])", "\\1 \\2", x, perl = TRUE)

  # ============================================================================
  # Dropped decimal fixes (v0.2.5)
  # PDF extraction sometimes drops the leading "0." from decimal values at page
  # boundaries, producing e.g. "p = 484" instead of "p = .484"
  # ============================================================================

  # Fix "p < 001" -> "p < .001" (missing dot before 001)
  # This is always an artifact -- "001" is never a valid p-value representation
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

#' Pick the document-level fallback sample size from all `N = ...` candidates
#'
#' Used only as the LAST resort, when a statistic's own clause, its local
#' context, and its extended context all fail to supply an N.
#'
#' The rule is "most frequently mentioned N, else the largest". The frequency
#' branch encodes a real signal -- a study total gets restated across the
#' methods and results -- but it is only meaningful when one value is actually
#' more frequent than the rest.
#'
#' v0.6.17: a TIED top frequency is broken among the TIED VALUES (largest of
#' them), never by falling through to the global maximum.
#'
#' `table()` orders its counts by ascending numeric name and `which.max()`
#' returns the first maximum, so a tie previously resolved to the SMALLEST tied
#' candidate. On 10.1016/j.jesp.2009.12.010 every candidate tied at frequency 2
#' (7, 13, 25, 31, 38 -- each twice, all cells of one accepters/rejecters
#' subgroup table), so the paper's global N became 7, its smallest subgroup
#' cell, and the Study 2 mediation z rows published r_from_z = 0.7341 and
#' d = 2.162 against a true N of 76 (0.312 and 0.328).
#'
#' Taking the largest of the TIED values -- rather than `max(ns)` over every
#' candidate -- matters, and an intermediate version of this fix that escaped to
#' `max(ns)` was caught by the corpus diff doing real damage: on
#' 10.1525/collabra.32572 the candidates are a tight cluster
#' (273 x3, 274 x4, 275 x4, 276 x2, 277 x2, 279) plus a lone 3302 outlier. With
#' 274 and 275 tied at the top, escaping to the maximum handed every F row
#' N = 3302 -- an order of magnitude above the study's real 999 (per gold) and
#' far worse than the 274 the old rule picked. A tie means the top candidates
#' are equally attested; it is not licence to prefer an unrelated number that
#' was mentioned once.
#'
#' KNOWN TRADE-OFF (cross-model review, codex 2026-08-04, reproduced): in a
#' multi-study paper whose per-study and pooled Ns are each mentioned equally
#' often -- e.g. Study 1 N=40 x2, Study 2 N=60 x2, pooled N=100 x2 -- this rule
#' returns the pooled 100, where the old rule returned 40. Neither is right in
#' general: for a row with no local N belonging to one of the three, each rule
#' scores exactly 1 of 3. The tie-break is chosen on the DIRECTION of its error,
#' not its rate. Effect sizes scale as 1/sqrt(N), so a too-SMALL N inflates the
#' computed effect and manufactures a false discrepancy against a correctly
#' reported paper, while a too-LARGE N attenuates it toward agreement. For a
#' consistency checker the first is the more damaging failure. The companion
#' df-authority override in check.R then rejects an over-large N outright on
#' every non-Welch t row, which the old rule's under-estimates could never be
#' caught by.
#'
#' Note this returns a best-effort fallback, not a verified N: callers that act
#' on it must mark provenance (`N_source = "global_text"`) so downstream checks
#' can surface the uncertainty to the user. Where a df is available it is
#' structurally authoritative and overrides this value entirely (check.R).
#'
#' @param ns Numeric vector of candidate sample sizes (already positive, non-NA)
#' @return A single numeric N, or `NA_real_` when there are no candidates
#' @keywords internal
global_n_from_candidates <- function(ns) {
  ns <- ns[!is.na(ns) & ns > 0]
  if (length(ns) == 0L) {
    return(NA_real_)
  }
  n_counts <- table(ns)
  top <- max(n_counts)
  if (top > 1L) {
    # One or more values are the most-attested. Prefer the largest of THEM: a
    # study total outranks a subgroup of itself, and both are better attested
    # than any once-mentioned number elsewhere in the document.
    max(as.numeric(names(n_counts)[n_counts == top]))
  } else {
    # Every candidate is unique -- no popularity signal at all. Take the
    # largest, which is the most likely total sample.
    max(ns)
  }
}

#' The `N = <int>` / `nobs = <int>` sample-size token
#'
#' v0.6.18: hoisted from a `parse_text()` local so `.doc_global_n()` (shared
#' with `check_text()`) uses the SAME pattern -- one definition, no drift
#' (the v0.5.9 chi_tok lesson: a token duplicated across sites diverges).
#' A lowercase bare `n = X` is deliberately NOT matched -- it is commonly a
#' per-group size (see the scoped v0.5.8 chi-square exception).
#' @keywords internal
.pat_doc_N <- "\\b(?:N|nobs)\\s*=\\s*(\\d[\\d,]*\\d|\\d+)"

#' Document-level N from normalized text
#'
#' v0.6.18: the single shared computation of the document-global sample size
#' (every `N = <int>` / `nobs = <int>` match in the text, resolved by
#' `global_n_from_candidates()`). Extracted from `parse_text()` so
#' `check_text()` can offer the same value to docpluck TABLE rows -- an
#' attribute on `parse_text()`'s return value was tried first and silently
#' vanished on the zero-statistics early-return paths, exactly the class of
#' seam bug a shared helper cannot have.
#'
#' @param text_normalized The full document text, already `normalize_text()`d
#' @return A single numeric N, or `NA_real_`
#' @keywords internal
.doc_global_n <- function(text_normalized) {
  m <- stringr::str_match_all(text_normalized, .pat_doc_N)
  if (length(m[[1]]) > 0) {
    ns <- as.numeric(gsub(",", "", m[[1]][, 2]))
    ns <- ns[!is.na(ns) & ns > 0]
    global_n_from_candidates(ns)
  } else {
    NA_real_
  }
}

#' Spelled-out integers admissible as a resample count
#'
#' v0.7.5. Bounded on purpose: only `<unit> <scale>` compounds, no general
#' English-number grammar. The corpus supplies exactly one shape --
#' `"For permutation tests, ten thousand random shuffles of labels ... were
#' sampled"` (PNAS 10.1073/pnas.2404157121) -- and a general number-word reader
#' would be a large surface for the benefit of one form. Everything here must be
#' a value an author would plausibly choose for B.
#' @keywords internal
.RESAMPLE_WORD_UNITS <- c(
  one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7,
  eight = 8, nine = 9, ten = 10, twenty = 20, thirty = 30, forty = 40,
  fifty = 50, sixty = 60, seventy = 70, eighty = 80, ninety = 90
)

#' @rdname dot-RESAMPLE_WORD_UNITS
#' @keywords internal
.RESAMPLE_WORD_SCALES <- c(hundred = 100, thousand = 1000, million = 1e6)

#' Resample-count declarations, as one shared definition
#'
#' v0.7.5. Used by BOTH the per-clause scan in `parse_text()` and the
#' document-level Methods prescan `.doc_resampling_b()`. One definition, no
#' drift -- the v0.5.9 `chi_tok` lesson, where a token duplicated across four
#' parse sites silently diverged and one copy stopped recognising a form the
#' others accepted.
#'
#' Returns `NA_real_` unless the string yields a plausible B (>= 50; below that
#' the clause is far likelier to be a sample size or an item count than a
#' resampling specification).
#'
#' THE SEPARATOR HANDLING IS LOAD-BEARING. `normalize_text()` has already run
#' its decimal-comma conversion, so `"10,000 permutations"` arrives as
#' `"10.000 permutations"` and `numify_int("10.000")` is 10. A resample count is
#' always an integer, so EVERY "." and "," inside it is a thousands separator:
#' strip them all rather than parse the number as written. Taking it at face
#' value would set B = 10, a floor of 1/11 = .09, and false-flag essentially
#' every permutation p in the corpus.
#'
#' WHAT IS DELIBERATELY REFUSED. A bare `<n> samples` / `<n> draws` never
#' counts, even inside a resampling sentence. Cross-model review (2026-08-07,
#' reproduced) showed `"Across 500 samples, a permutation test with 10,000
#' permutations ..."` binding B = 500 and then FALSE-FLAGGING the p as below
#' `1/(B+1) = 0.002` -- a wrong accusation built on a count scraped from the
#' wrong clause. `iterations` / `replications` / `simulations` are admitted with
#' a qualifier because they are not participant words; `samples` and `draws`
#' are, so they stay out.
#'
#' @param s A single string -- a clause, or one Methods sentence
#' @return A single numeric B, or `NA_real_`
#' @keywords internal
.resample_count_in <- function(s) {
  if (is.null(s) || length(s) == 0L || is.na(s) || !nzchar(s)) return(NA_real_)

  num <- "(\\d[\\d,.]*\\d|\\d)"
  qual <- "(?:random|bootstrap\\w*|permutation|permuted|monte[- ]?carlo|resampl\\w*|shuffl\\w*)"

  # A resample count is a positive INTEGER, so every "." and "," inside the
  # matched literal is a thousands separator and gets stripped. That rule is
  # correct and necessary (see the header), and it is also a loaded gun: applied
  # to a DECIMAL it manufactures a large integer out of a small fraction. Caught
  # in the corpus by this prescan's own first draft, which read
  #   "(b=0.81, z=2.80, p=0.005, OR=2.25, CI 1..." (brjpsych_1)
  # as B = 81 -- a regression coefficient becoming a Monte-Carlo floor of
  # 1/82 = 0.0122, which would have falsely flagged every p below .0122 in that
  # paper as unattainable. So a literal only counts when its SHAPE is integral:
  # bare digits, or digit groups where every group after the first is exactly
  # three wide. "10.000" and "5,000" pass; "0.81" and "2.25" cannot.
  integral_shape <- "^\\d+$|^\\d{1,3}(?:[,.]\\d{3})+$"
  # Nouns that MEAN a resample. No qualifier needed.
  noun_specific <- "(?:permutations?|resamples?|replicates?|bootstraps?|shuffles?|permutation\\s+samples?)"
  # Nouns that mean a resample only in context, and are not participant words.
  noun_qualified <- "(?:iterations?|replications?|simulations?|samples?|draws?)"
  # ... and the subset admissible when the qualifier is elsewhere in the
  # sentence rather than adjacent. `samples`/`draws` are excluded here: that is
  # the exact form the 2026-08-07 false accusation came in.
  noun_loose <- "(?:iterations?|replications?|simulations?)"

  word_unit <- paste0("(?:", paste(names(.RESAMPLE_WORD_UNITS), collapse = "|"), ")")
  word_scale <- paste0("(?:", paste(names(.RESAMPLE_WORD_SCALES), collapse = "|"), ")")

  # A resampling word anywhere in the string licenses the loose noun form.
  # "cluster-based permutation testing (1000 iterations at a threshold of 0.05)"
  # (eLife 10.7554/eLife.87747) states its B in a parenthesis whose noun is
  # generic and whose qualifier sits before the bracket, so no adjacency rule
  # can reach it.
  resamp_word <- paste0("\\b(?:permut\\w*|resampl\\w*|bootstrap\\w*|monte[- ]?carlo|",
                        "shuffl\\w*|randomi[sz]ation)\\b")
  has_resampling_word <- grepl(paste0("(?i)", resamp_word), s, perl = TRUE)

  # The qualifier is OPTIONAL before an unambiguous resample noun: the PNAS
  # declaration is "ten thousand RANDOM shuffles", with the qualifier sitting
  # between the count and its noun. Requiring adjacency there missed the one
  # paper this whole prescan exists for.
  digit_pats <- c(
    # `B = <n>` is the one form with NO noun to anchor it, so it carries three
    # guards instead of one. All three were earned against a real corpus string:
    #   * case-SENSITIVE uppercase B -- lowercase `b =` is the unstandardized
    #     regression coefficient, in nearly every paper in the corpus;
    #   * the integral-shape guard above -- it is what rejects `b=0.81`, which
    #     the first draft of this function read as B = 81 (brjpsych_1);
    #   * a resampling word required in the SAME sentence -- without it,
    #     "Grade A+=80% or above, A=70-79%, B=60-69%, C=50-59%" binds B = 60
    #     off a GRADING SCALE (bmcpsych_cbt_burnout_2025, measured). A grading
    #     scale in a Methods section is entirely ordinary, so the positional
    #     scope does not protect against this one -- only the semantic guard.
    # A floor of 1/61 would have called every p below .0164 in that paper
    # unattainable. Every guard here exists to prevent a FALSE ACCUSATION
    # against a correctly reported p-value.
    # ... and a FOURTH guard: nothing else may claim the number. A bare `B = n`
    # is a resample count only when no noun follows it. Cross-model review
    # (Codex/gpt-5.5, 2026-08-09, REPRODUCED) found
    #   "Bootstrap analyses were not used; vitamin B = 60 mg was administered."
    # binding B = 60 -- the sentence-level resampling word is satisfied by a
    # NEGATED mention, and `vitamin B` is not a resample count in any reading.
    # Detecting negation is fragile; requiring the number to stand alone is not.
    # This refuses "B = 60 mg", "B = 60 patients", "B = 60 years" and accepts
    # "B = 10000," / "B = 10000." / "B = 10000)". A genuine "B = 10,000
    # permutations" is refused HERE and caught by the noun-anchored pattern
    # below, which is the stronger evidence anyway.
    # The trailing guard refuses a letter AND a range/unit continuation. Both
    # halves were earned: `-` catches the UNSPACED grading scale "B=60-69%",
    # which the space-and-letter form alone let through (cross-model review,
    # Claude Sonnet, 2026-08-09, REPRODUCED -- it bound B = 60 exactly as the
    # spaced form once did).
    if (has_resampling_word) {
      paste0("\\bB\\s*=\\s*", num, "\\b(?!\\s*[-/%]|\\s+[A-Za-z])")
    },
    # EVERY branch requires a resampling word in the sentence, including this
    # one. `noun_specific` was ungated on the theory that its nouns are
    # unambiguous; `replicates` is not. Cross-model review (Claude Sonnet,
    # 2026-08-09, REPRODUCED): "Each condition was tested with 60 replicates."
    # -- an ordinary wet-lab sentence with no resampling anywhere -- bound
    # B = 60, and in a Methods section that becomes the document-level count,
    # producing a floor of 1/61 = .0164 that would falsely flag any correctly
    # reported permutation p below .0164 elsewhere in the same paper.
    #
    # The gate costs nothing on the genuine cases because the unambiguous nouns
    # SATISFY IT THEMSELVES: "permutations", "resamples", "shuffles" and
    # "bootstrapped" all match `resamp_word`. Only `replicates` did not -- which
    # is precisely the word that should not have been in this list.
    if (has_resampling_word) paste0("(?i)\\b", num, "\\s+(?:", qual, "\\s+)?", noun_specific, "\\b"),
    if (has_resampling_word) paste0("(?i)\\b", num, "\\s+", qual, "\\s+", noun_qualified, "\\b"),
    # The loose noun requires the resampling word to come BEFORE the number and
    # nearby -- not merely somewhere in the same sentence. Without the ordering
    # and distance constraint, "We enrolled 240 iterations of the survey; a
    # permutation test followed later." bound B = 240 (same review, REPRODUCED).
    # The real case this alternative exists for keeps working because its
    # qualifier does precede its count: "cluster-based permutation testing
    # (1000 iterations at a threshold of 0.05)".
    if (has_resampling_word) {
      paste0("(?i)\\b", resamp_word, "[^.;]{0,40}?\\b", num, "\\s+", noun_loose, "\\b")
    }
  )
  for (bp in digit_pats) {
    m <- stringr::str_match(s, bp)
    if (!is.na(m[1, 2]) && grepl(integral_shape, m[1, 2])) {
      b_val <- suppressWarnings(as.numeric(gsub("[,.]", "", m[1, 2])))
      if (!is.na(b_val) && b_val >= 50) return(b_val)
    }
  }

  # Spelled-out counts. Same noun requirements as the digit forms above, so
  # "ten thousand participants" can never become a resample count.
  word_pats <- c(
    paste0("(?i)\\b(", word_unit, ")\\s+(", word_scale, ")\\s+(?:", qual, "\\s+)?", noun_specific, "\\b"),
    paste0("(?i)\\b(", word_unit, ")\\s+(", word_scale, ")\\s+", qual, "\\s+", noun_qualified, "\\b"),
    if (has_resampling_word) paste0("(?i)\\b(", word_unit, ")\\s+(", word_scale, ")\\s+", noun_loose, "\\b")
  )
  for (wp in word_pats) {
    m <- stringr::str_match(s, wp)
    if (!is.na(m[1, 2])) {
      b_val <- .RESAMPLE_WORD_UNITS[[tolower(m[1, 2])]] * .RESAMPLE_WORD_SCALES[[tolower(m[1, 3])]]
      if (!is.na(b_val) && b_val >= 50) return(as.numeric(b_val))
    }
  }

  NA_real_
}

#' Section headings that open a Methods / Analysis region
#' @keywords internal
.pat_methods_heading <- paste0(
  "(?im)^[ \\t]*(?:\\d+[.)]?[ \\t]*)?(?:materials?\\s+and\\s+methods?|methods?|",
  "statistical\\s+analys[ei]s|data\\s+analys[ei]s|statistical\\s+approach|",
  "analytic\\s+(?:strategy|plan)|analysis\\s+plan)[ \\t]*:?[ \\t]*$"
)

#' Section headings that CLOSE a Methods / Analysis region
#'
#' Any heading NOT in this list leaves the Methods region open, so an omission
#' silently widens the scope -- the failure direction this prescan exists to
#' avoid. Cross-model review (Codex/gpt-5.5, 2026-08-09, REPRODUCED) found
#' `Appendix` missing: a `Methods` heading followed later by an `Appendix`
#' section let a count from the appendix bind as the document-level B.
#' @keywords internal
.pat_other_heading <- paste0(
  "(?im)^[ \\t]*(?:\\d+[.)]?[ \\t]*)?(?:results?|discussion|introduction|abstract|",
  "references?|bibliography|conclusions?|acknowledge?ments?|",
  "supplementary\\s+\\w+|general\\s+discussion|appendix(?:\\s+\\w+)?|appendices|",
  "supporting\\s+information|data\\s+availability|",
  "(?:author\\s+)?contributions?|funding|conflicts?\\s+of\\s+interest|",
  "competing\\s+interests?|limitations?|ethics(?:\\s+statement)?)[ \\t]*:?[ \\t]*$",
  # ... and ANY heading-like line, whatever it is called.
  #
  # A vocabulary is the wrong shape for a CLOSING list: every heading missing
  # from it silently WIDENS the Methods region, which is the failure direction
  # this prescan exists to avoid. Cross-model review (Claude Sonnet, 2026-08-09,
  # REPRODUCED): a subsection heading left the region open and a count after it
  # bound as the document-level B.
  #
  # It must be STRUCTURAL rather than numeric, because `normalize_text()` has
  # already stripped section numbers by this point -- "3.1 Sample
  # Characteristics" arrives as "Sample Characteristics", so a rule anchored on
  # the numbering can never fire. (Verified, not assumed: a first draft matched
  # on `\\d+(\\.\\d+)*` and the leak persisted.)
  #
  # A heading here is a SHORT STANDALONE LINE: blank line on both sides, starts
  # with a capital, no sentence-ending punctuation, and no "=" so a displayed
  # statistic is never mistaken for one. Closing the region EARLY is the safe
  # error -- it narrows scope, and the worst case is the status quo of not
  # finding a B at all.
  "|(?m)(?<=\\n\\n)[ \\t]*[A-Z][^=.!?\\n]{0,60}(?=\\n\\n)"
)

#' Document-level resample count, scoped to the Methods / Analysis section
#'
#' v0.7.5 (handoff Issue C). Authors declare B ONCE, in Methods, and never
#' restate it beside each reported p -- so the per-clause scan populated
#' `resampling_B` for **zero rows in the entire 48-paper validation corpus**,
#' and the Monte-Carlo floor check shipped in v0.6.22 (`p >= 1/(B+1)`,
#' Phipson & Smyth 2010) never fired once. A check that cannot fire is
#' indistinguishable from one that passes.
#'
#' WHY NOT A WIDER CONTEXT WINDOW. That is how the v0.6.18 Welch N-leak
#' happened: a neighbouring sentence's "Welch's" leaked onto a paired `t(131)`
#' and N went 132 -> 403. A document-level default with its OWN provenance
#' string is a different mechanism -- the consumer can see where the number came
#' from and discount it -- and it is scoped twice over:
#'
#'   1. POSITIONALLY, to text inside a Methods / Analysis section. A count in
#'      Results or in a Discussion citing another study never binds.
#'   2. SEMANTICALLY, to a sentence that itself names the resampling procedure.
#'      `.resample_count_in()` enforces the noun and qualifier requirements.
#'
#' When no Methods heading is detectable the function returns `NA_real_` rather
#' than falling back to a whole-document scan. A silent widening of scope on the
#' documents where scoping is hardest is precisely the wrong failure direction:
#' a wrong B produces a wrong FLOOR, which is a false accusation against a
#' correctly reported p-value.
#'
#' @param text_normalized The full document text, already `normalize_text()`d
#' @return A single numeric B, or `NA_real_`
#' @keywords internal
.doc_resampling_b <- function(text_normalized) {
  if (is.null(text_normalized) || length(text_normalized) == 0L ||
      is.na(text_normalized) || !nzchar(text_normalized)) {
    return(NA_real_)
  }

  starts <- stringr::str_locate_all(text_normalized, .pat_methods_heading)[[1]]
  if (nrow(starts) == 0L) return(NA_real_)
  closers <- stringr::str_locate_all(text_normalized, .pat_other_heading)[[1]]
  total <- nchar(text_normalized)

  for (i in seq_len(nrow(starts))) {
    from <- starts[i, "end"] + 1L
    # A Methods region runs to the next section heading of ANY kind. Papers
    # order sections differently -- this PNAS puts Materials and Methods AFTER
    # Discussion -- so "first Methods to first Results" is not a usable rule.
    after <- c(closers[closers[, "start"] > from, "start"],
               starts[starts[, "start"] > from, "start"])
    to <- if (length(after)) min(after) - 1L else total
    if (to <= from) next

    region <- substr(text_normalized, from, to)
    # One sentence at a time: the count and the resampling word must co-occur in
    # the SAME sentence, never merely in the same section.
    for (sentence in unlist(strsplit(region, "(?<=[\\.!?])\\s+", perl = TRUE))) {
      b_val <- .resample_count_in(sentence)
      if (!is.na(b_val)) return(b_val)
    }
  }
  NA_real_
}

#' Count decimal places in the raw matched string
#'
#' Counts trailing digits after the decimal point in a numeric string,
#' preserving trailing zeros (which numify() loses). Used for APA-precision
#' tracking -- "0.0400" returns 4, "0.04" returns 2, "2" returns 0.
#'
#' Must be called on the raw regex match group, before numify().
#'
#' @param x Character (single value) -- the raw matched string
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
      resampling_inference = logical(0),
      resampling_method = character(0),
      resampling_B = numeric(0),
      resampling_B_source = character(0),
      p_reported_secondary = numeric(0),
      p_secondary_symbol = character(0),
      resampling_is_permutation = logical(0),
      p_reported_is_resampling = logical(0),
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
      effect_guard_rejected = logical(0),
      effect_guard_reason = character(0),
      SE_guard_rejected = logical(0),
      SE_guard_reason = character(0),
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
  #
  # v0.7.4: a sentence that ENDS at a blank line and is followed by a line
  # opening with a DIGIT also ends a chunk. Without it the splitter had no
  # notion of a paragraph, so a two-column PDF whose columns the extractor
  # merged stayed ONE chunk and the parser paired an effect size from one column
  # with a test statistic from another -- spps.txt location 216 shipped
  #   "...was d = 0.33, 95% CI [0.09, 0.57].\n\n0.75, 95% CI = [...], t = 7.47"
  # as a single t row carrying stat_value 7.47 and effect_reported 0.33, graded
  # g_ind = 0.379 against N = 1555. Both numbers are in the article; the PAIRING
  # is fabricated. The existing rule could not break it for one reason only: its
  # `(?=[A-Z]|$)` lookahead requires a capital, and column B resumes with a
  # digit. So the fix is exactly that -- widen the LOOKAHEAD, keep everything
  # else, and require a blank line for the digit case because "sentence, space,
  # digit" on one line is ordinary prose ("...was reliable. 30% of trials...").
  #
  # WHY NOT A GENERAL PARAGRAPH RULE. The first version of this fix dropped the
  # `(?<=[\.!?])` anchor and split on ANY blank line followed by a capital or a
  # digit -- the shape a two-column merge takes when the extractor truncates a
  # column mid-sentence. Two independent cross-model reviews converged on the
  # same class of counterexample, six of which reproduced locally:
  #   "..., p = .026, d = 0.75\n\n95% CI [0.09, 1.41]."  -> t row loses its CI
  #   "..., p = .026\n\nCohen's d = 0.75, ..."           -> t row loses its effect
  #   "The effect was medium, d = 0.\n\n65, 95% CI ..."  -> effect lost mid-number
  #   "t(58) = 3.45, p = .03\n\n1, ..."                  -> p truncated to .03
  #   "N = 1\n\n204 participants, ..."                   -> N truncated to 1
  #   "..., d = 0.65, 95%\n\nCI [0.40, 0.90], t(58)..."  -> CI severed from its d
  # and the whole-corpus diff found the same class in the wild: collabra.37122's
  # flattened appendix table separates a chi-square from its own
  # "OR = 0.99, 95% CI [0.77, 1.27]" cell by a blank line, and the general rule
  # dropped that odds ratio from the output entirely. Requiring the boundary to
  # sit after sentence-ending punctuation refuses every one of them, because in
  # each case the character before the blank line is a digit, a `%`, or a comma
  # -- text that is mid-statement by construction.
  #
  # `(?<!\d\.)` is the one guard the anchor does not supply on its own: a period
  # preceded by a digit is a DECIMAL POINT, not the end of a sentence, so
  # "d = 0.\n\n65" must not split even though `.` satisfies the anchor. This is
  # the same family as the v0.6.20 bridging invariants -- a rule that separates
  # or discards a reported value is the worst defect class in this parser.
  # v0.7.5: a BULLETED list item is a sentence boundary too. The existing rule
  # needs a capital (or a digit, after a blank line) immediately after the
  # whitespace, and a list marker sits in between, so consecutive bullets stayed
  # ONE chunk and only the FIRST statistic in the list was ever extracted.
  # ieee_access_alt prints three checkable results as three bullets and yielded
  # one row. The markers include U+FFFD because that is what the extractor
  # actually delivers here -- the bullet glyph is already lost upstream, and a
  # rule that only knew about U+2022 would not fire on the real text.
  #
  # This is strictly the existing capital-letter rule plus an intervening marker:
  # it keeps the `(?<=[\.!?])` anchor that refused all seven of the v0.7.4
  # counterexamples, and it still requires a capital after. It therefore cannot
  # split inside a number or between a statistic and its own effect size, CI,
  # p-value or N (invariant 6) -- the character before the boundary is
  # sentence-ending punctuation by construction, and a marker glyph never appears
  # inside a reported quantity.
  # `-` IS in the set, and it is the member that actually matters: an earlier
  # comment here credited U+FFFD, and that was wrong. Checked rather than
  # assumed -- `normalize_text()` maps the replacement character to a HYPHEN
  # (codepoint 45) before chunking ever runs, so ieee_access_alt's bullets
  # arrive as "- Childhood cancer: ...". Dropping `-` silently reverted that
  # paper from 3 rows to 1.
  bullet_marker <- paste0("[", intToUtf8(0x2022), intToUtf8(0x00B7),
                          intToUtf8(0x25CF), intToUtf8(0x25AA), intToUtf8(0xFFFD),
                          "*+-]")
  # A hyphen is also a dash, a minus and a range, so the marker alone is not
  # enough evidence. Cross-model review (Claude Sonnet, 2026-08-09, REPRODUCED):
  #   "chi2(1) = 12.74, p = .013. - N = 211, OR = 0.99, 95% CI [0.77, 1.27]."
  # split a chi-square from its own odds ratio and interval, degrading the row
  # from `mcnemar_or` carrying OR = 0.99 [0.77, 1.27] to a bare `chisq` with
  # both NA. That is invariant 6, and collabra.37122 is already on record for
  # losing an OR exactly this way (v0.7.4).
  #
  # The discriminator is what FOLLOWS the marker. A new list item is prose ("-
  # Childhood cancer: Mean difference of ..."); a continued statistic is an
  # assignment ("- N = 211", "- OR = 0.99"). Refusing to split before a short
  # symbol-then-operator keeps both behaviours, which the corpus confirms: the
  # three bulleted results still separate and no row loses a value.
  bullet_not_stat <- "(?![A-Za-z]{1,3}\\s*[0-9]*\\s*[=<>])"
  chunk_boundary <- paste0(
    "(?<=[\\.!?])\\s+(?=[A-Z]|$)",                        # sentence boundary
    "|(?<!\\d\\.)(?<=[\\.!?])\\n[ \\t]*\\n\\s*(?=[0-9])", # ... resuming on a digit
    "|(?<=[\\.!?])\\s+", bullet_marker, "\\s+(?=[A-Z])", bullet_not_stat
  )
  chunks <- unlist(strsplit(text_normalized, chunk_boundary, perl = TRUE))
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
    # v0.7.0: the robust family must split too. Without these, two robust
    # statistics in one sentence stayed a single chunk and the surviving row
    # took the FIRST p-clause -- "WTS(2) = 12.34, p = .002; ATS(1.87, Inf) =
    # 3.45, p = .061" published the ATS row carrying the WTS's p = .002, and
    # dropped the WTS row entirely (cross-model review, reproduced).
    "|(?:^|(?<=\\s|,|;|\\(|\\{))WTS\\s*[\\(\\[=]",          # WTS(df) = / WTS =
    "|(?:^|(?<=\\s|,|;|\\(|\\{))(?:F[_-]?)?ATS\\s*[\\(\\[]", # ATS(df1, df2) =
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
      resampling_inference = logical(0),
      resampling_method = character(0),
      resampling_B = numeric(0),
      resampling_B_source = character(0),
      p_reported_secondary = numeric(0),
      p_secondary_symbol = character(0),
      resampling_is_permutation = logical(0),
      p_reported_is_resampling = logical(0),
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
      effect_guard_rejected = logical(0),
      effect_guard_reason = character(0),
      SE_guard_rejected = logical(0),
      SE_guard_reason = character(0),
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
  # v0.7.0: the modern nonparametric / robust family. All four report a
  # statistic against a KNOWN reference distribution, so unlike the NOTE-only
  # types the reported p IS independently verifiable -- it is the effect size
  # that is not recoverable. Same shape as cochran_q (v0.5.15).
  #
  # WTS (Wald-type statistic, GFD/rankFD): asymptotically chi-square with
  #   df = rank of the contrast matrix. "WTS(2) = 12.34" or "WTS = 12.34, df = 2".
  pat_wts <- paste0(
    "(?i)\\bWTS\\s*(?:[\\[(]\\s*(\\d+(?:\\.\\d+)?)\\s*[\\])]\\s*)?",
    "=\\s*([-+]?\\d*\\.?\\d+)",
    "(?:\\s*,?\\s*df\\s*[12]?\\s*=\\s*(\\d+(?:\\.\\d+)?))?"
  )
  # ATS (ANOVA-type statistic, Brunner-Dette-Munk; nparLD/rankFD/GFD):
  #   F-distributed with a NON-INTEGER df1 and a df2 that may be finite or Inf.
  #   "ATS(1.87, 45.30) = 3.45" / "ATS(1.87, Inf) = 3.45".
  pat_ats <- paste0(
    "(?i)\\b(?:F[_-]?)?ATS\\s*[\\[(]\\s*(\\d+(?:\\.\\d+)?)\\s*,\\s*",
    "(Inf|infinity|\\d+(?:\\.\\d+)?)\\s*[\\])]\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  )
  # Brunner-Munzel: a t-like statistic with a Satterthwaite-type df. The
  #   statistic is written W / W_BF / t, all of which collide with existing
  #   patterns, so the "Brunner-Munzel" name is REQUIRED in the clause -- the
  #   same discipline chisq_subtype uses for McNemar/Friedman.
  #   The gap excludes any OTHER test name: cross-model review (reproduced)
  #   showed "The Brunner-Munzel alternative was considered, but the reported
  #   Wilcoxon result was W = 123" binding the Wilcoxon's W as a Brunner-Munzel
  #   statistic -- wrong type AND wrong value. A competing name in between is
  #   evidence the statistic belongs to that other test, so the match is
  #   refused rather than guessed.
  pat_brunner_munzel <- paste0(
    "(?i)\\bbrunner[\\s-]*munzel\\b",
    "(?:(?!wilcoxon|mann[\\s-]*whitney|kruskal|friedman|kendall|student)[^.]){0,90}?",
    "\\b(?:W(?:[_-]?BF)?|t)\\s*(?:[\\[(]\\s*(\\d+(?:\\.\\d+)?)\\s*[\\])]\\s*)?",
    "=\\s*([-+]?\\d*\\.?\\d+)",
    "(?:\\s*,?\\s*df\\s*=\\s*(\\d+(?:\\.\\d+)?))?"
  )
  # Yuen's trimmed-mean t (WRS2). Written "Ty(df) = v" or "Yuen's t(df) = v";
  #   the bare-t form collides with an ordinary t-test, so require the name.
  pat_yuen <- paste0(
    "(?i)\\byuen\\b[^.]{0,120}?",
    "\\b(?:Ty|t)\\s*[\\[(]\\s*(\\d+(?:\\.\\d+)?)\\s*[\\])]\\s*",
    "=\\s*([-+]?\\d*\\.?\\d+)"
  )
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
  # v0.6.11 (E-mcnemar-OR): a McNemar test reported as a discordant-pairs ODDS RATIO
  # (no chi-square value) -- "We also conducted a McNemar test ... OR = 0.18, 95% CI
  # [0.10, 0.29], p < .001". Such a sentence carries no test statistic, so the
  # generic path produced NO row at all and the OR was never surfaced (collabra.37122
  # has 4 of these, all silently dropped). Anchor on a "McNemar" mention followed
  # (within ~80 non-period chars) by "OR = <val>"; both must co-occur. The McNemar OR
  # is not independently recomputable from the sentence (it needs the discordant-pair
  # counts), so check.R routes the row to an honest extraction-only NOTE that surfaces
  # the OR (+ its CI + p, bound by the existing OR/CI/p machinery). Group 1 = OR.
  # Up to ~140 non-period chars between "McNemar" and the OR -- the descriptive
  # clause ("...found support for the association between temporal distance and
  # action-inaction regret, OR = 0.18") runs ~99 chars in collabra.37122. `[^.]`
  # keeps the match inside the one sentence.
  # (?i): the text prints "McNemar" (mixed case); str_match is case-sensitive by
  # default, so without the inline flag a lowercase "mcnemar" pattern never matches.
  pat_mcnemar_or <- paste0(
    "(?i)mcnemar\\b[^.]{0,140}?\\b(?:OR|odds\\s*ratio)\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  )
  # v0.6.10 (E-mediation): a bootstrapped MEDIATION indirect effect reported with a
  # Sobel Z, e.g. "the (bootstrapped) indirect effect of X on Y was .05, 95% CI
  # [-.04, .12], Sobel Z = 0.84, p = .40, ACME found to be robust until rho = 0.7".
  # Without this the parser routes the "Sobel Z = 0.84" to a PLAIN z-test and then
  # the fallback-ES pattern grabs the sensitivity-analysis "rho = 0.7" (the value of
  # the error-term correlation at which the ACME stops being robust -- an
  # Imai/Keele/Tingley mediation sensitivity bound) as the EFFECT SIZE, discarding
  # the actual indirect effect (.05) and emitting a spurious WARN. Capture both
  # anchors -- the indirect-effect value (group 1, after "indirect effect ... was")
  # AND the Sobel Z (group 2) -- so the dispatch can bind the indirect effect as the
  # effect and the Sobel Z as the test statistic, and suppress the rho grab. Both
  # anchors must co-occur for the pattern to fire. collabra.126266 (Outcome Bias
  # replication+extension): H2 + H5 mediation rows (4 results).
  # Bounded lazy spans (.{0,N}?) rather than [^.]* because the reported values are
  # full of decimal points (".05", "[-.04, .12]") that a no-period class would stop
  # at. The bounds keep the match within the single reporting clause.
  pat_mediation_indirect <- paste0(
    "indirect\\s+effect\\b.{0,40}?\\bwas\\s+([-+]?\\d*\\.?\\d+)",
    ".{0,80}?\\bSobel\\s+[Zz]\\s*=\\s*([-+]?\\d*\\.?\\d+)"
  )
  # v0.6.16 (E10 / E-bare-mediation-ci): a BOOTSTRAPPED mediation effect is
  # reported with a CI (and usually a p), never a Sobel Z -- so the Sobel-anchored
  # pattern above never fired and the result was silently dropped. Cognition &
  # Emotion 10.1080/02699931.2024.2434156 reports "The average direct effect was
  # 0.15, 95% CI [-0.13 to 0.45], p = .3, whereas the bootstrapped unstandardised
  # indirect effect (Average Causal Mediation Effect, ACME) was 0.67, 95% CI
  # [0.47-0.89], p < .001." -- two extractable results, zero rows emitted. The
  # text WAS delivered by the extractor (verified in the render), so this is an
  # ESCImate parse defect, not a docpluck gap. Found by the Sonnet canary audit
  # 2026-08-04.
  #
  # Anchored on an explicit mediation-effect NAME so an ordinary "the effect was
  # 0.15, 95% CI [...]" sentence cannot be mistaken for a mediation path. The CI
  # separator accepts "to", "-", or "," (all three appear in this paper alone).
  pat_mediation_ci <- paste0(
    "\\b(?:average\\s+(?:causal\\s+mediation|direct)\\s+effect|ACME|ADE|",
    "indirect\\s+effect|direct\\s+effect)\\b",
    "[^.]{0,120}?\\bwas\\s+([-+]?\\d*\\.?\\d+)",
    "[^.]{0,40}?\\b(\\d{2})\\s*%\\s*CI\\s*[\\[\\(]\\s*",
    "([-+]?\\d*\\.?\\d+)\\s*(?:to|,|-)\\s*([-+]?\\d*\\.?\\d+)\\s*[\\]\\)]"
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
  # v0.7.5 (handoff Issue A residue): an UNSTANDARDIZED mean difference reported
  # with a confidence interval and a p-value, and no test statistic at all.
  #
  #   "Mean difference of 457.66 articles, p-value = 7.171e-11,
  #    confidence interval (320.98, 594.35)."   (ieee_access_alt, 3 occurrences)
  #
  # This produced ZERO rows: every existing pattern anchors on a test statistic
  # or on a standardized effect size, and this clause has neither. But the triple
  # IS mutually checkable without either -- SE = (594.35 - 320.98)/(2 x 1.96)
  # = 69.74, z = 457.66/69.74 = 6.56, p ~ 5.3e-11 against a reported 7.171e-11 --
  # and the estimate must be the midpoint of its own interval. `md_hl` already
  # establishes the precedent: a row that carries CI-symmetry and p-CI-consistency
  # checks and claims no standardized effect size.
  #
  # NO EFFECT SIZE IS EVER CLAIMED for this type. A mean difference in "articles"
  # has no standardizer in the clause -- there is no SD, so no d is recoverable --
  # and inventing one would be the cross-scale defect `ci_referent` exists to
  # prevent. check.R routes it to NOTE.
  #
  # The anchor is deliberately narrow: the words "mean difference", a value, AND
  # both a p-value and an interval. Two orderings are accepted because both occur
  # in the wild; each requires all three numbers, so an incidental "mean
  # difference" in prose cannot create a row.
  .pat_mdci_num <- "([-+]?\\d+(?:\\.\\d+)?)"
  .pat_mdci_p <- "([01]?\\.\\d+|[01]|\\d+(?:\\.\\d+)?[eE][-+]?\\d+)"
  .pat_mdci_ci <- paste0("(?:confidence\\s+interval|CI)\\s*[\\(\\[]\\s*", .pat_mdci_num,
                         "\\s*(?:,|to)\\s*", .pat_mdci_num, "\\s*[\\)\\]]")
  .pat_mdci_pv <- paste0("[pP][- ]?(?:value)?\\s*([<=>]{0,2})\\s*", .pat_mdci_p)
  .pat_mdci_head <- paste0("(?i)\\b(?:mean\\s+difference|difference\\s+in\\s+means)",
                           "\\s+(?:of\\s+|was\\s+|=\\s*)?", .pat_mdci_num)
  # order A: estimate, p-value, interval   |   order B: estimate, interval, p-value
  pat_mean_diff_ci_p <- paste0(.pat_mdci_head, "[^.;]{0,90}?", .pat_mdci_pv,
                               "[^.;]{0,70}?", .pat_mdci_ci)
  pat_mean_diff_ci_p_alt <- paste0(.pat_mdci_head, "[^.;]{0,90}?", .pat_mdci_ci,
                                   "[^.;]{0,70}?", .pat_mdci_pv)

  # v0.6.16 (E11 / E-bare-d-ci): a post-hoc contrast reported as a bare Cohen's
  # d with its own CI and no test statistic of its own -- the Scheffe /
  # Games-Howell reporting style. cog_emo 10.1080/02699931.2024.2434156 prints
  # "Md = 3.80, 95% CI [2.46, 5.15], p < .001; d = 0.60, 95% CI [0.43, 0.77]"
  # for six contrasts; all six produced ZERO rows even though the extractor
  # delivered the sentence. Distinct from the covered bare-r (v0.5.10) and
  # bare-b/SE (v0.5.6) forms: here the EFFECT SIZE carries the interval.
  #
  # Requires d + a bracketed CI immediately after it, so an incidental "d = .5"
  # in prose cannot spuriously create a row. There is no test statistic, so the
  # row is extraction-only and check.R routes it to a NOTE -- the d and its CI
  # are surfaced as reported, never presented as independently verified.
  # The CI must be IMMEDIATELY adjacent to the d it belongs to: only optional
  # whitespace/punctuation and an optional "95%" may intervene. A permissive
  # gap let a d bind a CI belonging to a DIFFERENT d in the same sentence --
  # collabra.57785 "the effect size of between-subjects design ..., d = 0.39,
  # was smaller and below the range of confidence intervals of that of
  # within-subjects design, d = 0.55, 95% CI [0.47, 0.62]" bound 0.39 to
  # [0.47, 0.62], which is 0.55's interval. Attaching another finding's CI to
  # this one is a fabricated pairing, so the anchor is deliberately strict:
  # a d whose own CI is not adjacent simply carries no CI.
  pat_d_ci_nostat <- paste0(
    "(?<![a-zA-Z_])d\\s*=\\s*([-+]?\\d*\\.?\\d+)",
    "\\s*[,;]?\\s*(?:(\\d{2})\\s*%\\s*)?CI\\s*[\\[\\(]\\s*",
    "([-+]?\\d*\\.?\\d+)\\s*(?:,|to|-)\\s*([-+]?\\d*\\.?\\d+)\\s*[\\]\\)]"
  )

  # Auxiliary z for nonparametric tests (z co-reported with U/W)
  pat_z_aux <- "(?<![a-zA-Z])z\\s*=\\s*([-+]?\\d*\\.?\\d+)"

  # Patterns for sample sizes and design info
  # Improved p-value regex: handle optional leading '0', various separators, and spaces
  # Match both lowercase p and uppercase P (Nature, Scientific Reports, medical journals)
  # Also match "p < 0.001" (with leading zero) and "p = .05" (without)
  # v0.6.10: tolerate the malformed "p = <.001" form (a spurious "=" immediately
  # before the real operator, a common PDF text-layer artifact). The optional
  # `(?:=\s*(?=[<>]))?` consumes a leading "= " ONLY when a real `<`/`>` operator
  # follows (lookahead), so a normal "p = .40" still captures "=" as the operator
  # and "p <= .05" still captures "<=". collabra.126266 H5 punishment mediation:
  # docpluck delivers "Sobel Z = 4.87, p = <.001" (the PDF prints "p < .001").
  # v0.6.20 (MetaESCI O-1 sweep, class B): the bare `[01]` alternative had no
  # right-hand boundary, so it matched the LEADING DIGIT of a longer number and
  # the rest was silently dropped -- "p = 10" was published as p_reported = 1,
  # with p_valid = TRUE and p_out_of_range = FALSE. The [0, 1] validation at the
  # p_reported extraction never saw the offending value because the regex had
  # already truncated it to something in range. That is a fabricated number
  # shipped with a clean flag, so the alternative now requires that no digit or
  # decimal point follows. A malformed p is instead detected by pat_p_malformed
  # below and reported honestly via p_out_of_range.
  # The lookahead rejects a following DIGIT ("p = 10" -> not a p) and a following
  # decimal point THAT INTRODUCES MORE DIGITS ("p = 1.05" is handled by the
  # `[01]\.[0-9]+` alternative above and then rejected by the [0, 1] check). It
  # must NOT reject a bare sentence-terminating period: "p = 1." is a legitimate
  # in-range p at the end of a sentence, and a first draft using `(?![0-9.])`
  # turned it into a false "out of valid range" claim (found by cross-model
  # review, reproduced before fixing).
  # v0.7.5: a p preceded by a SIGNIFICANCE-LEGEND MARKER is not a result.
  # "~P < 0.1, *P < 0.05, **P < 0.01, ***P < 0.001" is the asterisk key of a
  # table or figure, and the thresholds in it belong to no test at all.
  #
  # Found in the wild while working Issue D, on the very paper Issue D is about.
  # PNAS 10.1073/pnas.2404157121's figure caption is merged into the body text
  # by the extractor and the caption ends "...***P < 0.001." followed by a
  # LOWERCASE continuation ("higher than that of NR, ..."), so the chunk splitter
  # -- which requires a capital or a digit after the boundary -- cannot separate
  # them. pat_p then took the FIRST p in the merged chunk and the row published
  #   t(2037) = -2.19, p_reported = 0.1
  # where the paper prints P = 0.029. A threshold from an asterisk key, attached
  # to a real published statistic, with no flag: the same fabrication class as
  # the v0.6.20 "p = 10 published as p_reported = 1".
  #
  # Fixing it at the p-binding rather than at the chunk boundary is deliberate.
  # A boundary rule would have to guess where the caption ends; this states the
  # thing that is actually true -- a marker glyph immediately before `p` means a
  # legend -- and it cannot separate a statistic from its own values, which is
  # the invariant chunk-level fixes keep threatening (v0.7.4, invariant 6).
  # 80 occurrences across 12 of the 48 corpus papers; every sampled one is a
  # legend. Both spacings are refused ("*p < .05" and "* p < .05"); PCRE needs
  # fixed-width lookbehinds, hence two of them rather than one alternation.
  pat_p <- paste0(
    "(?<![*~+#])(?<![*~+#] )",
    "\\b[pP]\\s*(?:=\\s*(?=[<>]))?([<=>]{1,2})\\s*",
    "(0?\\.[0-9]+|[01]\\.[0-9]+|[01](?![0-9])(?!\\.[0-9]))")
  # Detection-only companion to pat_p: any numeric p-clause, in range or not.
  # Used SOLELY to set p_out_of_range when pat_p declined the value, so an
  # impossible p ("p = 10", "p = 3.3") surfaces as a flagged extraction rather
  # than as a silently absent p-value. Deliberately NOT used for the `has_p`
  # gating that pat_p drives, so row selection is unchanged.
  pat_p_malformed <- "\\b[pP]\\s*(?:=\\s*(?=[<>]))?[<=>]{1,2}\\s*[-+]?\\d*\\.?\\d+"
  # Scientific notation p-values: p < 10^-15, p < 10-12 (PDF strips ^ in exponent)
  pat_p_sci <- "\\b[pP]\\s*(?:=\\s*(?=[<>]))?([<=>]{1,2})\\s*10\\s*\\^?\\s*[-\u2212](\\d+)"
  # v0.5.3: scientific E-notation p-values -- p = 2.572e-08, p = 1.2e-3 (the
  # form R / JASP / Python emit). pat_p rejects these (the mantissa is not a
  # [01].x number) and pat_p_sci only handles the "10^-N" form. The whole
  # mantissa+exponent number is captured, then converted to a decimal string.
  # (normalize_text has already folded Unicode minus U+2212 to ASCII '-', so an
  # ASCII hyphen in the exponent suffices -- same assumption pat_p relies on.)
  # v0.7.5: accept the "p-value" spelling. The pattern required the operator to
  # sit directly after `p`, so "p-value = 7.171e-11" matched nothing and the row
  # published `p_reported = NA` with "not a valid probability (outside [0,1] or
  # unparseable)" -- the value is a perfectly ordinary probability, written the
  # way IEEE and the clinical journals write it. This is the same spelling the
  # RR / rdpct / md_hl branches already synthesize `m_p` for; fixing it in the
  # shared pattern means every test type gets it, rather than a fourth per-branch
  # workaround. Found on ieee_access_alt while adding `mean_diff_ci`.
  pat_p_enote <- paste0("\\b[pP](?:[- ]?value)?\\s*(?:=\\s*(?=[<>]))?",
                        "([<=>]{1,2})\\s*(\\d+(?:\\.\\d+)?[eE]\\s*-\\s*\\d+)")
  # "Not significant" notation: "ns", "n.s.", "NS" (only after comma/semicolon)
  pat_p_ns <- "[,;]\\s*(?:ns\\.?|n\\.s\\.?|NS|N\\.S\\.?)(?=[\\s.,;)]|$)"
  # N regex: restrict to word boundary and look for nearby equals
  # Belt-and-suspenders: also capture comma-thousands in case any slip through normalization
  # v0.5.5: also accept "nobs" (the JASP "number of observations" token = total
  # N). Bare lowercase "n =" is intentionally NOT matched -- it is commonly a
  # per-group size, and matching it would mis-read a group n as the total N.
  # v0.6.18: the pattern itself now lives at package level (`.pat_doc_N`) so
  # `.doc_global_n()` -- shared with check_text() -- cannot drift from it.
  pat_N  <- .pat_doc_N
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
  # v0.3.0f: Generalized eta-squared -- explicit labels + PDF corruption forms
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
  # v0.6.13: hazard ratio (Cox proportional-hazards / survival analysis). Accepts
  # the adjusted forms (aHR / adjusted HR) and the spelled-out "hazard ratio". The
  # HR is an effect-size-only report with no recoverable test statistic (a Cox HR
  # needs the full survival data), so check.R routes test_type "hazard_ratio" to an
  # extraction-only NOTE surfacing HR + CI + p. The value must be bound TIGHTLY to
  # the HR token by an explicit "=" / ":" / "of" (a bare "hazard ratio 95% CI ..."
  # must NOT grab the "95" of "95% CI" -- a real FP seen on the s41598 shredded
  # survival table). The value is also forbidden from being a percentage
  # (negative lookahead on "%") and must look like a ratio (< 100). The standalone
  # dispatch additionally requires a co-located CI so a passing "HR" mention never
  # spuriously fires.
  pat_hr <- paste0(
    "(?:a?HR|adjusted\\s+HR|hazard\\s*ratio)\\s*",
    "(?:=|:|\\bof\\b)\\s*",
    "([-+]?(?:\\d{1,2}(?:\\.\\d+)?|0?\\.\\d+))(?!\\s*%)"
  )
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
  # v0.6.13: medical / epidemiology CI reporting WITHOUT brackets, tightly anchored
  # on an explicit "95% CI" phrase so it never fires on a bare numeric range. Two
  # forms: a dash/en-dash/"to" range ("95% CI 1.54-2.28", "95% CI 1.40 to 3.00")
  # and a colon/comma pair ("95% CI: 0.45, 0.85"). Only consulted (for HR / OR /
  # RR ratio rows) when the bracketed pat_CI1..4 did not already bind a CI, so the
  # existing psychology-corpus behaviour is unchanged. The leading bound may not be
  # signed for a ratio (a hazard/odds/risk ratio and its CI bounds are all > 0),
  # which also keeps this off a "difference = a - b" subtraction.
  pat_CI_medical_range <- paste0(
    "(\\d+\\.?\\d*)\\s*%\\s*(?:CI|C\\.I\\.|confidence\\s*interval)\\s*[:=]?\\s*",
    "(\\d+\\.?\\d+)\\s*(?:-|\\x{2013}|\\x{2014}|\\bto\\b)\\s*(\\d+\\.?\\d+)"
  )
  pat_CI_medical_comma <- paste0(
    "(\\d+\\.?\\d*)\\s*%\\s*(?:CI|C\\.I\\.|confidence\\s*interval)\\s*[:=]\\s*",
    "(\\d+\\.?\\d+)\\s*,\\s*(\\d+\\.?\\d+)"
  )
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
  # v0.6.18: computation shared with check_text() via .doc_global_n().
  global_N <- .doc_global_n(text_normalized)

  # v0.7.5 (handoff Issue C): the document-level resample count, read ONCE from
  # the Methods / Analysis section. Authors declare B there and never restate it
  # beside each reported p. Computed here, outside the per-chunk loop, so the
  # cost is one scan per document rather than one per statistic.
  doc_resampling_B <- .doc_resampling_b(text_normalized)

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
    m_wts <- stringr::str_match(s, pat_wts)
    m_ats <- stringr::str_match(s, pat_ats)
    m_brunner_munzel <- stringr::str_match(s, pat_brunner_munzel)
    m_yuen <- stringr::str_match(s, pat_yuen)
    m_binom_h   <- stringr::str_match(s, pat_binom_h)
    m_binom_bare <- stringr::str_match(s, pat_binom_bare)
    m_interaction_p <- stringr::str_match(s, pat_interaction_p)
    m_mediation_indirect <- stringr::str_match(s, pat_mediation_indirect)
    m_mediation_ci       <- stringr::str_match(s, pat_mediation_ci)
    m_d_ci_nostat        <- stringr::str_match(s, pat_d_ci_nostat)
    m_mcnemar_or <- stringr::str_match(s, pat_mcnemar_or)
    m_n_outN    <- stringr::str_match(s, pat_n_out_of_N)
    m_RR_ci_p <- stringr::str_match(s, pat_RR_ci_p)
    m_two_props <- stringr::str_match(s, pat_two_props_slash)
    m_risk_diff <- stringr::str_match(s, pat_risk_diff)
    m_median_diff <- stringr::str_match(s, pat_median_diff)
    # v0.7.5: unstandardized mean difference + CI + p, no test statistic. Order A
    # (est, p, CI) is tried first because it is the shape observed in the corpus;
    # the groups are normalized to (est, p_op, p, ciL, ciU) either way, so the
    # consumer below never has to know which alternative matched.
    m_mean_diff_ci <- stringr::str_match(s, pat_mean_diff_ci_p)
    if (all(is.na(m_mean_diff_ci))) {
      m_alt <- stringr::str_match(s, pat_mean_diff_ci_p_alt)
      if (!all(is.na(m_alt))) {
        # alt groups: 1 full, 2 est, 3 ciL, 4 ciU, 5 p_op, 6 p
        m_mean_diff_ci <- matrix(
          c(m_alt[1, 1], m_alt[1, 2], m_alt[1, 5], m_alt[1, 6], m_alt[1, 3], m_alt[1, 4]),
          nrow = 1)
      }
    }

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

    # v0.6.21: detect a RESAMPLING-derived p-value (permutation / randomization
    # / bootstrap / Monte Carlo / jackknife).
    #
    # This is deliberately NOT folded into method_kw above. That flag means "this
    # number is not a result at all" (a power analysis, a meta-analytic aside),
    # and its user-facing message says so. A permutation result IS a genuine
    # finding; only its REFERENCE DISTRIBUTION differs. The consequence is
    # narrow and specific: the p-value is not recomputable from the test
    # statistic, while the effect size still is (a permutation does not change
    # how t is computed, so d = 2t/sqrt(df) remains exactly as valid).
    #
    # Scoped to the row's OWN clause (`s`), never `context`: the v0.6.18 Welch
    # fix established that a modifier read from the context window leaks onto a
    # neighbouring row (there, N went 132 -> 403).
    #
    # Two deliberate exclusions:
    #   * "randomization" must be qualified by "test" -- a bare \brandomi[sz]ed\b
    #     would match every randomized controlled trial in the corpus.
    #   * "exact test" is absent. Fisher's exact is a closed-form conditional
    #     test whose p IS computable; matching it would suppress a legitimate
    #     check. Only the qualified "exact permutation/randomization" forms
    #     reach this pattern, via the permut/randomization alternatives.
    # "randomization" is accepted with test / inference / based / procedure --
    # "randomization inference" is standard in econometrics and was a real miss
    # (cross-model review, 2026-08-07, reproduced) -- but never bare, or every
    # randomized controlled trial would match.
    resampling_kw <- paste0(
      "(?i)\\b(?:permut\\w*|",
      "randomi[sz]ation[- ]?(?:tests?|inference|based|procedures?)|",
      "resampl\\w*|monte[- ]?carlo|bootstrap\\w*|shuffl\\w*|jack[- ]?knife\\w*)\\b"
    )
    # A resampling word attached to an INTERVAL ("bootstrapped 95% CI [...]")
    # says the CI was resampled, not the p-value. Suppressing the parametric p
    # check on that basis HIDES a genuine p-mismatch -- caught by cross-model
    # review and reproduced: "t(58) = 2.31, p = .50, d = 0.61, bootstrapped 95%
    # CI [0.10, 1.10]" lost its (correct) decision error. So an occurrence
    # counts for the P-VALUE only when it is NOT immediately followed by
    # interval language.
    resamp_pos <- stringr::str_locate_all(s, resampling_kw)[[1]]
    ci_follows <- paste0(
      "(?i)^\\W*(?:\\w+\\s+)?",
      "(?:\\d+(?:\\.\\d+)?\\s*%\\s*)?(?:CIs?|confidence\\s+intervals?)\\b"
    )
    resampling_inference <- FALSE
    resampling_method <- NA_character_
    if (nrow(resamp_pos) > 0) {
      for (k in seq_len(nrow(resamp_pos))) {
        hit <- substr(s, resamp_pos[k, "start"], resamp_pos[k, "end"])
        after <- substr(s, resamp_pos[k, "end"] + 1L, nchar(s))
        if (!grepl(ci_follows, after, perl = TRUE)) {
          resampling_inference <- TRUE
          resampling_method <- tolower(hit)
          break
        }
      }
    }

    # v0.7.3: is the BOUND p-value itself resampling-derived, or merely sitting
    # in a clause that mentions resampling? These are different questions, and
    # conflating them shipped a false claim: for the real sentence
    # "t(2037) = -3.26, P = 0.001, P-permutation = 0.002", the bound p is the
    # PARAMETRIC 0.001 (verified: 2*pt(-3.26, 2037) = 0.001132), yet the row
    # asserted "this p-value is not reproducible even with the raw data" about
    # it. A false statement about what is knowable, attached to a number we had
    # just verified -- the v0.6.19 defect class exactly.
    #
    # A p is QUALIFIED when a resampling word is glued or adjacent to it
    # ("P-permutation =", "permutation p ="). Note pat_p cannot match the
    # hyphenated form at all -- the hyphen breaks its operator adjacency -- so
    # in that phrasing the bound p is the unqualified one by accident rather
    # than by design. This makes the distinction explicit.
    qualified_p <- paste0(
      "(?i)(?:", "\\b(?:permut\\w*|randomi[sz]ation|resampl\\w*|bootstrap\\w*|",
      "monte[- ]?carlo)[\\s_-]*p\\b",
      "|", "\\bp[\\s_-]*(?:permut\\w*|randomi[sz]ation|resampl\\w*|",
      "bootstrap\\w*|monte[- ]?carlo)", ")"
    )
    # The distinction that makes this SAFE: only the GLUED form
    # ("P-permutation =", "p_permutation =") is invisible to pat_p, because the
    # hyphen/underscore breaks its operator adjacency. When the qualifier is
    # glued, whatever pat_p bound is necessarily the unqualified (parametric)
    # value -- that is provable, not inferred.
    #
    # A SPACED qualifier ("permutation p = .062") is fully visible to pat_p and
    # may well be what it bound. Reproduced: "permutation p = .062, parametric
    # p = .025" binds .062. There the binding is NOT provable, so the row stays
    # conservative. Checking a number you cannot prove you bound correctly is
    # worse than not checking: it publishes a verdict about the wrong value.
    # v0.7.5: the qualifier alternation, hoisted to ONE definition. It was
    # written out twice (here and in `qualified_p` above) and Issue D needs a
    # third use; three hand-maintained copies is how the v0.5.9 chi_tok drift
    # happened, where one copy silently stopped accepting a form the others did.
    resamp_qual_tok <- paste0("(?:permut\\w*|randomi[sz]ation|resampl\\w*|",
                              "bootstrap\\w*|monte[- ]?carlo)")
    glued_qualified_p <- grepl(
      paste0("(?i)\\b", resamp_qual_tok, "[_-]+p\\b|\\bp[_-]+", resamp_qual_tok),
      s, perl = TRUE)
    spaced_qualified_p <- grepl(qualified_p, s, perl = TRUE) && !glued_qualified_p
    # A plain p that pat_p can see.
    # ASCII operators only. R/ files must be pure ASCII for CRAN, and the two
    # routes to a Unicode class both fail here: an R "\\u2264" puts a literal
    # backslash-u into the pattern, and PCRE's own "\x{2264}" needs UTF mode
    # that this call does not enable. The loss is negligible -- a bare p-clause
    # is written with =, < or > essentially always, and normalize_text has
    # already folded the typographic variants by this point.
    has_plain_p <- grepl("(?i)(?<![a-z_-])p\\s*[<>=]", s, perl = TRUE)

    # Parametric ONLY when the qualifier is glued (so pat_p could not have bound
    # it) AND a plain p is present for it to have bound instead.
    p_reported_is_resampling <- resampling_inference &&
      !(glued_qualified_p && has_plain_p && !spaced_qualified_p)

    # v0.7.5 (handoff Issue D): RECOVER the qualified p instead of discarding it.
    #
    # A clause can carry two p-values of different provenance:
    #   "t(2037) = -3.26, P = 0.001, P-permutation = 0.002"
    # `pat_p` binds the parametric 0.001 -- it cannot see the hyphenated form at
    # all, because the hyphen breaks its operator adjacency -- and 0.002 was then
    # thrown away. A reader of the output could not tell that a permutation p had
    # been reported at all, let alone check it. Seven such occurrences in the
    # corpus, all in PNAS 10.1073/pnas.2404157121.
    #
    # It lands in a SIBLING COLUMN, never a second row. A new row would change
    # `nrow()` for every consumer and silently shift every downstream index;
    # MetaESCI's field registry is frozen at v0.4.0, so it already tolerates new
    # columns it does not know about, and cannot tolerate new rows.
    #
    # Scoped to the GLUED form only, and that scoping is the whole safety
    # argument: a glued qualifier is PROVABLY invisible to `pat_p`, so whatever
    # this captures is provably NOT what `pat_p` bound. A spaced qualifier
    # ("permutation p = .062") is fully visible to pat_p and may well be the
    # primary already -- capturing it could publish the same number twice under
    # two provenances, which is worse than dropping it.
    p_reported_secondary <- NA_real_
    p_secondary_symbol <- NA_character_
    if (glued_qualified_p) {
      # Same numeric alternation as pat_p: a p-value is in [0, 1] and is written
      # ".002" / "0.002" / "1" / "0". Accepting a wider shape here would let a
      # neighbouring statistic through under a p-value's name.
      p_num <- "(0?\\.[0-9]+|[01]\\.[0-9]+|[01](?![0-9])(?!\\.[0-9]))"
      sec_pats <- c(
        paste0("(?i)\\b", resamp_qual_tok, "[_-]+p\\s*([<=>]{1,2})\\s*", p_num),
        paste0("(?i)\\bp[_-]+", resamp_qual_tok, "\\s*([<=>]{1,2})\\s*", p_num)
      )
      for (sp in sec_pats) {
        m_sec <- stringr::str_match(s, sp)
        if (!is.na(m_sec[1, 3])) {
          p_reported_secondary <- numify(m_sec[1, 3])
          p_secondary_symbol <- m_sec[1, 2]
          break
        }
      }
    }

    # v0.6.22: the RESAMPLE COUNT (B). With B known, the smallest p the
    # procedure can produce by counting is 1/(B+1) (Phipson & Smyth 2010) --
    # checkable with no raw data at all. Without B the p-value is not
    # reproducible even WITH the data, which is itself worth reporting.
    #
    # Gated on resampling_inference so an ordinary "1,000 samples" or a generic
    # count in non-resampling prose can never populate it. The accepted forms,
    # the separator handling and the refusals all live in
    # `.resample_count_in()`, shared with the document-level prescan -- see its
    # header. One definition, no drift (the v0.5.9 chi_tok lesson).
    #
    # v0.7.5: `resampling_B_source` records WHERE the count came from, because
    # the two provenances are not equally strong and a consumer must be able to
    # tell them apart. `own_clause` is stated beside the statistic;
    # `methods_prescan` is a document-level default read from the Methods /
    # Analysis section, which is where authors actually declare B -- across the
    # whole 48-paper validation corpus, the clause-level scan alone populated
    # `resampling_B` for ZERO rows, so every Monte-Carlo floor check shipped in
    # v0.6.22 was inert. A check that cannot fire is indistinguishable from one
    # that passes.
    resampling_B <- NA_real_
    resampling_B_source <- NA_character_
    if (resampling_inference) {
      resampling_B <- .resample_count_in(s)
      if (!is.na(resampling_B)) {
        resampling_B_source <- "own_clause"
      } else if (!is.na(doc_resampling_B)) {
        resampling_B <- doc_resampling_B
        resampling_B_source <- "methods_prescan"
      }
    }

    # v0.6.22: only a PERMUTATION-type procedure has the choose(n1+n2, n1)
    # reference set. A bootstrap resamples WITH replacement, so that floor does
    # not bind it at all -- applying it flagged a legitimate bootstrap p
    # (cross-model review, reproduced). Recorded here so check.R can gate the
    # exact-floor test without re-parsing the method string.
    resampling_is_permutation <- resampling_inference &&
      !is.na(resampling_method) &&
      grepl("(?i)^(?:permut|randomi[sz]ation|shuffl)", resampling_method)

    # Enhanced N extraction with extended context and global fallback (Phase 2C)
    # Priority: own sub-chunk > local context > extended context > global
    # Extract ALL N values from local context (not just first) for candidate selection
    m_N_all_local <- stringr::str_match_all(context, pat_N)[[1]]
    N_candidates <- if (nrow(m_N_all_local) > 0) {
      unique(na.omit(sapply(m_N_all_local[, 2], numify_int)))
    } else {
      numeric(0)
    }

    # v0.6.12 (E-ownclause-N): prefer an N that appears in the row's OWN sub-chunk
    # `s` over one that only appears in the wider +/-2-sentence context window.
    # The generic scan above reads `context` and takes N_candidates[1] -- the FIRST
    # N in the window -- which for a row whose context window opens on a PRECEDING
    # sentence binds the neighbor's N, not the N stated in the row's own clause.
    # collabra.57785 loc 170: the clause literally reads "(M = 4.90, SD = 1.42,
    # N = 743) ... (M = 4.11, SD = 1.44, N = 743; t(742) = 12.24, ...)" -- N = 743
    # twice -- yet the parser bound N = 350 from the previous sentence (loc 167
    # "N = 350 ... N = 393"), check.R rejected it as implausibly small for df=742,
    # fell back to the independent default N = df+2 = 744, and the wrong N produced
    # a spurious ci_check_status = INCONSISTENT on a genuine finding. The
    # own-sub-chunk N is authoritative for its own test; only fall back to the
    # surrounding window when the clause carries none. Mirrors the v0.6.8
    # "prefer the signal closest to / inside the row's own clause" discriminator.
    m_N_own <- stringr::str_match_all(s, pat_N)[[1]]
    N_own_candidates <- if (nrow(m_N_own) > 0) {
      unique(na.omit(sapply(m_N_own[, 2], numify_int)))
    } else {
      numeric(0)
    }
    # Bind the own-clause N only for the neighbor-bleed case this fix targets: a
    # t-test whose own sub-chunk carries an UNAMBIGUOUS N (exactly one distinct
    # value) co-located with the `t(df) =` report. loc 170's clause states
    # "N = 743" twice (unique -> 743) alongside "t(742) = 12.24", so binding it is
    # safe and correct. The gate is deliberately narrow:
    #   * `t(` in the own sub-chunk -- so this only fires for a t-test row, never
    #     the r-test N-candidate path (which has its own best-N-by-p-value-fit
    #     selection + "Multiple sample sizes" note that must keep running when the
    #     sub-chunk splitter glues a preceding "N = ..." sentence to the r's chunk),
    #     nor chi-square / z / F rows.
    #   * exactly one distinct own-clause N -- a clause with two different N's is
    #     genuinely ambiguous (e.g. a between-groups "N = 350 ... N = 393") and is
    #     left to the existing n1/n2 + context flow.
    own_is_ttest <- grepl("\\bt\\s*\\(", s, perl = TRUE)

    # v0.6.16 (E7 / E-zrow-subsample-n): a proportion/z clause that states its
    # OWN denominator as "<k>/<N>" is authoritative -- it names the exact sample
    # the statistic was computed on. collabra.37122's reversal-subsample rows
    # read "(113/133, the total number of participants who showed reversal ...)
    # versus ... (20/133, 15.04%), ... z = 7.98" yet bound N = 493, the PARENT
    # study total from the surrounding window. The wrong N is not cosmetic: it
    # feeds the emitted `all_variants` values a reader sees (r_from_z 0.3382
    # published where N = 133 gives ~0.57; d_ind 0.7188 vs the correct 1.3839 --
    # nearly double), and those fields are surfaced regardless of the row's
    # SKIP status. Same "prefer the signal inside the row's own clause"
    # discriminator as the v0.6.8 t-test fix above, which was gated to t-tests
    # and so never covered z / proportion rows.
    #
    # Gate: the clause must state a slash-denominator, and all such
    # denominators in the clause must AGREE (a clause mixing "113/133" and
    # "20/140" is genuinely ambiguous and is left to the existing flow).
    m_slash_den <- stringr::str_match_all(
      s, "\\b\\d+\\s*/\\s*(\\d+)\\b")[[1]]
    slash_dens <- if (nrow(m_slash_den) > 0) {
      unique(na.omit(sapply(m_slash_den[, 2], numify_int)))
    } else {
      numeric(0)
    }
    if (length(slash_dens) == 1L && is.finite(slash_dens[1]) && slash_dens[1] > 0) {
      N_value <- slash_dens[1]
      N_source <- "own_clause_denominator"
      N_candidates <- unique(c(slash_dens, N_candidates))
    } else if (own_is_ttest && length(N_own_candidates) == 1L) {
      N_value <- N_own_candidates[1]
      N_source <- "own_clause"
      # Surface the own-clause value FIRST among candidates so any downstream
      # "first candidate" consumer also sees the authoritative value.
      N_candidates <- unique(c(N_own_candidates, N_candidates))
    } else if (length(N_own_candidates) == 1L &&
               !grepl("\\br\\s*=|\\br\\s*\\(", s, perl = TRUE)) {
      # v0.6.16 (E7, generalized): the v0.6.8 own-clause preference was gated to
      # t-tests, so a z / chi-square / proportion row whose own clause states
      # exactly one unambiguous N still inherited a neighbour's N from the
      # window. The reasoning that made it right for a t-test -- the clause
      # states the sample its own statistic was computed on -- is test-type
      # independent. Still requires exactly one distinct own-clause N.
      #
      # r-tests are EXCLUDED (as in the v0.6.8 gate): a correlation without an
      # explicit df runs a best-N-by-p-value-fit selection over ALL candidates
      # and emits a "Multiple sample sizes" note. Binding the first own-clause N
      # short-circuits that selection and silently drops the ambiguity note --
      # caught by test-metaesci-v023.R:530 and
      # test-v0612-ownclause-n-and-repcol-dedup.R:188 when this branch was first
      # written without the exclusion.
      N_value <- N_own_candidates[1]
      N_source <- "own_clause"
      N_candidates <- unique(c(N_own_candidates, N_candidates))
    } else {
      N_value <- if (length(N_candidates) > 0) N_candidates[1] else NA_real_
      N_source <- if (!is.na(N_value)) "local_context" else NA_character_
    }

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

    # v0.6.11 (E-subgroupN): two unsubscripted per-group sizes reported as
    # "<group-A> ... N = <a> ... <group-B> ... N = <b>" -- e.g. collabra.74820
    # "we divided the sample in high CA (score >= 3, N = 223) and low CA
    # (score <= 2, N = 19)". The generic N extraction binds only the FIRST N (223)
    # as the TOTAL, which (a) is wrong (223 is a subgroup), (b) makes check.R fire a
    # bogus "N=223 implausibly small for df=240" warning, and (c) forces an
    # equal-split Cohen's d. When n1/n2 were NOT given explicitly (no n1=/n2=) and
    # the chunk shows a between-groups split (a high/low or two contrasting group
    # words) with EXACTLY TWO distinct "[Nn] = <int>" values, bind them as n1/n2.
    # Tightly guarded: requires the split keyword AND exactly two N values, so a
    # lone total N or a single subgroup is untouched. The total N (n1+n2) is left
    # for check.R to infer from the design + df, matching the existing pipeline.
    if (all(is.na(m_n1)) && all(is.na(m_n2))) {
      split_kw <- grepl(paste0(
        "\\b(high|low|older|younger|male|female|men|women|experimental|control|",
        "treatment|placebo|between[- ]groups?|two[- ]groups?|independent[- ]samples)\\b"),
        context, ignore.case = TRUE, perl = TRUE)
      Nn_vals <- suppressWarnings(as.integer(
        stringr::str_match_all(context, "\\b[Nn]\\s*=\\s*(\\d[\\d,]*\\d|\\d+)")[[1]][, 2]
      ))
      Nn_vals <- Nn_vals[!is.na(Nn_vals)]
      if (split_kw && length(unique(Nn_vals)) == 2L && length(Nn_vals) == 2L) {
        # Order is informational only (n1 = first reported group); the t-test is
        # symmetric in n1/n2 for the pooled-SD Cohen's d.
        m_n1 <- matrix(c(NA, as.character(Nn_vals[1])), nrow = 1)
        m_n2 <- matrix(c(NA, as.character(Nn_vals[2])), nrow = 1)
        # The generic extraction bound N_value to the FIRST subgroup (e.g. 223);
        # the true total is the sum of the two groups. Correct it so check.R does
        # not flag the subgroup size as an implausibly small total.
        N_value <- sum(Nn_vals)
        N_source <- "subgroup_sum"
      }
    }
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
    m_hr <- stringr::str_match(s, pat_hr)
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
    # v0.6.20 (MetaESCI O-1 request 2): set by the parse-time plausibility guard
    # when it suppresses a reported effect size, so the suppression is visible
    # downstream instead of reading as "no effect size was reported".
    effect_guard_rejected <- FALSE
    effect_guard_reason <- NA_character_
    # v0.6.10 (E-mediation): set when this sub-chunk is a Sobel-Z mediation indirect
    # effect, so the effect-detection block binds the indirect effect (not the
    # sensitivity-analysis rho) and suppresses the fallback-ES rho grab.
    is_mediation_indirect <- FALSE
    mediation_indirect_effect <- NA_real_
    mediation_indirect_effect_decimals <- NA_integer_
    # v0.6.16 (E10): CI bounds carried from pat_mediation_ci for adoption in the
    # CI block below (ciL/ciU are not in scope at the dispatch site above).
    mediation_ci_bounds <- c(NA_real_, NA_real_)
    mediation_ci_level  <- NA_real_
    # v0.6.16 (E11): the d matched by pat_d_ci_nostat (the one whose CI is
    # adjacent), so the generic first-"d ="-in-chunk scan cannot override it.
    d_ci_nostat_effect <- NA_real_
    d_ci_nostat_effect_decimals <- NA_integer_

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

    # v0.7.0 (cross-model review, reproduced): Yuen's trimmed-mean test and
    # Brunner-Munzel are most commonly written with a PLAIN "t(df) = v", which
    # this generic branch claims first -- so "Yuen's trimmed-mean test, t(18.5)
    # = 2.31" was typed `t` and had ordinary Cohen's d variants computed for it,
    # effect sizes those statistics do not imply. Both name-anchored patterns
    # therefore veto the generic t branch. (Cheaper and safer than relocating
    # the branch: the chain is long and order-sensitive elsewhere.)
    if (!all(is.na(m_t)) &&
        all(is.na(m_brunner_munzel)) && all(is.na(m_yuen))) {
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
      # exact p-check needs df. Less strict than pat_t_nodf -- the p-clause
      # anchor in the regex is what disambiguates a real t-test from any
      # unrelated "t = value". See parse.R::pat_t_p_nodf for the rationale.
      test_type <- "t"
      stat_value <- numify(m_t_p_nodf[2])
      stat_value_decimals <- count_decimal_places(m_t_p_nodf[2])
      # df1 remains NA -- downstream Phase 5/6/7 paths handle NA-df by either
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
    } else if (!all(is.na(m_mean_diff_ci))) {
      # v0.7.5: unstandardized mean difference with a CI and a p, no statistic.
      # `stat_value` holds the ESTIMATE, exactly as md_hl holds the median
      # difference -- it is the quantity the interval brackets, not a test
      # statistic, and check.R's mean_diff_ci branch treats it as such.
      test_type <- "mean_diff_ci"
      stat_value <- numify(m_mean_diff_ci[2])
      stat_value_decimals <- count_decimal_places(m_mean_diff_ci[2])
      if (all(is.na(m_p)) && !is.na(m_mean_diff_ci[4])) {
        p_op_mdci <- if (!is.na(m_mean_diff_ci[3]) && nchar(m_mean_diff_ci[3]) > 0) {
          m_mean_diff_ci[3]
        } else {
          "="
        }
        p_val_mdci <- m_mean_diff_ci[4]
        m_p <- matrix(c(paste0("p", p_op_mdci, p_val_mdci), p_op_mdci, p_val_mdci), nrow = 1)
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
    } else if (!all(is.na(m_brunner_munzel))) {
      # v0.7.0: Brunner-Munzel. The asymptotic form is a t-like statistic with
      # a Satterthwaite-type df, so its p is verifiable. Placed BEFORE the
      # generic W / t branches, which would otherwise claim it and attach a
      # rank-biserial r that the BM statistic does not imply. Its estimand is
      # p_hat = P(X<Y) + .5*P(X=Y), which is NOT a standard effect size we can
      # recover from the statistic.
      test_type <- "brunner_munzel"
      df1 <- numify(if (!is.na(m_brunner_munzel[2])) m_brunner_munzel[2]
                    else m_brunner_munzel[4])
      stat_value <- numify(m_brunner_munzel[3])
      stat_value_decimals <- count_decimal_places(m_brunner_munzel[3])
    } else if (!all(is.na(m_yuen))) {
      # v0.7.0: Yuen's trimmed-mean t (WRS2). Reference distribution is t with
      # the trimmed df, so the p is verifiable; the trimmed-mean effect size is
      # not recoverable from the statistic alone.
      test_type <- "yuen"
      df1 <- numify(m_yuen[2])
      stat_value <- numify(m_yuen[3])
      stat_value_decimals <- count_decimal_places(m_yuen[3])
    } else if (!all(is.na(m_ats))) {
      # v0.7.0: ANOVA-type statistic (Brunner-Dette-Munk). F-distributed with a
      # NON-INTEGER df1; df2 may be Inf, in which case pf(F, df1, Inf) reduces
      # exactly to pchisq(df1*F, df1).
      test_type <- "ats"
      df1 <- numify(m_ats[2])
      df2 <- if (grepl("^(?i)inf", m_ats[3])) Inf else numify(m_ats[3])
      stat_value <- numify(m_ats[4])
      stat_value_decimals <- count_decimal_places(m_ats[4])
    } else if (!all(is.na(m_wts))) {
      # v0.7.0: Wald-type statistic. Asymptotically chi-square with df = rank
      # of the contrast matrix -- structurally the same dispatch as Cochran Q.
      test_type <- "wts"
      df1 <- numify(if (!is.na(m_wts[2])) m_wts[2] else m_wts[4])
      stat_value <- numify(m_wts[3])
      stat_value_decimals <- count_decimal_places(m_wts[3])
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
    } else if (!all(is.na(m_mediation_indirect))) {
      # v0.6.10 (E-mediation): a bootstrapped mediation indirect effect reported
      # with a Sobel Z. Classify as "mediation_indirect" (NOT a plain z-test) and
      # bind the indirect-effect value (m[2]) as the reported effect and the Sobel Z
      # (m[3]) as the test statistic. Placed in the main chain BEFORE the trailing
      # z-test check so the Sobel Z is not consumed as a plain z. The
      # is_mediation_indirect flag (below) suppresses the fallback-ES rho grab so the
      # sensitivity bound "rho = 0.7" is NOT mistaken for the effect size. The
      # indirect effect via a Sobel Z is not independently recomputable from the
      # reported numbers (it needs the a/b path coefficients), so check.R routes the
      # row to an honest NOTE that surfaces the indirect effect + its CI.
      test_type <- "mediation_indirect"
      stat_value <- numify(m_mediation_indirect[3])      # the Sobel Z
      stat_value_decimals <- count_decimal_places(m_mediation_indirect[3])
      is_mediation_indirect <- TRUE
      mediation_indirect_effect <- numify(m_mediation_indirect[2])
      mediation_indirect_effect_decimals <- count_decimal_places(m_mediation_indirect[2])
    } else if (!all(is.na(m_mediation_ci))) {
      # v0.6.16 (E10 / E-bare-mediation-ci): a BOOTSTRAPPED mediation effect --
      # an ACME / ADE / indirect / direct effect reported with a CI and (usually)
      # a p, but NO Sobel Z. The v0.6.10 branch above requires the Sobel Z, so
      # this form produced ZERO rows (cog_emo 10.1080/02699931.2024.2434156: the
      # ADE 0.15 [-0.13, 0.45] p = .3 and the ACME 0.67 [0.47, 0.89] p < .001
      # were both dropped, though the extractor delivered the sentence).
      #
      # There is no test statistic to verify -- a bootstrapped ACME is not
      # recomputable from the reported numbers (it needs the resampled a/b path
      # draws) -- so this is an extraction-only row: the effect + CI + p are
      # surfaced honestly and check.R routes it to a NOTE. stat_value stays NA
      # (claiming a statistic we do not have would be pretending).
      test_type <- "mediation_indirect"
      is_mediation_indirect <- TRUE
      mediation_indirect_effect <- numify(m_mediation_ci[2])
      mediation_indirect_effect_decimals <- count_decimal_places(m_mediation_ci[2])
      # The CI is captured by the generic CI machinery further down (ciL/ciU are
      # not yet in scope here -- they are initialized ~500 lines later), so the
      # bounds this pattern matched are deliberately NOT bound at this point.
      # They are carried on the match object and adopted in the CI block below.
      mediation_ci_bounds <- c(numify(m_mediation_ci[4]), numify(m_mediation_ci[5]))
      mediation_ci_level  <- numify(m_mediation_ci[3])
    } else if (!all(is.na(m_mcnemar_or))) {
      # v0.6.11 (E-mcnemar-OR): a McNemar test reported as an odds ratio (no
      # chi-square value). No test statistic to verify; the OR is the reported
      # effect. check.R routes test_type "mcnemar_or" to an extraction-only NOTE
      # surfacing the OR (+ CI + p). collabra.37122 ("McNemar test ... OR = 0.18,
      # 95% CI [0.10, 0.29], p < .001", 4 rows). No df, no N inferred.
      test_type <- "mcnemar_or"
      # The OR token in the same sentence is picked up by the normal pat_OR match
      # (m_OR, below) and the effect-size + OR/CI machinery surfaces it; we only need
      # to claim the row here so it is not left unclassified.
    } else if (!all(is.na(m_hr)) &&
               grepl("\\d+\\s*%\\s*(?:CI|C\\.I\\.|confidence\\s*interval)|\\[\\s*[-+]?\\d*\\.?\\d+\\s*[,-]",
                     s, perl = TRUE)) {
      # v0.6.13 (hazard-ratio): a Cox proportional-hazards / survival-analysis
      # hazard ratio reported in a clean prose sentence -- "HR = 1.87, 95% CI
      # [1.54, 2.28], p < .01" (or aHR / adjusted HR / hazard ratio). No test
      # statistic is recoverable (a Cox HR needs the full survival data), so
      # check.R routes test_type "hazard_ratio" to an extraction-only NOTE that
      # surfaces the HR + its CI + p. Gated to a CO-LOCATED CI (a "95% CI" phrase
      # or a bracketed numeric range in the same sub-chunk) so a bare "HR" mention
      # -- or the abbreviation used in prose ("the HR analysis") -- never fires.
      # The HR value is bound as the reported effect by the effect-size dispatch
      # below (effect_name "HR"). Column-shredded survival TABLES (where the
      # univariate/multivariate HR/CI/p are interleaved and misaligned in the
      # docpluck text) are a docpluck extraction defect, not handled here.
      test_type <- "hazard_ratio"
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

    # v0.6.13 (E-ownclause-2arm): an independent t-test whose OWN sub-chunk `s`
    # states exactly two per-arm N's that sum to the independent-samples total
    # (n1 + n2 - 2 = df1) has those two N's bound as n1/n2, and the total N set to
    # their sum. Placed AFTER the df1 dispatch (df1 is assigned above) and only
    # when n1/n2 were not already resolved (the v0.6.11 E-subgroupN context scan
    # requires EXACTLY two N's across +-2 sentences, but a stats-dense results
    # section repeats the two arm N's in a neighbouring restatement, so the window
    # holds 4+ copies and that gate silently fails -- leaving the FIRST arm N bound
    # as a bogus TOTAL that check.R then flags "implausibly small for df"). The own
    # clause is authoritative and unambiguous: exactly two distinct N's there,
    # summing to df+2, ARE the per-arm sizes. collabra.57785 loc 167:
    # "(M = 4.75, SD = 1.36, N = 393) ... (M = 4.22, SD = 1.33, N = 350),
    # t(741) = 5.36 ... [independent Welch's t-test]" -- 393 + 350 = 743 = 741 + 2.
    # Gated to a t-test with a resolved df1 and the two-N-sum == df+2 invariant, so
    # a within/paired clause (sum would be df+1, not df+2) or an unrelated pair of
    # N's never matches. Surfaced by the 2026-07-02 cycle-2 canary re-audit
    # (WARN-FALSE-POSITIVE: n1/n2 unpopulated + a false "likely parsing error").
    if (!is.na(test_type) && test_type == "t" &&
        all(is.na(m_n1)) && all(is.na(m_n2)) && !is.na(df1)) {
      own_arm_ns <- suppressWarnings(as.integer(
        stringr::str_match_all(s, "\\b[Nn]\\s*=\\s*(\\d[\\d,]*\\d|\\d+)")[[1]][, 2]
      ))
      own_arm_ns <- own_arm_ns[!is.na(own_arm_ns)]
      if (length(own_arm_ns) == 2L && length(unique(own_arm_ns)) == 2L &&
          (own_arm_ns[1] + own_arm_ns[2]) == round(df1) + 2L) {
        m_n1 <- matrix(c(NA, as.character(own_arm_ns[1])), nrow = 1)
        m_n2 <- matrix(c(NA, as.character(own_arm_ns[2])), nrow = 1)
        N_value <- sum(own_arm_ns)
        N_source <- "own_clause_arms"
      }
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

    # v0.7.6: a standard error is NON-NEGATIVE BY DEFINITION. `pat_SE` accepts a
    # leading sign (it always has), and nothing in this package checked it -- so
    # an impossible SE flowed straight into the t-synthesis below.
    #
    # The occasion is docpluck's rule W0g, which infers a missing minus from
    # arithmetic ("this value must be negative, because otherwise it falls
    # outside its confidence interval"). Reproduced on OUR OWN corpus by diffing
    # docpluck 2.4.136 with the rule disabled:
    #   frontiers_music_mood_2024:  SE = 0.199 -> SE = -0.199 (and p -> -0.069)
    #   efendic_2022_affect:        [0.04, 0.25] -> [0.04, -0.25]
    # docpluck intends to delete W0g. This guard is NOT about W0g and does not
    # expire with it -- a negative SE is impossible whatever produced it.
    #
    # REJECT rather than flag-and-keep. "Flag, do not correct" governs a value
    # the PAPER printed and we might be wrong about; it does not license passing
    # a mathematically impossible number into arithmetic. Concretely, keeping it
    # sign-flips the synthesized t at the line below (b/SE), and the one check
    # that exists to validate that synthesis -- verify_t_from_b_SE -- absolutises
    # BOTH sides, so it is structurally incapable of noticing. The guard has to
    # sit UPSTREAM of the synthesis, which is here. The value itself is not lost:
    # it stays in the row's raw text, and the reason quotes it.
    #
    # This is the same shape check.R already applies one layer down, where the
    # b-referenced CI recompute gates on `SE > 0`. It was simply never propagated
    # to the extraction that feeds it.
    SE_guard_rejected <- FALSE
    SE_guard_reason <- NA_character_
    if (!is.na(SE_coeff) && SE_coeff < 0) {
      SE_guard_reason <- paste0(
        "A reported standard error of ", format(SE_coeff, trim = TRUE),
        " is impossible -- a standard error cannot be negative. The value was ",
        "rejected rather than used, so no test statistic was synthesized from ",
        "it and no interval was recomputed with it. A negative standard error ",
        "in extracted text is usually an extraction artefact rather than the ",
        "paper's own error; check the source document for the printed value.")
      SE_guard_rejected <- TRUE
      SE_coeff <- NA_real_
    }

    # v0.7.6: the two branches below decide whether a REGRESSION RESULT EXISTS,
    # which is a different question from whether its SE is usable. They must
    # therefore key on "the clause reported an SE at all", not on the post-guard
    # value -- otherwise refusing an impossible SE would delete the whole result
    # from the output, and a suppressed value would once again be
    # indistinguishable from an absent one. That silent-loss shape is the exact
    # defect class v0.6.20 was written to end.
    SE_present_in_clause <- !is.na(SE_coeff) || SE_guard_rejected

    # Regression type promotion: if t-test AND b + SE co-occur, set type to "regression"
    if (!is.na(test_type) && test_type == "t" && !is.na(b_coeff) && SE_present_in_clause) {
      test_type <- "regression"
    }
    # v0.5.6 (T5): a bare regression line -- "b = .., SE = .., p = .." with NO
    # test statistic of its own. Create a regression result and synthesize the
    # coefficient t = b / SE. All three of b, SE and a reported p are required,
    # so an incidental b/SE co-occurrence cannot spuriously create a result; df
    # is unknown (no test statistic was reported), so the downstream check is a
    # NOTE rather than a full verification.
    #
    # v0.7.6: the row is still created when the SE was refused, but NO statistic
    # is synthesized from it. b / (a negative SE) yields a t of the wrong sign,
    # and verify_t_from_b_SE absolutises both sides and so cannot catch it -- so
    # the only safe place to stop it is here, before the division.
    if (is.na(test_type) && !is.na(b_coeff) && SE_present_in_clause &&
        !all(is.na(m_p))) {
      test_type <- "regression"
      if (!is.na(SE_coeff) && SE_coeff != 0) {
        stat_value <- b_coeff / SE_coeff
      }
    }

    # v0.6.16 (E11 / E-bare-d-ci): LAST RESORT -- a post-hoc contrast reported as
    # a bare Cohen's d with its own CI and NO test statistic (Scheffe /
    # Games-Howell style). Fires only when nothing above claimed the chunk, so it
    # can never pre-empt a real t / F / chi-square / z row; the d + CI of a
    # normally-reported test is still bound by the effect-size machinery below.
    # cog_emo 10.1080/02699931.2024.2434156 reports six such contrasts
    # ("Md = 3.80, 95% CI [2.46, 5.15], p < .001; d = 0.60, 95% CI [0.43, 0.77]")
    # and emitted zero rows despite the text being delivered. Extraction-only:
    # there is no statistic to recompute from, so check.R routes it to a NOTE
    # surfacing the reported d and its interval -- never a verification claim.
    if (is.na(test_type) && !all(is.na(m_d_ci_nostat))) {
      test_type <- "d_reported_only"
      # Bind the d that THIS pattern matched -- the one whose CI is adjacent.
      # The generic effect-size scan below takes the FIRST "d =" in the chunk,
      # which on a two-effect sentence pairs one finding's d with another's CI:
      # collabra.57785 "..., d = 0.39, was smaller and below the range of
      # confidence intervals of that of within-subjects design, d = 0.55, 95%
      # CI [0.47, 0.62]" emitted d = 0.39 with 0.55's interval [0.47, 0.62] --
      # a fabricated pairing of two different results. Record the matched value
      # so the effect-size block below adopts it instead.
      d_ci_nostat_effect <- numify(m_d_ci_nostat[2])
      d_ci_nostat_effect_decimals <- count_decimal_places(m_d_ci_nostat[2])
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
    # v0.6.10 (E-mediation): for a Sobel-Z mediation indirect effect, the reported
    # effect is the indirect-effect coefficient captured by pat_mediation_indirect
    # (NOT any eta/d/rho token in the sentence). Bind it FIRST and unconditionally so
    # the trailing "ACME robust until rho = 0.7" sensitivity bound cannot be adopted.
    if (!is.na(test_type) && test_type == "d_reported_only" &&
        !is.na(d_ci_nostat_effect)) {
      # v0.6.16 (E11): adopt the d whose CI is adjacent, not the first d in the
      # chunk -- see the dispatch comment above (57785 two-effect sentence).
      effect_name <- "d"
      effect_reported <- d_ci_nostat_effect
      effect_reported_decimals <- d_ci_nostat_effect_decimals
    } else if (isTRUE(is_mediation_indirect) && !is.na(mediation_indirect_effect)) {
      effect_name <- "indirect_effect"
      effect_reported <- mediation_indirect_effect
      effect_reported_decimals <- mediation_indirect_effect_decimals
    } else if (!all(is.na(m_f2))) {
      # f^2 must come BEFORE plain f
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
      # Bare "f = value" after comma -- Cohen's f (for F-tests or t-tests reporting f)
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
    } else if (!all(is.na(m_hr)) && !is.na(test_type) &&
               test_type == "hazard_ratio") {
      # v0.6.13: bind the hazard ratio as the reported effect, but ONLY for a row
      # already classified hazard_ratio by the dispatch above (a co-located CI was
      # required there) -- so a stray "HR" token in another test's sentence never
      # binds as an effect size.
      effect_name <- "HR"
      effect_reported <- numify(m_hr[2])
      effect_reported_decimals <- count_decimal_places(m_hr[2])
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
    #
    # v0.6.20 (MetaESCI O-1 request 2): every rejection below is now RECORDED.
    # The guard used to null the value and say nothing, which made the row
    # indistinguishable downstream from "this statistic reported no effect
    # size" -- a false all-clear, and the more dangerous of the two O-1 failure
    # modes (27 of MetaESCI's 42 corrupted rows were silent losses of this
    # shape, shipped as OK / SKIP / NOTE). `effect_guard_rejected` and
    # `effect_guard_reason` travel with the row; check.R turns them into an
    # uncertainty message and sets extraction_suspect, so a suppressed value is
    # always visible as suppressed.
    # ========================================================================
    if (!is.na(effect_reported) && !is.na(effect_name)) {
      # Record the value/name BEFORE any rejection so the reason can quote it.
      .guard_reject <- function(rule) {
        effect_guard_rejected <<- TRUE
        effect_guard_reason <<- sprintf(
          "Reported effect size '%s = %s' was suppressed at parse time (%s); it is NOT absent from the source text.",
          effect_name, format(effect_reported, trim = TRUE), rule)
      }

      # Hard-bounded effect sizes: reject values outside mathematical bounds [0, 1]
      bounded_at_one <- c("R2", "r", "phi", "V", "eta2", "etap2", "omega2",
                          "rank_biserial_r", "cliffs_delta", "epsilon_squared", "kendalls_W")
      if (effect_name %in% bounded_at_one && abs(effect_reported) > 1.0) {
        .guard_reject(sprintf("|%s| > 1 is outside the mathematical bounds of %s",
                              format(effect_reported, trim = TRUE), effect_name))
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
        .guard_reject(sprintf("|%s| > 10 is implausible for the %s family",
                              format(effect_reported, trim = TRUE), effect_name))
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
          # d=6, d=8 etc. -- virtually never a real effect size
          .guard_reject(sprintf(
            "round integer |%s| > 5 for the %s family (page/line-number artifact)",
            format(effect_reported, trim = TRUE), effect_name))
          effect_name <- NA_character_
          effect_reported <- NA_real_
          effect_reported_decimals <- NA_integer_
        } else {
          # d=3, d=4, d=5 -- check context for spurious patterns
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
            .guard_reject(sprintf(
              "round integer %s for the %s family with a spurious-context or t-implausibility signal",
              format(effect_reported, trim = TRUE), effect_name))
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
    if (isTRUE(is_mediation_indirect) && !is.na(mediation_indirect_effect)) {
      # v0.6.10 (E-mediation): anchor the CI at the indirect-effect VALUE (e.g.
      # ".05") -- the bootstrapped CI sits right after it ("was .05, 95% CI
      # [-.04, .12]"), well before the Sobel Z and the trailing sensitivity rho.
      # v0.6.16 (E10): the same anchor for the CI-reported (no-Sobel) form,
      # whose value comes from pat_mediation_ci instead.
      effect_match_text <- if (!all(is.na(m_mediation_indirect))) {
        m_mediation_indirect[2]
      } else {
        m_mediation_ci[2]
      }
    } else if (!is.na(effect_name)) {
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

    # Guard: a confidence level outside [0.50, 1.00) is implausible (parsing
    # artifact). v0.6.20 (MetaESCI): this test was one-sided (`< 0.50` only), so
    # a level ABOVE 1 sailed through untouched -- "263.95% CI [0.11, 0.47]"
    # yielded ci_level = 2.6395, ci_level_mismatch = NA and status PASS. A
    # coverage probability of 1 or more is not merely implausible, it is
    # impossible: a 100% interval is the whole support, and anything beyond that
    # is undefined. A plausibility guard on a two-sided quantity has to be
    # two-sided, so the upper bound is now enforced with the lower one.
    if (!is.na(ci_level) && (ci_level < 0.50 || ci_level >= 1.00)) {
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
      if (!any(is.na(mediation_ci_bounds))) {
        # v0.6.16 (E10): a bootstrapped mediation CI printed with a "to" or "-"
        # separator ("95% CI [-0.13 to 0.45]", "95% CI [0.47-0.89]") is not
        # covered by the generic pat_CI* set, so adopt the bounds the
        # mediation pattern already captured. Without this the row surfaced an
        # effect and p with no interval -- the interval being the only
        # verification handle a bootstrapped ACME has.
        ciL <- mediation_ci_bounds[1]
        ciU <- mediation_ci_bounds[2]
        if (!is.na(mediation_ci_level)) {
          ci_level <- mediation_ci_level / 100
          ci_level_source <- "explicit_with_bounds"
        } else if (is.na(ci_level)) {
          ci_level <- 0.95
          ci_level_source <- "assumed_95"
        }
      } else if (!is.na(test_type) && test_type == "RR" && !all(is.na(m_RR_ci_p))) {
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
      } else if (!is.na(test_type) && test_type == "mean_diff_ci" &&
                 !all(is.na(m_mean_diff_ci))) {
        # v0.7.5: the interval is the ONLY verification handle this row has --
        # there is no test statistic to recompute a p from, so without the bounds
        # the row would be pure extraction with nothing checkable.
        ciL <- numify(m_mean_diff_ci[5])
        ciU <- numify(m_mean_diff_ci[6])
        ciL_reported_decimals <- count_decimal_places(m_mean_diff_ci[5])
        ciU_reported_decimals <- count_decimal_places(m_mean_diff_ci[6])
        if (is.na(ci_level)) { ci_level <- 0.95; ci_level_source <- "assumed_95" }
      } else if (!is.na(test_type) && test_type == "md_hl" && !all(is.na(m_median_diff))) {
        ciL <- numify(m_median_diff[3])
        ciU <- numify(m_median_diff[4])
        ciL_reported_decimals <- count_decimal_places(m_median_diff[3])
        ciU_reported_decimals <- count_decimal_places(m_median_diff[4])
        if (is.na(ci_level)) { ci_level <- 0.95; ci_level_source <- "assumed_95" }
      } else if (!is.na(effect_name) &&
                 effect_name %in% c("HR", "OR", "RR", "IRR")) {
        # v0.6.13: bracketless medical/epi CI for a ratio effect (HR / OR / RR /
        # IRR). Tried only when pat_CI1..4 bound nothing AND the row's own effect is
        # a ratio (so a bracketless "1.54-2.28" range can only be this ratio's CI,
        # never a subtraction elsewhere). The "95% CI 1.54-2.28" / "95% CI: 0.45,
        # 0.85" forms are common in survival / clinical reporting.
        m_ci_med_range <- stringr::str_match(s, pat_CI_medical_range)
        m_ci_med_comma <- stringr::str_match(s, pat_CI_medical_comma)
        if (!all(is.na(m_ci_med_range))) {
          ciL <- numify(m_ci_med_range[3])
          ciU <- numify(m_ci_med_range[4])
          ciL_reported_decimals <- count_decimal_places(m_ci_med_range[3])
          ciU_reported_decimals <- count_decimal_places(m_ci_med_range[4])
          if (is.na(ci_level)) {
            ci_level <- numify(m_ci_med_range[2]) / 100
            ci_level_source <- "inferred_from_context"
          }
        } else if (!all(is.na(m_ci_med_comma))) {
          ciL <- numify(m_ci_med_comma[3])
          ciU <- numify(m_ci_med_comma[4])
          ciL_reported_decimals <- count_decimal_places(m_ci_med_comma[3])
          ciU_reported_decimals <- count_decimal_places(m_ci_med_comma[4])
          if (is.na(ci_level)) {
            ci_level <- numify(m_ci_med_comma[2]) / 100
            ci_level_source <- "inferred_from_context"
          }
        }
      }
    }

    # v0.6.13 (E-mcnemar-chisq-OR): a 1-df chi-square whose ONLY reported effect
    # size is an ODDS RATIO with a CI is a McNemar test, not a contingency /
    # goodness-of-fit chi-square. A contingency/gof chi-square's canonical effect
    # is phi / Cramer's V; an odds ratio comes from the 2x2 discordant-pair
    # structure of a McNemar test. This is the mirror of the v0.6.5 rule ("a
    # V-bearing chi-square is contingency/gof, never McNemar"): here an OR-bearing
    # 1-df chi-square is McNemar. Reroute to test_type "mcnemar_or" so check.R
    # surfaces the OR + CI as an honest extraction-only NOTE, instead of leaving
    # it a chisq row whose OR is "unusual for chi-square" and gets SKIPped as a
    # likely extraction artifact. Caught by the 2026-07-02 cycle-2 canary re-audit
    # (collabra.37122 loc 305: a Table-6 restatement "chi2(1, N = 265) = 0.00,
    # OR = 0.99, 95% CI [0.77, 1.27]" of a McNemar finding the paper's 3 other
    # McNemar rows report in prose). Gated to df1 == 1 (a 2x2 table) AND a bound
    # CI AND effect_name == "OR" so a genuine contingency chi-square that also
    # mentions an OR without a CI, or a >1-df chi-square, is untouched.
    if (!is.na(test_type) && test_type == "chisq" &&
        !is.na(effect_name) && effect_name == "OR" &&
        !is.na(df1) && df1 == 1 &&
        !is.na(ciL) && !is.na(ciU)) {
      test_type <- "mcnemar_or"
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
        # v0.6.20 (class B): a p-clause that pat_p declined entirely (because the
        # value is not in [0, 1] at all -- "p = 10", "p = 3.3") is just as
        # out-of-range as one pat_p captured and the [0, 1] validation rejected.
        # Before this, only the second kind was flagged, so an impossible p read
        # downstream as "this result reported no p-value".
        #
        # The malformed detector is suppressed when the row reports an explicit
        # "ns": that row's p is legitimately non-numeric, so a stray impossible
        # p-clause elsewhere in the same chunk must not be attributed to it.
        # (Cross-model review, reproduced before fixing: "The reaction time on
        # trial p = 10 was recorded, and separately t(48) = 2.31, ns, d = 0.74."
        # flagged the t row as having an out-of-range p.) The detector is
        # necessarily chunk-scoped -- `s` IS the row's text, and there is no
        # finer granularity available -- so check.R's message is worded as a
        # statement about the row's text rather than about its result.
        is.na(p_reported) && !isTRUE(p_ns_flag) &&
          (!is.na(p_char) || grepl(pat_p_malformed, s, perl = TRUE))
      },
      p_decimal_corrected = p_decimal_corrected,
      p_ns = p_ns_flag,
      one_tailed_detected = one_tailed_detected,
      two_tailed_detected = two_tailed_detected,
      method_context_detected = method_context_detected,
      method_context_in_chunk = method_context_in_chunk,
      resampling_inference = resampling_inference,
      resampling_method = resampling_method,
      resampling_B = resampling_B,
      resampling_B_source = resampling_B_source,
      p_reported_secondary = p_reported_secondary,
      p_secondary_symbol = p_secondary_symbol,
      resampling_is_permutation = resampling_is_permutation,
      p_reported_is_resampling = p_reported_is_resampling,
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
      # v0.6.20 (MetaESCI O-1 request 2)
      effect_guard_rejected = effect_guard_rejected,
      effect_guard_reason = effect_guard_reason,
      # v0.7.6: an impossible standard error was refused. Separate from the
      # effect guard above because it suppresses a DIFFERENT field -- a row can
      # have a perfectly good effect size and an impossible SE.
      SE_guard_rejected = SE_guard_rejected,
      SE_guard_reason = SE_guard_reason,
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
    # v0.6.13 (F1): even when no test-statistic chunk parsed, a paper may still
    # carry a standalone Bayes factor (a RoBMA meta-analysis whose only "results"
    # in the extracted text are BF01 statements). Run the BF scan before the
    # empty-tibble early return so those rows are not silently dropped.
    bf_only <- .scan_standalone_bayes_factors(text_normalized)
    if (!is.null(bf_only) && nrow(bf_only) > 0L) {
      return(bf_only)
    }
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
      resampling_inference = logical(0),
      resampling_method = character(0),
      resampling_B = numeric(0),
      resampling_B_source = character(0),
      p_reported_secondary = numeric(0),
      p_secondary_symbol = character(0),
      resampling_is_permutation = logical(0),
      p_reported_is_resampling = logical(0),
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
      effect_guard_rejected = logical(0),
      effect_guard_reason = character(0),
      SE_guard_rejected = logical(0),
      SE_guard_reason = character(0),
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
  # parser legitimately picks up both -- but they are the same statistical
  # result, so emitting two rows inflates the row count and lets the table
  # fragment's check_scope drag the user-facing summary toward extraction-only
  # status.
  #
  # Dedup conservatively: only collapse rows that match exactly on
  # (test_type, stat_value within 1e-3, df1, df2, N). When two rows match,
  # keep the one whose raw_text contains the parenthesized canonical form
  # (e.g. `r(741)`, `t(50)`, `F(2,40)`) -- that is the body-text version. The
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
        # v0.6.14 (E-corr-two-prose): the group's rows share the dedup key, so
        # their (rounded) CI bounds are either all NA or all the same value.
        # CI-absent groups cannot be told apart by the CI discriminator.
        group_has_ci <- !is.na(ciL[idx[1L]]) || !is.na(ciU[idx[1L]])
        if (length(paren_idx) >= 2L && !group_has_ci) {
          # v0.6.14 (E-corr-two-prose): TWO OR MORE parenthesized body rows share
          # this key AND the group reports NO CI. This dedup exists to collapse a
          # body-text row against its TABLE-FRAGMENT restatement (paren body form
          # + non-paren `r = -.43` table cell) -- NOT to collapse two distinct
          # prose reports. A paper can report two genuinely different correlations
          # that coincidentally share the same r, df, N and carry no CI to tell
          # them apart -- e.g. collabra.23443's H2A "willingness to donate"
          # r(797) = .16 and H2C "estimates of others" r(797) = .16 (different
          # variables, different clauses). Both are parenthesized `r(797)` prose
          # forms, so neither is a table fragment; collapsing them silently drops
          # a real second result (PARSE-MISS). Keep EVERY parenthesized body row;
          # drop only the non-parenthesized fragment(s) in this key group.
          #
          # The !group_has_ci gate is load-bearing: when the group's rows DO share
          # an identical non-NA reported CI, they are a RESTATEMENT of one finding
          # (a repeated report quotes its own CI verbatim -- collabra.57785
          # loc-151 "we ran a two-tailed paired t-test ... t(742) = 3.15, d =
          # 0.15, 95% CI [0.07, 0.22]" vs loc-171 "Additionally, AS REPORTED IN
          # STUDY 3A ... t(742) = 3.15, d = 0.15, 95% CI [0.07, 0.22]"); two
          # genuinely-distinct results sharing stat, df, N, effect AND the exact
          # CI bounds is not a real case, while a restatement always does. Without
          # this gate the 57785 pair double-counts (caught by the 2026-07-04
          # whole-corpus baseline-vs-fixed render diff).
          keep[idx] <- FALSE
          keep[paren_idx] <- TRUE
        } else if (length(paren_idx) >= 1L) {
          # Either the classic body-vs-table-fragment duplicate (one parenthesized
          # body row + non-paren fragment(s)), or a RESTATEMENT group (2+ paren
          # rows sharing an identical non-NA CI -- see the v0.6.14 note above).
          # Both collapse to the first parenthesized row.
          keep[idx] <- FALSE
          keep[paren_idx[1L]] <- TRUE
        } else {
          # No parenthesized form -- keep the first, drop the rest
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

  # v0.6.13 (F1): standalone Bayes factor (BF01 / BF10) reported as a PRIMARY
  # finding with no accompanying test statistic. RoBMA / Bayesian meta-analyses
  # report the evidential Bayes factor for a meta-analytic property (publication
  # bias, heterogeneity) as a first-class result, e.g. "moderate evidence for
  # publication bias (BF01 = 0.11) ... weak evidence against heterogeneity
  # (BF01 = 1.24)". These are NOT recomputable (no per-study data), so each is an
  # extraction-only NOTE (check.R routes test_type "bayes_factor").
  #
  # Extraction is deliberately CONSERVATIVE (user-approved design,
  # TRIAGE_iterate_2026-07-02.md F1): a bare `BF01 = <v>` matcher would flood
  # every Bayesian paper -- collabra.90203 alone prints 13+ `BF01 =` values of
  # which only 2 are standalone primary results; the other 11 are companions of
  # an F / t test (their frequentist row is already extracted) or a
  # model-averaged-r companion (already the v0.6.6 `r_model_averaged` row) or a
  # DV-specific complementary Bayesian check. A qualifying standalone BF must
  # satisfy ALL THREE, evaluated per-occurrence over a bounded window around the
  # BF's own position (NOT the sub-chunk, which can hold two BFs -- the "0.11 ...
  # 14.93" clause):
  #   (1) ANCHOR: a primary-finding phrase within 70 chars BEFORE it -- one of
  #       "evidence (for|against) <finding>", "in favo(u)r of the
  #       (alternative|null)", or "Bayes factor (was|is|of|indicated|...)". Any
  #       of the three admits the BF; a bare table-cell / definitional BF (no
  #       such phrase within 70 chars) is excluded.
  #   (2) NO co-located frequentist statistic within +/-60 chars: no F(/t(/t=/
  #       r=/d=/OR=/chi/eta or a stripped-eta "= .0NN" token (excludes the F/t
  #       companions AND the model-averaged-r companion whose clause carries
  #       r = 0.002).
  #   (3) NOT about an effect estimate: the ~55 chars before must not name
  #       "(the|an|average|main) effect" (excludes "absence of the average
  #       effect (BF01 = 14.93)" -> the model-averaged r, and "evidence against
  #       an effect on moral responsibility (BF01 = 2.05)" -> a DV-specific
  #       complementary check).
  # The BF token itself accepts both "BF01"/"BF10" and the bare JASP/BayesFactor
  # "B01"/"B10" forms. Validated by a whole-corpus guard-live-vs-bypassed
  # false-positive sweep: fires on exactly BF01 = 0.11 (publication bias) +
  # BF01 = 1.24 (heterogeneity) on collabra.90203 and B10 = 20841.04 +
  # B10 = 1.25 on collabra.32572, and ZERO spurious rows on the other corpus
  # papers (including the SPPS Bayesian papers).
  bf_rows <- .scan_standalone_bayes_factors(text_normalized)
  if (!is.null(bf_rows) && nrow(bf_rows) > 0L) {
    raw_out <- dplyr::bind_rows(raw_out, bf_rows)
  }

  raw_out
}

#' Scan normalized text for standalone (primary-finding) Bayes factors
#'
#' v0.6.13 (F1). Emits one extraction-only row per qualifying `BF01`/`BF10`
#' occurrence (see the caller in `parse_text()` for the three-part discriminator
#' rationale). Scans the FULL normalized text per-occurrence rather than
#' per-sub-chunk, because a single clause can hold two Bayes factors ("... bias
#' (BF01 = 0.11) ... absence of the average effect (BF01 = 14.93) ...") and only
#' one is a standalone primary result.
#'
#' @param text_normalized The normalized full text (from `normalize_text()`).
#' @return A tibble of `test_type = "bayes_factor"` rows bindable to the
#'   `parse_text()` output, or NULL when none qualify. Each row carries the BF in
#'   `effect_reported` (named `BF01` or `BF10`), no test statistic, no p.
#' @keywords internal
.scan_standalone_bayes_factors <- function(text_normalized) {
  if (is.null(text_normalized) || length(text_normalized) == 0L ||
      !nzchar(text_normalized)) {
    return(NULL)
  }
  # Match a Bayes factor "BF01 / BF10 / B01 / B10 = <num>" (requires the "=" and a
  # numeric value, which alone excludes table-column headers and prose definitions
  # that carry no value). The "F" is optional: JASP / the BayesFactor R package
  # print the bare "B10" / "B01" form (collabra.32572: "B10 = 20841.04"). Capture
  # the subscript (01/10) and the value.
  bf_pat <- "\\bB\\s*F?\\s*(01|10)\\s*=\\s*([0-9]+(?:\\.[0-9]+)?)"
  mm <- gregexpr(bf_pat, text_normalized, perl = TRUE)[[1]]
  if (length(mm) == 0L || mm[1] == -1L) return(NULL)
  match_len <- attr(mm, "match.length")

  # Effect / test tokens that mark a co-located frequentist result (guard 2). The
  # "= .NN" alternative catches a docpluck-stripped eta symbol ("F(2,998)=..., =
  # .008") that prints as a nameless "= .NN". Case-insensitive.
  #
  # MECHANISM CORRECTED 2026-08-05 (triple-verification audit): this comment used
  # to attribute the stripping to "no ToUnicode mapping". REFUTED -- on
  # collabra.90203 the symbol is drawn as filled vector curves with no char
  # object, not a badly-encoded font glyph. The regex is unaffected and correct
  # either way: it keys on the OBSERVABLE token stream (a bare "= .NN" beside a
  # test statistic), not on why the symbol is missing -- which is the right way
  # to write it, since the same nameless form arises from vector ink, OCR loss,
  # and genuine encoding faults alike. Note the class is `\\.[0-9]`, i.e. ANY
  # decimal (".12", ".34"), not only ".0NN" -- do not narrow it to the one
  # observed ".008" case.
  effect_tok <- paste0(
    "F\\s*[\\(\\[]", "|\\bt\\s*\\(", "|\\bt\\s*=", "|\\br\\s*=",
    "|\\bd\\s*=", "|eta", "|chi", "|\\bOR\\s*=", "|=\\s*\\.[0-9]"
  )
  # An "effect"-estimate subject immediately before the BF (guard 3): "the/an/
  # average/main effect", incl. "absence of the average effect".
  effect_subject <- paste0(
    "(?:average|main|the|an|absence of the)\\s+",
    "(?:average\\s+|main\\s+)?effect"
  )

  keep_txt <- character(0)
  keep_name <- character(0)
  keep_val <- numeric(0)
  keep_dec <- integer(0)
  keep_ctx <- character(0)
  n_chars <- nchar(text_normalized)
  for (i in seq_along(mm)) {
    p <- mm[i]
    len <- match_len[i]
    # Collapse any internal whitespace (the pattern's \s* can span a line break
    # when the token wraps, e.g. "BF01\n= 1.24") so the subscript / value are
    # extracted from a clean single-line form and never carry a stray newline
    # into effect_reported_name.
    match_str <- gsub("\\s+", " ", substr(text_normalized, p, p + len - 1L))
    # subscript (01 / 10) and the numeric value from this occurrence
    bf_sub <- sub(".*\\bB\\s*F?\\s*(01|10).*", "\\1", match_str, perl = TRUE)
    val_str <- sub(".*=\\s*", "", match_str)
    bf_val <- suppressWarnings(as.numeric(val_str))
    if (is.na(bf_val)) next

    # (1) primary-finding anchor within 70 chars before. A standalone Bayes factor
    # is introduced by one of a small set of tight lexical forms: an
    # "evidence (for|against) <finding>" clause (collabra.90203: "evidence for
    # publication bias (BF01 = 0.11)"), an "in favo(u)r of the (alternative|null)"
    # verdict (collabra.32572: "the data was in favor of the alternative
    # hypothesis, B10 = 20841.04"), or a bare "Bayes factor was/is/of/indicated/..."
    # report (collabra.32572: "The Bayes factor was B10 = 1.25"). All three are
    # PRIMARY-finding phrasings; a bare table-cell / definitional BF (no such
    # phrase within 70 chars) is excluded.
    pre70 <- substr(text_normalized, max(1L, p - 70L), p - 1L)
    has_anchor <- grepl(
      paste0(
        "evidence\\s+(?:for|against)",
        "|in\\s+favou?r\\s+of\\s+the\\s+(?:alternative|null)",
        "|Bayes\\s+factor(?:s)?\\s+(?:was|is|of|indicated|were|suggests?|show(?:ed|s)?)"
      ),
      pre70, ignore.case = TRUE, perl = TRUE)
    if (!has_anchor) next

    # (2) no co-located frequentist statistic within +/-60 chars
    span_lo <- max(1L, p - 60L)
    span_hi <- min(n_chars, p + len + 60L)
    span <- substr(text_normalized, span_lo, span_hi)
    if (grepl(effect_tok, span, ignore.case = TRUE, perl = TRUE)) next

    # (3) not an effect-estimate subject
    pre55 <- substr(text_normalized, max(1L, p - 55L), p - 1L)
    if (grepl(effect_subject, pre55, ignore.case = TRUE, perl = TRUE)) next

    keep_txt  <- c(keep_txt, trimws(substr(text_normalized, span_lo, min(n_chars, p + len + 5L))))
    keep_name <- c(keep_name, paste0("BF", bf_sub))
    keep_val  <- c(keep_val, bf_val)
    keep_dec  <- c(keep_dec, count_decimal_places(val_str))
    keep_ctx  <- c(keep_ctx, trimws(substr(text_normalized, max(1L, p - 90L),
                                           min(n_chars, p + len + 30L))))
  }
  if (length(keep_val) == 0L) return(NULL)

  tibble::tibble(
    location = NA_integer_,
    raw_text = keep_txt,
    context_window = keep_ctx,
    test_type = "bayes_factor",
    df1 = NA_real_,
    df2 = NA_real_,
    stat_value = NA_real_,
    p_reported = NA_real_,
    p_symbol = NA_character_,
    p_valid = FALSE,
    p_out_of_range = FALSE,
    p_decimal_corrected = FALSE,
    p_ns = FALSE,
    one_tailed_detected = FALSE,
    two_tailed_detected = FALSE,
    method_context_detected = FALSE,
    method_context_in_chunk = FALSE,
    resampling_inference = FALSE,
    resampling_method = NA_character_,
    resampling_B = NA_real_,
    resampling_B_source = NA_character_,
    p_reported_secondary = NA_real_,
    p_secondary_symbol = NA_character_,
    resampling_is_permutation = FALSE,
    p_reported_is_resampling = FALSE,
    N = NA_real_,
    N_source = NA_character_,
    N_candidates_str = NA_character_,
    n1 = NA_real_,
    n2 = NA_real_,
    table_r = NA_real_,
    table_c = NA_real_,
    effect_reported_name = keep_name,
    effect_reported = keep_val,
    effect_reported_decimals = keep_dec,
    stat_value_decimals = NA_integer_,
    effect_fallback = FALSE,
    eta = NA_real_,
    ci_level = NA_real_,
    ci_level_source = NA_character_,
    ciL_reported = NA_real_,
    ciU_reported = NA_real_,
    ciL_reported_decimals = NA_integer_,
    ciU_reported_decimals = NA_integer_,
    z_auxiliary = NA_real_,
    b_coeff = NA_real_,
    SE_coeff = NA_real_,
    adj_R2 = NA_real_,
    df_arity_mismatch = FALSE,
    effect_guard_rejected = FALSE,
    effect_guard_reason = NA_character_,
    SE_guard_rejected = FALSE,
    SE_guard_reason = NA_character_,
    arm1_events = NA_real_,
    arm1_total  = NA_real_,
    arm2_events = NA_real_,
    arm2_total  = NA_real_
  )
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
    # v0.6.10 (E-origcol): the comparison column is not always labeled "Original
    # study/article" -- collabra.57785 Table 8 labels its two columns "Replication
    # Effect and CI" / "Original Effect and CI", so the v0.6.6 regex (which required
    # original + article|study|paper) missed it and every Table-8 finding was emitted
    # TWICE (the Original-column duplicate leaked as a spurious own-result). Widen the
    # "original ..." branch to also match the column-header forms "Original Effect",
    # "Original Result/Finding/Value", "Original Cohen's d / r / F", and an "Original
    # ... CI" / "Original ... [stat]" header. These all occur only as a flattened
    # comparison-table column header (row_label/group), never as one of the audited
    # paper's own substantive condition labels.
    # The "Original" comparison column appears in several real layouts, all of which
    # are the original/target study's values, NOT the audited paper's own results:
    #   - a header phrase: "Original study/article/paper", "Original Effect and CI",
    #     "Original Cohen's d", "Original r/F", "Original ... [95% CI]"
    #     (collabra.90203 Table 8-10 "Target article"; the v0.6.6 base case),
    #   - a parenthetical row-tag suffix: "3A: Insight into self (Original)" vs
    #     "(Replication)" (collabra.57785 Table 8 -- the v0.6.11 E-origcol case).
    # A standalone "(original)" / "(target article)" parenthetical tag, or "original"
    # as a whole flattened-table column label, is the comparison column and is
    # dropped. A substantive condition label that merely CONTAINS the word "original"
    # in running prose (no parenthetical tag, no stat-column word) is NOT dropped.
    comparison_col_re <- paste0(
      "(?i)(\\b(target\\s+article|source\\s+article|prior\\s+(study|work))\\b",
      "|\\boriginal\\s+(article|study|paper|effect|result|finding|value|",
      "cohen|d|r|f|estimate|statistic|column)\\b",
      "|\\boriginal\\b[^\\n]{0,30}\\b(ci|effect|result|d|r|f)\\b",
      "|\\(\\s*(original|target\\s+article|original\\s+study)\\s*\\)",
      "|^\\s*original\\s*$)"
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
      # v0.6.15 (E-modeb-t-n): docpluck types a per-sample `n` column on t-test
      # table rows that print n but NOT df (collabra.23443 Table 5:
      # `{t: 16.6, d: 0.59, n: 799, CI_lower, CI_upper}`). The r branch below has
      # always bound `fields.n`; the t branch discarded it, so such rows carried
      # no N at all and fell to SKIP/insufficient_data even though the sample
      # size was delivered typed. Bind it the same way. (df stays NA when the
      # table does not print it -- N alone lets check.R compute the dz / d_ind
      # variant family for the reported d.)
      if (has(f, "n")) nn <- num1(f$n)
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
      # untyped, so the value was discarded. The body-text symbol stays absent
      # from the delivered text -- so the table is the ONLY source.)
      #
      # MECHANISM CORRECTED 2026-08-05 (triple-verification audit). This comment
      # used to read "WON'T-FIX, no ToUnicode CMap". That stated cause is
      # REFUTED: on collabra.90203 the symbol is not a font glyph at all -- it is
      # 4 filled bezier curves (fill=TRUE, no fontname) with NO char object in
      # its x-range, i.e. drawn ink, not badly-encoded text. Measured across all
      # 21 pages: ZERO Greek-eta char objects anywhere in the text layer, while
      # glyph-sized filled curves cluster on exactly the affected pages. The
      # WON'T-FIX verdict for body prose stands (OCR / shape-recognition tier);
      # only its reason was wrong. Practical consequence: do NOT invest in
      # font/CMap/ToUnicode recovery for this class -- it targets the ordinary
      # space before `=` and can never recover drawn curves. The viable non-OCR
      # route is a vector-path recogniser. See docs/REPLY_FROM_DOCPLUCK_2026-06-25.md.
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

    # v0.7.6: range-validate the TYPED p, exactly as the prose path does.
    #
    # This path took whatever number the table channel typed as `p` and then
    # declared `p_valid = !is.na(p)` and `p_out_of_range = FALSE` -- two claims
    # the code never actually checked. Downstream, check.R has no lower bound on
    # p_reported and reads `p_reported < alpha` as "significant", so a negative p
    # would have read as overwhelmingly significant and could have driven a false
    # decision-error verdict.
    #
    # HONEST SCOPE: docpluck's own table channel already drops an out-of-domain
    # p, so this is defence in depth rather than a live defect being closed. It
    # is still worth doing on two grounds -- a column effectcheck publishes must
    # be computed by effectcheck and not inherited from someone else's invariant,
    # and a hardcoded FALSE is a claim this function cannot back.
    p_flat_out_of_range <- !is.na(p_val) && (p_val < 0 || p_val > 1)
    if (p_flat_out_of_range) p_val <- NA_real_
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
      p_out_of_range = p_flat_out_of_range,
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
