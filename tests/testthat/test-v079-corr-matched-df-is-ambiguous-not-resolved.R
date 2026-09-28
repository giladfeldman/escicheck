# v0.7.9 -- a bare correlation whose r value recurs elsewhere as `r(df)` is on an
# AMBIGUOUS sample. The tool must REPORT both readings, not pick one.
#
# Gilad's ruling, 2026-09-04: "We should never just correct things, the aim is
# highest transparency and accuracy."
#
# History. Commit 962b910 adopted the matched df and rebound N. Cross-model review
# (round 2026-09-04: Sol/GPT-5.6 openai, Sonnet 5 anthropic, Grok 4.6 xai -- 3
# seats, 3 providers, all OK) plus two local reproductions on the real document
# showed that picking moves the PUBLISHED verdict in BOTH directions:
#
#   * a correctly reported independent-sample CI whose row states its own N = 80
#     was rebound to N = 263 and graded INCONSISTENT -- a correct paper accused;
#   * a genuinely wrong CI on a row stating N = 50 was rebound to N = 263, where
#     the reported interval reproduces, and graded MATCH -- a real reporting error
#     silenced, with no test failing.
#
# Both directions are pinned below. WATCHED RED FIRST against the 962b910
# behaviour: before the fix, test 1 returned ci_check_status "INCONSISTENT" and
# test 2 returned "MATCH", and both N values came back as 263.
#
# The docstring defect behind the second case is pinned separately in test 3: the
# claim that a co-located n outranks this rule was false, because parse.R:3032
# excludes correlations from the own_clause branch, so their stated N lands as
# `local_context`, which IS in .SCRAPED_N_SOURCES.

# Select the row under test by a MARKER in its own clause, never by position.
#
# An earlier draft of this file selected "the last row with this r value" and
# silently tested a DONOR row instead: the recipient had been dropped by the
# v0.6.4 prose/table dedup because its reported CI was byte-identical to the
# donor's, and the assertions then passed against the wrong row. A fixture that
# does not touch the case is a wrong reject. `expect_gt(nrow(...), 0)` did not
# catch it, because the donors matched the value too -- hence the marker.
corr_row <- function(res, target, marker) {
  isr <- !is.na(res$test_type) & res$test_type == "r"
  sub <- res[isr, , drop = FALSE]
  hit <- sub[!is.na(sub$stat_value) & abs(sub$stat_value - target) < 1e-9, , drop = FALSE]
  # OWN CLAUSE ONLY. context_window spans neighbouring sentences, so in a short
  # fixture every row's context contains every marker -- matching on it returned
  # the donor rows too and the selector silently widened to 2 rows.
  rt <- if ("raw_text" %in% names(hit)) as.character(hit$raw_text) else rep("", nrow(hit))
  hit[grepl(marker, rt, fixed = TRUE), , drop = FALSE]
}

# Assert the fixture actually produced the row we intend to test, and that it is
# the RECIPIENT (no printed df of its own), not a donor.
expect_is_recipient <- function(row, expected_N) {
  expect_equal(nrow(row), 1L,
    info = "fixture must yield exactly one row under test -- 0 means it never parsed")
  expect_equal(as.numeric(row$N[1]), expected_N,
    info = "the row under test must carry its own stated N, not a donor's")
}

# A donor: the same r value stated elsewhere WITH a df, on a different sample.
DONOR <- paste(
  "We recruited N = 794 participants in total.",
  "In the control condition, r(261) = 0.45, 95% CI [0.35, 0.54], p < .001.",
  "In the control condition, forgiveness and revenge motivation were associated,",
  "r(261) = -0.43, 95% CI [-0.52, -0.33], p < .001.",
  # Filler: keeps the donor clauses out of the recipient's context window, so the
  # rule under test is the DOCUMENT-WIDE value match, not a co-located reading.
  "We then turned to the exploratory analyses.",
  "Model assumptions were inspected and are reported in the supplement.",
  "Robustness checks appear in the online materials.",
  "Data and code are available at the project repository.",
  "The following analyses concern a separate study entirely.")

test_that("a correct independent-sample CI is NOT accused when its r value recurs", {
  # The row states its own N = 80 and reports the interval that is CORRECT at
  # n = 80 ([-0.594, -0.232] to 3dp). Under 962b910 this was rebound to N = 263,
  # where the interval is [-0.524, -0.326], and published INCONSISTENT.
  txt <- paste(DONOR,
    "In an independent sample (N = 80), the association was",
    "r = -.43, 95% CI [-0.60, -0.22], p < .001.")

  row <- corr_row(check_text(txt), -0.43, "independent sample")
  expect_is_recipient(row, 80)

  expect_false(identical(as.character(row$ci_check_status[1]), "INCONSISTENT"),
    info = "a correctly reported CI must never be accused because its r value recurs")
  expect_identical(as.character(row$ci_check_status[1]), "UNVERIFIABLE")
  expect_true(is.na(row$ci_match[1]),
    info = "ci_match is a separate contract column; it must not carry a confident answer")
})

test_that("a genuinely wrong CI is NOT silenced when its r value recurs", {
  # The row states N = 50 but reports [0.35, 0.54] -- the n=263 interval, not its
  # own ([0.196, 0.647] at N = 50). That is a REAL reporting error. Under 962b910
  # it was rebound to 263, the interval reproduced, and it published MATCH.
  # NOTE the 3-decimal CI. It is the n=263 interval to full precision. An earlier
  # draft used the donor's own rounded [0.35, 0.54]; that made the row a textual
  # duplicate of the donor and the v0.6.4 dedup removed it, so the test silently
  # graded the donor instead.
  txt <- paste(DONOR,
    "In an independent sample, N = 50, r = .45, 95% CI [0.348, 0.541], p = .001.")

  row <- corr_row(check_text(txt), 0.45, "independent sample")
  expect_is_recipient(row, 50)

  expect_false(identical(as.character(row$ci_check_status[1]), "MATCH"),
    info = "a real reporting error must never be graded MATCH via a guessed sample size")
  expect_identical(as.character(row$ci_check_status[1]), "UNVERIFIABLE")
  expect_true(is.na(row$ci_match[1]))
})

test_that("both candidate sample sizes are reported in a machine-readable field", {
  # Transparency is the point: the reader must be able to see BOTH readings and
  # settle it themselves. uncertainty_reasons is a schema-stability contract
  # column, so this survives the hop to a downstream consumer; prose alone would not.
  txt <- paste(DONOR,
    "In an independent sample (N = 80), the association was",
    "r = -.43, 95% CI [-0.60, -0.22], p < .001.")

  row <- corr_row(check_text(txt), -0.43, "independent sample")
  expect_is_recipient(row, 80)

  reasons <- as.character(row$uncertainty_reasons[1])
  expect_true(grepl("AMBIGUOUS SAMPLE", reasons, fixed = TRUE))
  expect_true(grepl("N=80", reasons, fixed = TRUE),
    info = "the sample size the row itself states must be named")
  expect_true(grepl("N=263", reasons, fixed = TRUE),
    info = "the sample size implied by the matching r(df) must also be named")
  expect_true(grepl("r(261)", reasons, fixed = TRUE),
    info = "the evidence for the alternative must be quoted, not just its conclusion")
})

test_that("the row's own N is left exactly as the evidence had it -- nothing is rebound", {
  # The counterpart of "do not pick": we must not quietly move N either. A reader
  # who checks the source table needs to see the N the document actually gave.
  txt <- paste(DONOR,
    "In an independent sample (N = 80), the association was",
    "r = -.43, 95% CI [-0.60, -0.22], p < .001.")

  row <- corr_row(check_text(txt), -0.43, "independent sample")
  expect_is_recipient(row, 80)

  expect_equal(as.numeric(row$N[1]), 80)
  expect_false(identical(as.character(row$N_source[1]), "corr_matched_r_df"),
    info = "N_source must keep the provenance of the number actually bound")
})

test_that("CONTROL: with no matching r(df) anywhere, the row is graded normally", {
  # Two-sided control. This is what proves the three tests above are detecting the
  # ambiguity rule and not some unrelated failure to grade. r = -.37 has no donor,
  # so nothing is ambiguous and the ordinary verdict must stand.
  txt <- paste(DONOR,
    "In an independent sample (N = 80), the association was",
    "r = -.37, 95% CI [-0.55, -0.16], p < .001.")

  row <- corr_row(check_text(txt), -0.37, "independent sample")
  expect_is_recipient(row, 80)

  expect_equal(as.numeric(row$N[1]), 80)
  expect_false(identical(as.character(row$ci_check_status[1]), "UNVERIFIABLE"),
    info = "an unambiguous row must still receive a real verdict -- the cap must not be global")
  expect_false(grepl("AMBIGUOUS SAMPLE", as.character(row$uncertainty_reasons[1]), fixed = TRUE))
})

test_that("CONTROL: a correlation printing its own df is untouched", {
  # The donor rows themselves state r(261). They have a printed df, so df1_from_N
  # is FALSE and the ambiguity rule must never reach them.
  row <- corr_row(check_text(DONOR), 0.45, "control condition")
  expect_equal(nrow(row), 1L)

  expect_false(grepl("AMBIGUOUS SAMPLE", as.character(row$uncertainty_reasons[1]), fixed = TRUE))
  expect_equal(as.numeric(row$N[1]), 263)
})
