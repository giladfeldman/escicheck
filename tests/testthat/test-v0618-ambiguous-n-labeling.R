# v0.6.18 -- an ambiguous-design t-test must not publish the internal
# independent-assumption N (df + 2) as an unlabeled fact.
#
# Source: 2026-08-04 escicheck-iterate cycle 2 (Sonnet canary audit of
# collabra.57785, finding reproduced at HEAD before fixing; the same class
# explains the pci.rr.100726 "N = 870 with N_source = 'not_found'" finding).
#
# WHAT WENT WRONG
#
# (1) A docpluck flattened TABLE row never receives the document-global N
#     fallback (flattened_rows_to_parsed() emits no N_source at all), so a
#     table t-row with no printed n reached check.R with N = NA even when the
#     paper states its N in prose. collabra.57785 Table 8: t(742) rows
#     published N = 744 (df + 2) while the paper states N = 743 (= df + 1;
#     gold n_total = 743).
# (2) check.R's ambiguous-design branch stores the INDEPENDENT df + 2 in `N`
#     as an internal computation variable ("Use for independent calculations")
#     -- and that internal value leaked to the published top-level N with no
#     N_source and no uncertainty message naming the paired alternative.
#     repro_code then asserted "Assuming equal n" of (df+2)/2 + (df+2)/2.
#
# THE RULES
#
# (a) A table t-row with df and no printed n may bind the document-global N
#     ONLY when that N is df-compatible (equal to df + 1 or df + 2) -- an
#     exact-match window, so a scraped garbage N (the jesp.2009 class) cannot
#     slip in. The binding is labeled N_source = "global_text" like every
#     other global binding.
# (b) When N was inferred from df (no external source), the published
#     N_source says so: "df_inferred" -- never NA, never "not_found".
# (c) When the FINAL design label is still "ambiguous" and N is df-inferred,
#     the row's uncertainty_reasons must name BOTH candidates (df + 1 paired /
#     df + 2 independent) so the published N reads as the labeled assumption
#     it is, not an observation.

.tbl_row <- function(t, df, lo = NULL, hi = NULL) {
  fields <- list(t = list(t), df = list(df))
  if (!is.null(lo)) fields$CI_lower <- list(lo)
  if (!is.null(hi)) fields$CI_upper <- list(hi)
  list(list(
    label = "Table 8: Importance (Replication Effect)",
    row_label = "3A: Insight into self (Replication)",
    row_idx = 1L,
    fields = fields
  ))
}

test_that("table t-row binds a df-compatible global N (paired-consistent, df+1)", {
  txt <- paste0(
    "A total of N = 743 participants completed the study. ",
    "The analyzed sample was N = 743 after exclusions."
  )
  res <- effectcheck::check_text(txt, table_rows = .tbl_row(6.79, 742))
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]
  # Paper states 743 = df + 1: two independent numbers agree exactly, so the
  # stated N is adopted (not the internal df + 2 = 744).
  expect_equal(suppressWarnings(as.numeric(row$N[1])), 743)
  expect_equal(as.character(row$N_source[1]), "global_text")
  expect_equal(as.character(row$design_inferred[1]), "ambiguous")
})

test_that("table t-row binds a df-compatible global N (independent-consistent, df+2)", {
  txt <- paste0(
    "A total of N = 743 participants completed the study. ",
    "The analyzed sample was N = 743 after exclusions."
  )
  res <- effectcheck::check_text(txt, table_rows = .tbl_row(3.93, 741))
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]
  # 743 = df + 2 here: compatible with the independent reading; adopted and
  # LABELED (previously the same number appeared with N_source = NA by luck).
  expect_equal(suppressWarnings(as.numeric(row$N[1])), 743)
  expect_equal(as.character(row$N_source[1]), "global_text")
})

test_that("incompatible global N does not bind to a table t-row; df-inferred N is labeled", {
  txt <- paste0(
    "A total of N = 500 participants completed the study. ",
    "The analyzed sample was N = 500 after exclusions."
  )
  res <- effectcheck::check_text(txt, table_rows = .tbl_row(6.79, 742))
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]
  # 500 is not in {743, 744} -> must NOT bind; N falls back to the df
  # inference, which must be LABELED as such and name both candidates.
  expect_equal(suppressWarnings(as.numeric(row$N[1])), 744)
  expect_equal(as.character(row$N_source[1]), "df_inferred")
  expect_match(as.character(row$uncertainty_reasons[1]), "df \\+ 1 = 743")
  expect_match(as.character(row$uncertainty_reasons[1]), "df \\+ 2 = 744")
})

test_that("prose t-test with no N anywhere publishes a labeled df-inferred N with both candidates", {
  txt <- "The effect was significant, t(868) = 2.79, p = .006."
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]
  expect_equal(suppressWarnings(as.numeric(row$N[1])), 870)
  # Previously N_source said "not_found" while N = 870 sat next to it -- a
  # self-contradiction (the N WAS found: it was inferred from df).
  expect_equal(as.character(row$N_source[1]), "df_inferred")
  expect_match(as.character(row$uncertainty_reasons[1]), "df \\+ 1 = 869")
})

test_that("a paired row reconciled to df+1 is also labeled df_inferred", {
  filler <- paste(rep("Filler describing the procedure at length. ", 40),
                  collapse = "")
  txt <- paste0(
    filler,
    "A paired-samples t-test showed a difference, t(49) = 3.10, p = .003."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]
  design <- as.character(row$design_inferred[1])
  if (!is.na(design) && design == "paired") {
    expect_equal(suppressWarnings(as.numeric(row$N[1])), 50)
    expect_equal(as.character(row$N_source[1]), "df_inferred")
  }
})

test_that("an explicitly stated inline N keeps its own N_source (no relabel)", {
  txt <- "Groups differed, t(48) = 2.31, p = .025, d = 0.66, N = 50."
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  row <- t_rows[1, ]
  expect_equal(suppressWarnings(as.numeric(row$N[1])), 50)
  expect_false(identical(as.character(row$N_source[1]), "df_inferred"))
})
