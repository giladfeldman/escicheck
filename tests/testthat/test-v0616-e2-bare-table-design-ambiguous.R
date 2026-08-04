# v0.6.16 (E2 / E-design-label-vs-dz) -- a bare table row whose ONLY matching
# variant is dz must not claim the independent-samples design default.
#
# Surfaced by the 2026-08-03 Sonnet canary audit of collabra.23443
# (escicheck-iterate cycle 5); fixed cycle 8 (2026-08-04). Tables 5 and 7 are
# one-sample t-tests, and the check correctly matches dz -- but the row shipped
# `design_inferred = "independent"` (the bare default), a label that contradicts
# the variant the check itself matched. The verdicts were already right; only the
# metadata lied. Per user decision 2026-08-04 the label becomes "ambiguous"
# rather than flipping to one-sample/paired: a dz match is consistent with BOTH
# one-sample and paired designs, and a genuine independent row can match dz
# coincidentally, so asserting a within-design would overclaim a signal the text
# never delivered.
#
# Authored against the UNFIXED code and watched fail (design_inferred came back
# "independent") before the gate landed.

test_that("v0.6.16 E2: bare Mode B table row matching dz reports design ambiguous", {
  rows <- list(
    list(label = "Table 5: MTurk",   fields = list(t = 16.6, d = 0.59, n = 799)),
    list(label = "Table 5: Prolific", fields = list(t = 16.1, d = 0.57, n = 799))
  )
  res <- check_text("", table_rows = rows)
  expect_equal(nrow(res), 2L)
  expect_true(all(res$matched_variant == "dz"))
  # The label must no longer contradict the matched variant.
  expect_true(all(res$design_inferred == "ambiguous"),
              info = paste("design_inferred was", paste(res$design_inferred, collapse = ", ")))
  expect_false(any(res$design_inferred == "independent"))
  # The reason must be surfaced, never a silent relabel (No-pretending rule).
  expect_true(all(grepl("design reported as ambiguous", res$uncertainty_reasons)))
  # Verdicts were already correct and must be unchanged by a metadata fix.
  expect_true(all(res$status == "PASS"))
})

test_that("v0.6.16 E2: a table row that STATES its design keeps that design", {
  # A table row carrying an explicit design keyword in its own label is not
  # "bare" -- the ambiguation must not fire and erase a real signal.
  rows <- list(
    list(label = "Table 5: paired comparison", fields = list(t = 16.6, d = 0.59, n = 799))
  )
  res <- check_text("", table_rows = rows)
  expect_equal(nrow(res), 1L)
  expect_false(res$design_inferred[1] == "ambiguous")
})

test_that("v0.6.16 E2: a table row stating one-sample via 'against chance' is not ambiguated", {
  # Sonnet cross-model review finding, 2026-08-04 (reproduced before fixing):
  # the first negative guard hardcoded a short keyword list that omitted the
  # "against chance" / "against the midpoint" phrasings that this same branch
  # treats as a DEFINITIVE one-sample signal (one_sample_patterns). A row whose
  # own label states its design that way was therefore treated as "bare" and
  # relabeled ambiguous -- erasing a design the row did state. The guard now
  # reuses the branch's own vocabulary lists rather than a hand-copied subset.
  rows <- list(
    list(label = "Table 5: comparison against chance",
         fields = list(t = 16.6, d = 0.59, n = 799))
  )
  res <- check_text("", table_rows = rows)
  expect_equal(nrow(res), 1L)
  expect_false(res$design_inferred[1] == "ambiguous",
               info = paste("design_inferred was", res$design_inferred[1]))
})

test_that("v0.6.16 E2: repro code for an ambiguous dz row reproduces the dz, not d_ind", {
  # Codex CLI review finding #2, 2026-08-04 -- REPRODUCED before fixing. The
  # v0.6.16 ambiguation left the repro emission keyed only off design_inferred,
  # so an "ambiguous" row whose verdict rests on a matched dz emitted the
  # INDEPENDENT formula `d_ind <- 2 * stat / sqrt(df1)`. On a bare table row
  # (no df) that evaluates to NA -- a user checking our work would run a
  # formula that does not reproduce the number we passed. Same defect class as
  # the v0.6.8 one-sample fix, newly reachable through "ambiguous". The gate now
  # keys off the matched variant as well as the design label.
  rows <- list(
    list(label = "Table 5: MTurk", fields = list(t = 16.6, d = 0.59, n = 799))
  )
  res <- check_text("", table_rows = rows)
  expect_equal(nrow(res), 1L)
  expect_equal(res$design_inferred[1], "ambiguous")
  expect_equal(res$matched_variant[1], "dz")
  expect_match(res$repro_code[1], "dz <- stat / sqrt(N)", fixed = TRUE)
  expect_false(grepl("d_ind <- 2 * stat", res$repro_code[1], fixed = TRUE))
  # The emitted output must show the dz value the verdict used (16.6/sqrt(799)).
  expect_match(res$repro_output[1], "0.5873", fixed = TRUE)
})

test_that("v0.6.16 E2: a fractional-df Welch table row keeps its independent design", {
  # Codex CLI review finding #1, 2026-08-04: the claim was that the bare-table
  # ambiguation could overwrite the v0.6.3 fractional-df Welch reclassification
  # (df1 = 257.03 is a definitive independent-samples signal) and mislabel a
  # genuinely independent row "ambiguous". Probed at HEAD: it does NOT
  # reproduce -- such a row matches an independent-family variant
  # (d_ind_equalN), so the block's dz-family guard already excludes it. Pinned
  # here so a future widening of that guard cannot silently make the claim true.
  rows <- list(
    list(label = "Table 9: condition comparison",
         fields = list(t = 3.42, df = 257.03, d = 0.43, n = 260))
  )
  res <- check_text("", table_rows = rows)
  expect_equal(nrow(res), 1L)
  expect_equal(res$design_inferred[1], "independent")
  expect_false(res$design_inferred[1] == "ambiguous")
})

test_that("v0.6.16 E2: a PROSE row matching dz is untouched by the table-row gate", {
  # The gate is scoped to from_table rows. A prose t-test that matches dz keeps
  # whatever design the prose analysis inferred -- collabra.23443 S1-R13
  # (the E3 row) is prose and must not be ambiguated by this fix.
  txt <- paste0(
    "Price sensitivity predicted higher estimates of self-interest in others, ",
    "t(1596) = -7.67, p < .0001, d = 0.19 [0.14, 0.24]."
  )
  res <- check_text(txt)
  expect_equal(nrow(res), 1L)
  expect_false(res$design_inferred[1] == "ambiguous")
})
