# v0.6.8 (E-D-dedup): the v0.6.7 test-statistic-bearing dedup
# (.dedup_table_vs_prose stat_ci_dup) collapses a typed table F/t/r row against a
# prose row when their (stat_value, CI) match, with df AND p excluded from the key.
# That over-reaches when two GENUINELY-DISTINCT findings share an F and a rounded CI
# but differ in their reported p. The reported p is now part of the key, so two
# rows with the same F + CI but different p stay separate, while the intended
# glyph-stripped-prose collapse (whose p's agree) is preserved.
#
# Surfaced by the 2026-06-29 escicheck-iterate cycle-1 canary audit against
# collabra.90203: H2b (donations interaction, F(2,998)=1.48, p=.228, eta2p=.003,
# CI[0,.012]) was deduped away against H6 (perceived-impact interaction,
# F(2,998)=1.48, p=.229, CI[0,.012]) -- two different hypotheses with the same F
# and rounded CI but p .228 vs .229.

test_that("two findings with same F+CI but different p are NOT collapsed", {
  # A prose F-test (H6: p = .229) and a typed table F-row (H2b: p = .228) with the
  # same F and the same rounded CI must both survive: they are distinct hypotheses.
  prose <- paste0(
    "We ran a two-way ANOVA and found no interaction between Explicit Learning and ",
    "Identifiability on perceived impact, F(2, 998) = 1.48, p = 0.229, ",
    "partial eta-squared = 0.002, 95% CI [0.00, 0.012]."
  )
  table_rows <- list(list(
    table_id = "camelot_t20", row_label = "Replication",
    fields = list(
      F = list(1.48), eta2 = list(0.003), p = list(0.228), p_op = list("="),
      CI_lower = list(0), CI_upper = list(0.012)
    )
  ))
  res <- effectcheck::check_text(prose, table_rows = table_rows)
  fr <- res[!is.na(res$test_type) & res$test_type == "F" &
              !is.na(res$stat_value) & abs(res$stat_value - 1.48) < 1e-9, ]
  # Both the prose H6 (p=.229) and the table H2b (p=.228) rows must be present.
  expect_equal(nrow(fr), 2L)
  ps <- sort(round(as.numeric(fr$p_reported), 3L))
  expect_equal(ps, c(0.228, 0.229))
})

test_that("a glyph-stripped prose restatement with the SAME p IS still collapsed", {
  # The intended DP-3 collapse: a prose F-row whose eta-squared glyph was stripped
  # (so it carries only F + CI + p) and its typed table restatement (F + eta2 + CI +
  # the SAME p) are the same finding and must collapse to one row.
  prose <- paste0(
    "For the interaction we found F(1, 666) = 0.04, p = 0.842, 95% CI [0.00, 0.01]."
  )
  table_rows <- list(list(
    table_id = "camelot_t22", row_label = "Replication",
    fields = list(
      F = list(0.04), eta2 = list(0), p = list(0.842), p_op = list("="),
      df1 = list(1), df2 = list(114),
      CI_lower = list(0), CI_upper = list(0.01)
    )
  ))
  res <- effectcheck::check_text(prose, table_rows = table_rows)
  fr <- res[!is.na(res$test_type) & res$test_type == "F" &
              !is.na(res$stat_value) & abs(res$stat_value - 0.04) < 1e-9, ]
  # Collapsed to a single row (same F, same CI, same p .842).
  expect_equal(nrow(fr), 1L)
})
