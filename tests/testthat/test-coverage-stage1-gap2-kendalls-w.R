# Stage 1 / Gap 2 -- Kendall's W must not be misparsed as Wilcoxon's W.
#
# The bare "W =" token is shared by Wilcoxon's W (a large rank-sum) and
# Kendall's W (the coefficient of concordance, bounded 0-1). `pat_W` in parse.R
# grabbed "W = 0.30" and labelled it test_type = "W" (Wilcoxon), matched_variant
# = NA. The validation against Frank et al. (2025) surfaced this on 4+ results.
# A W in [0, 1] reported in a "Kendall" / "concordance" context is Kendall's W.

test_that("Gap 2: Kendall's W in a concordance context is labelled kendall_w", {
  # Real verbatim from the Frank et al. (2025) PCI-RR replication.
  txt <- paste(
    "We calculated Kendall's coefficient of concordance for perceived benefit",
    "and perceived risk and found low to moderate inter-participant agreement:",
    "W = 0.30, p < .001 for benefit raters and W = 0.39, p < .001 for risk raters."
  )
  res <- check_text(txt)
  row <- res[res$test_type == "kendall_w", ]
  expect_equal(nrow(row), 2)
  expect_setequal(round(row$stat_value, 2), c(0.30, 0.39))
  # The W value is itself the reported effect size (coefficient of concordance).
  expect_true(all(row$reported_type == "kendalls_W"))
  expect_setequal(round(row$effect_reported, 2), c(0.30, 0.39))
  # No Friedman chi-square / rater counts -> honest "cannot verify", not ERROR.
  expect_true(all(row$status == "NOTE"))
  # Must NOT be misparsed as Wilcoxon's W.
  expect_false(any(res$test_type == "W"))
})

test_that("Gap 2: an explicit 'Kendall's W =' is labelled kendall_w", {
  res <- check_text("Kendall's W = 0.22, p < .001 indicated low agreement.")
  row <- res[res$test_type == "kendall_w", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$stat_value[1], 0.22, tolerance = 1e-6)
  expect_equal(row$reported_type[1], "kendalls_W")
})

test_that("Gap 2 guard: a genuine Wilcoxon W is still labelled W", {
  # Large rank-sum + explicit "Wilcoxon" -> must stay Wilcoxon's W.
  res <- check_text("A Wilcoxon signed-rank test was significant, W = 462, p = .003.")
  row <- res[!is.na(res$test_type), ]
  expect_true(any(row$test_type == "W"))
  expect_false(any(row$test_type == "kendall_w"))
})

test_that("Gap 2 guard: a bare large W with no Kendall context stays Wilcoxon", {
  # No "Kendall"/"concordance" cue and W > 1 -> the conservative default (W).
  res <- check_text("The rank-sum statistic was W = 318, p = .012.")
  expect_true(any(res$test_type == "W"))
  expect_false(any(res$test_type == "kendall_w"))
})
