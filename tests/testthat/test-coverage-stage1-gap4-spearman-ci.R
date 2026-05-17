# Stage 1 / Gap 4 -- Spearman as an alternative CI method within the
# correlation-r check.
#
# A bare r(df) defaults to Pearson. When a paper used Spearman's rho but
# declared it only in a distant Methods section, the reported CI is a Spearman
# (Bonett-Wright) interval and used to draw a spurious CI mismatch against the
# Pearson Fisher-z interval. The fix offers the Spearman interval as an extra
# method in the CI candidate pool so Phase 6 multi-method matching accepts it,
# WITHOUT any document-level reclassification (which would false-positive on
# papers mixing Pearson and Spearman). The row stays labelled Pearson r.

test_that("Gap 4: a Pearson CI on a bare r(df) still matches Pearson", {
  pe <- ci_r(0.50, 22, 0.95)
  txt <- sprintf("Variables were correlated, r(20) = 0.50, 95%% CI [%.2f, %.2f], p = .018.",
                 pe$bounds[1], pe$bounds[2])
  res <- check_text(txt)
  row <- res[res$test_type == "r", ]
  expect_equal(nrow(row), 1)
  expect_true(isTRUE(row$ci_match[1]))
  expect_true(grepl("pearson", row$ci_method_match[1]))
})

test_that("Gap 4: a Spearman CI on a bare r(df) matches via the Spearman method", {
  # No "Spearman" cue near the statistic -> the row stays labelled Pearson r,
  # but the reported Spearman-method CI is accepted by the candidate pool.
  sc <- ci_spearman(0.50, 22, 0.95)
  txt <- sprintf("Variables were correlated, r(20) = 0.50, 95%% CI [%.2f, %.2f], p = .018.",
                 sc$ci_low, sc$ci_high)
  res <- check_text(txt)
  row <- res[res$test_type == "r", ]
  expect_equal(nrow(row), 1)
  expect_true(isTRUE(row$ci_match[1]))
  expect_true(grepl("spearman", row$ci_method_match[1]))
  # No relabeling: it is still reported as a (Pearson) correlation, not spearman.
  expect_equal(row$test_type[1], "r")
})

test_that("Gap 4 guard: an explicit Spearman context still reclassifies", {
  # The Stage 1 P2 near-statistic reclassification must be untouched.
  res <- check_text("A Spearman correlation was computed, r(20) = 0.50, p = .018.")
  row <- res[!is.na(res$test_type), ]
  expect_true(any(row$test_type == "spearman"))
})
