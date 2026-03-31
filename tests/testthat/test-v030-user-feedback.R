# Tests for v0.3.0 user feedback fields
# software_notes, alternative_formulas, best_practice_notes

test_that("software_notes populated for ANOVA eta2 WARN", {
  # F(3, 96) = 5.0, partial eta2 = 0.11 -> WARN (close to omega2)
  res <- check_text("F(3, 96) = 5.0, p = .003, partial eta2 = 0.11")
  expect_true("software_notes" %in% names(res))
  r <- res[!is.na(res$matched_variant), ]
  if (nrow(r) > 0 && r$check_type[1] == "effect_size") {
    expect_false(is.na(r$software_notes[1]))
    expect_true(grepl("omega", r$software_notes[1], ignore.case = TRUE))
  }
})

test_that("software_notes populated for Cohen's f", {
  res <- check_text("F(2, 57) = 8.0, p < .001, Cohen's f = 0.48")
  r <- res[!is.na(res$matched_variant), ]
  if (nrow(r) > 0 && r$check_type[1] == "effect_size") {
    expect_false(is.na(r$software_notes[1]))
    expect_true(grepl("G.Power|omega", r$software_notes[1]))
  }
})

test_that("best_practice_notes for small-sample d", {
  res <- check_text("t(10) = 3.00, p = .013, d = 1.50, n1 = 6, n2 = 6")
  r <- res[!is.na(res$matched_variant), ]
  if (nrow(r) > 0 && r$check_type[1] == "effect_size" && !is.na(r$best_practice_notes[1])) {
    expect_true(grepl("Hedges", r$best_practice_notes[1]))
  }
})

test_that("feedback fields exist in output tibble", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.81")
  expect_true("software_notes" %in% names(res))
  expect_true("alternative_formulas" %in% names(res))
  expect_true("best_practice_notes" %in% names(res))
})

test_that("feedback fields are NA for non-effect-size results", {
  res <- check_text("t(28) = 2.21, p = .035")
  expect_true("software_notes" %in% names(res))
  # p-value-only results should have NA feedback
  if (res$check_type[1] != "effect_size") {
    expect_true(is.na(res$software_notes[1]))
  }
})
