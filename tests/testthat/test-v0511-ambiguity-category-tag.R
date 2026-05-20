test_that("F(1,df)+d emits [category: structural-design] in ambiguity_reason", {
  r <- check_text("F(1, 30) = 5.00, p = .033, d = 0.60")
  df <- as.data.frame(r)
  expect_equal(nrow(df), 1)
  expect_true(df$design_ambiguous)
  expect_equal(df$ambiguity_level, "ambiguous")
  expect_true(
    grepl("[category: structural-design]", df$ambiguity_reason, fixed = TRUE),
    info = "F(1,30)+d is Phase 8A-bis paired-vs-independent — must be tagged structural-design"
  )
  expect_false(
    grepl("[category: cross-family]", df$ambiguity_reason, fixed = TRUE),
    info = "F(1,30)+d must NOT also carry the cross-family tag (the two are mutually exclusive)"
  )
  # The existing substring used by other code paths must remain intact
  expect_true(grepl("Multiple variants match similarly", df$ambiguity_reason, fixed = TRUE))
})

test_that("F(2,df)+d emits [category: cross-family] in ambiguity_reason", {
  r <- check_text("F(2, 30) = 5.00, p = .013, d = 0.60")
  df <- as.data.frame(r)
  expect_equal(nrow(df), 1)
  expect_true(df$design_ambiguous)
  expect_equal(df$ambiguity_level, "highly_ambiguous")
  expect_true(
    grepl("[category: cross-family]", df$ambiguity_reason, fixed = TRUE),
    info = "F(2,30)+d has no same-type d variants — must be tagged cross-family"
  )
  expect_false(
    grepl("[category: structural-design]", df$ambiguity_reason, fixed = TRUE),
    info = "F(2,30)+d is NOT a Phase 8A-bis structural-design case"
  )
  # The substring used by check.R:3415's cross-type fallback guard must remain
  expect_true(grepl("No same-type", df$ambiguity_reason, fixed = TRUE))
})

test_that("F(3,df)+d emits [category: cross-family] in ambiguity_reason", {
  r <- check_text("F(3, 60) = 5.00, p = .002, d = 0.60")
  df <- as.data.frame(r)
  expect_equal(nrow(df), 1)
  expect_true(df$design_ambiguous)
  expect_true(grepl("[category: cross-family]", df$ambiguity_reason, fixed = TRUE))
})

test_that("clear rows carry no category tag", {
  # A simple t-test where the reported d matches d_ind cleanly — no ambiguity
  r <- check_text("An independent-samples t-test, t(48) = 2.50, p = .016, Cohen's d = 0.72, n1 = 25, n2 = 25.")
  df <- as.data.frame(r)
  expect_equal(nrow(df), 1)
  if (!df$design_ambiguous) {
    expect_false(
      grepl("[category:", df$ambiguity_reason, fixed = TRUE),
      info = "A clear-status row must not carry a category tag"
    )
  }
})

test_that("category tag is idempotent (running check twice doesn't double-tag)", {
  # Defensive — the append rule guards on grepl("[category:") to refuse a
  # second append. Re-checking the same text through check_text() should
  # produce exactly one tag in ambiguity_reason.
  txt <- "F(2, 30) = 5.00, p = .013, d = 0.60"
  r1 <- as.data.frame(check_text(txt))
  r2 <- as.data.frame(check_text(txt))
  n1 <- length(regmatches(r1$ambiguity_reason,
                          gregexpr("[category:", r1$ambiguity_reason, fixed = TRUE))[[1]])
  n2 <- length(regmatches(r2$ambiguity_reason,
                          gregexpr("[category:", r2$ambiguity_reason, fixed = TRUE))[[1]])
  expect_equal(n1, 1L)
  expect_equal(n2, 1L)
  expect_equal(r1$ambiguity_reason, r2$ambiguity_reason)
})
