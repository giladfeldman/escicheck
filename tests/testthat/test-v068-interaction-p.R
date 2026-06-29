# v0.6.8 (E-interaction-p): a bare "p-value for interaction <op>? <pval>" report --
# a subgroup / moderation interaction test carrying ONLY a p, with no F statistic,
# no df, and no effect size (the interaction F is typically in a supplementary table
# not the main PDF) -- is extracted as test_type = "interaction_p" and routed to an
# extraction-only NOTE (the p is surfaced; nothing is independently recomputable).
#
# Surfaced by the 2026-06-29 escicheck-iterate cycle-1 canary audit against
# 10.1371/journal.pmed.1004323 (PLOS Medicine PROSECCO trial): "significant subgroup
# effects were demonstrated for parity (p-value for interaction 0.029; Table B in S1
# Text)" produced no row (PARSE-MISS) at v0.6.7.

test_that("a bare 'p-value for interaction' is extracted as an interaction_p NOTE", {
  txt <- paste0(
    "In the subgroup analyses performed for parity and fibroid size, significant ",
    "subgroup effects were demonstrated for parity (p-value for interaction 0.029; ",
    "Table B in S1 Text)."
  )
  res <- effectcheck::check_text(txt)
  ip <- res[!is.na(res$test_type) & res$test_type == "interaction_p", ]
  expect_equal(nrow(ip), 1L)
  expect_equal(as.numeric(ip$p_reported[1]), 0.029)
  expect_true(ip$status[1] == "NOTE")
  # No fabricated test statistic or effect size.
  expect_true(is.na(ip$stat_value[1]))
})

test_that("the 'interaction p = X' and 'p_interaction X' variants also parse", {
  for (txt in c(
    "The moderation was reliable, interaction p = .012.",
    "We report the moderation as p-interaction 0.04.",
    "The test was, interaction p-value < .001."
  )) {
    res <- effectcheck::check_text(txt)
    ip <- res[!is.na(res$test_type) & res$test_type == "interaction_p", ]
    expect_true(nrow(ip) >= 1)
    expect_true(any(ip$status == "NOTE", na.rm = TRUE))
  }
})

test_that("a bare interaction_p row carries NO sample size", {
  # A bare interaction p has no sample-size semantics. Any N bled from an adjacent
  # table header (e.g. "PSA N = 98") must be cleared -- it is not this test's N and
  # nothing uses it. prosecco: the interaction_p row had inherited N = 98.
  txt <- paste0(
    "In the PSA condition (N = 98 patients), significant subgroup effects were ",
    "demonstrated for parity (p-value for interaction 0.029)."
  )
  res <- effectcheck::check_text(txt)
  ip <- res[!is.na(res$test_type) & res$test_type == "interaction_p", ]
  expect_true(nrow(ip) >= 1)
  expect_true(is.na(ip$N[1]))
})

test_that("an interaction reported WITH its F binds the F, not the bare interaction_p", {
  # When the interaction's F statistic IS reported, the row must be an F-test (the
  # bare interaction_p branch is reached only when no test statistic matched).
  txt <- paste0(
    "We found a significant interaction between condition and time, ",
    "F(2, 198) = 4.51, p = .012, partial eta-squared = 0.04."
  )
  res <- effectcheck::check_text(txt)
  expect_true(any(res$test_type == "F", na.rm = TRUE))
  expect_false(any(res$test_type == "interaction_p", na.rm = TRUE))
})
