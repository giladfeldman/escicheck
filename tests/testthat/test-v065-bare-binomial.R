# v0.6.5: bare binomial test reported WITHOUT Cohen's h (and without in-sentence
# n/N counts) should still be extracted as a test_type = "binomial" row that
# routes to an extraction-only NOTE, rather than being dropped entirely.
#
# Surfaced by the 2026-06-21 escicheck-iterate canary audit against
# collabra.77859 (Less is Better): two binomial tests --
#   Study 1: "...preferred the scarf (63%) to the coat (37%; binomial: p = .002)"
#   Study 4: "...most participants (59%) ... binomial test: p = .047"
# -- were PARSE-MISSes because the v0.6.2 binomial pattern requires a co-occurring
# Cohen's h. The bare form carries no effect size or counts in the sentence, so the
# row cannot be verified; it is surfaced as a NOTE for completeness.

test_that("bare 'binomial: p = X' (no Cohen's h) is extracted as a binomial NOTE", {
  txt <- paste0(
    "Counter to expectations, participants in the joint condition preferred ",
    "the scarf (63%) to the coat (37%; binomial: p = .002)."
  )
  res <- effectcheck::check_text(txt)
  b <- res[!is.na(res$test_type) & res$test_type == "binomial", ]
  expect_true(nrow(b) >= 1)
  expect_equal(as.numeric(b$p_reported[1]), 0.002, tolerance = 1e-6)
  # No effect size or counts -> not independently verifiable -> no hard ERROR/WARN.
  expect_false(any(b$status %in% c("ERROR", "WARN"), na.rm = TRUE))
})

test_that("bare 'binomial test: p = X' form is extracted", {
  txt <- paste0(
    "In the joint evaluation condition, most participants (59%) were willing ",
    "to pay more for the larger set than for the smaller one (41%), ",
    "binomial test: p = .047."
  )
  res <- effectcheck::check_text(txt)
  b <- res[!is.na(res$test_type) & res$test_type == "binomial", ]
  expect_true(nrow(b) >= 1)
  expect_equal(as.numeric(b$p_reported[1]), 0.047, tolerance = 1e-6)
})

test_that("two distinct bare binomials in one document are not collapsed by dedup", {
  # Both bare binomials have stat_value/df/N/CI all NA, so the v0.5.14 dedup key
  # (which omits p) would collapse them; v0.6.5 adds p to the key for thin rows.
  txt <- paste0(
    "In Study 1, participants preferred A (63%) to B (37%; binomial: p = .002). ",
    "Much later, in Study 4, most participants (59%) preferred the larger set ",
    "to the smaller one (41%), binomial test: p = .047."
  )
  res <- effectcheck::check_text(txt)
  b <- res[!is.na(res$test_type) & res$test_type == "binomial", ]
  expect_equal(nrow(b), 2L)
  expect_setequal(round(as.numeric(b$p_reported), 3), c(0.002, 0.047))
})

test_that("the binomial-with-Cohen's-h path (v0.6.2) still takes precedence", {
  # When Cohen's h IS present, the row must keep h as its effect size (not be
  # captured as a bare binomial), preserving v0.6.2 behaviour.
  txt <- paste0(
    "A clear majority chose the target, 85 out of 134, binomial p = .002, ",
    "Cohen's h = 0.46."
  )
  res <- effectcheck::check_text(txt)
  b <- res[!is.na(res$test_type) & res$test_type == "binomial", ]
  expect_true(nrow(b) >= 1)
  expect_true(any(b$effect_reported_name == "h", na.rm = TRUE))
})
