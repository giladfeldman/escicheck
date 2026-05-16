# Regression test for v0.4.2 -- r-test must not adopt a foreign Cohen's d.
#
# Defect found by the escicheck-iterate pilot (2026-05-16) on the real docpluck
# extraction of Chen et al. (2023, Collabra) experiential-purchases replication.
# The abstract states two different analyses in adjacent clauses across a line
# break: a between-subjects effect "d=0.39[0.25, 0.54]" and a correlation
# "r=-.34[-.43, -.24]". The sub-chunk splitter only splits on test statistics,
# so the stray d stayed in the r-test's chunk, and the effect-size extractor
# adopted d=0.39 as the r-test's "reported effect" -- one row conflating the r
# from the second clause with the d from the first. A correlation's effect size
# IS r itself; parse.R now refuses any Cohen's-d-family token for an r-test.

test_that("v0.4.2: an r-test does not adopt a foreign d from another clause (Chen 2023 Collabra abstract)", {
  txt <- paste0(
    "Self-concept was more strongly associated with experiential purchases than ",
    "with material purchases (d=0.39[0.25, 0.54]), and that there was a negative ",
    "association between experiential purchase satisfaction and the willingness ",
    "to exchange memory\n(r=-.34[-.43, -.24]) (all effects above were p<.001)."
  )
  r <- check_text(txt)
  row <- r[r$test_type == "r" & !is.na(r$stat_value), ]
  expect_equal(nrow(row), 1)
  expect_lt(abs(row$stat_value[1] - (-0.34)), 1e-6)
  # the conflation bug set reported_type = "d", effect_reported = 0.39
  expect_false(isTRUE(row$reported_type[1] == "d"))
  expect_false(isTRUE(abs(row$effect_reported[1] - 0.39) < 1e-6))
})

test_that("v0.4.2: a t-test still adopts its co-reported Cohen's d (no regression)", {
  # The d-family guard fires only for r-tests. A t-test reporting a d must still
  # pair with it -- otherwise the fix would silently strip every t-test effect.
  r <- check_text("Groups differed, t(48) = 2.10, p = .041, d = 0.59.")
  row <- r[r$test_type == "t" & !is.na(r$stat_value), ]
  expect_equal(nrow(row), 1)
  expect_identical(row$reported_type[1], "d")
  expect_lt(abs(row$effect_reported[1] - 0.59), 1e-6)
})

test_that("v0.4.2: a plain correlation with df is unaffected", {
  # An r-test with no foreign effect token must parse exactly as before.
  r <- check_text("Satisfaction correlated with exchange index, r(741) = -0.43, p < .001.")
  row <- r[r$test_type == "r" & !is.na(r$stat_value), ]
  expect_equal(nrow(row), 1)
  expect_lt(abs(row$stat_value[1] - (-0.43)), 1e-6)
  expect_false(isTRUE(row$reported_type[1] == "d"))
})
