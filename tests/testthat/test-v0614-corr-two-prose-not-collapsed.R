# v0.6.14 (E-corr-two-prose): the v0.5.14 body-vs-table-fragment r-dedup must NOT
# collapse two DISTINCT prose correlations that coincidentally share the same r,
# df, and N and carry no CI to tell them apart.
#
# The v0.5.14 dedup was built to collapse a body-text correlation (`r(741) = -.43,
# 95% CI [...]`, parenthesized) against its TABLE-FRAGMENT restatement (`r = -.43
# [...]`, no parenthesized `r(df)` form). Its key is (test_type, stat, df1, df2, N,
# ciL, ciU, effect, effect_name). When a paper reports two genuinely different
# correlations that happen to share all of those AND carry no CI, the key is
# identical and one was silently dropped -- a PARSE-MISS of a real result.
#
# Surfaced by the 2026-07-04 escicheck-iterate corpus-expansion cycle against
# collabra.23443 (Brick et al., Miller & Ratner 1998 replication). Its Study-1
# "communalism" paragraph reports FOUR correlations in two sentences:
#   H2A willingness-to-donate:  paid r(797) = .16, unpaid r(797) = .25
#   H2C estimates-of-others:    paid r(797) = .15, unpaid r(797) = .16
# The H2A-paid `.16` and the H2C-unpaid `.16` are DIFFERENT correlations (different
# variables) that share r = .16, df = 797, N, and report no CI -- so the dedup
# collapsed them and dropped the H2C .16. Both are parenthesized `r(797)` prose
# forms (neither is a table fragment), so the fix keeps every parenthesized body
# row in a key group and only drops non-parenthesized fragment(s).

test_that("two distinct prose r's with the same r/df but no CI are BOTH kept", {
  # Exact docpluck-delivered text from collabra.23443's communalism paragraph.
  txt <- paste0(
    "In Study 1, correlations suggested that more communal individuals were more ",
    "likely to donate in both the paid, r(797) = .16, p < .001, and unpaid ",
    "conditions, r(797) = .25, p < .001 (H2A; point-biserial). Estimates for ",
    "others. Correlations suggested that more communal individuals gave higher ",
    "estimations of others donating blood in the paid, r(797) = .15, p < .001, ",
    "and unpaid conditions, r(797) = .16, p < .001 (H2C; Pearson's r)."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  # All four correlations survive: .15, .16 (H2C), .16 (H2A), .25.
  expect_equal(nrow(rr), 4L)
  vals <- sort(round(as.numeric(rr$stat_value), 2))
  expect_equal(vals, c(0.15, 0.16, 0.16, 0.25))
  # Exactly TWO of them are the r = .16 rows (the collapse would have left one).
  expect_equal(sum(abs(as.numeric(rr$stat_value) - 0.16) < 1e-6), 2L)
})

test_that("the smallest reproducer: two same-df r=.16 prose rows are both kept", {
  txt <- paste0(
    "communal in the paid, r(797) = .16, p < .001, and unpaid conditions, ",
    "r(797) = .16, p < .001 (H2C; Pearson's r)."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_equal(nrow(rr), 2L)
})

test_that("a body-text r and its NON-paren table-fragment restatement STILL collapse", {
  # The original v0.5.14 case must not regress: one parenthesized body row + one
  # bare `r = -.43` table fragment (no `r(df)`) share the key -> collapse to the
  # body row (keeps the parenthesized df).
  txt <- paste0(
    "The correlation was strong, r(741) = -.43, 95% CI [-.49, -.37]. ",
    "Table 8: r = -.43 [-.49, -.37]"
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_equal(nrow(rr), 1L)
  # The surviving row is the parenthesized body row (df from r(741)).
  expect_equal(as.numeric(rr$df1[1]), 741)
})

test_that("two same-r rows WITH DIFFERENT CIs remain separate (unchanged behavior)", {
  # Two correlations with the same r but different CI bounds were already kept by
  # the v0.5.14 key (CI is part of the key). Confirm the fix leaves this intact.
  txt <- paste0(
    "H1a was r(261) = 0.45, 95% CI [.35, .55]. H2a was r(261) = 0.45, ",
    "95% CI [.35, .54]."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "r", ]
  expect_equal(nrow(rr), 2L)
})
