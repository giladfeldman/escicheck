# v0.6.17 -- when a t-test states its df, a document-level (`global_text`) N may
# never override the df-implied N, no matter how close the two happen to be.
#
# Source: 2026-08-04 escicheck-iterate cycle 1. Found by the blast-radius diff
# (tmp/iterate/diff_renders.py) after that gate was extended to include N --
# without N in its verdict tuple the regression below was invisible and would
# have shipped.
#
# WHAT WENT WRONG
#
# check.R's global-N override (~L1209) correctly says "the df is structurally
# authoritative (independent N = df+2, paired N = df+1)", but only fires when
#
#     N > max_expected_N + 10        # i.e. N > df + 12
#
# The +10 slack means a global-text N that is wrong by a SMALL amount is kept.
# It also has to clear the minimum-N guard at N >= df+1, so any global N in
# [df+1, df+12] sails through both checks untouched.
#
# On 10.1016/j.jesp.2009.12.010 the Study 1 t-tests report df = 32, so the true
# independent N is df+2 = 34 (confirmed by the AI gold: n_total = 34). The
# paper's global-text N is 38 -- a subgroup table cell. 38 >= 33 clears the
# minimum-N guard and 38 <= 44 stays under the override threshold, so N = 38 is
# kept and every effect size is computed from it: g_ind comes out 1.0482 where
# the correct N = 34 gives 1.1052.
#
# This was latent but harmless until v0.6.17's companion parse.R fix: before it,
# this paper's global N was 7, which FAILED the minimum-N guard and so was
# rejected and correctly re-derived as 34 from df. Raising the global N to a
# more plausible 38 moved it from "obviously broken, caught by the safety net"
# to "plausible, silently kept" -- a wrong number that no guard fires on. A fix
# that improves one row can degrade another; only the corpus diff shows it.
#
# THE RULE: with a known df, N is determined to within one unit (df+1 paired vs
# df+2 independent). A scraped document-level N is a last-resort guess and must
# never outrank it.

test_that("a global-text N incompatible with df is replaced by the df-based N", {
  # df = 32 -> independent N must be 34. A global-text 38 is close enough to
  # clear the old +10 slack, and must still lose to df.
  txt <- paste0(
    "Rejecters only. Positive Expectations N = 38. Neutral Expectations N = 38. ",
    paste(rep("Filler describing the procedure at some length. ", 40), collapse = ""),
    "Participants in the positive-expectations condition offered more, ",
    "t(32) = 3.30, p = .002."
  )

  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)

  row <- t_rows[1, ]
  n_bound <- suppressWarnings(as.numeric(row$N[1]))
  n_source <- as.character(row$N_source[1])

  if (!is.na(n_source) && n_source == "global_text") {
    expect_true(
      n_bound %in% c(33, 34),
      info = paste0(
        "t(32) bound a global-text N of ", n_bound,
        " -- df fixes N at 33 (paired) or 34 (independent), so a scraped ",
        "document-level N must not survive."
      )
    )
  }
})

test_that("a WELCH row's global-text N is NOT overridden by df", {
  # Cross-model review (codex, 2026-08-04) claimed the v0.6.17 df-authority
  # override could clobber a legitimately larger N on a Welch row, where the
  # Welch-Satterthwaite df is non-integer and true N can exceed df+2.
  #
  # REFUTED by reproduction: the edited branch sits inside the `else` of
  # `if (is_welch)`, so a Welch row never reaches it. Pinned here so a later
  # audit does not re-raise the claim, and so the routing cannot silently
  # change: if someone hoists the override out of that `else`, this goes red.
  txt <- paste0(
    "The final sample was N = 800. ",
    paste(rep("Filler describing the procedure at some length. ", 40), collapse = ""),
    "Groups differed, t(500.34) = 3.10, p = .002."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  expect_equal(suppressWarnings(as.numeric(t_rows$N[1])), 800)
})

test_that("a df-compatible global-text N is still accepted", {
  # N = 34 IS df+2 for df = 32 -- nothing to override.
  txt <- paste0(
    "The final sample was N = 34. ",
    paste(rep("Filler describing the procedure at some length. ", 40), collapse = ""),
    "Participants offered more, t(32) = 3.30, p = .002."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  expect_equal(suppressWarnings(as.numeric(t_rows$N[1])), 34)
})
