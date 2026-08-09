# v0.7.5 -- Issue D (the discarded permutation p) and the significance-legend
# defect that Issue D uncovered on the same paper.
#
# ISSUE D. A clause can report two p-values of different provenance:
#   "t(2037) = -3.26, P = 0.001, P-permutation = 0.002"
# `pat_p` binds the parametric 0.001 -- it cannot see the hyphenated form,
# because the hyphen breaks its operator adjacency -- and 0.002 was discarded.
# A reader of the output could not tell a permutation p had been reported at
# all. It now lands in `p_reported_secondary`, a sibling COLUMN: a second ROW
# would change nrow() for every consumer and silently shift every downstream
# index, and MetaESCI's field registry is frozen at v0.4.0, so it already
# tolerates unknown columns and cannot tolerate unknown rows.
#
# THE LEGEND DEFECT. Found while verifying Issue D against the source. PNAS
# 10.1073/pnas.2404157121's figure caption is merged into the body text by the
# extractor, ending "...***P < 0.001." and continuing in LOWERCASE, so the chunk
# splitter cannot separate them. pat_p took the first p in the merged chunk and
# the row published
#     t(2037) = -2.19, p_reported = 0.1        (status WARN)
# where the paper prints P = 0.029 (and 2*pt(-2.19, 2037) = 0.02864, so the
# correct value is consistent and the row is OK). A threshold from an asterisk
# key, attached to a real published statistic, with no flag.
#
# Whole-corpus diff over 48 real-article texts / 764 rows: 0 rows gained, 0 lost,
# THREE changed -- and all three are this same defect in three different papers:
#   pnas_cognitive_memory_2024   loc 147  p 0.1   -> 0.029  (WARN -> OK)
#   frontiers_retrocue_2024      loc 386  p 0.05  -> 0.271
#   scireports_exercise_2025     loc 178  p 0.01  -> 0.021
# Each new value was checked against the article text before this was committed.

test_that("v0.7.5: a glued qualified p is recovered, not discarded", {
  txt <- "C-noncued versus U-noncued: t(2037) = -3.26, P = 0.001, P-permutation = 0.002."
  r <- check_text(txt)
  expect_equal(nrow(r), 1L)
  # The primary stays the PARAMETRIC value -- that is the v0.7.3 ruling and it
  # is provable: a glued qualifier is invisible to pat_p, so whatever pat_p
  # bound is necessarily the unqualified one.
  expect_equal(r$p_reported[1], 0.001)
  expect_false(isTRUE(r$p_reported_is_resampling[1]))
  # ... and the permutation p is now visible.
  expect_equal(r$p_reported_secondary[1], 0.002)
  expect_equal(r$p_secondary_symbol[1], "=")
})

test_that("v0.7.5: a SPACED qualifier does not populate the secondary", {
  # "permutation p = .062" is fully visible to pat_p and may already BE the
  # primary. Capturing it too could publish the same number twice under two
  # provenances, which is worse than dropping it. Only the glued form is
  # provably not-the-primary.
  r <- check_text("t(58) = 2.31, permutation p = 0.062, d = 0.61.")
  expect_true(all(is.na(r$p_reported_secondary)))
})

test_that("v0.7.5: the floor check retargets to the resampling p", {
  # B = 200 -> floor 1/(200+1) = 0.004975. The PERMUTATION p (0.002) is below
  # it; the parametric p (0.001) is not the value the floor is about. Before
  # v0.7.5 the whole block was skipped on this row, because p_reported_is_
  # resampling is FALSE -- the caveat was inert on the only dual-p paper in the
  # corpus, which is also the only paper that declares a resample count.
  txt <- paste(
    "Results", "",
    "The contrast held, t(198) = 3.41, P = 0.001, P-permutation = 0.002, d = 0.48.", "",
    "Methods", "",
    "For permutation tests, two hundred random shuffles of labels were sampled.",
    sep = "\n")
  r <- check_text(txt)
  expect_equal(r$p_reported_secondary[1], 0.002)
  expect_equal(r$resampling_B[1], 200)
  expect_true(isTRUE(r$resampling_p_below_floor[1]))
  msg <- paste(unlist(r$uncertainty_reasons), collapse = " ")
  # The message must name the value it is about. "Reported p" would point the
  # reader at 0.001 while the arithmetic is about 0.002.
  expect_match(msg, "Reported permutation p (0.002)", fixed = TRUE)
})

test_that("v0.7.5: a significance legend is not a reported p", {
  # The exact PNAS shape: caption legend, sentence-ending period, LOWERCASE
  # continuation, then the real statistic.
  txt <- paste0(
    "~P < 0.1, *P < 0.05, **P < 0.01, ***P < 0.001. higher than that of NR, ",
    "U-noncued and C-cued items (C-noncued versus NR: t(2037) = -2.19, ",
    "P = 0.029, P-permutation = 0.001).")
  r <- check_text(txt)
  expect_equal(nrow(r), 1L)
  expect_equal(r$p_reported[1], 0.029)   # the paper's value, not the legend's
  expect_equal(r$p_reported_secondary[1], 0.001)
})

test_that("v0.7.5: every legend marker and both spacings are refused", {
  # `#` is the marker in scireports_exercise_2025 ("T1, # P<0.01 vs. control
  # condition. found (F(1, 30)=5.91, P=0.021, ...)"), `*` in
  # frontiers_retrocue_2024, `~` in the PNAS caption.
  for (marker in c("*", "~", "+", "#")) {
    for (gap in c("", " ")) {
      txt <- paste0(marker, gap, "p < 0.05. the effect held, t(58) = 2.31, p = 0.024, d = 0.61.")
      r <- check_text(txt)
      expect_equal(r$p_reported[1], 0.024,
                   info = paste0("marker '", marker, "', gap '", gap, "'"))
    }
  }
})

test_that("v0.7.5: an ordinary p is untouched by the legend guard", {
  # The guard must not cost a legitimate p. A p preceded by a word character,
  # a space, a comma or an open paren is a result.
  expect_equal(check_text("t(58) = 2.31, p = 0.024, d = 0.61.")$p_reported[1], 0.024)
  expect_equal(check_text("t(58) = 2.31 (p = 0.024), d = 0.61.")$p_reported[1], 0.024)
  expect_equal(check_text("t(58) = 2.31; p < 0.001; d = 0.61.")$p_reported[1], 0.001)
})
