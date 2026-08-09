# v0.7.5 -- Issue C: the resample count B is declared ONCE, in Methods, and the
# Monte Carlo floor check shipped in v0.6.22 could therefore never fire.
#
# MEASURED AT HEAD BEFORE ANY CHANGE (2026-08-09): across the 48-file validation
# corpus (tests/outputs/validation_texts + tmp/iterate/dptext, >3 KB), TEN rows
# carried a `resampling_method` and `resampling_B` was non-NA on ZERO of them.
# So `resampling_p_below_floor` was FALSE everywhere -- not because every paper
# passed, but because the check had no B to test against. A check that cannot
# fire is indistinguishable from one that passes.
#
# The handoff attributed this to the count living in Methods. That is half of
# it. The other half only showed up by reading the paper: PNAS
# 10.1073/pnas.2404157121 does not write "B = 10,000" at all, it writes
#   "For permutation tests, ten thousand random shuffles of labels ... were
#    sampled"
# -- the count is SPELLED OUT. A Methods prescan for a digit form would have
# added a helper, a provenance value and a test, and still bound nothing on the
# one paper it was written for.
#
# Of the four resample-count declarations in the corpus, the pre-v0.7.5
# clause-level scan could read exactly one.

test_that("v0.7.5: the four real corpus declarations all yield their count", {
  # PNAS 10.1073/pnas.2404157121 -- spelled out, with a qualifier between the
  # count and its noun ("ten thousand RANDOM shuffles").
  expect_equal(effectcheck:::.resample_count_in(
    paste("For permutation tests, ten thousand random shuffles of labels among",
          "conscious and unconscious category labels were sampled")), 10000)

  # eLife 10.7554/eLife.87747 -- generic noun, qualifier outside the bracket.
  expect_equal(effectcheck:::.resample_count_in(
    paste("We used cluster-based permutation testing (1000 iterations at a",
          "threshold of 0.05) to correct for multiple comparisons")), 1000)

  # Frontiers -- normalize_text() has already turned "5,000" into "5.000", so
  # the separator stripping is what makes this 5000 rather than 5.
  expect_equal(effectcheck:::.resample_count_in(
    "The bootstrapping process involved 5.000 resamples, and the significance"), 5000)

  # collabra.126266 -- "bootstrapped" needs \\w* on the qualifier.
  expect_equal(effectcheck:::.resample_count_in(
    "was computed for each of 10000 bootstrapped samples, and the 95% CI"), 10000)
})

test_that("v0.7.5: a decimal can never become a resample count", {
  # THE FIRST DRAFT OF THIS FEATURE SHIPPED THIS DEFECT and the corpus scan
  # caught it: brjpsych_1.txt contains
  #   "(b=0.81, z=2.80, p=0.005, OR=2.25, CI 1.35-3.75)"
  # and the case-insensitive "B = <num>" form matched `b=0.81`, then stripped
  # the "." (correct for a thousands separator, catastrophic for a decimal) to
  # give B = 81. A floor of 1/82 = 0.0122 would have declared every p below
  # .0122 in that paper unattainable -- a false accusation against correctly
  # reported statistics, which is the worst outcome this module can produce.
  expect_true(is.na(effectcheck:::.resample_count_in(
    "a three-way interaction (b=0.81, z=2.80, p=0.005, OR=2.25) with bootstrap resampling")))
  # The integral-shape guard, stated directly: 2 digits after the separator is
  # not a thousands group.
  expect_true(is.na(effectcheck:::.resample_count_in("bootstrap B = 2.25")))
  expect_equal(effectcheck:::.resample_count_in("bootstrap B = 2.250"), 2250)
})

test_that("v0.7.5: a grading scale is not a resample count", {
  # bmcpsych_cbt_burnout_2025.txt: "Grade A+=80% or above, A=70-79%, B=60-69%,
  # C=50-59% and D=40-48% marks)". `B=60` is integral, uppercase, and above the
  # floor of 50 -- every shape guard passes. Only the requirement that the
  # SENTENCE name a resampling procedure refuses it. A grading scale inside a
  # Methods section is entirely ordinary, so the positional scope does not
  # protect against this one.
  expect_true(is.na(effectcheck:::.resample_count_in(
    "Grade A+=80% or above, A=70-79%, B=60-69%, C=50-59% and D=40-48% marks).")))
  # ... and the same string DOES bind once the sentence is about resampling,
  # which is what makes the guard a scope rule rather than a blanket refusal.
  expect_equal(effectcheck:::.resample_count_in(
    "Permutation tests used B=60000 shuffles."), 60000)
})

test_that("v0.7.5: a bare B = n must have nothing else claiming the number", {
  # Cross-model review (Codex/gpt-5.5, 2026-08-09), REPRODUCED against the code
  # as first written:
  #   "Bootstrap analyses were not used; vitamin B = 60 mg was administered."
  # bound B = 60. Two things fail at once -- the sentence-level resampling-word
  # guard is satisfied by a NEGATED mention, and `vitamin B` is not a resample
  # count in any reading. A floor of 1/61 = 0.0164 would then have called every
  # p below .0164 in that paper unattainable.
  #
  # Detecting negation is fragile; requiring the number to STAND ALONE is not.
  # `B = <n>` is the one accepted form with no noun to anchor it, so it now also
  # requires that no word follows the number.
  expect_true(is.na(effectcheck:::.resample_count_in(
    "Bootstrap analyses were not used; vitamin B = 60 mg was administered.")))
  expect_true(is.na(effectcheck:::.resample_count_in(
    "A permutation test was run; hepatitis B = 240 patients were screened.")))
  # Still accepted: the number stands alone, or a resampling noun claims it.
  expect_equal(effectcheck:::.resample_count_in(
    "Permutation tests were run with B = 10000."), 10000)
  expect_equal(effectcheck:::.resample_count_in(
    "Bootstrap resampling used B = 5000, and the CI was percentile-based."), 5000)
  expect_equal(effectcheck:::.resample_count_in(
    "Permutation tests used B = 60000 shuffles."), 60000)
})

test_that("v0.7.5: every count form requires a resampling word in its sentence", {
  # Cross-model review (Claude Sonnet, 2026-08-09), REPRODUCED. `noun_specific`
  # was ungated on the theory that its nouns are unambiguous. `replicates` is
  # not: "Each condition was tested with 60 replicates." is an ordinary wet-lab
  # sentence with no resampling anywhere, and it bound B = 60. Inside a Methods
  # section that becomes the DOCUMENT-level count, so a floor of 1/61 = .0164
  # would then falsely flag any correctly reported permutation p below .0164
  # anywhere else in the same paper.
  expect_true(is.na(effectcheck:::.resample_count_in(
    "Each condition was tested with 60 replicates.")))
  # The gate costs nothing on genuine cases: the unambiguous nouns satisfy it
  # themselves ("permutations", "resamples", "shuffles", "bootstrapped" all
  # match the resampling-word pattern). Only `replicates` did not.
  expect_equal(effectcheck:::.resample_count_in(
    "Bootstrap resampling used 60 replicates."), 60)

  # The loose noun (iterations / replications / simulations) additionally
  # requires the resampling word to come BEFORE the number and near it -- not
  # merely somewhere in the sentence. Same review, also reproduced.
  expect_true(is.na(effectcheck:::.resample_count_in(
    "We enrolled 240 iterations of the survey; a permutation test followed later.")))
  expect_equal(effectcheck:::.resample_count_in(
    "We used cluster-based permutation testing (1000 iterations at a threshold of 0.05)"), 1000)

  # The UNSPACED grading scale. The first fix refused "B = 60 mg" via a
  # space-and-letter lookahead; "B=60-69%" has neither and slipped through,
  # binding B = 60 off a grade band exactly as the spaced form once did.
  expect_true(is.na(effectcheck:::.resample_count_in(
    "Permutation tests were used. Grade A+=80%, A=70-79%, B=60-69%, C=50-59%.")))
})

test_that("v0.7.5: an Appendix closes the Methods region", {
  # Cross-model review (Codex/gpt-5.5, 2026-08-09), REPRODUCED: `Appendix` was
  # missing from `.pat_other_heading`, so a Methods region stayed open through
  # it and a count from the appendix bound as the document-level B. Any heading
  # NOT in the closing list silently WIDENS the scope, which is the failure
  # direction this prescan exists to avoid.
  # Also pinned: a SUBSECTION heading. `normalize_text()` strips section
  # numbers, so "3.1 Sample Characteristics" arrives as "Sample
  # Characteristics" and no numbering-based rule can see it -- the closing rule
  # has to be structural (a short standalone line between blank lines).
  numbered <- paste("Methods", "", "Participants were recruited online.", "",
                    "3.1 Sample Characteristics", "",
                    "We used 1000 permutations for a robustness check.", sep = "
")
  expect_true(is.na(
    effectcheck:::.doc_resampling_b(effectcheck:::normalize_text(numbered))))

  leaky <- paste("Methods", "", "See appendix.", "",
                 "Appendix", "",
                 "We used 1000 permutations for a robustness check.", sep = "\n")
  expect_true(is.na(
    effectcheck:::.doc_resampling_b(effectcheck:::normalize_text(leaky))))
  # The same declaration INSIDE Methods still binds -- the region is closed, not
  # the rule disabled.
  inside <- paste("Methods", "",
                  "We used 1000 permutations for a robustness check.", "",
                  "Appendix", "", "Supplementary tables follow.", sep = "\n")
  expect_equal(
    effectcheck:::.doc_resampling_b(effectcheck:::normalize_text(inside)), 1000)
})

test_that("v0.7.5: the 2026-08-07 wrong-clause refusal still holds", {
  # Regression protection carried forward from v0.6.22: a bare "<n> samples"
  # must never be the count, even inside a resampling sentence.
  # "Across 500 samples, a permutation test with 10,000 permutations ..." once
  # bound B = 500 and then flagged the p as below 1/(B+1) = 0.002.
  expect_equal(effectcheck:::.resample_count_in(
    "Across 500 samples, a permutation test with 10.000 permutations was run"), 10000)
  expect_true(is.na(effectcheck:::.resample_count_in(
    "A total of 5.000 draws were taken from the population register")))
  expect_true(is.na(effectcheck:::.resample_count_in(
    "We recruited ten thousand participants from Prolific")))
})

test_that("v0.7.5: the prescan is scoped to Methods, and refuses without one", {
  methods_doc <- paste(
    "Results", "",
    "The groups differed, t(198) = 3.41, permutation p = 0.02, d = 0.48.", "",
    "Methods", "",
    "For permutation tests, one thousand random shuffles of labels were sampled.",
    sep = "\n")
  expect_equal(
    effectcheck:::.doc_resampling_b(effectcheck:::normalize_text(methods_doc)), 1000)

  # Same declaration, no detectable Methods heading -> no document-level bind.
  # The refusal is deliberate: a silent widening of scope on the documents where
  # scoping is hardest is the wrong failure direction, because a wrong B is a
  # wrong FLOOR and a wrong floor is a false accusation.
  no_heading <- paste(
    "The groups differed, t(198) = 3.41, permutation p = 0.02, d = 0.48.", "",
    "For permutation tests, one thousand random shuffles of labels were sampled.",
    sep = "\n")
  expect_true(is.na(
    effectcheck:::.doc_resampling_b(effectcheck:::normalize_text(no_heading))))
})

test_that("v0.7.5: a Methods B reaches the row, labelled, and fires the floor", {
  # The end-to-end claim. Before v0.7.5 this row had resampling_B = NA and
  # resampling_p_below_floor = FALSE: 1e-04 IS below 1/(1000+1) = 9.99e-04, and
  # the check simply could not see it.
  txt <- paste(
    "Results", "",
    "The groups differed reliably, t(198) = 3.41, permutation p = 0.0001, d = 0.48.", "",
    "Methods", "",
    "For permutation tests, one thousand random shuffles of the condition labels were sampled.",
    sep = "\n")
  r <- check_text(txt)
  expect_equal(nrow(r), 1L)
  expect_equal(r$resampling_B[1], 1000)
  # Provenance is part of the contract: a document-level default must be
  # distinguishable from a count stated beside the statistic.
  expect_equal(r$resampling_B_source[1], "methods_prescan")
  expect_true(isTRUE(r$resampling_p_below_floor[1]))
  # ... and the user-facing message must SAY the B was not stated in this clause.
  expect_match(paste(unlist(r$uncertainty_reasons), collapse = " "),
               "Methods/Analysis section", fixed = TRUE)
})

test_that("v0.7.5: a count in the row's own clause outranks the document default", {
  txt <- paste(
    "Results", "",
    "A permutation test with 200 permutations gave t(198) = 3.41, permutation p = 0.02.", "",
    "Methods", "",
    "For permutation tests, ten thousand random shuffles of labels were sampled.",
    sep = "\n")
  r <- check_text(txt)
  expect_equal(r$resampling_B[1], 200)
  expect_equal(r$resampling_B_source[1], "own_clause")
})

test_that("v0.7.5: a non-resampling row never inherits the document B", {
  # The prescan is a default for RESAMPLING rows only. An ordinary parametric
  # t-test in the same paper must be untouched -- otherwise the document-level
  # default becomes exactly the context-window leak it was designed to avoid
  # (the v0.6.18 Welch N-leak, where a neighbouring "Welch's" moved N 132 -> 403).
  txt <- paste(
    "Results", "",
    "The groups differed, t(198) = 3.41, p = 0.0008, d = 0.48.", "",
    "Methods", "",
    "For permutation tests, one thousand random shuffles of labels were sampled.",
    sep = "\n")
  r <- check_text(txt)
  expect_true(all(is.na(r$resampling_B)))
  expect_true(all(is.na(r$resampling_B_source)))
})
