# v0.6.18 -- defects found by a cross-model review OF the v0.6.18 diff itself,
# each reproduced locally before being fixed (Codex, 2026-08-05).
#
# The review was run on the finished diff precisely because these changes decide
# the SAMPLE SIZE every effect size is divided by: a wrong N here publishes a
# wrong effect size with no test failing. Six findings; all six reproduced.

test_that("an omnibus-contrast N is refused unless the text says post-hoc", {
  # THE MOST DANGEROUS FINDING. Effect-size fit alone cannot justify adopting
  # the contrast N: an UNEQUAL-GROUPS full-sample t also reports a d larger
  # than the equal-n df+2 estimate, so the candidate can fit it coincidentally.
  #
  # Reproduced numerically before the fix: a document containing F(2, 98)
  # yields candidate N = 67; a genuine full-sample t(98) = 2.00 with unprinted
  # cells n1 = 20, n2 = 80 has true d = 0.500, where df+2 = 100 estimates 0.400
  # (delta 0.100) and the candidate estimates 0.489 (delta 0.011) -- so the old
  # gate ADOPTED N = 67 against a true 100.
  #
  # CI width cannot rescue the gate either (also verified): unequal groups
  # widen the CI in the same direction a smaller N does. And the structural
  # ratio is identical -- contrast_N/(df+2) is 2/k in BOTH cases. The only
  # honest discriminator is the paper SAYING this is a post-hoc contrast.
  txt <- paste0(
    "A total of N = 101 participants were randomized to three conditions. ",
    "The omnibus test was significant, F(2, 98) = 4.10, p = .02. ",
    "In a separate analysis of the full sample, the two groups differed, ",
    "t(98) = 2.00, p = .048, d = 0.50."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  # No post-hoc wording -> the hypothesis must not be adopted.
  expect_false(identical(as.character(rows$N_source[1]), "omnibus_df_contrast"))
})

test_that("the omnibus-contrast N is still adopted when the text DOES say post-hoc", {
  # The real collabra.90203 shape must keep working after the guard tightened.
  txt <- paste0(
    "A total of N = 1001 participants were randomly assigned to one of three ",
    "conditions. We found some support for a main effect of Identifiability, ",
    "F(2, 998) = 3.91, p = .02. Post hoc pairwise comparisons showed that we ",
    "found support for differences between the statistical and the joint ",
    "condition, t(998) = 2.46, p = .041, d = 0.19 [0.04, 0.35]."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_equal(as.character(rows$N_source[1]), "omnibus_df_contrast")
  expect_lt(suppressWarnings(as.numeric(rows$delta_effect[1])), 0.02)
})

test_that("a post-hoc declaration covers the whole RUN of sibling contrasts", {
  # A paper announces "post-hoc comparisons ... with Bonferroni correction"
  # ONCE and then reports several contrasts. With a per-row text test, only the
  # first row's context window still held the announcement, so on the real
  # collabra.90203 render the FIRST t(998) was fixed and the SECOND (the
  # primary target, WARN + INCONSISTENT CI) was left wrong. Caught by
  # re-rendering the paper right after the post-hoc guard was added. The
  # declaration now propagates across rows sharing the same omnibus candidate.
  txt <- paste0(
    "A total of N = 1001 participants were randomly assigned to one of three ",
    "conditions. We found a main effect of Identifiability, F(2, 998) = 3.91, ",
    "p = .02. To better understand the Identifiability main effect, we also ",
    "examined the post-hoc comparisons with Bonferroni correction. We found no ",
    "support for differences between statistical and identifiable victim ",
    "conditions, t(998) = 0.097, p = 1.00, d = 0.01, 95% CI [-0.16, 0.14], and ",
    "near threshold for the comparison between identifiable and joint, ",
    "t(998) = 2.37, p = .053, d = 0.18 [0.03, 0.34] with donations slightly ",
    "lower in the joint condition. We found support for differences between ",
    "the statistical and the joint condition t(998) = 2.46, p = .041, ",
    "d = 0.19 [0.04, 0.34]."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t" &
                !is.na(res$stat_value) & res$stat_value == 2.46, ]
  expect_gt(nrow(rows), 0)
  # The LAST contrast in the run -- furthest from the announcement -- must be
  # adopted just like the first.
  expect_equal(as.character(rows$N_source[1]), "omnibus_df_contrast")
  expect_lt(suppressWarnings(as.numeric(rows$delta_effect[1])), 0.02)
})

test_that("a p reported at its printed precision is not flagged as a discrepancy", {
  # A bare ratio test falsely flags legitimate ROUNDING: `p = .01` printed to
  # two decimals honestly represents a computed .0051 (ratio 1.96). The ratio
  # must apply only OUTSIDE the rounding band the printed precision implies.
  txt <- "The comparison was significant, t(500) = 2.81, p = .01."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  u <- as.character(rows$uncertainty_reasons[1])
  expect_false(grepl("differ by a factor of", u))
})

test_that("a genuine small-p discrepancy outside the rounding band still flags", {
  # The pci.rr.100726 case: p = .006 (3 decimals -> +/- .0005) vs a computed
  # .00269 is 0.0033 away -- far outside the rounding band, and 2.23x.
  txt <- "The effect was significant, t(868) = -3.01, p = .006."
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_match(as.character(rows$uncertainty_reasons[1]), "differ by a factor of")
})

test_that("a df-replaced N no longer advertises its discarded scraped source", {
  # The relabel originally fired only on NA / "not_found", so a row whose
  # scraped N had been DISCARDED and replaced by the df-derived value kept
  # advertising `global_text` -- naming a provenance the number no longer has.
  filler <- paste(rep("Filler describing the procedure at length. ", 40),
                  collapse = "")
  txt <- paste0(
    "The final sample was N = 500. ", filler,
    "Groups differed, t(48) = 2.31, p = .025, d = 0.66."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  expect_equal(suppressWarnings(as.numeric(rows$N[1])), 50)  # df + 2
  expect_equal(as.character(rows$N_source[1]), "df_inferred")
})

test_that("subgroup_sum is treated as a scraped source, not a clause-stated one", {
  # `subgroup_sum` reads the wider `context`, not the statistic's own clause
  # (verified in parse.R), so it is adjoining evidence like `local_context`.
  # Excluding it let a subgroup pair in [df+3, df+12] bypass df authority.
  expect_true("subgroup_sum" %in% effectcheck:::.SCRAPED_N_SOURCES)
  # The clause-stated sources stay exempt -- an explicitly reported N that
  # disagrees with df is a finding to surface, not a value to overwrite.
  for (src in c("own_clause", "own_clause_arms", "own_clause_denominator",
                "arm_totals_sum", "chi_inline", "chi_bare_n")) {
    expect_false(src %in% effectcheck:::.SCRAPED_N_SOURCES)
  }
})

test_that("a reported Hedges g is not back-computed as if it were Cohen d", {
  # The Welch back-computation N = 4t^2/d^2 assumes a Cohen's d. Hedges'
  # g = J(df) * d with J = 1 - 3/(4*df - 1) < 1, so feeding g in unchanged
  # inflates N. It is converted back to the d scale first.
  #
  # Note the FIRST attempt at this fix simply excluded `g` from the branch --
  # which was WORSE: refusing left the implausible scraped N = 5000 in place on
  # a row this branch exists to correct. Verified by probe before re-fixing.
  # At df = 30.5, J = 0.9752, so g = 0.70 -> d = 0.7178 -> N = 34 (an
  # uncorrected g would have given 36).
  filler <- paste(rep("Filler describing the procedure at length. ", 40),
                  collapse = "")
  txt <- paste0(
    "The final sample was N = 5000. ", filler,
    "Welch's t(30.5) = 2.10, p = .044, g = 0.70."
  )
  res <- effectcheck::check_text(txt)
  rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(rows), 0)
  n_bound <- suppressWarnings(as.numeric(rows$N[1]))
  # The implausible scraped N must be rejected...
  expect_lt(n_bound, 5000)
  # ...and the replacement must be the g-CORRECTED value (34), not the
  # g-as-d value (36).
  expect_equal(n_bound, 34)
})
