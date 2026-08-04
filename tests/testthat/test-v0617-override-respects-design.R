# v0.6.17 -- when the df-authority override replaces an incompatible
# `global_text` N, it must not silently commit to the INDEPENDENT reading
# (df + 2) on a row whose design is paired or undetermined.
#
# Source: 2026-08-04 escicheck-iterate cycle 1, /escicheck-review of the
# v0.6.17 diff. Reproduced before the fix.
#
# WHAT WENT WRONG
#
# The override (check.R ~L1209) picks the replacement N with
#
#     N <- if (canonical_type %in% c("dz","dav","drm")) df1 + 1 else df1 + 2
#
# `canonical_type` is the reported EFFECT-SIZE family, not the design. A paired
# t-test that reports no effect size at all has `canonical_type = NA`, so it
# falls to the `else` and is assigned df + 2 -- the independent-samples N --
# even when the row is otherwise correctly detected as `design_inferred =
# "paired"`. On a paired t(49) with a global-text N of 500, the override
# published N = 51 where the true paired N is 50.
#
# This mattered more after v0.6.17 widened the override: it previously fired
# only when the scraped N exceeded df + 12, and now fires for ANY `global_text`
# N above df + 2, so the wrong-branch assignment reaches many more rows.
#
# THE RULE: the df tells us N is either df+1 (paired) or df+2 (independent) --
# it does not tell us WHICH. When the design is not determinable, the row must
# go down the existing ambiguous-design path (which computes both variants and
# labels `design_inferred = "ambiguous"`) rather than silently picking one.

test_that("the override does not assign an independent N to a paired row", {
  filler <- paste(rep("Filler describing the procedure at length. ", 40),
                  collapse = "")
  txt <- paste0(
    "Participants completed both conditions. The final sample was N = 500. ",
    filler,
    "A paired-samples t-test showed a difference, t(49) = 3.10, p = .003."
  )

  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)

  row <- t_rows[1, ]
  n_bound <- suppressWarnings(as.numeric(row$N[1]))
  design <- as.character(row$design_inferred[1])

  # df = 49 -> paired N = 50, independent N = 51.
  # A row reported as paired must not carry the independent N.
  if (!is.na(design) && design == "paired") {
    expect_equal(
      n_bound, 50,
      info = paste0(
        "row is design_inferred='paired' (df=49, so paired N=50) but the ",
        "df-authority override assigned N=", n_bound,
        " -- it branches on canonical_type (the effect-size family), which is ",
        "NA here because no effect size is reported, so it fell through to the ",
        "independent df+2 formula."
      )
    )
  }
})

test_that("the override still corrects an independent row", {
  # Reported Cohen's d makes canonical_type = "d" -> independent, N = df + 2.
  filler <- paste(rep("Filler describing the procedure at length. ", 40),
                  collapse = "")
  txt <- paste0(
    "The final sample was N = 500. ", filler,
    "Groups differed, t(48) = 2.31, p = .025, d = 0.66."
  )
  res <- effectcheck::check_text(txt)
  t_rows <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_gt(nrow(t_rows), 0)
  n_bound <- suppressWarnings(as.numeric(t_rows$N[1]))
  expect_equal(n_bound, 50)  # df + 2
})
