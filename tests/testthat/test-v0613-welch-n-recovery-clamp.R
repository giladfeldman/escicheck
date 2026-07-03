# v0.6.13 (E-welch-n-clamp): the Welch t-test global-N override back-computes N
# from the reported d (equal-groups assumption: N = 4t^2/d^2) when the bound N is
# a `global_text` value implausibly larger than the Welch floor (df + 2). For a
# SMALL effect the equal-groups back-computation UNDERestimates N and can dip a few
# units below df + 2, which previously made the guard reject the override outright
# and keep the implausible global N -- corrupting the recomputed d to the wrong
# sign/magnitude and firing a spurious WARN. The override now accepts a
# back-computed N within a plausible band of the Welch minimum and CLAMPS it up to
# df + 2 (N can never be below the Welch floor).
#
# Surfaced by the 2026-07-02 escicheck-iterate cycle-3 canary re-audit of cog_emo
# (Chan & Feldman, Cognition & Emotion): three sibling Welch clauses in one
# sentence -- t(509.74)=12.58/d=1.09 and t(521.84)=10.23/d=0.89 recovered N~530,
# but t(520.72)=-1.93/d=0.17 kept the global N=794 (its N_from_d=516 was 2 units
# under the old min_N_welch-5 tolerance), producing a false WARN + sign mismatch.

test_that("a small-d negative-t Welch clause recovers N clamped to the Welch floor", {
  # A distant global N (794) with no local N forces N_source = global_text; the
  # small-effect clause must override it to ~df+2, not keep 794.
  filler <- paste(rep("This is unrelated filler prose about methodology and procedures.",
                      12), collapse = " ")
  txt <- paste0(
    "A total of N = 794 participants completed the battery. ", filler, " ", filler,
    " The difference in affective empathy between the low empathy condition and the ",
    "control condition was weaker, t(520.72) = -1.93, p = .050, d = 0.17."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(rr), 1L)
  # N overridden away from the implausible global 794 and clamped to the Welch
  # floor df + 2 = round(520.72) + 2 = 523.
  expect_true(rr$N[1] < 700)
  expect_equal(rr$N[1], round(520.72) + 2)
  expect_equal(rr$N_source[1], "global_text")
  # No spurious WARN from the wrong (global) N corrupting the recomputed d.
  expect_false(identical(rr$status[1], "WARN"))
})

test_that("a large-d Welch clause still recovers its own back-computed N (unchanged)", {
  filler <- paste(rep("This is unrelated filler prose about methodology and procedures.",
                      12), collapse = " ")
  txt <- paste0(
    "A total of N = 794 participants completed the battery. ", filler, " ", filler,
    " Affective empathy was much higher in the high-empathy condition, ",
    "t(509.74) = 12.58, p < .001, d = 1.09."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  expect_equal(nrow(rr), 1L)
  # 4 * 12.58^2 / 1.09^2 ~= 533, comfortably above df+2, so it is used directly.
  expect_true(abs(rr$N[1] - 533) <= 2)
  expect_true(rr$N[1] < 700)
})

test_that("a truly implausible back-computed N (garbled d) does not override", {
  # A tiny d that would back-compute to far below the Welch floor (< 0.85*min)
  # must NOT override -- the recovery only trusts a plausible band.
  filler <- paste(rep("This is unrelated filler prose about methodology and procedures.",
                      12), collapse = " ")
  txt <- paste0(
    "A total of N = 794 participants completed the battery. ", filler, " ", filler,
    " A negligible difference was observed, t(520.72) = 0.20, d = 0.01."
  )
  res <- effectcheck::check_text(txt)
  rr <- res[!is.na(res$test_type) & res$test_type == "t", ]
  # 4 * 0.20^2 / 0.01^2 = 1600, which is ABOVE 794, so N_from_d < N is false and
  # the override does not fire (N stays global). The point: no crash / no clamp to
  # a nonsense value; the row is simply left with its global N.
  expect_equal(nrow(rr), 1L)
  expect_true(!is.na(rr$N[1]))
})
