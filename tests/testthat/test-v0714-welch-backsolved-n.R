# v0.7.14 (d-48266c): a Welch N back-solved FROM the reported d cannot verify d.
#
# collabra.77859 prints "Welch's t(223) = 8.11, p < .001, (d = 0.99, 95% CI:
# [0.72, 1.25])". v0.6.18 fixed the filed symptom (the rounded Welch df bound
# N = df + 2 = 225 and raised a false WARN). Re-tested at 0.7.13 the row was
# PASS -- but for a circular reason. With group sizes unstated, a Welch df only
# bounds N from below, so the Welch branch back-solves N = 4 t^2 / d^2 from the
# REPORTED d and then recomputes d from that N, which reproduces the reported d
# by construction. Measured at 0.7.13: a deliberately wrong d = 0.70 on the same
# t(223) = 8.11 (true d ~ 0.99) also returned PASS, at N = 537. A check that
# cannot fail is not a check. It also labelled that N `global_text` /
# `not_found`, naming a provenance it did not have, and said "Non-integer df
# (223.00)" of an integer df.
#
# Now: the effect size is reported as not verifiable (the p-value still is),
# and N_source says what the N is: "effect_backsolved".

welch_row <- function(txt) {
  r <- as.data.frame(check_text(txt))
  r[!is.na(r$test_type) & r$test_type == "t", , drop = FALSE]
}

test_that("a WRONG d on a Welch test with no group sizes is not PASS", {
  r <- welch_row("The scarf was rated higher than the coat, Welch's t(223) = 8.11, p < .001, d = 0.70.")
  expect_equal(nrow(r), 1L)
  expect_false(r$status == "PASS")
  expect_equal(r$N_source, "effect_backsolved")
  expect_true(is.na(r$matched_value))
})

test_that("the paper's own (correct) row is honest too: no circular PASS, p still checked", {
  txt <- paste("We recruited N = 403 participants. Many analyses followed.",
               "The scarf was rated higher than the coat, Welch's t(223) = 8.11,",
               "p < .001, d = 0.99, 95% CI [0.72, 1.25].")
  r <- welch_row(txt)
  expect_equal(nrow(r), 1L)
  expect_equal(r$check_type, "p_value")
  expect_equal(r$N_source, "effect_backsolved")
  u <- paste(unlist(r$uncertainty_reasons), collapse = " | ")
  expect_true(grepl("back-solved from the reported effect size", u, fixed = TRUE))
  expect_false(grepl("Non-integer df (223.00)", u, fixed = TRUE))
  expect_true(grepl("Welch", u, fixed = TRUE))
})

test_that("stated group sizes still verify a Welch d (control)", {
  r <- welch_row("With n1 = 131 and n2 = 135, Welch's t(222.87) = 8.11, p < .001, d = 0.99.")
  expect_equal(nrow(r), 1L)
  expect_false(identical(r$N_source, "effect_backsolved"))
  expect_equal(r$check_type, "effect_size")
})

test_that("a non-Welch t with an equal-n df is unaffected (control)", {
  r <- welch_row("The groups differed, t(98) = 2.50, p = .014, d = 0.50.")
  expect_equal(r$check_type, "effect_size")
  expect_equal(r$status, "PASS")
})

test_that("a Welch row never matches a paired-design variant (review finding 2026-09-30)", {
  # A Welch test compares two independent groups by definition, so dz / dav /
  # drm cannot describe it. At 0.7.13 and in the 0.7.14 candidate a WRONG
  # d = 0.70 on Welch t(223) = 8.11 with n1 = 131, n2 = 135 (true d ~ 0.99)
  # published PASS by matching drm = 0.685.
  r <- welch_row("Welch t(223.00) = 8.11, p < .001, d = 0.70, n1 = 131, n2 = 135.")
  expect_false(r$status == "PASS")
  expect_false(isTRUE(r$matched_variant %in% c("dz", "dav", "drm", "gz", "gav", "grm")))
  # control: the correct d still passes against the independent-groups value
  ok <- welch_row("Welch t(223.00) = 8.11, p < .001, d = 0.99, n1 = 131, n2 = 135.")
  expect_equal(ok$status, "PASS")
  # control: a paired t-test still matches dz
  p <- welch_row("A paired t-test showed t(49) = 3.00, p = .004, dz = 0.42.")
  expect_equal(p$matched_variant, "dz")
})

test_that("an explicit paired label (dz) keeps its paired variants on a Welch-looking row", {
  # Tier-2 cross-model finding (2026-09-30): the Welch gate keyed on the word
  # "Welch" or a fractional df alone, so a correct dz on a within-subject
  # Satterthwaite contrast, or in a clause that merely mentions Welch, lost dz
  # and fell from PASS to NOTE. The author's own paired label is design evidence.
  sat <- welch_row("In the within-subject contrast, t(45.3) = 3.00, p = .004, dz = 0.44 (Satterthwaite df).")
  expect_equal(sat$matched_variant, "dz")
  expect_equal(sat$status, "PASS")
  neg <- welch_row("A paired t-test (Welch's correction not needed) showed t(49) = 3.00, p = .004, dz = 0.42.")
  expect_equal(neg$matched_variant, "dz")
  expect_equal(neg$status, "PASS")
  # control: a generic d on a Welch row is still never matched to a paired variant
  w <- welch_row("A Welch t-test showed t(223) = 8.11, p < .001, d = 0.70, n1 = 131, n2 = 135.")
  expect_false(isTRUE(w$matched_variant %in% c("dz", "dav", "drm")))
})

test_that("a tiny rounded Welch d is graded at the Welch floor, not a scraped study total", {
  # 10.1016/j.jesp.2020.104052 (0.7.14 + docpluck 2.4.147): Welch t(169.45) =
  # 0.24, d = 0.04 bound the document's global N = 827. The back-solve
  # N = 4t^2/d^2 = 144 fell just outside the 0.85 * floor band because d is
  # rounded to two decimals, so the scraped 827 stood and the recomputed
  # d = 0.0167 raised a WARN. At the floor N = df + 2 = 171, d = 0.0367, which
  # the printed 0.04 reproduces within its rounding -- a real (not fitted) check.
  filler <- paste(rep("This is unrelated filler prose about methodology and procedures.", 12),
                  collapse = " ")
  txt <- paste0("We recruited N = 827 participants. ", filler, " ", filler,
                " The conditions did not differ, Welch t(169.45) = 0.24, p = .81, d = 0.04.")
  r <- welch_row(txt)
  expect_equal(nrow(r), 1L)
  expect_equal(r$N, 171)
  expect_equal(r$N_source, "df_inferred")
  expect_false(r$status %in% c("WARN", "ERROR"))
  # control: a d the floor N cannot reproduce is still flagged (0.20 vs 0.037)
  bad <- welch_row(sub("d = 0.04", "d = 0.20", txt, fixed = TRUE))
  expect_false(bad$status %in% c("PASS"))
})
