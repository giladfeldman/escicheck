# v0.6.8 (E-A1 continuation): a t-test reported as a bare CONTINUATION of the
# previous test's sentence -- "...t(798) = 23.7 ... for the Prolific sample and
# t(798) = 24.3 ... for the MTurk sample" -- is split into its own sub-chunk, so
# its clause carries no design signal and it defaults to "independent" via the
# effect-type fallback, even though it is the SAME comparison (same design) as the
# preceding sibling. A determined paired / within / one-sample design is propagated
# from the immediately-preceding prose t-row when they share df1 and the reported
# effect-size name and the current row carries no design keyword of its own.
#
# Surfaced by the 2026-06-29 escicheck-iterate run against collabra.23443:
# t(798) = 24.3 (the MTurk continuation of the paired t(798) = 23.7) defaulted to
# "independent" though the gold marks it within-subjects paired. (Also recovered
# t(1599)=12.49 / t(1599)=33.89 continuations of one-sample tests.)

test_that("a continuation t-test inherits the preceding sibling's paired design", {
  txt <- paste0(
    "Being paid increased willingness based on paired-sample t-tests. ",
    "As expected, participants estimated others would be more willing when paid, ",
    "t(798) = 23.7, p < .001, d = 0.64 for the Prolific sample and ",
    "t(798) = 24.3, p < .001, d = 0.67 for the MTurk sample."
  )
  res <- effectcheck::check_text(txt)
  r237 <- res[!is.na(res$test_type) & res$test_type == "t" &
                !is.na(res$stat_value) & abs(res$stat_value - 23.7) < 1e-9, ]
  r243 <- res[!is.na(res$test_type) & res$test_type == "t" &
                !is.na(res$stat_value) & abs(res$stat_value - 24.3) < 1e-9, ]
  expect_true(nrow(r237) >= 1 && nrow(r243) >= 1)
  expect_true(any(r237$design_inferred == "paired", na.rm = TRUE))
  # The continuation row inherits paired (it would otherwise default to independent).
  expect_true(any(r243$design_inferred == "paired", na.rm = TRUE))
  expect_false(any(r243$design_inferred == "independent", na.rm = TRUE))
})

test_that("a continuation row does NOT inherit when it states its own design", {
  # If the second clause names its own (different) design, the inheritance must not
  # fire -- the explicit signal wins.
  txt <- paste0(
    "Participants differed on the paired measure, t(100) = 3.0, p = .003, d = 0.30, ",
    "and in an independent-samples t-test t(100) = 4.0, p < .001, d = 0.40."
  )
  res <- effectcheck::check_text(txt)
  r40 <- res[!is.na(res$test_type) & res$test_type == "t" &
               !is.na(res$stat_value) & abs(res$stat_value - 4.0) < 1e-9, ]
  expect_true(nrow(r40) >= 1)
  expect_true(any(r40$design_inferred == "independent", na.rm = TRUE))
  expect_false(any(r40$design_inferred == "paired", na.rm = TRUE))
})

test_that("the inheritance pass does NOT fire when the following test has a different df", {
  # The propagation must only fire on a genuine continuation (same df + same effect
  # family). A following t on a DIFFERENT df is a different comparison: the
  # inheritance note must be ABSENT (its final design label may still be set by the
  # ordinary context detector, which is a separate mechanism — here we assert that
  # OUR propagation specifically did not fire, via its uncertainty note).
  txt <- paste0(
    "On the within-subjects measure, t(50) = 2.5, p = .016, dz = 0.35. ",
    "A later comparison gave t(200) = 5.0, p < .001, d = 0.36."
  )
  res <- effectcheck::check_text(txt)
  r200 <- res[!is.na(res$test_type) & res$test_type == "t" &
                !is.na(res$stat_value) & abs(res$stat_value - 5.0) < 1e-9, ]
  expect_true(nrow(r200) >= 1)
  ur <- as.character(r200$uncertainty_reasons[1])
  expect_false(grepl("Design inherited", ur, fixed = TRUE))
})
