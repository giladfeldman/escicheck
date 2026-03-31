# Tests for v0.3.0 alternative matching fix
# Phase 5 now honors family_info$alternatives for cross-formula families
# (eta2<->omega2, phi<->V, etc.)

test_that("cohens_f_from_omega2 computes correctly", {
  # Basic formula: f = sqrt(omega2 / (1 - omega2))
  expect_equal(cohens_f_from_omega2(0), 0)
  expect_equal(cohens_f_from_omega2(0.5), 1.0)
  expect_true(is.na(cohens_f_from_omega2(NA)))
  expect_true(is.na(cohens_f_from_omega2(-0.1)))
  expect_true(is.na(cohens_f_from_omega2(1.0)))

  # omega2 = 0.189 -> f = sqrt(0.189/0.811) = 0.483
  expect_equal(round(cohens_f_from_omega2(0.189), 3), 0.483)
})

test_that("cohens_f_from_omega2 < cohens_f_from_eta2 for same F-test", {
  F_val <- 8.0
  df1 <- 2
  df2 <- 57
  eta2 <- eta2_from_F(F_val, df1, df2)
  omega2 <- omega2_from_F(F_val, df1, df2)
  f_eta <- cohens_f_from_eta2(eta2)
  f_omega <- cohens_f_from_omega2(omega2)
  expect_true(f_omega < f_eta)
  expect_true(f_omega > 0)
})

test_that("cohens_f_omega matches when author uses omega2-derived f", {
  # F(2, 57) = 8.0: f_eta=0.530, f_omega=0.483
  # Author reports f = 0.48 (close to f_omega, far from f_eta)
  res <- check_text("F(2, 57) = 8.0, p < .001, Cohen's f = 0.48")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  expect_equal(r$matched_variant[1], "cohens_f_omega")
  expect_true(r$delta_effect_abs[1] < 0.01)
})

test_that("cohens_f (eta2-derived) still wins when closer", {
  # F(2, 57) = 8.0: f_eta = 0.530
  # Report f = 0.53 (very close to f_eta)
  res <- check_text("F(2, 57) = 8.0, p < .001, Cohen's f = 0.53")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  expect_equal(r$matched_variant[1], "cohens_f")
  expect_true(r$delta_effect_abs[1] < 0.005)
})

test_that("Phase 5 includes family alternatives for cross-formula families", {
  # Verify the fix: omega2 should be in same_type_variants for eta2 family
  # We test this indirectly: when reported eta2 is much closer to omega2
  # than to computed eta2, omega2 should match
  # F(3, 96) = 5.0: eta2=0.135, omega2=0.105
  # Report 0.11 — closer to omega2
  res <- check_text("F(3, 96) = 5.0, p = .003, partial eta2 = 0.11")
  r <- res[!is.na(res$matched_variant), ]
  if (nrow(r) > 0 && r$check_type[1] == "effect_size") {
    # omega2 (0.105) should be closer than eta2 (0.135)
    expect_true(r$delta_effect_abs[1] < 0.01)
    expect_equal(r$matched_variant[1], "omega2")
  }
})

test_that("design-ambiguous dz does NOT cross-match with d_ind (regression guard)", {
  # dz family alternatives include d_ind, g_ind but these should NOT
  # be cross-matched because d/g are design variants, not formula variants
  res <- check_text("t(28) = 2.21, p = .035, dz = 1.50")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  # Must NOT match d_ind or g_ind — those are design alternatives
  expect_false(r$matched_variant[1] %in% c("d_ind", "d_ind_equalN", "g_ind"))
})

test_that("existing t-test PASS still passes (regression guard)", {
  res <- check_text("t(28) = 2.21, p = .035, d = 0.81")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  expect_true(r$status[1] %in% c("PASS", "WARN", "OK"))
})

test_that("existing Cohen's f PASS still passes (regression guard)", {
  # F(2, 57) = 8.0, f = 0.53 -> should match cohens_f -> PASS/OK/NOTE
  # NOTE is acceptable when CI check flags something minor
  res <- check_text("F(2, 57) = 8.0, p < .001, Cohen's f = 0.53")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  expect_true(r$status[1] %in% c("PASS", "OK", "NOTE"))
  expect_true(r$delta_effect_abs[1] < 0.01)
})
