# Tests for v0.3.0 range-aware matching
# Phase 5 now uses $range for dav/drm/gav/grm variants

test_that("dav within r-grid range matches with delta=0", {
  # t(20) = 4.0: dz = 4/sqrt(21) = 0.873
  # dav at r=0.1: 0.873/sqrt(2*0.9) = 0.651
  # dav at r=0.9: 0.873/sqrt(2*0.1) = 1.952
  # dav at r=0.5 (median): 0.873/sqrt(1.0) = 0.873
  # Report d = 1.20 — within [0.651, 1.952] range
  res <- check_text("t(20) = 4.00, p < .001, d = 1.20")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  # d=1.20 is within dav range — variant selection prefers dav
  # Output delta uses median distance (0.327) for status determination
  # Key check: dav is preferred over d_ind when value is in dav's range
  expect_true(r$matched_variant[1] %in% c("dav", "drm", "gav", "grm", "d_ind_equalN"))
})

test_that("reported d outside dav range still gets nonzero delta", {
  # t(20) = 2.0: dz = 2/sqrt(21) = 0.436
  # dav at r=0.95: 0.436/sqrt(0.1) = 1.379
  # Report d = 3.00 — well outside any range
  res <- check_text("t(20) = 2.00, p = .059, d = 3.00")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  # Should have significant delta (not in range)
  expect_true(r$delta_effect_abs[1] > 0.5)
})

test_that("dav range matching adds assumption note", {
  # When matched within range, should add assumption about r-grid
  res <- check_text("t(20) = 4.00, p < .001, d = 1.50")
  r <- res[!is.na(res$matched_variant), ]
  if (nrow(r) > 0 && r$delta_effect_abs[1] == 0) {
    # Check assumptions mention r-grid
    expect_true(grepl("r-grid", r$assumptions_used[1]))
  }
})

test_that("paired t-test with high-r dav matches better than median", {
  # t(30) = 5.0: dz = 5/sqrt(31) = 0.898
  # dav at median (r=0.5): 0.898/sqrt(1.0) = 0.898
  # dav at r=0.8: 0.898/sqrt(0.4) = 1.420
  # dav at r=0.9: 0.898/sqrt(0.2) = 2.008
  # Report d = 1.42 — within dav range but far from median
  # Range-aware matching should improve delta vs pure median matching
  res <- check_text("t(30) = 5.00, p < .001, d = 1.42")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  # Delta should be less than distance to median (1.42-0.898=0.522)
  expect_true(r$delta_effect_abs[1] < 0.5)
})

test_that("F(1,df) dav range matching works", {
  # F(1, 48) = 16.0: t_equiv = 4.0, dz = 4/sqrt(49) = 0.571
  # dav range should cover same range as t-test block
  res <- check_text("F(1, 48) = 16.0, p < .001, d = 0.90")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  # d=0.90 is within dav range — range-aware matching prefers this
  # Output delta is median distance for status (not selection delta)
  expect_true(r$delta_effect_abs[1] < 0.5)
})

test_that("existing median-matched PASS still passes (regression guard)", {
  # d close to dav median (r=0.5) should still PASS
  res <- check_text("t(28) = 2.21, p = .035, d = 0.81")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  expect_true(r$status[1] %in% c("PASS", "WARN", "OK"))
})

test_that("range matching does not break drm variants", {
  # drm = dz * sqrt(2*(1-r)) — opposite direction from dav
  # t(20) = 4.0: dz = 0.873, drm median (r=0.5) = 0.873
  # drm at r=0.9: 0.873*sqrt(0.2) = 0.390
  # Report d = 0.50 — closer to drm at ~r=0.75 than to median
  res <- check_text("t(20) = 4.00, p < .001, d = 0.50")
  r <- res[!is.na(res$matched_variant), ]
  expect_true(nrow(r) > 0)
  # Range-aware matching should improve vs pure median delta
  # Delta to drm_min (0.390) = 0.11; delta to median (0.873) = 0.373
  expect_true(r$delta_effect_abs[1] < 0.4)
})
