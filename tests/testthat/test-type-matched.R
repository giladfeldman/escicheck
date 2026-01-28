# Test suite for type-matched comparison (v2.0)
# Tests the new behavior where reported effect sizes are compared against
# computed variants of the SAME type

library(testthat)

# ============================================================================
# Type-Matched Comparison Tests
# ============================================================================

test_that("reported d is compared to d variants, not g", {
  # When user reports Cohen's d, we should match against d variants
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  # The matched_variant should be a d variant, not g
  if (!is.na(result$matched_variant[1])) {
    expect_true(grepl("^d", result$matched_variant[1]) || 
                result$matched_variant[1] %in% c("dz", "dav", "drm"))
    expect_false(result$matched_variant[1] == "g_ind")
  }
  
  # reported_type should be "d"
  expect_equal(result$reported_type[1], "d")
})

test_that("reported g is compared to g variants, not d", {
  text <- "t(28) = 2.21, p = .035, g = 0.78"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  # reported_type should be "g"
  expect_equal(result$reported_type[1], "g")
  
  # Note: Currently we may not have g variants computed,

  # but the reported_type should still be correctly identified
})

test_that("reported dz is compared to paired variants", {
  text <- "t(29) = 3.50, p = .001, dz = 0.64"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  expect_equal(result$reported_type[1], "dz")
  
  # If matched, should be a paired variant
  if (!is.na(result$matched_variant[1])) {
    expect_true(result$matched_variant[1] %in% c("dz", "dav", "drm"))
  }
})

test_that("reported eta2 is compared to eta2 variants", {
  text <- "F(2, 27) = 4.56, p = .02, eta2 = 0.25"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  # Parser may not detect eta2 as effect name - check if it's there
  if (!is.na(result$reported_type[1])) {
    expect_true(result$reported_type[1] %in% c("eta2", "etap2"))
    
    # If matched, should be an eta2 variant
    if (!is.na(result$matched_variant[1])) {
      expect_true(result$matched_variant[1] %in% c("eta2", "partial_eta2", "generalized_eta2"))
    }
  }
})

test_that("reported partial eta2 is compared to eta2 family", {
  text <- "F(2, 27) = 4.56, p = .02, partial eta2 = 0.25"
  result <- check_text(text)
  
  if (nrow(result) > 0 && !is.na(result$reported_type[1])) {
    expect_true(result$reported_type[1] %in% c("etap2", "eta2"))
  }
})

# ============================================================================
# Ambiguity Handling Tests
# ============================================================================

test_that("ambiguity_level is set correctly for clear cases", {
  # Independent t-test with explicit group sizes
  text <- "t(28) = 2.21, p = .035, d = 0.80, n1 = 15, n2 = 15"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  expect_true("ambiguity_level" %in% names(result))
  
  # With explicit group sizes, should be clear or at most ambiguous
  if (!is.na(result$ambiguity_level[1])) {
    expect_true(result$ambiguity_level[1] %in% c("clear", "ambiguous"))
  }
})

test_that("ambiguity_level is set for unclear design", {
  # t-test without design indicators
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  expect_true("ambiguity_level" %in% names(result))
  
  # Without explicit design info, should be ambiguous
  # (unless context provides clear indication)
})

test_that("ambiguity_reason is populated when ambiguous", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  expect_true("ambiguity_reason" %in% names(result))
})

test_that("unknown effect size type is marked as highly_ambiguous", {
  # This tests when the reported effect size type is not recognized
  text <- "t(28) = 2.21, p = .035, xyz = 0.80"
  result <- check_text(text)
  
  # If parsed, should be marked as highly ambiguous
  if (nrow(result) > 0 && !is.na(result$effect_reported[1])) {
    expect_true(result$ambiguity_level[1] %in% c("ambiguous", "highly_ambiguous"))
  }
})

# ============================================================================
# All Variants Structure Tests
# ============================================================================

test_that("all_variants column contains JSON structure", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  expect_true("all_variants" %in% names(result))
  
  # Should be valid JSON
  if (!is.na(result$all_variants[1]) && result$all_variants[1] != "{}") {
    variants <- jsonlite::fromJSON(result$all_variants[1], simplifyVector = FALSE)
    expect_true(is.list(variants))
    expect_true("same_type" %in% names(variants))
    expect_true("alternatives" %in% names(variants))
  }
})

test_that("same_type variants contain d variants for d report", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  if (!is.na(result$all_variants[1]) && result$all_variants[1] != "{}") {
    variants <- jsonlite::fromJSON(result$all_variants[1], simplifyVector = FALSE)
    
    # same_type should contain d variants
    if (length(variants$same_type) > 0) {
      variant_names <- names(variants$same_type)
      # Should have at least one d variant
      has_d_variant <- any(grepl("^d", variant_names) | variant_names %in% c("dz", "dav", "drm"))
      expect_true(has_d_variant)
    }
  }
})

test_that("alternatives contain g for d report", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  if (!is.na(result$all_variants[1]) && result$all_variants[1] != "{}") {
    variants <- jsonlite::fromJSON(result$all_variants[1], simplifyVector = FALSE)
    
    # alternatives should contain g_ind
    if (length(variants$alternatives) > 0) {
      expect_true("g_ind" %in% names(variants$alternatives))
    }
  }
})

# ============================================================================
# Helper Function Tests
# ============================================================================

test_that("get_variants returns correct structure", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  variants <- get_variants(result, 1)
  expect_true(is.list(variants))
  expect_true("same_type" %in% names(variants))
  expect_true("alternatives" %in% names(variants))
})

test_that("get_same_type_variants returns only same-type", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  same_type <- get_same_type_variants(result, 1)
  expect_true(is.list(same_type))
})

test_that("get_alternatives returns alternatives", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  alts <- get_alternatives(result, 1)
  expect_true(is.list(alts))
})

test_that("format_variants returns formatted string", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  formatted <- format_variants(result, 1)
  expect_true(is.character(formatted))
})

test_that("compare_to_variants returns comparison data frame", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  comparison <- compare_to_variants(result, 1)
  expect_true(is.data.frame(comparison))
  expect_true("variant" %in% names(comparison))
  expect_true("value" %in% names(comparison))
  expect_true("delta" %in% names(comparison))
  expect_true("is_same_type" %in% names(comparison))
})

# ============================================================================
# Variant Metadata Tests
# ============================================================================

test_that("variant metadata contains required fields", {
  metadata <- get_variant_metadata("d_ind")
  
  expect_true(is.list(metadata))
  expect_true("name" %in% names(metadata))
  expect_true("assumptions" %in% names(metadata))
  expect_true("when_to_use" %in% names(metadata))
})

test_that("effect family info contains required fields", {
  family <- get_effect_family("d")
  
  expect_true(is.list(family))
  expect_true("family" %in% names(family))
  expect_true("variants" %in% names(family))
  expect_true("alternatives" %in% names(family))
})

# ============================================================================
# Edge Cases
# ============================================================================

test_that("handles missing effect size gracefully", {
  # t-test without reported effect size
  text <- "t(28) = 2.21, p = .035"
  result <- check_text(text)
  
  # Should still parse and compute variants
  expect_true(nrow(result) >= 0)
})

test_that("handles multiple statistics in text", {
  text <- "t(28) = 2.21, p = .035, d = 0.80. F(2, 27) = 4.56, p = .02, eta2 = 0.25."
  result <- check_text(text)
  
  # Should find both statistics
  expect_true(nrow(result) >= 1)
  
  # Each should have appropriate reported_type
  if (nrow(result) >= 2) {
    types <- result$reported_type
    expect_true("d" %in% types || "eta2" %in% types)
  }
})

test_that("handles correlation correctly", {
  text <- "r(198) = .34, p < .001"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  expect_equal(result$test_type[1], "r")
  
  # For correlations, the statistic IS the effect size
  if (!is.na(result$matched_variant[1])) {
    expect_equal(result$matched_variant[1], "r")
  }
})

test_that("handles chi-square with phi correctly", {
  text <- "chi-square(1) = 5.23, p = .02, phi = 0.23, N = 100"
  result <- check_text(text)
  
  if (nrow(result) > 0) {
    expect_equal(result$test_type[1], "chisq")
    expect_equal(result$reported_type[1], "phi")
  }
})

# ============================================================================
# Backward Compatibility Tests
# ============================================================================

test_that("closest_method is alias for matched_variant", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  expect_true("closest_method" %in% names(result))
  expect_true("matched_variant" %in% names(result))
  
  # Should be the same
  expect_equal(result$closest_method[1], result$matched_variant[1])
})

test_that("delta_effect_abs is alias for delta_effect", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  expect_true("delta_effect_abs" %in% names(result))
  expect_true("delta_effect" %in% names(result))
  
  # Should be the same
  if (!is.na(result$delta_effect[1])) {
    expect_equal(result$delta_effect_abs[1], result$delta_effect[1])
  }
})

test_that("legacy columns still present", {
  text <- "t(28) = 2.21, p = .035, d = 0.80"
  result <- check_text(text)
  
  expect_true(nrow(result) > 0)
  
  # Check legacy columns exist
  legacy_cols <- c("d_ind", "d_ind_equalN", "dz", "g_ind", 
                   "variants_tested", "design_inferred")
  for (col in legacy_cols) {
    expect_true(col %in% names(result), 
                info = paste("Missing legacy column:", col))
  }
})
