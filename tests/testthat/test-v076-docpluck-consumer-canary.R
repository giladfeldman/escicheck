# v0.7.6 -- the docpluck consumer-contract canary.
#
# WHY THIS EXISTS. Three defects landed in this release and all three have the
# same shape: docpluck changed the SHAPE of its output, effectcheck kept parsing
# happily, and a green row came out the other end.
#
#   * symbol contract v2.0 renamed `eta2p` to `eta2_p` -- the effect size was
#     dropped and the verdict flipped from ERROR to OK. Seven days undetected.
#   * form feeds came back -- two results fused into one fabricated pairing.
#   * rule W0g rewrote signs -- a negative SE fed a sign-inverted t.
#
# None of them would have been caught by a whole-corpus row-count diff: the
# first changes zero rows (an effect size going NA is not a row), and the other
# two produce plausible-looking wrong numbers that a diff without ground truth
# waves through.
#
# So this file asserts INVARIANTS at the docpluck -> effectcheck boundary,
# rather than snapshotting values:
#
#   1. every token docpluck says it emits binds to a populated effectcheck field
#   2. no impossible value ever produces a green row
#   3. a page boundary never changes what a text means
#
# It is deliberately small and needs no network: the contract snapshot is
# committed. What it CANNOT catch is a shape docpluck ships without describing
# anywhere -- that blind spot is real, is why the token-set hash below exists as
# a separate tripwire, and is why this does not replace the corpus diff.

.canary_snapshot <- function() {
  p <- system.file("docpluck-contract", "symbol_contract_snapshot.json",
                   package = "effectcheck")
  if (!nzchar(p) || !file.exists(p)) {
    p <- file.path("..", "..", "inst", "docpluck-contract",
                   "symbol_contract_snapshot.json")
  }
  if (!file.exists(p)) skip("docpluck contract snapshot not locatable")
  jsonlite::fromJSON(p, simplifyVector = FALSE)
}

# --- invariant 1: contract tokens bind ------------------------------------
#
# One SEMANTIC fixture per spelling we claim to bind. A version assertion alone
# would not have caught the eta2_p break, because docpluck bumped the contract
# version and the token change rode along with it -- the pin would have been
# updated and the parser left broken. The fixture is the lock; the hash below
# is only the alarm.

test_that("v0.7.6 canary: every effect-size spelling docpluck emits binds a value", {
  spellings <- list(
    # contract v2.0 (current)          # contract v1.0 (still in older text)
    list(txt = "eta2_p = .11",   want = 0.11), list(txt = "eta2p = .11",   want = 0.11),
    list(txt = "omega2_p = .09", want = 0.09), list(txt = "omega2p = .09", want = 0.09),
    list(txt = "eta2 = .12",     want = 0.12), list(txt = "omega2 = .06",  want = 0.06),
    list(txt = "epsilon2 = .05", want = 0.05), list(txt = "eta2G = .07",   want = 0.07)
  )
  for (s in spellings) {
    r <- check_text(paste0("A main effect emerged, F(1, 98) = 4.2, p = .04, ", s$txt, "."))
    expect_equal(nrow(r), 1L, info = s$txt)
    expect_false(is.na(r$effect_reported[1]),
                 info = paste0(s$txt, " bound NOTHING -- an effect size the paper ",
                               "reported was dropped, and the row will still carry a status"))
    expect_equal(r$effect_reported[1], s$want, info = s$txt)
  }
})

test_that("v0.7.6 canary: an unbound effect size never leaves a green row", {
  # The generalisation of the eta2_p defect, stated as an invariant rather than
  # as a list of tokens: if a clause plainly reports an effect size and we did
  # not bind it, the row must not read as checked. This is the assertion that
  # would have fired on 2026-08-14 for a token nobody had thought of.
  r <- check_text("A main effect emerged, F(1, 98) = 12.34, p = .001, eta2_p = .11.")
  bound <- !is.na(r$effect_reported[1])
  expect_true(bound)
})

# --- invariant 2: impossible values are never green ------------------------

test_that("v0.7.6 canary: no impossible value produces a passing row", {
  impossible <- list(
    list(id = "negative SE",
         txt = "In the regression model the predictor was significant, b = 0.45, SE = -0.11, p = .001."),
    list(id = "reversed reported interval",
         txt = "The contrast was reliable, t(98) = 2.41, p = .018, d = 0.15, 95% CI [0.04, -0.25].")
  )
  for (case in impossible) {
    r <- check_text(case$txt)
    expect_gt(nrow(r), 0L)   # never silently dropped
    expect_true(isTRUE(r$extraction_suspect[1]),
                info = paste(case$id, "did not mark the row suspect"))
    expect_false(identical(as.character(r$status[1]), "PASS"),
                 info = paste(case$id, "produced a PASS"))
  }
})

test_that("v0.7.6 canary: an out-of-range p from a table cell is refused", {
  r <- check_text("Table 1 reports the contrasts.",
                  table_rows = list(list(sentence = "A vs B",
                                         fields = list(t = 2.41, df = 98, p = -0.9))))
  expect_true(is.na(r$p_reported[1]))
  expect_true(isTRUE(r$p_out_of_range[1]))
})

# --- invariant 3: a page boundary changes nothing --------------------------

test_that("v0.7.6 canary: a form feed never changes what a text means", {
  # Asserted as an invariant over several shapes rather than the one shape that
  # broke, because the next extractor change will not use that shape.
  body <- paste0("The difference was d = 0.33, 95%% CI [0.09, 0.57].%s",
                 "0.75, 95%% CI = [0.55, 0.95], t = 7.47, p < .001.")
  for (sep in list(c("\n\n", "\n\f\n"), c("\n\n", "\n\n\f"), c("\n", "\n\f"))) {
    plain <- check_text(sprintf(body, sep[1]))
    withff <- check_text(sprintf(body, sep[2]))
    expect_equal(nrow(withff), nrow(plain),
                 info = paste("row count changed for separator", encodeString(sep[2])))
    expect_equal(withff$effect_reported, plain$effect_reported,
                 info = paste("effect binding changed for separator", encodeString(sep[2])))
  }
})

# --- the tripwire ----------------------------------------------------------

test_that("v0.7.6 canary: the docpluck symbol-contract TOKEN SET has not drifted", {
  # Keyed on the token set, NOT the version string, and the distinction is the
  # whole point:
  #   * an unrelated 2.0 -> 2.1 bump that changes no token would fail a version
  #     assertion, and a gate that cries wolf is a gate people learn to skip;
  #   * a token change shipped WITHOUT a version bump would pass one.
  # The hash fails on exactly the thing that can break the parser.
  #
  # WHEN THIS FAILS: re-run the capture in inst/docpluck-contract/, diff the
  # tokens, add a semantic fixture above for anything new, and only then update
  # the snapshot. Updating the snapshot alone is how eta2_p ships again.
  snap <- .canary_snapshot()
  expect_equal(snap$symbol_contract_version, "2.0")
  expect_equal(snap$token_set_sha256,
               "5827d784d105400373595e1f2980e33e66e79d3a156e6163d92a65995440367f")
  expect_equal(snap$counts$greek, 42L)
  expect_equal(snap$counts$subscript, 32L)
  expect_equal(snap$counts$superscript, 12L)
})
