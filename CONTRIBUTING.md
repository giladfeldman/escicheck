# Contributing to effectcheck

Thank you for helping. effectcheck is a research tool whose output people rely on, so the most
valuable contributions are **verification reports**: a published result where effectcheck's
verdict is wrong, with the exact input text.

## Reporting a wrong verdict

Open an issue at <https://github.com/giladfeldman/escicheck/issues> with:

1. the exact text you passed to `check_text()` (a sentence or two is enough — please do not
   paste whole articles; refer to the paper by DOI);
2. the row you got (at least `test_type`, `status`, `matched_variant`, `matched_value`,
   `delta_effect`, `ci_check_status`, `uncertainty_reasons`);
3. what you expected and why (the correct value and how you computed it);
4. `packageVersion("effectcheck")` and `R.version.string`.

## Code changes

- This repository is **published from the maintainer's development tree**. Pull requests are
  welcome and are reviewed here, then applied upstream and mirrored back — so a merged change
  arrives through the next sync commit rather than as your original commit.
- Add a `testthat` test that fails without your change and passes with it.
- Keep `R/` sources ASCII-only (use `\uXXXX` escapes); R CMD check `--as-cran` warns otherwise,
  and `tests/testthat/test-ascii-source-discipline.R` enforces it.
- Document every user-visible change in `NEWS.md`, and in `README.md` /
  `docs/output-columns.md` if it adds or changes an exported function, an argument, a result
  column or a status value. `tools/check_docs_coverage.R` fails when any of those is missing
  from the documentation:

  ```sh
  Rscript tools/check_docs_coverage.R          # surface + versions + runs the README quickstart
  Rscript tools/check_docs_coverage.R --no-run # skip the quickstart
  ```

- Run the tests with `testthat::test_local()` (or `devtools::test()`) from the package root.

## Conduct

Be precise and be kind. Disagreement about a statistic is settled by computing it.
