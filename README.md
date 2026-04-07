# effectcheck

> **IMPORTANT: Development Version.** This package is under heavy development
> and has not yet been fully validated. Results should be independently verified
> before use in any consequential context (e.g., peer review, retractions,
> editorial decisions). Use of this package is at the sole responsibility of
> the user. We welcome contributions, verification reports, and bug reports at
> <https://github.com/giladfeldman/escicheck/issues> or by contacting
> Gilad Feldman (<giladfel@gmail.com>).

**EffectCheck** is a conservative, assumption-aware statistical consistency checker for published research results. It parses statistical results across multiple citation styles (APA, Harvard, Frontiers, PLOS ONE, Scientific Reports, Nature Human Behaviour, PeerJ, eLife, PNAS, and more), uses **type-matched comparison** to verify reported effect sizes against computed variants of the **same type**, explicitly flags all assumptions and uncertainty, and validates internal consistency between test statistics, effect sizes, and confidence intervals.

## Reference

This package implements effect size computations following the [Guide to Effect Sizes and Confidence Intervals](https://matthewbjane.quarto.pub/) by Jané et al. (2024). The `effectsize` R package is used as the primary computation engine when available.

**Citation for the Guide:**
> Jané, M.B., Xiao, Q., Yeung, S., et al. (2024). Guide to Effect Sizes and Confidence Intervals. http://dx.doi.org/10.17605/OSF.IO/D8C4G

## Features

### Statistical Coverage

- **Mean Differences**: Cohen's d, Hedges' g (independent); dz, dav, drm (paired)
- **ANOVA / F-tests**: Eta², partial eta², generalized eta², omega², Cohen's f
- **Correlations**: Pearson r, Spearman ρ, Kendall τ
- **Contingency Tables**: Phi (φ), Cramer's V
- **Regression / GLM**: Standardized β, partial r, semi-partial r, f², R²
- **Ratio Measures**: OR, RR, IRR
- **Nonparametric**: Rank-biserial r, Cliff's delta, Kendall's W, ε²

### Key Capabilities

- **Type-matched comparison**: Compares reported effect sizes against computed variants of the SAME type (e.g., reported d compared to d variants, not g)
- **Ambiguity handling**: When design is unclear, computes ALL variants and finds closest match among same-type variants
- **Alternative suggestions**: Shows other effect size types as suggestions (e.g., Hedges' g as alternative to Cohen's d)
- **Ultra-robust parsing** with Unicode normalization, locale-aware decimals, and PDF artifact handling
- **Comprehensive CI computation** with method tracking and fallback strategies
- **Uncertainty framework** that tracks all assumptions and design inferences
- **Context window extraction** for design inference (±2 sentences)
- **Decision error detection** (significance reversals) similar to statcheck
- **statcheck-compatible API** for batch processing of files and directories
- **Multiple export formats**: HTML (with expandable sections), CSV, JSON
- **S3 class with methods**: print, summary, plot, ec_identify, get_variants

## Installation

### From Source

```r
# Install dependencies
install.packages(c("devtools", "stringr", "stringi", "dplyr", "purrr", 
                   "tibble", "tidyr", "xml2", "rvest",
                   "DT", "shiny", "shinythemes", "testthat", "jsonlite"))

# Install effectcheck
devtools::install_local("effectcheck")
```

### Optional Dependencies

For enhanced CI computation:
- `MBESS` - Noncentral t-distribution CIs for Cohen's d
- `effectsize` - Additional effect size computations

## Quick Start

### Basic Usage

```r
library(effectcheck)

# Check text directly
text <- "t(28) = 2.21, p = .035, d = 0.80, 95% CI [0.12, 1.48]"
result <- check_text(text)
print(result)
summary(result)

# Check a single file
result <- check_file("paper.pdf")

# Check multiple files
results <- check_files(c("paper1.docx", "paper2.html"))

# Check a directory (recursively)
results <- check_dir("manuscripts/")

# Export results
render_report(result, "report.html")
export_csv(result, "results.csv")
export_json(result, "results.json")
```

### statcheck-Compatible API

```r
# Check PDF files (like statcheck's checkPDF)
results <- checkPDF(c("paper1.pdf", "paper2.pdf"))

# Check HTML files (like statcheck's checkHTML)
results <- checkHTML(c("paper1.html", "paper2.html"))

# Check directory of PDFs (like statcheck's checkPDFdir)
results <- checkPDFdir("manuscripts/")

# Check directory of HTML files (like statcheck's checkHTMLdir)
results <- checkHTMLdir("manuscripts/")
```

### Working with Results

```r
# Get summary statistics
summary(result)

# Plot results
plot(result, type = "status")      # Status distribution
plot(result, type = "uncertainty") # Uncertainty levels
plot(result, type = "all")         # All plots

# Filter problematic results
errors <- ec_identify(result, "errors")
warnings <- ec_identify(result, "warnings")
decision_errors <- get_decision_errors(result)
high_uncertainty <- filter_by_uncertainty(result, "high")

# Filter by test type
t_tests <- filter_by_test_type(result, "t")

# Count by category
count_by(result, "status")
count_by(result, "test_type")
```

### Working with Variants (New in v2.0)

```r
# Get all variants for a specific row
variants <- get_variants(result, row_index = 1)

# Get same-type variants only
same_type <- get_same_type_variants(result, row_index = 1)

# Get alternative suggestions
alternatives <- get_alternatives(result, row_index = 1)

# Format variants for display
cat(format_variants(result, row_index = 1))

# Compare reported value to all variants
comparison <- compare_to_variants(result, row_index = 1)
print(comparison)

# Get metadata for a specific variant type
metadata <- get_variant_metadata("dz")
print(metadata$when_to_use)
```

## Understanding Results

### Status Levels

- **PASS**: Computed effect size matches reported value within tolerance
- **WARN**: Minor mismatch (rounding, CI method difference, design ambiguity)
- **ERROR**: Significant mismatch (incompatible values)
- **SKIP**: Extraction-only result with nothing to check
- **NOTE**: Cannot fully verify, or decision error suppressed by context

### Uncertainty Levels

- **Low**: All required information present, no assumptions
- **Medium**: Minor assumptions made (e.g., equal group sizes)
- **High**: Major assumptions or missing critical information

### Output Columns

Key columns in results:

**Reported Values:**
- `reported_type`: The effect size type user reported (d, g, r, eta2, etc.)
- `effect_reported`: Reported effect size value
- `effect_reported_name`: Original name as parsed from text

**Type-Matched Comparison (New in v2.0):**
- `matched_variant`: Which same-type variant matched best
- `matched_value`: The computed value of that variant
- `delta_effect`: Absolute difference between reported and matched
- `ambiguity_level`: "clear", "ambiguous", or "highly_ambiguous"
- `ambiguity_reason`: Why comparison was ambiguous (if applicable)
- `all_variants`: JSON structure with all computed variants and alternatives

**Legacy Columns (backward compatible):**
- `test_type`: Type of test (t, F, r, chisq, z)
- `closest_method`: Alias for matched_variant
- `delta_effect_abs`: Alias for delta_effect

**P-value and Decision Errors:**
- `p_reported`: Reported p-value
- `p_computed`: Computed p-value from test statistic
- `decision_error`: TRUE if significance reversal detected

**Status and Uncertainty:**
- `status`: PASS/WARN/ERROR/INSUFFICIENT_DATA
- `uncertainty_level`: Low/medium/high
- `uncertainty_reasons`: Specific sources of uncertainty
- `assumptions_used`: All assumptions made during computation
- `design_inferred`: Inferred study design (paired, independent, etc.)
- `assumptions_used`: All assumptions made during computation
- `design_inferred`: Best guess at experimental design
- `variants_tested`: All effect size variants computed
- `source`: Source file name (or "text" for direct input)

## Examples

### Example 1: Independent t-test

```r
text <- "t(28) = 2.21, p = .035, d = 0.80, 95% CI [0.12, 1.48], N = 30, n1 = 15, n2 = 15"
result <- check_text(text)
```

### Example 2: ANOVA

```r
text <- "F(2, 27) = 4.56, p < .05, η² = 0.25"
result <- check_text(text)
# Computes: eta², partial eta², omega², Cohen's f
```

### Example 3: Correlation

```r
text <- "r(198) = .34, p < .001, 95% CI [.21, .45]"
result <- check_text(text)
```

### Example 4: Multiple Statistics

```r
text <- "t(28) = 2.21, d = 0.80. Another test: F(2, 27) = 4.56, η² = 0.25."
result <- check_text(text)
# Returns one row per statistic detected
```

## Advanced Usage

### Custom Tolerances

```r
result <- check_text(
  text,
  tol_effect = list(d = 0.02, r = 0.005, phi = 0.02, V = 0.02, eta2 = 0.01),
  tol_ci = 0.02
)
```

### Paired t-test r-grid

```r
result <- check_text(
  text,
  paired_r_grid = seq(0.1, 0.9, by = 0.1)  # Default
)
# Shows dav range across correlation assumptions
```

### CI Level Detection

EffectCheck automatically detects CI level from text:
- `95% CI [0.12, 0.45]` → uses 95%
- `CI [0.12, 0.45]` → defaults to 95% (flagged as assumption)

## File Format Support

- **.pdf**: PDF files (via `pdftotext` from poppler-utils, with optional table extraction and OCR)
- **.docx**: Microsoft Word documents (via `pandoc`; requires pandoc on PATH)
- **.html**: HTML files (via `rvest` package)
- **.txt**: Plain text files

### PDF Options

```r
# Enable table extraction from PDFs
results <- checkPDF("paper.pdf", try_tables = TRUE)

# Enable OCR for scanned PDFs
results <- checkPDF("scanned_paper.pdf", try_ocr = TRUE)
```

## Philosophy

EffectCheck follows these principles:

1. **Conservative by Design**: When ambiguous, compute all variants rather than guessing
2. **Transparency First**: All assumptions visible in output
3. **Never Silent**: Always flag uncertainty and assumptions
4. **Comprehensive**: Compute all plausible effect size variants

## Limitations

- Some effect sizes require additional information (e.g., generalized eta² for within-subjects)
- CI methods may differ from those used in original papers (flagged as WARN)
- Design inference from context is heuristic-based
- OCR for scanned PDFs requires `tesseract` and `magick` packages

## API Reference

### Core Functions

| Function | Description |
|----------|-------------|
| `check_text(text, ...)` | Check plain text for statistical consistency |
| `check_file(path, ...)` | Check a single file |
| `check_files(paths, ...)` | Check multiple files |
| `check_dir(dir, ...)` | Check all files in a directory |

### statcheck-Compatible Functions

| Function | Description |
|----------|-------------|
| `checkPDF(files, ...)` | Check PDF files |
| `checkHTML(files, ...)` | Check HTML files |
| `checkPDFdir(dir, ...)` | Check directory of PDFs |
| `checkHTMLdir(dir, ...)` | Check directory of HTML files |
| `checkDOCXdir(dir, ...)` | Check directory of Word documents |

### S3 Methods

| Method | Description |
|--------|-------------|
| `print(x)` | Print formatted summary |
| `summary(x)` | Comprehensive summary statistics |
| `plot(x, type)` | Visualize results |
| `ec_identify(x, what)` | Filter problematic results |

### Filter Functions

| Function | Description |
|----------|-------------|
| `get_errors(x)` | Get ERROR status results |
| `get_warnings(x)` | Get WARN status results |
| `get_decision_errors(x)` | Get significance reversals |
| `filter_by_test_type(x, types)` | Filter by test type |
| `filter_by_uncertainty(x, levels)` | Filter by uncertainty level |
| `filter_by_source(x, files)` | Filter by source file |
| `filter_by_delta(x, min, max)` | Filter by effect size delta |
| `count_by(x, by)` | Count results by category |

### Export Functions

| Function | Description |
|----------|-------------|
| `render_report(x, out)` | Generate HTML report |
| `export_csv(x, out)` | Export to CSV |
| `export_json(x, out)` | Export to JSON |

## Citation

If you use EffectCheck in your research, please cite:

```
EffectCheck: Statistical Consistency Checker for Published Research Results
Version 0.3.0
```

## License

MIT License - see LICENSE file for details

## Contributing

Contributions welcome! Please open an issue or pull request at <https://github.com/giladfeldman/escicheck>.

## Related Tools

- [statcheck](https://www.statcheck.io/) - Checks p-values in APA-style text
- [effectsize](https://easystats.github.io/effectsize/) - R package for effect size computation

## Support

For issues, questions, or feature requests, please open an issue on the project repository.
