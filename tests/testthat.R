library(testthat)
library(effectcheck)

# R CMD check evaluates this file with R CMD BATCH on Windows.  Make the
# runner's outcome explicit: testthat prints a useful summary, while this
# process exits successfully only when every expectation did.  This avoids a
# platform-dependent non-zero BATCH exit after an otherwise clean suite.
results <- test_check("effectcheck", reporter = "summary", stop_on_failure = FALSE)
result_table <- as.data.frame(results)
has_failure <- any(
  result_table$failed > 0L |
    result_table$error |
    result_table$warning > 0L,
  na.rm = TRUE
)
quit(save = "no", status = if (has_failure) 1L else 0L)
