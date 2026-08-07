#!/usr/bin/env Rscript

library(testthat)

# shinytest2's AppDriver calls skip_on_cran(), so without this every browser-driven
# test is skipped (and AppDriver errors with "Reason: On CRAN") rather than run.
if (!nzchar(Sys.getenv("NOT_CRAN"))) {
  Sys.setenv(NOT_CRAN = "true")
}

testthat::local_edition(3)
testthat::test_dir("tests/testthat", reporter = "summary")