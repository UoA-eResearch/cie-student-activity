#!/usr/bin/env Rscript

library(testthat)

testthat::local_edition(3)
testthat::test_dir("tests/testthat", reporter = "summary")