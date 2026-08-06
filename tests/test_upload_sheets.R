#!/usr/bin/env Rscript

# Regression checks for upload sheet selection logic.
source("cie-uploads/sheet_choices.R", local = TRUE)

stopifnot(identical(get_save_sheet_choices("From Rachel - ", NULL), "None"))
stopifnot(identical(get_save_sheet_choices("Members and Training ", ""), "None"))
stopifnot(identical(get_save_sheet_choices("From Rachel - ", "/tmp/does-not-exist.xlsx"), "None"))
stopifnot(identical(get_save_sheet_choices("None", NULL), "None"))

cat("PASS: upload sheet selection regression checks\n")
