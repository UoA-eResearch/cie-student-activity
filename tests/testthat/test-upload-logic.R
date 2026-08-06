source(repo_path("cie-uploads", "sheet_choices.R"), local = TRUE)

test_that("sheet choice logic handles missing files safely", {
  expect_identical(get_save_sheet_choices("From Rachel - ", NULL), "None")
  expect_identical(get_save_sheet_choices("Members and Training ", ""), "None")
  expect_identical(get_save_sheet_choices("From Rachel - ", "/tmp/does-not-exist.xlsx"), "None")
  expect_identical(get_save_sheet_choices("None", NULL), "None")
})