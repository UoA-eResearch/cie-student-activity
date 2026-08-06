test_that("upload app updates sheet choices, previews data, and saves in a sandbox", {
  sandbox <- make_upload_sandbox()

  app <- new_upload_app(c(
    CIE_DATA_DIR = sandbox$data_dir,
    CIE_BACKUP_DIR = sandbox$backup_dir
  ))
  on.exit(app$stop(), add = TRUE)

  app$set_inputs(saveType = "From Rachel - ")
  expect_identical(radio_choices(app, "saveSheet"), "None")

  app$upload_file(uploadFile = repo_path("data", "base", "From Rachel - 2019 CIE Participants.xlsx"))
  app$set_inputs(saveType = "From Rachel - ")
  app$wait_for_idle(timeout = 60000)

  expect_true(all(c("Student", "Applicant", "No Affil", "No citizenship") %in% radio_choices(app, "saveSheet")))

  app$set_inputs(saveSheet = "Student")
  app$wait_for_value(output = "saveFileName", timeout = 60000)
  app$wait_for_js("document.querySelectorAll('#contents table tbody tr').length > 0", timeout = 60000)

  save_name <- paste(app$get_text("#saveFileName"), collapse = " ")
  expect_match(save_name, "From Rachel - ", fixed = TRUE)
  expect_match(save_name, ".xlsx", fixed = TRUE)

  app$click("save")
  app$wait_for_js("document.querySelector('#status').textContent.includes('Success!')", timeout = 120000)

  archived_files <- list.files(file.path(sandbox$data_dir, "uploads", format(Sys.Date(), "%Y")), pattern = "^From Rachel - .*\\.xlsx$", full.names = TRUE)
  expect_gte(length(archived_files), 1)

  saved_files <- list.files(file.path(sandbox$data_dir, format(Sys.Date(), "%Y")), pattern = "^From Rachel - .*\\.xlsx$", full.names = TRUE)
  expect_gte(length(saved_files), 1)
  expect_true(file.exists(file.path(sandbox$data_dir, "all.csv")))
  expect_true(file.exists(file.path(sandbox$backup_dir, "all")))

  app$click("reload")
  app$wait_for_js("document.querySelector('#status').textContent.includes('Success!')", timeout = 120000)
})

test_that("upload app previews CRM csv files", {
  sandbox <- make_upload_sandbox()

  app <- new_upload_app(c(
    CIE_DATA_DIR = sandbox$data_dir,
    CIE_BACKUP_DIR = sandbox$backup_dir
  ))
  on.exit(app$stop(), add = TRUE)

  app$upload_file(uploadFile = repo_path("data", "base", "Original - 2017 CIE Participant - downloaded 10 July.csv"))
  app$set_inputs(saveType = "Original - ")
  app$wait_for_value(output = "saveFileName", timeout = 60000)
  app$wait_for_js("document.querySelectorAll('#contents table tbody tr').length > 0", timeout = 60000)

  save_name <- paste(app$get_text("#saveFileName"), collapse = " ")
  expect_match(save_name, "Original - ", fixed = TRUE)
  expect_match(save_name, ".csv", fixed = TRUE)
})