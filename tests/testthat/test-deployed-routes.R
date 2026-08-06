test_that("deployed shiny routes are reachable inside the container", {
  base_url <- Sys.getenv("BASE_URL", unset = "http://127.0.0.1:3838")

  dashboard_html <- fetch_url_body(paste0(base_url, "/cie-dashboards/"))
  uploads_html <- fetch_url_body(paste0(base_url, "/cie-uploads/"))

  expect_match(dashboard_html, "CIE Dashboard", fixed = TRUE)
  expect_match(uploads_html, "CIE Uploading Files", fixed = TRUE)
})

test_that("mounted data and backup directories are writable and complete", {
  required_files <- c(
    "/srv/shiny-server/data/all.csv",
    "/srv/shiny-server/data/all_training.csv",
    "/srv/shiny-server/data/all_studio.csv",
    "/srv/shiny-server/data/tags/tags_selection.csv"
  )

  expect_true(all(file.exists(required_files)), info = paste(required_files[!file.exists(required_files)], collapse = ", "))

  data_probe <- "/srv/shiny-server/data/.test-write-probe"
  backup_probe <- "/srv/shiny-server/backup_data/.test-write-probe"

  writeLines("ok", data_probe)
  writeLines("ok", backup_probe)

  expect_true(file.exists(data_probe))
  expect_true(file.exists(backup_probe))

  unlink(c(data_probe, backup_probe))
})