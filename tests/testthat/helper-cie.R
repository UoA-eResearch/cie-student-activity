local_edition(3)

ensure_browser <- function() {
  candidates <- unique(c(
    Sys.getenv("CHROMOTE_CHROME", unset = ""),
    "/usr/local/bin/chrome",
    "/opt/chrome/chrome"
  ))
  browser <- candidates[nzchar(candidates) & file.exists(candidates)][1]

  skip_if(is.na(browser) || !nzchar(browser), "Chrome binary not available for shinytest2")
  Sys.setenv(CHROMOTE_CHROME = browser)
  invisible(browser)
}

repo_path <- function(...) {
  testthat::test_path("..", "..", ...)
}

new_dashboard_app <- function() {
  ensure_browser()
  shinytest2::AppDriver$new(
    app_dir = repo_path("cie-dashboards"),
    name = "dashboard",
    load_timeout = 60000,
    timeout = 30000,
    height = 1400,
    width = 1600
  )
}

new_upload_app <- function(env = character()) {
  ensure_browser()
  withr::with_envvar(
    env,
    shinytest2::AppDriver$new(
      app_dir = repo_path("cie-uploads"),
      name = "uploads",
      load_timeout = 60000,
      timeout = 30000,
      height = 1400,
      width = 1600
    )
  )
}

# Copy only the source spreadsheets the upload app reads, not the whole data tree.
# Copying everything filled the disk: all.csv is 150MB+ and backup_data holds every
# historical version of it (tens of GB). The generated files this omits — all.csv,
# all_training.csv, all_studio.csv and the backup versions — are what process_write()
# produces, so the tests assert they get created rather than needing them up front.
make_upload_sandbox <- function() {
  root <- file.path(tempdir(), paste0("cie-upload-test-", as.integer(Sys.time()), "-", sample.int(100000, 1)))
  data_dir <- file.path(root, "data")
  backup_dir <- file.path(root, "backup_data")

  fs::dir_create(c(data_dir, backup_dir))

  source_data <- repo_path("data")
  year_dirs <- list.files(source_data, pattern = "^[0-9]{4}$")
  for (entry in c(year_dirs, "base", "tags", "training")) {
    from <- file.path(source_data, entry)
    if (dir.exists(from)) {
      fs::dir_copy(from, file.path(data_dir, entry))
    }
  }

  list(root = root, data_dir = data_dir, backup_dir = backup_dir)
}

fetch_url_body <- function(url, retries = 15, delay_sec = 1) {
  for (i in seq_len(retries)) {
    out <- tryCatch({
      con <- base::url(url, open = "rb")
      on.exit(close(con), add = TRUE)
      raw <- readBin(con, what = "raw", n = 5 * 1024 * 1024)
      rawToChar(raw)
    }, error = function(e) "")

    if (nzchar(out)) {
      return(out)
    }

    Sys.sleep(delay_sec)
  }

  stop(sprintf("Failed to fetch %s", url), call. = FALSE)
}

radio_choices <- function(app, id) {
  js <- sprintf(
    "Array.from(document.querySelectorAll('#%s .radio label')).map((el) => el.textContent.trim()).filter((x) => x.length > 0);",
    id
  )
  unlist(app$get_js(js), use.names = FALSE)
}