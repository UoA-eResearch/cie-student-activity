#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
base_url <- if (length(args) >= 1) args[[1]] else Sys.getenv("BASE_URL", unset = "http://127.0.0.1:3838")

trim_trailing_slash <- function(x) sub("/+$", "", x)
base_url <- trim_trailing_slash(base_url)

fetch_http <- function(path, retries = 20, delay_sec = 1) {
  url <- paste0(base_url, path)

  for (i in seq_len(retries)) {
    out <- tryCatch({
      con <- url(url, open = "rb")
      on.exit(close(con), add = TRUE)
      raw <- readBin(con, what = "raw", n = 5 * 1024 * 1024)
      list(ok = TRUE, body = rawToChar(raw), err = NULL)
    }, error = function(e) {
      list(ok = FALSE, body = "", err = conditionMessage(e))
    })

    if (isTRUE(out$ok) && nzchar(out$body)) {
      return(list(body = out$body, url = url))
    }

    Sys.sleep(delay_sec)
  }

  stop(sprintf("Failed to fetch %s successfully after %d retries", url, retries))
}

assert_contains <- function(text, needle, label) {
  if (!grepl(needle, text, fixed = TRUE)) {
    stop(sprintf("Missing %s: %s", label, needle))
  }
}

extract_asset_paths <- function(html, pattern) {
  href_match <- gregexpr('href="[^"]+"', html, perl = TRUE)
  hrefs <- regmatches(html, href_match)[[1]]
  if (length(hrefs) == 0) {
    return(character(0))
  }

  href_vals <- sub('^href="', "", sub('"$', "", hrefs))
  href_vals[grepl(pattern, href_vals)]
}

write_probe <- function(dir_path, filename) {
  probe <- file.path(dir_path, filename)
  ok <- tryCatch({
    writeLines("ok", probe)
    TRUE
  }, error = function(...) FALSE)

  if (!ok) {
    stop(sprintf("Cannot write probe file in %s", dir_path))
  }

  unlink(probe)
}

dash <- fetch_http("/cie-dashboards/")
uploads <- fetch_http("/cie-uploads/")

assert_contains(dash$body, "CIE Dashboard", "dashboard title")
assert_contains(dash$body, "Overview", "dashboard tab")
assert_contains(dash$body, "Programme", "dashboard tab")
assert_contains(dash$body, "Velocity", "dashboard tab")
assert_contains(dash$body, "Unleash Space", "dashboard tab")
assert_contains(dash$body, "Create and Maker Space", "dashboard tab")
assert_contains(dash$body, "Journey map", "dashboard tab")
assert_contains(dash$body, "Curricula vs co-curricular", "dashboard tab")

assert_contains(uploads$body, "CIE Uploading Files", "uploads title")
assert_contains(uploads$body, "Choose File", "uploads control")
assert_contains(uploads$body, "Select year", "uploads control")
assert_contains(uploads$body, "Select type of file", "uploads control")
assert_contains(uploads$body, "Save", "uploads action")
assert_contains(uploads$body, "Reload", "uploads action")

dash_assets <- extract_asset_paths(dash$body, "shiny.min.css|bootstrap")
if (length(dash_assets) == 0) {
  stop("No dashboard assets discovered in HTML")
}

for (asset in unique(dash_assets)) {
  if (grepl("^https?://", asset)) {
    next
  }
  if (!grepl("^/", asset)) {
    asset <- paste0("/cie-dashboards/", asset)
  }
  fetch_http(asset, retries = 5, delay_sec = 1)
}

required_files <- c(
  "/srv/shiny-server/data/all.csv",
  "/srv/shiny-server/data/all_training.csv",
  "/srv/shiny-server/data/all_studio.csv",
  "/srv/shiny-server/data/tags/tags_selection.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(sprintf("Missing required data files: %s", paste(missing_files, collapse = ", ")))
}

write_probe("/srv/shiny-server/data", ".write_probe_data")
write_probe("/srv/shiny-server/backup_data", ".write_probe_backup")

cat("PASS: functional checks for both apps succeeded\n")