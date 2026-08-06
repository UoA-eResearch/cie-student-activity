source(repo_path("cie-uploads", "functions.R"), local = TRUE)

test_that("uploaded files are archived outside the processing directories", {
  root <- file.path(tempdir(), paste0("upload-archive-", as.integer(Sys.time()), "-", sample.int(100000, 1)))
  data_dir <- file.path(root, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  source_file <- file.path(root, "sample.xlsx")
  writeLines("raw upload", source_file)

  archived <- persist_uploaded_file(source_file, data_dir, "2026", "sample.xlsx")

  expect_true(file.exists(archived))
  expect_identical(archived, file.path(data_dir, "uploads", "2026", "sample.xlsx"))
  expect_false(file.exists(file.path(data_dir, "2026", "sample.xlsx")))
})

test_that("csv backups are only versioned when content changes", {
  root <- file.path(tempdir(), paste0("backup-utils-", as.integer(Sys.time()), "-", sample.int(100000, 1)))
  data_dir <- file.path(root, "data")
  backup_dir <- file.path(root, "backup")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(backup_dir, recursive = TRUE, showWarnings = FALSE)

  output_path <- file.path(data_dir, "all.csv")
  mirror_path <- file.path(backup_dir, "all.csv")
  version_dir <- file.path(backup_dir, "all")

  df_one <- tibble::tibble(id = 1:2, value = c("a", "b"))
  df_two <- tibble::tibble(id = 1:3, value = c("a", "b", "c"))

  expect_true(update_csv_with_backups(df_one, output_path, mirror_path, version_dir, "all"))
  expect_equal(length(list.files(version_dir)), 1)
  expect_false(file.exists(mirror_path))

  expect_false(update_csv_with_backups(df_one, output_path, mirror_path, version_dir, "all"))
  expect_equal(length(list.files(version_dir)), 1)

  expect_true(update_csv_with_backups(df_two, output_path, mirror_path, version_dir, "all"))
  expect_true(file.exists(mirror_path))
  expect_equal(length(list.files(version_dir)), 2)
})

test_that("duplicate backups in the same directory are removed", {
  root <- file.path(tempdir(), paste0("backup-dedupe-", as.integer(Sys.time()), "-", sample.int(100000, 1)))
  dir.create(root, recursive = TRUE, showWarnings = FALSE)

  duplicate_one <- file.path(root, "dup-1.csv")
  duplicate_two <- file.path(root, "dup-2.csv")
  unique_file <- file.path(root, "unique.csv")

  writeLines(c("same", "content"), duplicate_one)
  writeLines(c("same", "content"), duplicate_two)
  writeLines(c("different"), unique_file)

  removed <- deduplicate_backup_dir(root)

  expect_equal(sort(basename(removed)), "dup-2.csv")
  expect_true(file.exists(duplicate_one))
  expect_false(file.exists(duplicate_two))
  expect_true(file.exists(unique_file))
})