get_save_sheet_choices <- function(save_type, upload_file_path) {
  sso_types <- c("From Rachel - ", "Members and Training ")
  allowed_sheets <- c(
    "3D Printer",
    "Laser Cutter",
    "3D Scanner",
    "Vinyl Cutter",
    "CNC Router",
    "Sewing Machine",
    "Soldering and Desoldering Stati",
    "Hand and Power Tools",
    "Student",
    "Applicant",
    "No Affil",
    "No citizenship"
  )

  if (!(save_type %in% sso_types)) {
    return("None")
  }

  if (is.null(upload_file_path) || !nzchar(upload_file_path) || !file.exists(upload_file_path)) {
    return("None")
  }

  sheets <- tryCatch(readxl::excel_sheets(upload_file_path), error = function(...) character(0))
  choices <- intersect(sheets, allowed_sheets)
  if (length(choices) == 0) {
    return("None")
  }

  choices
}
