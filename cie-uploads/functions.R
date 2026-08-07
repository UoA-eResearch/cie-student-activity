# Settings
options(java.parameters = "-Xmx4G")

# Library
library(tidyverse)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(widyr)
library(reshape2)
library(tools)
library(networkD3)
library(plotly)
library(stringi)

## Replacements
facultyRename <- tibble(
  oldFaculty = c("Faculty of Business & Economic", "Faculty of Science", "Faculty of Arts", "Faculty of Law", "Faculty of Education", 
                 "Faculty of Engineering", "Faculty Creative Arts & Indust", "Faculty of Medical & Hlth Sci",
                 "Medical & Health Sciences", "Science", "Business & Economics", "Engineering", "Creative Arts & Industries", "Arts",
                 "Law", "The University of Auckland", "Education & Social Work", "Auck Bioengineering Institute", "Bioengineering Institute", "New Start",
                 "Centre for Cont Education", "Theology"
  ),
  newFaculty = c("Business & Economics", "Science", "Arts", "Law", "Education & Social Work",
                 "Engineering", "Creative Arts & Industries", "Medical & Health Sciences",
                 "Medical & Health Sciences", "Science", "Business & Economics", "Engineering", "Creative Arts & Industries", "Arts",
                 "Law", "The University of Auckland", "Education & Social Work", "Auck Bioengineering Institute", "Bioengineering Institute", "New Start",
                 "Centre for Cont Education", "Theology"
  )
)

escape_regex <- function(value) {
  # The replacement must be a literal backslash followed by the backreference ("\\\1"),
  # otherwise the result is the invalid backreference "\1" rather than an escaped character.
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", value)
}

ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
}

read_xlsx_character <- function(path, sheet, startRow) {
  df <- openxlsx::read.xlsx(path, sheet = sheet, startRow = startRow, detectDates = FALSE, check.names = FALSE)
  as.data.frame(lapply(df, as.character), check.names = FALSE, stringsAsFactors = FALSE)
}

persist_uploaded_file <- function(source_path, data_dir, year, file_name) {
  upload_dir <- file.path(data_dir, "uploads", year)
  ensure_directory(upload_dir)

  destination <- file.path(upload_dir, file_name)
  file.copy(source_path, destination, overwrite = TRUE)
  destination
}

same_file_contents <- function(path_a, path_b) {
  if (!file.exists(path_a) || !file.exists(path_b)) {
    return(FALSE)
  }

  info_a <- file.info(path_a)
  info_b <- file.info(path_b)

  if (is.na(info_a$size) || is.na(info_b$size) || info_a$size != info_b$size) {
    return(FALSE)
  }

  identical(
    digest::digest(file = path_a, algo = "xxhash64"),
    digest::digest(file = path_b, algo = "xxhash64")
  )
}

backup_file_if_unique <- function(source_path, backup_dir, pattern = NULL) {
  if (!file.exists(source_path)) {
    return(FALSE)
  }

  ensure_directory(backup_dir)

  candidates <- if (is.null(pattern)) {
    dir(backup_dir, full.names = TRUE)
  } else {
    dir(backup_dir, pattern = pattern, full.names = TRUE)
  }
  candidates <- candidates[!dir.exists(candidates)]

  if (length(candidates) > 0 && any(vapply(candidates, function(candidate) same_file_contents(source_path, candidate), logical(1)))) {
    return(FALSE)
  }

  file.copy(source_path, backup_dir, overwrite = FALSE)
}

# How many timestamped versions of each generated CSV to keep in the backup directory.
# These files are derived: process_write() rebuilds them from the uploaded spreadsheets
# under backup_data/<year>, which are never pruned. Without a limit they accumulate
# indefinitely — one copy of all.csv per upload, at 150MB+ each.
versioned_backup_limit <- as.integer(Sys.getenv("CIE_BACKUP_VERSIONS", unset = "5"))

versioned_backup_files <- function(backup_dir, name_prefix, extension_suffix) {
  # Anchored so that the "all" prefix does not also match all_training / all_studio
  pattern <- paste0("^", escape_regex(name_prefix), "-.*", escape_regex(extension_suffix), "$")
  candidates <- dir(backup_dir, pattern = pattern, full.names = TRUE)
  candidates[!dir.exists(candidates)]
}

# Delete the oldest versions of `name_prefix`, keeping the `keep` most recent.
prune_versioned_backups <- function(backup_dir, name_prefix, extension_suffix = ".csv", keep = versioned_backup_limit) {
  if (is.na(keep) || keep < 1 || !dir.exists(backup_dir)) {
    return(character())
  }

  candidates <- versioned_backup_files(backup_dir, name_prefix, extension_suffix)
  if (length(candidates) <= keep) {
    return(character())
  }

  # Newest first by modification time, falling back to name so ties are deterministic
  info <- file.info(candidates)
  ordered <- candidates[order(info$mtime, basename(candidates), decreasing = TRUE, method = "radix")]
  stale <- ordered[-seq_len(keep)]

  file.remove(stale)
  stale
}

store_versioned_backup_if_unique <- function(source_path, backup_dir, name_prefix) {
  if (!file.exists(source_path)) {
    return(FALSE)
  }

  ensure_directory(backup_dir)

  file_extension <- tools::file_ext(source_path)
  extension_suffix <- if (nzchar(file_extension)) paste0(".", file_extension) else ""
  candidates <- versioned_backup_files(backup_dir, name_prefix, extension_suffix)

  if (length(candidates) > 0 && any(vapply(candidates, function(candidate) same_file_contents(source_path, candidate), logical(1)))) {
    return(FALSE)
  }

  timestamp <- format(Sys.time(), "%Y-%m-%dT%H-%M-%S")
  target_path <- file.path(backup_dir, paste0(name_prefix, "-", timestamp, extension_suffix))
  counter <- 1
  while (file.exists(target_path)) {
    target_path <- file.path(backup_dir, paste0(name_prefix, "-", timestamp, "-", counter, extension_suffix))
    counter <- counter + 1
  }

  stored <- file.copy(source_path, target_path, overwrite = FALSE)
  prune_versioned_backups(backup_dir, name_prefix, extension_suffix)

  stored
}

update_csv_with_backups <- function(data_frame, output_path, mirror_backup_path = NULL, versioned_backup_dir = NULL, versioned_backup_prefix = NULL) {
  ensure_directory(dirname(output_path))

  temp_path <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_path), add = TRUE)
  write_csv(data_frame, temp_path)

  changed <- !file.exists(output_path) || !same_file_contents(output_path, temp_path)
  if (!changed) {
    return(FALSE)
  }

  if (!is.null(mirror_backup_path) && file.exists(output_path)) {
    ensure_directory(dirname(mirror_backup_path))
    if (!file.exists(mirror_backup_path) || !same_file_contents(output_path, mirror_backup_path)) {
      file.copy(output_path, mirror_backup_path, overwrite = TRUE)
    }
  }

  if (!is.null(versioned_backup_dir) && !is.null(versioned_backup_prefix)) {
    store_versioned_backup_if_unique(temp_path, versioned_backup_dir, versioned_backup_prefix)
  }

  file.copy(temp_path, output_path, overwrite = TRUE)
  TRUE
}

deduplicate_directory_files <- function(dir_path) {
  files <- dir(dir_path, full.names = TRUE, recursive = FALSE)
  files <- files[!dir.exists(files)]

  if (length(files) < 2) {
    return(character())
  }

  info <- file.info(files)
  size_groups <- split(files, info$size)
  removed <- character()

  for (group_files in size_groups) {
    if (length(group_files) < 2) {
      next
    }

    hashes <- vapply(group_files, function(path) digest::digest(file = path, algo = "xxhash64"), character(1))
    hash_groups <- split(group_files, hashes)

    for (duplicate_group in hash_groups) {
      if (length(duplicate_group) < 2) {
        next
      }

      group_info <- file.info(duplicate_group)
      keep_index <- order(group_info$mtime, basename(duplicate_group), na.last = TRUE)[1]
      removed <- c(removed, duplicate_group[-keep_index])
    }
  }

  removed <- unique(removed)
  if (length(removed) > 0) {
    file.remove(removed)
  }

  removed
}

deduplicate_backup_dir <- function(backup_dir) {
  directories <- c(backup_dir, dir(backup_dir, full.names = TRUE, recursive = TRUE))
  directories <- directories[dir.exists(directories)]

  unlist(lapply(directories, deduplicate_directory_files), use.names = FALSE)
}

## Functions
# Process all data files and write to "all.csv"
process_write <- function(data_dir, backup_dir) {
  style <- "notification"
  withProgress(message = "Processing new data", style = style, value = 0, {
    # Load SSO
    withProgress(message = "Loading SSO data", style=style, value =.5, {
      partInfo <- load_sso(data_dir)
      incProgress(.4) # Increment the progress bar
    })
    incProgress(.2)
    
    # Load TAG
    withProgress(message = "Loading TAG data", style=style, value =.5, {
      selection <- load_tag(data_dir, backup_dir, TRUE)
      incProgress(.4)
    })
    incProgress(.1)
    
    # Load CRM
    withProgress(message = "Loading CRM data", style=style, value =.5, {
      partProg <- load_crm(data_dir)
      incProgress(.4) # Increment the progress bar
    })
    incProgress(.1)
    
    # Load STUDIO
    withProgress(message = "Loading CRM data", style=style, value =.5, {
      listStudio <- load_studio(data_dir, backup_dir)
      partStudio <- listStudio[[1]]
      studio <- listStudio[[2]]
      incProgress(.2) # Increment the progress bar
      partProg <- rbind(partProg, partStudio)
      incProgress(.2) # Increment the progress bar
    })
    incProgress(.1)
    
    # Load TRAINING
    withProgress(message = "Loading TRAINING data", style=style, value =.5, {
      training <- load_training(data_dir)
      incProgress(.4) # Increment the progress bar
    })
    incProgress(.1)
    
    # Filtering
    withProgress(message = "Filtering data", style=style, value =.5, {
      partProg <- filter_programme(partProg,selection)
      incProgress(.4)
    })
    incProgress(.1)
    
    # Merging
    withProgress(message = "Merging data", style=style, value =.5, {
      all_df <- join_table(partProg, partInfo)
      incProgress(.1)
      all_df <- rbind.fill(all_df, training)
      all_df <- rbind.fill(all_df, studio)
      incProgress(.1)
      all_df$ID <- simple_id(all_df, c("ID"))
      incProgress(.1)
      # Split the datasets
      all_studio <- all_df %>% filter(!is.na(`timestamp`)) %>% select(`ID`, `date`, `purpose`, `equipment`, `comment`, `programme`, `timestamp`, `month`, `year`)
      all_training <- all_df %>% filter(is.na(`timestamp`)) %>% filter(!is.na(`date`)) %>% select(`ID`, `date`, `training`)
      all_df <- all_df %>% filter(is.na(`date`)) %>% select(-`date`, -`training`, -`purpose`, -`timestamp`, -`equipment`, -`comment`)
      
      incProgress(.1)
    })
    incProgress(.1)
    
    # Export
    withProgress(message = "Uploading data", style=style, value =.5, {
      all_changed <- update_csv_with_backups(
        all_df,
        file.path(data_dir, "all.csv"),
        mirror_backup_path = file.path(backup_dir, "all.csv"),
        versioned_backup_dir = file.path(backup_dir, "all"),
        versioned_backup_prefix = "all"
      )
      incProgress(.1)
      training_changed <- update_csv_with_backups(
        all_training,
        file.path(data_dir, "all_training.csv"),
        mirror_backup_path = file.path(backup_dir, "all_training.csv"),
        versioned_backup_dir = file.path(backup_dir, "all"),
        versioned_backup_prefix = "all_training"
      )
      studio_changed <- update_csv_with_backups(
        all_studio,
        file.path(data_dir, "all_studio.csv"),
        mirror_backup_path = file.path(backup_dir, "all_studio.csv"),
        versioned_backup_dir = file.path(backup_dir, "all"),
        versioned_backup_prefix = "all_studio"
      )
      incProgress(.1)
      incProgress(.1)
      # Remove cache on the server. Touching the dashboard sources makes Shiny discard
      # its R process and reload the CSVs, which costs the next visitor a cold start,
      # so only do it when this run actually produced different data.
      tags_changed <- isTRUE(attr(selection, "tags_csv_changed"))
      if (any(all_changed, training_changed, studio_changed, tags_changed)) {
        system("touch ../cie-dashboards/*.R")
      }
      incProgress(.1)
      Sys.sleep(0.2)
    })
    incProgress(.1)
    Sys.sleep(0.2)
  })
  
  return("Success!")
}

# Load SSO
load_sso <- function(data_dir) {
  # Gather file paths
  years <- list.files(data_dir, pattern = "\\d+")
  files <- dir(file.path(data_dir, years), pattern = "From.*xlsx", full.names = TRUE)
  # Get sheets available in excel workbooks
  availSheet <- lapply(files, excel_sheets)
  
  ##  Student sheet
  # Get column types
  c <- sapply(read_excel("../data/base/From Rachel - 2019 CIE Participants.xlsx", sheet = "Student", skip = 1), class)
  c["Birthdate"] <- "integer"
  # Read and clean sheet
  student <- tibble(updated = basename(dirname(files[1])), filename = files[1]) %>% 
    mutate(file_contents = map(filename, ~read_xlsx_character(file.path(.), sheet = "Student", startRow = 2))) %>% 
    select(-filename) %>% 
    unnest() %>% 
    group_by(ID) %>%
    filter(as.numeric(`Admit.Term`)==max(as.numeric(as.character(`Admit.Term`)))) %>% # Only select most recent updates
    ungroup() %>%
    distinct() # Remove duplicates
  student[] <- lapply(student, as.character)
  if ("Owner.of.Major/Spec/Module" %in% names(student) && !"Owner.of.Major.Spec.Module" %in% names(student)) {
    names(student)[names(student) == "Owner.of.Major/Spec/Module"] <- "Owner.of.Major.Spec.Module"
  }
  if ("Owner.of.Major.Spec.Module" %in% names(student)) {
    student$Owner.of.Major.Spec.Module <- facultyRename$newFaculty[match(student$Owner.of.Major.Spec.Module, facultyRename$oldFaculty)]
  }
  isStudent <- files[-1]
  colNames <- colnames(student)
  colNames  <- colNames[-4]
  for (file in isStudent) {
    studentMock <- tibble(updated = basename(dirname(file)), filename = file) %>% 
      mutate(file_contents = map(filename, ~read_xlsx_character(file.path(.), sheet = "Student", startRow = 2))) %>% 
      select(-filename) %>% 
      unnest() %>% 
      group_by(ID) %>%
      filter(as.numeric(`Admit.Term`)==max(as.numeric(as.character(`Admit.Term`)))) %>% # Only select most recent updates
      ungroup() %>%
      distinct() 
    studentMock[] <- lapply(studentMock, as.character)
    if ("Owner.of.Major/Spec/Module" %in% names(studentMock) && !"Owner.of.Major.Spec.Module" %in% names(studentMock)) {
      names(studentMock)[names(studentMock) == "Owner.of.Major/Spec/Module"] <- "Owner.of.Major.Spec.Module"
    }
    student <- student %>% 
      bind_rows(studentMock)
    student <- student[!duplicated(student[,colNames]),] # Remove duplicates
    student$Residency.Status[!student$Residency.Status %in% c("International", "New Zealand Citizen", "NZ Permanent Resident")] <- "International" # Change everything should be International to International
  }
  # Remove Birthdate columns
  student <- student %>% 
    select(-`Birthdate`)
  # Change values for "Alumni"
  if (all(c("Status", "Programme.Level", "Descriptio", "Owner.of.Major.Spec.Module") %in% names(student))) {
    alumni_rows <- which(student$Status == "Completed Programme")
  } else {
    alumni_rows <- integer(0)
  }
  if (length(alumni_rows) > 0) {
    student[alumni_rows, c('Programme.Level', 'Descriptio', 'Owner.of.Major.Spec.Module')] <- "ALUMNI"
  }
  # Consistent sex
  if ("Sex" %in% names(student)) {
    student$Sex[student$Sex == "F"] <- "Female"
    student$Sex[student$Sex == "M"] <- "Male"
    student$Sex[student$Sex == "D"] <- "Diverse"
  }
  
  ## Applicant sheet
  # Get excel workbooks that have "Applicant" sheet
  isAppl <- lapply(availSheet, function(x) {"Applicant" %in% unlist(x)})
  isAppl <- files[which(unlist(isAppl))]
  # Read excel
  applicant <- tibble(updated = basename(dirname(isAppl)), filename = isAppl) %>% 
    mutate(file_contents = map(filename, ~read_xlsx_character(file.path(.), sheet = "Applicant", startRow = 2))) %>% 
    select(-filename) %>% 
    unnest() %>% 
    select(updated, ID, Sex, Age, `Residency.Status`, `Ethnic.Group`, `Ethnicity`, Iwi, Descr, NSN, Descr.1)
  # Change  column names
  colnames(applicant)[length(colnames(applicant))] <- "Descriptio"
  # Set NAs to EXTERNAL
  newCols <- setdiff(colnames(student), colnames(applicant))
  applicant[newCols] <- "EXTERNAL"
  
  ## No Affil sheet
  # Get excel workbooks that have "No Affil" sheet
  isAffil <- lapply(availSheet, function(x) {"No Affil" %in% unlist(x)})
  isAffil <- files[which(unlist(isAffil))]
  # Read excel
  affil <- tibble(updated = basename(dirname(isAffil)), filename = isAffil) %>% 
    mutate(file_contents = map(filename, ~read_xlsx_character(file.path(.), sheet = "No Affil", startRow = 2))) %>% 
    select(-filename) %>% 
    unnest() %>% 
    select(updated, ID, Sex, NSN, `Ethnic.grp.description`, `Citizenship.Passport`, `Descr`)
  # Change  column names
  colnames(affil)[5:7] <- c("Ethnicity", "Residency.Status", "Ethnic.Group")
  # Change values
  affil$Residency.Status[affil$Residency.Status!="NZL"] <- "International"
  # Consistent sex
  affil$Sex[affil$Sex == "F"] <- "Female"
  affil$Sex[affil$Sex == "M"] <- "Male"
  affil$Sex[affil$Sex == "D"] <- "Diverse"
  # Set NAs to EXTERNAL
  newCols <- setdiff(colnames(student), colnames(affil))
  affil[newCols] <- "STAFF"
  
  ## No Citizenship sheet
  # Get excel workbooks that have "No Affil" sheet
  isCitizenship <- lapply(availSheet, function(x) {"No citizenship" %in% unlist(x)})
  isCitizenship <- files[which(unlist(isCitizenship))]
  # Read excel
  citizenship <- tibble(updated = basename(dirname(isCitizenship)), filename = isCitizenship) %>% 
    mutate(file_contents = map(filename, ~read_xlsx_character(file.path(.), sheet = "No citizenship", startRow = 2))) %>% 
    select(-filename) %>% 
    unnest() %>% 
    select(-`Display.Name`, -`Birthdate`)
  # Set NAs to EXTERNAL
  newCols <- setdiff(colnames(student), colnames(citizenship))
  citizenship[newCols] <- "STAFF"
  # Consistent sex
  citizenship$Sex[citizenship$Sex == "F"] <- "Female"
  citizenship$Sex[citizenship$Sex == "M"] <- "Male"
  citizenship$Sex[citizenship$Sex == "D"] <- "Diverse"
  
  
  # Merge all sheets data
  all <- bind_rows(student, applicant)
  all <- bind_rows(all, affil)
  all <- bind_rows(all, citizenship)
  #all <- do.call("rbind", list(student, affil, applicant))
  
  return(all)
}

# Load CRM
load_crm <- function(data_dir) {
  
  # Gather file paths
  years <- list.files(data_dir, pattern = "\\d+")
  filesExcel <- dir(file.path(data_dir, years), pattern="Original.*xlsx", full.names = TRUE)
  filesCsv <- dir(file.path(data_dir, years), pattern="Original.*csv", full.names = TRUE)
  yearsExcel <- basename(dirname(filesExcel))
  yearsCsv <- basename(dirname(filesCsv))
  years <- unique(c(yearsExcel, yearsCsv))
  
  # Read data
  eventExcel <- filesExcel %>% 
    map(read_excel) %>% 
    reduce(rbind) %>% 
    select(`UoA ID`, `Tags`, ID) %>% 
    na.omit() # Ignore NAs
  
  eventCsv <- filesCsv %>% 
    map(read_csv) %>% 
    reduce(rbind) %>% 
    select(`UoA ID`, `Tags`, ID) %>% 
    na.omit() # Ignore NAs
  
  # Combine two data sets
  studentEvent <- eventCsv %>% 
    rbind(eventExcel) %>% 
    mutate(`UoA ID2` = replace(`UoA ID`, `UoA ID` == "EXTERNAL", paste0(`UoA ID`[`UoA ID` == "EXTERNAL"], `ID`[`UoA ID` == "EXTERNAL"]))) # Change EXTERNAL to EXTERNAL_randomID
  
  # Transform to participant programme
  partProg <- studentEvent$Tags %>% 
    strsplit(., ",") %>% 
    setNames(studentEvent$`UoA ID`) %>% 
    melt(value.name = "programme") %>% 
    filter(grepl("^\\d{4}", programme))
  
  # Change column names
  colnames(partProg) <- c("programme", "ID")
  
  # Filter to student
  studentEvent <- partProg %>% 
    filter(!grepl("EXTERNAL", `ID`)) %>%
    distinct() # Remove any duplicates
  
  # Merge with External
  partProg <- partProg %>% 
    filter(grepl("EXTERNAL", `ID`)) %>% 
    rbind(studentEvent)
  
  # Change column names
  colnames(partProg) <- c("programme", "ID")
  
  return(partProg)
}

# Load STUDIO
load_studio <- function(data_dir, backup_dir) {
  # Gather file paths
  years <- list.files(data_dir, pattern = "\\d+")
  # C&M
  filesCM <- dir(file.path(data_dir, years), pattern="C&M Space.*xlsx", full.names = TRUE)
  yearsCM <- basename(dirname(filesCM))
  # Innovation Hub
  filesIH <- dir(file.path(data_dir, years), pattern="Innovation Hub.*xlsx", full.names = TRUE)
  yearsIH <- basename(dirname(filesIH))
  years <- unique(c(yearsCM, yearsIH))
  
  # Read data
  studioCM <- filesCM %>% 
    map(read_excel) %>% 
    reduce(rbind) %>% 
    select(`Timestamp`, `Student ID`, `What are you using the space for today?`, `What equipment are you planning on using?`, `Any comments, queries or improvements?`) %>%
    mutate(date = as.Date(Timestamp)) %>% 
    mutate(programme = paste(format(date, "%Y"), "Create and Maker Space Studio Participant")) %>% 
    distinct()
  colnames(studioCM) <- c("timestamp", "ID", "purpose", "equipment", "comment", "date", "programme")
  studioCM <- studioCM %>% separate_rows(`purpose`, sep =", ")
  studioCM <- studioCM %>% separate_rows(`equipment`, sep =", ")
  eventCM <- studioCM %>% select(ID, programme) %>% distinct()
  
  studioIH <- filesIH %>% 
    map(read_excel) %>% 
    reduce(rbind) %>% 
    select(`Timestamp`, 
           `What is your University of Auckland ID number (i.e. the 7-9 digit number).                                          If you are not from the University, please write \"EXTERNAL\"`,
           `What are you here for today:`) %>% 
    mutate(date = as.Date(Timestamp)) %>% 
    mutate(programme = paste(format(date, "%Y"), "Innovation Hub Studio Participant")) %>% 
    distinct()
  colnames(studioIH) <- c("timestamp", "ID", "purpose", "date", "programme")
  eventIH <- studioIH %>% select(ID, programme) %>% distinct()
  
  # Rbind dataframes
  studio <- rbind.fill(studioCM, studioIH) %>% distinct() %>% mutate(year = format(date, "%Y")) %>% mutate(month = format(date, "%m/%Y"))
  studio_all <- rbind(eventCM, eventIH) %>% distinct() # This needs to be merged with partTag data frame
  
  # # Write studio to all_studio.csv
  # write_csv(studio, file.path(data_dir,"all_studio.csv"))
  # write_csv(studio, file.path(backup_dir, "all", paste0("all_studio-",Sys.time(),".csv")))
  
  return(list(studio_all, studio))
}

# Load TAG
load_tag <- function(data_dir, backup_dir, save = FALSE) {
  # Gather file paths
  file <-  dir(file.path(data_dir,"tags"), pattern="tags.*xlsx", full.names = TRUE)
  
  # Read data
  if (file_ext(basename(file)) == "csv") {
    selection <- read_csv(file)
  } else {
    selection <- read_excel(file)
  }
  
  # Clean
  selection <- selection %>%
    filter(`Dashboards Y / N` == "Y") %>%
    filter(grepl("^\\d{4}", `Tag from Capsule Download and in files`)) %>%
    mutate(year=substring(`Tag from Capsule Download and in files`,0,4), programme=substring(`Tag from Capsule Download and in files`,6)) %>%
    distinct()
  
  # Rename columns
  if (ncol(selection) == 14) {
    colnames(selection) <- c("tags", "final_tags", "dashboards", "overview", "programme", "velocity", "unleash", "createmaker", "curricula", "journey", "date", "comment", "year", "tag_programme")
  } else {
    # TODO: new
    colnames(selection) <- c("tags", "final_tags", "dashboards", "overview", "programme", "velocity", "unleash", "createmaker", "curricula", "co-curricula", "journey", "date", "comment", "year", "tag_programme")
  }
  
  # # Rename columns
  # colnames(selection) <- c("tags", "final_tags", "dashboards", "overview", "programme", "velocity", "unleash", "createmaker", "curricula", "journey", "date", "comment", "year", "tag_programme")
  
  # Convert to date
  selection$date <- as.Date(as.numeric(selection$date)+5, origin = "1904-01-01")
  
  if (save) {
    # process_write() reads this back to decide whether the dashboards need reloading
    attr(selection, "tags_csv_changed") <- update_csv_with_backups(
      selection,
      file.path(data_dir, "tags", "tags_selection.csv"),
      mirror_backup_path = file.path(backup_dir, "tags", "tags_selection.csv"),
      versioned_backup_dir = file.path(backup_dir, "tags"),
      versioned_backup_prefix = "tags_selection"
    )
  }
  
  return(selection)
}

# Filter programme in dashboard
filter_programme <- function(partProg, selection) {
  # Only rows that need to be in the dashboard
  selection <- selection %>% 
    filter(selection$dashboards == "Y")
  
  # Only participant and programmes that need to be in the dashboard
  selected_partProg <- partProg %>% 
    filter(programme %in% selection$tags)
  
  # Split into two sub-datasets to find which rows need to be replaced
  change_tags <- selection %>% 
    filter(tags != final_tags)
  constant_tags <- selected_partProg %>% 
    filter(!programme %in% change_tags$tags)
  need2change_tags <- selected_partProg %>% 
    filter(programme %in% change_tags$tags)
  
  # Replace the invalid tags
  new <- need2change_tags # create a copy of need2change
  new$programme <- lapply(need2change_tags$programme, function(x) change_tags$final_tags[match(x, change_tags$tags)])
  
  # Combine the two sub-datasets
  selected_partProg <- rbind(constant_tags,new)
  
  return(selected_partProg)
}

# Join tables
join_table <- function(selected_partProg, partInfo) {
  ## Outer join two tables
  df <- merge(x=selected_partProg, y=partInfo, by="ID", all.x = TRUE)
  
  ## Filter out non-students, overarching programmes, and add year
  df_stud <- df %>% 
    #filter(!is.na(`Acad.Prog`)) %>% 
    filter(grepl("^\\d{4}", programme)) %>% 
    mutate(year=stri_sub(`programme`,0,4), programme=stri_sub(`programme`,6)) %>%
    # NOTE: this step used to read `filter(updated == year || is.na(updated))`,
    # meaning "remove accumulative data exempt EXTERNAL data". `||` only ever looked at
    # the first element, so in practice every row was kept, and from R 4.3 it is an error.
    # Kept as a no-op deliberately to preserve the existing output; swap in
    # `filter(is.na(updated) | updated == year)` to actually apply the per-year filter.
    select(-`NSN`) %>%
    distinct() # Remove duplicates
  
  ## Change all NAs to EXTERNAL for EXTERNALs
  colsToChange <- colnames(df_stud)
  colsToChange <- colsToChange[!colsToChange%in%c("ID","updated","year", "programme")]
  external_rows <- grepl("EXTERNAL", df_stud$ID)
  if (any(external_rows)) {
    df_stud[external_rows, colsToChange] <- "EXTERNAL"
  }
  
  return(df_stud)
}

# Anonymise functions
simple_id <- function(data, cols_to_anon)
{
  to_anon <- subset(data, select = cols_to_anon)
  ids <- unname(apply(to_anon, 1, paste, collapse = ""))
  as.integer(factor(ids))
}
anonymise <- function(data, cols_to_anon, algo = "sha256")
{
  if(!require(digest)) stop("digest package is required")
  to_anon <- subset(data, select = cols_to_anon)
  unname(apply(to_anon, 1, digest, algo = algo))
}
generate_salt <- function(data, cols_to_anon, n_chars = 20)
{
  index <- simple_id(data, cols_to_anon)
  n_indicies <- length(unique(index))
  chars <- rawToChar(as.raw(32:126), multiple = TRUE)
  x <- replicate(n_indicies, paste(sample(chars, n_chars, replace = TRUE), collapse = ""))
  x[index]
}

# Load TRAINING
load_training <- function(data_dir) {
  # Gather file paths
  file <-  dir(file.path(data_dir,"training"), pattern="Members and Training.*xlsx", full.names = TRUE)
  
  # Gather available sheets
  availSheets <- excel_sheets(file)
  
  # Read in the data
  # Read every sheet as character: sheets share column names but not column types
  # (e.g. empty trailing columns read as numeric in one sheet, character in another),
  # which makes bind_rows fail with "Can't combine `..1$X..3` <double> and `..3$X..3` <character>."
  training <- map_df(availSheets, ~read_xlsx_character(file, sheet = ., startRow = 1)) %>% distinct() %>% select(`Date.Stamp...do.not.copy.into.here`, `ID`, `Tag`, `Tag.1`, `Tag.2`)
  colnames(training) <- c("date", "ID", "Tag", "Tag.1", "Tag.2")
  
  # Filter out NAs rows
  training <- training %>% filter(!date == "NA")
  training <- training %>% filter(!ID == "NA")
  training <- training %>% filter(!Tag == "NA")
  training <- training %>% filter(!Tag.1 == "NA")
  training$year <- substring(training$Tag,0,4)
  training <- training %>% filter(year!="")
  training <- training %>% mutate(training = paste(training$year, training$Tag.1)) %>% select(`ID`, `date`, `training`) %>% distinct()
  training$date <- as.Date(as.numeric(training$date)-2, origin = "1900-01-01")
  
  return(training)
}

