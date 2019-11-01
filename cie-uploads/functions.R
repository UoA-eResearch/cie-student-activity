# Library
library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(xlsx)
library(reshape2)

## Functions
# Process all data files and write to "all.csv"
process_write <- function(data_dir, backup_dir) {
  style <- "notification"
  withProgress(message = "Processing new data", style = style, value = 0.1, {
    # Load CRM
    withProgress(message = "Loading CRM data", style=style, value =.5, {
      partProg <- load_crm(data_dir)
      incProgress(.4) # Increment the progress bar
    })
    incProgress(.1)
    
    # Load SSO
    withProgress(message = "Loading SSO data", style=style, value =.5, {
      partInfo <- load_sso(data_dir)
      incProgress(.4) # Increment the progress bar
    })
    incProgress(.2)
    
    # Load TAG
    withProgress(message = "Loading CRM data", style=style, value =.5, {
      selection <- load_tag(data_dir, backup_dir)
      incProgress(.4)
    })
    incProgress(.2)
    
    # Filtering
    withProgress(message = "Filtering data", style=style, value =.5, {
      partProg <- filter_programme(partProg,selection)
      incProgress(.4)
    })
    incProgress(.1)
    
    # Merging
    withProgress(message = "Merging data", style=style, value =.5, {
      all_df <- join_table(partProg, partInfo)
      incProgress(.4)
    })
    incProgress(.1)
    
    # Copy old all.csv to backup_dir
    withProgress(message = "Merging data", style=style, value =.5, {
      allName <- dir(data_dir, pattern = "all.*csv", full.names = TRUE)
      incProgress(.2)
      Sys.sleep(0.2)
      file.copy(allName, backup_dir, overwrite = TRUE)
      incProgress(.2)
      Sys.sleep(0.2)
    })
    incProgress(.1)
    
    # Export
    withProgress(message = "Merging data", style=style, value =.5, {
      write_csv(all_df, file.path(data_dir,"all.csv"))
      incProgress(.2)
      Sys.sleep(0.2)
      write_csv(all_df, file.path(backup_dir, "all", paste0("all-",Sys.time(),".csv")))
      incProgress(.2)
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
  #years <- basename(dirname(files))
  
  # Get column types
  c <- sapply(read_excel("../data/base/From Rachel - 2019 CIE Participants.xlsx", sheet = "Student", skip = 1), class)
  c["Birthdate"] <- "POSIXct"
  colNames <- colnames(read.xlsx2("../data/base/From Rachel - 2019 CIE Participants.xlsx", sheetName = "Student", startRow = 2))
  
  # Read and clean
  student <- tibble(updated = years[1], filename = files[1]) %>% 
    mutate(file_contents = map(filename, ~read.xlsx2(file.path(.), sheetName="Student", startRow = 2, colClasses = c))) %>% 
    select(-filename) %>% 
    unnest() %>% 
    group_by(ID) %>%
    filter(`Admit.Term`==max(as.numeric(as.character(`Admit.Term`)))) %>% # Only select most recent updates
    ungroup() %>%
    distinct() # Remove duplicates
  files <- files[-1]
  for (file in files) {
    studentMock <- tibble(updated = basename(dirname(file)), filename = file) %>% 
      mutate(file_contents = map(filename, ~read.xlsx2(file.path(.), sheetName="Student", startRow = 2, colClasses = c))) %>% 
      select(-filename) %>% 
      unnest() %>% 
      group_by(ID) %>%
      filter(`Admit.Term`==max(as.numeric(as.character(`Admit.Term`)))) %>% # Only select most recent updates
      ungroup() %>%
      distinct() 
    student <- student %>% 
      rbind(studentMock)
    student <- student[!duplicated(student[,colNames]),] # Remove duplicates
  }
  
  # student <- tibble(updated = years, filename = files) %>% 
  #   mutate(file_contents = map(filename, ~read.xlsx2(file.path(.), sheetName="Student", startRow = 2, colClasses = c))) %>% 
  #   select(-filename) %>% 
  #   unnest() %>% 
  #   distinct() # Remove duplicates
    

  return(student)
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
    select(`UoA ID`, `Tags`)

  eventCsv <- filesCsv %>% 
    map(read_csv) %>% 
    reduce(rbind) %>% 
    select(`UoA ID`, `Tags`)
  
  # Combine two data sets
  studentEvent <- eventCsv %>% 
    rbind(eventExcel) %>% 
    group_by(`UoA ID`) %>% 
    mutate(events=str_length(Tags)) %>% # Count number of events
    filter(events==max(events)) # Latest rows would have higher number of events
  
  # Transform to participant programme
  partProg <- studentEvent$Tags %>% 
    strsplit(., ",") %>% 
    setNames(studentEvent$`UoA ID`) %>% 
    melt(value.name = "programme")
  
  colnames(partProg) <- c("programme", "ID")
  
  return(partProg)
}

# Load TAG
load_tag <- function(data_dir, backup_dir) {
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
  colnames(selection) <- c("tags", "final_tags", "dashboards", "overview", "programme", "velocity", "unleash", "createmaker", "curricula", "journey", "date", "comment", "year", "tag_programme")
  
  # Back up current tags_selection.csv file
  checkDir <- dir(file.path(data_dir, "tags"), pattern = paste0("tags_selection", ".*csv"), full.names = TRUE)
  checkDir2 <- file.path(backup_dir, "tags")
  file.copy(checkDir, checkDir2, recursive = TRUE)
  
  # Export
  write_csv(selection, file.path(data_dir,"tags", "tags_selection.csv"))
  write_csv(selection, file.path(backup_dir, "tags", paste0("tags_selection-",Sys.time(),".csv"))) # Another copy for backup
  
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
    filter(!is.na(`Acad.Prog`)) %>% 
    filter(grepl("^\\d{4}", programme)) %>% 
    mutate(year=substring(`programme`,0,4), programme=substring(`programme`,6)) %>% 
    filter(updated == year) %>% 
    distinct()
  
  return(df_stud)
}