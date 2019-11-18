# Library
library(tidyverse)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(xlsx)
library(reshape2)
library(tools)
library(networkD3)

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

## Functions
# Process all data files and write to "all.csv"
process_write <- function(data_dir, backup_dir) {
  style <- "notification"
  withProgress(message = "Processing new data", style = style, value = 0.1, {
    # Load SSO
    withProgress(message = "Loading SSO data", style=style, value =.5, {
      partInfo <- load_sso(data_dir)
      incProgress(.4) # Increment the progress bar
    })
    incProgress(.2)
    
    # Load TAG
    withProgress(message = "Loading TAG data", style=style, value =.5, {
      selection <- load_tag(data_dir, backup_dir)
      incProgress(.4)
    })
    incProgress(.2)
    
    # Load CRM
    withProgress(message = "Loading CRM data", style=style, value =.5, {
      partProg <- load_crm(data_dir)
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
      incProgress(.2)
      all_df$ID <- simple_id(all_df, c("ID"))
      incProgress(.2)
    })
    incProgress(.1)
    
    # Copy old all.csv to backup_dir
    withProgress(message = "Backingup data", style=style, value =.5, {
      allName <- dir(data_dir, pattern = "all.*csv", full.names = TRUE)
      incProgress(.2)
      Sys.sleep(0.2)
      file.copy(allName, backup_dir, overwrite = TRUE)
      incProgress(.2)
      Sys.sleep(0.2)
    })
    incProgress(.1)
    
    # Export
    withProgress(message = "Uploading data", style=style, value =.5, {
      write_csv(all_df, file.path(data_dir,"all.csv"))
      incProgress(.2)
      Sys.sleep(0.2)
      write_csv(all_df, file.path(backup_dir, "all", paste0("all-",Sys.time(),".csv")))
      incProgress(.1)
      Sys.sleep(0.2)
      # Remove cache on the server
      system("touch ../cie-dashboards/*.R")
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
  c["Birthdate"] <- "POSIXct"
  # Read and clean sheet
  student <- tibble(updated = basename(dirname(files[1])), filename = files[1]) %>% 
    mutate(file_contents = map(filename, ~read.xlsx2(file.path(.), sheetName="Student", startRow = 2, colClasses = c, stringsAsFactors = FALSE))) %>% 
    select(-filename) %>% 
    unnest() %>% 
    group_by(ID) %>%
    filter(as.numeric(`Admit.Term`)==max(as.numeric(as.character(`Admit.Term`)))) %>% # Only select most recent updates
    ungroup() %>%
    distinct() # Remove duplicates
  student$Owner.of.Major.Spec.Module <- facultyRename$newFaculty[match(student$Owner.of.Major.Spec.Module, facultyRename$oldFaculty)]
  isStudent <- files[-1]
  colNames <- colnames(student)
  colNames  <- colNames[-4]
  for (file in isStudent) {
    studentMock <- tibble(updated = basename(dirname(file)), filename = file) %>% 
      mutate(file_contents = map(filename, ~read.xlsx2(file.path(.), sheetName="Student", startRow = 2, colClasses = c, stringsAsFactors = FALSE))) %>% 
      select(-filename) %>% 
      unnest() %>% 
      group_by(ID) %>%
      filter(as.numeric(`Admit.Term`)==max(as.numeric(as.character(`Admit.Term`)))) %>% # Only select most recent updates
      ungroup() %>%
      distinct() 
    student <- student %>% 
      rbind(studentMock)
    student <- student[!duplicated(student[,colNames]),] # Remove duplicates
    student$Residency.Status[!student$Residency.Status %in% c("International", "New Zealand Citizen", "NZ Permanent Resident")] <- "International" # Change everything should be International to International
  }
  # Remove Birthdate columns
  student <- student %>% 
    select(-`Birthdate`)
  # Change values for "Alumni"
  student[which(student$Status == "Completed Programme"),][c('Programme.Level', 'Descriptio', 'Owner.of.Major.Spec.Module')] <- "ALUMNI"
  
  ## Applicant sheet
  # Get excel workbooks that have "Applicant" sheet
  isAppl <- lapply(availSheet, function(x) {"Applicant" %in% unlist(x)})
  isAppl <- files[which(unlist(isAppl))]
  # Read excel
  applicant <- tibble(updated = basename(dirname(isAppl)), filename = isAppl) %>% 
    mutate(file_contents = map(filename, ~read.xlsx2(file.path(.), sheetName="Applicant", startRow = 2, stringsAsFactors = FALSE))) %>% 
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
    mutate(file_contents = map(filename, ~read.xlsx2(file.path(.), sheetName="No Affil", startRow = 2,  stringsAsFactors = FALSE))) %>% 
    select(-filename) %>% 
    unnest() %>% 
    select(updated, ID, Sex, NSN, `Ethnic.grp.description`, `Citizenship.Passport`, `Descr`)
  # Change  column names
  colnames(affil)[5:7] <- c("Ethnicity", "Residency.Status", "Ethnic.Group")
  # Change values
  affil$Residency.Status[affil$Residency.Status!="NZL"] <- "International"
  affil$Sex[affil$Sex == "F"] <- "Female"
  affil$Sex[affil$Sex == "M"] <- "Male"
  # Set NAs to EXTERNAL
  newCols <- setdiff(colnames(student), colnames(affil))
  affil[newCols] <- "STAFF"
  
  ## No Citizenship sheet
  # Get excel workbooks that have "No Affil" sheet
  isCitizenship <- lapply(availSheet, function(x) {"No citizenship" %in% unlist(x)})
  isCitizenship <- files[which(unlist(isCitizenship))]
  # Read excel
  citizenship <- tibble(updated = basename(dirname(isCitizenship)), filename = isCitizenship) %>% 
    mutate(file_contents = map(filename, ~read.xlsx2(file.path(.), sheetName="No citizenship", startRow = 2,  stringsAsFactors = FALSE))) %>% 
    select(-filename) %>% 
    unnest() %>% 
    select(-`Display.Name`, -`Birthdate`)
  # Set NAs to EXTERNAL
  newCols <- setdiff(colnames(student), colnames(citizenship))
  citizenship[newCols] <- "STAFF"
  
  
  # Merge all sheets data
  all <- rbind(student, applicant)
  all <- rbind(all, affil)
  all <- rbind(all, citizenship)
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
    mutate(`UoA ID` = replace(`UoA ID`, `UoA ID` == "EXTERNAL", paste0(`UoA ID`, `ID`))) # Change EXTERNAL to EXTERNAL_randomID
  
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
    #filter(!is.na(`Acad.Prog`)) %>% 
    filter(grepl("^\\d{4}", programme)) %>% 
    mutate(year=substring(`programme`,0,4), programme=substring(`programme`,6)) %>% 
    filter(updated == year || is.na(updated)) %>% # Remove accumalive data exempt EXTERNAL data
    select(-`NSN`) %>% 
    distinct() # Remove duplicates
  
  ## Change all NAs to EXTERNAL for EXTERNALs
  colsToChange <- colnames(df_stud)
  colsToChange <- colsToChange[!colsToChange%in%c("ID","updated","year", "programme")]
  df_stud[grepl("EXTERNAL",df_stud$ID),][colsToChange] <- "EXTERNAL"
  
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

# Sankey Diagram
sankey_dataframe <- function(all,tags_selection) {
  # Cleans all data
  all <- all %>% select(`ID`, `programme`, `year`)
  all$programme <- paste(all$year,all$programme)
  all <- all[all$programme %in% tags$final_tags,]
  
  # Filter out Journey Table data
  tags <- tags %>% filter(journey=="Y")
  tags <- tags %>% filter(date !="Overarching Tag")
  
  # Do pairwise count
  pc <- pairwise_count(all, programme, ID)
  pc$year1 <- substring(pc$item1,0,4)
  pc$year2 <- substring(pc$item2,0,4)
  pc <- pc %>% filter(year1 < year2) # Have to change this to date
  
  # Node names
  nodes <- data.frame(name=c(as.character(pc$item1), as.character(pc$item2)) %>% unique())
  pc$ID1 <- match(pc$item1, nodes$name) - 1
  pc$ID2 <- match(pc$item2, nodes$name) - 1
  
  return(pc)
  # p <- sankeyNetwork(Links = pc, Nodes=nodes, Source = "ID1", "ID2", "n", NodeID = "name", NodeGroup = "year", LinkGroup = "role", colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"))
}