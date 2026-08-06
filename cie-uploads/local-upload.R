# Settings
options(java.parameters = "-Xmx2G")

setwd("/homevol/cie/cie-student-activity/cie-uploads/")
source("./functions.R")

# Library
library(tidyverse)
library(readxl)
library(plyr)
library(dplyr)
library(tidyr)
library(widyr)
library(xlsx)
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

saveName = function(saveType, uploadFile, uploadYear) {
  uploadPath <- basename(uploadFile)
  name <- paste0(input$saveType, input$saveYear, " ", Sys.time())
  
}

# --- upload tag (xlsx) ---
saveType = "tags-selection"
uploadFile = uploadPath = "~/Downloads/tags-selection.xlsx"
uploadYear = format(Sys.Date(), "%Y")


# file check
sheetCondition <- "Tags" %in% excel_sheets(uploadPath)
validate(
  need(sheetCondition==TRUE, message="TAG file needs sheet named 'Tags'")
)
df <- read_excel(uploadPath, sheet = "Tags")
check_colnames = unique(colnames(read_excel("../data/base/tags-selection2019 2019-12-05 23:56:11.xlsx")))
check_colnames[check_colnames == "Curricula"] = "Curricular"
check_colnames = c(check_colnames, "Co-Curricular")
# if ("Co-Curricular" %in% colnames(df)) {
#   # newly added Co-Curricular column
#   check_colnames = c(check_colnames, "Co-Curricular")
#   # renamed Curricula -> Curricular, Curricular -> Co-Curricular
#   check_colnames[check_colnames == "Curricula"] = "Curricular"
# }
columnCondition <- all(sort(check_colnames) == sort(colnames(df)))
validate(
  need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read_excel("../data/base/tags-selection2019 2019-12-05 23:56:11.xlsx")), colnames(df))))
)
# Change to dafa.frame
df <- as.data.frame(df)

# save it

# -------------------------------



# --- process_write ---
data_dir = "../data"
backup_dir = "../backup_data"

partInfo <- load_sso(data_dir)
selection <- load_tag(data_dir = data_dir, backup_dir = backup_dir, FALSE)
partProg <- load_crm(data_dir)
listStudio <- load_studio(data_dir, backup_dir)
partStudio <- listStudio[[1]]
studio <- listStudio[[2]]
partProg <- rbind(partProg, partStudio)
training <- load_training(data_dir)
partProg <- filter_programme(partProg,selection)

# Merging
all_df <- join_table(partProg, partInfo)
all_df <- rbind.fill(all_df, training)
all_df <- rbind.fill(all_df, studio)
all_df$ID <- simple_id(all_df, c("ID"))

# id_df = all_df
# id_df$ID2 <- simple_id(all_df, c("ID"))

# Split the datasets
all_studio <- all_df %>% filter(!is.na(`timestamp`)) %>% select(`ID`, `date`, `purpose`, `equipment`, `comment`, `programme`, `timestamp`, `month`, `year`)
all_training <- all_df %>% filter(is.na(`timestamp`)) %>% filter(!is.na(`date`)) %>% select(`ID`, `date`, `training`)
all_df <- all_df %>% filter(is.na(`date`)) %>% select(-`date`, -`training`, -`purpose`, -`timestamp`, -`equipment`, -`comment`)


# Copy old all.csv to backup_dir
allName <- dir(data_dir, pattern = "all.*csv", full.names = TRUE)
file.copy(allName, backup_dir, overwrite = TRUE)

# # Export
write_csv(all_df, file.path(data_dir,"all.csv"))
write_csv(all_training, file.path(data_dir,"all_training.csv"))
write_csv(all_studio, file.path(data_dir,"all_studio.csv"))
write_csv(all_df, file.path(backup_dir, "all", paste0("all-",Sys.time(),".csv")))
write_csv(all_training, file.path(backup_dir, "all", paste0("all_training-",Sys.time(),".csv")))
write_csv(all_studio, file.path(backup_dir, "all", paste0("all_studio-",Sys.time(),".csv")))
# Remove cache on the server
system("touch ../cie-dashboards/*.R")

# -------------------------------

Sys.getenv("JAVA_HOME")
library(xlsx)
read.xlsx2("~/Downloads/From Rachel - April.xlsx", sheetName="Student", startRow = 2)












