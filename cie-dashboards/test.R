library(readr)
library(readxl)
library(dplyr)

df = readr::read_csv("/homevol/cie/shiny-server/data/tags/tags_selection.csv")


colnames(df)

tail(df)

all_df = read_csv("/homevol/cie/shiny-server/data/all.csv")

all_df %>% filter(grepl("Kuru", programme))

length(unique(all_df$ID))

df <- filterData() %>% 
  distinct(ID,year,programme) %>% # Remove people who are conjoints
  filter(programme %in% input$baseProgramme)



zdf = read_excel('../data/2025/Original - 2025 2025-06-19 16:14:07.xlsx')
colnames(zdf)
zdf2 = read_excel('../data/2025/From Rachel - 2025 2025-06-19 12:45:53.xlsx')
zdf2 %>% select(Descr) %>% distinct() %>% View


to_anon <- subset(data, select = cols_to_anon)
ids <- unname(apply(to_anon, 1, paste, collapse = ""))
as.integer(factor(ids))


filtermap = list(
  "Gender" = "Sex",
  "Ethnic Group" = "Ethnic.Group",
  "Ethnicity" = "Ethnicity",
  "Iwi" = "Descr",
  "Faculty" = "Owner.of.Major.Spec.Module",
  "Department" = "Plan.Description",
  "Affiliation" = "Programme.Level",
  "Residency" = "Residency.Status",
  "Year" = "year"
)
# # A tibble: 20 × 1
# Owner.of.Major.Spec.Module   
# <chr>                        
#   1 NA                           
# 2 Business & Economics         
# 3 Arts                         
# 4 Law                          
# 5 Bioengineering Institute     
# 6 ALUMNI                       
# 7 Engineering                  
# 8 Science                      
# 9 Medical & Health Sciences    
# 10 Creative Arts & Industries   
# 11 EXTERNAL                     
# 12 STAFF                        
# 13 Education & Social Work      
# 14 The University of Auckland   
# 15 Centre for Cont Education    
# 16 Auck Bioengineering Institute
# 17 ~                            
#   18 New Start                    
# 19 Liggins Institute            
# 20 Theology  

 %>% 
      distinct(ID,year, `Owner.of.Major.Spec.Module`)





%>% 
  distinct(ID,year, `Owner.of.Major.Spec.Module`)




# # # local
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(reshape2)
library(tidyverse)
library(readxl)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(networkD3)

filtermap = list(
  "Gender" = "Sex",
  "Ethnic Group" = "Ethnic.Group",
  "Ethnicity" = "Ethnicity",
  "Iwi" = "Descr",
  "Faculty" = "Owner.of.Major.Spec.Module",
  "Department" = "Plan.Description",
  "Affiliation" = "Programme.Level",
  "Residency" = "Residency.Status",
  "Year" = "year"
)

# Functions
filter_data <- function(dashboard, data_df, selection_df) {
  # Filter programmes based on tab names
  colNum <- match(dashboard, colnames(selection_df))
  df1 <- selection_df %>% 
    filter(selection_df[,colNum] == "Y") %>% 
    select(tag_programme)
  df2 <- data_df %>% 
    filter(programme %in% df1$tag_programme)
  return(df2)
}

post_changes = function(df) {
  if(!(filtermap$Faculty %in% colnames(df))) {
    return(df)
  }
  
  # New Faculty names, 2025 onwards
  # 'Arts' and 'Education & Social Work' -> 'Arts and Education'
  # 'Engineering' and 'Creative Arts & Industries' -> 'Engineering and Design'
  df %>% mutate(
    !!sym(filtermap$Faculty) := case_when(
      (!!sym(filtermap$Year) >= 2025) & (!!sym(filtermap$Faculty) == "Arts") ~ "Arts and Education",
      (!!sym(filtermap$Year) >= 2025) & (!!sym(filtermap$Faculty) == "Education & Social Work") ~ "Arts and Education",
      (!!sym(filtermap$Year) >= 2025) & (!!sym(filtermap$Faculty) == "Engineering") ~ "Engineering and Design",
      (!!sym(filtermap$Year) >= 2025) & (!!sym(filtermap$Faculty) == "Creative Arts & Industries") ~ "Engineering and Design",
      TRUE ~ !!sym(filtermap$Faculty)
    )
  )
}


# Import data
allData <- read_csv("/homevol/cie/shiny-server/data/all.csv", col_types = cols(ID = col_character()))
selection <- read_csv("/homevol/cie/shiny-server/data/tags/tags_selection.csv")
selection$date <- as.Date(ifelse(is.na(selection$date), paste0(as.character(selection$year), "-01-01"), as.character(selection$date)))
overview_df <- filter_data("overview", allData, selection)
programme_df <- filter_data("programme", allData, selection)
velocity_df <- filter_data("velocity", allData, selection)
unleash_df <- filter_data("unleash", allData, selection)
createmaker_df <- filter_data("createmaker", allData, selection)
journey_df <- filter_data("journey", allData, selection)
all_training <- read_csv("/homevol/cie/shiny-server/data/all_training.csv", col_types = cols(ID = col_character())) %>% filter(!is.na(date)) %>% distinct()
all_studio <- read_csv("/homevol/cie/shiny-server/data/all_studio.csv", col_types = cols(ID = col_character(), year = col_character())) %>% filter(!is.na(timestamp)) %>% distinct()
colnames(all_training) <- c("ID", "date", "programme")


overview_df = post_changes(overview_df)

curricula_programmes = sort(unique(selection$tag_programme[selection$curricula == "Y"]))
if ("co-curricula" %in% colnames(selection)) {
  cocurricula_programmes = sort(unique(selection$tag_programme[selection$`co-curricula` == "Y"]))
  curricula_df = allData %>% mutate(
    programme = case_when(
      programme %in% curricula_programmes ~ "Curricula",
      programme %in% cocurricula_programmes ~ "Co-curricula"
    )
  ) %>% filter(!is.na(programme))
} else {
  curricula_df = programme_df %>% mutate(
    programme = ifelse(programme %in% curricula_programmes, "Curricula", "Co-curricula")
  )
}






overview_df %>%
  filter(year %in% 2025) %>% 
  distinct(ID,year, `Owner.of.Major.Spec.Module`) %>% group_by(year, `Owner.of.Major.Spec.Module`) %>% tally




allData %>% filter(year %in% 2025)  %>% 
  distinct(ID,year, `Owner.of.Major.Spec.Module`) %>% group_by(year, `Owner.of.Major.Spec.Module`) %>% tally


