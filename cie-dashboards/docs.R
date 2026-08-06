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

base_folder = "../data"

# Import data
allData <- read_csv(file.path(base_folder, "all.csv"), col_types = cols(ID = col_character()))
selection <- read_csv(file.path(base_folder, "tags_selection.csv"))
selection$date <- as.Date(ifelse(is.na(selection$date), paste0(as.character(selection$year), "-01-01"), as.character(selection$date)))
overview_df <- filter_data("overview", allData, selection)
programme_df <- filter_data("programme", allData, selection)
velocity_df <- filter_data("velocity", allData, selection)
unleash_df <- filter_data("unleash", allData, selection)
createmaker_df <- filter_data("createmaker", allData, selection)
journey_df <- filter_data("journey", allData, selection)
all_training <- read_csv(file.path(base_folder, "all_training.csv"), col_types = cols(ID = col_character())) %>% filter(!is.na(date)) %>% distinct()
all_studio <- read_csv(file.path("/all_studio.csv"), col_types = cols(ID = col_character(), year = col_character())) %>% filter(!is.na(timestamp)) %>% distinct()
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

filterData <- function(tab){
  if (tab == "overview") {
    return(post_changes(overview_df))
  } else if (tab == "programme") {
    return(post_changes(programme_df))
  } else if (tab == "velocity") {
    return(post_changes(velocity_df))
  } else if (tab == "unleash") {
    return(post_changes(unleash_df))
  } else if (tab =="journey") {
    return(post_changes(journey_df))
  } else if (tab == "createmaker") {
    return(post_changes(createmaker_df))
  } else if (tab == "curricula") {
    return(post_changes(curricula_df))
  }
}

################################
#                              #
#         OVERVIEW TAB         #
#                              #
################################
overviewPlot_df = function() {
  filterData("overview") %>% 
    distinct(ID,year,programme) #%>% # Remove people who are conjoints
  
}
programmePlot_df <- function(base_year) {
  df <- filterData("overview") %>%
    filter(year %in% base_year) %>% 
    distinct(ID,year,programme) %>%  # Remove people who are conjoints
    group_by(`year`, `programme`) %>% 
    summarise(count=n())
  return(df)
}
facultyPlot_df <- function(base_year) {
  df <- filterData("overview") %>%
    filter(year %in% base_year) %>% 
    distinct(ID,year, `Owner.of.Major.Spec.Module`)
  return(df)
}
heatmap_df <- function(base_year){
  df <- filterData("overview") %>% 
    filter(year %in% c(base_year)) %>%
    distinct(ID, year, `Owner.of.Major.Spec.Module`, `programme`)
  return(df)
}

# 1. Total participants by year
pdf = overviewPlot_df() %>% 
  #filter(!programme %in% c("CIE Participant")) %>% 
  select(ID,year) %>%
  group_by(year) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=year,y=count, label=count)) +
  geom_line(size=1, colour="#9ecae1") +
  geom_text(aes(y=count+200), alpha=0.8) +
  ggtitle("Total participants by year") +
  theme_minimal() + guides(fill=FALSE, color=FALSE) + labs(y="", x = "")


# 2. Unique participants by year
pdf = overviewPlot_df() %>% 
  select(ID, year, programme) %>%
  distinct() %>% # Avoid conjoint students appear twice
  arrange(year) %>% 
  group_by(ID, year) %>%
  filter(row_number()==2) %>% # Repeat students
  ungroup() %>% 
  #distinct(ID, year) %>% 
  group_by(year) %>% 
  summarise(repeatParticipant=n())

p = overviewPlot_df() %>% 
  select(ID, year) %>%
  distinct() %>% # Avoid conjoint students appear twice
  group_by(year) %>% 
  summarise(uniqueCount=n()) %>% 
  merge(pdf, by="year") %>% 
  mutate(oneTimeParticpant = uniqueCount - repeatParticipant) %>%
  mutate(repeatCount = repeatParticipant) %>% 
  gather(key="type_count", value="count", repeatParticipant, oneTimeParticpant) %>% 
  ggplot(aes(x=factor(year),y=count, fill=type_count)) +
  geom_bar(stat = "identity" ) +
  geom_text(aes(label=uniqueCount, y=uniqueCount+50), size=3, alpha=0.5) +
  geom_text(aes(label=paste0(repeatCount, " (", round(repeatCount*100/uniqueCount,0),"%)"), y=repeatCount*0.5), size=3, alpha=0.5) +
  ggtitle("Unique participants by year") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_brewer()

# 3. Programme split overall
pdf = programmePlot_df(base_year = 2025)

p = pdf %>% ggplot(aes(x=reorder(`programme`, count), count, fill=factor(year))) +
  geom_bar(stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=count, color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), hjust=-0.1) +
  guides(color=FALSE) +
  coord_flip() +
  ggtitle("Programme split overall") +
  theme_minimal(base_size = 14) + 
  scale_fill_tableau() + scale_colour_tableau() +
  labs(x="", y="")
  
# 4. Faculty split overall
pdf = facultyPlot_df(base_year = 2025) %>% 
  group_by(`Owner.of.Major.Spec.Module`,year) %>% 
  summarise(count=n()) %>%
  group_by(year) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, count), count, fill=factor(year))) +
  geom_bar(stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
  #geom_text(aes(label=count, color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), hjust=-0.1) +
  geom_text(aes(label=paste0(count, " (", round(count*100/sum_count,1),"%)"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), hjust=-0.1) +
  guides(color=FALSE) +
  coord_flip() +
  ggtitle("Faculty split overall") +
  theme_minimal(base_size = 14) + 
  scale_fill_tableau() + scale_colour_tableau() +
  labs(x="", y="")

# 5. Programme split by faculty
BASE_YEAR = 2025
pdf = heatmap_df(base_year = BASE_YEAR) %>% 
  group_by(programme,`Owner.of.Major.Spec.Module`, year) %>% 
  # summarise(count = n()) %>%
  # complete(`programme` =unique(programme),`Owner.of.Major.Spec.Module` = unique(filterData()$`Owner.of.Major.Spec.Module`), year=unique(heatmap_df()$year)) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(
    programme = unique(heatmap_df(base_year = BASE_YEAR)$programme),
    `Owner.of.Major.Spec.Module` = unique(heatmap_df(base_year = BASE_YEAR)$`Owner.of.Major.Spec.Module`), 
    year = unique(heatmap_df(base_year = BASE_YEAR)$year),
    fill = list(count = 0)
  ) %>%
  distinct()

p = pdf %>% 
  ggplot(aes(`Owner.of.Major.Spec.Module`,`programme`)) +
  geom_raster(aes(fill=count)) +
  geom_text(aes(label=count, colour=count>100), size=2, alpha=0.4) +
  facet_wrap(year~.) +
  guides(color=FALSE, fill=FALSE) +
  scale_fill_gradient_tableau(na.value = "grey") +
  scale_color_manual(guide = FALSE, values = c("black", "white")) +
  coord_equal() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = -30, hjust=0),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill="grey97")
  ) +
  labs(x="", y="")


################################
#                              #
#        PROGRAMME TAB         #
#                              #
################################
overviewPlot_df <- function(base_programme) {
  filterData("programme") %>% 
    distinct(ID,year,programme) %>% # Remove people who are conjoints
    filter(programme %in% base_programme)
}
generalPlot_df <- function(base_year, base_programme, input_list) {
  df <- filterData("programme") %>% 
    filter(year %in% base_year) %>% 
    filter(programme %in% base_programme)
  for (label in names(filtermap)) {
    key = filtermap[[label]]
    if (length(input_list[[key]]) >= 1) {
      print(paste("Filtering", key, label, input_list[[key]]))
      df <- df %>%
        filter(df[[key]] %in% input_list[[key]])
    }
  }
  return(df)
}

# 1. Unique participants by year
pdf = overviewPlot_df(base_programme = c("CIE Participant")) %>% 
  select(ID,year, programme) %>%
  distinct() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Unique participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))

# 2. Repeat participants by year
pdf = overviewPlot_df(base_programme = c("CIE Participant")) %>% 
  select(ID,year, programme) %>%
  distinct() %>% # Avoid conjoint students appear twice
  arrange(year) %>% 
  group_by(ID, programme) %>%
  filter(row_number()>1) %>% # Returning students
  ungroup() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Repeat participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))


# 3. Faculty
KEY = "year"
INPUT_LIST = list(
  year = c(2025)
)

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "CIE Participant",
  input_list = INPUT_LIST
) %>% 
  select(ID, !!sym(KEY), programme, `Owner.of.Major.Spec.Module`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Owner.of.Major.Spec.Module`, !!sym(KEY), programme) %>% 
  summarise(count=n()) %>% 
  group_by(!!sym(KEY), programme) %>% 
  mutate(sum_count=sum(count))


pdf %>% 
  ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, -count), y=count, label=count, fill=factor(!!sym(KEY)), colour=factor(!!sym(KEY)))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(!!sym(KEY))), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Faculty") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 4. Department
INPUT_LIST = list(
  year = c(2025)
)
# dropdown above the plot
facultyDepartment = c("Science")

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "CIE Participant",
  input_list = INPUT_LIST
) %>%
  filter(`Owner.of.Major.Spec.Module` %in% facultyDepartment) %>% # Filter selected faculties
  select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
  group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
  summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
  group_by(`Plan.Description`, programme ,`Owner.of.Major.Spec.Module`) %>%
  mutate(ymin=min(count), ymax=max(count))

p = pdf %>% 
  ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_segment(aes(y=ymin, yend=ymax), color="grey") +
  geom_point(size=4, alpha=1) +
  geom_text(color="white", size=2) +
  coord_flip() +
  facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Department") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()


# 5. Affiliation
KEY = "year"
INPUT_LIST = list(
  year = c(2025)
)

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "CIE Participant",
  input_list = INPUT_LIST
) %>% 
  select(ID, !!sym(KEY), programme, `Programme.Level`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Programme.Level`, !!sym(KEY), programme) %>% 
  summarise(count=n()) %>% 
  group_by(!!sym(KEY), programme) %>% 
  mutate(sum_count=sum(count))

        
p = pdf %>%     
  ggplot(aes(x=reorder(`Programme.Level`, -count), y=count, label=count, fill=factor(!!sym(KEY)), colour=factor(!!sym(KEY)))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(!!sym(KEY))), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Affiliation") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 6. Degree
INPUT_LIST = list(
  year = c(2025)
)
KEY = "year"
# dropdown above the plot
affiliationDegree = c("Undergraduate")

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "CIE Participant",
  input_list = INPUT_LIST
) %>%
  filter(`Programme.Level` %in% affiliationDegree) %>% # Filter selected
  select(ID, !!sym(KEY), programme, `Descriptio`, `Programme.Level`) %>%
  group_by(!!sym(KEY), programme ,`Descriptio`, `Programme.Level`) %>%
  summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
  group_by(programme , `Descriptio`, `Programme.Level`) %>%
  mutate(ymin=min(count), ymax=max(count)) 


pdf %>% 
  ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(!!sym(KEY)), colour=factor(!!sym(KEY)))) +
  geom_segment(aes(y=ymin, yend=ymax), color="grey") +
  geom_point(size=4, alpha=1) +
  geom_text(color="white", size=2) +
  coord_flip() +
  facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Degree") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 7. Gender
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "CIE Participant",
  input_list = INPUT_LIST
)  %>% 
  select(ID, year, programme, `Sex`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Sex`, year, programme) %>% 
  summarise(count=n()) %>% 
  group_by(year, programme) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Sex`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Gender") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  guides(colour=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 8. Residency.Status
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "CIE Participant",
  input_list = INPUT_LIST
)  %>% 
  select(ID, year, programme, `Residency.Status`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Residency.Status`, year, programme) %>% 
  summarise(count=n()) %>% 
  group_by(year, programme) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Residency.Status`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Residency.Status") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 9. Ethnic group
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "CIE Participant",
  input_list = INPUT_LIST
) %>% 
  select(ID, year, programme, `Ethnicity`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Ethnicity`, year, programme) %>% 
  summarise(count=n()) %>% 
  group_by(year, programme) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Ethnicity`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Ethnic group") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()


# 10. Iwi
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "CIE Participant",
  input_list = INPUT_LIST
) %>% 
  select(ID, year, programme, `Descr`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Descr`, year, programme) %>%
  summarise(count=n()) %>% 
  filter(!`Descr` == "NA") %>% 
  group_by(year, programme) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Descr`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .15))) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Iwi") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()



################################
#                              #
#         VELOCITY TAB         #
#                              #
################################
overviewPlot_df <- function(base_programme) {
  filterData("velocity") %>% 
    distinct(ID,year,programme) %>% # Remove people who are conjoints
    filter(programme %in% base_programme)
}

# 1. Unique participants by year
pdf = overviewPlot_df(base_programme = "Velocity Participant") %>% 
  select(ID,year, programme) %>%
  distinct() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>% 
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Unique participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))


# 2. Repeat participants by year
pdf = overviewPlot_df(base_programme = "Velocity Participant") %>% 
  select(ID,year, programme) %>%
  distinct() %>% # Avoid conjoint students appear twice
  arrange(year) %>% 
  group_by(ID, programme) %>%
  filter(row_number()>1) %>% # Returning students
  ungroup() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Repeat participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))


# 3. Faculty
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Velocity Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Owner.of.Major.Spec.Module`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Owner.of.Major.Spec.Module`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  #coord_flip() +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Faculty") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 4. Department
# dropdown above the plot
velocityFacultyDepartment = c("Science")

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Velocity Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  filter(`Owner.of.Major.Spec.Module` %in% velocityFacultyDepartment) %>% # Filter selected faculties
  select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
  group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_segment(aes(y=0)) +
  geom_point(size=2, alpha=.9) +
  geom_text(hjust=0, nudge_y=2.5, size=3) +
  coord_flip() +
  facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Department") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 5. Affiliation
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Velocity Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Programme.Level`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Programme.Level`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Programme.Level`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  #coord_flip() +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Affiliation") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 6. Degree
# dropdown above plot
velocityAffiliationDegree = "Undergraduate"
  
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Velocity Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  filter(`Programme.Level` %in% velocityAffiliationDegree) %>% # Filter selected
  select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
  group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_segment(aes(y=0)) +
  geom_point(size=2, alpha=.9) +
  geom_text(hjust=0, nudge_y=2.5, size=3) +
  coord_flip() +
  facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Degree") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 7. Gender
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Velocity Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Sex`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Sex`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Sex`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Gender") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  guides(colour=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 8. Ethnic group
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Velocity Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Ethnicity`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Ethnicity`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Ethnicity`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Ethnic group") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 9. Residency.Status
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Velocity Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Residency.Status`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Residency.Status`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Residency.Status`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Residency.Status") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 10. Iwi
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Velocity Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Descr`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Descr`, year, programme) %>% 
  summarise(count=n()) %>% 
  filter(!`Descr` == "NA")

p = pdf %>%
  ggplot(aes(x=reorder(`Descr`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Iwi") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()


################################
#                              #
#      UNLEASH SPACE TAB       #
#                              #
################################
# 

overviewPlot_df <- function(base_programme) {
  filterData("unleash") %>% 
    distinct(ID,year,programme) %>% # Remove people who are conjoints
    filter(programme %in% base_programme)
}

# 1. Unique participants by year
pdf = overviewPlot_df(base_programme = "Technology Hub Participant") %>% 
  select(ID,year, programme) %>%
  distinct() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Unique participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))

# 2. Repear participants by year
pdf = overviewPlot_df(base_programme = "Technology Hub Participant") %>% 
  select(ID,year, programme) %>%
  distinct() %>% # Avoid conjoint students appear twice
  arrange(year) %>% 
  group_by(ID, programme) %>%
  filter(row_number()>1) %>% # Returning students
  ungroup() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Repeat participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))


# 3. Faculty
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Technology Hub Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Owner.of.Major.Spec.Module`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Owner.of.Major.Spec.Module`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  #coord_flip() +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Faculty") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()


# 4. Faculty
unleashFacultyDepartment = "Science"

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Technology Hub Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  filter(`Owner.of.Major.Spec.Module` %in% unleashFacultyDepartment) %>% # Filter selected faculties
  select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
  group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_segment(aes(y=0)) +
  geom_point(size=2, alpha=.9) +
  geom_text(hjust=0, nudge_y=2.5, size=3) +
  coord_flip() +
  facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Department") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()


# 5. Affiliation
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Technology Hub Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Programme.Level`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Programme.Level`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Programme.Level`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Affiliation") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()


# 6. Degree
unleashAffiliationDegree = "Undergraduate"

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Technology Hub Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  filter(`Programme.Level` %in% unleashAffiliationDegree) %>% # Filter selected
  select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
  group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_segment(aes(y=0)) +
  geom_point(size=2, alpha=.9) +
  geom_text(hjust=0, nudge_y=2.5, size=3) +
  coord_flip() +
  facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Degree") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()


# 7. Gender
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Technology Hub Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Sex`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Sex`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Sex`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Gender") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  guides(colour=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 8. Ethnic group
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Technology Hub Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Ethnicity`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Ethnicity`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Ethnicity`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Ethnic group") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()


# 9. Residency.Status
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Technology Hub Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Residency.Status`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Residency.Status`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Residency.Status`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Residency.Status") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 10. Iwi
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Technology Hub Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Descr`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Descr`, year, programme) %>% 
  summarise(count=n()) %>% 
  filter(!`Descr` == "NA")

p = pdf %>%
  ggplot(aes(x=reorder(`Descr`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Iwi") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()


################################
#                              #
#  CREATE AND MAKER SPACE TAB  #
#                              #
################################
overviewPlot_df <- function(base_programme) {
  filterData("createmaker") %>% 
    distinct(ID,year,programme) %>% # Remove people who are conjoints
    filter(programme %in% base_programme)
}

# 1. Unique participants by year
pdf = overviewPlot_df(base_programme = "Equipment Training Participant") %>% 
  select(ID,year, programme) %>%
  distinct() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Unique participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))

# 2. Repeat participants by year
pdf = overviewPlot_df(base_programme = "Equipment Training Participant") %>% 
  select(ID,year, programme) %>%
  distinct() %>% # Avoid conjoint students appear twice
  arrange(year) %>% 
  group_by(ID, programme) %>%
  filter(row_number()>1) %>% # Returning students
  ungroup() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Repeat participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))

# 3. Faculty
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Equipment Training Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Owner.of.Major.Spec.Module`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Owner.of.Major.Spec.Module`, year, programme) %>% 
  summarise(count=n())
 
p = pdf %>%
  ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Faculty") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()


# 4. Department
# dropdown above plot
createmakerFacultyDepartment = "Science"

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Equipment Training Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  filter(`Owner.of.Major.Spec.Module` %in% createmakerFacultyDepartment) %>% # Filter selected faculties
  select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
  group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_segment(aes(y=0)) +
  geom_point(size=2, alpha=.9) +
  geom_text(hjust=0, nudge_y=2.5, size=3) +
  coord_flip() +
  facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Department") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()


# 5. Affiliation
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Equipment Training Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Programme.Level`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Programme.Level`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Programme.Level`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Affiliation") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()


# 6. Degree
# dropdown above aplot
createmakerAffiliationDegree = "Undergraduate"

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Equipment Training Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  filter(`Programme.Level` %in% createmakerAffiliationDegree) %>% # Filter selected
  select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
  group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_segment(aes(y=0)) +
  geom_point(size=2, alpha=.9) +
  geom_text(hjust=0, nudge_y=2.5, size=3) +
  coord_flip() +
  facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Degree") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 7. Gender
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Equipment Training Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Sex`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Sex`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Sex`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Gender") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  guides(colour=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 8. Ethnic group
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Equipment Training Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Ethnicity`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Ethnicity`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Ethnicity`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Ethnic group") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 9. Residency.Status
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Equipment Training Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Residency.Status`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Residency.Status`, year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=reorder(`Residency.Status`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Residency.Status") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 10. Iwi
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = "Equipment Training Participant",
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Descr`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Descr`, year, programme) %>% 
  summarise(count=n()) %>% 
  filter(!`Descr` == "NA") 

p = pdf %>%
  ggplot(aes(x=reorder(`Descr`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Iwi") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()



################################
#                              #
#       JOURNEY MAP TAB        #
#                              #
################################


journey_map_df <- function(base_year, base_programme, base_source, base_destination) {
  # Filter non-students
  df <- filterData("journey") %>%
    filter(!`Owner.of.Major.Spec.Module` %in% c("ALUMNI","STAFF", "EXTERNAL")) %>% select(`ID`, `programme`, `year`)
  
  # Add year to programme and remove year
  df$programme <- paste(df$year, df$programme)
  
  # Filter year and programme
  df <- df %>%
    filter(year %in% base_year) %>%
    filter(programme %in% base_programme)
  
  # Filter out Journey Table data
  tags <- selection %>% filter(journey=="Y")
  #tags <- tags %>% filter(date !="Overarching Tag") %>% filter(date !="Unleash Space Master List") %>% filter(date !="") %>% filter(!is.na(date))  # Need to include these in then
  #tags <- tags %>% filter(!is.na(date))
  tags <- tags %>% select(`final_tags`, `date`)
  
  # Filter ID that went to the destination or source
  if (base_source != "") {
    selectedIDs <- df %>% filter(programme %in% base_source) %>% distinct(ID)
  } else {
    selectedIDs <- df %>% filter(programme %in% base_destination) %>% distinct(ID)
  }
  df <- df %>% filter(ID %in% selectedIDs$ID) %>% distinct()
  if (nrow(df) == 0) {
    stop("No matching rows for inputs")
  }
  
  # Add count
  df$count <- 1
  
  # Fill in empty cells
  df <- df %>% complete(programme=unique(programme), ID=unique(ID)) %>% distinct() # Fill in empty cells
  df <- merge(df, tags, by.x="programme", by.y="final_tags", all.x = TRUE) %>% distinct() # Add date
  
  # Add all_training dfs
  training_df <- all_training %>% filter(ID %in% selectedIDs$ID)  %>% mutate(count=1, year=format(date, "%Y")) #%>% select(training, ID, count, date)
  df <- rbind(df, training_df)
  
  # Add all_studio dfs
  studio_df <- all_studio %>% filter(ID %in% selectedIDs$ID)  %>% mutate(count=1) %>% select(programme, ID, count, date, year)
  df <- rbind(df, studio_df)
  
  # Filter year and programme
  df <- df %>%
    filter(year %in% base_year) %>%
    filter(programme %in% base_programme)
  
  df <- df %>% complete(programme=unique(programme), ID=unique(ID)) %>% distinct() # Fill in empty cells
  df[is.na(df$count),]["count"] <- 0 # Replace NAs with 0
  
  # Filter events after the destination date
  if (base_source != "") {
    filteredDate <- selection[selection$final_tags==base_source,]$date
    df <- df %>% filter(!date <filteredDate) %>% distinct()
  } else {
    filteredDate <- selection[selection$final_tags==base_destination,]$date
    df <- df %>% filter(!date >filteredDate) %>% distinct()
  }
  if (nrow(df) == 0) {
    stop("No matching rows for inputs")
  }
  
  #print(unique(df$programme))
  return(df)
}


journey_table_df <- function(base_year, base_programme, base_source, base_destination) {
  # Filter count == 1
  df <- journey_map_df(
    base_year = base_year, 
    base_programme = base_programme, 
    base_source = base_source, 
    base_destination = base_destination
  ) %>% filter(count==1)
  
  # Add total number of events per ID, add total number of IDs per total
  df_total_event <-df %>% group_by(ID) %>% summarise(total=n()) %>% distinct()
  df_total_event <- df_total_event %>% group_by(total) %>% mutate(num_students=n()) %>% ungroup()
  
  # Merge
  df <- merge(df, df_total_event, by = "ID")
  
  # Sorting events, remove destination
  if (base_source != "") {
    sortedProg <- df %>% distinct(programme,date) %>% filter(programme!=base_source) %>% arrange(date) %>% distinct(programme)
    sortedProg <- c("total", "num_students", base_source, sortedProg$programme)
  } else {
    sortedProg <- df %>% distinct(programme,date) %>% filter(programme!=base_destination) %>% arrange(date) %>% distinct(programme)
    sortedProg <- c("total", "num_students", sortedProg$programme, base_destination)
  }
  
  # Spread
  df <- df %>% spread(key=programme, value = count)
  
  if (nrow(df) == 0) {
    stop("No matching rows for inputs")
  }
  
  # Replace NAs with 0s
  df[is.na(df)] = 0
  
  # Sorted the column
  df <-  df[,unlist(sortedProg)]
  df <- aggregate(.~num_students+total, df,FUN=sum)
  
  return(df)
}


journey_sankey_df <- function(base_year, base_programme, base_source, base_destination) {
  # Filter out Journey Table data
  tags <- selection %>% filter(journey=="Y")
  #tags <- tags %>% filter(!is.na(date))
  tags <- tags %>% select(`final_tags`, `date`)
  
  # Add number of events per ID
  df <- journey_map_df(
    base_year = base_year,
    base_programme = base_programme,
    base_source = base_source,
    base_destination = base_destination
  ) %>% filter(count==1)
  df <- df %>% group_by(ID) %>% mutate(count_event=n()) %>% ungroup()
  
  # Split datasets into single event goers and multiple event goers
  df_single <- df %>% filter(count_event==1) %>% group_by(ID) %>% arrange(date, .by_group=TRUE) %>% ungroup()
  df_not_single <- df %>% filter(count_event!=1) %>% group_by(ID) %>% arrange(date, .by_group=TRUE) %>% ungroup()
  
  # Add lags to both datasets
  df_single_lag <- df_single %>% group_by(ID) %>% mutate(source.programme=lead(programme, 1, default = NA)) %>% arrange(date, .by_group=TRUE) %>% ungroup()
  df_not_single_lag <- df_not_single %>% group_by(ID) %>% mutate(target.programme=lead(programme, 1, default = NA)) %>% arrange(date, .by_group=TRUE) %>% filter(!is.na(target.programme)) %>% ungroup()
  
  # Change column names
  df_not_single_lag <- df_not_single_lag %>% select(programme, target.programme, ID, date)
  colnames(df_not_single_lag) <- c("source.programme", "target.programme", "ID", "date")
  
  df_single_lag <- df_single_lag %>% select(programme, source.programme, ID, date)
  if (input$baseSource == "") {
    colnames(df_single_lag) <- c("target.programme", "source.programme", "ID", "date")
  } else {
    colnames(df_single_lag) <- c("source.programme", "target.programme", "ID", "date")
  }
  df_lag <- rbind(df_single_lag, df_not_single_lag)
  
  # Sum counts grouped by target and source
  df <- df_lag %>% group_by(`target.programme`,`source.programme`) %>% summarise(count=n())
  
  # Add dates to target + source
  df <- merge(df, tags, by.x="source.programme", by.y="final_tags", all.x = TRUE) %>% distinct() # Add date
  df <- df %>% mutate(date.source.programme = paste(date, source.programme)) %>% select(-date)
  df <- merge(df, tags, by.x="target.programme", by.y="final_tags", all.x = TRUE) %>% distinct() # Add date
  df <- df %>% mutate(date.target.programme = paste(date, target.programme)) %>% select(-date)
  df <- df %>% arrange(-count) %>% head(100)
  
  return(df)
}

# 1. mum_students
pdf = journey_table_df(
  base_year = 2023,
  base_programme = c("2023 100G Participant", "2023 CIE Participant", "2023 Co-curricula Participant"),
  base_source = "",
  base_destination = "2023 100G Participant"
) %>% 
  select(total, num_students)

p = pdf %>%
  ggplot(aes(factor(total),num_students)) + 
  geom_bar(stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=num_students), position = position_dodge2(width = 0.9, preserve = "single"), vjust=0) +
  theme_minimal()

# 2. Total split
journeyGroup = 2

{
  # Facet bar charts between totals
  df <- journey_table_df(
    base_year = 2023,
    base_programme = c("2023 100G Participant", "2023 CIE Participant", "2023 Co-curricula Participant"),
    base_source = "",
    base_destination = "2023 100G Participant"
  ) %>% select(-num_students) %>% gather(programme, count, -total) %>% filter(count>0)
  df <- df %>% filter(total %in% journeyGroup)
  
  # Filter out Journey Table data
  tags <- selection %>% filter(journey=="Y")
  #tags <- tags %>% filter(date !="Overarching Tag") %>% filter(date !="Unleash Space Master List") %>% filter(date !="") %>% filter(!is.na(date)) # Need to include these in then
  #tags <- tags %>% filter(!is.na(date))
  tags <- tags %>% select(`final_tags`, `date`)
  
  # Add date
  df <- merge(df, tags, by.x="programme", by.y="final_tags", all.x = TRUE) %>% distinct() 
  
  df %>% 
    ggplot(aes(reorder(programme,desc(date)), count, label=count)) +
    facet_wrap(total~., ncol=2, scale="free_y") +
    geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
    geom_text(hjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
    coord_flip() +
    ggtitle("Total split") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.background = element_rect(fill="grey97", colour = "white")) + 
    guides(colour=FALSE) + labs(y="", x = "") +
    #theme(axis.text.x = element_text(angle = -20, vjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
    scale_fill_tableau() + scale_colour_tableau()
}

# 3. table
journeyGroup = 2

journey_table_df(
  base_year = 2023,
  base_programme = c("2023 100G Participant", "2023 CIE Participant", "2023 Co-curricula Participant"),
  base_source = "",
  base_destination = "2023 100G Participant"
) %>% select(-num_students)

# 4. event heat map
{
  df <- journey_table_df(
    base_year = 2023,
    base_programme = c("2023 100G Participant", "2023 CIE Participant", "2023 Co-curricula Participant"),
    base_source = "",
    base_destination = "2023 100G Participant"
  ) %>% select(-num_students) %>% gather(programme, count, -total) %>% filter(count>0)
  df <- df %>% filter(total %in% journeyGroup)
  
  # Filter out Journey Table data
  tags <- selection %>% filter(journey=="Y")
  #tags <- tags %>% filter(date !="Overarching Tag") %>% filter(date !="Unleash Space Master List") %>% filter(date !="") %>% filter(!is.na(date)) # Need to include these in then
  #tags <- tags %>% filter(!is.na(date))
  tags <- tags %>% select(`final_tags`, `date`)
  
  # Add date
  df <- merge(df, tags, by.x="programme", by.y="final_tags", all.x = TRUE) %>% distinct() 
  
  df %>% 
    ggplot(aes(reorder(programme,desc(date)), count, label=count)) +
    facet_wrap(total~., ncol=2, scale="free_y") +
    geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
    geom_text(hjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
    coord_flip() +
    ggtitle("Total split") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.background = element_rect(fill="grey97", colour = "white")) + 
    guides(colour=FALSE) + labs(y="", x = "") +
    #theme(axis.text.x = element_text(angle = -20, vjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
    scale_fill_tableau() + scale_colour_tableau()
}

# 4. journeyEventHeatmap
base_year = 2023
base_programme = c("2023 100G Participant", "2023 CIE Participant", "2023 Co-curricula Participant")
base_source = ""
base_destination = "2023 100G Participant"
{
  df <- journey_sankey_df(
    base_year = base_year,
    base_programme = base_programme,
    base_source = base_source,
    base_destination = base_destination
  )
  # Add Node names
  nodes <- data.frame(name=c(as.character(df$source.programme), as.character(df$target.programme)) %>% unique())
  df$ID1 <- match(df$source.programme, nodes$name) - 1
  df$ID2 <- match(df$target.programme, nodes$name) - 1
  
  sankeyNetwork(Links = df, Nodes=nodes, Source = "ID1", "ID2", "count", NodeID = "name", nodePadding = 30, fontSize = 10)
}

# 5. journeyIndividualHeatmap
base_year = 2023
base_programme = c("2023 100G Participant", "2023 CIE Participant", "2023 Co-curricula Participant")
base_source = ""
base_destination = "2023 100G Participant"
{
  if (base_source != "") {
    df <- journey_map_df(
      base_year = base_year,
      base_programme = base_programme,
      base_source = base_source,
      base_destination = base_destination
    ) %>% mutate(programme=paste(date,programme))
  } else {
    df <- journey_map_df(
      base_year = base_year,
      base_programme = base_programme,
      base_source = base_source,
      base_destination =base_destination
    ) %>% mutate(programme=if_else(programme!=base_destination,paste(date,programme), paste("Destination: ", programme))) 
  }
  
  df <- df %>% complete(programme=unique(programme), ID=unique(ID)) %>% distinct()
  
  if (dim(df[is.na(df$count),])[1] != 0) {
    df[is.na(df$count),]["count"] <- 0 #Replace NAs with 0
  }
  
  df %>%  
    ggplot(aes(ID, fct_rev(programme))) + geom_tile(aes(fill=count)) +
    guides(color=FALSE, fill=FALSE) +
    scale_color_manual(guide = FALSE, values = c("black", "white")) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      #panel.grid.major = element_rect(fill="grey97"),
      panel.background = element_rect(fill="grey99")
    ) +
    labs(x="", y="")
}


###################################
#                                 #
# CURRICULA VS CO-CURRICULAR TAB  #
#                                 #
###################################
overviewPlot_df <- function(base_programme) {
  filterData("curricula") %>% 
    distinct(ID,year,programme) %>% # Remove people who are conjoints
    filter(programme %in% base_programme)
}
generalPlot_df <- function(base_year, base_programme, input_list) {
  df <- filterData("curricula") %>% 
    filter(year %in% base_year) %>% 
    filter(programme %in% base_programme)
  for (label in names(filtermap)) {
    key = filtermap[[label]]
    if (length(input_list[[key]]) >= 1) {
      print(paste("Filtering", key, label, input_list[[key]]))
      df <- df %>%
        filter(df[[key]] %in% input_list[[key]])
    }
  }
  return(df)
}

# 1. Unique participants by year
pdf = overviewPlot_df(base_programme = c("Co-curricula", "curricula")) %>% 
  select(ID,year, programme) %>%
  distinct() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Unique participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))

# 2. Repeat particupants by year
pdf = overviewPlot_df(base_programme = c("Co-curricula", "curricula")) %>% 
  select(ID,year, programme) %>%
  distinct() %>% # Avoid conjoint students appear twice
  arrange(year) %>% 
  group_by(ID, programme) %>%
  filter(row_number()>1) %>% # Returning students
  ungroup() %>% 
  group_by(year, programme) %>% 
  summarise(count=n())

p = pdf %>%
  ggplot(aes(x=factor(year),y=count, label=count)) +
  facet_wrap(programme~.) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  ggtitle("Repeat participants by year") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(panel.background = element_rect(fill="grey99", colour="grey99"))

# 3. Faculty
KEY = "year"

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = c("Co-curricula", "Curricula"),
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, !!sym(KEY), programme, `Owner.of.Major.Spec.Module`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Owner.of.Major.Spec.Module`, !!sym(KEY), programme) %>% 
  summarise(count=n()) %>% 
  group_by(!!sym(KEY), programme) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, -count), y=count, label=count, fill=factor(!!sym(KEY)), colour=factor(!!sym(KEY)))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(!!sym(KEY))), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Faculty") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 4. Department
# dropdown above plot
facultyDepartment = "Science"
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = c("Co-curricula", "Curricula"),
  input_list = list(
    year = c(2025)
  )
) %>% 
  filter(`Owner.of.Major.Spec.Module` %in% facultyDepartment) %>% # Filter selected faculties
  select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
  group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
  summarise(count=n()) 

p = pdf %>% 
  ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_segment(aes(y=0)) +
  geom_point(size=2, alpha=.9) +
  geom_text(hjust=0, nudge_y=2.5, size=3) +
  coord_flip() +
  facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Department") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 5. Affiliation
KEY = "year"

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = c("Co-curricula", "Curricula"),
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, !!sym(KEY), programme, `Programme.Level`) %>% 
  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
  group_by(`Programme.Level`, !!sym(KEY), programme) %>% 
  summarise(count=n()) %>% 
  group_by(!!sym(KEY), programme) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Programme.Level`, -count), y=count, label=count, fill=factor(!!sym(KEY)), colour=factor(!!sym(KEY)))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(!!sym(KEY))), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Affiliation") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 6. Degree
affiliationDegree = "Undergraduate"

pdf = generalPlot_df(
  base_year = 2025,
  base_programme = c("Co-curricula", "Curricula"),
  input_list = list(
    year = c(2025)
  )
) %>% 
  filter(`Programme.Level` %in% affiliationDegree) %>% # Filter selected
  select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
  group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
  summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
  group_by(programme , `Descriptio`, `Programme.Level`) %>%
  mutate(ymin=min(count), ymax=max(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_segment(aes(y=ymin, yend=ymax), color="grey") +
  geom_point(size=4, alpha=1) +
  geom_text(color="white", size=2) +
  coord_flip() +
  facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
  ggtitle("Degree") +
  theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 7. Gender
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = c("Co-curricula", "Curricula"),
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Sex`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Sex`, year, programme) %>% 
  summarise(count=n()) %>% 
  group_by(year, programme) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Sex`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Gender") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  guides(colour=FALSE) + labs(y="", x = "") +
  scale_fill_tableau() + scale_colour_tableau()

# 8. Residency.Status
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = c("Co-curricula", "Curricula"),
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Residency.Status`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Residency.Status`, year, programme) %>% 
  summarise(count=n()) %>% 
  group_by(year, programme) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Residency.Status`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Residency.Status") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 9. Ethnic group
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = c("Co-curricula", "Curricula"),
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Ethnicity`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Ethnicity`, year, programme) %>% 
  summarise(count=n()) %>% 
  group_by(year, programme) %>% 
  mutate(sum_count=sum(count))

p = pdf %>%
  ggplot(aes(x=reorder(`Ethnicity`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Ethnic group") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()

# 10. Iwi
pdf = generalPlot_df(
  base_year = 2025,
  base_programme = c("Co-curricula", "Curricula"),
  input_list = list(
    year = c(2025)
  )
) %>% 
  select(ID, year, programme, `Descr`) %>% 
  distinct() %>% # Avoid doublecounting conjoints
  group_by(`Descr`, year, programme) %>%
  summarise(count=n()) %>% 
  filter(!`Descr` == "NA") %>% 
  group_by(year, programme) %>% 
  mutate(sum_count=sum(count)) 

p = pdf %>%
  ggplot(aes(x=reorder(`Descr`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .15))) +
  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
  facet_wrap(programme~., ncol=3) +
  ggtitle("Iwi") +
  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
  scale_fill_tableau() + scale_colour_tableau()


