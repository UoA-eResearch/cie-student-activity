## global.R ##
## CIE Dashboards — shared setup
##
## Shiny sources this once per R process, before ui.R and server.R, and both of those
## can see what it defines. The data loads used to be duplicated across ui.R and
## server.R, which meant parsing the 150MB+ all.csv twice on every app start (Shiny
## sources ui.R and server.R into separate environments, so neither saw the other's
## copy) and holding two copies of it in memory.

# libraries
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
library(DT)
library(shinyWidgets)
library(networkD3)

source("../traceback_utils.R")

# Log a full stack trace for anything that fails, instead of a bare message
install_traceback_handler("cie-dashboards")

filter_data <- function(dashboard, data_df, selection_df) {
  col_num <- match(dashboard, colnames(selection_df))
  selected_programmes <- selection_df %>%
    filter(selection_df[, col_num] == "Y") %>%
    pull(tag_programme)

  data_df %>%
    filter(programme %in% selected_programmes)
}

# Import data
#
# delayedAssign, not a plain assignment: each of these loads on first use rather than
# up front. ui.R only needs allData/selection/allStudio to build its input choices, so
# anything a dashboard tab touches later (all_training) stays off the path to first
# paint. Loading it all eagerly here measurably delays the page appearing.
#
# read_csv() rather than a faster eager reader such as data.table::fread(): readr
# returns character columns as lazily materialised (ALTREP) vectors, so the columns the
# dashboards never look at are never fully realised. fread reads all.csv about 4x faster
# in isolation, but materialising all 24 columns leaves the app slower overall.
delayedAssign("allData", read_csv("../data/all.csv", col_types = cols(ID = col_character())))

delayedAssign("selection", {
  df <- read_csv("../data/tags/tags_selection.csv", show_col_types = FALSE)
  df$date <- as.Date(ifelse(is.na(df$date), paste0(as.character(df$year), "-01-01"), as.character(df$date)))
  df
})

delayedAssign("allStudio", read_csv("../data/all_studio.csv", col_types = cols(ID = col_character(), year = col_character())))
delayedAssign("all_studio", allStudio %>% filter(!is.na(timestamp)) %>% distinct())

delayedAssign("all_training", {
  df <- read_csv("../data/all_training.csv", col_types = cols(ID = col_character())) %>% filter(!is.na(date)) %>% distinct()
  colnames(df) <- c("ID", "date", "programme")
  df
})

delayedAssign("availProg", filter_data("programme", allData, selection))
