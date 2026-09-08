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
# The data has to be able to reload itself. These processes are long-lived
# (app_idle_timeout 0), and while an upload bumps the mtime of the dashboard sources to
# make Shiny re-read them, Shiny only ever sources global.R once per process - touching
# the app files re-runs ui.R and server.R but not this file. So a plain load here would
# be read once and then serve the same data forever.
#
# Instead every entry point calls refresh_dashboard_data(), which re-reads the CSVs when
# their modification times change and is a cheap no-op the rest of the time.
#
# The bindings are promises (delayedAssign), so each loads on first use rather than up
# front: ui.R only needs allData/selection/allStudio to build its input choices, so
# anything a dashboard tab touches later stays off the path to first paint. Loading it
# all eagerly measurably delays the page appearing.
#
# read_csv() rather than a faster eager reader such as data.table::fread(): readr
# returns character columns as lazily materialised (ALTREP) vectors, so the columns the
# dashboards never look at are never fully realised. fread reads all.csv about 4x faster
# in isolation, but materialising all 24 columns leaves the app slower overall.

dashboard_sources <- c(
  all      = "../data/all.csv",
  tags     = "../data/tags/tags_selection.csv",
  studio   = "../data/all_studio.csv",
  training = "../data/all_training.csv"
)

# Holds the mtimes the current bindings were built from, plus a counter that anything
# memoising derived frames can watch to know its cache is stale.
dashboard_state <- new.env(parent = emptyenv())
dashboard_state$stamp <- NULL
dashboard_state$generation <- 0L

# Rebuild the data bindings if the CSVs have changed since they were last built.
# Returns TRUE when it reloaded. Cheap when nothing has changed: four file.mtime calls.
refresh_dashboard_data <- function(force = FALSE) {
  stamp <- file.mtime(dashboard_sources)

  if (!force && !is.null(dashboard_state$stamp) && identical(stamp, dashboard_state$stamp)) {
    return(invisible(FALSE))
  }

  dashboard_state$stamp <- stamp
  dashboard_state$generation <- dashboard_state$generation + 1L

  env <- globalenv()

  delayedAssign(
    "allData",
    read_csv(dashboard_sources[["all"]], col_types = cols(ID = col_character())),
    eval.env = env, assign.env = env
  )

  delayedAssign(
    "selection",
    {
      df <- read_csv(dashboard_sources[["tags"]], show_col_types = FALSE)
      df$date <- as.Date(ifelse(is.na(df$date), paste0(as.character(df$year), "-01-01"), as.character(df$date)))
      df
    },
    eval.env = env, assign.env = env
  )

  delayedAssign(
    "allStudio",
    read_csv(dashboard_sources[["studio"]], col_types = cols(ID = col_character(), year = col_character())),
    eval.env = env, assign.env = env
  )

  delayedAssign(
    "all_studio",
    allStudio %>% filter(!is.na(timestamp)) %>% distinct(),
    eval.env = env, assign.env = env
  )

  delayedAssign(
    "all_training",
    {
      df <- read_csv(dashboard_sources[["training"]], col_types = cols(ID = col_character())) %>% filter(!is.na(date)) %>% distinct()
      colnames(df) <- c("ID", "date", "programme")
      df
    },
    eval.env = env, assign.env = env
  )

  delayedAssign(
    "availProg",
    filter_data("programme", allData, selection),
    eval.env = env, assign.env = env
  )

  invisible(TRUE)
}

refresh_dashboard_data()
