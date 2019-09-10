## ui.R ##
## CIE Dashboards ##

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

# Import data
overviewData <- read_csv("../data/overview.csv")


ui <- dashboardPage(

   # Application title
   dashboardHeader(title = "CIE Dashboard"),

   # Menu
   dashboardSidebar(
     sidebarMenu(

        # Dashboard menu
        menuItem("Overview", tabName = "overview", icon = icon("fas fa-square")),
        menuItem("Programme", tabName = "programme", icon = icon("fas fa-square")),
        menuItem("Velocity", tabName = "velocity", icon = icon("fas fa-square")),
        menuItem("Unleash Space", tabName = "unleash", icon = icon("fas fa-square")),
        menuItem("Create and Maker Space", tabName = "createmaker", icon = icon("fas fa-square")),

        # Sidebar Inputs
        selectInput(
          "baseYear",
          "Base year",
          choices = unique(overviewData$year),
          selected = "2017",
          multiple = FALSE 
          
        ),
        selectInput(
          "compareYears",
          "Comparing years",
          choices = unique(overviewData$year),
          multiple = TRUE,
          selectize = TRUE
        )
     )
   ),

   # Content
   dashboardBody(
     tabItems(

       # Overview
       tabItem(
         tabName = "overview",
         h2("Overview"),

         # Info boxes
         fluidRow(
           infoBoxOutput("totalParticipant"),
           infoBoxOutput("uniqueParticipant"),
           infoBoxOutput("repeatParticipant"),
           infoBoxOutput("onetimeParticipant")
         ),

         # Faculty split overall
         fluidRow(

         ),
         # Programme split overall
         fluidRow(

         ),
         # Faculty split by programme
         fluidRow(

         )
       ),

       # Programme
       tabItem(
         tabName = "programme",
         h2("Programme")

         #
       ),

       # Velocity
       tabItem(
         tabName = "velocity",
         h2("Velocity")
       ),

       # Unleash Space
       tabItem(
         tabName = "unleash",
         h2("Unleash Space")
       ),

       # Create and Maker Space
       tabItem(
         tabName = "createmaker",
         h2("Create and Maker Space")
       )
     )
   )
)

