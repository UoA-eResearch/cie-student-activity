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
library(DT)

# Import data
allData <- read_csv("../data/overview.csv")
selection <- read_csv("../data/tags_selection.csv")

ui <- dashboardPage(

   # Application title
   dashboardHeader(title = "CIE Dashboard"),

   # Menu
   dashboardSidebar(
     sidebarMenu(
        #style = "position: fixed; width: inherit",
       
        id = "tab",
        
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
          choices = sort(unique(selection$year), decreasing = TRUE),
          selected = "2017",
          multiple = FALSE 
          
        ),
        selectizeInput(
          "compareYears",
          "Comparing years",
          choices = sort(unique(selection$year), decreasing = TRUE),
          multiple = TRUE,
          options = list(placeholder="Select year..", plugins=list("remove_button"))
        )
     )
   ),

   # Content
   dashboardBody(
     tabItems(

       # Overview
       tabItem(
         tabName = "overview",
         h4("Overview"),
         
         # Info boxes
         fluidRow(
          infoBoxOutput("totalParticipant",width=3),
          infoBoxOutput("uniqueParticipant",width=3),
          infoBoxOutput("repeatParticipant",width=3),
          infoBoxOutput("onetimeParticipant",width=3)
         ),
         
         # Overview chart
         fluidRow(
           column(6,
                  tabItem(tabName="Overview plot", width=NULL,
                          plotlyOutput("totalPlot", height = "300px"))
           ),
           column(6,
                  tabItem(tabName="Overview plot", width=NULL,
                          plotlyOutput("uniquePlot", height = "300px"))
           )
         ),

         # Faculty split by programme
         h4("Programme split by faculty"),
         fluidRow(
           column(12,
                  plotlyOutput("programmeFaculty", height="800px")
           )     
         ),
         # Faculty split overall
         h4("Faculty split"),
         fluidRow(
            column(12,
                   plotlyOutput("facultyN", height = "600px")
            )       

         ),
         # Programme split overall
         h4("Programme split"),
         fluidRow(
           column(12,
                  plotlyOutput("programmeN", height = "600px")
           )
         ),
         # Table
         #h4("Data table"),
         fluidRow(
           column(12,
                  dataTableOutput("table")
            )
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

