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
library(shinyWidgets)

# Import data
allData <- read_csv("data/all.csv")
selection <- read_csv("data/tags_selection.csv")
availProg <- selection %>% 
        filter(selection[,5] == "Y") %>% 
        select(tag_programme)
availProg <- allData %>% 
        filter(programme %in% availProg$tag_programme)

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
        pickerInput(
                "baseYear",
                "Year",
                choices = sort(unique(selection$year), decreasing = TRUE),
                selected = "2017",
                options = list(`actions-box` = TRUE, placeholder="Select year..."),
                multiple = T
        ),
        pickerInput(
                "baseProgramme",
                "Programme",
                selected="CIE Participant",
                choices = sort(unique(availProg$programme)),
                options = list(`actions-box` = TRUE, placeholder="Select programme..."),
                multiple = T
        )
     )
   ),

   # Content
   dashboardBody(
     tabItems(

       # Overview
       tabItem(
         tabName = "overview",
         #h4("Overview"),
         
         # # Info boxes
         # fluidRow(
         #  infoBoxOutput("totalParticipant",width=3),
         #  infoBoxOutput("uniqueParticipant",width=3),
         #  infoBoxOutput("repeatParticipant",width=3),
         #  infoBoxOutput("onetimeParticipant",width=3)
         # ),
         
         # Overview chart
         fluidRow(
           column(6,
                  tabItem(tabName="Overview plot", width=NULL,
                          plotOutput("totalPlot", height = "300px"))
           ),
           column(6,
                  tabItem(tabName="Overview plot", width=NULL,
                          plotlyOutput("uniquePlot", height = "300px"))
           )
         ),
         # Programme split overall
         h4("Programme split"),
         fluidRow(
                 column(12,
                        plotOutput("programmeN", height = "800px")
                 )
         ),
         # Faculty split overall
         h4("Faculty split"),
         fluidRow(
            column(12,
                   plotOutput("facultyN", height = "800px")
            )       

         ),
         # Faculty split by programme
         h4("Programme split by faculty"),
         fluidRow(
                 column(12,
                        plotlyOutput("programmeSplitFaculty", height="800px")
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
               
               # Overview chart
               fluidRow(
                       column(6,
                              tabItem(tabName="programme", width=NULL,
                                      plotOutput("programmeUniquePlot", height = "300px"))
                       ),
                       column(6,
                              tabItem(tabName="programme", width=NULL,
                                      plotOutput("programmeRepeatPlot", height = "300px"))
                       )
               ),
               
               # Faculty split
               h3(""),
               fluidRow(
                       
                       column(12,
                              tabItem(tabName="programme", width=NULL,
                                      plotOutput("programmeFacultyPlot", height = "600px"))
                       )
               ),
               
               # Department split
               # h3(""),
               # fluidRow(
               #         column(12,
               #                tabItem(tabName="programme", width=NULL,
               #                        plotOutput("programmeDepartmentPlot"), height="1000px")
               #         )
               # )
               
               # Gender and Ethinicity Plot
               h3(""),
               fluidRow(
                       column(6,
                              tabItem(tabName="programme", width=NULL,
                                      plotOutput("programmeGenderPlot", height = "400px"))
                       ),
                       column(6,
                              tabItem(tabName="programme", width=NULL,
                                      plotOutput("programmeEthinicityPlot", height = "400px"))
                       )
               )

               # Degree programme
               
               # Affiliation
               
               # Gender
               
               # Ethinicity
               
               # Iwi
               
               # Residency status
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

