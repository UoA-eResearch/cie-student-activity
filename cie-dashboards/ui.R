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
library(networkD3)

# Import data
allData <- read_csv("../data/all.csv", col_types = cols(ID = col_character()))
selection <- read_csv("../data/tags/tags_selection.csv")
allStudio <- read_csv("../data/all_studio.csv", col_types = cols(ID = col_character()))

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
        menuItem("Journey map", tabName="journey", icon = icon("fas fa-square")),

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
                multiple = T)
     )
   ),

   # Content
   dashboardBody(
     tabItems(

       # Overview
       tabItem(
         tabName = "overview",
         
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
                  # column(4,
                  #        plotlyOutput("facultyNPercentage", height = "800px"))

         ),
         # Faculty split by programme
         h4("Programme split by faculty"),
         fluidRow(
                 column(12,
                        plotlyOutput("programmeSplitFaculty", height="800px"))     
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
               pickerInput(
                 "programmeFacultyDepartment",
                 "Faculty",
                 selected="Engineering",
                 choices = sort(unique(availProg$`Owner.of.Major.Spec.Module`)),
                 options = list(`actions-box` = TRUE, placeholder="Select faculty..."),
                 multiple = T
               ),
               fluidRow(
                       column(12,
                              tabItem(tabName="programme", width=NULL,
                                      plotlyOutput("programmeDepartmentPlot", height="800px"))
                       )
               ),
               
               # Affiliation Split
               h3(""),
               fluidRow(
                 column(12,
                        tabItem(tabName="programme", width=NULL,
                                plotOutput("programmeAffiliationPlot", height = "600px"))
                 )
               ),
               # Degree split
               pickerInput(
                 "programmeAffiliationDegree",
                 "Affiliation",
                 selected="Undergraduate",
                 choices = sort(unique(availProg$`Programme.Level`)),
                 options = list(`actions-box` = TRUE, placeholder="Select affiliation..."),
                 multiple = T
               ),
               fluidRow(
                 column(12,
                        tabItem(tabName="programme", width=NULL,
                                plotlyOutput("programmeDegreePlot", height="1000px"))
                 )
               ),
               
               # Gender and Ethinicity Plot
               h3(""),
               fluidRow(
                       column(6,
                              tabItem(tabName="programme", width=NULL,
                                      plotOutput("programmeGenderPlot", height = "400px"))
                       ),
                       column(6,
                              tabItem(tabName="programme", width=NULL,
                                      plotOutput("programmeResidencyPlot", height = "400px"))
                       )
               ),
               
               # Iwi and Residency Plot
               h3(""),
               fluidRow(
                       column(12,
                              tabItem(tabName="programme", width=NULL,
                                      plotOutput("programmeEthinicityPlot", height = "400px"))
                       )),
               h3(""),
               fluidRow(
                       column(12,
                              tabItem(tabName="programme", width=NULL,
                                      plotOutput("programmeIwiPlot", height = "400px"))
                       )
               )
       ),

       # Velocity
       tabItem(
               tabName = "velocity",
               
               # Overview chart
               fluidRow(
                 column(6,
                        tabItem(tabName="velocity", width=NULL,
                                plotOutput("velocityUniquePlot", height = "300px"))
                 ),
                 column(6,
                        tabItem(tabName="velocity", width=NULL,
                                plotOutput("velocityRepeatPlot", height = "300px"))
                 )
               ),
               
               # Faculty split
               h3(""),
               fluidRow(
                 column(12,
                        tabItem(tabName="velocity", width=NULL,
                                plotOutput("velocityFacultyPlot", height = "600px"))
                 )
               ),
               
               # Department split
               pickerInput(
                 "velocityFacultyDepartment",
                 "Faculty",
                 selected="Engineering",
                 choices = sort(unique(availProg$`Owner.of.Major.Spec.Module`)),
                 options = list(`actions-box` = TRUE, placeholder="Select faculty..."),
                 multiple = T
               ),
               fluidRow(
                 column(12,
                        tabItem(tabName="velocity", width=NULL,
                                plotlyOutput("velocityDepartmentPlot", height="800px"))
                 )
               ),
               
               # Affiliation Split
               h3(""),
               fluidRow(
                 column(12,
                        tabItem(tabName="velocity", width=NULL,
                                plotOutput("velocityAffiliationPlot", height = "600px"))
                 )
               ),
               # Degree split
               pickerInput(
                 "velocityAffiliationDegree",
                 "Affiliation",
                 selected="Undergraduate",
                 choices = sort(unique(availProg$`Programme.Level`)),
                 options = list(`actions-box` = TRUE, placeholder="Select affiliation..."),
                 multiple = T
               ),
               fluidRow(
                 column(12,
                        tabItem(tabName="velocity", width=NULL,
                                plotlyOutput("velocityDegreePlot", height="1000px"))
                 )
               ),
               
               # Gender and Ethinicity Plot
               h3(""),
               fluidRow(
                 column(6,
                        tabItem(tabName="velocity", width=NULL,
                                plotOutput("velocityGenderPlot", height = "400px"))
                 ),
                 column(6,
                        tabItem(tabName="velocity", width=NULL,
                                plotOutput("velocityEthinicityPlot", height = "400px"))
                 )
               ),
               
               # Iwi and Residency Plot
               h3(""),
               fluidRow(
                 column(6,
                        tabItem(tabName="velocity", width=NULL,
                                plotOutput("velocityResidencyPlot", height = "400px"))
                 ),
                 column(6,
                        tabItem(tabName="velocity", width=NULL,
                                plotOutput("velocityIwiPlot", height = "400px"))
                 )
               )
       ),

       # Unleash Space
       tabItem(
               tabName = "unleash",
               
               # Overview chart
               fluidRow(
                 column(6,
                        tabItem(tabName="unleash", width=NULL,
                                plotOutput("unleashUniquePlot", height = "300px"))
                 ),
                 column(6,
                        tabItem(tabName="unleash", width=NULL,
                                plotOutput("unleashRepeatPlot", height = "300px"))
                 )
               ),
               
               # Faculty split
               h3(""),
               fluidRow(
                 column(12,
                        tabItem(tabName="unleash", width=NULL,
                                plotOutput("unleashFacultyPlot", height = "600px"))
                 )
               ),
               
               # Department split
               pickerInput(
                 "unleashFacultyDepartment",
                 "Faculty",
                 selected="Engineering",
                 choices = sort(unique(availProg$`Owner.of.Major.Spec.Module`)),
                 options = list(`actions-box` = TRUE, placeholder="Select faculty..."),
                 multiple = T
               ),
               fluidRow(
                 column(12,
                        tabItem(tabName="unleash", width=NULL,
                                plotlyOutput("unleashDepartmentPlot", height="800px"))
                 )
               ),
               
               # Affiliation Split
               h3(""),
               fluidRow(
                 column(12,
                        tabItem(tabName="unleash", width=NULL,
                                plotOutput("unleashAffiliationPlot", height = "600px"))
                 )
               ),
               # Degree split
               pickerInput(
                 "unleashAffiliationDegree",
                 "Affiliation",
                 selected="Undergraduate",
                 choices = sort(unique(availProg$`Programme.Level`)),
                 options = list(`actions-box` = TRUE, placeholder="Select affiliation..."),
                 multiple = T
               ),
               fluidRow(
                 column(12,
                        tabItem(tabName="unleash", width=NULL,
                                plotlyOutput("unleashDegreePlot", height="1000px"))
                 )
               ),
               
               # Gender and Ethinicity Plot
               h3(""),
               fluidRow(
                 column(6,
                        tabItem(tabName="unleash", width=NULL,
                                plotOutput("unleashGenderPlot", height = "400px"))
                 ),
                 column(6,
                        tabItem(tabName="unleash", width=NULL,
                                plotOutput("unleashEthinicityPlot", height = "400px"))
                 )
               ),
               
               # Iwi and Residency Plot
               h3(""),
               fluidRow(
                 column(6,
                        tabItem(tabName="unleash", width=NULL,
                                plotOutput("unleashResidencyPlot", height = "400px"))
                 ),
                 column(6,
                        tabItem(tabName="unleash", width=NULL,
                                plotOutput("unleashIwiPlot", height = "400px"))
                 )
               ),
               
               # Studio time series
               conditionalPanel(
                 condition = "input.baseProgramme == 'Innovation Hub Studio Participant'",
                 h3("Studio Time"),
                 fluidRow(
                   column(12,
                          tabItem(tabName="unleash", width=NULL,
                                  plotlyOutput("unleashStudioTimeseriesPlot", height = "400px"))
                   )),
                 pickerInput(
                   "unleashStudioMonth",
                   "Month",
                   selected = sort(unique(allStudio$month))[1:2],
                   choices = sort(unique(allStudio$month)),
                   options = list(`actions-box` = TRUE, placeholder="Select faculty..."),
                   multiple = T
                 ),
                 h3(""),
                 fluidRow(
                   column(12,
                          tabItem(tabName="unleash", width=NULL,
                                  plotOutput("unleashStudioPurposePlot", height = "800px"))
                   ))
                 
               )
       ),

       # Create and Maker Space
       tabItem(
               tabName = "createmaker",
               
               # Overview chart
               fluidRow(
                 column(6,
                        tabItem(tabName="createmaker", width=NULL,
                                plotOutput("createmakerUniquePlot", height = "300px"))
                 ),
                 column(6,
                        tabItem(tabName="createmaker", width=NULL,
                                plotOutput("createmakerRepeatPlot", height = "300px"))
                 )
               ),
               
               # Faculty split
               h3(""),
               fluidRow(
                 column(12,
                        tabItem(tabName="createmaker", width=NULL,
                                plotOutput("createmakerFacultyPlot", height = "600px"))
                 )
               ),
               
               # Department split
               pickerInput(
                 "createmakerFacultyDepartment",
                 "Faculty",
                 selected="Engineering",
                 choices = sort(unique(availProg$`Owner.of.Major.Spec.Module`)),
                 options = list(`actions-box` = TRUE, placeholder="Select faculty..."),
                 multiple = T
               ),
               fluidRow(
                 column(12,
                        tabItem(tabName="createmaker", width=NULL,
                                plotlyOutput("createmakerDepartmentPlot", height="800px"))
                 )
               ),
               
               # Affiliation Split
               h3(""),
               fluidRow(
                 column(12,
                        tabItem(tabName="createmaker", width=NULL,
                                plotOutput("createmakerAffiliationPlot", height = "600px"))
                 )
               ),
               # Degree split
               pickerInput(
                 "createmakerAffiliationDegree",
                 "Affiliation",
                 selected="Undergraduate",
                 choices = sort(unique(availProg$`Programme.Level`)),
                 options = list(`actions-box` = TRUE, placeholder="Select affiliation..."),
                 multiple = T
               ),
               fluidRow(
                 column(12,
                        tabItem(tabName="createmaker", width=NULL,
                                plotlyOutput("createmakerDegreePlot", height="1000px"))
                 )
               ),
               
               # Gender and Ethinicity Plot
               h3(""),
               fluidRow(
                 column(6,
                        tabItem(tabName="createmaker", width=NULL,
                                plotOutput("createmakerGenderPlot", height = "400px"))
                 ),
                 column(6,
                        tabItem(tabName="createmaker", width=NULL,
                                plotOutput("createmakerEthinicityPlot", height = "400px"))
                 )
               ),
               
               # Iwi and Residency Plot
               h3(""),
               fluidRow(
                 column(6,
                        tabItem(tabName="createmaker", width=NULL,
                                plotOutput("createmakerResidencyPlot", height = "400px"))
                 ),
                 column(6,
                        tabItem(tabName="createmaker", width=NULL,
                                plotOutput("createmakerIwiPlot", height = "400px"))
                 )
               ),
               
               # Studio time series
               conditionalPanel(
                 condition = "input.baseProgramme == 'Create and Maker Space Studio Participant'",
                 h3("Studio Time"),
                 fluidRow(
                   column(12,
                          tabItem(tabName="createmaker", width=NULL,
                                  plotlyOutput("createmakerStudioTimeseriesPlot", height = "400px"))
                   )),
                 pickerInput(
                   "createmakerStudioMonth",
                   "Month",
                   selected = sort(unique(allStudio$month))[1:2],
                   choices = sort(unique(allStudio$month)),
                   options = list(`actions-box` = TRUE, placeholder="Select faculty..."),
                   multiple = T
                 ),
                 h3(""),
                 fluidRow(
                   column(12,
                          tabItem(tabName="createmaker", width=NULL,
                                  plotOutput("createmakerStudioPurposePlot", height = "800px"))
                 )),
                 h3(""),
                 fluidRow(
                   column(12,
                          tabItem(tabName="createmaker", width=NULL,
                                  plotOutput("createmakerStudioEquipmentPlot", height = "800px"))
                   ))
                 
               )
       ),
       
       # Journey table
       tabItem(
         tabName = "journey",
         fluidRow(
           column(6,
                  tabItem(tabName = "journey", width=NULL,
                          pickerInput(
                            "baseSource",
                            "Source",
                            selected="",
                            choices = sort(unique(availProg$programme)),
                            options = list(`actions-box` = TRUE, placeholder="Select source..."),
                            multiple = F
                          ))),
           column(6,
                  tabItem(tabName = "journey", width=NULL,
                          pickerInput(
                            "baseDestination",
                            "Destination",
                            selected="",
                            choices = sort(unique(availProg$programme)),
                            options = list(`actions-box` = TRUE, placeholder="Select destination..."),
                            multiple = F
                          ))),
           column(12,
                    htmlOutput("journeyTotal"))),
         fluidRow(
           column(12,
                  tabItem(tabName = "journey", width=NULL,
                          plotOutput("journeyBarChart", height = "400px")))),
         pickerInput(
           "journeyGroup",
           "Group total",
           selected=c("1","2","3"),
           choices =c("1","2","3","4","5","6","7","8"),
           options = list(`actions-box` = TRUE, placeholder="Select group..."),
           multiple = T
         ),
         actionButton("updateTotal", "Update"),
         h3(""),
         fluidRow(
           column(12,
                  tabItem(tabName = "journey", width=NULL,
                          plotOutput("journeyEventHeatmap", height="800px")))
         ),
         h3(""),
         fluidRow(
           column(12,
                  tabItem(tabName = "journey", width=NULL,
                          dataTableOutput("journeyTable")))
         ),
         h3(""),
         fluidRow(
           column(12,
                  tabItem(tabName = "journey", width=NULL,
                          sankeyNetworkOutput("journeySankey", height="800px")))
         ),
         h3(""),
         fluidRow(
           column(12,
                  tabItem(tabName = "journey", width=NULL, 
                          plotOutput("journeyIndividualHeatmap", height="800px")))
         )
       )
       # End of tabItem
     )
   )
)

