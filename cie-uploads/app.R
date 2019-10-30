## CIE Uploads ##

# libraries
library(shiny)
# library(shinydashboard)
# library(ggplot2)
# library(ggthemes)
library(tidyr)
# library(reshape2)
library(tidyverse)
library(readxl)
library(dplyr)
# library(plotly)
library(DT)
library(tools)
# library(shinyWidgets)
data_dir <- "../data"
backup_dir <- "../backup_data"

# Define UI for data upload app ----
ui <- fluidPage(
        
        # App title ----
        titlePanel("CIE Uploading Files"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
                
                # Sidebar panel for inputs ----
                sidebarPanel(
                        
                        # Input: Select a file ----
                        fileInput("uploadFile", "Choose File",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv",
                                             ".xlsx",
                                             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
                        
                        # Name
                        textInput("saveName", "Save file name as", placeholder="Enter file name..."),
                        
                        # Year
                        selectInput("saveYear", "Select year", choices = 2015:as.numeric(format(Sys.Date(),"%Y"))+1),
                        
                        # Save Button ----
                        actionButton("save", "Save")
                        
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                        
                        # Output: Data file ----
                        dataTableOutput("contents")
                        
                )
                
        )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
        data <- reactive({
                # Error messages
                validate(
                        need(input$saveName != "" && (grepl("From",input$saveName, fixed = TRUE) || grepl("Original",input$saveName, fixed =TRUE)), message="Please enter a valid filename"),
                        #need(!file.exists(file.path("data", input$saveYear, input$saveName)), message = "File already exists"),
                        need(input$saveYear != "", message="Please select valid year")
                        
                )
                req(input$uploadFile, input$saveName, input$saveYear)
                
                # Basename
                uploadPath <- input$uploadFile$datapath

                # Import Original.*csv
                if (file_ext(uploadPath) == "csv") {

                        df <- read_csv(uploadPath)

                } else if (file_ext(uploadPath) == "xlsx") {

                        # Import From.*xlsx
                        if ("Student" %in% excel_sheets(uploadPath)) {

                                df <- read_excel(uploadPath, sheet="Student", skip = 1)
                        }

                        # Import From.*xlsx
                        else {
                                df  <- read_excel(uploadPath)
                        }

                }
                return(df)
        })

        output$contents <- renderDataTable({
                # input$file1 will be NULL initially. After the user selects
                # and uploads a file, head of that data file by default,
                # or all rows if selected, will be shown
                
                #df <- read_csv(input$uploadFile$datapath) %>% head(10)
                df <- data() %>% head(100)

                return(df)
        })

        observeEvent(input$save, {
                req(input$csvFile, input$saveName, input$saveYear)
                uploadPath <- basename(input$uploadFile$datapath)
                
                saveName <- basename(input$saveName)
                
                if (file_ext(uploadPath == "csv")) {
                        
                        write.csv(data(), file = file.path(data_dir, input$saveYear, saveName), row.names = FALSE, quote = TRUE)
                        
                } else if (file_ext(uploadPath == "xlsx")) {
                        
                        df  <- read_excel(uploadPath)
                        
                }
                
        })
}

shinyApp(ui, server)