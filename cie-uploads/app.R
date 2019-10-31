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
library(xlsx)
library(dplyr)
# library(plotly)
library(DT)
library(tools)
# library(shinyWidgets)
data_dir <- "../data"
backup_dir <- "../backup_data"
#tags_dir <- "../ta"

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
                        
                        # Year
                        selectInput("saveYear", "Select year", choices = 2015:as.numeric(format(Sys.Date(),"%Y"))+1, selected = as.numeric(format(Sys.Date(),"%Y"))),
                        
                        # Type
                        radioButtons("saveType", "Select type of file", choices = c("None", "SSO" = "From Rachel - ", "CRM" = "Original - ", "TAG" = "tags_selection -")),
                        
                        # Save Button ----
                        actionButton("save", "Save"),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Guide ----
                        tags$em("For EXCEL files"),
                        tags$li(tags$sub("SSO excel files must contain 'Student' Sheet")),
                        tags$li(tags$sub("CRM excel files must contain 'contacts' Sheet")),
                        tags$li(tags$sub("TAGS excel files must contain 'Tags' Sheet")),
                        tags$br()
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                        # Name
                        #textInput("saveName", "Save file name as", placeholder="Enter file name..."),
                        verbatimTextOutput("saveFileName"),
                  
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
                        #need(input$saveName != "" && (grepl("From",input$saveName, fixed = TRUE) || grepl("Original",input$saveName, fixed =TRUE)), message="Please enter a valid filename"),
                        #need(!file.exists(file.path("data", input$saveYear, input$saveName)), message = "File already exists"),
                        need(input$saveYear != "", message="Please select valid year"),
                        #need(input$uploadFile$datapath != "", message="Please select files to upload"),
                        need(input$saveType != "None", message = "Please select file type")
                        
                )
                req(input$uploadFile, input$saveType, input$saveYear)
                
                # Basename
                uploadPath <- input$uploadFile$datapath

                # Import Original.*csv
                if (file_ext(uploadPath) == "csv") {

                        df <- read_csv(uploadPath)

                } else if (file_ext(uploadPath) == "xlsx") {
                        
                        # Import tags_selection.xlsx
                        if (input$saveType == "tags_selection" && "Tags" %in% excel_sheets(uploadPath)) {
                                df <- read_excel(uploadPath, sheet = "Tags")
                        }
                        # Import From.*xlsx
                        else if ("Student" %in% excel_sheets(uploadPath) && input$saveType == "From Rachel - ") {
                                df <- read_excel(uploadPath, sheet="Student", skip = 1)
                        }
                        else if ("contacts" %in% excel_sheets(uploadPath) && input$saveType == "Original - ") {
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
        
        saveName <- reactive({
          req(input$uploadFile, input$saveType, input$saveYear)
          validate(
            need(input$saveType != "None", message="")
          )
          uploadPath <- basename(input$uploadFile$datapath)
          
          # Add time stamp
          name <- paste0(input$saveType, input$saveYear, " ", Sys.time())
          
          # Add file extension
          name <- paste(name, file_ext(uploadPath), sep =".")
          
          return(name)
        })
        
        #output$value <- renderPrint( {input$saveType})
        output$saveFileName <- renderPrint({saveName()})
        #output$saveFileName <- renderPrint(file.exists(file.path(data_dir,input$saveYear)))

        observeEvent(input$save, {
                req(input$uploadFile, input$saveType, input$saveYear)
                uploadPath <- basename(saveName())
                
                # create subdirectory by year if it doesn't exist
                if (!file.exists(file.path(data_dir,input$saveYear))) {
                  dir.create(file.path(data_dir, input$saveYear))
                }
                if (!file.exists(file.path(backup_dir, input$saveYear))) {
                  dir.create(file.path(backup_dir, input$saveYear))
                }
                
                # Copy overwrite previous data to backup directory
                if (input$saveType == "tags_selection - ") {
                  checkDir <- dir(file.path(data_dir, "tags"), pattern = paste0(input$saveType, ".*"), full.names = TRUE)
                  checkDir2 <- file.path(backup_dir, "tags")
                } else {
                  checkDir <- dir(file.path(data_dir, input$saveYear), pattern = paste0(input$saveType, ".*"), full.names = TRUE)
                  checkDir2 <- file.path(backup_dir, input$saveYear)
                }
                file.copy(checkDir, checkDir2, recursive = TRUE)
                
                #remove previous data from data directories
                file.remove(checkDir)
                
                # save uploaded files to data and backup data directories
                if (file_ext(uploadPath) == "csv") {
                        # Save .csv data files
                        write.csv(data(), file = file.path(data_dir, input$saveYear, saveName()), row.names = FALSE, quote = TRUE)
                  
                } else if (file_ext(uploadPath) == "xlsx") {
                  
                        if (input$saveType == "tags_selection -") {
                          
                          # Save tag files
                          write.xlsx2(data(), file=file.path(data_dir, "tags", saveName()))
                          
                        } else {
                          
                          # Save .xlsx data files
                          write.xlsx2(data(), file=file.path(data_dir, input$saveYear, saveName()))
                          
                        }
                }
                
                # run the data management script functions
                
                
                # print output messsage
        })
        
}

shinyApp(ui, server)