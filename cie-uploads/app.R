## CIE Uploads ##

# Settings
options(java.parameters = "- Xmx1024m")
system("touch app.R")

# libraries
library(shiny)
library(tidyr)
library(tidyverse)
library(readxl)
library(xlsx)
library(plyr)
library(dplyr)
library(DT)
library(tools)
source("functions.R")
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
                        
                        # Year
                        selectInput("saveYear", "Select year", choices = 2015:as.numeric(format(Sys.Date(),"%Y"))+1, selected = as.numeric(format(Sys.Date(),"%Y"))),
                        
                        # Type
                        radioButtons("saveType", "Select type of file", choices = c("None", "SSO" = "From Rachel - ", "CRM" = "Original - ", "TAG" = "tags-selection", "TRAINING"="Members and Training ")),
                        
                        # Type
                        radioButtons("saveSheet", "Select sheet to preview", choices = c("None")),
                        
                        # Save Button ----
                        actionButton("save", "Save"),
                        actionButton("reload", "Reload"),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Guide ----
                        tags$em("For EXCEL files"),
                        tags$li(tags$sub("SSO excel files must contain 'Student' Sheet")),
                        tags$li(tags$sub("CRM excel files must contain 'contacts' Sheet")),
                        tags$li(tags$sub("TAGS excel files must contain 'Tags' Sheet")),
                        tags$br(),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Status
                        verbatimTextOutput("status")
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
server <- function(input, output, session) {
        # Update the filers based on selected year
        observe({
          if (input$saveType %in% c("From Rachel - ", "Members and Training ")) {
            updateRadioButtons(session, "saveSheet", choices = c(intersect(excel_sheets(input$uploadFile$datapath), c("3D Printer", "Laser Cutter", "3D Scanner", "Vinyl Cutter","CNC Router", "Sewing Machine", "Soldering and Desoldering Stati", "Hand and Power Tools"))))
          }
        })
  
        data <- reactive({
                # Error messages
                validate(
                        need(input$saveYear != "", message="Please select valid year"),
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
                        if (input$saveType == "tags-selection" && "Tags" %in% excel_sheets(uploadPath)) {
                                df <- read_excel(uploadPath, sheet = "Tags")
                        }
                        # Import From.*xlsx
                        else if ( ("Student" %in% excel_sheets(uploadPath) || "Students" %in% excel_sheets(uploadPath))  && input$saveType == "From Rachel - " ) {
                                if (input$saveSheet %in% c("Student", "Applicant", "No Affil", "No citizenship")) {
                                  df <- read.xlsx2(uploadPath, sheetName=input$saveSheet, startRow = 2)
                                  
                                  # Add column names row
                                  cols <- as.data.frame(t(colnames(df)))
                                  colnames(cols) <- colnames(df)
                                  df <- rbind.fill(cols, df)
                                  # Add an empty row
                                  df <- add_row(df, .before = 1)
                                }
                        }
                        # Import Original.*xlsx
                        else if ( "contacts" %in% excel_sheets(uploadPath) && input$saveType == "Original - ") {
                                df  <- read_excel(uploadPath)
                        }
                        # Import Member and Training
                        else if ( input$saveType == "Members and Training ") {
                          if ( input$saveSheet %in% c("3D Printer", "Laser Cutter", "3D Scanner", "Vinyl Cutter","CNC Router", "Sewing Machine", "Soldering and Desoldering Stati", "Hand and Power Tools")) {
                                df <- read.xlsx2(uploadPath, sheetName=input$saveSheet, startRow = 1)
                          }
                        }
                }
                
                # Change to dafa.frame
                df <- as.data.frame(df)
                
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
        output$saveFileName <- renderPrint({
                saveName()
                })

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
                if (input$saveType == "tags-selection") { # For TAGS
                  checkDir <- dir(file.path(data_dir, "tags"), pattern = paste0(input$saveType, ".*"), full.names = TRUE)
                  checkDir2 <- file.path(backup_dir, "tags")
               
                } else if (input$saveType == "Members and Training ") {  # For TRAINING
                  checkDir <- dir(file.path(data_dir, "training"), pattern = paste0(input$saveType, ".*"), full.names = TRUE)
                  checkDir2 <- file.path(backup_dir, "training")
                  
                } else { # FOR SSO & CRM
                  checkDir <- dir(file.path(data_dir, input$saveYear), pattern = paste0(input$saveType, ".*"), full.names = TRUE)
                  checkDir2 <- file.path(backup_dir, input$saveYear)
                }
                
                if (!is_empty(checkDir)){
                  file.copy(checkDir, checkDir2, recursive = TRUE)
                
                  #remove previous data from data directories
                  file.remove(checkDir)
                }  
                
                withProgress(message = "Save uploaded file to server", style = "notification", value = 0.1, {
                  # save uploaded files to data and backup data directories
                  if (file_ext(uploadPath) == "csv") {
                    incProgress(.4)
                    Sys.sleep(.1)
                    # Save .csv data files
                    write.csv(data(), file = file.path(data_dir, input$saveYear, saveName()), row.names = FALSE, quote = TRUE)
                    
                  } else if (file_ext(uploadPath) == "xlsx") {
                    incProgress(.4)
                    Sys.sleep(.1)
                    if (input$saveType == "tags-selection") {
                      
                      # Save tag files
                      write.xlsx2(data(), file=file.path(data_dir, "tags", saveName()), sheetName = "Tags", row.names = FALSE)
                      
                    } else if (input$saveType == "From Rachel - ") { 
                      # FOR SSO  
                      # Create an empty workbook
                      wb = createWorkbook()
                      
                      ## Read every sheet and rbind after
                      for (availSheet in intersect(excel_sheets(input$uploadFile$datapath), c("Student", "Applicant", "No Affil", "No citizenship"))) {
                        # Create an empty sheet
                        sheet = createSheet(wb, availSheet)
                        
                        # Read the sheet
                        df <- read.xlsx2(input$uploadFile$datapath, sheetName=availSheet, startRow = 2, stringsAsFactors = FALSE)
                        cols <- as.data.frame(t(colnames(df))) # Add column names row
                        colnames(cols) <- colnames(df)
                        df <- rbind.fill(cols, df)
                        df <- add_row(df, .before = 1) # Add an empty row
                        
                        # Add data frame to the sheet
                        addDataFrame(df, sheet = sheet, startColumn = 1, row.names = FALSE)
                      }
                      
                      # Save .xlsx data files
                      saveWorkbook(wb, file.path(data_dir, input$saveYear, saveName()))
                      
                    } else if (input$saveType == "Original - ") {
                      # FOR CRM
                      # Save .xlsx data files
                      write.xlsx2(data(), file=file.path(data_dir, input$saveYear, saveName()), sheetName = "contacts", row.names = FALSE)
                      
                    } else if (input$saveType == "Members and Training ") {
                      # FOR TRAINING
                      # Create an empty workbook
                      wb = createWorkbook()
                      
                      ## Read every sheet and rbind after
                      for (availSheet in intersect(excel_sheets(input$uploadFile$datapath), c("3D Printer", "Laser Cutter", "3D Scanner", "Vinyl Cutter","CNC Router", "Sewing Machine", "Soldering and Desoldering Stati", "Hand and Power Tools"))) {
                        # FOR TRAINING
                        # Create an empty sheet
                        sheet = createSheet(wb, availSheet)
                        
                        # Read the sheet
                        df <- read.xlsx2(input$uploadFile$datapath, sheetName=availSheet, startRow = 1, stringsAsFactors = FALSE)
                        
                        # Add data frame to the sheet
                        addDataFrame(df, sheet = sheet, startColumn = 1, row.names = FALSE)
                      }
                      
                      # Save .xlsx data files
                      saveWorkbook(wb, file.path(data_dir, "training", saveName()))
                    }
                  }
                  incProgress(.4)
                  Sys.sleep(.1)
                  
                  # print output message
                  output$status <- renderPrint({"Saving sucessfully!"})
                })
                if (!input$saveType == "Members and Training ") {
                      # run the data management script functions
                      status <- process_write(data_dir, backup_dir)
                      
                      # print output messsage
                      output$status <- renderPrint({status})
                }
                
                
                
        })
        
        observeEvent(input$reload, {
                # run the data management script functions
                status <- process_write(data_dir, backup_dir)
                
                # print output messsage
                output$status <- renderPrint({status})
        })
        
}

shinyApp(ui, server)