## CIE Uploads ##

# Settings
options(java.parameters = "-Xmx4G")
system("sudo touch app.R")

# libraries
library(shiny)
library(tidyr)
library(tidyverse)
library(readxl)
library(plyr)
library(dplyr)
library(widyr)

library(DT)
library(tools)
source("functions.R")
source("sheet_choices.R")
data_dir <- Sys.getenv("CIE_DATA_DIR", unset = "../data")
backup_dir <- Sys.getenv("CIE_BACKUP_DIR", unset = "../backup_data")

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
      radioButtons("saveType", "Select type of file", choices = c("None", "SSO" = "From Rachel - ", 
                                                                  "CRM" = "Original - ", 
                                                                  "TAG" = "tags-selection", 
                                                                  "TRAINING" = "Members and Training ",
                                                                  "C&M" = "C&M Space Sign In ",
                                                                  "INNOVATION" = "Innovation Hub Sign In ")),
      
      # Type
      radioButtons("saveSheet", "Select sheet to preview", choices = c("None")),
      
      # Save Button ----
      actionButton("save", "Save"),
      actionButton("reload", "Reload"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Guide ----
      tags$em("For EXCEL files"),
      tags$li(tags$sub("SSO excel files must contain 'Student', 'Applicant', 'No Affil', 'No citizenship' Sheet")),
      tags$li(tags$sub("CRM excel files must contain 'contacts' Sheet")),
      tags$li(tags$sub("TAGS excel files must contain 'Tags' Sheet")),
      tags$li(tags$sub("C&M and INNOVATION excel files must contain 'Form Responses 1' Sheet")),
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
      
      # Error message
      verbatimTextOutput("error"),
      
      # Output: Data file ----
      dataTableOutput("contents")
    )
    
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  # Update the filers based on selected year
  observe({
    upload_path <- NULL
    if (!is.null(input$uploadFile) && !is.null(input$uploadFile$datapath)) {
      upload_path <- input$uploadFile$datapath
    }

    sheet_choices <- get_save_sheet_choices(input$saveType, upload_path)
    updateRadioButtons(session, "saveSheet", choices = sheet_choices)
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
      
      if (input$saveType == "tags-selection") {
        # # colnames to check, change "Curricula" to "Curricular"
        # check_colnames = colnames(read_excel("../data/base/tags-selection2019 2019-12-05 23:56:11.xlsx"))
        # check_colnames[check_colnames == "Curricula"] = "Curricular"
        # check_colnames = c(check_colnames, "Co-Curricular")
        # # Check if column names are consistent
        # columnCondition <- all(sort(check_colnames) == sort(colnames(df)))
        
        # Check if column names are consistent
        columnCondition <- all(sort(colnames(read_excel("../data/base/tags-selection2019 2019-12-05 23:56:11.xlsx"))) == sort(colnames(df)))
        validate(
          need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read_excel("../data/base/tags-selection2019 2019-12-05 23:56:11.xlsx")), colnames(df))))
        )
        
      } else if (input$saveType == "Original - ") {
        
        # Check if column names are consistent
        columnCondition <- any(all(sort(colnames(read_excel("../data/base/Original - 2016 CIE Participants at 20190708.xlsx"))) == sort(colnames(df))),
                               all(sort(colnames(read_csv("../data/base/Original - 2017 CIE Participant - downloaded 10 July.csv"))) == sort(colnames(df))))
        validate(
          need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read_excel("../data/base/Original - 2016 CIE Participants at 20190708.xlsx")), colnames(df))))
        )
        
      } else if (input$saveType == "C&M Space Sign In ") {
        
        # Check if column names are consistent
        columnCondition <- all(sort(colnames(read_excel("../data/base/C&M Space Sign In 2017 2019-12-05 03:25:16.xlsx"))) == sort(colnames(df)))
        validate(
          need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read_excel("../data/base/C&M Space Sign In 2017 2019-12-05 03:25:16.xlsx")), colnames(df))))
        )
        
      } else if (input$saveType == "Innovation Hub Sign In ") {
        
        # Check if column names are consistent
        columnCondition <- all(sort(colnames(read_excel("../data/base/Innovation Hub Sign In 2019 2019-12-05 21:39:14.xlsx"))) == sort(colnames(df)))
        validate(
          need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read_excel("../data/base/Innovation Hub Sign In 2019 2019-12-05 21:39:14.xlsx")), colnames(df))))
        )
        
      }
      
    } else if (file_ext(uploadPath) == "xlsx") {
      
      # Import tags_selection.xlsx
      if (input$saveType == "tags-selection") {
        # Check if there is a sheet named Tag
        sheetCondition <- "Tags" %in% excel_sheets(uploadPath)
        validate(
          need(sheetCondition==TRUE, message="TAG file needs sheet named 'Tags'")
        )
        
        # Read in dataframe
        df <- read_excel(uploadPath, sheet = "Tags")
        
        # colnames to check, change "Curricula" to "Curricular"
        check_colnames = colnames(read_excel("../data/base/tags-selection2019 2019-12-05 23:56:11.xlsx"))
        check_colnames[check_colnames == "Curricula"] = "Curricular"
        check_colnames = c(check_colnames, "Co-Curricular")
        
        # Check if column names are consistent
        columnCondition <- all(sort(check_colnames) == sort(colnames(df)))
        validate(
          need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read_excel("../data/base/tags-selection2019 2019-12-05 23:56:11.xlsx")), colnames(df))))
        )
      }
      # Import From.*xlsx
      else if (input$saveType == "From Rachel - " ) {
        if (input$saveSheet %in% c("Student", "Applicant", "No Affil", "No citizenship")) {
          # df <- read.xlsx2(uploadPath, sheetName=input$saveSheet, startRow = 2)
          df = read_excel(uploadPath, sheet = input$saveSheet, col_names = TRUE)
          #print(read.xlsx2(uploadPath, sheetName=input$saveSheet, startRow = 2))
          
          # Add column names row
          cols <- as.data.frame(t(colnames(df)))
          colnames(cols) <- colnames(df)
          df <- rbind.fill(cols, df)
          # Add an empty row
          #df <- add_row(df, .before = 1)
          
          # Check if column names are consistent
          # columnCondition <- all(sort(colnames(read.xlsx2("../data/base/From Rachel - 2019 CIE Participants.xlsx", sheetName = input$saveSheet, startRow = 2))) == sort(colnames(df)))
          # validate(
          #   need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read.xlsx2("../data/base/From Rachel - 2019 CIE Participants.xlsx", sheetName = input$saveSheet, startRow = 2)), colnames(df))))
          # )
          columnCondition <- all(sort(colnames(read_excel("../data/base/From Rachel - 2019 CIE Participants.xlsx", sheet = input$saveSheet, col_names = TRUE, skip = 1))) == sort(colnames(df)))
          validate(
            need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read_excel("../data/base/From Rachel - 2019 CIE Participants.xlsx", sheet = input$saveSheet, col_names = TRUE, skip = 1)), colnames(df))))
          )
          
        }
      }
      
      # Import Original.*xlsx
      else if (input$saveType == "Original - ") {
        
        # Check if sheet names are correct
        validate(
          need("contacts" %in% excel_sheets(uploadPath), message = "CRM file needs at least one sheet named 'contacts'")
        )
        
        # Read in dataframe
        df  <- read_excel(uploadPath)
        
        # Check if column names are consistent
        coldiff = setdiff(colnames(read_excel("../data/base/Original - 2016 CIE Participants at 20190708.xlsx")), colnames(df))
        validate(
          need(length(coldiff) == 0, message=paste0("Error in column names: ", coldiff))
        )
        # Drop any extra cols
        df = df[colnames(read_excel("../data/base/Original - 2016 CIE Participants at 20190708.xlsx"))]
      }
      # Import Member and Training
      else if (input$saveType == "Members and Training ") {
        if (input$saveSheet %in% c("3D Printer", "Laser Cutter", "3D Scanner", "Vinyl Cutter","CNC Router", "Sewing Machine", "Soldering and Desoldering Stati", "Hand and Power Tools")) {
          df <- read.xlsx2(uploadPath, sheetName=input$saveSheet, startRow = 1)
          
          # Check if column names are consistent
          columnCondition <- all(sort(colnames(read.xlsx2("../data/base/Members and Training 2019 2019-11-22 05:05:06.xlsx", sheetName = input$saveSheet))) == sort(colnames(df)))
          validate(
            need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read.xlsx2("../data/base/Members and Training 2019 2019-11-22 05:05:06.xlsx", sheetName = input$saveSheet)), colnames(df))))
          )
          
        }
      }
      # Import C&M Sign In and Innovation Hub Sign In
      else if (input$saveType %in% c("C&M Space Sign In ", "Innovation Hub Sign In ")) {
        
        # Check if sheet names are correct
        validate(
          need("Form Responses 1" %in% excel_sheets(uploadPath), "File needs at laste one sheet named 'Form Responses 1'")
        )
        
        # Read in data frame
        df <- read_excel(uploadPath)
        
        if (input$saveType == "Innovation Hub Sign In ") {
          
          # Check if column names are consistent
          columnCondition <- all(sort(colnames(read_excel("../data/base/Innovation Hub Sign In 2019 2019-12-05 21:39:14.xlsx"))) == sort(colnames(df)))
          validate(
            need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read_excel("../data/base/Innovation Hub Sign In 2019 2019-12-05 21:39:14.xlsx")), colnames(df))))
          )
          
        } else {
          
          # Check if column names are consistent
          columnCondition <- all(sort(colnames(read_excel("../data/base/C&M Space Sign In 2017 2019-12-05 03:25:16.xlsx"))) == sort(colnames(df)))
          validate(
            need(columnCondition==TRUE, message=paste0("Error in column names: ",setdiff(colnames(read_excel("../data/base/C&M Space Sign In 2017 2019-12-05 03:25:16.xlsx")), colnames(df))))
          )
          
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
  
  output$saveFileName <- renderPrint({
    saveName()
  })
  
  observeEvent(input$save, {
    req(input$uploadFile, input$saveType, input$saveYear)
    saved_upload_path <- persist_uploaded_file(
      input$uploadFile$datapath,
      data_dir,
      input$saveYear,
      saveName()
    )
    source_path <- saved_upload_path
    output_path <- NULL
    
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
      
    } else { # FOR SSO & CRM & Sign Ins
      checkDir <- dir(file.path(data_dir, input$saveYear), pattern = paste0(input$saveType, ".*"), full.names = TRUE)
      checkDir2 <- file.path(backup_dir, input$saveYear)
    }
    
    status <- tryCatch({
      withProgress(message = "Save uploaded files to server", style = "notification", value = 0.1, {
        # save uploaded files to data and backup data directories
        if (file_ext(source_path) == "csv") {
          incProgress(.4)
          Sys.sleep(.1)

          # Save .csv data files
          output_path <- file.path(data_dir, input$saveYear, saveName())
          write.csv(data(), file = output_path, row.names = FALSE, quote = TRUE)

        } else if (file_ext(source_path) == "xlsx") {
          incProgress(.4)
          Sys.sleep(.1)

          if (input$saveType == "tags-selection") {

            # Save tag files
            output_path <- file.path(data_dir, "tags", saveName())
            openxlsx::write.xlsx(data(), file = output_path, sheetName = "Tags", rowNames = FALSE, overwrite = TRUE)

          } else if (input$saveType == "From Rachel - ") {
            # FOR SSO
            sheets <- list()

            ## Read every sheet and rbind after
            for (availSheet in intersect(excel_sheets(source_path), c("Student", "Applicant", "No Affil", "No citizenship"))) {
              # Read the sheet
              df <- openxlsx::read.xlsx(source_path, sheet = availSheet, startRow = 2, detectDates = FALSE, check.names = FALSE)
              cols <- as.data.frame(t(colnames(df))) # Add column names row
              colnames(cols) <- colnames(df)
              sheets[[availSheet]] <- rbind.fill(cols, df)
            }

            # Save .xlsx data files
            output_path <- file.path(data_dir, input$saveYear, saveName())
            openxlsx::write.xlsx(sheets, file = output_path, overwrite = TRUE)

          } else if (input$saveType == "Original - ") {
            # FOR CRM
            # Save .xlsx data files
            output_path <- file.path(data_dir, input$saveYear, saveName())
            openxlsx::write.xlsx(data(), file = output_path, sheetName = "contacts", rowNames = FALSE, overwrite = TRUE)

          } else if (input$saveType == "Members and Training ") {
            # FOR TRAINING
            sheets <- list()

            ## Read every sheet and rbind after
            for (availSheet in intersect(excel_sheets(source_path), c("3D Printer", "Laser Cutter", "3D Scanner", "Vinyl Cutter","CNC Router", "Sewing Machine", "Soldering and Desoldering Stati", "Hand and Power Tools"))) {
              # Read the sheet
              sheets[[availSheet]] <- openxlsx::read.xlsx(source_path, sheet = availSheet, startRow = 1, detectDates = FALSE, check.names = FALSE)
            }

            # Save .xlsx data files
            output_path <- file.path(data_dir, "training", saveName())
            openxlsx::write.xlsx(sheets, file = output_path, overwrite = TRUE)

          } else if (input$saveType %in%  c("C&M Space Sign In ", "Innovation Hub Sign In ")) {
            # FOR SIGNINS
            output_path <- file.path(data_dir, input$saveYear, saveName())
            openxlsx::write.xlsx(data(), file = output_path, sheetName = "Form Responses 1", rowNames = FALSE, overwrite = TRUE)
          }
        }

        if (!is_empty(checkDir)) {
          backup_pattern <- paste0("^", escape_regex(input$saveType))
          for (existing_file in checkDir) {
            if (!same_file_contents(existing_file, output_path)) {
              backup_file_if_unique(existing_file, checkDir2, pattern = backup_pattern)
            }
            file.remove(existing_file)
          }
        }

        incProgress(.4)
        Sys.sleep(.1)

        # print output message
        output$status <- renderPrint({"Saving sucessfully!"})
      })

      # run the data management script functions
      process_write(data_dir, backup_dir)
    }, error = function(e) {
      paste0("Saved raw upload at ", source_path, ", but processing failed: ", conditionMessage(e))
    })

    # print output messsage
    output$status <- renderPrint({status})
  })
  
  observeEvent(input$reload, {
    # run the data management script functions
    status <- process_write(data_dir, backup_dir)
    
    # print output messsage
    output$status <- renderPrint({status})
  })
  
}

shinyApp(ui, server)
