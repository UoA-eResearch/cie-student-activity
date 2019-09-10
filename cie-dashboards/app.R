## ui.R ##
## CIE Dashboards ##

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
   
   # Application title
   dashboardHeader(title = "CIE Dashboard"),
   
   # Menu
   dashboardSidebar(
     sidebarMenu(
       
       # Dashboard menu
       menuItem("Overview", tabName = "overview", icon = icon("fas fa-square"))
       
       
       
       
       
       
     )
   ),
   
   # Content
   dashboardBody(
     tabItems(
       
       # Overview
       tabItem(
         tabName = "overview",
         
         # Title
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
       )
     )
   )
)

## server.R ##
## CIE Dashboards ##
# Define server logic required to draw a histogram
server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)

