## server.R ##
## CIE Dashboards

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
allData <- read_csv("../data/overview.csv")
selection <- read_csv("../data/tags_selection.csv")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactivate dataframes
  filterData <- reactive({
    # Filter programmes based on tab names
    colNum <- match(input$tab, colnames(selection))
    df1 <- selection %>% 
      filter(selection[,colNum] == "Y") %>% 
      select(tag_programme)
    df2 <- allData %>% 
      filter(programme %in% df1$tag_programme)
    return(df2)
  })
  
  infoOverview_r <- reactive({
    # Apply inputs as filter criteria
    df <- filterData() %>% 
      filter(year == input$baseYear)
    return(df)
  })
  overviewPlot_df <- reactive({
    df <- filterData() %>% 
      select(ID,year,programme) %>% 
      distinct() # Remove duplicates
    return(df)
  })
  facultyPlot_df <- reactive({
    df <- filterData() %>%
      filter(year %in% c(input$baseYear, input$compareYears)) %>% 
      select(ID,year,`Owner of Major/Spec/Module`) %>% 
      distinct() # Remove duplicates
    return(df)
  })
  programmePlot_df <- reactive({
    df <- filterData() %>%
      filter(year %in% c(input$baseYear, input$compareYears)) %>% 
      select(ID,year,programme) %>% 
      distinct() # Remove duplicates
    return(df)
  })
  heatmap_df <- reactive({
    df <- filterData() %>% 
      filter(year %in% c(input$baseYear, input$compareYears)) %>%
      select(ID,`Owner of Major/Spec/Module`, programme, year) %>% 
      distinct() # Remove duplicates
    return(df)
  })
  
  
  # Info boxes
  output$totalParticipant <- renderInfoBox(
    {
      total_participant <- nrow(infoOverview_r())
      infoBox(
        "Total participant", #m vlaue here, icon= icon(), color=
        total_participant
      )
    })
  output$uniqueParticipant <- renderInfoBox(
    {
      unique_particpant <- infoOverview_r() %>% distinct(`ID`) %>% count()
      infoBox(
        "Unique participant", #m vlaue here, icon= icon(), color=
        unique_particpant
      )
    })
  output$repeatParticipant <- renderInfoBox(
    {
      repeat_participant <- infoOverview_r() %>% group_by(ID) %>% filter(row_number()==2) %>% distinct(`ID`) %>% nrow()
      unique_particpant <- infoOverview_r() %>% distinct(`ID`) %>% count()
      infoBox(
        "Repeat participant", #m vlaue here, icon= icon(), color=
        paste0(repeat_participant, " (", round(repeat_participant*100/unique_particpant,1),"%)")
      )
    })
  output$onetimeParticipant <- renderInfoBox(
    {
      onetime_participant <- infoOverview_r() %>% group_by(ID) %>% summarise(count=n()) %>% filter(count < 2) %>% distinct(`ID`) %>% nrow()
      infoBox(
        "One-time participant", #m vlaue here, icon= icon(), color=
        onetime_participant
      )
    })
  
  # Overview plot
  output$totalPlot <- renderPlotly({
    p <- overviewPlot_df() %>% 
      select(ID,year) %>% 
      group_by(year) %>% 
      summarise(count=n()) %>% 
      ggplot() +
      geom_line(aes(x=year,y=count)) +
      ggtitle("Total participants by year") +
      theme_minimal() + guides(fill=FALSE, color=FALSE) + labs(y="", x = "") + scale_color_continuous_tableau()
    
    ggplotly(p)
  })
  
  output$uniquePlot <- renderPlotly({
    p <- overviewPlot_df() %>% 
      select(ID,year) %>% 
      distinct() %>% 
      group_by(year) %>%
      #distinct(ID) %>% 
      summarise(count=n()) %>% 
      ggplot() +
      geom_line(aes(x=year,y=count)) +
      ggtitle("Unique participants by year") +
      theme_minimal() + guides(fill=FALSE, color=FALSE) + labs(y="", x = "") + scale_color_continuous_tableau() 
    #+ annotate("segment", xmin=(input$baseYear - 0.5), xmax = (input$baseYear + 0.5), alpha= .2)
    ggplotly(p)
  })
  
  # Faculty
  output$facultyN <- renderPlotly({
    facultyPlot_df() %>% 
      group_by(`Owner of Major/Spec/Module`,year) %>% 
      summarise(count=n()) %>% 
      ggplot() +
      geom_bar(aes(x=reorder(`Owner of Major/Spec/Module`, count), count, fill=factor(year)), stat="identity", position = "dodge") +
      guides(fill=FALSE) +
      coord_flip() +
      ggtitle("Faculty split overall") +
      theme_minimal() + 
      scale_fill_tableau() + labs(x="", y="")
  })
  
  # Programme
  output$programmeN <- renderPlotly({
    programmePlot_df() %>% 
      group_by(`year`, `programme`) %>% 
      summarise(count=n()) %>% 
      ggplot() +
      geom_bar(aes(x=reorder(`programme`, count), count, fill=factor(year)), stat="identity", position = "dodge") +
      guides(fill=FALSE) +
      coord_flip() +
      ggtitle("Programme split overall") +
      theme_minimal() + 
      #labs(x="", y="", title="Programme split overall") +
      #theme(plot.title = element_text(hjust=0)) +
      scale_fill_tableau() + labs(x="", y="")
  })
  
  # Programme split by faculty
  # Create dataframe for heatmap
  # TODO: facet by year
  output$programmeFaculty <- renderPlotly({
    p <- heatmap_df() %>% 
      group_by(`programme`,`Owner of Major/Spec/Module`, year) %>% 
      summarise(count=n()) %>%
      complete(`programme` =unique(programme),`Owner of Major/Spec/Module` = unique(filterData()$`Owner of Major/Spec/Module`), year=unique(heatmap_df()$year)) %>% 
      distinct() %>% 
      ggplot(aes(`Owner of Major/Spec/Module`,`programme`)) +
      geom_raster(aes(fill=count)) +
      facet_wrap(year~.) +
      guides(color=FALSE, fill=FALSE) +
      scale_fill_gradient_tableau(na.value = "grey") +
      coord_equal() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = -30, hjust=0),
        panel.grid.major = element_blank(),
        panel.background = element_rect(fill="grey97")
        ) +
      labs(x="", y="")
    ggplotly(p) %>%
      layout(
              xaxis = list(showgrid = FALSE, scaleanchor="y", constrain="domain", zeroline=FALSE),
              yaxis = list(showgrid = FALSE, zeroline=FALSE)
        )
  })
  
  output$table <- renderDataTable(filterData())
}