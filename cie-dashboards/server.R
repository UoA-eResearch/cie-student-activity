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
overviewData <- read_csv("../data/overview.csv")

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactivate dataframes
  infoOverview_r <- reactive({
    # Apply inputs as filter criteria
    df <- overviewData %>% 
      filter(year == input$baseYear)
    return(df)
  })
  overviewPlot_df <- reactive({
    df <- overviewData %>% 
      select(ID,year) %>% 
      distinct() # Remove duplicates
    return(df)
  })
  facultyPlot_df <- reactive({
    df <- overviewData %>%
      filter(year %in% c(input$baseYear, input$compareYears)) %>% 
      select(ID,year,`Owner of Major/Spec/Module`) %>% 
      distinct() # Remove duplicates
    return(df)
  })
  programmePlot_df <- reactive({
    df <- overviewData %>%
      filter(year %in% c(input$baseYear, input$compareYears)) %>% 
      filter(programme %in% c("CIE Participant", "Velocity Participant", "Unleash Space Participant", "Unleash Space Access", "Equipment Training Participant" )) %>% 
      select(ID,year,programme) %>% 
      distinct() # Remove duplicates
    return(df)
  })
  heatmap_df <- reactive({
    df <- overviewData %>% 
      filter(year == input$baseYear) %>% 
      filter(programme %in% c("CIE Participant", "Velocity Participant", "Unleash Space Participant", "Unleash Space Access", "Equipment Training Participant" )) %>% 
      select(ID,`Owner of Major/Spec/Module`, programme) %>% 
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
      print(repeat_participant)
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
      group_by(year) %>%
      distinct(ID) %>% 
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
      scale_fill_tableau() 
  })
  
  # Programme split by faculty
  # Create dataframe for heatmap
  output$programmeFaculty <- renderPlotly({
    p <- heatmap_df() %>% 
      group_by(`programme`,`Owner of Major/Spec/Module`) %>% 
      summarise(count=n()) %>%
      complete(`Owner of Major/Spec/Module` = unique(df_stud$`Owner of Major/Spec/Module`)) %>% 
      ggplot(aes(`Owner of Major/Spec/Module`,`programme`)) + 
      geom_tile(aes(fill=count)) +
      guides(color=FALSE) +
      #ggtitle("Programme split by faulcuty") +
      scale_fill_gradient_tableau(na.value = "grey") +
      #scale_x_discrete(position="top") +
      #scale_fill_gradient(low="white", high = "steelblue", na.value="grey80") +
      #coord_fixed(ratio=.5) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 30, hjust=0), axis.ticks = element_blank()) +
      labs(x="", y="")
    ggplotly(p) %>% 
      layout(
              xaxis = list(showgrid = FALSE, side="top", scaleanchor="y", constrain="domain", zeroline=FALSE),
              yaxis = list(showgrid = FALSE, zeroline=FALSE)
        )
  })
}