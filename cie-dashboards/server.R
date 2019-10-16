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
library(shinyWidgets)

# Import data
allData <- read_csv("data/overview.csv")
selection <- read_csv("data/tags_selection.csv")

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
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
      distinct(ID, year, programme) %>%  # Remove people who are conjoints
      filter(!programme %in% c("CIE Participant")) %>% 
      filter(year %in% input$baseYear)
    
    if (input$tab == "programme") {
      df <- filterData() %>% 
        distinct(ID, year, programme) %>%  # Remove people who are conjoints
        filter(programme %in% input$baseProgramme) %>% 
        filter(year %in% input$baseYear)
    }
    return(df)
  })
  overviewPlot_df <- reactive({
    df <- filterData() %>% 
      distinct(ID,year,programme) %>% # Remove people who are conjoints
      filter(!programme %in% c("CIE Participant"))
    
    if (input$tab == "programme") {
      df <- filterData() %>% 
        distinct(ID,year,programme) %>% # Remove people who are conjoints
        filter(programme %in% input$baseProgramme)
    }
    return(df)
  })
  facultyPlot_df <- reactive({
    df <- filterData() %>%
      filter(year %in% input$baseYear) %>% 
      distinct(ID,year,programme, `Owner of Major/Spec/Module`) # Remove people who are conjoints
    return(df)
  })
  programmePlot_df <- reactive({
    df <- filterData() %>%
      filter(year %in% input$baseYear) %>% 
      distinct(ID,year,programme) %>%  # Remove people who are conjoints
      group_by(`year`, `programme`) %>% 
      summarise(count=n())
    return(df)
  })
  # Update the filers based on selected year
  # observe({
  #   updatePickerInput(session, "baseProgramme", choices = sort(unique(facultyPlot_df()$programme)))
  # })
  heatmap_df <- reactive({
    df <- filterData() %>% 
      filter(year %in% c(input$baseYear)) %>%
      distinct(ID,year,programme, `Owner of Major/Spec/Module`) # Remove people who are conjoints
    return(df)
  })
  debug_df <- reactive({
    df <- filterData() %>%
      filter(year %in% c(input$baseYear)) %>% 
      distinct(ID,year,programme)  # Remove people who are conjoints
    return(df)
  })
  
  # # Info boxes
  # output$totalParticipant <- renderInfoBox(
  #   {
  #     total_participant <- nrow(infoOverview_r() %>% filter(!programme %in% c("CIE Participant")))
  #     infoBox(
  #       "Total participant", #m vlaue here, icon= icon(), color=
  #       total_participant
  #     )
  #   })
  # output$uniqueParticipant <- renderInfoBox(
  #   {
  #     unique_particpant <- infoOverview_r() %>% distinct(`ID`) %>% count()
  #     infoBox(
  #       "Unique participant", #m vlaue here, icon= icon(), color=
  #       unique_particpant
  #     )
  #   })
  # output$repeatParticipant <- renderInfoBox(
  #   {
  #     repeat_participant <- infoOverview_r() %>% 
  #       distinct(ID,programme,year) %>% # Avoid conjoint students appear twice
  #       group_by(ID) %>% 
  #       filter(row_number()==2) %>%
  #       distinct(`ID`) %>%
  #       nrow()
  #     unique_particpant <- infoOverview_r() %>% distinct(`ID`) %>% count()
  #     infoBox(
  #       "Repeat participant", #m vlaue here, icon= icon(), color=
  #       paste0(repeat_participant, " (", round(repeat_participant*100/unique_particpant,1),"%)")
  #     )
  #   })
  # output$onetimeParticipant <- renderInfoBox(
  #   {
  #     onetime_participant <- infoOverview_r() %>% group_by(ID) %>% distinct(ID,programme,year) %>% summarise(count=n()) %>% filter(count < 2) %>% distinct(`ID`) %>% nrow()
  #     unique_particpant <- infoOverview_r() %>% distinct(`ID`) %>% count()
  #     infoBox(
  #       "One-time participant", #m vlaue here, icon= icon(), color=
  #       paste0(onetime_participant, " (", round(onetime_participant*100/unique_particpant,1),"%)")
  #     )
  #   })
  
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
  
  # output$uniquePlot <- renderPlotly({
  #   p <- overviewPlot_df() %>% 
  #     select(ID,year) %>% 
  #     distinct() %>% 
  #     group_by(year) %>%
  #     #distinct(ID) %>% 
  #     summarise(count=n()) %>% 
  #     ggplot() +
  #     geom_line(aes(x=year,y=count)) +
  #     ggtitle("Unique participants by year") +
  #     theme_minimal() + guides(fill=FALSE, color=FALSE) + labs(y="", x = "") + scale_color_continuous_tableau() 
  #   ggplotly(p)
  # })
  
  output$uniquePlot <- renderPlotly({
    p1 <- overviewPlot_df() %>% 
      select(ID, year, programme) %>%
      distinct() %>% # Avoid conjoint students appear twice
      arrange(year) %>% 
      group_by(ID, year) %>%
      filter(row_number()==2) %>% # Repeat students
      ungroup() %>% 
      #distinct(ID, year) %>% 
      group_by(year) %>% 
      summarise(repeatParticipant=n())
    
    p2 <- overviewPlot_df() %>% 
      select(ID, year) %>%
      distinct() %>% # Avoid conjoint students appear twice
      group_by(year) %>% 
      summarise(uniqueCount=n()) %>% 
      merge(p1, by="year") %>% 
      mutate(oneTimeParticpant = uniqueCount - repeatParticipant) %>% 
      gather(key="type_count", value="count", repeatParticipant, oneTimeParticpant) %>% 
      ggplot(aes(x=factor(year),y=count, fill=type_count)) +
      geom_bar(stat = "identity" ) +
      ggtitle("Unique participants by year") +
      theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
      scale_fill_tableau()
    
    ggplotly(p2)
  })
  
  output$programmeUniquePlot <- renderPlotly({
    p <- overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, fill=programme)) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      ggtitle("Unique participants by year") +
      theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
      scale_fill_tableau()
    
    ggplotly(p)
  })
  
  output$programmeRepeatPlot <- renderPlotly({
    p <- overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% # Avoid conjoint students appear twice
      arrange(year) %>% 
      group_by(ID, programme) %>%
      filter(row_number()>1) %>% # Returning students
      ungroup() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, fill=programme)) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      ggtitle("Repeat participants by year") +
      theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
      scale_fill_tableau()
    
    ggplotly(p)
  })
  
  # Faculty
  output$facultyN <- renderPlot({
    facultyPlot_df() %>% 
      group_by(`Owner of Major/Spec/Module`,year) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Owner of Major/Spec/Module`, count), count, fill=factor(year))) +
      geom_bar(stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
      geom_text(aes(label=count, color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), hjust=-0.1) +
      guides(color=FALSE) +
      coord_flip() +
      ggtitle("Faculty split overall") +
      theme_minimal() + 
      scale_fill_tableau() + scale_colour_tableau() +
      labs(x="", y="")
  })
  
  # Programme
  output$programmeN <- renderPlot({
    programmePlot_df() %>% 
      ggplot(aes(x=reorder(`programme`, count), count, fill=factor(year))) +
      geom_bar(stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
      geom_text(aes(label=count, color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), hjust=-0.1) +
      guides(color=FALSE) +
      coord_flip() +
      ggtitle("Programme split overall") +
      theme_minimal() + 
      scale_fill_tableau() + scale_colour_tableau() +
      labs(x="", y="")
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
  
  output$table <- renderDataTable(debug_df())
}