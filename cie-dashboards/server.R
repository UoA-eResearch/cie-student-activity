## server.R ##
## CIE Dashboards

# 

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
library(networkD3)

filtermap = list(
  "Gender" = "Sex",
  "Ethnic Group" = "Ethnic.Group",
  "Ethnicity" = "Ethnicity",
  "Iwi" = "Descr",
  "Faculty" = "Owner.of.Major.Spec.Module",
  "Department" = "Plan.Description",
  "Affiliation" = "Programme.Level",
  "Residency" = "Residency.Status",
  "Year" = "year"
)

# Functions
filter_data <- function(dashboard, data_df, selection_df) {
  # Filter programmes based on tab names
  colNum <- match(dashboard, colnames(selection_df))
  df1 <- selection_df %>% 
    filter(selection_df[,colNum] == "Y") %>% 
    select(tag_programme)
  df2 <- data_df %>% 
    filter(programme %in% df1$tag_programme)
  return(df2)
}

# Import data
allData <- read_csv("../data/all.csv", col_types = cols(ID = col_character()))
selection <- read_csv("../data/tags/tags_selection.csv")
selection$date <- as.Date(ifelse(is.na(selection$date), paste0(as.character(selection$year), "-01-01"), as.character(selection$date)))
overview_df <- filter_data("overview", allData, selection)
programme_df <- filter_data("programme", allData, selection)
velocity_df <- filter_data("velocity", allData, selection)
unleash_df <- filter_data("unleash", allData, selection)
createmaker_df <- filter_data("createmaker", allData, selection)
journey_df <- filter_data("journey", allData, selection)
all_training <- read_csv("../data/all_training.csv", col_types = cols(ID = col_character())) %>% filter(!is.na(date)) %>% distinct()
all_studio <- read_csv("../data/all_studio.csv", col_types = cols(ID = col_character(), year = col_character())) %>% filter(!is.na(timestamp)) %>% distinct()
colnames(all_training) <- c("ID", "date", "programme")

curricula_programmes = sort(unique(selection$tag_programme[selection$curricula == "Y"]))
if ("co-curricula" %in% colnames(selection)) {
  cocurricula_programmes = sort(unique(selection$tag_programme[selection$`co-curricula` == "Y"]))
  curricula_df = allData %>% mutate(
    programme = case_when(
      programme %in% curricula_programmes ~ "Curricula",
      programme %in% cocurricula_programmes ~ "Co-curricula"
    )
  ) %>% filter(!is.na(programme))
} else {
  curricula_df = programme_df %>% mutate(
    programme = ifelse(programme %in% curricula_programmes, "Curricula", "Co-curricula")
  )
}


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Reactivate dataframes
  filterData <- reactive({
    if (input$tab == "overview") {
      return(overview_df)
    } else if (input$tab == "programme") {
      return(programme_df)
    } else if (input$tab == "velocity") {
      return(velocity_df)
    } else if (input$tab == "unleash") {
      return(unleash_df)
    } else if (input$tab =="journey") {
      return(journey_df)
    } else if (input$tab == "createmaker") {
      return(createmaker_df)
    } else if (input$tab == "curricula") {
      return(curricula_df)
    }
  })
  
  facultyDepartment = reactive({
    if (input$tab == "programme") {
      return(input$programmeFacultyDepartment)
    } else if (input$tab == "curricula") {
      return(input$curriculaFacultyDepartment)
    }
  })
  
  affiliationDegree = reactive({
    if (input$tab == "programme") {
      return(input$programmeAffiliationDegree)
    } else if (input$tab == "curricula") {
      return(input$curriculaAffiliationDegree)
    }
  })
  
  # Update the filers based on selected year
  observe({
    if (input$tab == "velocity") {
      
      updatePickerInput(session, "baseProgramme", selected = "Velocity Participant", choices = sort(unique(filterData()$programme)))
      
    } else if (input$tab == "unleash") {
      
      updatePickerInput(session, "baseProgramme", selected = "Unleash Space Participant", choices = sort(unique(filterData()$programme)))
      updatePickerInput(session, "unleashStudioMonth", selected = sort(unique(studio_df()$month))[1:4], choices = sort(unique(studio_df()$month)))
      
    } else if (input$tab == "createmaker") {
      
      updatePickerInput(session, "baseProgramme", selected = "Equipment Training Participant", choices = sort(unique(filterData()$programme)))
      updatePickerInput(session, "createmakerStudioMonth", selected = sort(unique(studio_df()$month))[1:4], choices = sort(unique(studio_df()$month)))
      
    } else if (input$tab == "journey") {
      
      availChoices <- filterData() %>% filter(year %in% input$baseYear) %>% mutate(programme = paste(year, programme)) %>% distinct(programme)
      availTraining <- all_training %>% mutate(year=format(date, "%Y")) %>% filter(year %in% input$baseYear) %>% distinct(programme)
      availStudio <- all_studio %>% filter(year %in% input$baseYear) %>% distinct(programme)
      availChoices <- bind_rows(availChoices, availTraining, availStudio)
      availChoices_baseDestination <- selection %>% filter(journey=="Y") %>% filter(!is.na(date)) %>% filter(year %in% input$baseYear) %>% filter(!grepl("^\\D", date)) %>% arrange(date) %>% distinct(final_tags)
      
      updatePickerInput(session, "baseProgramme", selected = sort(unique(availChoices$programme)), choices = sort(unique(availChoices$programme)))
      updatePickerInput(session, "baseDestination", choices = sort(unique(availChoices_baseDestination$final_tags)))
      updatePickerInput(session, "baseSource", selected = "", choices = c("",availChoices_baseDestination$final_tags))
      
    } else if (input$tab %in% c("overview","programme")) {
      
      updatePickerInput(session, "baseProgramme", selected = "CIE Participant", choices = sort(unique(filterData()$programme)))
      
    } else if (input$tab == "curricula") {
      curricula_options = sort(unique(curricula_df$programme))
      updatePickerInput(session, "baseProgramme", selected = curricula_options, choices = curricula_options)
    }
  })
  
  studio_df <- reactive({
    if (input$tab == "unleash") {
      df <- all_studio %>% 
        filter(year %in% input$baseYear) %>% 
        filter(grepl("Innovation", programme))
    } else if (input$tab == "createmaker") {
      df <- all_studio %>% 
        filter(year %in% input$baseYear) %>% 
        filter(!grepl("Innovation", programme))
    }
    
    return(df)
  })
  
  overviewPlot_df <- reactive({
    if (!input$tab %in% c("overview")) {
      df <- filterData() %>% 
        distinct(ID,year,programme) %>% # Remove people who are conjoints
        filter(programme %in% input$baseProgramme)
    } else {
      df <- filterData() %>% 
        distinct(ID,year,programme) #%>% # Remove people who are conjoints
      #filter(!programme %in% c("CIE Participant"))
    }
    return(df)
  })
  
  facultyPlot_df <- reactive({
    df <- filterData() %>%
      filter(year %in% input$baseYear) %>% 
      distinct(ID,year, `Owner.of.Major.Spec.Module`)
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
  
  generalPlot_df <- reactive({
    if (!input$tab %in% c("overview")) {
      df <- filterData() %>% 
        filter(year %in% input$baseYear) %>% 
        filter(programme %in% input$baseProgramme)
      for (label in names(filtermap)) {
        key = filtermap[[label]]
        if (length(input[[key]]) >= 1) {
          print(paste("Filtering", key, label, input[[key]]))
          df <- df %>%
            filter(df[[key]] %in% input[[key]])
        }
      }
      return(df)
    }
  })
  
  heatmap_df <- reactive({
    df <- filterData() %>% 
      filter(year %in% c(input$baseYear)) %>%
      distinct(ID, year, `Owner.of.Major.Spec.Module`, `programme`)
    return(df)
  })
  
  debug_df <- reactive({
    df <- filterData() %>% 
      filter(year %in% input$baseYear) %>% 
      filter(programme %in% input$baseProgramme)
    return(df)
  })
  journey_map_df <- reactive({
    # Filter non-students
    df <- filterData() %>%
      filter(!`Owner.of.Major.Spec.Module` %in% c("ALUMNI","STAFF", "EXTERNAL")) %>% select(`ID`, `programme`, `year`)
    
    # Add year to programme and remove year
    df$programme <- paste(df$year, df$programme)
    
    # Filter year and programme
    df <- df %>%
      filter(year %in% input$baseYear) %>%
      filter(programme %in% input$baseProgramme)
    
    # Filter out Journey Table data
    tags <- selection %>% filter(journey=="Y")
    #tags <- tags %>% filter(date !="Overarching Tag") %>% filter(date !="Unleash Space Master List") %>% filter(date !="") %>% filter(!is.na(date))  # Need to include these in then
    #tags <- tags %>% filter(!is.na(date))
    tags <- tags %>% select(`final_tags`, `date`)
    
    # Filter ID that went to the destination or source
    if (input$baseSource != "") {
      selectedIDs <- df %>% filter(programme %in% input$baseSource) %>% distinct(ID)
    } else {
      selectedIDs <- df %>% filter(programme %in% input$baseDestination) %>% distinct(ID)
    }
    df <- df %>% filter(ID %in% selectedIDs$ID) %>% distinct()
    if (nrow(df) == 0) {
      stop("No matching rows for inputs")
    }
    
    # Add count
    df$count <- 1
    
    # Fill in empty cells
    df <- df %>% complete(programme=unique(programme), ID=unique(ID)) %>% distinct() # Fill in empty cells
    df <- merge(df, tags, by.x="programme", by.y="final_tags", all.x = TRUE) %>% distinct() # Add date
    
    # Add all_training dfs
    training_df <- all_training %>% filter(ID %in% selectedIDs$ID)  %>% mutate(count=1, year=format(date, "%Y")) #%>% select(training, ID, count, date)
    df <- rbind(df, training_df)
    
    # Add all_studio dfs
    studio_df <- all_studio %>% filter(ID %in% selectedIDs$ID)  %>% mutate(count=1) %>% select(programme, ID, count, date, year)
    df <- rbind(df, studio_df)
    
    # Filter year and programme
    df <- df %>%
      filter(year %in% input$baseYear) %>%
      filter(programme %in% input$baseProgramme)
    
    df <- df %>% complete(programme=unique(programme), ID=unique(ID)) %>% distinct() # Fill in empty cells
    df[is.na(df$count),]["count"] <- 0 # Replace NAs with 0
    
    # Filter events after the destination date
    if (input$baseSource != "") {
      filteredDate <- selection[selection$final_tags==input$baseSource,]$date
      df <- df %>% filter(!date <filteredDate) %>% distinct()
    } else {
      filteredDate <- selection[selection$final_tags==input$baseDestination,]$date
      df <- df %>% filter(!date >filteredDate) %>% distinct()
    }
    if (nrow(df) == 0) {
      stop("No matching rows for inputs")
    }
    
    #print(unique(df$programme))
    return(df)
  })
  
  journey_table_df <- reactive({
    # Filter count == 1
    df <- journey_map_df() %>% filter(count==1)
    
    # Add total number of events per ID, add total number of IDs per total
    df_total_event <-df %>% group_by(ID) %>% summarise(total=n()) %>% distinct()
    df_total_event <- df_total_event %>% group_by(total) %>% mutate(num_students=n()) %>% ungroup()
    
    # Merge
    df <- merge(df, df_total_event, by = "ID")
    
    # Sorting events, remove destination
    if (input$baseSource != "") {
      sortedProg <- df %>% distinct(programme,date) %>% filter(programme!=input$baseSource) %>% arrange(date) %>% distinct(programme)
      sortedProg <- c("total", "num_students", input$baseSource, sortedProg$programme)
    } else {
      sortedProg <- df %>% distinct(programme,date) %>% filter(programme!=input$baseDestination) %>% arrange(date) %>% distinct(programme)
      sortedProg <- c("total", "num_students", sortedProg$programme, input$baseDestination)
    }
    
    # Spread
    df <- df %>% spread(key=programme, value = count)
    
    if (nrow(df) == 0) {
      stop("No matching rows for inputs")
    }
    
    # Replace NAs with 0s
    df[is.na(df)] = 0
    
    # Sorted the column
    df <-  df[,unlist(sortedProg)]
    df <- aggregate(.~num_students+total, df,FUN=sum)
    
    return(df)
  })
  
  journey_sankey_df <- reactive({
    # Filter out Journey Table data
    tags <- selection %>% filter(journey=="Y")
    #tags <- tags %>% filter(!is.na(date))
    tags <- tags %>% select(`final_tags`, `date`)
    
    # Add number of events per ID
    df <- journey_map_df() %>% filter(count==1)
    df <- df %>% group_by(ID) %>% mutate(count_event=n()) %>% ungroup()
    
    # Split datasets into single event goers and multiple event goers
    df_single <- df %>% filter(count_event==1) %>% group_by(ID) %>% arrange(date, .by_group=TRUE) %>% ungroup()
    df_not_single <- df %>% filter(count_event!=1) %>% group_by(ID) %>% arrange(date, .by_group=TRUE) %>% ungroup()
    
    # Add lags to both datasets
    df_single_lag <- df_single %>% group_by(ID) %>% mutate(source.programme=lead(programme, 1, default = NA)) %>% arrange(date, .by_group=TRUE) %>% ungroup()
    df_not_single_lag <- df_not_single %>% group_by(ID) %>% mutate(target.programme=lead(programme, 1, default = NA)) %>% arrange(date, .by_group=TRUE) %>% filter(!is.na(target.programme)) %>% ungroup()
    
    # Change column names
    df_not_single_lag <- df_not_single_lag %>% select(programme, target.programme, ID, date)
    colnames(df_not_single_lag) <- c("source.programme", "target.programme", "ID", "date")
    
    df_single_lag <- df_single_lag %>% select(programme, source.programme, ID, date)
    if (input$baseSource == "") {
      colnames(df_single_lag) <- c("target.programme", "source.programme", "ID", "date")
    } else {
      colnames(df_single_lag) <- c("source.programme", "target.programme", "ID", "date")
    }
    df_lag <- rbind(df_single_lag, df_not_single_lag)
    
    # Sum counts grouped by target and source
    df <- df_lag %>% group_by(`target.programme`,`source.programme`) %>% summarise(count=n())
    
    # Add dates to target + source
    df <- merge(df, tags, by.x="source.programme", by.y="final_tags", all.x = TRUE) %>% distinct() # Add date
    df <- df %>% mutate(date.source.programme = paste(date, source.programme)) %>% select(-date)
    df <- merge(df, tags, by.x="target.programme", by.y="final_tags", all.x = TRUE) %>% distinct() # Add date
    df <- df %>% mutate(date.target.programme = paste(date, target.programme)) %>% select(-date)
    df <- df %>% arrange(-count) %>% head(100)
    
    return(df)
  })
  
  ## Overview Dashboard
  output$totalPlot <- renderPlot({
    overviewPlot_df() %>% 
      #filter(!programme %in% c("CIE Participant")) %>% 
      select(ID,year) %>%
      group_by(year) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=year,y=count, label=count)) +
      geom_line(size=1, colour="#9ecae1") +
      geom_text(aes(y=count+200), alpha=0.8) +
      ggtitle("Total participants by year") +
      theme_minimal() + guides(fill=FALSE, color=FALSE) + labs(y="", x = "")
  })
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
      mutate(repeatCount = repeatParticipant) %>% 
      gather(key="type_count", value="count", repeatParticipant, oneTimeParticpant) %>% 
      ggplot(aes(x=factor(year),y=count, fill=type_count)) +
      geom_bar(stat = "identity" ) +
      geom_text(aes(label=uniqueCount, y=uniqueCount+50), size=3, alpha=0.5) +
      geom_text(aes(label=paste0(repeatCount, " (", round(repeatCount*100/uniqueCount,0),"%)"), y=repeatCount*0.5), size=3, alpha=0.5) +
      ggtitle("Unique participants by year") +
      theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
      scale_fill_brewer()
    
    ggplotly(p2)
  })
  
  # Faculty
  output$facultyN <- renderPlot({
    facultyPlot_df() %>% 
      group_by(`Owner.of.Major.Spec.Module`,year) %>% 
      summarise(count=n()) %>%
      group_by(year) %>% 
      mutate(sum_count=sum(count)) %>% 
      ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, count), count, fill=factor(year))) +
      geom_bar(stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
      #geom_text(aes(label=count, color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), hjust=-0.1) +
      geom_text(aes(label=paste0(count, " (", round(count*100/sum_count,1),"%)"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), hjust=-0.1) +
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
  output$programmeSplitFaculty <- renderPlotly({
    p <- heatmap_df() %>% 
      group_by(`programme`,`Owner.of.Major.Spec.Module`, year) %>% 
      summarise(count=n()) %>%
      complete(`programme` =unique(programme),`Owner.of.Major.Spec.Module` = unique(filterData()$`Owner.of.Major.Spec.Module`), year=unique(heatmap_df()$year)) %>% 
      distinct() %>% 
      ggplot(aes(`Owner.of.Major.Spec.Module`,`programme`)) +
      geom_raster(aes(fill=count)) +
      geom_text(aes(label=count, colour=count>100), size=2, alpha=0.4) +
      facet_wrap(year~.) +
      guides(color=FALSE, fill=FALSE) +
      scale_fill_gradient_tableau(na.value = "grey") +
      scale_color_manual(guide = FALSE, values = c("black", "white")) +
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
  
  ## Programme Dashboard
  output$curriculaUniquePlot <- output$programmeUniquePlot <- renderPlot({
    overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, label=count)) +
      facet_wrap(programme~.) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      ggtitle("Unique participants by year") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(panel.background = element_rect(fill="grey99", colour="grey99"))
    #scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$curriculaRepeatPlot <- output$programmeRepeatPlot <- renderPlot({
    overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% # Avoid conjoint students appear twice
      arrange(year) %>% 
      group_by(ID, programme) %>%
      filter(row_number()>1) %>% # Returning students
      ungroup() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, label=count)) +
      facet_wrap(programme~.) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      ggtitle("Repeat participants by year") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(panel.background = element_rect(fill="grey99", colour="grey99"))
  })
  
  output$curriculaFacultyPlot <- output$programmeFacultyPlot <- renderPlot({
    for (label in names(filtermap)) {
      key = filtermap[[label]]
      if (length(input[[key]]) >= 1 || key == "year") {
        key = sym(key)
        return(generalPlot_df() %>% 
                 select(ID, !!key, programme, `Owner.of.Major.Spec.Module`) %>% 
                 distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
                 group_by(`Owner.of.Major.Spec.Module`, !!key, programme) %>% 
                 summarise(count=n()) %>% 
                 group_by(!!key, programme) %>% 
                 mutate(sum_count=sum(count)) %>% 
                 ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, -count), y=count, label=count, fill=factor(!!key), colour=factor(!!key))) +
                 geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
                 geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(!!key)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
                 geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
                 facet_wrap(programme~., ncol=3) +
                 ggtitle("Faculty") +
                 theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
                 theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
                 scale_fill_tableau() + scale_colour_tableau())
      }
    }
  })
  
  output$curriculaDepartmentPlot <- output$programmeDepartmentPlot <- renderPlotly({
    if (length(input$baseYear)>1) {
      generalPlot_df() %>%
        filter(`Owner.of.Major.Spec.Module` %in% facultyDepartment()) %>% # Filter selected faculties
        select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
        group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
        summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
        group_by(`Plan.Description`, programme ,`Owner.of.Major.Spec.Module`) %>%
        mutate(ymin=min(count), ymax=max(count)) %>% 
        ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=ymin, yend=ymax), color="grey") +
        geom_point(size=4, alpha=1) +
        geom_text(color="white", size=2) +
        coord_flip() +
        facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Department") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
    else {
      generalPlot_df() %>%
        filter(`Owner.of.Major.Spec.Module` %in% facultyDepartment()) %>% # Filter selected faculties
        select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
        group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
        summarise(count=n()) %>% 
        ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=0)) +
        geom_point(size=2, alpha=.9) +
        geom_text(hjust=0, nudge_y=2.5, size=3) +
        coord_flip() +
        facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Department") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
  })
  
  output$curriculaAffiliationPlot <- output$programmeAffiliationPlot <- renderPlot({
    for (label in names(filtermap)) {
      key = filtermap[[label]]
      if (length(input[[key]]) >= 1 || key == "year") {
        key = sym(key)
        return (generalPlot_df() %>% 
                  select(ID, !!key, programme, `Programme.Level`) %>% 
                  distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
                  group_by(`Programme.Level`, !!key, programme) %>% 
                  summarise(count=n()) %>% 
                  group_by(!!key, programme) %>% 
                  mutate(sum_count=sum(count)) %>% 
                  ggplot(aes(x=reorder(`Programme.Level`, -count), y=count, label=count, fill=factor(!!key), colour=factor(!!key))) +
                  geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
                  geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
                  geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(!!key)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
                  facet_wrap(programme~., ncol=3) +
                  ggtitle("Affiliation") +
                  theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
                  theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
                  scale_fill_tableau() + scale_colour_tableau())
      }
    }
  })
  
  output$curriculaDegreePlot <- output$programmeDegreePlot <- renderPlotly({
    key = ""
    for (label in names(filtermap)) {
      this_key = filtermap[[label]]
      if (length(input[[this_key]]) >= 1) {
        key = sym(this_key)
      }
    }
    if (key != "") {
      print(paste("Faceting programmeDegreePlot by", key))
      generalPlot_df() %>%
        filter(`Programme.Level` %in% affiliationDegree()) %>% # Filter selected
        select(ID, !!key, programme, `Descriptio`, `Programme.Level`) %>%
        group_by(!!key, programme ,`Descriptio`, `Programme.Level`) %>%
        summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
        group_by(programme , `Descriptio`, `Programme.Level`) %>%
        mutate(ymin=min(count), ymax=max(count)) %>% 
        ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(!!key), colour=factor(!!key))) +
        geom_segment(aes(y=ymin, yend=ymax), color="grey") +
        geom_point(size=4, alpha=1) +
        geom_text(color="white", size=2) +
        coord_flip() +
        facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Degree") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    } else if (length(input$baseYear)>1) {
      generalPlot_df() %>%
        filter(`Programme.Level` %in% affiliationDegree()) %>% # Filter selected
        select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
        group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
        summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
        group_by(programme , `Descriptio`, `Programme.Level`) %>%
        mutate(ymin=min(count), ymax=max(count)) %>% 
        ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=ymin, yend=ymax), color="grey") +
        geom_point(size=4, alpha=1) +
        geom_text(color="white", size=2) +
        coord_flip() +
        facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Degree") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
    else {
      generalPlot_df() %>%
        filter(`Programme.Level` %in% affiliationDegree()) %>% # Filter selected
        select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
        group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
        summarise(count=n()) %>%
        ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=0)) +
        geom_point(size=2, alpha=.9) +
        geom_text(hjust=0, nudge_y=2.5, size=3) +
        coord_flip() +
        facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Degree") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
  })
  
  output$curriculaGenderPlot <- output$programmeGenderPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Sex`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Sex`, year, programme) %>% 
      summarise(count=n()) %>% 
      group_by(year, programme) %>% 
      mutate(sum_count=sum(count)) %>% 
      ggplot(aes(x=reorder(`Sex`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Gender") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      guides(colour=FALSE) + labs(y="", x = "") +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$curriculaEthinicityPlot <- output$programmeEthinicityPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Ethnicity`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Ethnicity`, year, programme) %>% 
      summarise(count=n()) %>% 
      group_by(year, programme) %>% 
      mutate(sum_count=sum(count)) %>% 
      ggplot(aes(x=reorder(`Ethnicity`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Ethnic group") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$curriculaResidencyPlot <- output$programmeResidencyPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Residency.Status`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Residency.Status`, year, programme) %>% 
      summarise(count=n()) %>% 
      group_by(year, programme) %>% 
      mutate(sum_count=sum(count)) %>% 
      ggplot(aes(x=reorder(`Residency.Status`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .1))) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Residency.Status") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$curriculaIwiPlot <- output$programmeIwiPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Descr`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Descr`, year, programme) %>%
      summarise(count=n()) %>% 
      filter(!`Descr` == "NA") %>% 
      group_by(year, programme) %>% 
      mutate(sum_count=sum(count)) %>%
      ggplot(aes(x=reorder(`Descr`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .15))) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      geom_text(aes(label=paste0(round(count*100/sum_count,1),"%"), color=factor(year)), position = position_dodge2(width = 0.9, preserve = "single"), vjust=-1.6, alpha=.8) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Iwi") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  ## Velocity Dashboard
  output$velocityUniquePlot <- renderPlot({
    overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, label=count)) +
      facet_wrap(programme~.) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      ggtitle("Unique participants by year") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(panel.background = element_rect(fill="grey99", colour="grey99"))
  })
  
  output$velocityRepeatPlot <- renderPlot({
    overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% # Avoid conjoint students appear twice
      arrange(year) %>% 
      group_by(ID, programme) %>%
      filter(row_number()>1) %>% # Returning students
      ungroup() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, label=count)) +
      facet_wrap(programme~.) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      ggtitle("Repeat participants by year") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(panel.background = element_rect(fill="grey99", colour="grey99"))
  })
  
  output$velocityFacultyPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Owner.of.Major.Spec.Module`) %>% 
      distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
      group_by(`Owner.of.Major.Spec.Module`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      #coord_flip() +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Faculty") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$velocityDepartmentPlot <- renderPlotly({
    if (length(input$baseYear)>1) {
      generalPlot_df() %>%
        filter(`Owner.of.Major.Spec.Module` %in% input$velocityFacultyDepartment) %>% # Filter selected faculties
        select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
        group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
        summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
        group_by(`Plan.Description`, programme ,`Owner.of.Major.Spec.Module`) %>%
        mutate(ymin=min(count), ymax=max(count)) %>% 
        ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=ymin, yend=ymax), color="grey") +
        geom_point(size=4, alpha=1) +
        geom_text(color="white", size=2) +
        coord_flip() +
        facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Department") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
    else {
      generalPlot_df() %>%
        filter(`Owner.of.Major.Spec.Module` %in% input$velocityFacultyDepartment) %>% # Filter selected faculties
        select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
        group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
        summarise(count=n()) %>% 
        ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=0)) +
        geom_point(size=2, alpha=.9) +
        geom_text(hjust=0, nudge_y=2.5, size=3) +
        coord_flip() +
        facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Department") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
  })
  
  output$velocityAffiliationPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Programme.Level`) %>% 
      distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
      group_by(`Programme.Level`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Programme.Level`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      #coord_flip() +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Affiliation") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$velocityDegreePlot <- renderPlotly({
    if (length(input$baseYear)>1) {
      generalPlot_df() %>%
        filter(`Programme.Level` %in% input$velocityAffiliationDegree) %>% # Filter selected
        select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
        group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
        summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
        group_by(programme , `Descriptio`, `Programme.Level`) %>%
        mutate(ymin=min(count), ymax=max(count)) %>% 
        ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=ymin, yend=ymax), color="grey") +
        geom_point(size=4, alpha=1) +
        geom_text(color="white", size=2) +
        coord_flip() +
        facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Degree") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
    else {
      generalPlot_df() %>%
        filter(`Programme.Level` %in% input$velocityAffiliationDegree) %>% # Filter selected
        select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
        group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
        summarise(count=n()) %>%
        ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=0)) +
        geom_point(size=2, alpha=.9) +
        geom_text(hjust=0, nudge_y=2.5, size=3) +
        coord_flip() +
        facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Degree") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
  })
  
  output$velocityGenderPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Sex`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Sex`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Sex`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Gender") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      guides(colour=FALSE) + labs(y="", x = "") +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$velocityEthinicityPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Ethnicity`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Ethnicity`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Ethnicity`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Ethnic group") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$velocityResidencyPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Residency.Status`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Residency.Status`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Residency.Status`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Residency.Status") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$velocityIwiPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Descr`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Descr`, year, programme) %>% 
      summarise(count=n()) %>% 
      filter(!`Descr` == "NA") %>% 
      ggplot(aes(x=reorder(`Descr`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Iwi") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  ## Unleash Dashboard
  output$unleashUniquePlot <- renderPlot({
    overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, label=count)) +
      facet_wrap(programme~.) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      ggtitle("Unique participants by year") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(panel.background = element_rect(fill="grey99", colour="grey99"))
  })
  
  output$unleashRepeatPlot <- renderPlot({
    overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% # Avoid conjoint students appear twice
      arrange(year) %>% 
      group_by(ID, programme) %>%
      filter(row_number()>1) %>% # Returning students
      ungroup() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, label=count)) +
      facet_wrap(programme~.) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      ggtitle("Repeat participants by year") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(panel.background = element_rect(fill="grey99", colour="grey99"))
  })
  
  output$unleashFacultyPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Owner.of.Major.Spec.Module`) %>% 
      distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
      group_by(`Owner.of.Major.Spec.Module`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      #coord_flip() +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Faculty") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$unleashDepartmentPlot <- renderPlotly({
    if (length(input$baseYear)>1) {
      generalPlot_df() %>%
        filter(`Owner.of.Major.Spec.Module` %in% input$unleashFacultyDepartment) %>% # Filter selected faculties
        select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
        group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
        summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
        group_by(`Plan.Description`, programme ,`Owner.of.Major.Spec.Module`) %>%
        mutate(ymin=min(count), ymax=max(count)) %>% 
        ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=ymin, yend=ymax), color="grey") +
        geom_point(size=4, alpha=1) +
        geom_text(color="white", size=2) +
        coord_flip() +
        facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Department") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
    else {
      generalPlot_df() %>%
        filter(`Owner.of.Major.Spec.Module` %in% input$unleashFacultyDepartment) %>% # Filter selected faculties
        select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
        group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
        summarise(count=n()) %>% 
        ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=0)) +
        geom_point(size=2, alpha=.9) +
        geom_text(hjust=0, nudge_y=2.5, size=3) +
        coord_flip() +
        facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Department") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
  })
  
  output$unleashAffiliationPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Programme.Level`) %>% 
      distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
      group_by(`Programme.Level`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Programme.Level`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Affiliation") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$unleashDegreePlot <- renderPlotly({
    if (length(input$baseYear)>1) {
      generalPlot_df() %>%
        filter(`Programme.Level` %in% input$unleashAffiliationDegree) %>% # Filter selected
        select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
        group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
        summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
        group_by(programme , `Descriptio`, `Programme.Level`) %>%
        mutate(ymin=min(count), ymax=max(count)) %>% 
        ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=ymin, yend=ymax), color="grey") +
        geom_point(size=4, alpha=1) +
        geom_text(color="white", size=2) +
        coord_flip() +
        facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Degree") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
    else {
      generalPlot_df() %>%
        filter(`Programme.Level` %in% input$unleashAffiliationDegree) %>% # Filter selected
        select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
        group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
        summarise(count=n()) %>%
        ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=0)) +
        geom_point(size=2, alpha=.9) +
        geom_text(hjust=0, nudge_y=2.5, size=3) +
        coord_flip() +
        facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Degree") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
  })
  
  output$unleashGenderPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Sex`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Sex`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Sex`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Gender") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      guides(colour=FALSE) + labs(y="", x = "") +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$unleashEthinicityPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Ethnicity`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Ethnicity`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Ethnicity`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Ethnic group") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$unleashResidencyPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Residency.Status`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Residency.Status`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Residency.Status`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Residency.Status") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$unleashIwiPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Descr`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Descr`, year, programme) %>% 
      summarise(count=n()) %>% 
      filter(!`Descr` == "NA") %>% 
      ggplot(aes(x=reorder(`Descr`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Iwi") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$unleashStudioTimeseriesPlot <- renderPlotly({
    # Repeat count
    repeat_count <- studio_df() %>% 
      filter(!is.na(date)) %>% 
      select(date, ID) %>% 
      distinct() %>% 
      group_by(ID) %>% 
      arrange(date, .by_group=TRUE) %>% 
      ungroup() %>% 
      group_by(ID) %>% 
      mutate(last.date = lag(date, 1, default = NA)) %>% 
      filter(!is.na(`last.date`)) %>% 
      group_by(date) %>% 
      summarise(repeat.count=n())
    
    studio_df() %>% 
      filter(!is.na(date)) %>% 
      select(date, ID) %>% 
      distinct() %>% 
      group_by(date) %>% 
      summarise(unique.count=n()) %>% 
      merge(repeat_count, by="date", all.x = TRUE) %>%
      gather(key="type", value="value", 2:3) %>% 
      ggplot(aes(date, value, color=type)) + 
      geom_line(linetype="dotted") + 
      geom_point() +
      ggtitle("Studio Participant Timeseries") +
      theme_minimal()
  })
  
  output$unleashStudioPurposePlot <- renderPlot({
    studio_df() %>% 
      filter(month %in% input$unleashStudioMonth) %>% 
      group_by(month, purpose) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(reorder(purpose,count), count, label=count)) +
      facet_wrap(month~., ncol=2, scale="free_y") +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(hjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      coord_flip() +
      ggtitle("Studio purpose per month") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.background = element_rect(fill="grey97", colour = "white")) + 
      guides(colour=FALSE) + labs(y="", x = "") +
      #theme(axis.text.x = element_text(angle = -20, vjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  ## Create Maker Dashboard
  output$createmakerUniquePlot <- renderPlot({
    overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, label=count)) +
      facet_wrap(programme~.) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      ggtitle("Unique participants by year") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(panel.background = element_rect(fill="grey99", colour="grey99"))
  })
  
  output$createmakerRepeatPlot <- renderPlot({
    overviewPlot_df() %>% 
      select(ID,year, programme) %>%
      distinct() %>% # Avoid conjoint students appear twice
      arrange(year) %>% 
      group_by(ID, programme) %>%
      filter(row_number()>1) %>% # Returning students
      ungroup() %>% 
      group_by(year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=factor(year),y=count, label=count)) +
      facet_wrap(programme~.) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      ggtitle("Repeat participants by year") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(panel.background = element_rect(fill="grey99", colour="grey99"))
  })
  
  output$createmakerFacultyPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Owner.of.Major.Spec.Module`) %>% 
      distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
      group_by(`Owner.of.Major.Spec.Module`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Owner.of.Major.Spec.Module`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Faculty") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$createmakerDepartmentPlot <- renderPlotly({
    if (length(input$baseYear)>1) {
      generalPlot_df() %>%
        filter(`Owner.of.Major.Spec.Module` %in% input$createmakerFacultyDepartment) %>% # Filter selected faculties
        select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
        group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
        summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
        group_by(`Plan.Description`, programme ,`Owner.of.Major.Spec.Module`) %>%
        mutate(ymin=min(count), ymax=max(count)) %>% 
        ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=ymin, yend=ymax), color="grey") +
        geom_point(size=4, alpha=1) +
        geom_text(color="white", size=2) +
        coord_flip() +
        facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Department") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
    else {
      generalPlot_df() %>%
        filter(`Owner.of.Major.Spec.Module` %in% input$createmakerFacultyDepartment) %>% # Filter selected faculties
        select(ID, year, programme, `Plan.Description`, `Owner.of.Major.Spec.Module`) %>%
        group_by(`Plan.Description`, year, programme ,`Owner.of.Major.Spec.Module`) %>%
        summarise(count=n()) %>% 
        ggplot(aes(x=reorder(`Plan.Description`, count), xend=reorder(`Plan.Description`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=0)) +
        geom_point(size=2, alpha=.9) +
        geom_text(hjust=0, nudge_y=2.5, size=3) +
        coord_flip() +
        facet_grid(`Owner.of.Major.Spec.Module` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Department") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
  })
  
  output$createmakerAffiliationPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Programme.Level`) %>% 
      distinct() %>% # Avoid double counts people who switch degree levels from undergraduate to postgrad
      group_by(`Programme.Level`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Programme.Level`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Affiliation") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$createmakerDegreePlot <- renderPlotly({
    if (length(input$baseYear)>1) {
      generalPlot_df() %>%
        filter(`Programme.Level` %in% input$createmakerAffiliationDegree) %>% # Filter selected
        select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
        group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
        summarise(count=n(), ymin=min(count), ymax=max(count)) %>%
        group_by(programme , `Descriptio`, `Programme.Level`) %>%
        mutate(ymin=min(count), ymax=max(count)) %>% 
        ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=ymin, yend=ymax), color="grey") +
        geom_point(size=4, alpha=1) +
        geom_text(color="white", size=2) +
        coord_flip() +
        facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Degree") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
    else {
      generalPlot_df() %>%
        filter(`Programme.Level` %in% input$createmakerAffiliationDegree) %>% # Filter selected
        select(ID, year, programme, `Descriptio`, `Programme.Level`) %>%
        group_by(year, programme ,`Descriptio`, `Programme.Level`) %>%
        summarise(count=n()) %>%
        ggplot(aes(x=reorder(`Descriptio`, count), xend=reorder(`Descriptio`, count), y=count, yend=count, label=count, fill=factor(year), colour=factor(year))) +
        geom_segment(aes(y=0)) +
        geom_point(size=2, alpha=.9) +
        geom_text(hjust=0, nudge_y=2.5, size=3) +
        coord_flip() +
        facet_grid(`Programme.Level` ~ programme,  scales = "free_y", space = "free_y") +
        ggtitle("Degree") +
        theme_minimal() + guides(fill=FALSE) + labs(y="", x = "") +
        scale_fill_tableau() + scale_colour_tableau()
    }
  })
  
  output$createmakerGenderPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Sex`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Sex`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Sex`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Gender") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      guides(colour=FALSE) + labs(y="", x = "") +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$createmakerEthinicityPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Ethnicity`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Ethnicity`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Ethnicity`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Ethnic group") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$createmakerResidencyPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Residency.Status`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Residency.Status`, year, programme) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(x=reorder(`Residency.Status`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Residency.Status") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$createmakerIwiPlot <- renderPlot({
    generalPlot_df() %>% 
      select(ID, year, programme, `Descr`) %>% 
      distinct() %>% # Avoid doublecounting conjoints
      group_by(`Descr`, year, programme) %>% 
      summarise(count=n()) %>% 
      filter(!`Descr` == "NA") %>% 
      ggplot(aes(x=reorder(`Descr`, -count), y=count, label=count, fill=factor(year), colour=factor(year))) +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(vjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      facet_wrap(programme~., ncol=3) +
      ggtitle("Iwi") +
      theme_minimal() + guides(colour=FALSE) + labs(y="", x = "") +
      theme(axis.text.x = element_text(angle = 45, hjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  # Create Maker Studio
  output$createmakerStudioTimeseriesPlot <- renderPlotly({
    # Repeat count
    repeat_count <- studio_df() %>% 
      filter(!is.na(date)) %>% 
      select(date, ID) %>% 
      distinct() %>% 
      group_by(ID) %>% 
      arrange(date, .by_group=TRUE) %>% 
      ungroup() %>% 
      group_by(ID) %>% 
      mutate(last.date = lag(date, 1, default = NA)) %>% 
      filter(!is.na(`last.date`)) %>% 
      group_by(date) %>% 
      summarise(repeat.count=n())
    
    studio_df() %>% 
      #filter(grepl("Innovation", programme)) %>% 
      filter(!is.na(date)) %>% 
      select(date, ID) %>% 
      distinct() %>% 
      group_by(date) %>% 
      summarise(unique.count=n()) %>% 
      merge(repeat_count, by="date", all.x = TRUE) %>%
      gather(key="type", value="value", 2:3) %>% 
      ggplot(aes(date, value, color=type)) + 
      geom_line(linetype="dotted") + 
      geom_point() +
      ggtitle("Studio Participant Timeseries") +
      theme_minimal()
  })
  
  output$createmakerStudioPurposePlot <- renderPlot({
    studio_df() %>% 
      filter(month %in% input$createmakerStudioMonth) %>% 
      group_by(month, purpose) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(reorder(purpose,count), count, label=count)) +
      facet_wrap(month~., ncol=2, scale="free_y") +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(hjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      coord_flip() +
      ggtitle("Studio purpose per month") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.background = element_rect(fill="grey97", colour = "white")) + 
      guides(colour=FALSE) + labs(y="", x = "") +
      #theme(axis.text.x = element_text(angle = -20, vjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  output$createmakerStudioEquipmentPlot <- renderPlot({
    studio_df() %>% 
      filter(month %in% input$createmakerStudioMonth) %>% 
      group_by(month, equipment) %>% 
      summarise(count=n()) %>% 
      ggplot(aes(reorder(equipment,count), count, label=count)) +
      facet_wrap(month~., ncol=2, scale="free_y") +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(hjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      coord_flip() +
      ggtitle("Studio equipment per month") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.background = element_rect(fill="grey97", colour = "white")) + 
      guides(colour=FALSE) + labs(y="", x = "") +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  # Journey map
  output$journeyTotal <- renderText({
    df <- journey_map_df() %>% distinct(ID)
    return(paste("<b>Total: </b>",length(df$ID)))
  })
  
  output$journeyBarChart <- renderPlot({
    journey_table_df() %>% 
      select(total, num_students) %>% 
      ggplot(aes(factor(total),num_students)) + 
      geom_bar(stat="identity", position = position_dodge2(width = 0.9, preserve = "single")) +
      geom_text(aes(label=num_students), position = position_dodge2(width = 0.9, preserve = "single"), vjust=0) +
      theme_minimal() 
  })
  
  output$journeyTable <- renderDataTable({
    df <- journey_table_df() %>% select(-num_students)
    #df <- t(df) # Transpose
    return(df)
    #return(journey_table_df())
    #return(journey_map_df())
    #return(journey_sankey_df())
  }, options = list(scrollX = TRUE))
  
  output$journeyEventHeatmap <- renderPlot({
    # Facet bar charts between totals
    df <- journey_table_df() %>% select(-num_students) %>% gather(programme, count, -total) %>% filter(count>0)
    df <- df %>% filter(total %in% input$journeyGroup)
    
    # Filter out Journey Table data
    tags <- selection %>% filter(journey=="Y")
    #tags <- tags %>% filter(date !="Overarching Tag") %>% filter(date !="Unleash Space Master List") %>% filter(date !="") %>% filter(!is.na(date)) # Need to include these in then
    #tags <- tags %>% filter(!is.na(date))
    tags <- tags %>% select(`final_tags`, `date`)
    
    # Add date
    df <- merge(df, tags, by.x="programme", by.y="final_tags", all.x = TRUE) %>% distinct() 
    
    df %>% 
      ggplot(aes(reorder(programme,desc(date)), count, label=count)) +
      facet_wrap(total~., ncol=2, scale="free_y") +
      geom_bar(position = position_dodge2(width = 0.9, preserve = "single"), stat = "identity" ) +
      geom_text(hjust=0, position = position_dodge2(width = 0.9, preserve = "single")) +
      coord_flip() +
      ggtitle("Total split") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.background = element_rect(fill="grey97", colour = "white")) + 
      guides(colour=FALSE) + labs(y="", x = "") +
      #theme(axis.text.x = element_text(angle = -20, vjust=1) , panel.background = element_rect(fill="grey99", colour="grey99")) +
      scale_fill_tableau() + scale_colour_tableau()
  })
  
  observeEvent(input$updateTotal, {
    updatePickerInput(session, "journeyGroup", selected = journey_table_df()$total[1], choices = sort(unique(journey_table_df()$total)))
  })
  
  output$journeyIndividualHeatmap <- renderPlot({
    if (input$baseSource != "") {
      df <- journey_map_df() %>% mutate(programme=paste(date,programme))
    } else {
      df <- journey_map_df() %>% mutate(programme=if_else(programme!=input$baseDestination,paste(date,programme), paste("Destination: ", programme))) 
    }
    
    df <- df %>% complete(programme=unique(programme), ID=unique(ID)) %>% distinct()
    
    if (dim(df[is.na(df$count),])[1] != 0) {
      df[is.na(df$count),]["count"] <- 0 #Replace NAs with 0
    }
    
    df %>%  
      ggplot(aes(ID, fct_rev(programme))) + geom_tile(aes(fill=count)) +
      guides(color=FALSE, fill=FALSE) +
      scale_color_manual(guide = FALSE, values = c("black", "white")) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        #panel.grid.major = element_rect(fill="grey97"),
        panel.background = element_rect(fill="grey99")
      ) +
      labs(x="", y="")
  })
  
  output$journeySankey <- renderSankeyNetwork({
    df <- journey_sankey_df()
    # Add Node names
    nodes <- data.frame(name=c(as.character(df$source.programme), as.character(df$target.programme)) %>% unique())
    df$ID1 <- match(df$source.programme, nodes$name) - 1
    df$ID2 <- match(df$target.programme, nodes$name) - 1
    
    sankeyNetwork(Links = df, Nodes=nodes, Source = "ID1", "ID2", "count", NodeID = "name", nodePadding = 30, fontSize = 10)
  })
  #}) # End
}
