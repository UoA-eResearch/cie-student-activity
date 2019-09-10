## server.R ##
## CIE Dashboards

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Reactivate dataframes
  infoOverview_r <- reactive({
    # Apply inputs as filter criteria
    df <- overviewData %>% 
      filter(year == input$baseYear)
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
      print(repeat_participant)
      infoBox(
        "Repeat participant", #m vlaue here, icon= icon(), color=
        repeat_participant
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
      
}