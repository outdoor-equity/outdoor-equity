# server instructions ----
server <- function(input, output){
  
  # filter agency ----
  length_of_stay_df <- reactive({
    
    data_hist_length_of_stay %>% filter(agency %in% input$agency)
  })

  # render scatterplot ----
  output$agency_scat_col <- renderPlot({
    
    ## -- create plot -- ##

    # parameters
    hist_colors <- c("#009900FF")

    # plot for shiny app
    ggplot(data = length_of_stay_df()) +
      geom_histogram(aes_string(x = input$agency_hist_vars),
                     fill = hist_colors,
                     bins = 29) +
      theme_minimal()
  })

}