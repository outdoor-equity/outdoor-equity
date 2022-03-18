# server instructions ----
server <- function(input, output){
  
  # filter agency ----
  distance_traveled_df <- reactive({
    
    data_hist_distance_traveled %>% filter(agency %in% input$agency)
  })

  # render scatterplot ----
    output$agency_hist_dist_travel <- renderPlot({
  
      # parameters
      hist_colors <- c("#009900FF")
      
      # plot for shiny app
      if(input$agency_hist_vars != ""){
        ggplot(data = distance_traveled_df()) +
          geom_histogram(aes_string(x = !!input$agency_hist_vars),
                         fill = hist_colors) +
          theme_minimal() +
          theme(plot.background = element_rect("white"),
                panel.grid.major.y = element_blank())
      } # end of if input$agency_hist_vars statement 
    })

}