# server instructions ----
server <- function(input, output){
  
  ## REACTIVE DATA FRAMES ## ----
  # dist traveled df ----
  distance_traveled_df <- reactive({
    data_hist_distance_traveled %>% filter(agency %in% input$agency)
  })
  
  # race df ----
  race_df <- reactive({
    data_hist_race %>% filter(agency %in% input$agency)
  })

  
  
  ## RENDERING PLOTS ## ----
  # render dist traveled hist ----
  output$agency_analysis <- renderPlot({
    
    # if statement for dist traveled ----
    if(input$agency_hist_vars == "distance_traveled_mi"){
      
      hist_colors <- c("#009900FF")
      
      # plot for shiny app
      ggplot(data = data_hist_distance_traveled) +
        geom_histogram(aes(x = distance_traveled_mi),
                       fill = hist_colors) +
        scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500), 
                           minor_breaks = seq(0, 3000, 250)) +
        labs(x = "Distance traveled (miles)",
             y = "") +
        theme_minimal() +
        theme(plot.background = element_rect("white"),
              panel.grid.major.y = element_blank())
      
    } # end of if statement for dist traveled ----
    
    # else if for race hist ----
    else if(input$agency_hist_vars == "race"){
      
      
      
    }
  
    
    
  })
  
  
  ### OLD render hist ### ----
    # output$agency_hist_dist_travel <- renderPlot({
    #   # all if else statements live inside renderPlot{}
    # 
    #   # parameters
    #   hist_colors <- c("#009900FF")
    #   
    #   # plot for shiny app
    #   if(input$agency_hist_vars != ""){
    #     ggplot(data = distance_traveled_df()) +
    #       geom_histogram(aes_string(x = !!input$agency_hist_vars),
    #                      fill = hist_colors) +
    #       theme_minimal() +
    #       theme(plot.background = element_rect("white"),
    #             panel.grid.major.y = element_blank())
    #   } # end of if input$agency_hist_vars statement 
    # })

}