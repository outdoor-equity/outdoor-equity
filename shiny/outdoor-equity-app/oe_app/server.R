
# server instructions ----
server <- function(input, output, session){
  
# observeEvent press agency
  observeEvent(input$agency, {
    
    print(paste0("You have chosen: ", input$agency))
    
    test <- data_joined_2018 %>%
      filter(agency %in% input$agency)
    
    admin_test <- as.vector(unique(test$regional_area))
    
    updateSelectizeInput(session, "admin_unit",
                         choices = admin_test
    )
    
  })

  ## REACTIVE DATA FRAMES ----
  
  ### data download ----
  # data_download_df <- reactive({
  #   data_joined_2018 %>%
  #     filter(agency %in% input$agency,
  #            regional_area %in% input$admin_unit,
  #            park %in% input$site)
  # 
  # })
  # 
  ### dist traveled df ----
  # distance_traveled_df <- reactive({
  #   data_hist_distance_traveled %>% filter(agency %in% input$agency)
  # })
  # 
  # ### race df ----
  # # NEED TO ADD AGENCY IF WE WANT IT TO BE REACTIVE 
  # # race_df <- reactive({
  # #   data_hist_race %>% filter(agency %in% input$agency)
  # # })
  # 
  # 
  # 
  # ## RENDERING OUTPUTS  ----
  # 
  # #### data download ----
  # # Note(HD) Data too large to put in DT table?
  # # output$data_download <- DT:renderDataTable({
  # #   DT::datatable(data_download_df())
  # #   
  # # })
  # # 
  # #### dist traveled hist ----
  # output$agency_analysis <- renderPlot({
  #   
  #   # if statement for dist traveled
  #   if(input$agency_hist_vars == "distance_traveled_mi"){
  #     
  #     hist_colors <- c("#009900FF")
  #     
      # plot for shiny app
      # ggplot(data = distance_traveled_df()) +
      #   geom_histogram(aes(x = distance_traveled_mi),
      #                  fill = hist_colors) +
      #   scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500),
      #                      minor_breaks = seq(0, 3000, 250)) +
      #   labs(x = "Distance traveled (miles)",
      #        y = "") +
      #   theme_minimal() +
      #   theme(plot.background = element_rect("white"),
      #         panel.grid.major.y = element_blank())
  #     
  #   } # end of if statement for dist traveled
  #   
  #   # else if for race hist
  #   else if(input$agency_hist_vars == "race"){
  #     
  #     # parameters
  #     groups_colors_ridb_ca <- c("RIDB" = "#009900FF", "CA" = "#666666")
  #     
  #     ggplot(data = data_hist_race) +
  #       geom_col(aes(x = race_percent_average,
  #                    y = race,
  #                    fill = data_source),
  #                stat = "identity",
  #                position = "dodge") +
  #       scale_fill_manual(values = groups_colors_ridb_ca) +  
  #       geom_text(aes(x = race_percent_average,
  #                     y = race,
  #                     label = paste0(round(race_percent_average, 1), "%"),
  #                     col = data_source), 
  #                 position = position_dodge(width = 1), 
  #                 hjust = -0.1, size = 4) +
  #       scale_color_manual(values = groups_colors_ridb_ca) +
  #       labs(x = "Percentage (%)",
  #            y = "") +
  #       scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10), 
  #                          minor_breaks = seq(0, 60, 5)) +
  #       theme_minimal() +
  #       theme(plot.background = element_rect("white"),
  #             panel.grid.major.y = element_blank())
  #     
  #   } # end of race plot
  #   
  #   # render dist traveled x race column comp
  # 
  #   # if statement for dist traveled x race
  #   if(input$scat_ridb_vars == "race"){
  # 
  #     # parameters
  #     racial_group_colors <- c("Other" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
  #                              "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
  #                              "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  #     
  #     # plot for shiny
  #     ggplot(data = data_comp_dist_travel_race) +
  #       geom_col(aes(x = distance_traveled_bins,
  #                    y = race_percentage,
  #                    fill = race),
  #                stat = "identity",
  #                position = "dodge")  +
  #       scale_fill_manual(values = racial_group_colors) +
  #       labs(y = "Distance Traveled (miles)",
  #            x = "Percentage (%)",
  #            caption = "Distance Travel Quintiles: 1 = less than 60.2 (with the lowest being 0.5), 2 = 60.2 - 116.9, 3 = 116.7 - 180.9, 4 = 180.9 - 229.3, 5 = above 229.3 (with the highest being 3613.7") +
  #       theme_minimal() +
  #       theme(plot.background = element_rect("white"),
  #             panel.grid.major.y = element_blank())
  #     
  #   } # end of if statement for dist traveled x race column comp
  # 
  #   
  # 
  #   
  #   
  # })
  # 
  ### OLD render hist ----
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

} # EO server 