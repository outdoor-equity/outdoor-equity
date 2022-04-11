
# agency_to_admin_unit_dict
# admin_units_to_site_dict

# server instructions ----
server <- function(input, output, session){
  
# OE press agency 
observeEvent(input$agency_summary, {

  print(paste0("You have chosen: ", input$agency_summary))
  print(class(input$agency_summary))

  choices <- vector()
  
  for (i in seq_along(input$agency_summary)){
    
    choices <- append(choices, agency_to_admin_unit_dict$get(input$agency_summary[[i]]))

  }

  # Note(HD): alphabetical order of choices
  updateSelectizeInput(session, "admin_unit_summary",
                       choices = sort(choices)
  )

})

observeEvent(input$agency_relationships, {
  
  print(paste0("You have chosen: ", input$agency_relationships))
  
  test <- data_joined_2018 %>%
    filter(agency %in% input$agency_relationships)
  
  admin_test <- as.vector(unique(test$admin_unit))
  
  updateSelectizeInput(session, "admin_unit_relationships",
                       choices = admin_test
  )
  
})

observeEvent(input$agency_visitorsheds, {
  
  print(paste0("You have chosen: ", input$agency_visitorsheds))
  
  test <- data_joined_2018 %>%
    filter(agency %in% input$agency_visitorsheds)
  
  admin_test <- as.vector(unique(test$admin_unit))
  
  updateSelectizeInput(session, "admin_unit_visitorsheds",
                       choices = admin_test
  )
  
})

observeEvent(input$agency_data_download, {
  
  print(paste0("You have chosen: ", input$agency_data_download))
  
  test <- data_joined_2018 %>%
    filter(agency %in% input$agency_data_download)
  
  admin_test <- as.vector(unique(test$admin_unit))
  
  updateSelectizeInput(session, "admin_unit_data_download",
                       choices = admin_test
  )
  
})

# OE for admin unit -> site change ----
# empty dictionary with empty key value admin_unit
# reactive element bc writing an observeEvent for the selected object
selected_au <- reactiveValues(admin_unit = NULL)

observeEvent(input$admin_unit_summary, selected_au$admin_unit <- (input$admin_unit_summary))
observeEvent(input$admin_unit_relationships, selected_au$admin_unit <- (input$admin_unit_relationships))
observeEvent(input$admin_unit_visitorsheds, selected_au$admin_unit <- (input$admin_unit_visitorsheds))
observeEvent(input$admin_unit_data_download, selected_au$admin_unit <- (input$admin_unit_data_download))

# observeEvent press admin_unit
observeEvent(selected_au$admin_unit, {
  
  print(paste0("You have chosen: ", selected_au$admin_unit))
  
  au <- data_joined_2018 %>%
    filter(admin_unit %in% selected_au$admin_unit)
  
  park_test <- as.vector(unique(au$park))
  
  updateSelectizeInput(session, "site_summary",
                       choices = park_test
  )
  
})

observeEvent(selected_au$admin_unit, {
  
  print(paste0("You have chosen: ", selected_au$admin_unit))
  
  au <- data_joined_2018 %>%
    filter(admin_unit %in% selected_au$admin_unit)
  
  park_test <- as.vector(unique(au$park))
  
  updateSelectizeInput(session, "site_relationships",
                       choices = park_test
  )
  
})

observeEvent(selected_au$admin_unit, {
  
  print(paste0("You have chosen: ", selected_au$admin_unit))
  
  au <- data_joined_2018 %>%
    filter(admin_unit %in% selected_au$admin_unit)
  
  park_test <- as.vector(unique(au$park))
  
  updateSelectizeInput(session, "site_visitorsheds",
                       choices = park_test
  )
  
})

observeEvent(selected_au$admin_unit, {
  
  print(paste0("You have chosen: ", selected_au$admin_unit))
  
  au <- data_joined_2018 %>%
    filter(admin_unit %in% selected_au$admin_unit)
  
  park_test <- as.vector(unique(au$park))
  
  updateSelectizeInput(session, "site_data_download",
                       choices = park_test
  )
  
})
## EO OE agency, admin unit, and site ----

# REACTIVE DATA FRAMES ----

booking_window_df <- reactive({
  data_plot_boooking_window %>%
    filter(agency %in% input$agency_summary,
           admin_unit %in% input$admin_unit_summary,
           park %in% input$site_summary)

})

# RENDER PLOTS ----

## data_relationships_plot NO REACTIVE
output$data_relationships_plot <- renderPlot({
  
  # parameters
  racial_group_colors <- c("Other" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                           "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                           "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  # plot for shiny
  ggplot(data = data_race_dist_travel) +
    geom_col(aes(x = factor(distance_traveled_bins),
                 y = race_percentage,
                 fill = race)) +
    facet_wrap(~race, scales = "free_x") +
    scale_fill_manual(values = racial_group_colors) +
    scale_x_discrete(labels = c("1" = "0 - 60 mi", 
                                "2" = "60 - 117 mi", 
                                "3" = "117 - 181 mi", 
                                "4" = "181 - 299 mi", 
                                "5" = "299 - 3,614 mi")) + 
    labs(x = "Distance Traveled (miles)",
         y = "Percentage (%)",
         title = "Breakdown of Race of Home ZIP Codes vs. Distance Traveled",
         subtitle = "for Overnight Reservations in California") +
    theme_minimal() +
    theme(legend.position = "none",
          plot.background = element_rect("white"),
          panel.grid.major.x = element_blank()
          # ## only needed for saving as image
          # axis.text = element_text(size = 20),
          # axis.title = element_text(size = 22, face = "bold"),
          # title = element_text(size = 24, face = "bold")
    ) +
    coord_flip()
  
})


## visitorshed yosemite YES REACTIVE ----
output$caVisitorshed_plot <- renderTmap({
  
  tm_shape(data_zip_geometries_ca) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "PuRd",
            style = "jenks",
            n = 10, 
            popup.vars = c("Total Visits" = "number_reservations")) +
    tm_shape(data_yosemite_upper_pines_geom) +
    tm_dots(col = "#009900FF", size = 0.1, alpha = 0.9,
            id = "park") +
    tm_view(set.view = c(-119.559917, 37.061753, 6))
  
})


output$usVisitorshed_plot <- renderTmap({
  
  tm_shape(data_geometries_us) +
    tm_borders(col = "grey", alpha = 0.5) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "YlGn",
            n = 10, 
            style = "jenks",
            id = "zip_state_abbr", 
            popup.vars = c("Total Visits" = "number_reservations")) +
    tm_view(set.view = c(-101.834335, 40.022356, 3))
  
})

output$data_summary_plot <- renderPlot({

  # SO if statement for booking window
    if(input$data_summary == "Booking Window"){

      hist_colors <- c("#009900FF")

      # plot for shiny app
      ggplot(data = booking_window_df()) + # reactive df
        geom_histogram(aes(x = booking_window),
                       binwidth = 7,
                       fill = hist_colors) +
        labs(x = "Days elapsed from order to visit (each bar = 1 week)",
             y = "Number of reservations",
             title = paste("Booking Window for", input$agency_summary), # Note(HD) call name for multiple selections
             subtitle = "Overnight Reservations in California in 2018") +
        scale_x_continuous(limits = c(0, 510),
                           breaks = seq(0, 510, by = 30)) +
        scale_y_continuous(labels = comma) +
        geom_vline(xintercept = 180,
                   linetype = "dashed", size = .3, alpha = .5) +
        annotate("text", label = "6 months", # Note(HD) need to make y dynamic
                 x = 210, y = 10000) +
        geom_vline(xintercept = 360,
                   linetype = "dashed", size = .3, alpha = .5) +
        annotate("text", label = "1 year",
                 x = 380, y = 10000) + # Note(HD) need to make y dynamic
        theme_minimal() +
        theme(plot.background = element_rect("white"),
              panel.grid.major.y = element_blank())

    } ## EO if statement booking window

}) ## EO data_summary_plot booking window ----




  ## REACTIVE DATA FRAMES
  
  ### data download
  # data_download_df <- reactive({
  #   data_joined_2018 %>%
  #     filter(agency %in% input$agency,
  #            regional_area %in% input$admin_unit,
  #            park %in% input$site)
  # 
  # })
  # 
  ### dist traveled df
  # distance_traveled_df <- reactive({
  #   data_hist_distance_traveled %>% filter(agency %in% input$agency)
  # })
  # 
  # ### race df
  # # NEED TO ADD AGENCY IF WE WANT IT TO BE REACTIVE 
  # # race_df <- reactive({
  # #   data_hist_race %>% filter(agency %in% input$agency)
  # # })
  # 
  # 
  # 
  # ## RENDERING OUTPUTS
  # 
  # #### data download
  # # Note(HD) Data too large to put in DT table?
  # # output$data_download <- DT:renderDataTable({
  # #   DT::datatable(data_download_df())
  # #   
  # # })
  # # 
  # #### dist traveled hist
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
  ### OLD render hist
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