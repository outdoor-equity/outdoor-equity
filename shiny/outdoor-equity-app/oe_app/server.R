# server instructions ----
server <- function(input, output, session){
  
# SO OE press agency ----
## SO press agency on summary page ----
observeEvent(input$agency_summary, {

  # Note(HD): these print statements are temporary 
  print(paste0("You have chosen: ", input$agency_summary))
  print(class(input$agency_summary))

  # create choices based on dictionary
  choices <- vector()
  
  for (i in seq_along(input$agency_summary)){
    
    choices <- append(choices, 
                      agency_to_admin_unit_dict$get(input$agency_summary[[i]]))
  }

  # update input with new choices
  updateSelectizeInput(session, "admin_unit_summary",
                       choices = sort(choices)
  ) # EO update choices 

}) ## EO OE press agency on summary page

## SO press agency on relationships page ----
observeEvent(input$agency_relationships, {
  
  print(paste0("You have chosen: ", input$agency_relationships))
  print(class(input$agency_relationships))
  
  choices <- vector()
  
  for (i in seq_along(input$agency_relationships)){
    
    choices <- append(choices,
                      agency_to_admin_unit_dict$get(input$agency_relationships[[i]]))
  }
  
  updateSelectizeInput(session, "admin_unit_relationships",
                       choices = sort(choices)
  )
  
}) ## EO OE press agency on relationships page

## SO press agency on visitorsheds page ----
observeEvent(input$agency_visitorsheds, {

  print(paste0("You have chosen: ", input$agency_visitorsheds))
  print(class(input$agency_visitorsheds))

  choices <- vector()

  for (i in seq_along(input$agency_visitorsheds)){

    choices <- append(choices,
                      agency_to_admin_unit_dict$get(input$agency_visitorsheds[[i]]))
  }

  updateSelectizeInput(session, "admin_unit_visitorsheds",
                       choices = sort(choices)
  )

}) ## EO OE press agency on visitorsheds page

## SO press agency on data download page ----
observeEvent(input$agency_data_download, {
  
  print(paste0("You have chosen: ", input$agency_data_download))
  print(class(input$agency_data_download))
  
  choices <- vector()
  
  for (i in seq_along(input$agency_data_download)){
    
    choices <- append(choices,
                      agency_to_admin_unit_dict$get(input$agency_data_download[[i]]))
  }
  
  updateSelectizeInput(session, "admin_unit_data_download",
                       choices = sort(choices)
  )
  
}) ## EO press agency on data download page
  
  
# SO OE press admin unit ----
## SO press admin unit on summary page ----
observeEvent(input$admin_unit_summary, {
  
  print(paste0("You have chosen: ", input$admin_unit_summary))
  print(class(input$admin_unit_summary))
  
  choices <- vector()
  
  for (i in seq_along(input$admin_unit_summary)){
    
    choices <- append(choices,
                      admin_units_to_site_dict$get(input$admin_unit_summary[i]))
  }
  
  updateSelectizeInput(session, "site_summary",
                       choices = sort(choices)
  )
  
}) ## EO press admin unit on summary page

## SO press admin unit on relationships page ----
observeEvent(input$admin_unit_relationships, {

  print(paste0("You have chosen: ", input$admin_unit_relationships))
  print(class(input$admin_unit_relationships))

  choices <- vector()

  for (i in seq_along(input$admin_unit_relationships)){

    choices <- append(choices,
                      admin_units_to_site_dict$get(input$admin_unit_relationships[[i]]))
  }

  updateSelectizeInput(session, "site_relationships",
                       choices = sort(choices)
                       )
  }) ## EO press admin unit on relationships page
  
## SO press admin unit on visitorsheds page ----
# observeEvent(input$admin_unit_visitorsheds, {
#   
#   print(paste0("You have chosen: ", input$admin_unit_visitorsheds))
#   print(class(input$admin_unit_visitorsheds))
#   
#   choices <- vector()
#   
#   for (i in seq_along(input$admin_unit_visitorsheds)){
#     
#     choices <- append(choices,
#                       admin_units_to_site_dict$get(input$admin_unit_visitorsheds[[i]]))
#     }
#   
#   updateSelectizeInput(session, "site_visitorsheds",
#                        choices = sort(choices)
#                        )
#   }) ## EO press admin unit on visitorsheds page

# ## SO press admin unit on data download page ----
observeEvent(input$admin_unit_data_download, {

  print(paste0("You have chosen: ", input$admin_unit_data_download))
  print(class(input$admin_unit_data_download))

  choices <- vector()

  for (i in seq_along(input$admin_unit_data_download)){

    choices <- append(choices,
                      admin_units_to_site_dict$get(input$admin_unit_data_download[[i]]))
  }

  updateSelectizeInput(session, "site_data_download",
                       choices = sort(choices)
  )

}) ## EO press admin unit on data download page


# REACTIVE DATA FRAMES ----
## SO distance traveled RDF ----
dist_travel_rdf <- reactive ({
  
  #dist_travel_rdf()
  data_joined_2018 %>%
    filter(agency %in% input$agency_summary,
           admin_unit %in% input$admin_unit_summary,
           park %in% input$site_summary) %>%
    mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>%
    select(agency, admin_unit, park, distance_traveled_mi)

}) # EO distance traveled RDF

## race RDF ----
race_df <- reactive({
  
  data_joined_2018 %>% 
    filter(agency %in% input$agency_summary,
           admin_unit %in% input$admin_unit_summary,
           park %in% input$site_summary) %>% 
    summarize(white = (mean(white, na.rm = TRUE) * 100),
              black = (mean(black, na.rm = TRUE) * 100),
              asian = (mean(asian, na.rm = TRUE) * 100),
              multiracial = (mean(multiracial, na.rm = TRUE) * 100),
              other = (mean(other, na.rm = TRUE) * 100),
              native_american = (mean(native_american, na.rm = TRUE) * 100),
              pacific_islander = (mean(pacific_islander, na.rm = TRUE) * 100),
              hispanic_latinx = (mean(hispanic_latinx, na.rm = TRUE) * 100)) %>%
    pivot_longer(cols = 1:8, names_to = "race", values_to = "race_percent_average") %>% 
    mutate(race = str_replace(string = race,
                              pattern = "_",
                              replacement = " "),
           race = str_to_title(race))
})
  
## SO booking window RDF ----
booking_window_df <- reactive({
  
  data_joined_2018 %>%
    filter(agency %in% input$agency_summary,
           admin_unit %in% input$admin_unit_summary,
           park %in% input$site_summary) %>%
    filter(booking_window > 0,
           booking_window < 510) %>% 
    select(agency, admin_unit, park, booking_window)
}) ## EO booking window RDF 

# RENDER PLOTS ----

## data_relationships_plots NO REACTIVE
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
          panel.grid.major.x = element_blank()) +
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

## SO DATA SUMMARY PLOTS ----
## IMPORTANT NOTE(HD): MAY NEED TO CHAGNE FROM RENDER PLOT TO RENDER PLOTLY----
output$data_summary_plot <- renderPlot({
  ### SO distance traveled ----
  if (input$data_summary == "distance_traveled_mi") {
    
    # parameters
    hist_colors <- c("#009900FF")
    # plot for shiny app
    ggplot(data = dist_travel_rdf()) +
      geom_histogram(aes(x = distance_traveled_mi),
                     fill = hist_colors) +
      scale_y_continuous(labels = comma) +
      labs(x = "Distance traveled (miles)",
           y = "",
           title = paste("Distribution of Distance Traveled to", input$site_summary),
           subtitle = "Overnight Reservations in California in 2018") +
      scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500), minor_breaks = seq(0, 3000, 250)) +
      theme_minimal() +
      theme(plot.background = element_rect("white"),
            panel.grid.major.y = element_blank())
    
  } ## EO if distance traveled
  
  ## SO booking window ----
  else if (input$data_summary == "booking_window") {
    
    # parameters
    hist_colors <- c("#009900FF")
    
    # plot for shiny app
    ggplot(data = booking_window_df()) +
      geom_histogram(aes(x = booking_window),
                     binwidth = 7,
                     fill = hist_colors) +
      labs(x = "Days elapsed from order to visit (each bar = 1 week)",
           y = "",
           title = paste("Distribution of Booking Windows for", input$site_summary),
           subtitle = "Overnight Reservations in California in 2018") +
      scale_x_continuous(limits = c(0, 510), 
                         breaks = seq(0, 510, by = 30)) +
      scale_y_continuous(labels = comma) +
      geom_vline(xintercept = 180, 
                 linetype = "dashed", size = .3, alpha = .5) +
      annotate("text", label = "6 months", 
               x = 210, y = 65000) +
      geom_vline(xintercept = 360, 
                 linetype = "dashed", size = .3, alpha = .5) +
      annotate("text", label = "1 year", 
               x = 380, y = 65000) +
      theme_minimal() +
      theme(plot.background = element_rect("white"),
            panel.grid.major.y = element_blank())
    
  } ### EO else if booking window
  
  
}) ## EO data_summary_plot


} ## EO server


## race plot
# else if(input$data_summary == "Race"){
#   
#   ggplot(data = race_df()) +
#     geom_col(aes(x = race_percent_average,
#                  y = reorder(race, race_percent_average)),
#              stat = "identity") +
#     scale_fill_manual(values = groups_colors_ridb_ca) +  
#     geom_text(aes(x = race_percent_average,
#                   y = reorder(race, race_percent_average),
#                   label = paste0(round(race_percent_average, 1), "%")), 
#               position = position_dodge(width = 1), 
#               hjust = -0.1, size = 4) +
#     scale_color_manual(values = groups_colors_ridb_ca) +
#     labs(x = "Percentage (%)",
#          y = "",
#          title = "Racial Breakdown of ZIP Codes in 2018",
#          subtitle = "Visitors' home ZIP codes for Overnight Reservations in California \nvs. California Residents") +
#     scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10), minor_breaks = seq(0, 60, 5)) +
#     theme_minimal() +
#     theme(plot.background = element_rect("white"),
#           panel.grid.major.y = element_blank()
#     )
#   
#   
# } ## EO of race plot