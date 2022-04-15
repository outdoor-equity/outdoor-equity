## distance traveled plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

dist_travel_plot <- function(agencyInput, admin_unitInput, siteInput, titleInput){
  
  # reactive data frame 
  dist_travel_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(agency %in% agencyInput,
             admin_unit %in% admin_unitInput,
             park %in% siteInput) %>%
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
      select(agency, admin_unit, park, distance_traveled_mi)
    
  })

  # parameters
  hist_colors <- c("#009900FF")
  # plot for shiny app
  ggplot(data = dist_travel_rdf()) +
    geom_histogram(aes(x = distance_traveled_mi),
                   fill = hist_colors) +
    scale_y_continuous(labels = comma) +
    labs(x = "Distance traveled (miles)",
         y = "",
         title = paste("Distribution of Distance Traveled to", titleInput),
         subtitle = "Overnight Reservations in California in 2018") +
    scale_x_continuous(limits = c(0, 3000), breaks = seq(0, 3000, 500), minor_breaks = seq(0, 3000, 250)) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank()
    )

  
} # EO function