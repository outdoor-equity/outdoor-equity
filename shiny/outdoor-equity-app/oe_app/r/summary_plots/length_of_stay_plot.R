## length of stay plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

length_of_stay_plot <- function(agencyInput, admin_unitInput, siteInput, titleInput){
  
  # reactive data frame 
  length_of_stay_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(agency %in% agencyInput,
             admin_unit %in% admin_unitInput,
             park %in% siteInput) %>%
      select(agency, admin_unit, park, length_of_stay)
    
  })
  
  # parameters
  hist_colors <- c("#009900FF")
  
  # plot for shiny app
  ggplot(data = length_of_stay_rdf()) +
    geom_histogram(aes(x = length_of_stay),
                   binwidth = 1,
                   fill = hist_colors) +
    labs(x = "Length of stay (days)",
         y = "",
         title = paste("Distribution of Length of Stay for", titleInput),
         subtitle = "Overnight Reservations in California in 2018") +
    #scale_x_continuous(breaks = seq(0, 20, by = 1)) +
    scale_y_continuous(labels = comma) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
} # EO function