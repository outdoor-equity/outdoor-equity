## site type plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

site_type_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  site_type_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      select(agency, admin_unit, park, aggregated_site_type)
    
  })
  
  # parameters
  hist_colors <- c("#009900FF")
  
  # plot for shiny app
  ggplot(data = site_type_rdf() %>%
           group_by(aggregated_site_type) %>%
           summarise(count = n())) +
    geom_col(aes(x = count,
                 y = reorder(aggregated_site_type, count)),
             fill = hist_colors) +
    scale_x_continuous(labels = comma) +
    labs(x = "Number of Sites",
         y = "Types of Sites",
         title = paste0("Distribution of Type of Sites for ", admin_unitInput, ", ", siteInput),
         subtitle = "Overnight Reservations in California in 2018") +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
} # EO function