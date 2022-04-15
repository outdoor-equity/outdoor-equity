## daily cost per visitor plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

daily_cost_visitor_plot <- function(agencyInput, admin_unitInput, siteInput, titleInput){
  
  # reactive data frame 
  daily_cost_visitor_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(agency %in% agencyInput,
             admin_unit %in% admin_unitInput,
             park %in% siteInput) %>%
      filter(daily_cost_per_visitor != "Inf",
             daily_cost_per_visitor <= 50) %>% 
      select(agency, admin_unit, park, daily_cost_per_visitor)
    
  })
  
  # parameters
  hist_colors <- c("#009900FF")
  
  # plot for shiny app
  ggplot(data = daily_cost_visitor_rdf()) +
    geom_histogram(aes(x = daily_cost_per_visitor),
                   fill = hist_colors) +
    labs(x = "Daily cost per visitor ($)",
         y = "",
         title = paste("Distribution of Daily Costs per Visitor for", titleInput),
         subtitle = "Overnight Reservations in California in 2018") +
    scale_x_continuous(limits = c(0, 50), breaks = seq(0, 50, by = 10)) +
    scale_y_continuous(labels = comma) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  
} # EO function