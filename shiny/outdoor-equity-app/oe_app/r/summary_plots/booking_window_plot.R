## booking window plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

booking_window_plot <- function(agencyInput, admin_unitInput, siteInput, titleInput){
  
  # reactive data frame
  booking_window_rdf <- reactive({
    
    data_joined_2018 %>%
      filter(agency %in% agencyInput,
             admin_unit %in% admin_unitInput,
             park %in% siteInput) %>%
      filter(booking_window > 0,
             booking_window < 510) %>% 
      select(agency, admin_unit, park, booking_window)
  })
    
  # parameters
  hist_colors <- c("#009900FF")
  
  # plot for shiny app
  ggplot(data = booking_window_rdf()) +
    geom_histogram(aes(x = booking_window),
                   binwidth = 7,
                   fill = hist_colors) +
    labs(x = "Days elapsed from order to visit (each bar = 1 week)",
         y = "",
         title = paste("Distribution of Booking Windows for", titleInput),
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
  
} # EO function