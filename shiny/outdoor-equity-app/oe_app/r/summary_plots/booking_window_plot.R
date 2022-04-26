## booking window plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

booking_window_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame
  booking_window_rdf <- reactive({
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      filter(booking_window > 0) %>% 
      select(agency, admin_unit, park, booking_window)
  })

  # wrangling
  x_max <- (round(max(booking_window_plot()$booking_window) / 5) * 5) + 5 # max x rounded to nearest 5
   
  # parameters
  hist_colors <- c("#009900FF")
  
  # plot for shiny app
  booking_window_plotly <- ggplot(data = booking_window_rdf()) +
    geom_histogram(aes(x = booking_window,
                       text = paste(scales::percent(..count.. / nrow(dist_travel_rdf()), accuracy = 0.1), 
                                    " of all reservations book their trip between ", xmin, " and ", xmax, 
                                    "<br>days before the start of the trip",
                                    "<br>(", scales::comma(..count.., accuracy = 1), " reservations)")),
                   binwidth = 7,
                   center = 7 / 2,
                   fill = hist_colors) +
    labs(x = "Days elapsed from order to visit (each bar = 1 week)",
         y = "",
         title = paste0("Number of Days Before a Visit that Reservations are Booked", "<br>for ", 
                       siteInput, ", ", admin_unitInput)) +
    scale_x_continuous(limits = c(0, x_max)) +
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
  
  ggplotly(booking_window_plotly,
           tooltip = list("text"))
  
} # EO function