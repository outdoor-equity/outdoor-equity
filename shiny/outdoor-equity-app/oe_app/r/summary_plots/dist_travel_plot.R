## distance traveled plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

dist_travel_plot <- function(agencyInput, admin_unitInput, siteInput){
  
  # reactive data frame 
  dist_travel_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(agency %in% agencyInput,
             admin_unit %in% admin_unitInput,
             park %in% siteInput) %>%
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
      select(agency, admin_unit, park, distance_traveled_mi) %>% 
      filter(!is.na(distance_traveled_mi))
    
  })

  # parameters
  hist_colors <- c("#009900FF", "#00c000")
  
  x_max <- (round(max(dist_travel_rdf$distance_traveled_mi) / 5) * 5) + 5 # max x rounded to nearest 5
  center_bin <- 
    if (max(dist_travel_rdf$distance_traveled_mi) > 100) {
      round((max(dist_travel_rdf$distance_traveled_mi) / 100) / 5) * 5
    } else if (max(dist_travel_rdf$distance_traveled_mi) > 10) {
      round((max(dist_travel_rdf$distance_traveled_mi) / 10) / 5) * 5
    } else {
      0.5
    }
  
  quant_80 <- quantile(x = dist_travel_rdf$distance_traveled_mi,
                       probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  split_all <- data.frame(table(cut(x = dist_travel_rdf$distance_traveled_mi, 
                                    breaks = seq(0, 
                                                 x_max,
                                                 center_bin * 2))))
  
  # plot for shiny app
  dist_travel_plot <- ggplot(data = dist_travel_rdf()) +
    geom_histogram(aes(x = distance_traveled_mi,
                       text = paste0(scales::percent(..count.. / nrow(data_plot_distance_traveled_all), accuracy = 0.1), 
                                     " of all reservations traveled between ", xmin, " and ", xmax, " miles",
                                     "<br>(", scales::comma(..count.., accuracy = 1), " reservations)")),
                   binwidth = center_bin * 2,
                   center = center_bin,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    scale_x_continuous(limits = c(0, x_max)) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept = quant_80,
               linetype = "dashed", alpha = 0.5, color = "darkred") +
    labs(x = "Distance traveled (miles)",
         y = "",
         title = paste0("Distance Traveled to Reservations for <br>", siteInput, ", ", admin_unitInput, " in 2018")) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank()
    )
  
  ggplotly(dist_travel_plot,
           tooltip = list("text")) %>% 
    layout(margin = list(b = 130, t = 100), 
           annotations =  list(x = 1, 
                               y = -0.35, 
                               text = paste0("80% of reservations to California overnight sites in 2018 traveled less than ", 
                                             quant_80, " miles."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'left', 
                               yancho = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = "darkred")))

  
} # EO function