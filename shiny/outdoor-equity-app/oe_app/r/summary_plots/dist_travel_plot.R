## distance traveled plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui
# agencyInput, admin_unitInput,

#' Title
#'
#' @param admin_unitInput 
#' @param siteInput 
#'
#' @return
#'
#' @examples
dist_travel_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  dist_travel_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>%
      select(park, distance_traveled_mi) %>% 
      filter(!is.na(distance_traveled_mi))
    
  })
  
  print(class(dist_travel_rdf))
  
  # wrangling
  x_max <- (round(max(dist_travel_rdf()$distance_traveled_mi) / 5) * 5) + 5 # max x rounded to nearest 5
  
  center_bin <-
    if (max(dist_travel_rdf()$distance_traveled_mi) > 100) {
      round((max(dist_travel_rdf()$distance_traveled_mi) / 100) / 5) * 5
    } else if (max(dist_travel_rdf()$distance_traveled_mi) > 10) {
      round((max(dist_travel_rdf()$distance_traveled_mi) / 10) / 5) * 5
    } else {
      0.5
    }
  
  print(paste("the class of center bin is", class(center_bin)))
  
  print(center_bin)
  
  quant_80 <- quantile(x = dist_travel_rdf()$distance_traveled_mi,
                       probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  
  print(paste("the class of quant_80 bin is", class(quant_80)))
  
  print(quant_80)
  
  
  # parameters
  hist_colors <- c("#009900FF", "#00c000")
  
  # plot for shiny app
  dist_travel_plotly <- ggplot(data = dist_travel_rdf()) +
    geom_histogram(aes(x = distance_traveled_mi,
                       text = paste0(percent(..count.. / nrow(dist_travel_rdf()), accuracy = 0.1), 
                                     " of all reservations traveled between ", comma(xmin, accuracy = 1), " and ", 
                                     comma(xmax, accuracy = 1), " miles", "<br>(All reservations to site: ",
                                     comma(nrow(dist_travel_rdf()), accuracy = 1), ")")),
                   binwidth = center_bin * 2,
                   center = center_bin,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    scale_x_continuous(limits = c(0, x_max)) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept = quant_80,
               linetype = "dashed", alpha = 0.5, color = "#000099") +
    labs(x = "Distance traveled (miles)",
         y = "",
         title = paste0("Distance Traveled from Home to Reservation for <br>", 
                        siteInput, ", ", admin_unitInput, " in 2018")) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(dist_travel_plotly,
           tooltip = list("text")) %>% 
    layout(margin = list(b = 130, t = 100), 
           annotations =  list(x = 1, 
                               y = -0.5, 
                               text = paste0("80% of reservations to ", siteInput, ", ", admin_unitInput, 
                                             "<br>traveled less than ", quant_80, " miles (shown on plot with dotted line)."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'left', 
                               yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = "#000099")))
  
  
} # EO function