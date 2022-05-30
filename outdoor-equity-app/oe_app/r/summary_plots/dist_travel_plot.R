
#' Distance Traveled Data Summary Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#'
#' @return Plotly of distance traveled
#'
#' @examples
dist_travel_plot <- function(admin_unitInput, siteInput, ridb_df){
  
  # reactive data frame 
  dist_travel_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      filter(park %in% siteInput) %>%
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>%
      select(park, distance_traveled_mi) %>% 
      filter(!is.na(distance_traveled_mi))
    
  })
  
  # wrangling
  x_max <- (round(max(dist_travel_rdf()$distance_traveled_mi) / 5) * 5) + 5 # max x rounded to nearest 5
  
  center_bin <-
    if (max(dist_travel_rdf()$distance_traveled_mi) > 100) {
      (round((max(dist_travel_rdf()$distance_traveled_mi) / 100) / 5) * 5) + 5
    } else if (max(dist_travel_rdf()$distance_traveled_mi) > 10) {
      (round((max(dist_travel_rdf()$distance_traveled_mi) / 10) / 5) * 5) + 5
    } else {
      0.5
    }
  
  quant_80 <- quantile(x = dist_travel_rdf()$distance_traveled_mi,
                       probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  
  
  # parameters
  hist_colors <- c("#64863C", "#466C04")
  quant_80_color <- c("#FACE00")
  caption_color <- c("#ac8d00")
  
  # plot for shiny app
  dist_travel_plotly <- ggplot(data = dist_travel_rdf()) +
    geom_histogram(aes(x = distance_traveled_mi,
                       text = paste0(percent(..count.. / nrow(dist_travel_rdf()), accuracy = 0.1), 
                                     " of all reservations traveled between ", comma(xmin, accuracy = 1), " and ", 
                                     comma(xmax, accuracy = 1), " miles", 
                                     "<br>",
                                     "(Total reservations to site: ",
                                     comma(nrow(dist_travel_rdf()), accuracy = 1), ")")),
                   binwidth = center_bin * 2,
                   center = center_bin,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    scale_x_continuous(limits = c(0, x_max)) +
    geom_vline(xintercept = quant_80,
               linetype = "dashed", alpha = 0.5, color = quant_80_color) +
    labs(x = "Distance traveled (miles)",
         y = "") +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(dist_travel_plotly,
           tooltip = list("text"),
           dynamicTicks = TRUE) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Distance Traveled from Home to Reservation'),
                        font = list(size = 15)),
                        margin = list(b = 130, t = 100), 
           xaxis = list(separatethousands = TRUE),
           yaxis = list(separatethousands = TRUE),
           annotations =  list(x = x_max/2, y = -0.6, 
                               text = paste0("80% of reservations to traveled less than ", '<b>', quant_80, '</b>', " miles", 
                                             "<br>", 
                                             "(shown on plot with blue dotted line)."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'middle', yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = caption_color))) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian"))
  
  
} # EO function