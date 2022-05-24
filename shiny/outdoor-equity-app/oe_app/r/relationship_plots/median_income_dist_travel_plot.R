
#' Median-income x Distance Traveled Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param median_income_decile_df Object name for dataframe of all reservations split into median-income deciles
#'
#' @return Plotly of median-income categories compared to distance traveled
#'
#' @examples

median_income_dist_travel_plot <- function(admin_unitInput, 
                                           siteInput, 
                                           median_income_decile_df){
  
  # create reactive dataframe and further subset
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    median_income_decile_df %>%
      # filter to user site of choice
      filter(park == siteInput) %>%
      # select to variables of interest
      select(park, customer_zip, median_income_binned, distance_traveled_mi) %>% 
      drop_na(median_income_binned) %>% 
      # summarize to inner quartile range, median, and total reservations
      group_by(median_income_binned) %>% 
      summarize(median_distance_traveled_mi = median(distance_traveled_mi),
                quartile_lower = quantile(distance_traveled_mi)[[2]],
                quartile_upper = quantile(distance_traveled_mi)[[4]],
                count = n())
  }) # EO rdf
  
  # create plot
  plotly <- ggplot(data = rdf(), 
                   aes(x = median_distance_traveled_mi,
                       y = median_income_binned)) +
    geom_segment(aes(xend = 0, yend = median_income_binned)) +
    geom_point(aes(color = median_income_binned, fill = median_income_binned,
                   text = paste0(comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes with a<br>median household income between ",
                                 median_income_binned, 
                                 ". Typically these visitors<br>traveled between ",
                                 comma(quartile_lower, accuracy = 1), 
                                 " and ", comma(quartile_upper, accuracy = 1), 
                                 " miles, with a median distance of ", 
                                 comma(median_distance_traveled_mi, accuracy = 1), 
                                 " miles.")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
    labs(x = paste("Estimated Distance Traveled from Home to Site (miles)"),
         y = "") + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none")
  
  # create plotly
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Distance Traveled by Visitors with Different Median Household Incomes'),
                        font = list(size = 15)))
  
} # EO function