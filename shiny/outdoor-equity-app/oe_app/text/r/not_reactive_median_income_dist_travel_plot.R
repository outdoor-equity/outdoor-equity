
#' About Example: Median-income x Distance Traveled Plotly
#'
#' @param admin_unit User pick for admin unit
#' @param site User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param median_income_binned List of decile values
#'
#' @return Plotly of median-income categories compared to distance traveled
#'
#' @examples

not_reactive_median_income_dist_travel_plot <- function(admin_unit, site, ridb_df, median_income_binned){
  
  # categorize and summarize data to median-income decile groups
  plot_data <- not_reactive_median_income_dist_travel_data(ridb_df = ridb_df, site = site,
                                              median_income_binned = median_income_binned)
  
  # create plot
  plotly <- ggplot(data = plot_data, 
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
    layout(title = list(text = paste0('<b>', site, '<br>', admin_unit, '</b>',
                                      '<br>',
                                      'Distance Traveled by Visitors with Different Median Household Incomes'),
                        font = list(size = 15)))
  
} # EO function