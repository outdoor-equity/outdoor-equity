
#' Median-income x Booking Window Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param median_income_binned List of decile values
#'
#' @return Plotly of median-income categories compared to booking window
#'
#' @examples

median_income_booking_window_plot <- function(admin_unitInput, siteInput, ridb_df, median_income_binned){
  
  # categorize and summarize data to median-income decile groups
  plot_data <- median_income_booking_window_data(ridb_df = ridb_df, siteInput = siteInput,
                                                 median_income_binned = median_income_binned)
  
  # create plot
  plotly <- ggplot(data = plot_data, 
                   aes(x = median_booking_window,
                       y = median_income_binned)) +
    geom_segment(aes(xend = 0, yend = median_income_binned)) +
    geom_point(aes(color = median_income_binned, fill = median_income_binned,
                   text = paste0(comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes with a<br>median household income between ",
                                 median_income_binned, 
                                 ". Typically these visitors<br>reserved their site between ",
                                 comma(quartile_lower, accuracy = 1), 
                                 " and ", comma(quartile_upper, accuracy = 1), 
                                 " days before the start of their trip, with a median of ", 
                                 comma(median_booking_window, accuracy = 1), 
                                 " days.")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
    labs(x = "Estimated Number of Days in Advance Site is Reserved",
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
                                      'Number of Days in Advance Site is Reserved by Different Median Household Incomes'),
                        font = list(size = 15)))
  
} # EO function