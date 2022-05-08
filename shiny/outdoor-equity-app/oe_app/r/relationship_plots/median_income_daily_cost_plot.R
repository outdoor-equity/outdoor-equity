
#' Median-income x Daily Cost Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param median_income_binned List of decile values
#'
#' @return Plotly of median-income categories compared to daily cost
#'
#' @examples

median_income_daily_cost_plot <- function(admin_unitInput, siteInput, ridb_df, median_income_binned){
  
  # categorize and summarize data to median-income decile groups
  plot_data <- median_income_daily_cost_data(ridb_df = ridb_df, siteInput = siteInput,
                                             median_income_binned = median_income_binned)
  
  # create plot
  plotly <- ggplot(data = plot_data, 
                   aes(x = median_daily_cost,
                       y = median_income_binned)) +
    geom_segment(aes(xend = 0, yend = median_income_binned)) +
    geom_point(aes(color = median_income_binned, fill = median_income_binned,
                   text = paste0(comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes with a<br>median household income between ",
                                 median_income_binned, 
                                 ". Typically these visitors<br>pay between ",
                                 dollar(quartile_lower), 
                                 " and ", dollar(quartile_upper), 
                                 " per day, with a median of ", 
                                 dollar(median_daily_cost), 
                                 ".")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_x_continuous(labels = dollar) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
    labs(x = "Estimated Daily Cost per Reservation (US $)",
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
                                      'Daily Cost Paid by Visitors with Different Median Household Incomes'),
                        font = list(size = 15)))
  
} # EO function