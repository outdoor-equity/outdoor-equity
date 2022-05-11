
#' Median-income Number of Reservations Above Top Quartile Data Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param median_income_binned List of decile values
#'
#' @return Plotly of median-income categories compared to booking window
#'
#' @examples

median_income_top_quartile_res_plot <- function(admin_unitInput, siteInput, ridb_df, median_income_binned){
  
  # categorize and summarize data to median-income decile groups
  plot_data <- median_income_booking_window_data(ridb_df = ridb_df, siteInput = siteInput,
                                                 median_income_binned = median_income_binned)
  
  # create plot
  plotly <- ggplot(data = plot_data) +
    geom_col(aes(x = count,
                 y = median_income_binned,
                 fill = median_income_binned,
                 text = paste0(comma(count, accuracy = 1), 
                               " reservations were made by people who live in ZIP codes<br>with median-incomes between ", 
                               median_income_binned, "."))) +
    scale_x_continuous(labels = comma_format(accuracy = 1)) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
    labs(x = paste("Number of Reservations"),
         y = "") + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8))
  
  # create plotly
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0("Reservations to this site from<br>ZIP codes with median-incomes of:"),
                        font = list(size = 10.5)),
           height = 405,
           width = 340)
  
} # EO function