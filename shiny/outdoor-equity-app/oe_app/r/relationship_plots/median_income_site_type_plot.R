#' Median-income x Site Type Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param median_income_binned List of decile values
#'
#' @return Plotly of median-income categories compared to site type
#'
#' @examples

median_income_site_type_plot <- function(admin_unitInput, siteInput, 
                                           ridb_df, median_income_binned, site_type_string){
  
  # categorize and summarize data to median-income decile groups
  plot_data <- median_income_site_type_data(ridb_df = ridb_df, siteInput = siteInput, 
                                            median_income_binned = median_income_binned,
                                            site_type_string = site_type_string)
  
  validate(
    need(nrow(plot_data) > 0,
         paste0("There are no ", site_type_string %>%
                  str_replace(string = ., pattern = "Rv", replacement = "RV"), " sites at ", siteInput, ", ", admin_unitInput, "."))
  ) # EO validate
  
  # create plot
  plotly <- ggplot(data = plot_data) +
    geom_col(aes(x = count,
                 y = median_income_binned,
                 fill = median_income_binned,
                 text = paste0("Of visits to ", aggregated_site_type, " overnight reservable sites, ", 
                               comma(count, accuracy = 1), 
                               " reservations were made by <br>people who live in ZIP codes with median household incomes between ",
                               median_income_binned, "."))) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
    labs(x = paste("Number of Reservations"),
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
                                      "Number of Reservations to ", 
                                      str_to_title(site_type_string) %>% 
                                        str_replace(string = ., pattern = "Rv", replacement = "RV"), 
                                      " Sites by Visitors with Different Median Household Incomes"),
                        font = list(size = 15)))

} # EO function