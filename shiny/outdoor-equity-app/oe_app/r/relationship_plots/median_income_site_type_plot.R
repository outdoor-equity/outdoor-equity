
## education x distance traveled and parameters ##

median_income_dist_travel_plot <- function(admin_unitInput, siteInput, 
                                           ridb_df, site_type_string){
  
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- median_income_site_type_data(ridb_df = ridb_df, siteInput = siteInput, 
                                            site_type_string = site_type_string)
  
  print(head(plot_data))
  
  # create plot
  
  # create plot (or say no such site type if none exist at siteInput)
  plotly <- ggplot(data = plot_data) +
    geom_col(aes(x = count,
                 y = median_income_binned,
                 fill = median_income_binned,
                 text = paste0("Of visits to ", site_type_string, " overnight reservable sites, ", 
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
  
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      "Number of Reservations to ", str_to_title(site_type_string), 
                                      " Sites by Visitors with Different Median Household Incomes"),
                        font = list(size = 15)))

} # EO function