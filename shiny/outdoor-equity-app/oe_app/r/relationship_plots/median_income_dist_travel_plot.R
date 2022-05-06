
## education x distance traveled and parameters ##

median_income_dist_travel_plot <- function(admin_unitInput, siteInput, ridb_df){
  
  print(language_dist_travel_data)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- median_income_dist_travel_data(ridb_df = ridb_df, siteInput = siteInput)
  
  print(head(plot_data))
  
  # create plot
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(plot_data) == 0){
    
    print(paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
                 " that come from communities that fall into the high range for languages spoken in the home."))
    
  } else if (nrow(plot_data) > 0){
    
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
    
    ggplotly(plotly,
             tooltip = list("text")) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                           "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
      layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                        '<br>',
                                        'Distance Traveled by Visitors with Different Median Household Incomes'),
                          font = list(size = 15)))
    
  } # EO else if
  
} # EO function