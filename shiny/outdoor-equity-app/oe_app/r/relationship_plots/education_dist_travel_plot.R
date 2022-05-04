
## race x distance traveled and parameters ##

education_dist_travel_plot <- function(admin_unitInput, siteInput,
                             education_top_quartile_df, ridb_df){
  
  print(education_dist_travel_data)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- 
    education_top_quartile_df %>% pmap_dfr(education_dist_travel_data, 
                                           ridb_df = ridb_df, 
                                           siteInput = siteInput)
  
  print(head(plot_data))
  
  # create plot
  education_group_colors <- c("HS, GED,\nor Below" = "#a6cee3", "Some College or\nTrade School"  = "#1f78b4", 
                              "Associates or\nBachelors Degree" = "#b2df8a", "Masters Degree\nor Above" = "#33a02c")
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(plot_data) == 0){
    
    print(paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
                 " that come from communities that fall into the high range for any educational level."))
    
  } else if (nrow(plot_data) > 0){
    
    plotly <- ggplot(data = plot_data, 
                     aes(x = median_distance_traveled_mi,
                         y = education_y_lab)) +
      geom_segment(aes(xend = 0, yend = education_y_lab)) +
      geom_point(aes(color = education_y_lab, fill = education_y_lab,
                     text = paste0(comma(count, accuracy = 1), 
                                   " unique visits were made by people who live in ZIP codes with high rates of<br>",
                                   education, 
                                   " as the highest level of education. Typically these visitors<br>traveled between ",
                                   comma(quartile_lower, accuracy = 1), 
                                   " and ", comma(quartile_upper, accuracy = 1), 
                                   " miles, with a median distance of ", 
                                   comma(median_distance_traveled_mi, accuracy = 1), 
                                   " miles.")),
                 size = 3.5, 
                 shape = 21, stroke = 2) +
      scale_y_discrete(expand = c(0.35, 0)) +
      scale_fill_manual(values = education_group_colors) +
      scale_color_manual(values = education_group_colors) +
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
                                        'Distance Traveled by Visitors with Different Levels of Education'),
                          font = list(size = 15))) %>%  
      add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                      x = -0.15, xref = 'paper', y = 0.93, yref = 'paper', 
                      showarrow = FALSE)
    
  } # EO else if
  
} # EO function