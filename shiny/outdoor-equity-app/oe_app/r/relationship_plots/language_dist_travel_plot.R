
## education x distance traveled and parameters ##

language___plot <- function(admin_unitInput, siteInput,
                            language_top_quartile_df, ridb_df){
  
  print(language_dist_travel_data)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- 
    language_top_quartile_df %>% pmap_dfr(language_dist_travel_data, 
                                          ridb_df = ridb_df, 
                                          siteInput = siteInput)
  
  print(head(plot_data))
  
  # create plot
  language_group_colors <- c("People Who Speak Only<br>English At Home" = "#66c2a5", 
                             "People Who Speak<br>Language(s) Other Than<br>English At Home" = "#8da0cb")
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(plot_data) == 0){
    
    print(paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
                 " that come from communities that fall into the high range for any home language group."))
    
  } else if (nrow(plot_data) > 0){
    
    plotly <- ggplot(data = plot_data, 
                     aes(x = median_distance_traveled_mi,
                         y = language_y_lab)) +
      geom_segment(aes(xend = 0, yend = language_y_lab)) +
      geom_point(aes(color = language_y_lab, fill = language_y_lab,
                     text = paste0(comma(count, accuracy = 1), 
                                   " unique visits were made by people who live in ZIP codes with high rates of<br>people who ",
                                   language, 
                                   ". Typically these visitors<br>traveled between ",
                                   comma(quartile_lower, accuracy = 1), 
                                   " and ", comma(quartile_upper, accuracy = 1), 
                                   " miles, with a median distance of ", 
                                   comma(median_distance_traveled_mi, accuracy = 1), 
                                   " miles.")),
                 size = 3.5, 
                 shape = 21, stroke = 2) +
      scale_y_discrete(expand_scale(mult = 0.1)) +
      scale_fill_manual(values = language_group_colors) +
      scale_color_manual(values = language_group_colors) +
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
                                        'Distance Traveled by Visitors with Different Home Lanugages'),
                          font = list(size = 15))) %>%  
      add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                      x = -0.15, xref = 'paper', y = 0.9, yref = 'paper', 
                      showarrow = FALSE)
    
  } # EO else if
  
} # EO function