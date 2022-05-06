
## language x site type and parameters ##

language_site_type_plot <- function(admin_unitInput, siteInput,
                            language_top_quartile_df, ridb_df,
                            site_type_string){
  
  print(language_dist_travel_data)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- 
    education_top_quartile_df %>% pmap_dfr(education_site_type_data, 
                                           ridb_df = ridb_df, 
                                           siteInput = siteInput) %>% 
    filter(aggregated_site_type == site_type_string) %>% 
    mutate(aggregated_site_type = str_to_title(aggregated_site_type),
           aggregated_site_type = str_replace(string = aggregated_site_type,
                                              pattern = "Rv", 
                                              replacement = "RV"))
  
  print(head(plot_data))
  
  # create plot
  language_group_colors <- c("Speak Only<br>English At Home" = "#66c2a5", "Speak Language(s) Other<br>Than English At Home" = "#8da0cb")
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(plot_data) == 0){
    
    print(paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
                 " that come from communities that fall into the high range for languages spoken in the home."))
    
  } else if (nrow(plot_data) > 0){
    
    plotly <- ggplot(data = plot_data) +
      geom_col(aes(x = count,
                   y = language_y_lab,
                   fill = language_y_lab,
                   text = paste0("Of visits to ", aggregated_site_type, " overnight reservable sites, ", 
                                 comma(count, accuracy = 1), 
                                 " reservations were made by <br>people who live in ZIP codes with high rates of people who ",
                                 language, "."))) +
      scale_y_discrete(expand = c(0.9, 0)) +
      scale_fill_manual(values = language_group_colors) +
      scale_color_manual(values = language_group_colors) +
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
                                        "Number of Reservations to ", str_to_title(site_type_categories[[1]]) %>%
                                          str_replace(string = ., pattern = "Rv", replacement = "RV"), 
                                        " Sites by Visitors with Different Home Lanugages"),
                          font = list(size = 15))) %>%  
      add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                      x = -0.4, xref = 'paper', y = 0.93, yref = 'paper', 
                      showarrow = FALSE)
    
  } # EO else if
  
} # EO function