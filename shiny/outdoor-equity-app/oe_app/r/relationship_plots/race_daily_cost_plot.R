
## race x distance traveled and parameters ##

race_dist_travel_plot <- function(admin_unitInput, siteInput,
                                  race_top_quartile_df, ridb_df){
  
  print(race_dist_travel_data)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- 
    race_top_quartile_df %>% pmap_dfr(race_dist_travel_data, 
                                      ridb_df = ridb_df, 
                                      siteInput = siteInput)
  
  print(head(plot_data))
  
  # create plot
  race_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(plot_data) == 0){
    
    print(paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
                 " that come from communities that fall into the high range for any racial groups."))
    
  } else if (nrow(plot_data) > 0){
    
    plotly <- ggplot(data = plot_data, 
                     aes(x = median_daily_cost,
                         y = reorder(race, median_daily_cost))) +
      geom_segment(aes(xend = 0, yend = race)) +
      geom_point(aes(color = race, fill = race,
                     text = paste0(comma(count, accuracy = 1), 
                                   " unique visits were made by people who live in ZIP codes<br>with high ",
                                   str_to_title(race) %>% 
                                     str_replace(string = ., pattern = "\\(S\\)", "\\(s\\)"), 
                                   " populations. Typically these visitors paid between<br>",
                                   dollar(quartile_lower), 
                                   " and ", dollar(quartile_upper), 
                                   " per day for reservations, with a median of ", 
                                   dollar(median_daily_cost), 
                                   ".")),
                 size = 3.5, 
                 shape = 21, stroke = 2) +
      scale_x_continuous(labels = dollar) +
      scale_y_discrete(expand = c(0.3, 0)) +
      scale_fill_manual(values = race_group_colors) +
      scale_color_manual(values = race_group_colors) +
      labs(x = paste("Estimated Daily Cost per Reservation (US $)"),
           y = "",
           title = paste0("Daily Costs per Reservation Paid by Different Racial Groups to<br>", 
                          siteInput, ", ", admin_unitInput, " in 2018")) + 
      theme_minimal() +
      theme(plot.background = element_rect("white"),
            panel.grid.major.y = element_blank(),
            legend.position = "none")
    
    ggplotly(plotly,
             tooltip = list("text")) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                           "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
      add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                      x = -0.1, xref = 'paper', y = 0.95, yref = 'paper', 
                      showarrow = FALSE)
    
  } # EO else if
  
} # EO function