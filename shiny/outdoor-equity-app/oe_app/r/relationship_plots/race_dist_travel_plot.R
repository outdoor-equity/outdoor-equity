
## race x distance traveled and parameters ##

race_dist_travel_plot <- function(admin_unitInput, siteInput,
                                  race_top_quartile_df, ridb_df){
  
  print(race_dist_travel_data)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  data_race_dist_travel <- 
    race_top_quartile_df %>% pmap_dfr(race_dist_travel_data, 
                                      ridb_df = ridb_df, 
                                      siteInput = siteInput)
  
  print(head(data_race_dist_travel))
  
  # create plot
  race_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(data_race_dist_travel) == 0){
    
    print(paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
                 " that come from communities that fall into the high range for any racial groups."))
    
  } else if (nrow(data_race_dist_travel) > 0){
    
    race_distance_traveled_plotly <- ggplot(data = data_race_dist_travel, 
                                            aes(x = median_distance_traveled_mi,
                                                y = reorder(race, median_distance_traveled_mi))) +
      geom_segment(aes(xend = 0, yend = race)) +
      geom_point(aes(color = race, fill = race,
                     text = paste0(comma(count, accuracy = 1), 
                                   " unique visits were made by people who live in ZIP codes<br>with high ",
                                   str_to_title(race) %>% 
                                     str_replace(string = ., pattern = "\\(S\\)", "\\(s\\)"), 
                                   " populations. Typically folks traveled between<br>",
                                   comma(quartile_lower, accuracy = 1), 
                                   " and ", comma(quartile_upper, accuracy = 1), 
                                   " miles, with a median distance of ", 
                                   comma(median_distance_traveled_mi, accuracy = 1), 
                                   " miles.")),
                 size = 3.5, 
                 shape = 21, stroke = 2) +
      scale_y_discrete(expand = c(0.3, 0)) +
      scale_fill_manual(values = race_group_colors) +
      scale_color_manual(values = race_group_colors) +
      annotate(geom = "text", 
               x = 50, y = 9,
               label = "Reservations from ZIP codes with high proportions of:") +
      labs(x = paste("Estimated Distance Traveled from Home to Site (miles)"),
           y = "",
           title = paste0("Distance Traveled by Different Racial Groups to<br>", 
                          siteInput, ", ", admin_unitInput, " in 2018")) + 
      theme_minimal() +
      theme(plot.background = element_rect("white"),
            panel.grid.major.y = element_blank(),
            legend.position = "none")
    
    ggplotly(race_distance_traveled_plotly,
             tooltip = list("text")) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                           "hoverClosestCartesian", "hoverCompareCartesian"))
    
  } # EO else if
  
} # EO function