
## race x distance traveled and parameters ##

race_dist_travel_plot <- function(agencyInput, admin_unitInput, siteInput,
                                  race_top_quartile_df, ridb_df){
  
  
  
  data_plot_race_distanceTravel <- 
    race_top_quartile_df %>% pmap_dfr(race_dist_travel_data, 
                                      ridb_df = ridb_df, 
                                      agencyInput = agencyInput, 
                                      admin_unitInput = admin_unitInput, 
                                      siteInput = siteInput)
  
  # create plot
  race_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  plot_race_distance_traveled <- ggplot(data = data_plot_race_distanceTravel, 
                                        aes(x = mean_distance_traveled_mi,
                                            y = reorder(race, mean_distance_traveled_mi))) +
    geom_segment(aes(xend = 0, yend = race)) +
    geom_point(aes(x = mean_distance_traveled_mi,
                   y = reorder(race, mean_distance_traveled_mi),
                   color = race, fill = race,
                   text = paste("People who live in ZIP codes with high", 
                                str_to_title(race) %>% 
                                  str_replace(string = ., pattern = "\\(S\\)", "\\(s\\)"), 
                                "populations", "<br>travel an estimated distance of", 
                                round(mean_distance_traveled_mi, 0), 
                                "miles to overnight reservable sites.")),
               width = .25,
               size = 4, shape = 21, stroke = 2) +
    scale_fill_manual(values = race_group_colors) +
    scale_color_manual(values = race_group_colors) +
    labs(x = paste("Average Distance Traveled from Home to Site (miles)"),
         y = "",
         title = paste0("Average Distance Traveled to ", siteInput, ", ", admin_unitInput, " for <br>Different Racial Groups")) + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none")
  
  ggplotly(plot_race_distance_traveled,
           tooltip = list("text"))   
  
} # EO function