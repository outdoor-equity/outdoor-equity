
## race x distance traveled and parameters ##

dist_travel_plot <- function(agencyInput, admin_unitInput, siteInput){
  
  # reactive data frame 
  race_dist_travel_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(agency %in% agencyInput,
             admin_unit %in% admin_unitInput,
             park %in% siteInput) %>% 
      select(agency, admin_unit, park, customer_zip, asian, black, hispanic_latinx, 
             multiracial, native_american, other, pacific_islander, white,
             distance_traveled_m) %>% 
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
      drop_na(distance_traveled_mi) %>% 
      select(-distance_traveled_m)
      pivot_longer(cols = 5:12,
                   names_to = "race",
                   values_to = "race_percentage")
    
  })
  
  # wrangle reactive data frame for plotting
  racial_groups <- c("other", "pacific_islander", "multiracial", "asian", 
                     "black", "white", "native_american", "hispanic_latinx")
  
  data_plot_race_distance_traveled <- data.frame()
  
  for (i in seq_along(racial_groups)){
    # df_ca_bin_breaks <- data_ca_acs_2018 %>% 
    #   select(zip_code, asian, black, hispanic_latinx, multiracial, 
    #          native_american, other, pacific_islander, white, mean_zip_code_population) %>% 
    #   pivot_longer(cols = 2:9,
    #                names_to = "race",
    #                values_to = "race_percentage") %>% 
    #   filter(race == racial_groups[[i]]) %>% ## BACK TO i
    #   drop_na(race_percentage) 
    # 
    # weighted_half <- weighted.mean(x = df_ca_bin_breaks$race_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
    # 
    # df_ca_bin_breaks <- df_ca_bin_breaks %>% filter(race_percentage >= weighted_half)
    # 
    # weighted_quartile <- weighted.mean(x = df_ca_bin_breaks$race_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
    
    df_racial_group_i_longer <- race_dist_travel_rdf %>% 
      filter(race == racial_groups[[i]]) %>% ## BACK TO i
      drop_na(race_percentage)
    
    max_racial_group_ridb <- df_racial_group_i_longer %>%
      summarize(max = max(race_percentage))
    
    df_racial_group_i <- df_racial_group_i_longer %>% 
      filter(race_percentage >= weighted_quartile) %>% 
      summarize(mean_distance_traveled_mi = mean(distance_traveled_mi)) %>% 
      mutate(race = paste0(racial_groups[[i]])) %>%  ## BACK TO i
      relocate(race, .before = 1) %>% 
      mutate(race = str_replace(string = race,
                                pattern = "_",
                                replacement = " "),
             race = str_to_title(race),
             race = str_replace(string = race,
                                pattern = "Other",
                                replacement = "Other Race(s)"))
    
    data_plot_race_distance_traveled <- rbind(data_plot_race_distance_traveled, df_racial_group_i)
    
    # assign(paste0("top_quartile_ca_", racial_groups[[i]]), 
    #        c(weighted_quartile), 
    #        envir = .GlobalEnv)
    assign(paste0("max_race_ridb_", racial_groups[[i]]),
           data.frame(max_racial_group_ridb),
           envir = .GlobalEnv)
  }
  
  # parameters
  racial_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                           "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                           "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  
  # plot for shiny app
  race_dist_travel_plot <- ggplot(data = data_plot_race_distance_traveled, 
                                  aes(x = mean_distance_traveled_mi,
                                      y = reorder(race, mean_distance_traveled_mi))) +
    geom_segment(aes(xend = 0, yend = race)) +
    geom_point(aes(x = mean_distance_traveled_mi,
                   y = reorder(race, mean_distance_traveled_mi),
                   color = race, fill = race,
                   text = paste("People who live in ZIP codes with high", 
                                str_to_title(race) %>% str_replace(string = ., pattern = "\\(S\\)", "\\(s\\)"), "populations",
                                "<br>travel an estimated distance of", round(mean_distance_traveled_mi, 0), "miles.")),
               width = .25,
               size = 4, shape = 21, stroke = 2) +
    scale_fill_manual(values = racial_group_colors) +
    scale_color_manual(values = racial_group_colors) +
    labs(x = paste("Average Distance Traveled from Home to Site (miles)"),
         y = "",
         title = "Average Distance Traveled to Reservation for <br>Different Racial Groups") + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none")
  
  ggplotly(race_dist_travel_plot,
           tooltip = list("text"))
  
} # EO function