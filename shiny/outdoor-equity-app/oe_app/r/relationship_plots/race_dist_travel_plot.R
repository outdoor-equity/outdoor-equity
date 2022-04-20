
## race x distance traveled and parameters ##

dist_travel_plot <- function(agencyInput, admin_unitInput, siteInput, 
                             race_group, 
                             acs_df = data_ca_acs_2018, 
                             ridb_df = data_ridb_acs_2018){
  
  ## NEED TO ADD REACTIVE DF ELEMENTS STILL

    df_ca_bin_breaks <- acs_df %>%
      select(zip_code, asian, black, hispanic_latinx, multiracial,
             native_american, other, pacific_islander, white, mean_zip_code_population) %>%
      pivot_longer(cols = 2:9,
                   names_to = "race",
                   values_to = "race_percentage") %>%
      filter(race == race_group) %>% ## BACK TO i
      drop_na(race_percentage)
    
    weighted_half <- weighted.mean(x = df_ca_bin_breaks$race_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
    
    df_ca_bin_breaks <- df_ca_bin_breaks %>% filter(race_percentage >= weighted_half)
    
    weighted_quartile <- weighted.mean(x = df_ca_bin_breaks$race_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
    
    df_racial_group_i_longer <- data_ridb_acs_2018 %>% 
      select(agency, admin_unit, park, customer_zip, asian, black, hispanic_latinx, 
             multiracial, native_american, other, pacific_islander, white,
             distance_traveled_m) %>% 
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
      drop_na(distance_traveled_mi) %>% 
      pivot_longer(cols = 5:12,
                   names_to = "race",
                   values_to = "race_percentage") %>% 
      filter(race == race_group) %>% ## BACK TO i
      drop_na(race_percentage)
    
    max_racial_group_ridb <- df_racial_group_i_longer %>%
      summarize(max = max(race_percentage))
    
    df_racial_group_i <- df_racial_group_i_longer %>%
      filter(race_percentage >= weighted_quartile) %>% 
      summarize(mean_distance_traveled_mi = mean(distance_traveled_mi)) %>% 
      mutate(race = paste0(race_group)) %>%  ## BACK TO i
      relocate(race, .before = 1) %>% 
      mutate(race = str_replace(string = race,
                                pattern = "_",
                                replacement = " "),
             race = str_to_title(race),
             race = str_replace(string = race,
                                pattern = "Other",
                                replacement = "Other Race(s)"))
} # EO function