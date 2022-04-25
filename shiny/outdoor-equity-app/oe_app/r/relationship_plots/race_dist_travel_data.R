
function_plot_race_distanceTravel <- function(agencyInput, admin_unitInput, siteInput, 
                                              race_group = c("other", "pacific_islander", "multiracial", "asian", 
                                                             "black", "white", "native_american", "hispanic_latinx"), 
                                              weighted_quartile,
                                              ridb_df){
  # reactive data frame 
  race_dist_travel_rdf <- reactive ({
    
    ridb_df %>%
      filter(agency %in% agencyInput,
             admin_unit %in% admin_unitInput,
             park %in% siteInput) %>%
      select(agency, admin_unit, park, customer_zip, asian, black, hispanic_latinx, 
             multiracial, native_american, other, pacific_islander, white,
             distance_traveled_m) %>% 
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
      drop_na(distance_traveled_mi) %>% 
      pivot_longer(cols = 5:12,
                   names_to = "race",
                   values_to = "race_percentage") %>% 
      filter(race == paste0(race_group)) %>% 
      drop_na(race_percentage)
  })
  
  
  max_racial_group_ridb <- ridb_df() %>%
    summarize(max = max(race_percentage))
  
  df_racial_group_i <- ridb_df() %>%
    filter(race_percentage >= weighted_quartile) %>% 
    summarize(mean_distance_traveled_mi = mean(distance_traveled_mi)) %>% 
    mutate(race = paste0(race_group)) %>% 
    relocate(race, .before = 1) %>% 
    mutate(race = str_replace(string = race,
                              pattern = "_",
                              replacement = " "),
           race = str_to_title(race),
           race = str_replace(string = race,
                              pattern = "Other",
                              replacement = "Other Race(s)"))
  
  return(df_racial_group_i)
  
} # EO function