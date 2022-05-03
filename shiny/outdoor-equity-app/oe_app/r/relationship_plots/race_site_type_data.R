
race_dist_travel_data <- function(siteInput, race_group, weighted_quartile, ridb_df){
  
  # reactive data frame 
  race_dist_travel_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      select(park, customer_zip, asian, black, hispanic_latinx, 
             multiracial, native_american, other, pacific_islander, white,
             aggregated_site_type) %>% 
      drop_na(aggregated_site_type) %>% 
      pivot_longer(cols = 3:10,
                   names_to = "race",
                   values_to = "race_percentage") %>% 
      filter(race == paste0(race_group)) %>%
      drop_na(race_percentage) %>%
      filter(race_percentage >= weighted_quartile) %>% 
      count(race, aggregated_site_type) %>% 
      rename(count = n) %>%
      mutate(race = paste0(race_group)) %>%
      relocate(race, .before = 1) %>% 
      mutate(race = str_replace(string = race,
                                pattern = "_",
                                replacement = " "),
             race = str_to_title(race),
             race = str_replace(string = race,
                                pattern = "Other",
                                replacement = "Other Race(s)"))
  })
  
  return(race_dist_travel_rdf())
  
} # EO function