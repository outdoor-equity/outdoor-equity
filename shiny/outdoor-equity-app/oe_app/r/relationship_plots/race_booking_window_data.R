
race_booking_window_data <- function(siteInput, race_group, weighted_quartile, ridb_df){
  # reactive data frame 
  race_booking_window_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate    
    
    ridb_df %>%
      filter(park %in% siteInput) %>%
      select(park, customer_zip, asian, black, hispanic_latinx, 
             multiracial, native_american, other, pacific_islander, white,
             booking_window) %>% 
      drop_na(booking_window) %>% 
      pivot_longer(cols = 3:10,
                   names_to = "race",
                   values_to = "race_percentage") %>% 
      filter(race == race_group) %>%
      drop_na(race_percentage) %>% 
      filter(race_percentage >= weighted_quartile) %>% 
      summarize(median_booking_window = median(booking_window),
                quartile_lower = quantile(booking_window)[[2]],
                quartile_upper = quantile(booking_window)[[4]],
                count = n()) %>% 
      mutate(race = paste0(race_group)) %>%  ## BACK TO i
      relocate(race, .before = 1) %>% 
      mutate(race = str_replace(string = race,
                                pattern = "_",
                                replacement = " "),
             race = str_to_title(race),
             race = str_replace(string = race,
                                pattern = "Other",
                                replacement = "Other Race(s)"))
    
  })
  
  return(race_booking_window_rdf())
  
} # EO function