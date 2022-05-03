
race_daily_cost_per_visitor_data <- function(siteInput, race_group, weighted_quartile, ridb_df){
  # reactive data frame 
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      filter(park %in% siteInput) %>%
      select(park, customer_zip, asian, black, hispanic_latinx, 
             multiracial, native_american, other, pacific_islander, white,
             daily_cost_per_visitor) %>% 
      drop_na(daily_cost_per_visitor) %>% 
      filter(daily_cost_per_visitor != Inf) %>% 
      pivot_longer(cols = 3:10,
                   names_to = "race",
                   values_to = "race_percentage") %>% 
      filter(race == race_group) %>%
      drop_na(race_percentage) %>% 
      filter(race_percentage >= weighted_quartile) %>% 
      summarize(median_daily_cost_per_visitor = median(daily_cost_per_visitor),
                quartile_lower = quantile(daily_cost_per_visitor)[[2]],
                quartile_upper = quantile(daily_cost_per_visitor)[[4]],
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
  
  return(rdf())
  
} # EO function