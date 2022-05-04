
education_dist_travel_data <- function(siteInput, education_group, weighted_quartile, ridb_df){
  # reactive data frame 
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      filter(park %in% siteInput) %>%
      select(park, customer_zip, hs_GED_or_below, some_college, 
             college, master_or_above, distance_traveled_m) %>% 
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
      drop_na(distance_traveled_mi) %>% 
      pivot_longer(cols = 3:6,
                   names_to = "education",
                   values_to = "education_percentage") %>% 
      filter(education == paste0(education_group)) %>% 
      drop_na(education_percentage) %>% 
      filter(education_percentage >= weighted_quartile) %>% 
      summarize(median_distance_traveled_mi = median(distance_traveled_mi),
                quartile_lower = quantile(distance_traveled_mi)[[2]],
                quartile_upper = quantile(distance_traveled_mi)[[4]],
                count = n()) %>% 
      mutate(education = paste0(education_group)) %>% 
      relocate(education, .before = 1) %>% 
      mutate(education = str_replace_all(string = education,
                                         pattern = "_",
                                         replacement = " "),
             education = str_to_title(education),
             education = str_replace(string = education,
                                     pattern = "Hs Ged Or", 
                                     replacement = "HS, GED, or"),
             education = str_replace(string = education,
                                     pattern = "Some College",
                                     replacement = "Some College or Trade School"),
             education = str_replace(string = education,
                                     pattern = "^College$",
                                     replacement = "Associates or Bachelors Degree"),
             education = str_replace(string = education,
                                     pattern = "Master Or Above",
                                     replacement = "Masters Degree or Above"),
             education_y_lab = case_when(education == "HS, GED, or Below" ~ "HS, GED,\nor Below",
                                         education == "Some College or Trade School" ~ "Some College or\nTrade School",
                                         education == "Associates or Bachelors Degree" ~ "Associates or\nBachelors Degree",
                                         education == "Masters Degree or Above" ~ "Masters Degree\nor Above"),
             education_y_lab = factor(education_y_lab, levels = c("HS, GED,\nor Below", 
                                                                  "Some College or\nTrade School", 
                                                                  "Associates or\nBachelors Degree", 
                                                                  "Masters Degree\nor Above")))
    
  })
  
  return(rdf())
  
} # EO function