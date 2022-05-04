
education_site_type_data <- function(siteInput, education_group, weighted_quartile, ridb_df){
  # reactive data frame 
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      filter(park %in% siteInput) %>%
      select(park, customer_zip, hs_GED_or_below, 
             some_college, college, master_or_above, aggregated_site_type) %>% 
      drop_na(aggregated_site_type) %>% 
      pivot_longer(cols = 3:6,
                   names_to = "education",
                   values_to = "education_percentage") %>% 
      filter(education == paste0(education_group)) %>% 
      drop_na(education_percentage) %>% 
      filter(education_percentage >= weighted_quartile) %>% 
      count(education, aggregated_site_type) %>% 
      rename(count = n) %>% 
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