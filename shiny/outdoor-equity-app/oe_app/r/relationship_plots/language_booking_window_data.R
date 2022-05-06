
language_booking_window_data <- function(siteInput, language_group, weighted_quartile, ridb_df){
  # reactive data frame 
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      filter(park %in% siteInput) %>%
      select(park, customer_zip, english_only, not_english_only,
             booking_window) %>% 
      drop_na(booking_window) %>% 
      pivot_longer(cols = 3:4,
                   names_to = "language",
                   values_to = "language_percentage") %>% 
      filter(language == language_group) %>% 
      drop_na(language_percentage) %>% 
      filter(language_percentage >= weighted_quartile) %>% 
      summarize(median_booking_window = median(booking_window),
                quartile_lower = quantile(booking_window)[[2]],
                quartile_upper = quantile(booking_window)[[4]],
                count = n()) %>% 
      mutate(language = paste0(language_group)) %>% 
      relocate(language, .before = 1) %>% 
      mutate(language = str_replace_all(string = language,
                                        pattern = "_",
                                        replacement = " "),
             language = str_replace(string = language,
                                    pattern = "^english only$",
                                    replacement = "Speak Only English at Home"),
             language = str_replace(string = language,
                                    pattern = "^not english only$",
                                    replacement = "Speak Language(s) Other Than English at Home"),
             language = str_to_lower(language),
             language = str_replace(string = language,
                                    pattern = "english",
                                    replacement = "English"),
             language_y_lab = case_when(language == "speak only English at home" ~ "People Who Speak Only<br>English At Home",
                                        language == "speak language(s) other than English at home" ~ "People Who Speak<br>Language(s) Other Than<br>English At Home"))
    
  }) #EO reactive df
  
  return(rdf())
  
} # EO function