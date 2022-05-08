
#' Language x Distance Traveled Data
#'
#' @param siteInput User pick for site
#' @param language_group String indicating language category of interest
#' @param weighted_quartile Value of 3rd quartile for language category
#' @param ridb_df RIDB dataframe object name
#'
#' @return Reactive dataframe of all reservations that fall above 3rd quartile value for given language category at user picked site
#'
#' @examples

language_dist_travel_data <- function(siteInput, language_group, weighted_quartile, ridb_df){
  # reactive data frame 
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      # filter to user site of choice
      filter(park %in% siteInput) %>%
      # select to variables of interest
      select(park, customer_zip, 
             english_only, not_english_only,
             distance_traveled_m) %>% 
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
      drop_na(distance_traveled_mi) %>% 
      pivot_longer(cols = 3:4,
                   names_to = "language",
                   values_to = "language_percentage") %>% 
      # filter for specific language category
      filter(language == language_group) %>% 
      drop_na(language_percentage) %>% 
      # filter rows that fall above 3rd quartile value
      filter(language_percentage >= weighted_quartile) %>% 
      # summarize to inner quartile range, median, and total reservations
      summarize(median_distance_traveled_mi = median(distance_traveled_mi),
                quartile_lower = quantile(distance_traveled_mi)[[2]],
                quartile_upper = quantile(distance_traveled_mi)[[4]],
                count = n()) %>% 
      mutate(language = paste0(language_group)) %>% 
      relocate(language, .before = 1) %>% 
      # updated language category name strings for plotting
      mutate(language = str_replace_all(string = language,
                                        pattern = "_",
                                        replacement = " "),
             language = str_replace(string = language,
                                    pattern = "^english only$",
                                    replacement = "speak only English at home"),
             language = str_replace(string = language,
                                    pattern = "^not english only$",
                                    replacement = "speak language(s) other than English at home"),
             language = str_to_lower(language),
             language = str_replace(string = language,
                                    pattern = "english",
                                    replacement = "English"),
             language_y_lab = case_when(language == "speak only English at home" ~ "People Who Speak Only<br>English At Home",
                                        language == "speak language(s) other than English at home" ~ "People Who Speak<br>Language(s) Other Than<br>English At Home"))
    
  }) #EO reactive df
  
  return(rdf())
  
} # EO function