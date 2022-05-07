
#' Education x Length of Stay Data
#'
#' @param siteInput User pick for site
#' @param education_group String indicating educational category of interest
#' @param weighted_quartile Value of 3rd quartile for educational category
#' @param ridb_df RIDB dataframe object name
#'
#' @return Reactive dataframe of all reservations that fall above 3rd quartile value for given educational category at user picked site
#'
#' @examples

education_length_of_stay_data <- function(siteInput, education_group, weighted_quartile, ridb_df){
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
             hs_GED_or_below, some_college, college, master_or_above, 
             length_of_stay) %>% 
      drop_na(length_of_stay) %>% 
      pivot_longer(cols = 3:6,
                   names_to = "education",
                   values_to = "education_percentage") %>% 
      # filter for specific educational category
      filter(education == education_group) %>% 
      drop_na(education_percentage) %>% 
      # filter rows that fall above 3rd quartile value
      filter(education_percentage >= weighted_quartile) %>% 
      # summarize to inner quartile range, median, and total reservations
      summarize(median_length_of_stay = median(length_of_stay),
                quartile_lower = quantile(length_of_stay)[[2]],
                quartile_upper = quantile(length_of_stay)[[4]],
                count = n()) %>% 
      mutate(education = paste0(education_group)) %>% 
      relocate(education, .before = 1) %>%   
      # updated education category name strings for plotting
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
    
  }) #EO reactive df
  
  return(rdf())
  
} # EO function