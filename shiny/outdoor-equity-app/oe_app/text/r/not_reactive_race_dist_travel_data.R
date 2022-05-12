
#' Race x Distance Traveled Data
#'
#' @param siteInput User pick for site
#' @param education_group String indicating racial category of interest
#' @param weighted_quartile Value of 3rd quartile for racial category
#' @param ridb_df RIDB dataframe object name
#'
#' @return Reactive dataframe of all reservations that fall above 3rd quartile value for given racial category at user picked site
#'
#' @examples

race_dist_travel_data <- function(siteInput, race_group, weighted_quartile, ridb_df){
  # reactive data frame 
  race_dist_travel_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      # filter to user site of choice
      filter(park %in% siteInput) %>%
      # select to variables of interest
      select(park, customer_zip, 
             asian, black, hispanic_latinx, multiracial, 
             native_american, other, pacific_islander, white,
             distance_traveled_m) %>% 
      mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
      select(-distance_traveled_m) %>% 
      drop_na(distance_traveled_mi) %>% 
      pivot_longer(cols = 3:10, 
                   names_to = "race",
                   values_to = "race_percentage") %>% 
      # filter for specific racial category
      filter(race == race_group) %>% 
      drop_na(race_percentage) %>% 
      # filter rows that fall above 3rd quartile value
      filter(race_percentage >= weighted_quartile) %>% 
      # summarize to inner quartile range, median, and total reservations
      summarize(median_distance_traveled_mi = median(distance_traveled_mi),
                quartile_lower = quantile(distance_traveled_mi)[[2]],
                quartile_upper = quantile(distance_traveled_mi)[[4]],
                count = n()) %>% 
      # updated racial category name strings for plotting
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