
#' RIDB Reservations Above "High" Threshold for Race
#'
#' @param race_group String indicating racial category of interest
#' @param weighted_quartile Value of 3rd quartile for educational category
#' @param joined_ridb_acs_df joined RIDB / ACS dataframe object name
#'
#' @return Dataframe of all reservations that fall above 3rd quartile value for given language category
#'
#' @examples

race_ridb_top_quartile <- function(race_group, weighted_quartile, joined_ridb_acs_df){
  
  df <- joined_ridb_acs_df %>% 
    # select variables of interest
    select(park, customer_zip, 
           asian, black, hispanic_latinx, multiracial, 
           native_american, other, pacific_islander, white, 
           booking_window, daily_cost, daily_cost_per_visitor,
           distance_traveled_m, length_of_stay, aggregated_site_type) %>% 
    # convert distance traveled meters to miles
    mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
    select(!distance_traveled_m) %>% 
    pivot_longer(cols = 3:10,
                 names_to = "race",
                 values_to = "race_percentage") %>% 
    # filter for specific racial category
    filter(race == race_group) %>%
    # filter rows that fall above 3rd quartile value
    filter(race_percentage >= weighted_quartile) %>% 
    # updated racial category name strings for plotting
    mutate(race = str_replace(string = race,
                              pattern = "_",
                              replacement = " "),
           race = str_to_title(race),
           race = str_replace(string = race,
                              pattern = "Other",
                              replacement = "Other Race(s)"))
  
  return(df)
}