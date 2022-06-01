
#' RIDB Reservations Above "High" Threshold for Education
#'
#' @param education_group String indicating educational category of interest
#' @param weighted_quartile Value of 3rd quartile for educational category
#' @param joined_ridb_acs_df joined RIDB / ACS dataframe object name
#'
#' @return Dataframe of all reservations that fall above 3rd quartile value for given educational category
#'
#' @examples
education_ridb_top_quartile <- function(education_group, weighted_quartile, joined_ridb_acs_df){
  
  df <- joined_ridb_acs_df %>% 
    # select variables of interest
    select(park, customer_zip, 
           hs_GED_or_below, some_college, college, master_or_above, 
           booking_window, daily_cost, daily_cost_per_visitor,
           distance_traveled_m, length_of_stay, aggregated_site_type) %>% 
    # convert distance traveled meters to miles
    mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
    select(!distance_traveled_m) %>% 
    pivot_longer(cols = 3:6,
                 names_to = "education",
                 values_to = "education_percentage") %>% 
    # filter for specific educational category
    filter(education == education_group) %>% 
    # filter rows that fall above 3rd quartile value
    filter(education_percentage >= weighted_quartile) %>% 
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
  
  return(df)
}