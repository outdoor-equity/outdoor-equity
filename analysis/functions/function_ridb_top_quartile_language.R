
#' RIDB Reservations Above "High" Threshold for Language
#'
#' @param language_group String indicating language category of interest
#' @param weighted_quartile Value of 3rd quartile for educational category
#' @param joined_ridb_acs_df joined RIDB / ACS dataframe object name
#'
#' @return Dataframe of all reservations that fall above 3rd quartile value for given language category
#'
#' @examples

language_ridb_top_quartile <- function(language_group, weighted_quartile, joined_ridb_acs_df){
  
  df <- joined_ridb_acs_df %>% 
    # select variables of interest
    select(park, customer_zip, 
           english_only, not_english_only, 
           booking_window, daily_cost, daily_cost_per_visitor,
           distance_traveled_m, length_of_stay, aggregated_site_type) %>% 
    # convert distance traveled meters to miles
    mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
    select(!distance_traveled_m) %>% 
    pivot_longer(cols = 3:4,
                 names_to = "language",
                 values_to = "language_percentage") %>% 
    # filter for specific language category
    filter(language == language_group) %>% 
    # filter rows that fall above 3rd quartile value
    filter(language_percentage >= weighted_quartile) %>% 
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
           language_y_lab = case_when(language == "speak only English at home" ~ 
                                        "Only English<br>At Home",
                                      language == "speak language(s) other than English at home" ~ 
                                        "Language(s)<br>Other Than<br>English At Home"))
  
  return(df)
}