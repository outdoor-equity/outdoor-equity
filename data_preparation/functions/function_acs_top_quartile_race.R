
#' Race ACS Variable 3rd Quartile Value
#'
#' @param race_group String indicating racial category of interest
#' @param acs_df ACS US Census dataframe object name
#'
#' @return Value of 3rd quartile for specific racial category
#'
#' @examples


race_top_quartile <- function(race_group, acs_df){
  
  df <- acs_df %>%
    # filter variables of interest
    select(zip_code, asian, black, hispanic_latinx, multiracial,
           native_american, other, pacific_islander, white, mean_zip_code_population) %>%
    pivot_longer(cols = 2:9,
                 names_to = "race",
                 values_to = "race_percentage") %>%
    # filter category of interest
    filter(race == race_group) %>% 
    drop_na(race_percentage)
  
  # weighted median value (weighted based on ZIP code populations)
  weighted_half <- weighted.mean(x = df$race_percentage, w = df$mean_zip_code_population)
  
  # drop rows below weighted median
  df_half <- df %>% filter(race_percentage >= weighted_half)
  
  # weighted 3rd quartile -- weighted median value of top half (weighted based on ZIP code populations)
  weighted_quartile <- weighted.mean(x = df_half$race_percentage, w = df_half$mean_zip_code_population)
  
  return(weighted_quartile)
  
} # EO function