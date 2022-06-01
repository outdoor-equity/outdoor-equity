#' Education ACS Variable 3rd Quartile Value
#'
#' @param education_group String indicating educational category of interest
#' @param acs_df ACS US Census dataframe object name
#'
#' @return Value of 3rd quartile for specific education category
#'
#' @examples

education_top_quartile <- function(education_group, acs_df){
  
  df <- acs_df %>%
    # filter variables of interest
    select(zip_code, hs_GED_or_below, some_college, college, master_or_above, mean_zip_code_population) %>% 
    pivot_longer(cols = 2:5,
                 names_to = "education",
                 values_to = "education_percentage") %>% 
    # filter category of interest
    filter(education == education_group) %>% 
    drop_na(education_percentage)
  
  # weighted median value (weighted based on ZIP code populations)
  weighted_half <- weighted.mean(x = df$education_percentage, w = df$mean_zip_code_population)
  
  # drop rows below weighted median
  df_half <- df %>% filter(education_percentage >= weighted_half)
  
  # weighted 3rd quartile -- weighted median value of top half (weighted based on ZIP code populations)
  weighted_quartile <- weighted.mean(x = df_half$education_percentage, w = df_half$mean_zip_code_population)
  
  return(weighted_quartile)
  
} # EO function