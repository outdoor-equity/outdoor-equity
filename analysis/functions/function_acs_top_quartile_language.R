
#' Language ACS Variable 3rd Quartile Value
#'
#' @param language_group String indicating language category of interest
#' @param acs_df ACS US Census dataframe object name
#'
#' @return Value of 3rd quartile for specific language category
#'
#' @examples


language_top_quartile <- function(language_group, acs_df){
  
  df <- acs_df %>%
    # filter variables of interest
    select(zip_code, english_only, not_english_only, mean_zip_code_population) %>% 
    pivot_longer(cols = 2:3,
                 names_to = "language",
                 values_to = "language_percentage") %>% 
    # filter category of interest
    filter(language == language_group) %>% 
    drop_na(language_percentage)
  
  # weighted median value (weighted based on ZIP code populations)
  weighted_half <- weighted.mean(x = df$language_percentage, w = df$mean_zip_code_population)
  
  # drop rows below weighted median
  df_half <- df %>% filter(language_percentage >= weighted_half)
  
  # weighted 3rd quartile -- weighted median value of top half (weighted based on ZIP code populations)
  weighted_quartile <- weighted.mean(x = df_half$language_percentage, w = df_half$mean_zip_code_population)
  
  return(weighted_quartile)
  
} # EO function