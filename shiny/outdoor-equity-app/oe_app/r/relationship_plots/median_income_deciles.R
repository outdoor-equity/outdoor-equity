
#' Median-income ACS Variable Decimal Values
#'
#' @param acs_df ACS US Census dataframe object name
#'
#' @return
#'
#' @examples
#' NOTES: save to object called `median_income_binned` in server to work with other function


median_income_deciles <- function(acs_df){
  
  unweighetd <- acs_df %>%
    # filter variables of interest
    select(zip_code, median_income, mean_zip_code_population) %>% 
    drop_na(median_income) %>%
    # round ZIP population to whole numbers
    mutate(mean_zip_code_population = round(mean_zip_code_population, 0)) %>% 
    # expand to have individual rows equal to 1 for ZIP populations
    uncount(mean_zip_code_population)
  
  # calculated decile values weighed by number of people at each median income level
  deciles <- quantile(unweighetd$median_income, probs = seq(0, 1, 1/10))
  
  return(deciles)
  
} # EO function
