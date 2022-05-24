
#' RIDB Reservations Above "High" Threshold for Median-Income
#'
#' @param median_income_binned List of decile values
#' @param joined_ridb_acs_df joined RIDB / ACS dataframe object name
#'
#' @return Dataframe of all reservations group into deciles
#'
#' @examples
median_income_ridb_deciles <- function(median_income_binned, joined_ridb_acs_df){
  
  df <- joined_ridb_acs_df %>% 
    # select variables of interest
    select(park, customer_zip, 
           median_income, 
           booking_window, daily_cost, daily_cost_per_visitor,
           distance_traveled_m, length_of_stay, aggregated_site_type) %>% 
    # convert distance traveled meters to miles
    mutate(distance_traveled_mi = distance_traveled_m * 0.000621371) %>% 
    select(!distance_traveled_m) %>% 
    # split data into median-income decile groups
    mutate(median_income_binned = factor(case_when(median_income <= median_income_binned[[2]] ~ 
                                                     paste(dollar(median_income_binned[[1]]), "-", dollar(median_income_binned[[2]])),
                                                   median_income > median_income_binned[[2]] & median_income <= median_income_binned[[3]] ~ 
                                                     paste(dollar(median_income_binned[[2]]), "-", dollar(median_income_binned[[3]])),
                                                   median_income > median_income_binned[[3]] & median_income <= median_income_binned[[4]] ~ 
                                                     paste(dollar(median_income_binned[[3]]), "-", dollar(median_income_binned[[4]])),
                                                   median_income > median_income_binned[[4]] & median_income <= median_income_binned[[5]] ~ 
                                                     paste(dollar(median_income_binned[[4]]), "-", dollar(median_income_binned[[5]])),
                                                   median_income > median_income_binned[[5]] & median_income <= median_income_binned[[6]] ~ 
                                                     paste(dollar(median_income_binned[[5]]), "-", dollar(median_income_binned[[6]])),
                                                   median_income > median_income_binned[[6]] & median_income <= median_income_binned[[7]] ~ 
                                                     paste(dollar(median_income_binned[[6]]), "-", dollar(median_income_binned[[7]])),
                                                   median_income > median_income_binned[[7]] & median_income <= median_income_binned[[8]] ~ 
                                                     paste(dollar(median_income_binned[[7]]), "-", dollar(median_income_binned[[8]])),
                                                   median_income > median_income_binned[[8]] & median_income <= median_income_binned[[9]] ~ 
                                                     paste(dollar(median_income_binned[[8]]), "-", dollar(median_income_binned[[9]])),
                                                   median_income > median_income_binned[[9]] & median_income <= median_income_binned[[10]] ~ 
                                                     paste(dollar(median_income_binned[[9]]), "-", dollar(median_income_binned[[10]])),
                                                   median_income > median_income_binned[[10]] ~ 
                                                     paste(dollar(median_income_binned[[10]]), "-", dollar(median_income_binned[[11]]))),
                                         levels = c(paste(dollar(median_income_binned[[1]]), "-", dollar(median_income_binned[[2]])),
                                                    paste(dollar(median_income_binned[[2]]), "-", dollar(median_income_binned[[3]])),
                                                    paste(dollar(median_income_binned[[3]]), "-", dollar(median_income_binned[[4]])),
                                                    paste(dollar(median_income_binned[[4]]), "-", dollar(median_income_binned[[5]])),
                                                    paste(dollar(median_income_binned[[5]]), "-", dollar(median_income_binned[[6]])),
                                                    paste(dollar(median_income_binned[[6]]), "-", dollar(median_income_binned[[7]])),
                                                    paste(dollar(median_income_binned[[7]]), "-", dollar(median_income_binned[[8]])),
                                                    paste(dollar(median_income_binned[[8]]), "-", dollar(median_income_binned[[9]])),
                                                    paste(dollar(median_income_binned[[9]]), "-", dollar(median_income_binned[[10]])),
                                                    paste(dollar(median_income_binned[[10]]), "-", dollar(median_income_binned[[11]])))
    ))
  
  return(df)
}