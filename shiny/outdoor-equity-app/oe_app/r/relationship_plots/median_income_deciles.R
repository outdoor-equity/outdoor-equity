
## list of cut-off values for deciles bins of CA census median-income --

# NOTE: save to object called `median_income_binned` in server to work with other function

median_income_deciles <- function(acs_df){
  
  unweighetd <- acs_df %>%
    select(zip_code, median_income, mean_zip_code_population) %>% 
    drop_na(median_income) %>%  
    mutate(mean_zip_code_population = round(mean_zip_code_population, 0)) %>% 
    uncount(mean_zip_code_population)
  
  deciles <- quantile(unweighetd$median_income, probs = seq(0, 1, 1/10))
  
  return(deciles)
  
} # EO function