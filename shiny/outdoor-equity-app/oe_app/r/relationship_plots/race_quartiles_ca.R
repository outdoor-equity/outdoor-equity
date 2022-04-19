
race_ca_quartiles <- function(acs_data){
  
  racial_groups <- c("other", "pacific_islander", "multiracial", "asian", 
                     "black", "white", "native_american", "hispanic_latinx")
  
  for (i in seq_along(racial_groups)){
    df_ca_bin_breaks <- data_ca_acs_2018 %>% 
      select(zip_code, asian, black, hispanic_latinx, multiracial, 
             native_american, other, pacific_islander, white, mean_zip_code_population) %>% 
      pivot_longer(cols = 2:9,
                   names_to = "race",
                   values_to = "race_percentage") %>% 
      filter(race == racial_groups[[i]]) %>% ## BACK TO i
      drop_na(race_percentage) 
    
    weighted_half <- weighted.mean(x = df_ca_bin_breaks$race_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
    
    df_ca_bin_breaks <- df_ca_bin_breaks %>% filter(race_percentage >= weighted_half)
    
    weighted_quartile <- weighted.mean(x = df_ca_bin_breaks$race_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
    
    
    assign(paste0("top_quartile_ca_", racial_groups[[i]]), 
           c(weighted_quartile), 
           envir = .GlobalEnv)
  }
}