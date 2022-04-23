
race_top_quartile <- function(race_group = c("other", "pacific_islander", "multiracial", "asian", 
                                             "black", "white", "native_american", "hispanic_latinx"), 
                              acs_df){
  
  df_ca_bin_breaks <- acs_df %>%
    select(zip_code, asian, black, hispanic_latinx, multiracial,
           native_american, other, pacific_islander, white, mean_zip_code_population) %>%
    pivot_longer(cols = 2:9,
                 names_to = "race",
                 values_to = "race_percentage") %>%
    filter(race == race_group) %>% 
    drop_na(race_percentage)
  
  weighted_half <- weighted.mean(x = df_ca_bin_breaks$race_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
  
  df_ca_bin_breaks <- df_ca_bin_breaks %>% filter(race_percentage >= weighted_half)
  
  weighted_quartile <- weighted.mean(x = df_ca_bin_breaks$race_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
  
  return(weighted_quartile)
  
} # EO function