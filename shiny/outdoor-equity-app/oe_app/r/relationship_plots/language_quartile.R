
race_top_quartile <- function(language_group, acs_df){
  
  df_ca_bin_breaks <- acs_df %>%
    select(zip_code, english_only, not_english_only, mean_zip_code_population) %>% 
    pivot_longer(cols = 2:3,
                 names_to = "language",
                 values_to = "language_percentage") %>% 
    filter(language == language_group) %>% 
    drop_na(language_percentage)
  
  weighted_half <- weighted.mean(x = df_ca_bin_breaks$language_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
  
  df_ca_bin_breaks <- df_ca_bin_breaks %>% filter(language_percentage >= weighted_half)
  
  weighted_quartile <- weighted.mean(x = df_ca_bin_breaks$language_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
  
  return(weighted_quartile)
  
} # EO function