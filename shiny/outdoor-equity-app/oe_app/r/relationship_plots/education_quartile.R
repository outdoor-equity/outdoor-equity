
race_top_quartile <- function(education_group, acs_df){
  
  df_ca_bin_breaks <- acs_df %>%
    select(zip_code, hs_GED_or_below, some_college, college, master_or_above, mean_zip_code_population) %>% 
    pivot_longer(cols = 2:5,
                 names_to = "education",
                 values_to = "education_percentage") %>% 
    filter(education == education_group) %>% 
    drop_na(education_percentage)
  
  weighted_half <- weighted.mean(x = df_ca_bin_breaks$education_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
  
  df_ca_bin_breaks <- df_ca_bin_breaks %>% filter(education_percentage >= weighted_half)
  
  weighted_quartile <- weighted.mean(x = df_ca_bin_breaks$education_percentage, w = df_ca_bin_breaks$mean_zip_code_population)
  
  return(weighted_quartile)
  
} # EO function