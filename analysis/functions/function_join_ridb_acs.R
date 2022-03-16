
join_ridb_acs_data <- 
  function(ridb_df, acs_df_race, acs_df_education, 
           acs_df_median_income, acs_df_transportation,
           output_df_string){
    # join input dfs
    df <- 
      left_join(x = ridb_df,
                y = acs_df_race,
                by = c("customer_zip" = "zip_code")) %>% 
      left_join(y = acs_df_education,
                by = c("customer_zip" = "zip_code")) %>% 
      left_join(y = acs_df_median_income,
                by = c("customer_zip" = "zip_code")) %>% 
      left_join(y = acs_df_transportation,
                by = c("customer_zip" = "zip_code"))
    
    # create df
    assign(paste(output_df_string), data.frame(df), envir = .GlobalEnv)
  }