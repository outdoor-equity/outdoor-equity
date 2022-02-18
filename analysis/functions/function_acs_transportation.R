
acs_subset_calculate_transportation <- 
  function(geography, year, geometry){
    df <- 
      get_acs(geography = geography,
              year = year,
              geometry = geometry,
              summary_var = "B08141_001", #Estimate!!Total:
              variables = c(
                no_vehicle = "B08141_002" # Estimate!!Total:!!No vehicle available
              )) %>% 
      clean_names() %>% 
      mutate(zip_code = str_sub(name, start = -5, end = -1)) %>% 
      rename(no_vehicle = variable)
    # create df
    assign(x = paste0("data_acs_", year, "_transportation"), 
           data.frame(df), envir = .GlobalEnv)
    
    # calculate percentage
    df_percent <- 
      df %>% 
      group_by(zip_code, no_vehicle) %>% 
      summarise(percent = estimate / summary_est)
    # create column for each percentage for each group (pivot wider)
    # necessary to be able to left_join() with RIDB data
    df_percent_wider <- 
      df_percent %>% 
      select(zip_code, no_vehicle, percent) %>% 
      pivot_wider(names_from = "no_vehicle",
                  values_from = "percent")
    # create df
    assign(paste0("data_acs_", year, "_transportation_percent"), 
           data.frame(df_percent_wider), envir = .GlobalEnv)
  }