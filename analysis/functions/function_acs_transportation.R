
acs_subset_calculate_transportation <- 
  function(geography, # string indicating grouping for census data 
           #("zcta" = zip code, all options found here: https://walker-data.com/tidycensus/articles/basic-usage.html)
           year, # year of census datat
           state # string indicating state to be include, use NULL for all US states
  ){
    df <- 
      get_acs(geography = geography,
              year = year,
              state = state,
              summary_var = "B08141_001", #Estimate!!Total:
              variables = c(
                no_vehicle = "B08141_002" # Estimate!!Total:!!No vehicle available
              )) %>% 
      clean_names() %>% 
      mutate(zip_code = str_sub(name, start = -5, end = -1)) %>% 
      rename(no_vehicle = variable)
    # create df
    if (is.null(state)) {
      assign(x = paste0("data_acs_", year, "_transportation"), 
             data.frame(df), envir = .GlobalEnv) 
    } else {
      assign(x = paste0("data_acs_", year, "_transportation_", state), 
             data.frame(df), envir = .GlobalEnv)
    }
    
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
    if (is.null(state)) {
      assign(paste0("data_acs_", year, "_transportation_percent"), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    } else {
      assign(paste0("data_acs_", year, "_transportation_percent_", state), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    }
  }