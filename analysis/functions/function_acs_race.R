
acs_subset_calculate_race <- 
  function(geography, # string indicating grouping for census data 
           #("zcta" = zip code, all options found here: https://walker-data.com/tidycensus/articles/basic-usage.html)
           year, # year of census datat
           geometry, # FALSE/TRUE include geometries of geography areas
           state # string indicating state to be include, use NULL for all US states
  ){
    # read in raw data
    df <- 
      get_acs(geography = geography,
              year = year,
              geometry = geometry,
              summary_var = "B02001_001", #Estimate!!Total:
              variables = c(
                white = "B02001_002", # Estimate!!Total:!!White alone (INCLUDING HISPANIC AND/OR LATINO)
                black = "B02001_003", # Estimate!!Total:!!Black or African American alone
                native_american = "B02001_004", # Estimate!!Total:!!American Indian and Alaska Native alone
                asian = "B02001_005", # Estimate!!Total:!!Asian alone
                pacific_islander = "B02001_006", # Estimate!!Total:!!Native Hawaiian and Other Pacific Islander alone
                other = "B02001_007", # Estimate!!Total:!!Some other race alone
                multiracial = "B02001_008" # Estimate!!Total:!!Two or more races
              )) %>% 
      clean_names() %>% 
      rename(race = variable) %>% 
      mutate(zip_code = str_sub(name, start = -5, end = -1)) %>% 
      # create df
      if (state = NULL) {
        assign(x = paste0("data_acs_", year, "_race"), 
               data.frame(df), envir = .GlobalEnv)
      } else {assign(x = paste0("data_acs_", year, "_race_", state), 
                     data.frame(df), envir = .GlobalEnv)
      }
    
    # calculate percentage
    df_percent <- 
      df %>% 
      group_by(zip_code, race) %>% 
      summarise(percent = estimate / summary_est)
    
    # create column for each percentage for each group (pivot wider)
    # necessary to be able to left_join() with RIDB data
    df_percent_wider <- 
      df_percent %>% 
      select(zip_code, race, percent) %>% 
      pivot_wider(names_from = "race",
                  values_from = "percent")
    # create df
    if (state = NULL) {
      assign(paste0("data_acs_", year, "_race_percent"), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    } else {
      assign(paste0("data_acs_", year, "_race_percent_", state), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    }
  }


