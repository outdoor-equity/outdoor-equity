
acs_subset_calculate_median_income <- 
  function(geography, year, geometry){
    df <- 
      get_acs(geography = geography,
              year = year,
              geometry = geometry,
              variables = c(
                median_income = "B19013_001" # Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
              )) %>% 
      clean_names() %>% 
      rename(median_income = estimate) %>% 
      mutate(zip_code = str_sub(name, start = -5, end = -1)) %>% 
      select(median_income, zip_code)
    
    # create df
    assign(x = paste0("data_acs_", year, "_median_income"), 
           data.frame(df), envir = .GlobalEnv)
  }