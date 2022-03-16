
acs_subset_calculate_median_income <- 
  function(geography, # string indicating grouping for census data 
           #("zcta" = zip code, all options found here: https://walker-data.com/tidycensus/articles/basic-usage.html)
           year, # year of census datat
           state # string indicating state to be include, use NULL for all US states
  ){
    df <- 
      get_acs(geography = geography,
              year = year,
              state = state,
              survey = "acs5",
              variables = c(
                median_income = "B19013_001" # Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
              )) %>% 
      clean_names() %>% 
      rename(median_income = estimate) %>% 
      mutate(zip_code = str_sub(name, start = -5, end = -1)) %>% 
      select(median_income, zip_code)
    
    # create df
    if (is.null(state)) {
      assign(x = paste0("data_acs_", year, "_median_income"), 
             data.frame(df), envir = .GlobalEnv)
    } else {
      assign(x = paste0("data_acs_", year, "_median_income_", state), 
             data.frame(df), envir = .GlobalEnv)
    }
  }