
zip_state_dataframe <- function(state, year, 
                                fips_list, state_list, state_names_list){
  
  ## -- CA ZIP geometries -- ##
  zip_geometries_ca <- get_acs(geography = "zcta", year = 2018, geometry = TRUE, 
                               state = "California",
                               summary_var = "B01001_001",
                               variables = c(male = "B01001_002")) %>% 
    select(NAME, geometry) %>% 
    mutate(zip_code = str_sub(NAME, start = -5, end = -1)) %>% 
    select(zip_code, geometry) 
  # simplify ZIP geometries for mapping
  zip_geometries_ca <- rmapshaper::ms_simplify(input = zip_geometries_ca$geometry)
  
  
  ## -- state geometries for full US -- ##
  state_geometries_us <- get_acs(geography = "state", year = 2018, geometry = TRUE, 
                                 summary_var = "B01001_001",
                                 variables = c(male = "B01001_002")) %>% 
    select(GEOID, NAME, geometry) %>% 
    rename(fips = GEOID, state = NAME)
  
  ## -- dataframe of ZIP codes and respective states -- ##
  # create dataframe of state abbreviations and FIPS codes
  df_states_fips <- as.data.frame(list(fips = fips_list,
                                       state = state_list,
                                       state_full = states_names_list))
  
  ## -- dataframe of ZIP codes and states they're in -- ##
  df_states_zip_codes <- data.frame()
  
  for (i in seq_along(fips_list)){
    state <- zipcodeR::search_fips(state_fips = fips_list[[i]]) %>% 
      select(zipcode, state)
    df_states_zip_codes <- rbind(df_states_zip_codes, state)
  }
  # join in fips/states
  df_states_fips_zip_codes <- df_states_zip_codes %>% 
    left_join(y = df_states_fips,
              by = "state") %>% 
    rename(state_abbrev = state) %>% 
    relocate(fips, .before = 2)
  
  
  ## -- data for plotting -- ##
  data_map_us <- data_ridb_acs_2018 %>% 
    left_join(y = df_states_fips_zip_codes,
              by = c("customer_zip" = "zipcode")) %>% 
    group_by(zip_state_abbr, zip_fips) %>% 
    summarize(number_reservations = n()) %>% 
    filter(!is.na(zip_state_abbr))
  
  data_map_geometries_us <- 
    state_geometries_us %>% 
    left_join(y = data_map_us_yosemite_upper_pines,
              by = c("fips" = "zip_fips"))
  
  
  return()
}