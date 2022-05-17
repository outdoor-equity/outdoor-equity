
#' California ZIP code data for visitorshed map
#'
#' @param state String indicating full state name to pull ZIP codes for
#' @param year Year to pull ZIP codes from ACS data
#'
#' @return Dataframe of all ZIP codes for indicated state with simplified geometries
#'
#' @examples
zip_ca_dataframe <- function(state = "California", year = 2018){
  
  ## -- ZIP codes and respective states -- ##
  fips_list <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
                 "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
                 "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
                 "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
                 "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", 
                 "56", "72")
  state_list <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                  "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                  "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
                  "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                  "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                  "WY", "PR")
  states_names_list <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                         "Colorado", "Connecticut", "Delaware", "District of Columbia", 
                         "Florida","Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", 
                         "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine","Maryland", 
                         "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
                         "Montana", "Nebraska", "Nevada", "New Hampshire","New Jersey", 
                         "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", 
                         "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island","South Carolina", 
                         "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", 
                         "Washington", "West Virginia", "Wisconsin","Wyoming", "Puerto Rico")
  
  # create dataframe of states
  df_states_fips <- as.data.frame(list(fips = fips_list,
                                       state = state_list,
                                       state_full = states_names_list))
  
  # loop through state df to get all ZIP codes w/in state
  df_states_zip_codes <- data.frame()
  
  for (i in seq_along(fips_list)){
    state <- zipcodeR::search_fips(state_fips = fips_list[[i]]) %>% 
      select(zipcode, state)
    df_states_zip_codes <- rbind(df_states_zip_codes, state)
  }
  # add full state name, fips code, etc. to list of all ZIP codes for each state
  df_states_fips_zip_codes <- 
    left_join(x = df_states_zip_codes,
              y = df_states_fips,
              by = "state") %>% 
    rename(state_abbrev = state,
           zip_code = zipcode) %>% 
    relocate(fips, .before = 2)
  
  
  ## -- CA ZIP geometries -- ##
  df_zip_geometries_ca <- get_acs(geography = "zcta", year = 2018, geometry = TRUE,
                                  state = "California",
                                  summary_var = "B01001_001",
                                  variables = c(male = "B01001_002")) %>%
    select(NAME, geometry) %>%
    mutate(zip_code = str_sub(NAME, start = -5, end = -1)) %>%
    select(zip_code, geometry) %>% 
    rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE) %>% 
    left_join(y = df_states_fips_zip_codes,
              by = "zip_code") %>% 
    relocate(zip_code, .before = 1)
  
  return(df_zip_geometries_ca)
}