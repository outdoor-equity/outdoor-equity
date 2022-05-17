
#' US State data for visitorshed map
#'
#' @param year Year to pull ZIP codes from ACS data
#'
#' @return Dataframe of all ZIP codes for indicated state with simplified geometries
#'
#' @examples
zip_state_dataframe <- function(year = 2018){
  
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
    select(fips, zipcode) %>% 
    rename(zip_code = zipcode)
  
  
  # state geometries for full US
  df_state_geometries_us <- tigris::states(year = 2018) %>%
    select(GEOID, STUSPS, NAME, geometry) %>% 
    rename(fips = GEOID,
           state_abbrev = STUSPS,
           state = NAME) %>% 
    rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE) %>% 
    left_join(y = df_states_fips_zip_codes,
              by = "fips") %>% 
    relocate(zip_code, .before = 1)
  
  return(df_state_geometries_us)
}