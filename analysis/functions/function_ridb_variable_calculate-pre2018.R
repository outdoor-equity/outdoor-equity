
#' Variable calculated for RIDB data for use in Outdoor Equity App
#'
#' @param input_df_name Object name of dataframe output by subsetting function
#'
#' @return Dataframe with all variables calculated for use in Outdoor Equity App
#'
#' @examples
RIDB_calculate_pre2018 <- 
  function(input_df_name){
    
    df <- input_df_name %>% 
      ## -- calculate variables -- ##
      mutate(start_date = as.Date(start_date),
             end_date = as.Date(end_date),
             order_date = as.Date(order_date),
             # calculate new variables
             length_of_stay = as.numeric(difftime(end_date, start_date), units = "days"),
             booking_window = as.numeric(difftime(start_date, order_date), units = "days"),
             daily_cost = total_paid / length_of_stay,
             daily_cost_per_visitor = daily_cost / number_of_people,
             # create column for administrative unit
             admin_unit = case_when(agency == "USFS" ~ parent_location,
                                    agency %in% c("NPS", "BOR", "USACE") ~ region_description),
             # aggregate site type
             aggregated_site_type = 
               case_when(site_type %in% c("walk to",
                                          "hike to",
                                          "group hike to",
                                          "group walk to") ~ "remote",
                         site_type %in% c("cabin nonelectric",
                                          "cabin electric",
                                          "yurt",
                                          "shelter nonelectric") ~ "shelter",
                         site_type %in% c("boat in",
                                          "anchorage") ~ "water",
                         site_type %in% c("group equestrian",
                                          "equestrian nonelectric") ~ "equestrian",
                         site_type %in% c("rv nonelectric",
                                          "rv electric",
                                          "group rv area nonelectric") ~ "rv only",
                         site_type %in% c("group standard nonelectric",
                                          "standard nonelectric",
                                          "standard electric",
                                          "group standard area nonelectric",
                                          "group standard electric") ~ "rv or tent",
                         site_type %in% c("tent only nonelectric",
                                          "group tent only area nonelectric",
                                          "tent only electric") ~ "tent only")
      ) %>% # close mutate for creating new variables
      select(!c("parent_location", "region_description", "site_type")) %>% 
      select("agency", "admin_unit", "park", "aggregated_site_type", "facility_state", 
             "facility_longitude", "facility_latitude", "customer_zip", "total_paid", 
             "start_date", "end_date", "order_date", "number_of_people", 
             "length_of_stay", "booking_window", "daily_cost", "daily_cost_per_visitor") %>% 
      ## -- edit values -- ##
      mutate(
        # agency abbreviations to names
        agency = str_replace(string = agency,
                             pattern = "NPS",
                             replacement = "National Park Service"),
        agency = str_replace(string = agency,
                             pattern = "USFS", 
                             replacement = "US Forest Service"),
        agency = str_replace(string = agency,
                             pattern = "USACE",
                             replacement = "US Army Corps of Engineers"),
        agency = str_replace(string = agency,
                             pattern = "BOR",
                             replacement = "Bureau of Reclamation"),
        # update admin_unit values (generic)
        admin_unit = str_replace(string = admin_unit,
                                 pattern = paste(c("NF - FS", "NF -FS", "NF- FS", 
                                                   "NF-FS", "-FS", " - FS"), 
                                                 collapse = "|"),
                                 replacement = "National Forest"),
        admin_unit = str_to_title(admin_unit),
        admin_unit = str_replace(string = admin_unit,
                                 pattern = "And",
                                 replacement = "&"),
        # update park values (generic)
        park = str_remove(string = park,
                          pattern = paste(c("\\(.*", " \\(.*",
                                            "---.*", " ---.*",
                                            ",.*"), collapse = "|")),
        park = str_to_title(park),
        park = str_replace(string = park,
                           pattern = "Cg",
                           replacement = "Campground"),
        park = str_replace(string = park,
                           pattern = "Nhp",
                           replacement = "National Historic Park"),
        park = str_replace(string = park,
                           pattern = "@",
                           replacement = "At"),
        park = str_replace(string = park,
                           pattern = "&",
                           replacement = "And"),
        park = str_replace(string = park,
                           pattern = paste(c("/", " / "), collapse = "|"),
                           replacement = " "),
        park = str_remove_all(string = park,
                              pattern = " \\d.*"),
        # update park values (CA specific)
        park = str_remove(string = park,
                          pattern = paste(c(" - Angeles Nf", " -Hwy"), 
                                          collapse = "|")),
        park = str_replace(string = park,
                           pattern = "Tunnel Mills Il",
                           replacement = "Tunnel Mills")
      ) # close mutate for update values
    
    
    ## -- calculate distance traveled -- ##
    
    # bootstrap geometries and reproject to NAD 83
    df_geometries <- df %>% 
      st_as_sf(coords = c("facility_longitude", "facility_latitude"),
               crs = 4326) %>% 
      st_transform(crs = 4269) # using NAD83 because measured in meters
    
    # get centroid of geometries for all US ZIP codes 
    df_zip_centroids_us <- get_acs(geography = "zcta", year = 2018, geometry = TRUE, 
                                   summary_var = "B01001_001",
                                   survey = "acs5",
                                   variables = c(male = "B01001_002")) %>% 
      select(NAME, geometry) %>% 
      mutate(zip_code = str_sub(NAME, start = -5, end = -1)) %>% 
      select(zip_code, geometry) %>% 
      st_centroid()
    
    # join data and calculate `distance_traveled` variable
    df_joined_geometries <- 
      left_join(x = df_geometries %>% as.data.frame(),
                y = df_zip_centroids_us %>% as.data.frame(), 
                by = c("customer_zip" = "zip_code")) %>%
      st_sf(sf_column_name = 'geometry.x') %>% 
      mutate(distance_traveled_m = st_distance(x = geometry.x, 
                                               y = geometry.y,
                                               by_element = TRUE),
             distance_traveled_m = as.numeric(distance_traveled_m)) 
    
    # convert back to data.frame (from sf data.frame), remove geometries
    df_joined <- df_joined_geometries %>% 
      as.data.frame() %>% 
      extract(col = geometry.x, 
              into = c('facility_latitude', 'facility_longitude'), 
              regex = '\\((.*), (.*)\\)', 
              convert = TRUE, remove = TRUE) %>% 
      select(-geometry.y)
    
    
    ## -- add state for each ZIP code -- ##
    # create df of fips and full state names
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
    
    # add full state name and fips code to list of all ZIP codes for each state
    df_states_fips_zip_codes <- 
      left_join(x = df_states_zip_codes,
                y = df_states_fips,
                by = "state") %>% 
      select(-fips) %>% 
      rename(customer_zip_state = state,
             customer_zip_state_full = state_full,
             zip_code = zipcode)
    
    
    ## -- create final dataframe -- ##
    df_final <- df_joined %>% 
      left_join(y = df_states_fips_zip_codes,
                by = c("customer_zip" = "zip_code")) %>% 
      relocate(customer_zip_state, .after = customer_zip) %>% 
      relocate(customer_zip_state_full, .after = customer_zip_state)
    
    # create df
    return(df_final)
  }
