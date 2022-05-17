
#' Variable calculated for RIDB data for use in Outdoor Equity App
#'
#' @param input_df_name Object name of dataframe output by subsetting function
#'
#' @return Dataframe with all variables calculated for use in Outdoor Equity App
#'
#' @examples
RIDB_calculate_pre2018 <- 
  function(input_df_name){
    ## update variables
    df <- input_df_name %>% 
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
      # update values
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
    
    ## calculate distance traveled
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
    
    ## create final dataframes
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
    
    # create df
    return(df_joined)
  }
