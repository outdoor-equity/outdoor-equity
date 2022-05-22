
#' CA ZIP Code Visitorshed Map
#'
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param zip_geometries_df State geometries dataframe object name
#'
#' @return Interactive map of visitorshed by state
#'
#' @examples
ca_zip_code_visitorshed_map <- function(siteInput, ridb_df, zip_geometries_df){
  
  print(paste0("You have chosen park: ", siteInput, " on the CA visitorshed map and the class is: ", class(siteInput)))
  ## -- data wrangle -- ##
  # reactive data frame for siteInput
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>% 
      select(agency, admin_unit, park, customer_zip, facility_latitude, facility_longitude) %>% 
      filter(park %in% siteInput)
  })
  
  # number of reservations per ZIP code
  map_data <- rdf() %>% 
    group_by(customer_zip) %>%
    summarize(number_reservations = n())
  
  # join with ZIP geometries
  map_data_geometries <-
    zip_geometries_df %>% # read in saved RDS file
    left_join(map_data,
              by = c("zip_code" = "customer_zip")) %>%
    mutate_at(vars(number_reservations), 
              ~replace(number_reservations, is.na(number_reservations), 0))  %>% 
    select(zip_code, number_reservations, geometry)
  
  # value of total CA reservations for this park
  total_reservations <- sum(map_data_geometries$number_reservations)
  
  map_data_geometries <- map_data_geometries %>% 
    mutate(percentage_reservations = percent((number_reservations / total_reservations), accuracy = 0.01)) %>%
    mutate_at(vars(percentage_reservations), 
              ~replace(percentage_reservations, is.na(percentage_reservations), 0))
  
  # calculate location of park for point on map
  park_location_geom <- rdf() %>%
    group_by(park) %>%
    summarise(facility_latitude = median(facility_latitude),
              facility_longitude = median(facility_longitude)) %>%
    st_as_sf(coords = c("facility_latitude", "facility_longitude"),
             crs = 4326) %>%
    st_transform(crs = 4269) # using NAD83 because measured in meters
  
  ## -- create map -- ##
  tmap_mode("view")
  
  tm_shape(map_data_geometries) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "PuRd",
            style = "jenks",
            n = 10,
            popup.vars = c("Total Visits" = "number_reservations",
                           "Percentage of All CA Visits" = "percentage_reservations")) +
    tm_shape(park_location_geom) +
    tm_symbols(shape = map_site_icon,
               id = "park") +
    # tm_markers(shape = marker_icon(),
    #            col = "#64863C",
    #            id = "park") +
    tm_view(set.view = c(-119.559917, 37.061753, 6))
  
}
