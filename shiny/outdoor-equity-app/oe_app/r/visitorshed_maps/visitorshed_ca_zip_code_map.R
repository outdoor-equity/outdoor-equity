
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
    summarize(number_reservations = n()) %>%
    mutate(customer_zip = as.character(customer_zip))
  # add geometries
  map_data_geometries <-
    zip_geometries_df %>% # read in saved RDS file
    left_join(map_data,
              by = c("zip_code" = "customer_zip")) %>%
    mutate(number_reservations = ifelse(is.na(number_reservations), 0, number_reservations))
  
  # calculate location of park for point on map
  park_location_geom <- rdf() %>%
    group_by(park) %>%
    summarise(facility_latitude = median(facility_latitude),
              facility_longitude = median(facility_longitude)) %>%
    st_as_sf(coords = c("facility_latitude", "facility_longitude"),
             crs = 4326) %>%
    st_transform(crs = 4269) # using NAD83 because measured in meters
  
  ## -- create map -- ##
  tm_shape(map_data_geometries) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "PuRd",
            style = "jenks",
            n = 10,
            popup.vars = c("Total Visits" = "number_reservations")) +
    tm_shape(park_location_geom) +
    tm_dots(col = "#466C04F", size = 0.1, alpha = 0.9,
            id = "park") +
    tm_view(set.view = c(-119.559917, 37.061753, 6))
  
}
