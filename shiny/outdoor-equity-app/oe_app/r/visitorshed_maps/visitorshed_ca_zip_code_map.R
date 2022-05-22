
#' CA ZIP Code Visitorshed Map
#'
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param ridb_df_geometries RIDB dataframe object name with geometries for parks
#' @param zip_geometries_df State geometries dataframe object name
#'
#' @return Interactive map of visitorshed by state
#'
#' @examples
ca_zip_code_visitorshed_map <- function(siteInput, ridb_df, ridb_df_geometries, zip_geometries_df){
  
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
    mutate(total_reservations = nrow(.)) %>% 
    group_by(customer_zip, total_reservations) %>%
    summarize(number_reservations = n(),
              reservation_percentage = number_reservations / total_reservations) %>%
    mutate(customer_zip = as.character(customer_zip))
  
  # join with ZIP geometries
  map_data_geometries <-
    zip_geometries_df %>% # read in saved RDS file
    left_join(map_data,
              by = c("zip_code" = "customer_zip")) %>%
    select(zip_code, number_reservations, reservation_percentage, geometry) %>% 
    mutate_at(vars(number_reservations), 
              ~replace(number_reservations, is.na(number_reservations), 0)) %>%
    mutate_at(vars(reservation_percentage), 
              ~replace(reservation_percentage, is.na(reservation_percentage), 0))
  
  print(paste0("Zip geometries have been created with:", colnames(map_data_geometries)))
  
  # calculate location of park for point on map
  park_location_geom <- rdf() %>% 
    group_by(park) %>%
      summarise(facility_latitude = median(facility_latitude),
                facility_longitude = median(facility_longitude)) %>%
      st_as_sf(coords = c("facility_latitude", "facility_longitude"),
               crs = 4326) %>%
      st_transform(crs = 4269) # using NAD83 because measured in meters

  
  print(paste0("Park location geometry has been created with:", colnames(park_location_geom)))
  
  ## -- create map -- ##
  tmap_mode("plot")
  
  tm_shape(map_data_geometries) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "PuRd",
            style = "jenks",
            n = 10,
            popup.vars = c("Total Visits" = "number_reservations"#,
                           #"Percentage of All Visits" = "reservation_percentage"
            )) +
    tm_shape(park_location_geom) +
    tm_symbols(shape = map_site_icon,
               id = "park") +
    # tm_markers(shape = marker_icon(),
    #            col = "#64863C",
    #            id = "park") +
    tm_view(set.view = c(-119.559917, 37.061753, 6))
  
}
