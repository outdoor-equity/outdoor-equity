
#' State Visitorshed Map
#'
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param state_geometries_df State geometries dataframe object name
#'
#' @return Interactive map of visitorshed by state
#'
#' @examples
state_visitorshed_map <- function(siteInput, ridb_df, state_geometries_df){
  print(paste0("You have chosen park: ", siteInput, " on the US visitorshed map and the class is: ", class(siteInput)))
  
  ## -- data wrangle -- ##
  # reactive dataframe of number of reservations per state
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>% 
      filter(park %in% siteInput) %>%
      select(agency, admin_unit, park, customer_zip_state_full, customer_zip_state)
  })
  
  # value of total reservations for this park
  total_reservations <- nrow(rdf())
  
  # number of reservations and % per state
  map_data <- rdf() %>% 
    group_by(customer_zip_state_full, customer_zip_state) %>%
    summarize(number_reservations = n(),
              percentage_reservations = percent((number_reservations / total_reservations), accuracy = 0.01)) %>%
    filter(!is.na(customer_zip_state_full))
  
  # add state geometries
  map_data_geometries <-
    state_geometries_df %>%
    left_join(y = map_data,
              by = c("state_abbrev" = "customer_zip_state")) %>%
    mutate_at(vars(number_reservations), 
              ~replace(number_reservations, is.na(number_reservations), 0)) %>%
    mutate_at(vars(percentage_reservations), 
              ~replace(percentage_reservations, is.na(percentage_reservations), 0)) %>% 
    select(customer_zip_state_full, number_reservations, percentage_reservations, geometry)
  
  
  ## -- create map -- ##
  tmap_mode("view")
  
  tm_shape(map_data_geometries) +
    tm_borders(col = "grey", alpha = 0.5) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "YlGn",
            n = 10,
            style = "jenks",
            id = "customer_zip_state_full",
            popup.vars = c("Total Visits" = "number_reservations",
                           "Percentage of All Visits" = "percentage_reservations")) +
    tm_view(set.view = c(-101.834335, 40.022356, 2)) +
    tmap_options(basemaps = 'https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}')
  
}
