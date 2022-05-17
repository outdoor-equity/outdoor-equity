
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
  
  ## -- data wrangle -- ##
  # reactive dataframe of number of reservations per state
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>% 
      filter(park %in% siteInput) %>%
      group_by(customer_zip_state_full) %>%
      summarize(number_reservations = n()) %>%
      filter(!is.na(customer_zip_state_full))
  })
  
  # add geometries
  map_data <-
    state_geometries_df %>%
    left_join(y = rdf(),
              by = c("fips" = "zip_fips"))
  
  
  ## -- create map -- ##
  tm_shape(map_data) +
    tm_borders(col = "grey", alpha = 0.5) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "YlGn",
            n = 10,
            style = "jenks",
            id = "customer_zip_state_full",
            popup.vars = c("State" = "customer_zip_state_full",
                           "Total Visits" = "number_reservations")) +
    tm_view(set.view = c(-101.834335, 40.022356, 3)) # update zoom
  
}
