
#' State Visitorshed Map for About Example
#'
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param state_geometries_df State geometries dataframe object name
#'
#' @return Interactive map of visitorshed by state
#'
#' @examples
not_reactive_state_visitorshed_map <- function(site, ridb_df, state_geometries_df){
  
  ## -- data wrangle -- ##
  # reactive dataframe of number of reservations per state
  data <- ridb_df %>% 
      filter(park %in% site) %>%
      group_by(customer_zip_state_full, customer_zip_state) %>%
      summarize(number_reservations = n()) %>%
      filter(!is.na(customer_zip_state_full))
  
  # add geometries
  map_data <-
    state_geometries_df %>%
    left_join(y = data,
              by = c("state_abbrev" = "customer_zip_state"))
  
  
  ## -- create map -- ##
  tmap_mode("view")
  
  tm_shape(map_data) +
    tm_borders(col = "grey", alpha = 0.5) +
    tm_fill(col = "number_reservations",
            title = "Number of Visits",
            palette = "YlGn",
            n = 10,
            style = "jenks",
            id = "customer_zip_state_full",
            popup.vars = c("Total Visits" = "number_reservations")) +
    tm_view(set.view = c(-101.834335, 40.022356, 2)) # update zoom
  
}
