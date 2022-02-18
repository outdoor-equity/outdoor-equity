
RIDB_calculate_pre2018 <- 
  function(input_df_name, output_df_name){
    # update variables
    df <- input_df_name %>% 
      mutate(start_date = as.Date(start_date),
             end_date = as.Date(end_date),
             order_date = as.Date(order_date),
             # calculate variables
             length_of_stay = as.numeric(difftime(end_date, start_date), units = "days"),
             booking_window = as.numeric(difftime(start_date, order_date), units = "days"),
             daily_cost_per_visitor = (total_paid / number_of_people) / length_of_stay) %>% 
      # create geometries
      st_as_sf(coords = c("facility_longitude", "facility_latitude"),
               crs = 4326)
    
    # create df
    assign(paste(output_df_name), data.frame(df), envir = .GlobalEnv)
  }

# # check
# RIDB_variable_calculations_pre2018(input_df_name = ridb_2018, output_df_name = "ridb_2018")
