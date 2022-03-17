
joined_park_summary <- 
  function(input_df_name, output_df_string){
    df <- 
      input_df_name %>% 
      select(agency, regional_area, park, facility_latitude, facility_longitude,
             total_paid, number_of_people, length_of_stay, booking_window, 
             daily_cost_per_visitor, distance_traveled_m, asian, black, 
             hispanic_latinx, multiracial, native_american, other, pacific_islander, 
             white, college, hs_GED_or_below, master_or_above, some_college, 
             median_income, no_vehicle, english_only, not_english_only) %>% 
      #filter(daily_cost_per_visitor != "Inf") %>% #drop 48 rows with "Inf" for cost/visitor/day
      pivot_longer(cols = c(facility_latitude, facility_longitude,
                            total_paid, number_of_people, length_of_stay, booking_window, 
                            daily_cost_per_visitor, distance_traveled_m, asian, black, 
                            hispanic_latinx, multiracial, native_american, other, 
                            pacific_islander, white, college, hs_GED_or_below, 
                            master_or_above, some_college, median_income, no_vehicle, 
                            english_only, not_english_only), 
                   names_to = "acs_variable",
                   values_to = "acs_value") %>% 
      group_by(agency, regional_area, park, acs_variable) %>% 
      summarize(median = median(acs_value, na.rm = TRUE),
                mean = mean(acs_value, na.rm = TRUE),
                count = n()) %>% 
      pivot_wider(names_from = acs_variable, 
                  values_from = c(median, mean)) %>% 
      select(-c(median_facility_latitude, median_facility_longitude))
    # create df
    assign(paste(output_df_string), data.frame(df), envir = .GlobalEnv)
  }
