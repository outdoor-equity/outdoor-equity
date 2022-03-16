
RIDB_calculate_pre2018 <- 
  function(input_df_name, output_df_name){
    # update variables
    df <- input_df_name %>% 
      mutate(start_date = as.Date(start_date),
             end_date = as.Date(end_date),
             order_date = as.Date(order_date),
             # calculate new variables
             length_of_stay = as.numeric(difftime(end_date, start_date), units = "days"),
             booking_window = as.numeric(difftime(start_date, order_date), units = "days"),
             daily_cost_per_visitor = (total_paid / number_of_people) / length_of_stay,
             # create column for administrative unit
             regional_area = case_when(agency == "USFS" ~ parent_location,
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
                         site_type %in% c("boat in") ~ "water",
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
             ) %>% 
      select(!c("parent_location", "region_description", "site_type")) %>% # (CB) I think this is repetative, remove?
      select("agency", "regional_area", "park", "aggregated_site_type", "facility_state", 
             "facility_longitude", "facility_latitude", "customer_zip", "total_paid", 
             "start_date", "end_date", "order_date", "number_of_people", 
             "length_of_stay", "booking_window", "daily_cost_per_visitor") %>% 
      # update values
      mutate(
        regional_area = str_replace(string = regional_area,
                                    pattern = paste(c("NF - FS", "NF -FS", "NF- FS", 
                                                      "NF-FS", "-FS", " - FS"), 
                                                    collapse = "|"),
                                    replacement = "National Forest"),
        regional_area = str_to_title(regional_area),
        regional_area = str_replace(string = regional_area,
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
                                          collapse = "|")))
    
    # create df
    assign(paste(output_df_name), data.frame(df), envir = .GlobalEnv)
  }
