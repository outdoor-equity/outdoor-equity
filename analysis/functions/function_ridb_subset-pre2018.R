
RIDB_subset_pre2018 <- function(full_file_path, state_abbrev, year) {
  # read in file
  ridb_data <- read_csv(here::here(full_file_path))
  # clean data
  df <- ridb_data %>% 
    janitor::clean_names() %>% 
    # filter for state
    filter(facility_state == state_abbrev) %>%
    # filter for use type
    filter(use_type == "Overnight") %>% 
    # select variables
    select(c("agency",
             "parent_location",
             "region_description",
             "park",
             "site_type",
             "facility_state", 
             "facility_longitude", 
             "facility_latitude",
             "customer_zip", 
             "total_paid",
             "start_date",
             "end_date", 
             "order_date",
             "number_of_people")) %>% 
    mutate(site_type = tolower(site_type)) %>% 
    filter(!site_type %in% c("historic tour",
                             "hiking zone",
                             "group picnic area",
                             "cave tour",
                             "management",
                             "anchorage",
                             "picnic",
                             "entry point",
                             "trailhead")) %>%
    # filter out invalid ZIP codes
    filter(str_detect(string = customer_zip, 
                      pattern = "^[:digit:]{5}(?!.)") | 
             str_detect(string = customer_zip, 
                        pattern = "^[:digit:]{5}(?=-)")) %>% 
    filter(!customer_zip %in% c("00000", "99999")) %>% 
    mutate(customer_zip = str_extract(string = customer_zip,
                                      pattern = "[:digit:]{5}"))
  
  # create df
  assign(paste0("data_ridb_", year), data.frame(df), envir = .GlobalEnv)
}


