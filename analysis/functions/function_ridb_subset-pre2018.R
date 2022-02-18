
RIDB_subset_pre2018 <- function(full_file_path, state_abbrev, year) {
  # read in file
  ridb_data <- read_csv(here::here(full_file_path))
  # clean data
  df <- ridb_data %>% 
    janitor::clean_names() %>% 
    # filter for state
    filter(facility_state == state_abbrev) %>%
    # select variables
    select(c("agency",
             "region_description",
             "parent_location",
             "park",
             "site_type", 
             "use_type",
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
    filter(use_type == "Overnight") %>%
    # filter out invalid ZIP codes
    filter(customer_zip == str_extract_all(customer_zip, "[[:digit:]]{5}")) %>% 
    # remove use type column
    select(!use_type)
  
  # create df
  assign(paste0("data_ridb_", year), data.frame(df), envir = .GlobalEnv)
}


