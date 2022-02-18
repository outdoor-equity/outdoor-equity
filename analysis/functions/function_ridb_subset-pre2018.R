
RIDB_subset_pre2018 <- function(full_file_path, state_abbrev, year) {
  # variables to keep
  select_columns <- c("agency",
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
                      "number_of_people")
  # site types to remove
  rm_sitetype <- c("historic tour",
                   "hiking zone",
                   "group picnic area",
                   "cave tour",
                   "management",
                   "anchorage",
                   "picnic",
                   "entry point",
                   "trailhead")
  # read in file
  ridb_data <- read_csv(here::here(full_file_path))
  # clean data
  df <- ridb_data %>% 
    janitor::clean_names() %>% 
    select(select_columns) %>% 
    filter(facility_state == state_abbrev) %>% 
    mutate(site_type = tolower(site_type)) %>% 
    filter(!site_type %in% rm_sitetype) %>% 
    filter(use_type == "Overnight") %>%
    filter(customer_zip == str_extract_all(customer_zip, "[[:digit:]]{5}"))
  # remove use type column
  df <- 
    df %>% 
    select(!use_type)
  # create df
  assign(paste0("data_ridb_", year), data.frame(df), envir = .GlobalEnv)
}


