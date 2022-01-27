
RIDB_cleaning_pre2018 <- function(full_file_path, state_2letter, df_name) {
  # variables to keep
  select_columns <- c("historical_reservation_id",
                         "order_number",
                         "agency",
                         "org_id",
                         "region_code", "region_description",
                         "parent_location_id", "parent_location",
                         "site_type", "use_type",
                         "product_id", 
                         "facility_id", "facility_zip", "facility_state", "facility_longitude", "facility_latitude",
                         "customer_zip", 
                         "total_before_tax", "total_paid",
                         "start_date", "end_date", "order_date",
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
    filter(facility_state == state_2letter) %>% 
    mutate(site_type = tolower(site_type)) %>% 
    filter(!site_type %in% rm_sitetype) %>% 
    filter(use_type == "Overnight") %>%
    filter(customer_zip == str_extract_all(customer_zip, "[[:digit:]]{5}"))
  # create df
  assign(paste(df_name), data.frame(df), envir = .GlobalEnv)
}

# test function
# library(tidyverse)
# library(janitor)
# library(here)
# 
# RIDB_cleaning_pre2018(full_file_path = "../../../capstone/outdoorequity/data/2017.csv", state = "CA",
#                       df_name = "RIDB_CA_2017")

