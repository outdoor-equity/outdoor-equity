
RIDB_cleaning_post2019 <- function(full_file_path, state_full_name, df_name) {
  # variables to keep
  select_columns <- c("historicalreservationid",
                      "ordernumber",
                      "agency",
                      "orgid",
                      "regioncode", "regiondescription",
                      "parentlocationid", "parentlocation",
                      "park",
                      "sitetype", "usetype",
                      "productid", 
                      "inventorytype", 
                      "facilityid", "facilityzip", "facilitystate", "facilitylongitude", "facilitylatitude",
                      "customerzip", 
                      "totalbeforetax", "discount", "totalpaid",
                      "startdate", "enddate", "orderdate",
                      "numberofpeople")
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
    filter(facilitystate == state_full_name) %>% 
    mutate(sitetype = tolower(sitetype)) %>% 
    filter(!sitetype %in% rm_sitetype) %>% 
    filter(usetype == "Overnight") %>%
    filter(customerzip == str_extract_all(customerzip, "[[:digit:]]{5}"))
  # create df
  assign(paste(df_name), data.frame(df), envir = .GlobalEnv)
}

# test function
# library(tidyverse)
# library(janitor)
# library(here)
# 
# RIDB_cleaning_post2019(full_file_path = "../../../capstone/outdoorequity/data/reservations2019.csv", state = "California",
#                       df_name = "RIDB_CA_2019")

