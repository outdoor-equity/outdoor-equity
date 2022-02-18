
RIDB_subset_post2019 <- function(full_file_path, state_full_name, year) {
  # read in file
  ridb_data <- read_csv(here::here(full_file_path))
  # clean data
  df <- ridb_data %>% 
    janitor::clean_names() %>% 
    # filter to state
    filter(facilitystate == state_full_name) %>% 
    # select variables
    select(c("agency", 
             "regiondescription", 
             "parentlocation",
             "park", 
             "sitetype", 
             "usetype", 
             "facilitystate", 
             "facilitylongitude", 
             "facilitylatitude", 
             "customerzip", 
             "totalpaid", 
             "startdate", 
             "enddate", 
             "orderdate", 
             "numberofpeople")) %>% 
    mutate(sitetype = tolower(sitetype)) %>% 
    filter(!sitetype %in% c("historic tour",
                            "hiking zone",
                            "group picnic area",
                            "cave tour",
                            "management",
                            "anchorage",
                            "picnic",
                            "entry point",
                            "trailhead")) %>% 
    filter(usetype == "Overnight") %>%
    filter(customerzip == str_extract_all(customerzip, "[[:digit:]]{5}")) %>% 
    # remove use type column
    select(!usetype)
  
  # create df
  assign(paste("data_ridb_", year), data.frame(df), envir = .GlobalEnv)
}

