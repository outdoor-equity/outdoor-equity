
#' Subset RIDB Data for Use in Outdoor Equity Shiny App
#'
#' @param full_file_path String of full file path to RIDB CSV file of raw data (from directory where .Rproj is saved)
#' @param state_abbrev String of state abbreviation for subsetting (ex: "CA")
#' @param year Year for subsetting (ex: 2018)
#'
#' @return Data frame for called state and year including only overnight sites, with valid US ZIP codes 
#'     (ZIP codes updated to include only 5 digits)
#'
#' @examples
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
             "facility_id",
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
  return(df)
}


