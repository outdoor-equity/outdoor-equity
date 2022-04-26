
#' American Communities Surveys Median-Income
#'
#' @param geography String indicating geographic grouping for census data. Defaults to "zcta" (ZIP Code Tabulation Area). 
#'     See all geographic options within the `tidycensus` package here: https://walker-data.com/tidycensus/articles/basic-usage.html
#' @param year Year of census data
#' @param state String indicating state to be included, use NULL to get all US states.
#'
#' @return
#' @export
#'
#' @examples
acs_subset_calculate_median_income <- function(geography = "zcta", year, state){
    df <- 
      get_acs(geography = geography,
              year = year,
              state = state,
              survey = "acs5",
              variables = c(
                median_income = "B19013_001" # Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
              )) %>% 
      clean_names() %>% 
      rename(median_income = estimate) %>% 
      mutate(zip_code = str_sub(name, start = -5, end = -1)) %>% 
      select(median_income, zip_code)
    
    # create df
    if (is.null(state)) {
      assign(x = paste0("data_acs_", year, "_median_income"), 
             data.frame(df), envir = .GlobalEnv)
    } else {
      assign(x = paste0("data_acs_", year, "_median_income_", state), 
             data.frame(df), envir = .GlobalEnv)
    }
  }