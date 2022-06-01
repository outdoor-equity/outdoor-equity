
#' American Communities Surveys Racial Grouping Percentages
#'
#' @param geography String indicating geographic grouping for census data. Defaults to "zcta" (ZIP Code Tabulation Area). 
#'     See all geographic options within the `tidycensus` package here: https://walker-data.com/tidycensus/articles/basic-usage.html
#' @param year Year of census data
#' @param state String indicating state to be included, use NULL to get all US states.
#'
#' @return 
#'
#' @examples
acs_subset_calculate_race <- function(geography = "zcta", year, state){
    # read in raw data
    df <- 
      get_acs(geography = geography,
              year = year,
              state = state,
              survey = "acs5",
              summary_var = "B03002_001", #Estimate!!Total: 
              variables = c(
                white = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone
                black = "B03002_004", # Estimate!!Total:!!Not Hispanic or Latino!!Black or African American alone
                native_american = "B03002_005", # Estimate!!Total:!!Not Hispanic or Latino!!American Indian and Alaska Native alone
                asian = "B03002_006", # Estimate!!Total:!!Not Hispanic or Latino!!Asian alone
                pacific_islander = "B03002_007", # Estimate!!Total:!!Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone
                other = "B03002_008", # Estimate!!Total:!!Not Hispanic or Latino!!Some other race alone
                multiracial = "B03002_009", # Estimate!!Total:!!Not Hispanic or Latino!!Two or more races
                hispanic_latinx = "B03002_012" # Estimate!!Total!!Hispanic or Latino
              )) %>% 
      clean_names() %>% 
      rename(race = variable) %>% 
      # create column of 5 digit ZIP code
      mutate(zip_code = str_sub(name, start = -5, end = -1))
    # calculate percentage
    df_percent <- 
      df %>% 
      group_by(zip_code, race) %>% 
      summarise(estimate = sum(estimate),
                moe = sum(moe),
                summary_est = unique(summary_est),
                summary_moe = unique(summary_moe),
                percent = estimate / summary_est)
    # create column for each percentage for each group (pivot wider)
    # necessary to be able to left_join() with RIDB data
    df_percent_wider <- 
      df_percent %>% 
      select(zip_code, summary_est, race, percent) %>% 
      rename(zip_code_population = summary_est) %>% 
      pivot_wider(names_from = "race",
                  values_from = "percent")
    
    # create df
    if (is.null(state)) {
      assign(paste0("data_acs_", year, "_race_percent"), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    } else {
      assign(paste0("data_acs_", year, "_race_percent_", state), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    }
  }

