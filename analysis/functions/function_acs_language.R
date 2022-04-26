
#' American Communities Surveys Language Spoken at Home Percentages
#'
#' @param geography String indicating geographic grouping for census data. Defaults to "zcta" (ZIP Code Tabulation Area). 
#'     See all geographic options within the `tidycensus` package here: https://walker-data.com/tidycensus/articles/basic-usage.html
#' @param state String indicating state to be included, use NULL to get all US states.
#'
#' @return
#'
#' @examples
acs_subset_calculate_language <- function(geography = "zcta", state){
    # read in raw data
    df <- 
      get_acs(geography = geography,
              year = 2015, # closest year to 2018 that doesn't pull all NA values
              state = state,
              survey = "acs5",
              summary_var = "B16001_001", # Estimate!!Total:
              variables = c(
                english_only = "B16001_002" # Estimate!!Total!!Speak only English
              )) %>% 
      clean_names() %>% 
      rename(language = variable) %>% 
      # create ZIP column with just 5 digit numbers
      mutate(zip_code = str_sub(name, start = -5, end = -1))
    # calculate percentage
    df_percent <- 
      df %>% 
      group_by(zip_code, language) %>% 
      summarize(estimate = sum(estimate),
                moe = sum(moe),
                summary_est = unique(summary_est),
                summary_moe = unique(summary_moe),
                percent = estimate / summary_est)
    # create column for each percentage for each group (pivot wider)
    # necessary to be able to left_join() with RIDB data
    df_percent_wider <- 
      df_percent %>% 
      select(zip_code, summary_est, language, percent) %>% 
      rename(zip_code_population = summary_est) %>% 
      pivot_wider(names_from = "language",
                  values_from = "percent") %>% 
      mutate(not_english_only = 1 - english_only)
    
    # create df
    if (is.null(state)) {
      assign(paste0("data_acs_", "2020", "_language_percent"), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    } else {
      assign(paste0("data_acs_", "2020", "_language_percent_", state), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    }
  }