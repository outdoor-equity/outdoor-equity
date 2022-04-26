
#' American Communities Surveys Education Percentages
#'
#' @param geography String indicating geographic grouping for census data. Defaults to "zcta" (ZIP Code Tabulation Area). 
#'     See all geographic options within the `tidycensus` package here: https://walker-data.com/tidycensus/articles/basic-usage.html
#' @param year Year of census data
#' @param state String indicating state to be included, use NULL to get all US states. 
#'
#' @return
#'
#' @examples
acs_subset_calculate_education <- 
  function(geography = "zcta", year, state){
    # read in raw data
    df <- 
      get_acs(geography = geography,
              year = year,
              state = state,
              survey = "acs5",
              summary_var = "B15003_001", # Estimate!!Total:
              variables = c(
                # Estimate!!Total:!!GED or alternative credential or below (including above)
                hs_GED_or_below = "B15003_002", 
                hs_GED_or_below = "B15003_003",  
                hs_GED_or_below = "B15003_004", 
                hs_GED_or_below = "B15003_005", 
                hs_GED_or_below = "B15003_006", 
                hs_GED_or_below = "B15003_007", 
                hs_GED_or_below = "B15003_008", 
                hs_GED_or_below = "B15003_009",
                hs_GED_or_below = "B15003_010", 
                hs_GED_or_below = "B15003_011", 
                hs_GED_or_below = "B15003_012", 
                hs_GED_or_below = "B15003_013",
                hs_GED_or_below = "B15003_014", 
                hs_GED_or_below = "B15003_015", 
                hs_GED_or_below = "B15003_016", 
                hs_GED_or_below = "B15003_017",
                hs_GED_or_below = "B15003_018", 
                # Estimate!!Total:!!Some college
                some_college = "B15003_019", 
                some_college = "B15003_020", 
                # Estimate!!Total:!!Associates or bachelors degree
                college = "B15003_021", 
                college = "B15003_022", 
                # Estimate!!Total:!!Masters degree and above
                master_or_above = "B15003_023", 
                master_or_above = "B15003_024", 
                master_or_above = "B15003_025" 
              )) %>% 
      clean_names() %>% 
      rename(education = variable) %>% 
      # create ZIP column with just 5 digit numbers
      mutate(zip_code = str_sub(name, start = -5, end = -1))
    # calculate percentage
    df_percent <- 
      df %>% 
      group_by(zip_code, education) %>% 
      summarize(estimate = sum(estimate),
                moe = sum(moe),
                summary_est = unique(summary_est),
                summary_moe = unique(summary_moe),
                percent = estimate / summary_est)
    # create column for each percentage for each group (pivot wider)
    # necessary to be able to left_join() with RIDB data
    df_percent_wider <- 
      df_percent %>% 
      select(zip_code, summary_est, education, percent) %>% 
      rename(zip_code_population = summary_est) %>% 
      pivot_wider(names_from = education,
                  values_from = percent)
    
    # create df
    if (is.null(state)) {
      assign(paste0("data_acs_", year, "_education_percent"), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    } else {
      assign(paste0("data_acs_", year, "_education_percent_", state), 
             data.frame(df_percent_wider), envir = .GlobalEnv)
    }
  }