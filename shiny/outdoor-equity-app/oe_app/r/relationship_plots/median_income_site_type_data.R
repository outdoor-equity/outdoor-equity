
#' Median-income x Site Type Data
#'
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#' @param median_income_binned List of decile values
#'
#' @return Reactive dataframe of all reservations group into deciles
#'
#' @examples

median_income_site_type_data <- function(siteInput, ridb_df, median_income_binned, site_type_string){
  # reactive data frame 
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      # filter to user site of choice
      filter(park %in% siteInput) %>%
      # select to variables of interest
      select(park, customer_zip, median_income, aggregated_site_type) %>% 
      drop_na(median_income) %>% 
      # split data into median-income decile groups
      mutate(median_income_binned = factor(case_when(median_income <= median_income_binned[[2]] ~ 
                                                       paste(dollar(median_income_binned[[1]]), "-", dollar(median_income_binned[[2]])),
                                                     median_income > median_income_binned[[2]] & median_income <= median_income_binned[[3]] ~ 
                                                       paste(dollar(median_income_binned[[2]]), "-", dollar(median_income_binned[[3]])),
                                                     median_income > median_income_binned[[3]] & median_income <= median_income_binned[[4]] ~ 
                                                       paste(dollar(median_income_binned[[3]]), "-", dollar(median_income_binned[[4]])),
                                                     median_income > median_income_binned[[4]] & median_income <= median_income_binned[[5]] ~ 
                                                       paste(dollar(median_income_binned[[4]]), "-", dollar(median_income_binned[[5]])),
                                                     median_income > median_income_binned[[5]] & median_income <= median_income_binned[[6]] ~ 
                                                       paste(dollar(median_income_binned[[5]]), "-", dollar(median_income_binned[[6]])),
                                                     median_income > median_income_binned[[6]] & median_income <= median_income_binned[[7]] ~ 
                                                       paste(dollar(median_income_binned[[6]]), "-", dollar(median_income_binned[[7]])),
                                                     median_income > median_income_binned[[7]] & median_income <= median_income_binned[[8]] ~ 
                                                       paste(dollar(median_income_binned[[7]]), "-", dollar(median_income_binned[[8]])),
                                                     median_income > median_income_binned[[8]] & median_income <= median_income_binned[[9]] ~ 
                                                       paste(dollar(median_income_binned[[8]]), "-", dollar(median_income_binned[[9]])),
                                                     median_income > median_income_binned[[9]] & median_income <= median_income_binned[[10]] ~ 
                                                       paste(dollar(median_income_binned[[9]]), "-", dollar(median_income_binned[[10]])),
                                                     median_income > median_income_binned[[10]] ~ 
                                                       paste(dollar(median_income_binned[[10]]), "-", dollar(median_income_binned[[11]]))),
                                           levels = c(paste(dollar(median_income_binned[[1]]), "-", dollar(median_income_binned[[2]])),
                                                      paste(dollar(median_income_binned[[2]]), "-", dollar(median_income_binned[[3]])),
                                                      paste(dollar(median_income_binned[[3]]), "-", dollar(median_income_binned[[4]])),
                                                      paste(dollar(median_income_binned[[4]]), "-", dollar(median_income_binned[[5]])),
                                                      paste(dollar(median_income_binned[[5]]), "-", dollar(median_income_binned[[6]])),
                                                      paste(dollar(median_income_binned[[6]]), "-", dollar(median_income_binned[[7]])),
                                                      paste(dollar(median_income_binned[[7]]), "-", dollar(median_income_binned[[8]])),
                                                      paste(dollar(median_income_binned[[8]]), "-", dollar(median_income_binned[[9]])),
                                                      paste(dollar(median_income_binned[[9]]), "-", dollar(median_income_binned[[10]])),
                                                      paste(dollar(median_income_binned[[10]]), "-", dollar(median_income_binned[[11]])))
      )) %>% 
      # summarize to total reservations for each site type
      group_by(aggregated_site_type, median_income_binned) %>% 
      summarize(count = n()) %>% 
      filter(aggregated_site_type == site_type_string) %>% 
      mutate(aggregated_site_type = str_replace(string = aggregated_site_type,
                                                pattern = "rv", 
                                                replacement = "RV"))
    
  }) #EO reactive df
  
  return(rdf())
  
} # EO function