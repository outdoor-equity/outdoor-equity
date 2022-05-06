
median_income_dist_travel_data <- function(siteInput, ridb_df){
  # reactive data frame 
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
      filter(park %in% siteInput) %>%
      ...
    
  }) #EO reactive df
  
  return(rdf())
  
} # EO function