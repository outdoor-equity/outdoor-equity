
## education x distance traveled and parameters ##

education___plot <- function(admin_unitInput, siteInput,
                        education_top_quartile_df, ridb_df){
  
  print(education_dist_travel_data)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- 
    education_top_quartile_df %>% pmap_dfr(education___data, 
                                      ridb_df = ridb_df, 
                                      siteInput = siteInput)
  
  print(head(plot_data))
  
  # create plot
  education_group_colors <- c("HS, GED,\nor Below" = "#a6cee3", "Some College or\nTrade School"  = "#1f78b4", 
                              "Associates or\nBachelors Degree" = "#b2df8a", "Masters Degree\nor Above" = "#33a02c")
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(plot_data) == 0){
    
    print(paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
                 " that come from communities that fall into the high range for any educational level."))
    
  } else if (nrow(plot_data) > 0){
    
    plotly <- ggplot(data = plot_data, 
                     ...)
    
    ggplotly(plotly,
             ...)
    
  } # EO else if
  
} # EO function