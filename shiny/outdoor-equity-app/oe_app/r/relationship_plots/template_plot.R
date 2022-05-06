
## education x distance traveled and parameters ##

language___plot <- function(admin_unitInput, siteInput,
                            language_top_quartile_df, ridb_df){
  
  print(language_dist_travel_data)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- 
    education_top_quartile_df %>% pmap_dfr(language___data, 
                                           ridb_df = ridb_df, 
                                           siteInput = siteInput)
  
  print(head(plot_data))
  
  # create plot
  language_group_colors <- c("Speak Only<br>English At Home" = "#66c2a5", "Speak Language(s) Other<br>Than English At Home" = "#8da0cb")
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(plot_data) == 0){
    
    print(paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
                 " that come from communities that fall into the high range for languages spoken in the home."))
    
  } else if (nrow(plot_data) > 0){
    
    plotly <- ggplot(data = plot_data, 
                     ...)
    
    ggplotly(plotly,
             ...)
    
  } # EO else if
  
} # EO function