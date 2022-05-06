
## education x distance traveled and parameters ##

median_income_dist_travel_plot <- function(admin_unitInput, siteInput, ridb_df){
  
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- ...(ridb_df = ridb_df, siteInput = siteInput)
  
  print(head(plot_data))
  
  # create plot
  
  # create plot (or say no such site type if none exist at siteInput)
  plotly <- ggplot(data = plot_data, 
                   ...)
  
  ggplotly(plotly,
           ...)

} # EO function