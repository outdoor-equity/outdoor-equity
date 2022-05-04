
## education x distance traveled and parameters ##

education___plot <- function(admin_unitInput, siteInput,
                        education_top_quartile_df, ridb_df){
  
  print(race_dist_travel_data)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  plot_data <- 
    race_top_quartile_df %>% pmap_dfr(race___data, 
                                      ridb_df = ridb_df, 
                                      siteInput = siteInput)
  
  print(head(plot_data))
  
  # create plot
  race_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(plot_data) == 0){
    
    print(paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
                 " that come from communities that fall into the high range for any racial groups."))
    
  } else if (nrow(plot_data) > 0){
    
    plotly <- ggplot(data = plot_data, 
                     ...)
    
    ggplotly(plotly,
             tooltip = list("text")) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                           "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
      add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                      x = -0.1, xref = 'paper', y = 0.95, yref = 'paper', 
                      showarrow = FALSE)
    
  } # EO else if
  
} # EO function