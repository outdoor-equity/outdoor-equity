
## race x distance traveled and parameters ##

#' Title
#'
#' @param admin_unitInput 
#' @param siteInput 
#' @param race_top_quartile_df 
#' @param ridb_df 
#' @param site_type_string String listing type of site to include in graph. 
#'     Can be: "equestrian", "remote", "rv only", "rv or tent", "shelter", "tent only", or "water" 
#'
#' @return
#' @export
#'
#' @examples
race_site_type_plot <- function(admin_unitInput, siteInput,
                                  race_top_quartile_df, ridb_df,
                                  site_type_string){
  
  print(race_site_type_plot)
  print(ridb_df)
  print(admin_unitInput)
  print(siteInput)
  
  # iterate to create df for plotting
  data_race_site_type <- 
    race_top_quartile_df %>% pmap_dfr(race_site_type_data, 
                                      ridb_df = ridb_df, 
                                      siteInput = siteInput) %>% 
    filter(aggregated_site_type == site_type_string) %>% 
    mutate(aggregated_site_type = str_to_title(aggregated_site_type),
           aggregated_site_type = str_replace(string = aggregated_site_type,
                                              pattern = "Rv", 
                                              replacement = "RV"))
  
  print(head(data_race_site_type))
  
  ## -- create plot -- ##
  # parameters
  race_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  
  # create plot (or say no such site type if none exist at siteInput)
  if (nrow(data_race_site_type) == 0){
    
    print(paste0("There are either no ", str_to_title(site_type_string) %>% 
                   str_replace(string = ., pattern = "Rv", replacement = "RV"), " 
                 sites at ", siteInput, ", ", admin_unitInput, 
                 "<bror there are no reservations that come from communities that fall into the high range for any racial groups."))
    
  } else if (nrow(data_race_site_type) > 0){
    
    race_site_type_plotly <- ggplot(data = data_race_site_type) +
      geom_col(aes(x = count,
                   y = reorder(race, count),
                   fill = race,
                   text = paste0("Of visits to ", aggregated_site_type, " overnight reservable sites, ", 
                                 comma(count, accuracy = 1), 
                                 " reservations were made by <br>people who live in ZIP codes with high ", 
                                 race, " populations."))) +
      scale_y_discrete(expand = c(0.3, 0)) +
      scale_fill_manual(values = race_group_colors) +
      scale_color_manual(values = race_group_colors) +
      labs(x = paste("Number of Reservations"),
           y = "",
           title = paste0("Number of Reservations to ", str_to_title(site_type_categories[[1]]) %>% 
                            str_replace(string = ., pattern = "Rv", replacement = "RV"),
                          " Sites by <br> People from Communities with High Rates of Different Racial Groups")) + 
      annotate(geom = "text", 
               x = 250, y = 9,
               label = "Reservations from ZIP codes with high proportions of:") +
      theme_minimal() +
      theme(plot.background = element_rect("white"),
            panel.grid.major.y = element_blank(),
            legend.position = "none")
    
    ggplotly(race_site_type_plotly,
             tooltip = list("text")) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                           "hoverClosestCartesian", "hoverCompareCartesian"))  
  } # EO else if
  
} # EO function