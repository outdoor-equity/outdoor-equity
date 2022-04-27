## site type plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

#' Title
#'
#' @param admin_unitInput 
#' @param siteInput 
#'
#' @return
#'
#' @examples
site_type_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  site_type_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      select(park, aggregated_site_type) %>% 
      mutate(aggregated_site_type = str_to_title(string = aggregated_site_type),
             aggregated_site_type = str_replace(string = aggregated_site_type,
                                                pattern = "Rv",
                                                replacement = "RV"))
    
  })
  
  # parameters
  hist_colors <- c("#009900FF")
  
  # plot for shiny app
  site_type_plotly <- ggplot(data = site_type_rdf() %>%
           group_by(aggregated_site_type) %>%
           summarise(count = n())) +
    geom_col(aes(x = count,
                 y = reorder(aggregated_site_type, count), 
                 text = paste0(..count.., "reservations were made to ", aggregated_site_type, 
                               "sites.", "<br>(", count, " total reservations were made to ", 
                               siteInput, ", ", admin_unitInput, "in 2018.)")),
             fill = hist_colors) +
    scale_x_continuous(labels = comma) +
    labs(x = "Number of Reservations",
         y = "",
         title = paste0("Number of Visits by Site Type for ", 
                        admin_unitInput, ", ", siteInput, "in 2018")) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(site_type_plotly,
           tooltip = list("text"))
  
} # EO function