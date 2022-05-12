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
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      select(park, aggregated_site_type) %>% 
      mutate(aggregated_site_type = str_to_title(string = aggregated_site_type),
             aggregated_site_type = str_replace(string = aggregated_site_type,
                                                pattern = "Rv",
                                                replacement = "RV")) %>% 
      count(aggregated_site_type)
    
  })
  
  # parameters
  hist_colors <- c("#64863C")
  
  # if statement for an empty df
  col <- geom_col()
  if (dim(site_type_rdf()) != 0) {

    col <- geom_col(aes(x = n/sum(n), 
                        y = reorder(aggregated_site_type, n), 
                        text = paste0(percent(n/sum(n), accuracy = 1), " of reservations were made to ", 
                                      aggregated_site_type, " sites", 
                                      "<br>",
                                      "(Total reservations to site: ",
                                      comma(sum(n), accuracy = 1), ")")),
                    fill = hist_colors)
    
  } # EO if statement for geom_col
  
  # plot for shiny app
  plotly <- ggplot(data = site_type_rdf()) +
    col + # object contains geom_col 
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_discrete(expand = c(0.7, 0)) +
    labs(x = paste0("Percentage of Reservations to ", siteInput),
         y = "") +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(plotly,
           tooltip = list("text")) %>%
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Number of Visits by Site Type'),
                        font = list(size = 15))) %>% 
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian"))
  
  
} # EO function