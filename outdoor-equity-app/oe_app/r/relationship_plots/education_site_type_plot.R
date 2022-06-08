#' Education x Site Type Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param education_top_quartile_df Object name for dataframe of all reservations above "high" threshold for education
#' @param site_type_string String indicating what site to create plot of
#'     Options include: "equestrian", "remote", "rv only", "rv or tent", "shelter", "tent only", "water"
#'
#' @return Plotly of educational categories compared to site type
#'
#' @examples

education_site_type_plot <- function(admin_unitInput, 
                                     siteInput,
                                     education_top_quartile_df,  
                                     site_type_string){
  
  # create reactive dataframe and further subset
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    education_top_quartile_df %>%
      # filter to user site of choice
      filter(park == siteInput) %>%
      # select to variables of interest
      select(park, customer_zip, 
             education, education_percentage, education_y_lab, 
             aggregated_site_type) %>% 
      drop_na(aggregated_site_type, education_percentage) %>% 
      # summarize to total reservations for each site type
      count(education, education_y_lab, aggregated_site_type) %>% 
      rename(count = n) %>% 
      # filter to indicated site type and update string for plotting
      filter(aggregated_site_type == site_type_string) %>% 
      mutate(aggregated_site_type = str_replace(string = aggregated_site_type,
                                                pattern = "rv", 
                                                replacement = "RV"))
  }) #EO rdf
  
  validate(
    need(nrow(rdf()) > 0,
         paste0("There are no ", site_type_string %>%
                  str_replace(string = ., pattern = "rv", replacement = "RV"), " sites at ", siteInput, ", ", admin_unitInput, "."))
  ) # EO validate
  
  # parameters
  education_group_colors <- c("HS, GED,\nor Below" = "#a6cee3", 
                              "Some College or\nTrade School"  = "#1f78b4", 
                              "Associates or\nBachelors Degree" = "#b2df8a", 
                              "Masters Degree\nor Above" = "#33a02c")
  
  # create plot
  plotly <- ggplot(data = rdf()) + 
    geom_col(aes(x = count,
                 y = education_y_lab,
                 fill = education_y_lab,
                 text = paste0("Of visits to ", aggregated_site_type, 
                               " overnight reservable sites, ", scales::comma(count, accuracy = 1), 
                               " reservations were made by ",
                               "<br>",
                               "people who live in ZIP codes with high rates of ", 
                               education, " as the highest level of education"))) +
    scale_y_discrete(expand = c(0.5, 0)) +
    scale_fill_manual(values = education_group_colors) +
    scale_color_manual(values = education_group_colors) +
    labs(x = paste("Number of Reservations"),
         y = "") + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none")
  
  # create plotly
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("zoom", "pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      "Number of Reservations to ", str_to_title(site_type_string) %>%
                                        str_replace(string = ., pattern = "Rv", replacement = "RV"), 
                                      " Sites by Visitors with Different Levels of Education"),
                        font = list(size = 13))) %>%  
    add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                    x = -0.15, y = 0.93, 
                    font = list(size = 11),
                    xref = 'paper', yref = 'paper', 
                    showarrow = FALSE)
  
} # EO function