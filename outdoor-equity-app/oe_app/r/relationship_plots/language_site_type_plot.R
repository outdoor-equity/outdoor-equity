
#' Language x Site Type Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param language_top_quartile_df Object name for dataframe of all reservations above "high" threshold for language
#' @param site_type_string String indicating what site to create plot of
#'     Options include: "equestrian", "remote", "rv only", "rv or tent", "shelter", "tent only", "water"
#'
#' @return Plotly of language categories compared to site type
#'
#' @examples

language_site_type_plot <- function(admin_unitInput, 
                                    siteInput,
                                    language_top_quartile_df,
                                    site_type_string){
  
  # create reactive dataframe and further subset
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    language_top_quartile_df %>%
      # filter to user site of choice
      filter(park == siteInput) %>%
      # select to variables of interest
      select(park, customer_zip, 
             language, language_percentage, language_y_lab, 
             aggregated_site_type) %>% 
      drop_na(aggregated_site_type, language_percentage) %>% 
      # summarize to total reservations for each site type
      count(language, language_y_lab, aggregated_site_type) %>% 
      rename(count = n) %>% 
      # filter to indicated site type and update string for plotting
      filter(aggregated_site_type == site_type_string) %>% 
      mutate(aggregated_site_type = str_replace(string = aggregated_site_type,
                                                pattern = "rv", 
                                                replacement = "RV"))
  }) # EO rdf
  
  
  validate(
    need(nrow(rdf()) > 0,
         paste0("There are no ", site_type_string %>%
                  str_replace(string = ., pattern = "rv", replacement = "RV"), " sites at ", siteInput, ", ", admin_unitInput, "."))
  ) # EO validate
  
  
  # parameters
  language_group_colors <- c("Only English<br>At Home" = "#66c2a5", 
                             "Language(s)<br>Other Than<br>English At Home" = "#8da0cb")
  
  # create plot
  plotly <- ggplot(data = rdf()) +
    geom_col(aes(x = count,
                 y = language_y_lab,
                 fill = language_y_lab,
                 text = paste0("Of visits to ", 
                               aggregated_site_type, 
                               " overnight reservable sites, ", comma(count, accuracy = 1), 
                               " reservations were made by <br>people who live in ZIP codes with high rates of people who ",
                               language, "."))) +
    geom_text(aes(x = max(count) / 4,
                  y = language_y_lab,
                  label = language_y_lab), 
              size = 3,
              nudge_x = 0) +
    scale_x_continuous(labels = comma_format(accuracy = 1)) +
    scale_y_discrete(expand = c(0.99, 0)) +
    scale_fill_manual(values = language_group_colors) +
    scale_color_manual(values = language_group_colors) +
    labs(x = "Number of Reservations",
         y = NULL) + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          axis.title.x = element_text(size = 8),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = margin(5, 5, 0, 0, unit = "pt"))
  
  # create plotly
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("zoom", "pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      "Number of Reservations to ", str_to_title(site_type_string) %>%
                                        str_replace(string = ., pattern = "Rv", replacement = "RV"), 
                                      " Sites by Visitors with Different Home Lanugages"),
                        font = list(size = 13))) %>%  
    add_annotations(text = "Reservations from ZIP codes with high<br>proportionof people who speak:", 
                    x = 0, y = 0.93, 
                    font = list(size = 11),
                    xref = 'paper', yref = 'paper', 
                    showarrow = FALSE)
  
} # EO function