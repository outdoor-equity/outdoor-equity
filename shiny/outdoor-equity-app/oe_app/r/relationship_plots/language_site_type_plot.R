
#' Language x Site Type Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param language_top_quartile_df Name of dataframe of values to iterate through for all 
#'     language categories and 3rd quartile values associated with each
#' @param ridb_df RIDB dataframe object name
#' @param site_type_string String indicating what site to create plot of
#'     Options include: "equestrian", "remote", "rv only", "rv or tent", "shelter", "tent only", "water"
#'
#' @return Plotly of language categories compared to site type
#'
#' @examples

language_site_type_plot <- function(admin_unitInput, siteInput,
                                    language_top_quartile_df, ridb_df,
                                    site_type_string){
  
  # iterate through dataframe of all language categories and 3rd quartile values
  # return combined dataframe of reservations in "high" range for all categories
  plot_data <- 
    language_top_quartile_df %>% pmap_dfr(language_site_type_data, 
                                          ridb_df = ridb_df, 
                                          siteInput = siteInput) %>% 
    # filter to indicated site type and update string for plotting
    filter(aggregated_site_type == site_type_string) %>% 
    mutate(aggregated_site_type = str_replace(string = aggregated_site_type,
                                              pattern = "rv", 
                                              replacement = "RV"))
  
  print(head(plot_data))
  
  # validate(
  #   need(nrow(plot_data) == 0,
  #        paste0("There are either no ", site_type_string %>% 
  #                 str_replace(string = ., pattern = "Rv", replacement = "RV"), " 
  #                sites at ", siteInput, ", ", admin_unitInput, 
  #               "<bror there are no reservations at the site that come from communities that fall into the high range for any language groups."))
  # ) # EO validate
  
  # parameters
  language_group_colors <- c("Speak Only<br>English At Home" = "#66c2a5", "Speak Language(s) Other<br>Than English At Home" = "#8da0cb")
  
  # create plot
  plotly <- ggplot(data = plot_data) +
    geom_col(aes(x = count,
                 y = language_y_lab,
                 fill = language_y_lab,
                 text = paste0("Of visits to ", 
                               aggregated_site_type, 
                               " overnight reservable sites, ", comma(count, accuracy = 1), 
                               " reservations were made by <br>people who live in ZIP codes with high rates of people who ",
                               language, "."))) +
    scale_y_discrete(expand = c(0.9, 0)) +
    scale_fill_manual(values = language_group_colors) +
    scale_color_manual(values = language_group_colors) +
    labs(x = paste("Number of Reservations"),
         y = "") + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none")
  
  # create plotly
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      "Number of Reservations to ", str_to_title(site_type_string) %>%
                                        str_replace(string = ., pattern = "Rv", replacement = "RV"), 
                                      " Sites by Visitors with Different Home Lanugages"),
                        font = list(size = 15))) %>%  
    add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                    x = -0.4, xref = 'paper', y = 0.93, yref = 'paper', 
                    showarrow = FALSE)
  
} # EO function