
#' Language x Length of Stay Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param language_top_quartile_df Object name for dataframe of all reservations above "high" threshold for language
#'
#' @return Plotly of language categories compared to length of stay
#'
#' @examples

language_length_of_stay_plot <- function(admin_unitInput, 
                                         siteInput,
                                         language_top_quartile_df){
  
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
             length_of_stay) %>% 
      drop_na(length_of_stay, language_percentage) %>% 
      # summarize to inner quartile range, median, and total reservations
      group_by(language, language_y_lab) %>% 
      summarize(median_length_of_stay = median(length_of_stay),
                quartile_lower = quantile(length_of_stay)[[2]],
                quartile_upper = quantile(length_of_stay)[[4]],
                count = n())
  }) # EO rdf
  
  validate(
    need(nrow(rdf()) > 0,
         paste0("There are no reservations to ", siteInput, ", ", admin_unitInput,
                " that come from communities in the high range for any language categories."))
  ) # EO validate
  
  # parameters
  language_group_colors <- c("Only English<br>At Home" = "#66c2a5", 
                             "Language(s)<br>Other Than<br>English At Home" = "#8da0cb")
  
  # create plot (or say no such site type if none exist at siteInput)
  plotly <- ggplot(data = rdf(), 
                   aes(x = median_length_of_stay,
                       y = language_y_lab)) +
    geom_segment(aes(xend = 0, yend = language_y_lab)) +
    geom_point(aes(color = language_y_lab, fill = language_y_lab,
                   text = paste0(comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes with high rates of<br>people who ",
                                 language, 
                                 ". Typically these visitors<br>stay between ",
                                 comma(quartile_lower, accuracy = 1), 
                                 " and ", comma(quartile_upper, accuracy = 1), 
                                 " days, with a median stay of ", 
                                 comma(median_length_of_stay, accuracy = 1), 
                                 " days")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_y_discrete(expand = c(0.55, 0)) +
    scale_fill_manual(values = language_group_colors) +
    scale_color_manual(values = language_group_colors) +
    labs(x = paste("Estimated Length of Stay (days)"),
         y = "") + 
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          plot.margin = margin(5, 5, 0, 0, unit = "pt"))
  
  # create plotly
  ggplotly(plotly,
           tooltip = list("text")) %>%
    config(modeBarButtonsToRemove = list("zoom", "pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian")) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Length of Stay by Visitors with Different Home Lanugages'),
                        font = list(size = 15))) %>%  
    add_annotations(text = "Reservations from ZIP codes with high<br>proportionof people who speak:", 
                    x = -0.2, y = 0.9, 
                    font = list(size = 11),
                    xref = 'paper', yref = 'paper', 
                    showarrow = FALSE)
  
} # EO function