
#' Language x Daily Cost Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param language_top_quartile_df Name of dataframe of values to iterate through for all 
#'     language categories and 3rd quartile values associated with each
#' @param ridb_df RIDB dataframe object name
#'
#' @return Plotly of language categories compared to daily cost
#'
#' @examples

language_daily_cost_plot <- function(admin_unitInput, siteInput,
                                     language_top_quartile_df, ridb_df){
  
  # iterate through dataframe of all educational categories and 3rd quartile values
  # return combined dataframe of reservations in "high" range for all categories
  plot_data <- 
    language_top_quartile_df %>% pmap_dfr(language_daily_cost_data, 
                                          ridb_df = ridb_df, 
                                          siteInput = siteInput)
  
  validate(
    need(nrow(plot_data) > 0,
         paste0("There are no reservations to ", siteInput, ", ", admin_unitInput,
                " that come from communities in the high range for any language categories."))
  ) # EO validate
  
  # parameters
  language_group_colors <- c("People Who Speak Only<br>English At Home" = "#66c2a5", 
                             "People Who Speak<br>Language(s) Other Than<br>English At Home" = "#8da0cb")
  
  # create plot
  plotly <- ggplot(data = plot_data, 
                   aes(x = median_daily_cost,
                       y = language_y_lab)) +
    geom_segment(aes(xend = 0, yend = language_y_lab)) +
    geom_point(aes(color = language_y_lab, fill = language_y_lab,
                   text = paste0(comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes with high rates of<br>people who ",
                                 language, 
                                 ". Typically these visitors<br>paid between ",
                                 dollar(quartile_lower), " and ", dollar(quartile_upper), 
                                 " per day, with a median of ", 
                                 dollar(median_daily_cost), 
                                 ".")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_x_continuous(labels = dollar) +
    scale_y_discrete(expand = c(0.35, 0)) +
    scale_fill_manual(values = language_group_colors) +
    scale_color_manual(values = language_group_colors) +
    labs(x = "Estimated Daily Cost per Reservation (US $)",
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
                                      'Daily Cost Paid by Visitors with Different Home Lanugages'),
                        font = list(size = 15))) %>%  
    add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                    x = -0.099, xref = 'paper', y = 1, yref = 'paper', 
                    showarrow = FALSE)
  
} # EO function