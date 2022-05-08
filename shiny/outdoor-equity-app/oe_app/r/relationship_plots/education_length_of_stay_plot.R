
#' Education x Length of Stay Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param education_top_quartile_df Name of dataframe of values to iterate through for all 
#'     educational categories and 3rd quartile values associated with each
#' @param ridb_df RIDB dataframe object name
#'
#' @return Plotly of educational categories compared to length of stay
#'
#' @examples

education_length_of_stay_plot <- function(admin_unitInput, siteInput,
                                          education_top_quartile_df, ridb_df){
  
  # iterate through dataframe of all educational categories and 3rd quartile values
  # return combined dataframe of reservations in "high" range for all categories
  plot_data <- 
    education_top_quartile_df %>% pmap_dfr(education_length_of_stay_data, 
                                           ridb_df = ridb_df, 
                                           siteInput = siteInput)
  
  # validate(
  #   need(nrow(plot_data) == 0,
  #        paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
  #               " that come from communities in the high range for any educational category"))
  # ) # EO validate
  
  # parameters
  education_group_colors <- c("HS, GED,\nor Below" = "#a6cee3", "Some College or\nTrade School"  = "#1f78b4", 
                              "Associates or\nBachelors Degree" = "#b2df8a", "Masters Degree\nor Above" = "#33a02c")
  
  # create plot
  plotly <- ggplot(data = plot_data, 
                   aes(x = median_length_of_stay,
                       y = education_y_lab)) +
    geom_segment(aes(xend = 0, yend = education_y_lab)) +
    geom_point(aes(color = education_y_lab, fill = education_y_lab,
                   text = paste0(comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes with<br>high rates of ",
                                 education, 
                                 " as the highest level of education.<br>Typically these visitor stay between ",
                                 comma(quartile_lower, accuracy = 1), 
                                 " and ", comma(quartile_upper, accuracy = 1), 
                                 " days, with a median stay of ", 
                                 comma(median_length_of_stay, accuracy = 1), 
                                 " days")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_y_discrete(expand = c(0.35, 0)) +
    scale_fill_manual(values = education_group_colors) +
    scale_color_manual(values = education_group_colors) +
    labs(x = "Estimated Length of Stay (days)",
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
    layout(title = list(text = paste0("<b>", siteInput, "<br>", admin_unitInput, "</b>",
                                      "<br>",
                                      "Length of Stay by Visitors with Different Levels of Education"),
                        font = list(size = 15))) %>%  
    add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                    x = -0.15, xref = 'paper', y = 0.93, yref = 'paper', 
                    showarrow = FALSE)
  
} # EO function