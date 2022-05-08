
#' Race x Length of Stay Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param education_top_quartile_df Name of dataframe of values to iterate through for all 
#'     racial categories and 3rd quartile values associated with each
#' @param ridb_df RIDB dataframe object name
#'
#' @return Plotly of racial categories compared to length of stay
#'
#' @examples

race_length_of_stay_plot <- function(admin_unitInput, siteInput,
                                     race_top_quartile_df, ridb_df){
  
  # iterate through dataframe of all racial categories and 3rd quartile values
  # return combined dataframe of reservations in "high" range for all categories
  plot_data <- 
    race_top_quartile_df %>% pmap_dfr(race_length_of_stay_data, 
                                      ridb_df = ridb_df, 
                                      siteInput = siteInput)
  
  # validate(
  #   need(nrow(plot_data) == 0,
  #        paste0("There are no reservations to ", siteInput, ", ", admin_unitInput, 
  #               " that come from communities in the high range for any racial categories."))
  # ) # EO validate
  
  # parameters
  race_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  # create plot
  plotly <- ggplot(data = plot_data, 
                   aes(x = median_length_of_stay,
                       y = reorder(race, median_length_of_stay))) +
    geom_segment(aes(xend = 0, yend = race)) +
    geom_point(aes(color = race, fill = race,
                   text = paste0(comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes<br>with high ",
                                 race, " populations. Typically these visitors stayed between<br>",
                                 dollar(quartile_lower), 
                                 " and ", dollar(quartile_upper), 
                                 " days, with a median of ", 
                                 dollar(median_length_of_stay), 
                                 " days.")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_x_continuous(labels = comma) +
    scale_y_discrete(expand = c(0.3, 0)) +
    scale_fill_manual(values = race_group_colors) +
    scale_color_manual(values = race_group_colors) +
    labs(x = paste("Estimated Length of Stay (days)"),
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
                                      'Length of Stay by Different Racial Groups'),
                        font = list(size = 15))) %>% 
    add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                    x = -0.05, xref = 'paper', y = 0.9, yref = 'paper', 
                    showarrow = FALSE)
  
} # EO function