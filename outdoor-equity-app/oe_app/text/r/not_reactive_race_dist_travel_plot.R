
#' About Example: Race x Distance Traveled Plotly
#'
#' @param admin_unit User pick for admin unit
#' @param site User pick for site
#' @param race_top_quartile_df Object name for dataframe of all reservations above "high" threshold for race
#'
#' @return Plotly of racial categories compared to distance traveled
#'
#' @examples

not_reactive_race_dist_travel_plot <- function(admin_unit, 
                                               site,
                                               race_top_quartile_df){
  
  # create dataframe and further subset
  plot_data <- race_top_quartile_df %>%
    # filter to user site of choice
    filter(park == site) %>%
    # select to variables of interest
    select(park, customer_zip, 
           race, race_percentage,
           distance_traveled_mi) %>% 
    drop_na(distance_traveled_mi, race_percentage) %>% 
    # summarize to inner quartile range, median, and total reservations
    group_by(race) %>%
    summarize(median_distance_traveled_mi = median(distance_traveled_mi),
              quartile_lower = quantile(distance_traveled_mi)[[2]],
              quartile_upper = quantile(distance_traveled_mi)[[4]],
              count = n())
  
  # parameters
  race_group_colors <- c("Other Race(s)" = "#999999", "Pacific Islander" = "#E69F00", "Multiracial" = "#56B4E9",
                         "Asian" = "#009E73", "Black" = "#F0E442", "White" = "#0072B2", 
                         "Native American" = "#D55E00", "Hispanic Latinx" = "#CC79A7")
  
  # create plot
  plotly <- ggplot(data = plot_data, 
                   aes(x = median_distance_traveled_mi,
                       y = reorder(race, median_distance_traveled_mi))) +
    geom_segment(aes(xend = 0, yend = race)) +
    geom_point(aes(color = race, fill = race,
                   text = paste0(scales::comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes<br>with high ",
                                 race, " populations. Typically these visitors traveled between<br>",
                                 scales::comma(quartile_lower, accuracy = 1), 
                                 " and ", 
                                 scales::comma(quartile_upper, accuracy = 1), 
                                 " miles, with a median distance of ", 
                                 scales::comma(median_distance_traveled_mi, accuracy = 1), 
                                 " miles.")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_y_discrete(expand = c(0.45, 0)) +
    scale_fill_manual(values = race_group_colors) +
    scale_color_manual(values = race_group_colors) +
    labs(x = paste("Estimated Distance Traveled from Home to Site (miles)"),
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
    layout(title = list(text = paste0('<b>', site, '<br>', admin_unit, '</b>',
                                      '<br>',
                                      'Distance Traveled by Different Racial Groups'),
                        font = list(size = 15))) %>%
    add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                    x = -0.5, y = 0.9, 
                    font = list(size = 11),
                    xref = 'paper', yref = 'paper', 
                    showarrow = FALSE)
  
} # EO function