
#' Median-income x Length of Stay Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param median_income_decile_df Object name for dataframe of all reservations split into median-income deciles
#'
#' @return Plotly of median-income categories compared to length of stay
#'
#' @examples

median_income_length_of_stay_plot <- function(admin_unitInput, 
                                              siteInput, 
                                              median_income_decile_df){
  
  # create reactive dataframe and further subset
  rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    median_income_decile_df %>%
      # filter to user site of choice
      filter(park %in% siteInput) %>%
      # select to variables of interest
      select(park, customer_zip, median_income_binned, length_of_stay) %>% 
      drop_na(median_income_binned) %>% 
      # summarize to inner quartile range, median, and total reservations
      group_by(median_income_binned) %>% 
      summarize(median_length_of_stay = median(length_of_stay),
                quartile_lower = quantile(length_of_stay)[[2]],
                quartile_upper = quantile(length_of_stay)[[4]],
                count = n())
  }) # EO rdf
  
  # create plot
  plotly <- ggplot(data = rdf(), 
                   aes(x = median_length_of_stay,
                       y = median_income_binned)) +
    geom_segment(aes(xend = 0, yend = median_income_binned)) +
    geom_point(aes(color = median_income_binned, fill = median_income_binned,
                   text = paste0(scales::comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes with a<br>median household income between ",
                                 median_income_binned, 
                                 ". Typically these visitors<br>stay between ",
                                 scales::comma(quartile_lower, accuracy = 1), 
                                 " and ", scales::comma(quartile_upper, accuracy = 1), 
                                 " days, with a median stay of ", 
                                 scales::comma(median_length_of_stay, accuracy = 1), 
                                 " days")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
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
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Length of Stay by Visitors with Different Median Household Incomes'),
                        font = list(size = 15)))

} # EO function