
#' Median-income x Daily Cost per Visitor Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param median_income_decile_df Object name for dataframe of all reservations split into median-income deciles
#'
#' @return Plotly of median-income categories compared to daily cost per visitor
#'
#' @examples

median_income_daily_cost_per_visitor_plot <- function(admin_unitInput, 
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
      select(park, customer_zip, median_income_binned, daily_cost_per_visitor) %>% 
      drop_na(median_income_binned) %>% 
      # summarize to inner quartile range, median, and total reservations
      group_by(median_income_binned) %>% 
      summarize(median_daily_cost_per_visitor = median(daily_cost_per_visitor),
                quartile_lower = quantile(daily_cost_per_visitor)[[2]],
                quartile_upper = quantile(daily_cost_per_visitor)[[4]],
                count = n())
  }) # EO rdf
  
  # create plot
  plotly <- ggplot(data = rdf(), 
                   aes(x = median_daily_cost_per_visitor,
                       y = median_income_binned)) +
    geom_segment(aes(xend = 0, yend = median_income_binned)) +
    geom_point(aes(color = median_income_binned, fill = median_income_binned,
                   text = paste0(comma(count, accuracy = 1), 
                                 " unique visits were made by people who live in ZIP codes with a<br>median household income between ",
                                 median_income_binned, 
                                 ". Typically these visitors<br>pay between ",
                                 dollar(quartile_lower), 
                                 " and ", dollar(quartile_upper), 
                                 " per visitor per day, with a median of ", 
                                 dollar(median_daily_cost_per_visitor), 
                                 ".")),
               size = 3.5, 
               shape = 21, stroke = 2) +
    scale_x_continuous(labels = dollar) +
    scale_y_discrete(expand = c(0.2, 0)) +
    scale_fill_viridis_d(direction = -1) +
    scale_color_viridis_d(direction = -1) +
    labs(x = "Estimated Daily Cost per Visitor (US $)",
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
                                      'Daily Cost per Person Paid by Visitors with Different Median Household Incomes'),
                        font = list(size = 15)))

} # EO function