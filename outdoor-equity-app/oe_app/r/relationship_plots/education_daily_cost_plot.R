#' Education x Daily Cost Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param education_top_quartile_df Object name for dataframe of all reservations above "high" threshold for education
#'
#' @return Plotly of educational categories compared to daily cost
#'
#' @examples

education_daily_cost_plot <- 
  function(admin_unitInput, 
           siteInput,
           education_top_quartile_df){
    
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
               daily_cost) %>% 
        drop_na(daily_cost, education_percentage) %>% 
        # summarize to inner quartile range, median, and total reservations
        group_by(education, education_y_lab) %>% 
        summarize(median_daily_cost = median(daily_cost),
                  quartile_lower = quantile(daily_cost)[[2]],
                  quartile_upper = quantile(daily_cost)[[4]],
                  count = n())
    }) #EO rdf
    
    validate(
      need(nrow(rdf()) > 0,
           paste0("There are no reservations to ", 
                  siteInput, 
                  ", 
                  ", admin_unitInput,
                  " that come from communities in the high range for any educational categories."))
    ) # EO validate
    
    # parameters
    education_group_colors <- c("HS, GED,\nor Below" = "#a6cee3", 
                                "Some College or\nTrade School"  = "#1f78b4", 
                                "Associates or\nBachelors Degree" = "#b2df8a", 
                                "Masters Degree\nor Above" = "#33a02c")
    
    # create plot
    plotly <- ggplot(data = rdf(), 
                     aes(x = median_daily_cost,
                         y = education_y_lab)) +
      geom_segment(aes(xend = 0, yend = education_y_lab)) +
      geom_point(aes(color = education_y_lab, fill = education_y_lab,
                     text = paste0(comma(count, accuracy = 1), 
                                   " unique visits were made by people who live in ZIP codes with high rates of",
                                   "<br>",
                                   education, " as the highest level of education. Typically these visitors", 
                                   "<br>", 
                                   "pay between ", dollar(quartile_lower), " and ", dollar(quartile_upper), 
                                   " per day, with a median of ", dollar(median_daily_cost), ".")),
                 size = 3.5, 
                 shape = 21, stroke = 2) +
      scale_x_continuous(labels = dollar) +
      scale_y_discrete(expand = c(0.45, 0)) +
      scale_fill_manual(values = education_group_colors) +
      scale_color_manual(values = education_group_colors) +
      labs(x = "Estimated Daily Cost per Reservation (US $)",
           y = "") + 
      theme_minimal() +
      theme(plot.background = element_rect("white"),
            panel.grid.major.y = element_blank(),
            legend.position = "none")
    
    # create plotly
    ggplotly(plotly,
             tooltip = list("text")) %>%
      config(modeBarButtonsToRemove = list("zoom", 
                                           "pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian")) %>% 
      layout(title = list(text = paste0('<b>', siteInput, 
                                        '<br>', 
                                        admin_unitInput, '</b>',
                                        '<br>',
                                        'Daily Cost Paid by Visitors with Different Levels of Education'),
                          font = list(size = 15))) %>%  
      add_annotations(text = "Reservations from ZIP codes<br>with high proportions of:", 
                      x = -0.15, y = 0.9, 
                      font = list(size = 11),
                      xref = 'paper', yref = 'paper', 
                      showarrow = FALSE)
    
  } # EO function