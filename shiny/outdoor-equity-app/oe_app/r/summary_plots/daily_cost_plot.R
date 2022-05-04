## daily cost per visitor plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

#' Title
#'
#' @param admin_unitInput 
#' @param siteInput 
#'
#' @return
#'
#' @examples
daily_cost_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  daily_cost_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    data_joined_2018 %>%
      filter(park %in% siteInput,
             daily_cost != "Inf") %>% 
      select(park, daily_cost)
    
  })
  
  # wrangling
  x_max <- (round(max(daily_cost_rdf()$daily_cost) / 5) * 5) + 5 # max x rounded to nearest 5
  
  center_bin <-
    if (x_max > 100) {
      (round((max(daily_cost_rdf()$daily_cost) / 100) / 5) * 5) + 5
    } else if (x_max > 10) {
      (round((max(daily_cost_rdf()$daily_cost) / 10) / 5) * 5) + 5
    } else {
      0.5
    }

  
  quant_80 <- quantile(x = daily_cost_rdf()$daily_cost,
                       probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  
  # parameters
  hist_colors <- c("#009900FF", "#00c000")
  
  # plot for shiny app
  daily_cost_plotly <- ggplot(data = daily_cost_rdf()) +
    geom_histogram(aes(x = daily_cost, 
                       text = paste(percent(..count.. / nrow(daily_cost_rdf()), accuracy = 0.1), 
                                    " of all reservations paid between ", dollar(xmin), " and ", dollar(xmax),
                                    "<br>(All reservations to site: ",
                                    comma(nrow(daily_cost_rdf()), accuracy = 1), ")")),
                   binwidth = center_bin * 2,
                   center = center_bin,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    labs(x = "Daily cost per visit ($)",
         y = "") +
    scale_x_continuous(limits = c(0, x_max), labels = dollar) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept = quant_80,
               linetype = "dashed", alpha = 0.5, color = "#000099") +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(daily_cost_plotly,
           tooltip = list("text"),
           dynamicTicks = TRUE) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Daily Cost of Visits'),
                        font = list(size = 15)),
           margin = list(b = 130, t = 100), 
           annotations =  list(x = 5, 
                               y = -0.6, 
                               text = paste0("80% of reservations paid less than ", '<b>', dollar(quant_80), '</b>', "<br> (shown on plot with dotted line)."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'left', 
                               yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = "#000099"))) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian"))
  
  
} # EO function