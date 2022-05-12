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
daily_cost_visitor_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  daily_cost_visitor_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      filter(daily_cost_per_visitor != "Inf") %>% 
      select(park, daily_cost_per_visitor)
    
  })
  
  # wrangling
  x_max <- (round(max(daily_cost_visitor_rdf()$daily_cost_per_visitor) / 5) * 5) + 5 # max x rounded to nearest 5
  
  quant_80 <- quantile(x = daily_cost_visitor_rdf()$daily_cost_per_visitor,
                       probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  
  
  # parameters
  hist_colors <- c("#64863C", "#466C04")
  quant_80_color <- c("#97D4EA")
  caption_color <- c("#345D96")
  
  # plot for shiny app
  daily_cost_plotly <- ggplot(data = daily_cost_visitor_rdf()) +
    geom_histogram(aes(x = daily_cost_per_visitor, 
                       text = paste(percent(..count.. / nrow(daily_cost_visitor_rdf()), accuracy = 0.1), 
                                    "of all reservations paid between", dollar(xmin), "and", dollar(xmax),
                                    "<br>",
                                    "(Total reservations to site: ",
                                    comma(nrow(daily_cost_visitor_rdf()), accuracy = 1), ")")),
                   binwidth = 1,
                   center = 0.5,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    labs(x = "Daily cost per visitor ($)",
         y = "") +
    scale_x_continuous(limits = c(0, x_max)) +
    geom_vline(xintercept = quant_80,
               linetype = "dashed", alpha = 0.5, color = quant_80_color) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(daily_cost_plotly,
           tooltip = list("text"),
           dynamicTicks = TRUE) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Daily Cost per Visitor'),
                        font = list(size = 15)),
           xaxis = list(tickformat = "$"),
           yaxis = list(separatethousands = TRUE),
           margin = list(b = 130, t = 100), 
           annotations =  list(x = x_max/2, y = -0.6, 
                               text = paste0("80% of reservations paid less than ", '<b>', dollar(quant_80), '</b>',
                                             " per visitor per day",
                                             "<br>", 
                                             "(shown on plot with blue dotted line)."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'middle', yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = caption_color))) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian"))
  
  
} # EO function