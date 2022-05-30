
#' Daily Cost Data Summary Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#'
#' @return Plotly of daily cost
#'
#' @examples
daily_cost_plot <- function(admin_unitInput, siteInput, ridb_df){
  
  # reactive data frame 
  daily_cost_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    ridb_df %>%
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
  hist_colors <- c("#64863C", "#466C04")
  quant_80_color <- c("#FACE00")
  caption_color <- c("#ac8d00")
  
  # plot for shiny app
  daily_cost_plotly <- ggplot(data = daily_cost_rdf()) +
    geom_histogram(aes(x = daily_cost, 
                       text = paste(percent(..count.. / nrow(daily_cost_rdf()), accuracy = 0.1), 
                                    " of all reservations paid between ", dollar(xmin), " and ", dollar(xmax),
                                    "<br>",
                                    "(Total reservations to site: ",
                                    comma(nrow(daily_cost_rdf()), accuracy = 1), ")")),
                   binwidth = center_bin * 2,
                   center = center_bin,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    labs(x = "Daily cost per visit ($)",
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
                                      'Daily Cost of Visits'),
                        font = list(size = 15)),
           xaxis = list(tickformat = "$"),
           yaxis = list(separatethousands = TRUE),
           margin = list(b = 130, t = 100), 
           annotations =  list(x = x_max/2, y = -0.6, 
                               text = paste0("80% of reservations paid less than ", '<b>', dollar(quant_80), '</b>', 
                                             " per day",
                                             "<br>", 
                                             "(shown on plot with yellow dotted line)."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'middle', yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = caption_color))) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian"))
  
  
} # EO function