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
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      filter(daily_cost_per_visitor != "Inf") %>% 
      select(park, daily_cost_per_visitor)
    
  })
  
  # wrangling
  x_max <- (round(max(daily_cost_visitor_rdf()$daily_cost_per_visitor) / 5) * 5) + 5 # max x rounded to nearest 5
  
  quant_80 <- quantile(x = daily_cost_visitor_rdf()$daily_cost_per_visitor,
                       probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  
  print(paste("the class of quant_80 bin is", class(quant_80)))
  
  print(quant_80)
  
  
  # parameters
  hist_colors <- c("#009900FF", "#00c000")
  
  # plot for shiny app
  daily_cost_plotly <- ggplot(data = daily_cost_visitor_rdf()) +
    geom_histogram(aes(x = daily_cost_per_visitor, 
                       text = paste(percent(..count.. / nrow(daily_cost_visitor_rdf()), accuracy = 0.1), 
                                    "of all reservations paid between", dollar(xmin), "and", dollar(xmax),
                                    "<br>(All reservations to site: ",
                                    comma(nrow(daily_cost_visitor_rdf()), accuracy = 1), ")")),
                   binwidth = 1,
                   center = 0.5,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    labs(x = "Daily cost per visitor ($)",
         y = "",
         title = paste0("Daily Cost per Visitor for Visits to <br>", siteInput, 
                        ", ", admin_unitInput, " in 2018")) +
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
    layout(margin = list(b = 130, t = 100), 
           annotations =  list(x = 1, 
                               y = -0.4, 
                               text = paste0("80% of reservations to ", siteInput, ", ", admin_unitInput, 
                                             "<br>paid less than ", dollar(quant_80), 
                                             " per visitor per day (shown on plot with dotted line)."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'left', 
                               yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = "#000099")))
  
  
} # EO function