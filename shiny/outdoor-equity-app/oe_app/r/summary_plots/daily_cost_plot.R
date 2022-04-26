## daily cost per visitor plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

daily_cost_visitor_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  daily_cost_visitor_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      filter(daily_cost != "Inf") %>% 
      select(agency, admin_unit, park, daily_cost)
    
  })
  
  # wrangling
  x_max <- (round(max(daily_cost_visitor_rdf()$daily_cost) / 5) * 5) + 5 # max x rounded to nearest 5
  
  center_bin <-
    if (max(daily_cost_visitor_rdf()$daily_cost) > 100) {
      round((max(daily_cost_visitor_rdf()$daily_cost) / 100) / 5) * 5
    } else if (max(daily_cost_visitor_rdf()$daily_cost) > 10) {
      round((max(daily_cost_visitor_rdf()$daily_cost) / 10) / 5) * 5
    } else {
      0.5
    }
  
  print(paste("the class of center bin is", class(center_bin)))
  
  print(center_bin)
  
  quant_80 <- quantile(x = daily_cost_visitor_rdf()$daily_cost,
                       probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  
  print(paste("the class of quant_80 bin is", class(quant_80)))
  
  print(quant_80)
  
  
  # parameters
  hist_colors <- c("#009900FF")
  
  # plot for shiny app
  daily_cost_plotly <- ggplot(data = daily_cost_visitor_rdf()) +
    geom_histogram(aes(x = daily_cost, 
                       text = paste0(scales::percent(..count.. / nrow(dist_travel_rdf()), accuracy = 0.1), 
                                     " of all reservations traveled between ", xmin, " and ", xmax, " miles",
                                     "<br>(", scales::comma(..count.., accuracy = 1), " reservations)")),
                   binwidth = center_bin * 2,
                   center = center_bin,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    labs(x = "Daily cost per visit ($)",
         y = "",
         title = paste0("Daily Cost for Visits to <br>", siteInput, 
                       ", ", admin_unitInput, " in 2018")) +
    scale_x_continuous(limits = c(0, x_max), labels = dollar) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept = quant_80,
               linetype = "dashed", alpha = 0.5, color = "#000099") +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(daily_cost_plotly,
           tooltip = list("text")) %>% 
    layout(margin = list(b = 130, t = 100), 
           annotations =  list(x = 1, 
                               y = -0.5, 
                               text = paste0("80% of reservations to California overnight sites in 2018 paid less than ", 
                                             quant_80, " US dollars per day (shown on plot with dotted line)."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'left', 
                               yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = "#000099")))
  
  
} # EO function