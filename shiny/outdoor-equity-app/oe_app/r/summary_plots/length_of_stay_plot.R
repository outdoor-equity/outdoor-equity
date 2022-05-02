## length of stay plot and parameters ##
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
length_of_stay_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  length_of_stay_rdf <- reactive ({
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      select(park, length_of_stay)
    
  })
  
  # wrangling
  x_max <- (round(max(length_of_stay_rdf()$length_of_stay) / 5) * 5) + 5 # max value x rounded up to nearest 5
  
  print(paste("the class of center bin is", class(center_bin)))
  
  print(center_bin)
  
  quant_80 <- quantile(x = length_of_stay_rdf()$length_of_stay,
                       probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  
  print(paste("the class of quant_80 bin is", class(quant_80)))
  
  print(quant_80)
  
  # parameters
  hist_colors <- c("#009900FF", "#00c000")
  
  # plot for shiny app
  length_of_stay_plotly <- ggplot(data = length_of_stay_rdf()) +
    geom_histogram(aes(x = length_of_stay,
                       text = paste0(percent(..count.. / nrow(length_of_stay_rdf()), accuracy = 0.1), 
                                     " of all reservations stay between ", comma(xmin, accuracy = 1), " and ", 
                                     comma(xmax, accuracy = 1), " days",  "<br>(All reservations to site: ",
                                     comma(nrow(length_of_stay_rdf()), accuracy = 1), ")")),
                   binwidth = 1,
                   center = 0.5,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    scale_x_continuous(limits = c(0, x_max)) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept = quant_80,
               linetype = "dashed", alpha = 0.5, color = "#000099") +
    labs(x = "Length of visit (days)",
         y = "",
         title = paste0("Length of Visit for Reservations at <br>", 
                        siteInput, ", ", admin_unitInput)) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 11))
  
  ggplotly(length_of_stay_plotly,
           tooltip = list("text"),
           dynamicTicks = TRUE) %>% 
    layout(margin = list(b = 130, t = 100), 
           annotations =  list(x = 1, 
                               y = -0.6, 
                               text = paste0("80% of reservations stay less than ", quant_80, " days <br>(shown on plot with dashed line)."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'left', 
                               yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = "#000099"))) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian"))
} # EO function