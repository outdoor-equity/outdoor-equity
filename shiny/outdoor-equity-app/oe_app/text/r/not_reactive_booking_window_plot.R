
#' Data Summary Booking Window Non-reactive
#'
#' @param admin_unit String indicating admin unit of site
#' @param site String indicating which site to filter data to for plotting
#'
#' @return Plotly plot of booking window for indicated site
#'
#' @examples
booking_window_plot <- function(admin_unit, site){
  
  # data frame
  data <- data_joined_2018 %>%
    filter(park == site,
           booking_window > 0,
           booking_window != "Inf") %>% 
    select(park, booking_window) %>% 
    filter(!is.na(booking_window))
  
  
  # wrangling
  x_max <- numeric(0)
  if(dim(data) != 0) {
    
    x_max <- (round(max(data$booking_window) / 5) * 5) + 5 # max x rounded to nearest 5
  }
  
  quant_80 <- numeric()
  if(dim(data) != 0) {
    
    quant_80 <- quantile(x = data$booking_window,
                         probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  }
  
  # parameters
  hist_colors <- c("#009900FF", "#00c000")
  
  # plot for shiny app
  ggplot(data = data) +
    geom_histogram(aes(x = booking_window),
                   binwidth = 7,
                   center = 7 / 2,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    labs(x = "Days in advance before visit (each bar = 1 week)",
         y = NULL,
         title = paste0(site, ", ", admin_unit),
         subtitle = "Number of days from reservation to start of visit",
         caption = paste0("80% of reservations reserve their visit less than ", 
                          quant_80, 
                          " days before the start date", 
                          "\n(shown on plot with dashed line).")) +
    scale_x_continuous(limits = c(0, x_max), labels = comma) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept = quant_80,
               linetype = "dashed", alpha = 0.5, color = "#000099") +
    annotate("label",
             x = 68, y = 7000,
             label = paste0("29% of all visits are reserved between", 
                            "\n", 
                            "1 and 7 days before the start of the trip.")) +
    annotate("label", 
             x = 225, y = 7000,
             label = paste0(comma(nrow(data), accuracy = 1), " total reservations were", 
                            "\n", "made to this site.")) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(color = "#000099", size = 11))
  
    
    # paste0(percent(..count.. / nrow(data), accuracy = 0.1), 
    #        " of all visits are reserved between ", xmin, " and ", xmax, 
    #        " days before the start of the visit", 
    #        "<br>(All reservations to site: ",
    #        comma(nrow(data), accuracy = 1), ")")
    
  # ggplotly(plotly,
  #          tooltip = list("text"),
  #          dynamicTicks = TRUE) %>% 
  #   layout(title = list(text = paste0('<b>', site, '<br>', admin_unit, '</b>',
  #                                     '<br>',
  #                                     'Number of days from reservation to start of visit'),
  #                       font = list(size = 15)),
  #          xaxis = list(separatethousands = TRUE),
  #          yaxis = list(separatethousands = TRUE),
  #          margin = list(b = 130, t = 100), 
  #          annotations =  list(x = x_max/2, y = -0.4, 
  #                              text = paste0("80% of reservations reserve their visit less than ", '<b>', quant_80, '</b>', 
  #                                            " days before the start date <br>(shown on plot with dashed line)."), 
  #                              showarrow = F, 
  #                              xre = 'paper', yref = 'paper', 
  #                              xanchor = 'middle', yanchor = 'auto', 
  #                              xshift = 0, yshift = 0,
  #                              font = list(size = 12, color = "#000099"))) %>%
  #   config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
  #                                        "hoverClosestCartesian", "hoverCompareCartesian"))
  
  
} # EO function