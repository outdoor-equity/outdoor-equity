## booking window plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

#' Title
#'
#' @param admin_unitInput 
#' @param siteInput 
#'
#' @return
#' @export
#'
#' @examples
booking_window_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame
  booking_window_rdf <- reactive({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    data_joined_2018 %>%
      filter(park %in% siteInput,
             booking_window > 0,
             booking_window != "Inf") %>% 
      select(park, booking_window) %>% 
      filter(!is.na(booking_window))
  })
  
  print(class(booking_window_rdf()))
  print(paste("length df:", length(booking_window_rdf())))

  # wrangling
  x_max <- numeric(0)
  if(dim(booking_window_rdf()) != 0) {
    
  x_max <- (round(max(booking_window_rdf()$booking_window) / 5) * 5) + 5 # max x rounded to nearest 5
  }
  
  print(paste("x_max: ", x_max))
  
  quant_80 <- numeric()
  if(dim(booking_window_rdf()) != 0) {
    
  quant_80 <- quantile(x = booking_window_rdf()$booking_window,
                       probs = seq(0, 1, 0.1))[[9]] %>% round(0)
  }
   
  # parameters
  hist_colors <- c("#009900FF", "#00c000")
  
  # plot for shiny app
  booking_window_plotly <- ggplot(
    data = booking_window_rdf()) +
    geom_histogram(aes(x = booking_window,
                       text = paste0(percent(..count.. / nrow(booking_window_rdf()), accuracy = 0.1), 
                                     " of all visits are reserved between ", xmin, " and ", xmax, 
                                     " days before the start of the visit", 
                                     "<br>(All reservations to site: ",
                                     comma(nrow(booking_window_rdf()), accuracy = 1), ")")),
                   binwidth = 7,
                   center = 7 / 2,
                   fill = hist_colors[[1]], 
                   col = hist_colors[[2]], size = 0.05) +
    labs(x = "Days elapsed from order to visit (each bar = 1 week)",
         y = "",
         title = paste0("Number of days between reservation made and start of visit for<br>", 
                        siteInput, ", ", admin_unitInput)) +
    scale_x_continuous(limits = c(0, x_max), breaks = seq(0, x_max, 30)) +
    scale_y_continuous(labels = comma) +
    geom_vline(xintercept = quant_80,
               linetype = "dashed", alpha = 0.5, color = "#000099") +
    # geom_vline(xintercept = 180, 
    #            linetype = "dashed", size = .3, alpha = .5) +
    # annotate("text", label = "6 months", 
    #          x = 210, y = 65000) +
    # geom_vline(xintercept = 360, 
    #            linetype = "dashed", size = .3, alpha = .5) +
    # annotate("text", label = "1 year", 
    #          x = 380, y = 65000) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank(),
          plot.title = element_text(size = 11))
  
  ggplotly(booking_window_plotly,
           tooltip = list("text"),
           dynamicTicks = TRUE) %>% 
    layout(margin = list(b = 130, t = 100), 
           annotations =  list(x = 1, 
                               y = -0.6, 
                               text = paste0("80% of reservations reserve their visit less than ", quant_80, 
                                             " days before the start date <br>(shown on plot with dashed line)."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'left', 
                               yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 12, color = "#000099")))
  
} # EO function