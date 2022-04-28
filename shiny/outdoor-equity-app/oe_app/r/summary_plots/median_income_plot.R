## median income plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

median_income_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  median_income_rdf <- reactive ({
    
    # reservations in CA
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      select(park, customer_zip, median_income)
    
  }) # EO RDF
  
  # non RDF
  # CA population
  data_plot_median_income_ca <- data_ca_acs_2018 %>%
    select(zip_code, median_income)
  
  x_max <- max(median_income_rdf()$education_percent_average) + 0.1 # max x rounded to nearest 5
  
  # parameters
  groups_colors_ridb_ca <- c("RIDB" = "#009900FF", "CA" = "#666666")
  
  # plot for shiny app
  ggplot() +
    geom_histogram(data = data_plot_median_income_ca,
                   aes(x = median_income,
                       y = stat(count) / sum(count),
                       bins = 10),
                   fill = groups_colors_ridb_ca[[2]], 
                   alpha = 0.75) +
    # RDF here
    income_plotly <- geom_histogram(data = median_income_rdf(),
                   aes(x = median_income,
                       y = stat(count) / sum(count),
                       bins = 10), 
                   fill = groups_colors_ridb_ca[[1]], 
                   alpha = 0.75) +
    scale_x_continuous(limits = c(0, x_max), labels = comma) +
    scale_y_continuous(labels = percent) +
    geom_vline(xintercept = 75277, 
               linetype = "dashed", size = .3, alpha = .5) +
    annotate("text", label = "Median household income \nin 2018 in CA ($75,277)", 
             x = 118000, y = .15) +
    labs(x = "Median Income ($))",
         y = "",
         title = paste0("Median Income in Home in ZIP Codes in 2018 for ", siteInput, ", ", admin_unitInput)) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(income_plotly)
  
} # EO function