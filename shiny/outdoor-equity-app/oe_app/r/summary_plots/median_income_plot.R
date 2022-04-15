## median income plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

median_income_plot <- function(agencyInput, admin_unitInput, siteInput, titleInput){
  
  # reactive data frame 
  median_income_rdf <- reactive ({
    
    # reservations in CA
    data_joined_2018 %>%
      filter(agency %in% agencyInput,
             admin_unit %in% admin_unitInput,
             park %in% siteInput) %>%
      select(agency, admin_unit, park, customer_zip, median_income)
    
  }) # EO RDF
  
  # non RDF
  # CA population
  data_plot_median_income_ca <- data_ca_acs_2018 %>%
    select(zip_code, median_income)
  
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
    geom_histogram(data = median_income_rdf(),
                   aes(x = median_income,
                       y = stat(count) / sum(count),
                       bins = 10), 
                   fill = groups_colors_ridb_ca[[1]], 
                   alpha = 0.75) +
    geom_vline(xintercept = 75277, 
               linetype = "dashed", size = .3, alpha = .5) +
    annotate("text", label = "Median household income \nin 2018 in CA ($75,277)", 
             x = 118000, y = .15) +
    labs(x = "Median Income ($))",
         y = "",
         title = paste("Distribution of Median Income in Home in ZIP Codes in 2018 for", titleInput),
         subtitle = "Visitors' home ZIP codes for Overnight Reservations in California \nvs. California Residents") +
    scale_x_continuous(limits = c(0, 260000), breaks = seq(0, 260000, 50000), minor_breaks = seq(0, 260000, 25000), labels = comma) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
} # EO function