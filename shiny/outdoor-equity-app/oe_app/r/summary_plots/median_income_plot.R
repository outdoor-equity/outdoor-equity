## median income plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

median_income_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  median_income_rdf <- reactive ({
    
    # reservations in CA
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      select(park, median_income) %>% 
      rename(location_indicator = park) %>% 
      mutate(mean_zip_code_population = 1,
             data_source = "Visitors to California Sites")
    
  }) # EO RDF
  
  # non RDF
  # CA population
  median_income_ca <- data_ca_acs_2018 %>%
    select(zip_code, median_income, mean_zip_code_population) %>% 
    rename(location_indicator = zip_code) %>% 
    mutate(data_source = "California Residents")
  
  median_income_data_plot <- rbind(median_income_rdf(), median_income_ca)
  
  # parameters
  groups_colors_ridb_ca <- c("RIDB" = "#009900FF", "CA" = "#666666")
  
  # plot for shiny app
  median_income_plotly <- ggplot() +
    geom_histogram(data = median_income_data_plot,
                   aes(x = median_income,
                       color = data_source,
                       fill = data_source,
                       weight = mean_zip_code_population,
                       text = data_source),
                   alpha = 0.5) +
    scale_fill_manual(values = fill_ridb_ca) +
    scale_color_manual(values = color_ridb_ca) +
    scale_x_continuous(labels = dollar) +
    labs(x = "Household Median Income (US $)",
         y = "Density",
         title = paste0("Median-incomes for California Residents vs. <br>Visitors to ", 
                        siteInput, ", ", admin_unitInput)) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(plot_median_income, 
           tooltip = list("text")) %>% 
    layout(showlegend = FALSE)
  
} # EO function