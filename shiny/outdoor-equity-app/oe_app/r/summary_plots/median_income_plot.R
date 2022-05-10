## median income plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

median_income_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  median_income_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
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
  color_ridb_ca <- c("Visitors to California Sites" = "#009900FF", 
                     "California Residents" = "#666666")
  fill_ridb_ca <- c("Visitors to California Sites" = "#00c000", 
                    "California Residents" = "#848484")
  
  # plot for shiny app
  median_income_plotly <- ggplot() +
    geom_density(data = median_income_data_plot,
                 aes(x = median_income,
                     color = data_source,
                     fill = data_source,
                     weight = mean_zip_code_population,
                     text = data_source),
                 alpha = 0.5) +
    scale_fill_manual(values = fill_ridb_ca) +
    scale_color_manual(values = color_ridb_ca) +
    scale_x_continuous(labels = dollar) +
    scale_y_continuous(labels = NULL, expand = c(0.3, 0)) +
    labs(x = "Household Median Income (US $)",
         y = NULL) +
    theme_minimal() +
    theme(plot.background = element_rect("white"))
          #panel.grid.major.y = element_blank())
  
  ggplotly(median_income_plotly, 
           tooltip = list("text")) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Median-incomes for California Residents vs. Visitors'),
                        font = list(size = 15)),
           showlegend = FALSE,
           margin = list(b = 130, t = 100),
           annotations =  list(x = 250000/2, y = -0.75, 
                               text = paste0("The shape of the curve can give you a sense of the distribution of median-income.<br>",
                                             "Taller peaks indicate a concentration of more people at a given income, while flatter curves indicate<br>",
                                             "a more even distribution. Comparing the green curve (all visitors to this site) to the grey curve (all CA residents),<br>",
                                             "can show who is more or less represented at this site compared to the CA census."), 
                               showarrow = F, 
                               xre = 'paper', yref = 'paper', 
                               xanchor = 'middle', yanchor = 'auto', 
                               xshift = 0, yshift = 0,
                               font = list(size = 11, color = "black"))) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian"))
  
} # EO function