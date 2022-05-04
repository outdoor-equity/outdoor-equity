## language plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

language_plot <- function(admin_unitInput, siteInput){
  
  # reactive data frame 
  language_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    # reservations in CA
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      select(park, not_english_only, english_only) %>% 
      drop_na(not_english_only, english_only) %>% 
      mutate(mean_zip_code_population = 1) %>% 
      rename(location_indicator = park) %>% 
      relocate(mean_zip_code_population, .before = not_english_only) %>% 
      mutate(data_source = "Visitors to California Sites")
    
  }) # EO RDF
  
  # non RDF
  # CA population
  language_ca <- data_ca_acs_2018 %>%
    select(zip_code, mean_zip_code_population, not_english_only, english_only) %>% 
    drop_na(not_english_only, english_only) %>% 
    rename(location_indicator = zip_code) %>% 
    mutate(data_source = "California Residents")
  
  # join data for plotting
  language_data_plot <- rbind(language_rdf(), language_ca)
  
  # parameters
  color_ridb_ca <- c("Visitors to California Sites" = "#009900FF", 
                     "California Residents" = "#666666")
  fill_ridb_ca <- c("Visitors to California Sites" = "#00c000", 
                    "California Residents" = "#848484")
  
  # plot for shiny app
  language_plotly <- ggplot() +
    geom_density(data = language_data_plot,
                 aes(x = not_english_only,
                     color = data_source,
                     fill = data_source,
                     weight = mean_zip_code_population, 
                     text = data_source),
                 alpha = 0.5) +
    scale_fill_manual(values = fill_ridb_ca) +
    scale_color_manual(values = color_ridb_ca) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = NULL) +
    labs(x = "Percentage of Population that Speaks a Language Other than English at Home",
         y = NULL) +
    theme_minimal() +
    theme(plot.background = element_rect("white"))
          #panel.grid.major.y = element_blank())
  
  ggplotly(language_plotly, 
           tooltip = list("text")) %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Non-English Spoken in the Home of California Residents vs. Visitors'),
                        font = list(size = 15)),
           showlegend = FALSE) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian"))
  
} # EO function