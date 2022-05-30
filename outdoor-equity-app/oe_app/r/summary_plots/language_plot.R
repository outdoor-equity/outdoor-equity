
#' Language Data Summary Plotly
#'
#' @param admin_unitInput User pick for admin unit
#' @param siteInput User pick for site
#' @param ridb_df RIDB dataframe object name
#'
#' @return Plotly of language
#'
#' @examples
language_plot <- function(admin_unitInput, siteInput, ridb_df){
  
  # reactive data frame 
  language_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    # reservations in CA
    ridb_df %>%
      filter(park %in% siteInput) %>%
      select(park, not_english_only, english_only) %>% 
      drop_na(not_english_only, english_only) %>% 
      mutate(mean_zip_code_population = 1) %>% 
      rename(location_indicator = park) %>% 
      relocate(mean_zip_code_population, .before = not_english_only) %>% 
      mutate(data_source = "Visitors to California Sites",
             tooltip_text = 
               paste("The green curve represents all visitors to this site.",
                     "<br>",
                     "If it is above the grey curve at a specific percent level", 
                     "<br>",
                     "that percentage of people who speak a language other than English at home", 
                     "<br>",
                     "is over-represented at this site compared to the California census."))
    
  }) # EO RDF
  
  # non RDF
  # CA population
  language_ca <- data_ca_acs_2018 %>%
    select(zip_code, mean_zip_code_population, not_english_only, english_only) %>% 
    drop_na(not_english_only, english_only) %>% 
    rename(location_indicator = zip_code) %>% 
    mutate(data_source = "California Residents",
           tooltip_text = 
             paste("The grey curve represents all visitors to this site.",
                   "<br>",
                   "If it is above the green curve at a specific percent level", 
                   "<br>",
                   "that percentage of people who speak a language other than English at home", 
                   "<br>",
                   "is under-represented at this site compared to the California census."))
  
  # join data for plotting
  language_data_plot <- rbind(language_rdf(), language_ca)
  
  # parameters
  color_ridb_ca <- c("Visitors to California Sites" = "#466C04", 
                     "California Residents" = "#848484")
  fill_ridb_ca <- c("Visitors to California Sites" = "#64863C", 
                    "California Residents" = "#a3a3a3")
  
  # plot for shiny app
  language_plotly <- ggplot() +
    geom_density(data = language_data_plot,
                 aes(x = not_english_only,
                     color = data_source,
                     fill = data_source,
                     weight = mean_zip_code_population, 
                     text = tooltip_text),
                 alpha = 0.5) +
    scale_fill_manual(values = fill_ridb_ca) +
    scale_color_manual(values = color_ridb_ca) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = NULL, expand = c(0.3, 0)) +
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