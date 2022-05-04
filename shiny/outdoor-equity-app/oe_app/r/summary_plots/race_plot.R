## race plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

race_plot <- function(admin_unitInput, siteInput){
  
  # reactive df
  # reservations in CA
  race_rdf <- reactive ({
    
    validate(
      need(siteInput != "",
           "Please select a reservable site to visualize.")
    ) # EO validate
    
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      summarize(white = mean(white, na.rm = TRUE),
                black = mean(black, na.rm = TRUE),
                asian = mean(asian, na.rm = TRUE),
                multiracial = mean(multiracial, na.rm = TRUE),
                other = mean(other, na.rm = TRUE),
                native_american = mean(native_american, na.rm = TRUE),
                pacific_islander = mean(pacific_islander, na.rm = TRUE),
                hispanic_latinx = mean(hispanic_latinx, na.rm = TRUE)) %>%
      pivot_longer(cols = 1:8, names_to = "race", values_to = "race_percent_average") %>% 
      mutate(race = str_replace(string = race,
                                pattern = "_",
                                replacement = " "),
             race = str_to_title(race))
    
  }) # EO RDF
  
  # non RDFs
  # CA population
  race_ca <- data_ca_acs_2018 %>%
    summarize(white = weighted.mean(white, mean_zip_code_population, 
                                    na.rm = TRUE),
              black = weighted.mean(black,  mean_zip_code_population,
                                    na.rm = TRUE),
              asian = weighted.mean(asian, mean_zip_code_population, 
                                    na.rm = TRUE),
              multiracial = weighted.mean(multiracial, mean_zip_code_population, 
                                          na.rm = TRUE),
              other = weighted.mean(other, mean_zip_code_population, 
                                    na.rm = TRUE),
              native_american = weighted.mean(native_american, mean_zip_code_population, 
                                              na.rm = TRUE),
              pacific_islander = weighted.mean(pacific_islander, mean_zip_code_population, 
                                               na.rm = TRUE),
              hispanic_latinx = weighted.mean(hispanic_latinx, mean_zip_code_population, 
                                              na.rm = TRUE)) %>%
    pivot_longer(cols = 1:8, names_to = "race", values_to = "race_percent_average") %>% 
    mutate(race = str_replace(string = race,
                              pattern = "_",
                              replacement = " "),
           race = str_to_title(race))
  
 # join data for plotting
  race_data_plot <- race_rdf() %>%
    left_join(y = race_ca,
              by = c("race"),
              suffix = c("_ridb", "_ca")) %>% 
    rename(RIDB = race_percent_average_ridb,
           CA = race_percent_average_ca) %>% 
    pivot_longer(cols = 2:3,
                 names_to = "data_source",
                 values_to = "race_percent_average") %>% 
    mutate(data_source = factor(data_source, levels = c("RIDB", "CA")),
           tooltip_start = case_when(data_source == "RIDB" ~ "Visitors to California sites live in <br>communities with an estimated ",
                                     data_source == "CA" ~ ""),
           tooltip_middle = case_when(data_source == "RIDB" ~ " ",
                                      data_source == "CA" ~ " of Californians are "),
           tooltip_end = case_when(data_source == "RIDB" ~ " populations",
                                   data_source == "CA" ~ ""),
           race = str_replace(string = race,
                              pattern = "Other",
                              replacement = "Other Race(s)"))
  
  print(paste("race data plot:", head(race_data_plot)))
  
  x_max <- max(race_rdf()$race_percent_average) + 0.1 # max x rounded to nearest 5
  
  # parameters
  groups_colors_ridb_ca <- c("RIDB" = "#009900FF", "CA" = "#666666")
  text_colors_ridb_ca <- c("RIDB" = "#006600", "CA" = "#282828")
  
  # plot for shiny app
  race_plotly <- ggplot(data = race_data_plot) +
    geom_col(aes(x = race_percent_average,
                 y = reorder(race, race_percent_average),
                 fill = data_source,
                 text = paste0(tooltip_start, percent(race_percent_average, accuracy = 0.1), 
                               tooltip_middle, race, tooltip_end)),
             position = "dodge") +
    scale_x_continuous(labels = percent, limits = c(0, x_max)) +
    scale_fill_manual(values = groups_colors_ridb_ca) + 
    geom_text(aes(x = race_percent_average,
                  y = reorder(race, race_percent_average),
                  label = percent(race_percent_average, accuracy = 0.1),
                  col = data_source), 
              position = position_dodge(width = 1), 
              size = 3) +
    scale_color_manual(values = text_colors_ridb_ca) +
    labs(x = "Percentage (%)",
         y = "") +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(race_plotly,
           tooltip = list("text")) %>%
    style(hoverinfo = "none", traces = c(3, 4),
          textposition = "right") %>% 
    layout(title = list(text = paste0('<b>', siteInput, '<br>', admin_unitInput, '</b>',
                                      '<br>',
                                      'Estimated Racial Percentages of California Residents vs. Visitors'),
                        font = list(size = 15)),
           showlegend = FALSE) %>%
    config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", 
                                         "hoverClosestCartesian", "hoverCompareCartesian"))
  
} # EO function