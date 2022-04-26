## race plot and parameters ##
# used in DATA SUMMARY PLOTS in server
# using input id's for summary page in ui

race_plot <- function(admin_unitInput, siteInput){
  
  # reactive df
  # reservations in CA
  race_rdf <- reactive ({
    data_joined_2018 %>%
      filter(park %in% siteInput) %>%
      summarize(white = (mean(white, na.rm = TRUE) * 100),
                black = (mean(black, na.rm = TRUE) * 100),
                asian = (mean(asian, na.rm = TRUE) * 100),
                multiracial = (mean(multiracial, na.rm = TRUE) * 100),
                other = (mean(other, na.rm = TRUE) * 100),
                native_american = (mean(native_american, na.rm = TRUE) * 100),
                pacific_islander = (mean(pacific_islander, na.rm = TRUE) * 100),
                hispanic_latinx = (mean(hispanic_latinx, na.rm = TRUE) * 100)) %>%
      pivot_longer(cols = 1:8, names_to = "race", values_to = "race_percent_average") %>% 
      mutate(race = str_replace(string = race,
                                pattern = "_",
                                replacement = " "),
             race = str_to_title(race))
  }) # EO RDF
  
  # non RDFs
  # CA population
  race_ca <- data_ca_acs_2018 %>%
    summarize(white = (weighted.mean(white, mean_zip_code_population, 
                                     na.rm = TRUE) * 100),
              black = (weighted.mean(black,  mean_zip_code_population,
                                     na.rm = TRUE) * 100),
              asian = (weighted.mean(asian, mean_zip_code_population, 
                                     na.rm = TRUE) * 100),
              multiracial = (weighted.mean(multiracial, mean_zip_code_population, 
                                           na.rm = TRUE) * 100),
              other = (weighted.mean(other, mean_zip_code_population, 
                                     na.rm = TRUE) * 100),
              native_american = (weighted.mean(native_american, mean_zip_code_population, 
                                               na.rm = TRUE) * 100),
              pacific_islander = (weighted.mean(pacific_islander, mean_zip_code_population, 
                                                na.rm = TRUE) * 100),
              hispanic_latinx = (weighted.mean(hispanic_latinx, mean_zip_code_population, 
                                               na.rm = TRUE) * 100)) %>%
    pivot_longer(cols = 1:8, names_to = "race", values_to = "race_percent_average") %>% 
    mutate(race = str_replace(string = race,
                              pattern = "_",
                              replacement = " "),
           race = str_to_title(race))
  
 # join data for plotting
  race_data_plot <- race_rdf() %>%
      left_join(y = data_plot_race_ca,
                by = c("race"),
                suffix = c("_ridb", "_ca")) %>% 
      rename(RIDB = race_percent_average_ridb,
             CA = race_percent_average_ca) %>% 
      pivot_longer(cols = 2:3,
                   names_to = "data_source",
                   values_to = "race_percent_average") %>% 
      mutate(data_source = factor(data_source, levels = c("RIDB", "CA")))
  
  # parameters
  groups_colors_ridb_ca <- c("RIDB" = "#009900FF", "CA" = "#666666")
  
  # plot for shiny app
  race_plotly <- ggplot(data = race_data_plot) +
    geom_col(aes(x = race_percent_average,
                 y = reorder(race, race_percent_average),
                 fill = data_source,
                 ## think about how to have text that works for grey and green bars
                 text = paste0("Visitors to ", siteInput, ", ", admin_unitInput,
                               "<br>live in ZIP codes with an estimated ",
                               percent(race_percent_average, accuracy = 0.1), " of ",
                               race, " people.")),
             stat = "identity",
             position = "dodge") +
    ## need to fix this to work for different subsets
    scale_x_continuous(limits = c(0, 60), breaks = seq(0, 60, 10), minor_breaks = seq(0, 60, 5)) +
    scale_fill_manual(values = groups_colors_ridb_ca) +  
    scale_color_manual(values = groups_colors_ridb_ca) +
    geom_text(aes(x = race_percent_average,
                  y = reorder(race, race_percent_average),
                  label = paste0(round(race_percent_average, 1), "%"),
                  col = data_source), 
              position = position_dodge(width = 1), 
              hjust = -0.1, size = 4) +

    labs(x = "Percentage (%)",
         y = "",
         title = paste0("Racial Breakdown of ZIP Codes for Visits to ", siteInput, ", ", admin_unitInput)) +
    theme_minimal() +
    theme(plot.background = element_rect("white"),
          panel.grid.major.y = element_blank())
  
  ggplotly(race_plotly,
           tooltip = list("text"))
} # EO function